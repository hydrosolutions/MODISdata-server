########### 0.HEADER ##############
library(rgdal)
library(rgeos)
library(gdalUtils)
library(raster)
library(MODIS)
library(rts)
library(RCurl)
library(leaflet)
library(lubridate)
library(XML)
library(R.utils)
library(httr)

########### 0.HELPER FUNCTIONS ##############

# Simple delper function to set the compress argument in gdal
GenerateCompressionArgument <- function(compression) {
  # Helper function to set the compression argument for gdalwarp
  
  if (compression) {
    compressionmethod <- c("COMPRESS=deflate","zlevel=9")
  } else {
    compressionmethod <- "compress=none" 
  }
  return(compressionmethod)
}

# Helper function to find local files
listHDFfiles <- function(product, datapath, tiles, begin=NULL, end=NULL) {
  # Searches a local folder for MODIS .hdf files within the specifications for product, tile and daterange
  #
  # Args:
  #   product: any MODIS product abbreviation as character, e.g. "MOD10A1" or "MYD10A1"
  #   tileH,tileV: The path where to search for files.
  #   tiles: A data.frame with h and v of the tiles which should be downloaded. 
  #   begin,end: the begin or end date within which files should be selected, e.g. as.Date(2017-01-01). If NULL, all files of any date will be selected.
  #
  # Returns:
  #  data.frame with rows (path, file, date) of all files that have been found
  
  # Concentate search pattern
  tilepattern <- paste(sprintf("%s", tiles),collapse="|")
  pattern <- paste(product,".*(",tilepattern,").*","\\.hdf$",sep="")
  
  # Find files with specified pattern
  localfilesearch <- list.files(path=datapath, pattern=pattern, recursive = TRUE, full.names=TRUE)
  
  # Filter files for daterange begin to end
  localfiles <- data.frame(path=c(),date=c(), file=c())
  if (length(localfilesearch) > 0) {
    filename <- basename(localfilesearch)
    dates <- extractDate(filename,asDate=TRUE,pos1=10, pos2=16)$inputLayerDates
    index <- rep(TRUE,length(dates))
    if (!is.null(begin)) {
      index <- index & (dates >= begin)
    }
    if (!is.null(end)) {
      index <- index & (dates <= end)
    }
    localfiles <- rbind(localfiles,data.frame(path=localfilesearch[index],date=dates[index], file=filename[index]))
  }
  return(localfiles)
}

# Helper function to check integrity of local files
removeCorruptHDF <- function(path, prefix=NULL, max.deletions=Inf) {
  # Deletes all .hdf files in path that can not be properly read by gdalinfo as a HDF4 or HDF5 file.
  #
  # Args:
  #   path: a vector defining the start- and enddate for which MODIS data shall be generated. e.g. daterange=c(as.Date(2018-01-01),today())
  #   prefix: Only .hdf files with the specified prefix are considered. Can be used to differentiate between e.g. different MODIS products (MOD10A1,MYD10A1)
  #   max.deletions: max. number of deletions before an error is raised. Default is Infinity. Can bes used to avoid accidental deletion of complete data record.
  #
  # Returns:
  #   Nothing, but prints message n console which files have been deleted or if max.deletions has been reached
  
  list <- list.files(path, pattern=paste(prefix,".*\\.hdf$",sep=""), recursive = TRUE, full.names = TRUE)
  count = 0
  for (file in list) {
    validfile <- isvalidHDF_4_5(file)
    if (!validfile && file.exists(file) && count<max.deletions) {
      count <- count + 1
      file.remove(file)
      cat(file," has been removed because it was an invalid or corrupted HDF file \n")
    } else if (count>=max.deletions) {
      stop("The maximum number of files to be deleted has been reached \n")
    }
  }
}

isvalidHDF_4_5 <- function(filename) {
  # Checks if the file at filename is a valid HDF4 or HDF5 file and can be read by gdal.
  #
  # Args:
  #   filename: path to the file that should be checked.
  #
  # Returns:
  #   TRUE(valid HDF file) or FALSE
  
  validfile <- tryCatch({gdalinfo(filename)}, warning = function(w) {FALSE} ,error = function(e) {FALSE})
  if (validfile[1]=="Driver: HDF4/Hierarchical Data Format Release 4" || validfile[1]=="Driver: HDF5/Hierarchical Data Format Release 5") {
    out <- TRUE} 
  else {
    out <- FALSE
  }
  return(out)
}

DownloadFromNSIDC <- function(product, collection="006", datapath, daterange, tiles, max_wait=300, checkIntegrity=FALSE) {
  # Updates local MODIS MOD10A1/MYD10A1 files within the specified range
  #
  # Args:
  #   product: any MODIS product abbreviation available at https://n5eil01u.ecs.nsidc.org/MOST/ and https://n5eil01u.ecs.nsidc.org/MOSA/ e.g. "MOD10A1" or "MYD10A1"
  #   datapath: The path where existing files are currently stored. Must be of the following structure: <datapath>/MODIS/<PRODUCT.COLLECTION>
  #             e.g. /home/user/datastorage/MODIS/MYD10A1.006/. If the strcuture does not exist, it will be created under datapath.
  #   daterange: a vector with the start- and enddate for whcih data should be downloaded, e.g. c(as.Date("2018-01-01"),as.Date("2018-01-31"))
  #   tileH, tileV: A vector with the tile numbers which should be downloaded. Each element in tileH corresponds to the element at the same index in tileV. 
  #                 Both vectors must be of the same length.
  #   max_wait: The maximum time in seconds, that should be waited after a failed request to the server
  #   checkIntegrity: If True, all hdf files for the specified product in datapath are checked for validity. Invalid files are removed. Use with care!
  #
  # Returns:
  #   a vector of the absolute path to all .hdf files, that are available in datapath within the specified range. Includes all files, not only those which have been downloaded.
  
  # Set the server path for the specified product
  product.collection <- paste(product,collection,sep=".")
  if (grepl("MOD", product)) {
    baselink <- paste("https://n5eil01u.ecs.nsidc.org/MOST",product.collection,sep="/")
  } else if (grepl("MYD", product)) {
    baselink <- paste("https://n5eil01u.ecs.nsidc.org/MOSA",product.collection,sep="/")
  } 
  
  # Test Server path
  out <- RETRY("GET",baselink, pause_cap = max_wait, times=3, authenticate("hydrosolutions", "Hydromet2018"))
  if (!out$status_code==200) {
    stop(paste("The server at ",baselink, " can not be rached: Does the specified product.collection combination exist on that server?"))
  }
  
  # Check if datapath exists, or else create it. Check file validity if argument checkIntegrity=TRUE
  datapath <- file.path(datapath,"MODIS",product.collection)
  if (!dir.exists(datapath)) {
    dir.create(datapath, recursive=TRUE)
  } else if (checkIntegrity) {
    removeCorruptHDF(datapath, product=product, max.deletions=Inf)
  }
  tempfile <- tempfile()
  
  # input preparations
  begin <- daterange[1]
  end <- daterange[2]
  
  # get a list of available local files
  localfiles <- listHDFfiles(product=product,datapath,tiles)
  
  # get available online files. Start at webserver root and fetch all links from index.html.
  # Webserver structure is: root/YYYY.MM.DD/****.hdf
  cat("Browsing the webserver file structure for new files in ",baselink,"\n")
  tilepattern <- paste(sprintf("%s", tiles),collapse="|")
  pattern <- paste(product,".*(",tilepattern,").*","\\.hdf$",sep="")
  links <- xpathSApply(htmlParse(out), "//a/@href")
  links <- unname(links[!duplicated(links)])
  
  # Valid links to HDF files can be read with as.Date(). Go one level deeper into server file structure and again fetch all links.
  # Filter links by product***hxxvxx***.hdf pattern
  onlinefiles <- data.frame(file = c(), date = c(), link = c())
  for (link in links) {
    date <- as.Date(link,format="%Y.%m.%d/")
    if (!is.na(date) && date >= begin && date <= end) {
      nextlink <- paste(baselink,as.character(date,format="%Y.%m.%d"),sep="/")
      out <- RETRY("GET", nextlink, pause_cap = max_wait, times=3, authenticate("hydrosolutions", "Hydromet2018"))
      if (out$status_code==200) {
        links2 <- xpathSApply(htmlParse(out), "//a/@href")
        links2 <- unname(links2[!duplicated(links2)])
        files <- links2[grepl(pattern, links2)]
        if (length(files)>0) {
          onlinefiles <- rbind(onlinefiles, data.frame(file = files, date = date, link = paste(nextlink,files,sep="/")))
        }
      }  
    }
  }
  
  # Compare available online files with existing local files.
  newfiles <- onlinefiles[!onlinefiles$file %in% localfiles$file,]
  
  # Download any files that are missing in the local file storage. After download, check file validity, then move file to datapath
  if (nrow(newfiles) > 0) {
    for (i in 1:nrow(newfiles)) {
      savepath <- file.path(datapath,format(newfiles$date[i],format="%Y.%m.%d"))
      if (!dir.exists(savepath)) {
        dir.create(savepath, recursive=TRUE)
      }
      dstfile <- file.path(savepath,newfiles$file[i])
      filelink <- as.character(newfiles$link[i])
      cat("Downloading ",filelink,"\n")
      response <- RETRY("GET",filelink, pause_cap = max_wait, times=3, authenticate("julesair2", "538-FuJnS"), write_disk(path = tempfile,overwrite = TRUE))
      if (response$status_code==200 && isvalidHDF_4_5(tempfile)) {
        file.copy(tempfile,dstfile)
      } 
      file.remove(tempfile)
    }
  }
  
  # Compile a list of all file in datapath that fit into the specified range for product, daterange and tile
  availablefile <- listHDFfiles(product=product,datapath,tiles=tiles, begin=begin, end=end)
  return(as.vector(availablefile$path))
}

########### 1.MAIN FUNCTION: Raw2Geotiff() ##############

# This function does the downloading&processing of new observations. It outputs a data.frame with 2 rows: file -> absolute path to geotiff, date -> date of the geotiff observation
# 1.Step: Compare local RAW data files with online storage/ 2.Step: Download new RAW files/ 3. Process RAW files to geotiffs
# ProcessMODIS_10A1 accesses the NSDIC HTTPS Server
# They can be adapted to work with other MODIS Products on those servers, but it requires some effort.
Raw2Geotiff <- function(daterange, shapefilepath, dstfolder, srcstorage=NULL, geotiff_compression=TRUE){
  # Download and Processes the MODIS data for the specified daterange and extent. Uses an internal download function to fetch data from NSDIC server
  #
  # Args:
  #   daterange: a vector defining the start- and enddate for which MODIS data shall be generated. e.g. daterange=c(as.Date(2018-01-01),today())
  #   shapefilepath: A path to a shapefile with one polygon, that defines the extent for which data shall be generated. Output is cropped to the extent of the poylgon.
  #   dstfolder: The folder where the output geotiffs are written to.
  #   srcstorage: The folder where prior downloaded MODIS hdf files are stored and the newly downlaoded files will be written to. Default is NULL: no storage. HDF files will be deleted at the end.
  #   geotiff_compression: Default: TRUE if output geotiff should be compressed (lossless,deflate level=9). FALSE for no compression.
  #
  # Returns:
  #   data.frame(file=c(character()),date=c(date())): A dataframe that contains the full filename and date of the newly generated geotiff files.
  # Helper function to convert tile names
  tilestrings2tilevec <- function(tilestrings) {
    out <- data.frame(h = c(), v = c())
    for (string in tilestrings) {
      h <- substr(string,2,3)
      v <- substr(string,5,6)
      out <- rbind(out,data.frame(h = h, v=v))
    }
    return(out)
  }
  

  
  compressionmethod <- GenerateCompressionArgument(geotiff_compression)
  
  tempfolder = file.path(tempdir(),"processing")
  
  if (dir.exists(tempfolder)) {
    do.call(unlink, list(tempfolder,recursive=TRUE))
  }
  dir.create(tempfolder)
  oldwd = getwd()
  setwd(tempfolder)
  if (!is.null(srcstorage)) {
    localArcPath=srcstorage
  } else {
    localArcPath=tempfolder
  } 
  
  if (!dir.exists(dstfolder)) {
    dir.create(dstfolder, recursive = TRUE)
  }
  
  output=data.frame(file=c(),date=c())
  
  # Get vector of required tiles for the specified shapefile
  myshp <- readOGR(shapefilepath, verbose=FALSE)
  e <- extent(myshp)
  tile <- getTile(e)
  # Try MODIS Aqua first
  collection="006"
  x='MYD10A1'
  out1=tryCatch(DownloadFromNSIDC(product=x,datapath=localArcPath,daterange=daterange,tiles=tile@tile), error = function(e) {NULL})
  
  # Then MODIS Terra
  x='MOD10A1'
  out2=tryCatch(DownloadFromNSIDC(product=x,collection=collection,datapath=localArcPath,daterange=daterange,tiles=tile@tile), error = function(e) {NULL})
  
  # Merge lists of downloaded HDF Tiles
  files <- c(out1,out2)
  # abort when nothing has been downloaded or the number of downlaoded tiles does not coincide with the number of required tiles for this shapefile
  if (length(files)==0) {
    setwd(oldwd)
    return(output)
  }
  
  # Sort list of downloaded files by date
  dates = extractDate(basename(files),asDate=TRUE, pos1=10, pos2=16)$inputLayerDates
  tiles <- substr(basename(as.character(files)),start=18,stop=23)
  observationsbydate = data.frame(date=dates,tile=tiles,file=files)
  observationsbydate = by(as.data.frame(observationsbydate), as.data.frame(observationsbydate)[,"date"], function(x) x)
  
  # Process all Tiles of each observation date
  for (k in 1:nrow(observationsbydate)) {
    filesvalid=TRUE #help variable in case a HDF File is corrupted
    HDFlist =  observationsbydate[[k]]
    HDFlistbydateandtile <- by(as.data.frame(HDFlist), as.data.frame(HDFlist)[,"tile"], function(x) x)
    if ((length(HDFlistbydateandtile) != length(tile@tile))) {
      cat('Some Tile are missing for date ',observationsbydate[[k]][1,'date'],'. Processing is skipped', '\n',sep='')
      next 
    } 
    GTifflist <- c()
    GTifflist2 <- c()
    
    # Go through each Tile of the current date and: check file integrity, extract EVI layer, cut to shapefile, reproject to EPSG:4326
    for (i in 1:nrow(HDFlistbydateandtile)){
      for (j in 1:nrow(HDFlistbydateandtile[[i]])) {
        HDFfile <- HDFlistbydateandtile[[i]][j,]
        cat('Processing ... ',as.character(HDFfile$file),'\n',sep='')
        GTifflist[i]=tempfile(tmpdir=tempfolder,fileext = ".tif")
        GTifflist2[i]=tempfile(tmpdir=tempfolder,fileext = ".tif")
        
        validHDF <- isvalidHDF_4_5(HDFfile$file)
        if (!validHDF) {
          unlink(as.character(HDFfile$file))
          cat('The downloaded file has not been recognized as a valid HDF4/5 file. Try again or manually download the following file and check its validity: ', as.character(HDFfile$files),'\n',sep='')
          filesvalid=FALSE
          next
        }
        
        # GEOTIFF PROCESSING: 
        sds<-getSds(as.character(HDFfile$file))
        qualitymask<-raster(readGDAL(sds$SDS4gdal[2], as.is=TRUE, silent=TRUE))
        if (j>1) {
          evi2 <- evi
        } 
        evi<-raster(readGDAL(sds$SDS4gdal[1], as.is=TRUE, silent=TRUE))
        
        watermask <-  (qualitymask == 239) | ((raster(readGDAL(sds$SDS4gdal[3], as.is=TRUE, silent=TRUE)) %% 2) == 1)
        cloudmask <- (qualitymask > 2 | is.na(evi) | evi>100)
        evi[watermask | cloudmask] <- NA
        #evi[evi>0] <- 0.06+1.21*evi[evi>0] # https://www.sciencedirect.com/science/article/pii/S0034425703002864
        #evi[evi>100] <- 100
        
        rm(qualitymask);gc()
        if (j>1) {
          evi<-mosaic(evi,evi2,fun=mean) #mean of both rasters, na.rm=TRUE
        }
      }
      
      evi[is.na(evi) & cloudmask]<--32767
      evi[is.na(evi) & watermask]<--32766
      writeRaster(evi,filename=GTifflist[i],format="GTiff",overwrite=TRUE, datatype="INT2S",NAflag=-32768)
      rm(evi,cloudmask,watermask);gc()
      tryCatch({
        gdalwarp(srcfile=GTifflist[i],dstfile=GTifflist2[i],cutline=shapefilepath,crop_to_cutline = TRUE, t_srs="EPSG:4326", ot="Int16",dstnodata=-32768) #Transform GTiff and crop to shapefile
      }, error = function(e) {cat('gdalwarp error:',e);filesvalid=FALSE})
    }
    
    # In case all Tiles of the current date are valid, mosaic them to one Gtiff File
    if (filesvalid) {
      filename=paste(HDFlist$date[i],'.tif',sep='')
      dstfile <- file.path(dstfolder,filename)
      mosaic_rasters(GTifflist2,dstfile,co=compressionmethod, ot="Int16",srcnodata=-32768)  #
      output <- rbind(output,data.frame(file=dstfile,date=as.Date(HDFlist$date[i]))) #TODO, date format correct?
    }
  }
  # Delete all temporary working data and HDF Files in the temporary folder (if argument hdfstorage is NULL)
  do.call(unlink, list(tempfolder,recursive=TRUE))
  
  setwd(oldwd)
  return(output)
}
