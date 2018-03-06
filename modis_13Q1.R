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

########### 1.MAIN FUNCTION: Raw2Geotiff() ##############

# This function does the downloading&processing of new observations. It outputs a data.frame with 2 rows: file -> absolute path to geotiff, date -> date of the geotiff observation
# 1.Step: Compare local RAW data files with online storage/ 2.Step: Download new RAW files/ 3. Process RAW files to geotiffs
# ProcessMODIS_13Q1 acesses the LP DAAC and LAADS FTP Server
# They can be adapted to work with other MODIS Products on those servers, but it requires some effort.

Raw2Geotiff <- function(daterange, shapefilepath, dstfolder, srcstorage=NULL, crop_to_cutline=TRUE, geotiff_compression=TRUE){
  # Download and Processes the MODIS data for the specified daterange and extent. Uses the MODIS package for download from LP DAAC and LAADS.
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
  
  compressionmethod <- GenerateCompressionArgument(geotiff_compression)
  
  tempfolder = file.path(tempdir(),"processing")
  
  if (dir.exists(tempfolder)) {
    do.call(unlink, list(tempfolder,recursive=TRUE))
  }
  dir.create(tempfolder)
  oldwd = getwd()
  setwd(tempfolder)
  
  if (!is.null(srcstorage)) {
    capture.output(capture.output(MODISoptions(quiet=TRUE,localArcPath=srcstorage,save=FALSE),file='NULL', type="message"),file='NULL') #capture.output to drop all the initiliastion messages of MODIS package
  } else {
    capture.output(capture.output(MODISoptions(quiet=TRUE,localArcPath=tempfolder,save=FALSE),file='NULL', type="message"),file='NULL')
    checkHDFintegrity()
  } 
  
  if (!dir.exists(dstfolder)) {
    dir.create(dstfolder)
  }
  
  output=data.frame(file=c(),date=c())
  
  # Get vector of required tiles for the specified shapefile
  myshp <- readOGR(shapefilepath, verbose=FALSE)
  e <- extent(myshp)
  tile <- getTile(e)
  
  # Try MODIS Aqua first
  x='MYD13Q1'
  out1=tryCatch(getHdf(product=x,begin=daterange[1],end=daterange[2],tileH=tile@tileH,tileV=tile@tileV, wait=0.5, checkIntegrity=TRUE), error = function(e) {list()})
  
  # Then MODIS Terra
  x='MOD13Q1'
  out2=tryCatch(getHdf(product=x,bbegin=daterange[1],end=daterange[2],tileH=tile@tileH,tileV=tile@tileV, wait=0.5, checkIntegrity=TRUE), error = function(e) {list()})
  
  # Merge lists of downloaded HDF Tiles
  files <- c(unname(unlist(out1)),unname(unlist(out2)))
  
  # abort when nothing has been downloaded or the number of downlaoded tiles does not coincide with the number of required tiles for this shapefile
  if ((length(files) %% length(tile@tileH)*length(tile@tileV)) > 0) {
    setwd(oldwd)
    return(output) 
  } else if (length(files)==0) {
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
    GTifflist <- c()
    GTifflist2 <- c()
    
    # Go through each Tile of the current date and: check file integrity, extract EVI layer, cut to shapefile, reproject to EPSG:4326
    for (i in 1:nrow(HDFlistbydateandtile)){
      for (j in 1:nrow(HDFlistbydateandtile[[i]])) {
        HDFfile <- HDFlistbydateandtile[[i]][j,]
        cat('Processing ... ',as.character(HDFfile$file),'\n',sep='')
        GTifflist[i]=tempfile(tmpdir=tempfolder,fileext = ".tif")
        GTifflist2[i]=tempfile(tmpdir=tempfolder, fileext = ".tif")
        
        validHDF <- isvalidHDF_4_5(HDFfile$file)
        if (!validHDF) {
          unlink(as.character(HDFfile$file))
          cat('The downloaded file has not been recognized as a valid HDF4/5 file. Try again or manually download the following file and check its validity: ', as.character(HDFfile$files),'\n',sep='')
          filesvalid=FALSE
          setwd(oldwd)
          break
        }
      }
      
      # GEOTIFF PROCESSING: TODO: adapt for NDVI
      sds<-getSds(as.character(HDFfile$file))
      qualityband<-raster(readGDAL(sds$SDS4gdal[12], as.is=TRUE, silent=TRUE))
      mask<-calc(qualityband, fun = function(x) {bitwAnd(2,x)})
      rm(qualityband);gc() # free memory
      if (j>1) {
        evi2 <- evi
      } 
      evi<-raster(readGDAL(sds$SDS4gdal[2], as.is=TRUE, silent=TRUE))
      
      evi[mask==2]<- -10000
      evi[is.na(evi)]<- -10000
      
      
      rm(mask);gc()
      
      if (j>1) {
        evi<-mosaic(evi,evi2,fun=mean) #mean of both rasters, na.rm=TRUE
      }
      
      writeRaster(evi,filename=GTifflist[i],format="GTiff",datatype="INT2S")
      rm(evi);gc()
      gdalwarp(srcfile=GTifflist[i],dstfile=GTifflist2[i],cutline=shapefilepath,crop_to_cutline = TRUE, t_srs="EPSG:4326") #Transform GTiff and crop to shapefile
      
    }
    
    # In case all Tiles of the current date are valid, mosaic them to one Gtiff File
    if (filesvalid) {
      filename=paste(HDFlist$dates[i],'.tif',sep='')
      dstfile <- file.path(dstfolder,filename)
      mosaic_rasters(GTifflist2,dstfile,co=compressionmethod, ot="Int16")  #
      output <- rbind(output,data.frame(file=dstfile,date=as.Date(HDFlist$date[i]))) #TODO, date format correct?
    }
    
    
    # Delete all temporary working data and HDF Files in the temporary folder (if argument hdfstorage is NULL)
    do.call(unlink, list(tempfolder,recursive=TRUE))
    
    setwd(oldwd)
    return(output)
  }
}
