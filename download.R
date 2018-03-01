#!/usr/bin/env Rscript

#################### MODIS SNOW MOD10A1/MYD10A1 ##############################

# Check if Script is run from RStudio and load the default configuration file
# Else receive command line argument for "path to configuration file"
cmd = TRUE
try({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  source('config.R')
  cmd = FALSE
  }, silent = TRUE
)

# Check validity of args
if (cmd) {
  args = commandArgs(trailingOnly=TRUE)
  if (length(args)<1) {
    stop("Argument for configuration file is missing, try: Rscript download.R path/config.R")
  } else if (length(args)>1) {
    stop("Too many arguments, try: Rscript download.R path/config.R")
  } else {
    if (!file.exists(args)) {
      stop(paste("The file ",args," does not exist."))
    } else {
      tryCatch({
        source(args)
      }, error = function(e) {
        stop(paste(e,"\nThe file ",args," is not a valid configuration file"))
      })
    }
  }
}  


# LOAD LIBRARIES
library(rts)
library(RCurl)
library(raster)
rasterOptions(chunksize=1e+05,maxmemory=1e+06)
library(rgdal)
library(rgeos)
library(MODIS)
library(gdalUtils)
library(leaflet)
library(lubridate)
library(XML)
library(R.utils)
library(httr)

listHDFfiles <- function(product, datapath,tileH=tile@tileH,tileV=tile@tileV,begin=NULL,end=NULL) {
  tileHpattern <- paste(sprintf("%02d", tileH),collapse="|")
  tileVpattern <- paste(sprintf("%02d", tileV),collapse="|")
  pattern <- paste(product,".*h(",tileHpattern,")v(",tileVpattern,").*","\\.hdf$",sep="")
  localfilesearch <- list.files(path=datapath, pattern=pattern, recursive = TRUE, full.names=TRUE)
  
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

getMODIS_SNOW <- function(product, datapath, begin=date[[1]],end=date[[2]],tileH=tile@tileH,tileV=tile@tileV, wait=0.5, checkIntegrity=TRUE) {
  if (product == "MOD10A1") {
    product.collection <- "MOD10A1.006"
    baselink <- "https://n5eil01u.ecs.nsidc.org/MOST/MOD10A1.006/"
  } else if (product == "MYD10A1") {
    product.collection <- "MYD10A1.006"
    baselink <- "https://n5eil01u.ecs.nsidc.org/MOSA/MYD10A1.006/"
  } else {
    stop("This function only supports MOD10A1 or MYD10A1")
  }
  
  datapath <- file.path(datapath,"MODIS",product.collection)
  if (!dir.exists(datapath)) {
    dir.create(datapath)
  }
  
  # getavailable local files
  localfiles <- listHDFfiles(product=product,datapath,tileH=tileH,tileV=tileV)
  
  # get available online files
  tileHpattern <- paste(sprintf("%02d", tileH),collapse="|")
  tileVpattern <- paste(sprintf("%02d", tileV),collapse="|")
  pattern <- paste(product,".*h(",tileHpattern,")v(",tileVpattern,").*","\\.hdf$",sep="")
  out <- GET(baselink, 
             authenticate("hydrosolutions", "Hydromet2018"))
  links <- xpathSApply(htmlParse(out), "//a/@href")
  links <- unname(links[!duplicated(links)])
  
  onlinefiles <- data.frame(file = c(), date = c(), link = c())
  for (link in links) {
    date <- as.Date(link,format="%Y.%m.%d/")
    if (!is.na(date) && date >= begin && date <= end) {
      nextlink <- paste(baselink,link,sep="")
      out <- GET(nextlink, authenticate("hydrosolutions", "Hydromet2018"))
      links2 <- xpathSApply(htmlParse(out), "//a/@href")
      links2 <- unname(links2[!duplicated(links2)])
      files <- links2[grepl(pattern, links2)]
      if (length(files)>0) {
        onlinefiles <- rbind(onlinefiles, data.frame(file = files, date = date, link = paste(nextlink,files,sep="")))
      }
    }
  }
  
  newfiles <- onlinefiles[!onlinefiles$file %in% localfiles$file,]
  if (nrow(newfiles) > 0) {
    for (i in 1:nrow(newfiles)) {
      savepath <- file.path(datapath,format(newfiles$date[i],format="%Y.%m.%d"))
      if (!dir.exists(savepath)) {
        dir.create(savepath)
      }
      response <- RETRY("GET",as.character(newfiles$link[i]), times=3, authenticate("julesair2", "538-FuJnS"), write_disk(path = file.path(savepath,newfiles$file[i]),overwrite = TRUE))
      if (!response$status_code==200) {
        file.remove(file.path(savepath,newfiles$file[i]))
      }
    }
  }
  availablefile <- listHDFfiles(product=product,datapath,tileH=tileH,tileV=tileV, begin=begin, end=end)
  return(as.vector(availablefile$path))
}

GenerateCompressionArgument <- function(compression) {
  if (compression) {
    compressionmethod <- c("COMPRESS=deflate","zlevel=9")
  } else {
    compressionmethod <- "compress=none" 
  }
  return(compressionmethod)
}

isString <- function(value) {
  sapply(value, FUN = function(x) {
    if (is.na(x) || is.null(x) || x == "") {
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
}

isDate <- function(value) {
  return(tryCatch({as.Date(value);TRUE}, error = function(e) {FALSE}))
}

removeCorruptHDF <- function(path, product=NULL, max.deletions=3) {
  # Deletes all files that can not be properly read by gdalinfo as a HDF4 or HDF5 file.
  # Only files that match the MODIS filename pattern (can be further limited with the argument product) are considered
  list <- list.files(path, pattern=paste(product,".*\\.hdf$",sep=""), recursive = TRUE, full.names = TRUE)
  count = 0
  for (file in list) {
    validfile <- tryCatch({gdalinfo(file)}, warning = function(w) {FALSE} ,error = function(e) {FALSE})
    if (validfile[1]=="Driver: HDF4/Hierarchical Data Format Release 4" || validfile[1]=="Driver: HDF5/Hierarchical Data Format Release 5") {
      validfile <- TRUE} 
    else {
      validfile <- FALSE
    }
    if (!validfile && file.exists(file) && count<max.deletions) {
      count <- count + 1
      file.remove(file)
      cat(file," has been removed because it was an invalid or corrupted HDF file.")
    } else if (count>=max.deletions) {
      cat("The maximum number of files to be deleted has been reached")
      break
    }
  }
}

UpdateAndProcessMODIS <- function(date, shapefilepath, dstfolder, hdfstorage=NULL, cloudmask=TRUE, compression=FALSE){
  # Input: 
  # date=c(begindate,enddate)
  # shapefile = path to a valid shapefile
  # dstfolder = path to the folder, where output GeoTiff Files shall be saved
  # path to a Folder for persistant MODIS HDF storage (when there is a chance that the HDFs are used again). NULL when 
  # no persistent storage should be used. All downloaded MODIS HDF Files will then be deleted after processing.
  # Set compression argument for gdalwarp option -co
  compressionmethod <- GenerateCompressionArgument(compression)
  
  tempfolder = file.path(tempdir(),"processing")
  
  if (dir.exists(tempfolder)) {
    do.call(unlink, list(tempfolder,recursive=TRUE))
  }
  dir.create(tempfolder)
  oldwd = getwd()
  setwd(tempfolder)
  if (!is.na(hdfstorage)) {
    localArcPath=hdfstorage
  } else {
    localArcPath=tempfolder
  } 
  
  if (!dir.exists(dstfolder)) {
    dir.create(dstfolder)
  }
  
  outputdates=c()
  outputfiles=c()

  # Get vector of required tiles for the specified shapefile
  myshp <- readOGR(shapefilepath, verbose=FALSE)
  e <- extent(myshp)
  tile <- getTile(e)

  # Try MODIS Aqua first
  x='MYD10A1'
  out1=tryCatch(getMODIS_SNOW(product=x,datapath=localArcPath,begin=date[[1]],end=date[[2]],tileH=tile@tileH,tileV=tile@tileV, wait=0.5, checkIntegrity=TRUE), error = function(e) {list()})
  
  # Then MODIS Terra
  x='MOD10A1'
  out2=tryCatch(getMODIS_SNOW(product=x,datapath=localArcPath,begin=date[[1]],end=date[[2]],tileH=tile@tileH,tileV=tile@tileV, wait=0.5, checkIntegrity=TRUE), error = function(e) {list()})
  
  # Merge lists of downloaded HDF Tiles
  files <- c(unname(unlist(out1)),unname(unlist(out2)))

  # abort when nothing has been downloaded or the number of downlaoded tiles does not coincide with the number of required tiles for this shapefile
  if ((length(files) %% length(tile@tileH)*length(tile@tileV)) > 0) {
    setwd(oldwd)
    return(NULL) #TODO: return more meaningful output, and delete files maybe?
  } else if (length(files)==0) {
    setwd(oldwd)
    return(NULL)
  }
  
  # Sort list of downloaded files by date
  dates = extractDate(basename(files),asDate=TRUE, pos1=10, pos2=16)$inputLayerDates
  tiles <- substr(basename(as.character(files)),start=18,stop=23)
  observationsbydate = data.frame(dates,tiles,files)
  observationsbydate = by(as.data.frame(observationsbydate), as.data.frame(observationsbydate)[,"dates"], function(x) x)
  
  # Process all Tiles of each observation date
  for (k in 1:nrow(observationsbydate)) {
    
    filesvalid=TRUE #help variable in case a HDF File is corrupted
    HDFlist =  observationsbydate[[k]]
    HDFlistbydateandtile <- by(as.data.frame(HDFlist), as.data.frame(HDFlist)[,"tiles"], function(x) x)
    GTifflist <- c()
    GTifflist2 <- c()

	# Go through each Tile of the current date and: check file integrity, extract EVI layer, cut to shapefile, reproject to EPSG:4326
    for (i in 1:nrow(HDFlistbydateandtile)){
      for (j in 1:nrow(HDFlistbydateandtile[[i]])) {
        HDFfile <- HDFlistbydateandtile[[i]][j,]
        cat('Processing ... ',as.character(HDFfile$files),'\n',sep='')
        GTifflist[i]=tempfile('NDVI',tempfolder,fileext = ".tif")
        GTifflist2[i]=tempfile('NDVI',tempfolder, fileext = ".tif")
        
        t <- tryCatch(gdalinfo(HDFfile$files), warning = function(w) {NULL} ,error = function(e) {NULL})
        if (is.null(t)) {
          unlink(as.character(HDFfile$files))
          try(getHdf(HdfName=basename(as.character(HDFfile$files)), wait=10))
          t <- tryCatch(gdalinfo(HDFfile$files), warning = function(w) {NULL} ,error = function(e) {NULL})
          if (is.null(t)) {
            cat('Download was not successfull. Try to manually download the following file and check its validity: ', as.character(HDFfile$files),'\n',sep='')
            filesvalid=FALSE
            setwd(oldwd)
            break
          }
        }
        
        sds<-getSds(as.character(HDFfile$files))
        mask<-raster(readGDAL(sds$SDS4gdal[2], as.is=TRUE, silent=TRUE))
        if (j>1) {
          evi2 <- evi
        } 
        evi<-raster(readGDAL(sds$SDS4gdal[1], as.is=TRUE, silent=TRUE))
        
        if (cloudmask) {
          evi[mask > 2 | is.na(evi) | evi>100]<- NA
          evi[evi>0] <- 0.06+1.21*evi[evi>0] # https://www.sciencedirect.com/science/article/pii/S0034425703002864
          evi[evi>100] <- 100
        }
        rm(mask);gc()
        if (j>1) {
          evi<-mosaic(evi,evi2,fun=mean) #mean of both rasters, na.rm=TRUE
        }
      }
	    
      
      writeRaster(evi,filename=GTifflist[i],format="GTiff",overwrite=TRUE, dataType="INT1S")
      rm(evi);gc()
      gdalwarp(srcfile=GTifflist[i],dstfile=GTifflist2[i],cutline=shapefilepath,crop_to_cutline = TRUE, t_srs="EPSG:4326", ot="Int16") #Transform GTiff and crop to shapefile

    }
	
	# In case all Tiles of the current date are valid, mosaic them to one Gtiff File
    if (filesvalid) {
    filename=paste(HDFlist$dates[i],'.tif',sep='')
    dstfile <- file.path(dstfolder,filename)
    mosaic_rasters(GTifflist2,dstfile,co=compressionmethod, ot="Int16")  #
    outputdates=c(outputdates,as.character(HDFlist$dates[i]))
    outputfiles=c(outputfiles, dstfile)
    }
  }

  output=data.frame(outputfiles,outputdates)
  names(output)[1] <- 'file'
  names(output)[2] <- 'date'
  
  # Delete all temporary working data and HDF Files if argument hdfstorage is NULL (no persistent storage)
  do.call(unlink, list(tempfolder,recursive=TRUE))
  
  if (is.null(hdfstorage)) {
    delHdf("MOD13Q1",ask=FALSE)
    delHdf("MYD13Q1",ask=FALSE)
  }
  
  setwd(oldwd)
  return(output)
}

isPartOfExtent <- function(outside,inside) {
  outside <- round(outside,3) # to avoid error due to precision errors
  inside <- round(inside,3)
  left <- xmin(inside) >= xmin(outside)
  right <- xmax(inside) <= xmax(outside)
  top <- ymin(inside) >= ymin(outside)
  bottom <- ymax(inside) <= ymax(outside)
  return(all(c(left,right,top,bottom)))
}

cropFromGeotiff <- function(date, shapefilepath, srcfolder, dstfolder, compression=FALSE) {
  compressionmethod <- GenerateCompressionArgument(compression)
  
  if (!dir.exists(dstfolder)) {
    dir.create(dstfolder)
  }
  
  gtifffiles = list.files(srcfolder, pattern = "\\.tif$", full.names = TRUE)
  dates=as.Date(sub(".tif","",basename(gtifffiles)))
  availabledata = data.frame(gtifffiles, dates)
  
  availabledata <- availabledata[as.numeric(availabledata$dates) %in% as.numeric(seq(from=date[1],to=date[2],by=1)),]
  
  outputdates=c()
  outputfiles=c()
  
  if (nrow(availabledata)>0) {
    
    # Check if bounding box of destination is within bbox of source
    shp <- readOGR(shapefilepath, verbose=FALSE)
    r <- raster(as.character(availabledata$gtifffiles[1]))
    ispartof <- isPartOfExtent(extent(r),extent(shp))
    rm(r,shp)
    gc()
    if (ispartof) {
      for (i in 1:nrow(availabledata)) {
        srcfile <- availabledata$gtifffiles[i]
        dstfile <- file.path(dstfolder,paste(availabledata$dates[i],'.tif',sep=''))
        cat('Processing ... ',as.character(srcfile),'\n',sep='')
        gdalwarp(srcfile=srcfile,dstfile=dstfile,cutline=shapefilepath,crop_to_cutline = TRUE, t_srs="EPSG:4326",co=compressionmethod)
        outputdates=c(outputdates,as.character(availabledata$dates[i]))
        outputfiles=c(outputfiles, dstfile) 
      } 
    } else {
      warning(paste(shapefilepath,"is not within the bounding box of",availabledata$gtifffiles[1]),sep=" ")
    }
    return(outputfiles)
  }

  output=data.frame(file=outputfiles,date=outputdates)
  if (nrow(output)==0) {
    return(NULL)
  } else {
    return(output) 
  }
}

check_available_data <- function(datapath, timeseries_filename) {
  

  csv_file <- file.path(datapath,timeseries_filename)
  if (file.exists(csv_file)) {
    ts <- read.csv(csv_file)
    ts_dates <- as.data.frame(as.Date(ts$date))
  } else {
    ts_dates <- NULL
  }
  
  geotiff_list <- list.files(datapath, pattern="*.tif")
  if (length(geotiff_list>0)) {
    geotiff_dates=as.data.frame(as.Date(sub(".tif","",basename(geotiff_list))))
  } else {
    geotiff_dates <- NULL
  }
  
  
  return(list(TS = unname(ts_dates), GEOTIFF = unname(geotiff_dates)))
}

get_latest_observation <- function(datapath, geotiff=FALSE, timeseries_filename) {
  available_dates <- check_available_data(datapath, timeseries_filename)
  if (geotiff) {
    available_dates <- available_dates$GEOTIFF
  } else {
    available_dates <- available_dates$TS
  }
  
  if (is.null(available_dates)) {
    return(NULL)
  } else {
    return(as.Date(max(unlist(available_dates))))
  }
}

UpdateAndProcess <- function(database, storage_location, modis_datastorage=NULL, timeseries_filename = "timeseries.csv", max_download_chunk=15, geotiff_compression = TRUE) {
  
  # Check Storage Location for RAW Modis Data (or set up a temporary folder) and output files and stop, if they do not exist. 
  if (!isString(modis_datastorage)) {
    localArcPath <- file.path(tempdir(), "MODIS")
    dir.create(localArcPath)
    cat("Persistent Storage of MODIS RAW Data is turned off. Configurate MODIS_DATASTORAGE to a valid path to enable it.")
  } else if (dir.exists(modis_datastorage)) {
    localArcPath <- modis_datastorage
  } else {
    stop(paste("The path modis_datastorage=",modis_datastorage," does not exist",sep=""))
  }
  
  if (!dir.exists(storage_location)) {
    stop(paste("The path storage_location=",storage_location," does not exist",sep=""))
  }

  # Initialise MODIS package. 
  MODISoptions(MODISserverOrder="LAADS",quiet=TRUE,localArcPath=localArcPath,outDirPath=storage_location) 
  
  # Check and Rearrange database list. Order dataframe such that subregions come last.
  if (!is.logical(database$store_geotiff) || any(is.na(database$store_geotiff))) {
    stop("Invalid entry in the database for store_geotiff. Only logicals (TRUE/FALSE) are allowed.")
  } else if (!is.numeric(database$store_length) | !all(database$store_length>0, na.rm=TRUE)) {
    stop("Invalid entry in the database for store_length. Only numerics (1,2,3,... or NA) are allowed.")
  } else if (!is.logical(database$cloud_correct) || any(is.na(database$cloud_correct))) {
    stop("Invalid entry in the database for cloud_correct. Only logicals (TRUE/FALSE) are allowed.")
  } else if (any(duplicated(database$ID))) {
    stop("The ID entries in the given database are not unique")
  } else if (any(duplicated(database$name))) {
    stop("The name entries in the given database are not unique")
  } else if (!isDate(database$earliestdate)) {
    stop("The entries for earliestdate must be of format YYYY-MM-DD or NA (NA -> as far back in time as possibe)")
  } else if (!isDate(database$latestdate)) {
    stop("The entries for latestdate must be of format YYYY-MM-DD or NA (NA -> today")
  } else if (!all(database$is_subregion_of[isString(database$is_subregion_of)] %in% database$ID)) {
    stop("One or more entries for is_subregion_of do not have a correspondend entry for a parent region")
  } else if (!all(file.exists(as.character(database$shapefile)))) {
    stop("One or more of the shapefiles in the database do not exist. Make sure the pathname is correct")
  }
  
  database <- database[order(database$is_subregion_of, na.last =  FALSE),] 
  
  # Resolve nested subregion dependancy to the last parentregion and check for circular dependancy
  for (i in 1:nrow(database)) {
    parentregion=data.frame()
    subregion <- as.character(database$is_subregion_of[i])
    subregionlist <- c(subregion)
    while (isString(subregion)) {
      parentregion <- database[database$ID==subregion,]
      subregion <- as.character(parentregion$is_subregion_of)
      subregionlist <- c(subregionlist,subregion)
      if (any(duplicated(subregionlist))) {
        stop("There is a circular dependancy of subregions!")
      }
    }
    if (nrow(parentregion)==1) {
      database$is_subregion_of[i] <- as.character(parentregion$ID)
    }
  }
  
  # Find the latest observation for each database entry
  # Earliest date is either the entry in the database if no geotiffs or timeseries is found in the datapath. Otherwise the latest observation 
  # of the timeseries is taken (store_geotiff=FALSE) or the latest observation that exists as geotiff (store_geotiff=TRUE)
  # Latest date is either the entry latestdate in the database, but if it is NA, latest date is set to today (systemtime)
  df_dates <- data.frame()
  for (i in 1:nrow(database)) {
    name=as.character(database$name[i])
    datapath <- file.path(storage_location,name)
    date <- get_latest_observation(datapath, geotiff=database$store_geotiff[i], timeseries_filename=timeseries_filename)
    if (is.null(date)) {
      startdate <- as.Date(database$earliestdate[i])
    } else {
      startdate <- date+1
    }
    
    datevalue <- database$latestdate[i]
    if (is.na(datevalue)) {
      enddate <- Sys.Date()
    } else {
      enddate <- as.Date(database$latestdate[i])
    }
    df <- data.frame(ID = database$ID[i], startdate=startdate, enddate = enddate)
    df_dates <- rbind(df_dates,df)
  }
  rownames(df_dates) <- df_dates$ID
  
  # daterange from earliest to latest date of all database entries
  startdate <- min(df_dates$startdate)
  enddate <- max(df_dates$enddate)
  daterange = c(startdate,enddate)
  daterange_days = enddate-startdate
  
  # Split processing window into chunks if the daterange exceeds maxDOWNLOADchunk
  if (daterange_days>max_download_chunk) {
    chunks_startdate <- seq(startdate,enddate, by=max_download_chunk)
  } else {
    chunks_startdate <- daterange[1] 
  }
  downloadchunks <- data.frame(start=chunks_startdate, end=c(chunks_startdate[-1]-1,enddate))
  
  # Start Updating data: Loop over all downloadchunks resp. daterange pieces. After every chunk, delete temporary files to free harddisk space. 
  # Within the outer loop, loop over every entry of the database 
  for (j in 1:nrow(downloadchunks)) {
    removefinally <- c() # Empty character vector that collects temporary files, that are not required anymore
    for (i in 1:nrow(database)) {
      ID <- as.character(database$ID[i])
      
      # if startdate of database entry is within downloadchunk window, begin updating data
      # crop daterange if shapefiles startdate/enddate is later/earlier than startdate/enddate of downloadchunk
      if ((downloadchunks$end[j] >= df_dates[ID,"startdate"]) && (downloadchunks$start[j] <= df_dates[ID,"enddate"])) {
        if (df_dates[ID,"startdate"] > downloadchunks$start[j]) { 
          daterange[1] <- df_dates[ID,"startdate"]
        } else {
          daterange[1] <- downloadchunks$start[j]
        }
        
        if (df_dates[ID,"enddate"] < downloadchunks$end[j]) { 
          daterange[2] <- df_dates[ID,"enddate"]
        } else {
          daterange[2] <- downloadchunks$end[j]
        }
        
        # concentate datapath from entry name and storage location.
        # Create folder if does not yet exist
        name=as.character(database$name[i])
        datapath <- file.path(storage_location,name)
        if (!dir.exists(datapath)) {
          dir.create(datapath, recursive=TRUE)
        }
        
        # Now start downloading and processing new MODIS observations
        cat('\n','############### Data for ',name,' are being updated from ',as.character(daterange[1]),' to ',as.character(daterange[2]),' ... ###############','\n',sep='')
        
        shapefilepath <- as.character(database$shapefile[i])
        
        # fetch the entry of the parentregion if current entry is a subregion. 
        subregion <- as.character(database$is_subregion_of[i])
          
        # If the current entry is a a subregion, fetch data from parent region datapath. Otherwise access MODIS FTP via MODIS package
        if (isString(subregion)) {
          parentregion <- database[database$ID==subregion,]
          cat('... using data from PARENTREGION with ID ',as.character(parentregion$ID),' ...','\n',sep='')
          srcdatapath <- file.path(storage_location,as.character(parentregion$name))
          rasterimages <- cropFromGeotiff(date = daterange, shapefilepath = shapefilepath, srcfolder = srcdatapath, dstfolder = datapath, compression = geotiff_compression)
        } else {
          cat('... using data from MODIS FTP server ...','\n',sep='')
          rasterimages <- UpdateAndProcessMODIS(date = daterange, shapefilepath=shapefilepath, dstfolder=datapath, hdfstorage=modis_datastorage, compression=geotiff_compression) #Download&Process MODIS Data
        }
        
        # Add new observations to timeseries file and tag rasterimages, that are not anymore required.
        if (!is.null(rasterimages)) {
          # Get a list of all available rasterimages/geotiffs. Extract date vector from filenames and create dataframe with filename-date pairs.
          gtifffiles <- list.files(datapath, pattern = "\\.tif$", full.names = TRUE)
          dates=as.Date(sub(".tif","",basename(gtifffiles)))
          datainstorage = data.frame(file=gtifffiles, date=dates)
          
          if (database$cloud_correct[i]) {
            cat('Cloud Correcting Geotiff images ... ')
            newdata = rasterimages[order(rasterimages$date, decreasing=FALSE),]
            olddata<-datainstorage[!(datainstorage$file %in% newdata$file),]
            if (nrow(olddata)>0) {
              olddata = olddata[order(olddata$date, decreasing=TRUE),]
              old_r <- raster(as.character(olddata$file[1]))
            } else {
              old_r <- raster(as.character(newdata$file[1]))
              newdata <- newdata[-1,]
            }
            
            old_r[old_r<0]<-NA
            
            for (imagefile in newdata$file) {
              r <- raster(imagefile)
              r[r<0]=NA
              r <- merge(r,old_r)
              old_r<-r
              remove(r);gc()
              writeRaster(old_r,filename=imagefile,format="GTiff",overwrite=TRUE, dataType="INT1S")
            }
          }
          
          
          cat('Updating Time Series for ',name,' ...','\n',sep='')
          # Read any existing timeseries file, othwerwise create empty dataframe. Extract the dates from datainstorage, for which no entry in the timeseries exists.
          csvpath <- file.path(datapath,timeseries_filename)
          if (file.exists(csvpath)) {
            ts <- read.csv(csvpath,stringsAsFactors = FALSE, header = TRUE)
            newtsdata <- datainstorage[!as.Date(datainstorage$date) %in% as.Date(ts$date),]
          } else {
            ts <- data.frame()
            newtsdata <- datainstorage
          }
          
          # if new data for timeseries are available, read the corresponding rasterimages and add value to the timeseries.
          if (nrow(newtsdata)>0) {
            values <- vector(mode="numeric", length=length(newtsdata[,1]))
            dates <- vector(mode='character',length=length(newtsdata[,1]))
            for (k in 1:length(values)) {
              r <- raster(as.character(newtsdata$file[k]))
              r[r<0]=NA
              values[k]=mean(values(r), na.rm=TRUE)
              dates[k]=as.character(newtsdata$date[k])
            }
            ts <- rbind(ts,data.frame(date=dates,value=values))
            ts = ts[order(ts$date, decreasing=FALSE),]
            write.csv(ts,file=csvpath,row.names=FALSE) 
          }
          
          # Add rasterimages that shall not be stored and are thus no longer required after the current download/daterangechunk to the vector removefinally.
          # If cloud correct is activated, keep the most recent rasterimage, even if store_geotiff is set to FALSE
          if (!database$store_geotiff[i]) {
            if (database$cloud_correct[i]) {
              datainstorage2remove <- datainstorage[!datainstorage$date %in% max(datainstorage$date),] #Excludes the most recent files
              removefinally <- c(removefinally,as.character(datainstorage2remove$file))  
            } else {
              removefinally <- c(removefinally,list.files(datapath, pattern = "\\.tif$", full.names = TRUE))
            }
          } else {
            if (!is.na(database$store_length[i]) && is.numeric(database$store_length[i]) && database$store_length[i]>0) {
              alldates <- datainstorage$date
              alldates <- alldates[order(alldates,decreasing=TRUE)]
              dates2keep <- alldates[1:round(database$store_length[i])] 
              datainstorage2remove <- datainstorage[!datainstorage$date %in% dates2keep,]
              removefinally <- c(removefinally,as.character(datainstorage2remove$file)) 
            } 
          }
        } else {
          cat('No new data were found for ',name,'\n',sep='')
        }
        
      }
    }
    if (length(removefinally)>0) {
      file.remove(removefinally) 
    }
  }
}

# Read list of shapefiles
database <- database <- read.csv(DATABASE_LOC, comment.char='#', stringsAsFactors = TRUE, colClasses = c("character","character","character","character","logical","numeric","logical","character","character"), na.strings = "NA")

UpdateAndProcess(database, storage_location=DATASTORAGE_LOC, modis_datastorage = MODIS_DATASTORAGE, max_download_chunk = maxDOWNLOADchunk)
  
  

