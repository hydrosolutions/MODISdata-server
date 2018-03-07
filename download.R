#!/usr/bin/env Rscript

########### 0.HEADER ##############
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
        stop(paste(e,"\nThe file ",args," can not be sourced in R"))
      })
    }
  }
}  

source(GEOTIFF_PROCESSOR)

########### 1.FUNCTION DEFINITIONS ##############
# The main function which organises and triggers the downloading and processing for all entries in the database
UpdateData <- function(database, storage_location, srcstorage=NULL, geotiff_processor,timeseries_filename = "timeseries.csv", max_download_chunk=15, geotiff_compression = TRUE) {
  # Updates and Processes the MODIS Data for all entries listed in database.
  #
  # Args:
  #   database: a .csv file containing a list of region, for which MODIS data shall be updated. For a sample file and description see example_database.csv.
  #   storage_location: The path, where prior existing data are stored and where the output of the function will go.
  #   srcstorage: The path, where the downlaoded RAW MODIS .hdf files are kept. NULL if no persistent storage is required. hdf files will be deleted after every downloadchunk.
  #   timeseries_filename: The filename of the csv file where the timeseries data is written to. Default: timeseries.csv.
  #   max_download_chunk: The number of days for one updating&processing step. During this time, all temporary files (hdf & geotiff) are kept in order to avoid multiple download of the same file.
  #                       Default is 15 days. If harddisk storage is critical, the value should be lower. If a path is given for argument modis_datastorage, max_download_chunk is not critical.
  #   geotiff_compression: Default: TRUE if output geotiff should be compressed (lossless,deflate level=9). FALSE for no compression.
  #
  # Returns:
  #   Nothing, but status messages on the console
  
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
  
  isString <- function(value) {
    # Can value be read as a string or is it NA,NULL or empty?
    #
    # Args:
    #   value: the data object or a vector of data objects to be checked
    # Returns:
    #   a vector of TRUE or FALSE
    
    sapply(value, FUN = function(x) {
      if (is.na(x) || is.null(x) || x == "") {
        return(FALSE)
      } else {
        return(TRUE)
      }
    })
  }
  
  isDate <- function(value) {
    # Can value be converted to a Date object?
    #
    # Args:
    #   value: the data object or a vector of data objects to be read as date
    # Returns:
    #   a vector of TRUE or FALSE
    
    sapply(value, FUN = function(x) {
      tryCatch({as.Date(x);TRUE}, error = function(e) {FALSE})
    })
  }
  
  
  # Check argument modis_datastorage (or set up a temporary folder) and storage_location and stop, if they do not exist. 
  if (!isString(srcstorage)) {
    localArcPath <- file.path(tempdir(), "MODIS")
    dir.create(localArcPath)
    cat("Persistent Storage of MODIS RAW Data is turned off. Configurate MODIS_DATASTORAGE to a valid path to enable it.")
  } else if (dir.exists(srcstorage)) {
    localArcPath <- srcstorage
  } else {
    stop(paste("The path modis_datastorage=",srcstorage," does not exist",sep=""))
  }
  
  if (!dir.exists(storage_location)) {
    stop(paste("The path storage_location=",storage_location," does not exist",sep=""))
  }
  
  # Initialise MODIS package. 
  # MODISoptions(MODISserverOrder="LAADS",quiet=TRUE,localArcPath=localArcPath,outDirPath=storage_location) 
  
  # Check and Rearrange database list. Order dataframe such that subregions come last.
  if (!is.logical(database$store_geotiff) || any(is.na(database$store_geotiff))) {
    stop("Invalid entry in the database for store_geotiff. Only logicals (TRUE/FALSE) are allowed.")
  } else if (!(is.numeric(database$store_length) | is.na(database$store_length)) | !all(database$store_length>0, na.rm=TRUE)) {
    stop("Invalid entry in the database for store_length. Only numerics (1,2,3,... or NA) are allowed.")
  } else if (!is.logical(database$cloud_correct) || any(is.na(database$cloud_correct))) {
    stop("Invalid entry in the database for cloud_correct. Only logicals (TRUE/FALSE) are allowed.")
  } else if (!is.logical(database$crop_to_cutline) || any(is.na(database$crop_to_cutline))) {
    stop("Invalid entry in the database for crop_to_cutline Only logicals (TRUE/FALSE) are allowed.")
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
        cropoption <- database$crop_to_cutline[i]
        # If the current entry is a a subregion, fetch data from parent region datapath. Otherwise access Online Dataserver
        if (isString(subregion)) {
          parentregion <- database[database$ID==subregion,]
          cat('... using data from PARENTREGION with ID ',as.character(parentregion$ID),' ...','\n',sep='')
          srcdatapath <- file.path(storage_location,as.character(parentregion$name))
          rasterimages <- CropFromGeotiff(daterange = daterange, shapefilepath = shapefilepath, srcfolder = srcdatapath, crop_to_cutline = cropoption, dstfolder = datapath, geotiff_compression = geotiff_compression)
        } else {
          cat('... using data from WEBSERVER ...','\n',sep='')
          rasterimages <- Raw2Geotiff(daterange = daterange, shapefilepath=shapefilepath, dstfolder=datapath, srcstorage=srcstorage, crop_to_cutline = ,geotiff_compression=geotiff_compression) #Download&Process MODIS Data
        }
        
        if (nrow(rasterimages)==0) {
          cat('No new data were found for ',name,'\n',sep='')
        } else {
          cat('POST-PROCESSING of new data for ',name,' ...','\n',sep='')
            # Get a list of all available rasterimages/geotiffs. Extract date vector from filenames and create dataframe with filename-date pairs.
            gtifffiles <- list.files(datapath, pattern = "\\.tif$", full.names = TRUE)
            dates=as.Date(sub(".tif","",basename(gtifffiles)))
            datainstorage = data.frame(file=gtifffiles, date=dates)
            
            #if cloud_correct=TRUE; use last observation to fill in cloud gaps in the new observations.
            if (database$cloud_correct[i]) {
              cat('Cloud Correcting Geotiff images ... \n')
              newdata = rasterimages[order(rasterimages$date, decreasing=FALSE),]
              olddata<-datainstorage[!(datainstorage$file %in% newdata$file),]
              if (nrow(olddata)>0) {
                olddata = olddata[order(olddata$date, decreasing=TRUE),]
                old_r <- raster(as.character(olddata$file[1]))
              } else {
                old_r <- raster(as.character(newdata$file[1]))
                newdata <- newdata[-1,]
              }
              
              old_r[old_r==-32767]<-NA
              
              for (imagefile in newdata$file) {
                r <- raster(imagefile)
                mask <- (is.na(r))
                r[r==-32767]=NA
                r <- merge(r,old_r)
                r[is.na(r)]<--32767
                r[mask]=NA
                old_r<-r
                remove(r);gc()
                writeRaster(old_r,filename=imagefile,format="GTiff",overwrite=TRUE, dataType="INT2S")
              }
            }
            
            # Update timeseries.csv file with new observations
            # Read any existing timeseries file, othwerwise create empty dataframe. Extract the dates from datainstorage, for which no entry in the timeseries exists.
            cat('Updating Time Series ... \n')
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
                r[r==-32766 | r==-32767]=NA  #remove cloud, water mask
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
        } 
        
      }
    }
    # Delete temporary files after one DownloadChunk
    if (length(removefinally)>0) {
      file.remove(removefinally) 
    }
  }
}

CropFromGeotiff <- function(daterange, shapefilepath, srcfolder, dstfolder, crop_to_cutline=TRUE, geotiff_compression=FALSE) {
  # Uses existing geotiff data files to produce the geotiffs for the specified shapefile
  #
  # Args:
  #   daterange: a vector defining the start- and enddate for which MODIS data shall be generated. e.g. daterange=c(as.Date(2018-01-01),today())
  #   shapefilepath: The path to a shapefile that contains one polygon, defining the area of interest
  #   scrfolder: the path to the folder that contains the geotiffs that are used as data source. Naming convention is: yyyy-mm-dd.tif
  #   dstfolder: the path to the folder where the newly generated data should be stored.
  #   geotiff_compression: Default: TRUE if output geotiff should be compressed (lossless,deflate level=9). FALSE for no compression.
  #
  # Returns:
  #   data.frame(file=c(character()),date=c(date())): A dataframe that contains the full filename and date of the newly generated geotiff files.
  
  # Helper function to compare to Extent objects
  isPartOfExtent <- function(outside,inside) {
    outside <- round(outside,3) # to avoid error due to precision errors
    inside <- round(inside,3)
    left <- xmin(inside) >= xmin(outside)
    right <- xmax(inside) <= xmax(outside)
    top <- ymin(inside) >= ymin(outside)
    bottom <- ymax(inside) <= ymax(outside)
    return(all(c(left,right,top,bottom)))
  }
  
  compressionmethod <- GenerateCompressionArgument(geotiff_compression)
  
  if (!dir.exists(dstfolder)) {
    dir.create(dstfolder)
  }
  
  gtifffiles = list.files(srcfolder, pattern = "\\.tif$", full.names = TRUE)
  dates=as.Date(sub(".tif","",basename(gtifffiles)))
  availabledata = data.frame(file=gtifffiles, date=dates)
  
  availabledata <- availabledata[as.numeric(availabledata$date) %in% as.numeric(seq(from=daterange[1],to=daterange[2],by=1)),]
  
  output <- data.frame(file=c(),date = c())
  
  if (nrow(availabledata)>0) {
    
    # Check if bounding box of destination is within bbox of source
    shp <- readOGR(shapefilepath, verbose=FALSE)
    r <- raster(as.character(availabledata$file[1]))
    ispartof <- isPartOfExtent(extent(r),extent(shp))
    rm(r,shp)
    gc()
    if (ispartof) {
      for (i in 1:nrow(availabledata)) {
        srcfile <- availabledata$file[i]
        dstfile <- file.path(dstfolder,paste(availabledata$date[i],'.tif',sep=''))
        cat('Processing ... ',as.character(srcfile),'\n',sep='')
        gdalwarp(srcfile=srcfile,dstfile=dstfile,cutline=shapefilepath,crop_to_cutline = crop_to_cutline, t_srs="EPSG:4326",co=compressionmethod)
        output <- rbind(output,data.frame(file=dstfile,date=as.Date(as.character(availabledata$date[i]))))
      } 
    } else {
      warning(paste(shapefilepath,"is not within the bounding box of",availabledata$file[1]),sep=" ")
    }
    return(output)
  }
  
  return(output) 
  
}

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


########### 2.MAIN SCRIPT ##############
# Read list of shapefiles
database <- read.csv(DATABASE_LOC, comment.char='#', stringsAsFactors = TRUE)
UpdateData(database, storage_location=DATASTORAGE_LOC, srcstorage = MODIS_DATASTORAGE, geotiff_processor=GEOTIFF_PROCESSOR, max_download_chunk = maxDOWNLOADchunk)
  
  

