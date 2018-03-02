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
        
        # If the current entry is a a subregion, fetch data from parent region datapath. Otherwise access Online Dataserver
        if (isString(subregion)) {
          parentregion <- database[database$ID==subregion,]
          cat('... using data from PARENTREGION with ID ',as.character(parentregion$ID),' ...','\n',sep='')
          srcdatapath <- file.path(storage_location,as.character(parentregion$name))
          rasterimages <- CropFromGeotiff(date = daterange, shapefilepath = shapefilepath, srcfolder = srcdatapath, dstfolder = datapath, geotiff_compression = geotiff_compression)
        } else {
          cat('... using data from WEBSERVER ...','\n',sep='')
          rasterimages <- ProcessMODIS_10A1(date = daterange, shapefilepath=shapefilepath, dstfolder=datapath, srcstorage=srcstorage, geotiff_compression=geotiff_compression) #Download&Process MODIS Data
        }
        
        if (nrow(rasterimages)==0) {
          cat('No new data were found for ',name,'\n',sep='')
        } else {
          cat('POST-PROCESSING of new data for ',name,' ...','\n',sep='')
          -
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
        } 
        
      }
    }
    # Delete temporary files after one DownloadChunk
    if (length(removefinally)>0) {
      file.remove(removefinally) 
    }
  }
}

# These functions do the downloading&processing. They output a data.frame with 2 rows: file -> absolute path to geotiff, date -> date of tthe geotiff observation
# ProcessMODIS_10A1 accesses the NSDIC HTTPS Server
# ProcessMODIS_13Q1 acesses the LP DAAC and LAADS FTP Server
# They can be adapted to work with other MODIS Products on those servers, but it requires some effort.
ProcessMODIS_10A1 <- function(daterange, shapefilepath, dstfolder, srcstorage=NULL, geotiff_compression=TRUE){
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
  
  DownloadFromNSIDC <- function(product, collection="006", datapath, daterange, tileH, tileV, max_wait=300, checkIntegrity=FALSE) {
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
    
    # Helper function to find local files
    listHDFfiles <- function(product, datapath,tileH=NULL,tileV=NULL,begin=NULL,end=NULL) {
      # Searches a local folder for MODIS .hdf files within the specifications for product, tile and daterange
      #
      # Args:
      #   product: any MODIS product abbreviation as character, e.g. "MOD10A1" or "MYD10A1"
      #   tileH,tileV: The path where to search for files.
      #   tileH, tileV: A vector with the tile numbers which should be downloaded. Each element in tileH corresponds to the element at the same index in tileV. 
      #                 Both vectors must be of the same length. If NULL, any tiles will be selected.
      #   begin,end: the begin or end date within which files should be selected, e.g. as.Date(2017-01-01). If NULL, all files of any date will be selected.
      #
      # Returns:
      #  data.frame with rows (path, file, date) of all files that have been found
      
      # Concentate search pattern
      tileHpattern <- paste(sprintf("%02d", tileH),collapse="|")
      tileVpattern <- paste(sprintf("%02d", tileV),collapse="|")
      pattern <- paste(product,".*h(",tileHpattern,")v(",tileVpattern,").*","\\.hdf$",sep="")
      
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
      dir.create(datapath)
    } else if (checkIntegrity) {
      removeCorruptHDF(datapath, product=product, max.deletions=Inf)
    }
    tempfile <- tempfile()
    
    # input preparations
    begin <- daterange[1]
    end <- daterange[2]
    
    if (!length(tileH)==length(tileV)) {
      stop("Argument tileH and tileV must be of the same length.")
    }
    
    # get a list of available local files
    localfiles <- listHDFfiles(product=product,datapath,tileH=tileH,tileV=tileV)
    
    # get available online files. Start at webserver root and fetch all links from index.html.
    # Webserver structure is: root/YYYY.MM.DD/****.hdf
    cat("Browsing the webserver file structure for new files in ",baselink,"\n")
    tileHpattern <- paste(sprintf("%02d", tileH),collapse="|")
    tileVpattern <- paste(sprintf("%02d", tileV),collapse="|")
    pattern <- paste(product,".*h(",tileHpattern,")v(",tileVpattern,").*","\\.hdf$",sep="")
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
          dir.create(savepath)
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
    availablefile <- listHDFfiles(product=product,datapath,tileH=tileH,tileV=tileV, begin=begin, end=end)
    return(as.vector(availablefile$path))
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
    dir.create(dstfolder)
  }
  
  output=data.frame(file=c(),date=c())
  
  # Get vector of required tiles for the specified shapefile
  myshp <- readOGR(shapefilepath, verbose=FALSE)
  e <- extent(myshp)
  tile <- getTile(e)
  
  # Try MODIS Aqua first
  collection="006"
  x='MYD10A1'
  out1=tryCatch(DownloadFromNSIDC(product=x,datapath=localArcPath,daterange=daterange,tileH=tile@tileH,tileV=tile@tileV), error = function(e) {NULL})
  
  # Then MODIS Terra
  x='MOD10A1'
  out2=tryCatch(DownloadFromNSIDC(product=x,collection=collection,datapath=localArcPath,daterange=daterange,tileH=tile@tileH,tileV=tile@tileV), error = function(e) {NULL})
  
  # Merge lists of downloaded HDF Tiles
  files <- c(out1,out2)
  
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
        GTifflist2[i]=tempfile(tmpdir=tempfolder,fileext = ".tif")
        
        validHDF <- isvalidHDF_4_5(HDFfile$file)
        if (!validHDF) {
          unlink(as.character(HDFfile$file))
          cat('The downloaded file has not been recognized as a valid HDF4/5 file. Try again or manually download the following file and check its validity: ', as.character(HDFfile$files),'\n',sep='')
          filesvalid=FALSE
          setwd(oldwd)
          break
        }
      
        # GEOTIFF PROCESSING: 
        sds<-getSds(as.character(HDFfile$file))
        mask<-raster(readGDAL(sds$SDS4gdal[2], as.is=TRUE, silent=TRUE))
        if (j>1) {
          evi2 <- evi
        } 
        evi<-raster(readGDAL(sds$SDS4gdal[1], as.is=TRUE, silent=TRUE))
        
        evi[mask > 2 | is.na(evi) | evi>100]<- NA
        evi[evi>0] <- 0.06+1.21*evi[evi>0] # https://www.sciencedirect.com/science/article/pii/S0034425703002864
        evi[evi>100] <- 100
        
        rm(mask);gc()
        if (j>1) {
          evi<-mosaic(evi,evi2,fun=mean) #mean of both rasters, na.rm=TRUE
        }
      }
      
      writeRaster(evi,filename=GTifflist[i],format="GTiff",overwrite=TRUE, dataType="INT1S")
      rm(evi);gc()
      gdalwarp(srcfile=GTifflist[i],dstfile=GTifflist2[i],cutline=shapefilepath,crop_to_cutline = TRUE, t_srs="EPSG:4326", ot="Int16",dstnodata=-32768) #Transform GTiff and crop to shapefile
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

ProcessMODIS_13Q1 <- function(daterange, shapefilepath, dstfolder, srcstorage=NULL, geotiff_compression=TRUE){
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

CropFromGeotiff <- function(daterange, shapefilepath, srcfolder, dstfolder, geotiff_compression=FALSE) {
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
  availabledata = data.frame(gtifffiles, dates)
  
  availabledata <- availabledata[as.numeric(availabledata$dates) %in% as.numeric(seq(from=daterange[1],to=daterange[2],by=1)),]
  
  output <- data.frame(file=c(),date = c())
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
        output <- rbind(output,data.frame(file=dstfile,date=as.Date(as.character(availabledata$dates[i]))))
      } 
    } else {
      warning(paste(shapefilepath,"is not within the bounding box of",availabledata$gtifffiles[1]),sep=" ")
    }
    return(output)
  }
  
  output=data.frame(file=outputfiles,date=outputdates)
  if (nrow(output)==0) {
    return(NULL)
  } else {
    return(output) 
  }
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
  
  

