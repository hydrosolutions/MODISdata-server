#!/usr/bin/env Rscript

########### 0.HEADER ##############
# Check if Script is run from RStudio and load the default configuration file given below
# Else receive command line argument for "path to configuration file"
cmd = TRUE
try({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  source('/srv/modis-snow-server/config.R')
  cmd = FALSE
  }, silent = TRUE)

# Check validity of args
limit_past_days = NA
if (cmd) {
  args = commandArgs(trailingOnly=TRUE)
  if (length(args)<1) {
    stop("Argument for configuration file is missing, try: Rscript download.R path/config.R")
  } else if (length(args) == 2) {
    limit_past_days = as.numeric(args[2])
  } else if (length(args)>2) {
    stop("Too many arguments, try: Rscript download.R path/config.R")
  }
    
  if (!file.exists(args[1])) {
    stop(paste("The file ",args[1]," does not exist."))
  } else {
    tryCatch({
      source(args[1])
    }, error = function(e) {
      stop(paste(e,"\nThe file ",args[1]," can not be sourced in R"))
    })
  }
}  

# Cleanup log files that are older than 14 days
oldlogfiles <- list.files(LOGFILE_LOC,pattern="*.log", full.names = TRUE)
for (file in oldlogfiles) {
  log_datetime <- file.info(file)$mtime
  if (difftime(Sys.time(),log_datetime,units="days") > 14) {
    file.remove(file)
  }
}

# Create new log file for current process
newlogfile <- file.path(LOGFILE_LOC,paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),".log",sep=""))
if (!file.create(newlogfile)) {stop("Processing has been terminated. Check write access on logfile location.")}
# Divert console output and messages to lockfile
sink(file=newlogfile,split=TRUE,append = TRUE, type=c('output','message'))

source(GEOTIFF_PROCESSOR)
library(elevatr)
library(RSQLite)
library(geojsonio)

########### 1.FUNCTION DEFINITIONS ##############
# The main function which organises and triggers the downloading and processing for all entries in the database
UpdateData <- function(db, storage_location, srcstorage=NULL, geotiff_processor, max_download_chunk=3, geotiff_compression = TRUE, update_days_limit = NA) {
  # Updates and Processes the MODIS Data for all entries listed in database.
  #
  # Args:
  #   db: The filepath to to a valid SQLite database containing the tables settings, geotiff and timeseries
  #   storage_location: The path, where the produced data are going to be stored.
  #   srcstorage: The path, where the downlaoded RAW MODIS .hdf files are kept. NULL if no persistent storage is required. hdf files will be deleted after every step resp. downloadchunk.
  #   max_download_chunk: The number of days for one updating&processing step. During this time, all temporary files (hdf & geotiff) are kept in order to avoid multiple download of the same file.
  #                       Default is 3 days. If harddisk storage is critical, the value should be lower. If a path is given for argument srcstorage, max_download_chunk is not critical, because the files will anyway be stored.
  #   geotiff_compression: Default: TRUE if output geotiff should be compressed (lossless,deflate level=9). FALSE for no compression.
  #   update_days_limit: Default NA (no limit). 
  #           A positive integer n: execution date - n days gives the minimum date a catchment must be already updated to in order to be updated further
  #           A negative integer n: execution date - n days gives the maximum date a catchment can be already updated to in order to be updated further 
  #
  # Returns:
  #   Nothing, but status messages on the console

  # SECTION 1: HELPER FUNCTION DEFINITIONS
  isDate <- function(value) {
    # Can value be converted to a Date object?
    #
    # Args:
    #   value: the data object or a vector of data objects to be read as date
    # Returns:
    #   a vector of TRUE or FALSE
    
    sapply(value, USE.NAMES = FALSE, FUN = function(x) {
      tryCatch({as.Date(x);TRUE}, error = function(e) {FALSE})
    })
  }
  
  get_DEM <- function(r, savepath=NULL, start_z=9) {
    # Downloads and stores a DEM with the same extent and resolution as r
    #
    # Args:
    #   r: a raster object, e.g. loaded with raster("file.tif")
    #   savepath: the filepath to whcih the produced DEM shall be stored. NULL for no storage.
    #   start_z: Integer between 0 and 19. Gives the intial zoom level of the raster to be downlaoded. If it is to large, 
    #            the resulting DEM might not fit into memory and the code will automatically try a lower zoom level until the DEM can be loaded.
    # Returns:
    #   a raster object
    
    z = start_z
    while(!exists("dem") & z>=0) {
      try(dem <- get_elev_raster(locations=r,z=z,src="aws")) 
      gc()
      z = z-1
    }
    if (!exists("dem")) {
      stop(sprintf('The catchment with ID = %s is too large. The resulting DEM does not fit into memory, even at zoom level %s',ID,z+1))
    }
    dem_resampled <- raster::resample(dem, r)
    rm(r,dem); gc()
    if (!is.null(savepath)) {
      writeRaster(dem_resampled, savepath)
    }
    return(dem_resampled)
  }
  
  cloud_correct <- function(images,maskvalue=NA,geotiffcompression=TRUE) {
    # Takes a list of rasterimages and replaces pixels with value=maskvalue with the pixel value of the preceeding image in the list.
    #
    # Args:
    #   images: A list of sorted filenames. The image at i+1 will use the values of the image at i. The image at i=1 will not be altered (it has no preceeding image)
    #           IMPORTANT: The given filenames will be overwritten with the corrected images.
    #   maskvalue: The pixel value that will be corrected with the pixel value from the preceeding image.
    #   geotiffcompression: disables/enables geotiff compression
    # Returns:
    #   nothin, apart from console messages.
    cat('... Cloud Correcting Geotiff images ')

    old_r <- raster(as.character(images[1]))
    newimages <- images[-1]
    
    old_r[old_r==maskvalue]<-NA 
    
    for (a in seq(length=length(newimages))) {
      cat('..',a,sep="")
      imagefile <- as.character(newimages[a])
      r <- raster(imagefile)
      mask <- (is.na(r))
      r[r==maskvalue]=NA
      r <- merge(r,old_r)
      r[is.na(r)]<-maskvalue
      r[mask]=NA
      old_r<-r
      remove(r);gc()
      writeRaster(old_r,filename=imagefile,format="GTiff",overwrite=TRUE, datatype="INT2S",options=GenerateCompressionArgument(TRUE))
    }
    rm(old_r,mask);gc()
    cat('..Done! \n')
  }
  
  # SECTION 2: MAIN FUNCTION DEFINITION
  starttime <- Sys.time()
  
  if (!dir.exists(storage_location)) {
    stop(paste("The path storage_location=",storage_location," does not exist",sep=""))
  }
  
  # Check argument modis_datastorage (or set up a temporary folder) and storage_location and stop, if they do not exist. 
  if (is.null(srcstorage)) {
    localArcPath <- file.path(tempdir(), "MODIS")
    dir.create(localArcPath, recursive = TRUE)
    cat("! Persistent Storage of MODIS RAW Data is turned off. Configurate MODIS_DATASTORAGE to a valid path to enable it.")
  } else if (dir.exists(srcstorage)) {
    localArcPath <- srcstorage
  } else {
    stop(paste("The path modis_datastorage=",srcstorage," does not exist",sep=""))
  }
  
  # Connect to the database
  db <- dbConnect(drv = RSQLite::SQLite(), dbname=DATABASE_LOC)
  on.exit({
    cat("\nProcessing DONE .... \n")
    if (exists('db')) {
      if (exists('toBeUpdated')) {
        for (ID in toBeUpdated$ID) {
          query <- dbSendStatement(db, sprintf("UPDATE settings SET locked=0 WHERE ID=%s",ID))
          dbClearResult(query)
        }
      }
      dbDisconnect(db)}
    }, add=TRUE)
  
  # Search for entries in table settings that are tagged for deletion.
  # Delete all information for those entries in the database as well as all related files.
  df_delete <- dbGetQuery(db, "SELECT * FROM settings WHERE deletion=1 AND locked=0")
  query <- dbSendStatement(db, "DELETE FROM settings WHERE deletion=1 AND locked=0")
  dbClearResult(query)
  for (i in seq(length=nrow(df_delete))) {
    ID <- as.character(df_delete$ID[i])
    query <- dbSendStatement(db, sprintf("DELETE FROM geotiffs WHERE catchmentid=%s",ID))
    dbClearResult(query)
    query <- dbSendStatement(db, sprintf("DELETE FROM timeseries WHERE catchmentid=%s",ID))
    dbClearResult(query)
    datapath <- file.path(storage_location,ID)
    unlink(datapath, recursive = TRUE)
  }
   
  if (!is.na(update_days_limit) && update_days_limit > 0) {
    minDate2Update = Sys.time()-update_days_limit*24*3600
    toBeUpdated = dbGetQuery(db, sprintf("SELECT ID FROM settings WHERE locked=0 AND last_obs_gtif>\"%s\"",as.character(minDate2Update,"%Y-%m-%d")))
    query <- dbSendStatement(db, sprintf("UPDATE settings SET locked=1 WHERE locked=0 AND last_obs_gtif>\"%s\"",as.character(minDate2Update,"%Y-%m-%d")))
    dbClearResult(query)
    cat("\n! The data update is limited to catchments that are newer than ",as.character(minDate2Update), sep="")
  } else if (!is.na(update_days_limit) && update_days_limit < 0) {
    maxDate2Update = Sys.time()+update_days_limit*24*3600
    toBeUpdated = dbGetQuery(db, sprintf("SELECT ID FROM settings WHERE locked=0 AND (last_obs_gtif IS NULL OR last_obs_gtif<=\"%s\")",as.character(maxDate2Update,"%Y-%m-%d")))
    query <- dbSendStatement(db, sprintf("UPDATE settings SET locked=1 WHERE locked=0 AND (last_obs_gtif IS NULL OR last_obs_gtif<=\"%s\")",as.character(maxDate2Update,"%Y-%m-%d")))
    dbClearResult(query)
    cat("\n! The data update is limited to catchments that are older than ",as.character(maxDate2Update), sep="")
  } else {
    toBeUpdated = dbGetQuery(db, "SELECT ID FROM settings WHERE locked=0")
    query <- dbSendStatement(db, "UPDATE settings SET locked=1 WHERE locked=0")
    dbClearResult(query)
  }
  
  # freeze database at this moment of time. 
  db_frozen = dbReadTable(db,"settings")
  row.names(db_frozen)<-db_frozen$ID
  
  index2Update = match(toBeUpdated$ID,db_frozen$ID)
  index2Update = index2Update[!is.na(index2Update)]

  # Find the daterange of each entry in table settings for which new data might exists, either for geotiffs or timeseries or both.
  df_dates <- data.frame()
  for (i in index2Update) {
    ID <- db_frozen$ID[i]
    # Limit latestdate,earliestdate with the settings of the masterregion
    max_obs <- min(as.Date(db_frozen$latestdate[i]),as.Date(db_frozen$latestdate[1]),na.rm=TRUE)
    min_obs <- max(as.Date(db_frozen$earliestdate[i]),as.Date(db_frozen$earliestdate[1]),na.rm=TRUE)-1
    ts <- db_frozen$timeseries[i]
    gtif <- db_frozen$store_geotiff[i]

    if (gtif) {
      last_obs_gtif <- as.Date(db_frozen$last_obs_gtif[i])
      last_obs_gtif <- if (is.na(last_obs_gtif)) min_obs else last_obs_gtif
    } else {
      last_obs_gtif <- Inf
    }
    
    if (ts) {
      last_obs_ts <- as.Date(db_frozen$last_obs_ts[i])
      last_obs_ts <- if (is.na(last_obs_ts)) min_obs else last_obs_ts
    } else {
      last_obs_ts <- Inf
    }
    
    startdate <- min(last_obs_gtif,last_obs_ts)+1
    enddate <- if (is.infinite(max_obs)) today() else max_obs
    
    df <- data.frame(ID = db_frozen$ID[i], startdate=startdate, enddate = enddate)
    df_dates <- rbind(df_dates,df)
  }
  
  if (length(df_dates)>0) {
    rownames(df_dates) <- df_dates$ID
    
    # daterange from earliest to latest date of all database entries.
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
  } else {
    downloadchunks = data.frame()
  }
  
  # Start Updating data: Loop over all downloadchunks resp. daterange pieces. After every chunk, delete temporary files to free harddisk space. 
  # Within the downloadchunk loop, loop over every entry of the database 
  for (j in seq(length=nrow(downloadchunks))) {
    removefinally <- c() # Empty character vector that collects temporary files, that are not required anymore after each downloadchunk
    for (i in index2Update) {
      ID <- as.character(db_frozen$ID[i])
      name=as.character(db_frozen$name[i])
      
      # if startdate of database entry is within downloadchunk window, begin updating data
      # crop daterange if current catchments startdate/enddate is later/earlier than startdate/enddate of downloadchunk
      daterange[1] <- max(df_dates[ID,"startdate"],downloadchunks$start[j])
      daterange[2] <- min(df_dates[ID,"enddate"],downloadchunks$end[j])
      if (daterange[2]<daterange[1]) {
        cat(difftime(Sys.time(),starttime, units="mins"),' : Data is already up-to-date for ',name,' until ',as.character(daterange[2]),' ... ','\n',sep='')
      } else { 
        # concentate datapath from entry name and storage location.
        # Create folder if does not yet exist
        datapath <- file.path(storage_location,ID)
        if (!dir.exists(datapath)) {
          dir.create(datapath, recursive=TRUE)
        }
        
        timediff <- round(difftime(Sys.time(),starttime, units="mins"),2)
        # Now start downloading and processing new MODIS observations
        cat("\n### ",timediff,' mins : Data for ',name,' are being updated from ',as.character(daterange[1]),' to ',as.character(daterange[2]),' ... ','\n',sep='')
        
        # Load geojson for this catchment into temporary file
        shapefilepath <- tempfile(fileext = ".geojson")
        capture.output(spatial_obj <- geojson_sp(as.json(db_frozen$geojson[i])),file=NULL)
        capture.output(capture.output(geojson_write(spatial_obj,file=shapefilepath),file=NULL,type="message"),file=NULL,type="output")
        removefinally <- c(removefinally,shapefilepath)
        
        # fetch the entry of the parentregion if current entry is a subregion. 
        subregion <- as.character(db_frozen$is_subregion_of[i])
        # If the current entry is a a subregion, fetch data from parent region datapath. Otherwise use online data
        if (!is.na(subregion)) {
          parentregion <- db_frozen[db_frozen$ID==subregion,]
          cat('! using data from PARENTREGION with ID ',as.character(parentregion$ID),'\n',sep='')
          srcdatapath <- file.path(storage_location,as.character(parentregion$ID))
          rasterimages <- tryCatch({
            CropFromGeotiff(daterange = daterange, shapefilepath = shapefilepath, srcfolder = srcdatapath, dstfolder = datapath, geotiff_compression = geotiff_compression)},
            error = function(e) {NULL},
            warning = function(w) {NULL})
        } else {
          cat('! using data from WEBSERVER','\n',sep='')
          rasterimages <- tryCatch({
            #Download&Process MODIS Data or return NULL when an error occured
            Raw2Geotiff(daterange = daterange, shapefilepath=shapefilepath, dstfolder=datapath, srcstorage=srcstorage, geotiff_compression=geotiff_compression)},
            error = function(e) {NULL})
        }
        
        if (is.null(rasterimages)) {
          cat('! There was an error downloading and processing files for ',name,'\n',sep='')
          if (STOP_ON_DOWNLOAD_ERROR) {
            stop("! UpdateData has been stopped due to an error while downloading and processing of the data. \n Set STOP_ON_DOWNLOAD_ERROR=FALSE if you do not want this behaviour (However, this can cause data gaps).")
          }
        } else if (nrow(rasterimages)==0) {
          cat('! No new data were found for ',name,'\n',sep='')
        } else {
          cat('... Starting post-processing of new data for ',name,'\n',sep='')
            newdata = rasterimages[order(rasterimages$date, decreasing=FALSE),]
            # Get a list of all available rasterimages/geotiffs. Extract date vector from filenames and create dataframe with filename-date pairs.
            gtifffiles <- list.files(datapath, pattern = "\\-daily.tif$", full.names = TRUE)
            dates=as.Date(sub("-daily.tif","",basename(gtifffiles)))
            datainstorage = data.frame(file=gtifffiles, date=dates)
            otherfiles <- datainstorage[datainstorage$date > max(newdata$date),] # remove newer, interfering files, e.g. after database reset 
            removefinally <- c(removefinally,as.character(otherfiles$file))
            datainstorage <- datainstorage[datainstorage$date <= max(newdata$date),]
            olddata<-datainstorage[!(datainstorage$file %in% newdata$file),]
            if (nrow(olddata)>0) {
              olddata = olddata[order(olddata$date, decreasing=TRUE),]
              current_image <- as.character(olddata$file[1])
            } else {
              current_image <- NULL
            }
            
            #if cloud_correct=TRUE; use last observation to fill in cloud gaps in the new observations.
            if (db_frozen$cloud_correct[i]) {
              cloud_correct(images=c(current_image,as.vector(newdata$file)),maskvalue=-32767)
            }
            
            # Update timeseries.csv file with new observations
            # If required for elevation zone splitting, check if DEM exists, otherwise fetch and save it
            
            if (db_frozen$timeseries[i]) {

              
              # Get the list of the most recent timeseries files
              df_ts_files <- dbGetQuery(db,sprintf("SELECT filepath,min_elev,max_elev FROM timeseries WHERE catchmentid=%s",ID))
              
              if (db_frozen$ID[i] > 1) {
                dempath <- file.path(datapath,"dem.grd")
                if (!file.exists(dempath)) {
                  cat(' -> Downloading DEM data for the first time')
                  r <- raster(gtifffiles[1])
                  dem_resampled <- get_DEM(r, savepath=dempath)
                } else {
                  dem_resampled <- raster(dempath) 
                }
              }
              
              # if eleveation splitting is activated, preapre the DEM and elevation zone information
              if (!is.na(db_frozen$elev_split[i])) {
                cat('! Elevation Splitting is activated')
                if (nrow(df_ts_files)==0) {
                  stepsize <- db_frozen$elev_split[i]
                  min <- floor(min(values(dem_resampled))/stepsize)*stepsize
                  max <- ceiling(max(values(dem_resampled))/stepsize)*stepsize
                  alt_steps <- seq(min,max,by=stepsize)
                  min_elev <- alt_steps[-length(alt_steps)]
                  max_elev <- alt_steps[-1]
                  elev_zone <- min_elev %/% 500 + 1
                  df_ts_files <- data.frame(filepath=rep(NA,length(alt_steps)-1),min_elev=min_elev,max_elev=max_elev, elev_zone=elev_zone)
                } 
              } else if (nrow(df_ts_files)==0) {
                  # no elevation splitting: use dummy values
                  df_ts_files <- data.frame(filepath=NA,min_elev=-9999,max_elev=9999, elev_zone=1)
              }
              cat("\n ... Updating Time Series ")
              
              # select all data from storage location that is newer than the last timeseries observation.
              last_obs_ts <- if (is.na(db_frozen[ID,'last_obs_ts'])) -Inf else as.Date(db_frozen[ID,'last_obs_ts'])
              newtsdata <- datainstorage[as.Date(datainstorage$date) > last_obs_ts,]
              
              # if new data for timeseries are available, read the corresponding rasterimages and add value to the timeseries.
              if (nrow(newtsdata)>0) {
                values <- matrix(data = NA, nrow=length(newtsdata[,1]),ncol=nrow(df_ts_files))
                dates <- c()
                for (k in 1:length(newtsdata[,1])) {
                  cat('..',k,sep="")
                  r <- raster(as.character(newtsdata$file[k]))
                  r[r==-32766 | r==-32767]=NA  #remove cloud, water mask
                  dates[k] <- as.character(newtsdata$date[k])
                  for (p in seq(length=nrow(df_ts_files))) { 
                    min <- df_ts_files$min_elev[p]
                    max <- df_ts_files$max_elev[p]
                    if (min == -9999 & max == 9999) {
                      values[k,p] <- mean(values(r), na.rm=TRUE)
                    } else {
                      r_masked <- mask(r, (dem_resampled >= min & dem_resampled < max),maskvalue=FALSE)
                      values[k,p] <- mean(values(r_masked), na.rm=TRUE)
                    }
                  }
                }
                rm(r); if (exists("dem_resampled")) {rm(dem_resampled)}; gc()
                
                # write new timeseries files and register them in the database table timeseries (automatically overwrites old timeseries file registration)
                for (p in seq(length=nrow(df_ts_files))) {
                  newpath <- sprintf("%s/ts-m%s-m%s-t%s.csv",ID,df_ts_files$min_elev[p],df_ts_files$max_elev[p],round(as.numeric(Sys.time())))
                  oldpath <- as.character(df_ts_files$filepath[p])
                  ts_new <- data.frame(date=dates,value=values[,p])
                  if (file.exists(file.path(storage_location,oldpath))) {
                    ts <-  read.csv(file.path(storage_location,oldpath),header=TRUE,stringsAsFactors = FALSE)
                    ts <- ts[as.Date(ts$date)<=last_obs_ts,]
                    ts_new <- rbind(ts,ts_new)
                  }
                  write.csv(ts_new,file = file.path(storage_location,newpath),row.names=FALSE, na="NaN") 
                  query <- dbSendStatement(db, sprintf("UPDATE timeseries SET filepath = '%s' WHERE filepath = '%s';", newpath, oldpath))
                  if (dbGetRowsAffected(query)==0) {
                    dbClearResult(query)
                    min <- df_ts_files$min_elev[p]
                    max <- df_ts_files$max_elev[p]
                    elev_zone <- df_ts_files$elev_zone[p]
                    query <- dbSendStatement(db, sprintf("INSERT INTO timeseries (catchmentid, filepath, min_elev, max_elev, elev_zone) VALUES (%s,'%s','%s','%s','%s')", ID, newpath, min, max, elev_zone))
                  }
                  dbClearResult(query)
                }
                db_frozen$last_obs_ts[i] <- max(dates)
                query <- dbSendStatement(db, sprintf("UPDATE settings SET last_obs_ts = '%s' WHERE ID = %s;",max(dates),ID))
                dbClearResult(query)
                removefinally <- c(removefinally,file.path(storage_location,df_ts_files$filepath))
                cat('..Done! \n')
              }
            }
            
            
            # Add rasterimages that shall not be stored and are thus no longer required after the current download/daterangechunk to the vector removefinally.
            # If cloud correct is activated, keep the most recent rasterimage, even if store_geotiff is set to FALSE
            if (!db_frozen$store_geotiff[i]) {
              if (db_frozen$cloud_correct[i]) {
                datainstorage2remove <- datainstorage[!datainstorage$date %in% max(datainstorage$date),] #Excludes the most recent files
                datainstorage2keep <- datainstorage[datainstorage$date %in% max(datainstorage$date),]
                removefinally <- c(removefinally,as.character(datainstorage2remove$file))  
              } else {
                removefinally <- c(removefinally,list.files(datapath, pattern = "\\.tif$", full.names = TRUE))
              }
            } else if (!is.na(db_frozen$store_length[i]) && is.numeric(db_frozen$store_length[i]) && db_frozen$store_length[i]>0) {
                alldates <- datainstorage$date
                alldates <- alldates[order(alldates,decreasing=TRUE)]
                dates2keep <- alldates[1:round(db_frozen$store_length[i])] 
                datainstorage2keep <-datainstorage[datainstorage$date %in% dates2keep,]
                datainstorage2remove <- datainstorage[!datainstorage$date %in% dates2keep,]
                removefinally <- c(removefinally,as.character(datainstorage2remove$file)) 
            } else {
              datainstorage2keep <- datainstorage
            } 
            
            # Register the remaning geotiff files in the database table geotiffs
            df <- dbGetQuery(db, sprintf("SELECT * FROM geotiffs WHERE catchmentid ='%s'",ID))
            df$fullpath <- file.path(storage_location,df$filepath) 
            data2db <- datainstorage2keep[!datainstorage2keep$file %in% df$fullpath,]
            data2db$file <- file.path(ID,basename(as.character(data2db$file)))
            for (o in seq(length = nrow(data2db))) {
              query <- dbSendStatement(db, sprintf("INSERT INTO geotiffs (filepath, date, catchmentid) VALUES ('%s','%s','%s')", data2db$file[o],data2db$date[o],ID))
              dbClearResult(query)
            }
            lastdate <- max(datainstorage2keep$date)
            db_frozen$last_obs_gtif[i] <- lastdate
            query <- dbSendStatement(db,sprintf("UPDATE settings SET last_obs_gtif = '%s' WHERE ID = %s;)",lastdate,ID))
            dbClearResult(query)
        } 
        
      
      } 
    }
    # Delete temporary files and unwanted geotiffs after one DownloadChunk and deregister the corresponding files in the database
    if (length(removefinally)>0) {
      query <- dbSendStatement(db,sprintf("DELETE FROM geotiffs WHERE filepath in ('%s')",paste(gsub(paste(storage_location,"/",sep=""),"",removefinally),collapse="','")))
      dbClearResult(query)
      unlink(removefinally) 
    }
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
    dir.create(dstfolder, recursive = TRUE)
  }
  
  gtifffiles = list.files(srcfolder, pattern = "\\-daily.tif$", full.names = TRUE)
  dates=as.Date(sub("-daily.tif","",basename(gtifffiles)))
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
        dstfile <- file.path(dstfolder,paste(availabledata$date[i],'-daily.tif',sep=''))
        cat('... Processing ',as.character(srcfile),'\n',sep='')
        gdalwarp(srcfile=srcfile,dstfile=dstfile,cutline=shapefilepath,crop_to_cutline = TRUE, t_srs="EPSG:4326",co=compressionmethod, overwrite=TRUE)
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
db <- dbConnect(drv = RSQLite::SQLite(), dbname=DATABASE_LOC)

if (length(dbListTables(db))==0) {
  cat("! First startup: Initialising database")
  query <- dbSendStatement(conn = db,"CREATE TABLE settings (ID INTEGER PRIMARY KEY,name TEXT, geojson TEXT NOT NULL, is_subregion_of INTEGER DEFAULT 1, store_geotiff INTEGER DEFAULT 1, store_length INTEGER, cloud_correct INTEGER DEFAULT 1, timeseries INTEGER DEFAULT 1, elev_split INTEGER, earliestdate TEXT DEFAULT '2000-01-01', latestdate TEXT, deletion INTEGER DEFAULT 0, last_obs_ts TEXT, last_obs_gtif TEXT, locked INTEGER DEFAULT 0)")
  dbClearResult(query)
  query <- dbSendStatement(conn = db,"CREATE TABLE geotiffs (ID INTEGER PRIMARY KEY, filepath TEXT,catchmentid INTEGER NOT NULL, date TEXT, CONSTRAINT file_unique UNIQUE (filepath))")
  dbClearResult(query)
  query <- dbSendStatement(conn = db,"CREATE TABLE timeseries (ID INTEGER PRIMARY KEY, filepath TEXT,catchmentid INTEGER NOT NULL, min_elev INTEGER, elev_zone INTEGER,max_elev INTEGER, CONSTRAINT file_unique UNIQUE (filepath))")
  dbClearResult(query)
  
  geojson_masterregion <- as.json(MASTERREGION_SHAPEFILE)
  
  query <- dbSendStatement(conn = db,sprintf("INSERT INTO settings (ID, name, geojson, is_subregion_of, store_geotiff, earliestdate, latestdate, timeseries, cloud_correct) VALUES (1,'MASTERREGION','%s',NULL,1,'%s','%s',0,0);",geojson_masterregion,MASTERREGION_EARLIEST_DATE,MASTERREGION_LATEST_DATE))
  dbClearResult(query)
} else if (!all(sort(dbListTables(db)) == sort(c("settings","geotiffs","timeseries")))) {
  stop("Database is invalid or corrupted. Please check the filepath in DATABASE_LOC")
}
dbDisconnect(db)

#database <- read.csv(DATABASE_LOC, comment.char='#', stringsAsFactors = TRUE)
UpdateData(db = DATABASE_LOC, storage_location=DATASTORAGE_LOC, srcstorage = MODIS_DATASTORAGE, geotiff_processor=GEOTIFF_PROCESSOR, max_download_chunk = maxDOWNLOADchunk, update_days_limit = limit_past_days)
  
sink()
