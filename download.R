#!/usr/bin/env Rscript

# Check if Script is run from RStudio and load the default configuration file
# Else receive command line argument for "path to configuration file"
cmd = TRUE
try({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  source('download.conf')
  cmd = FALSE
  }, silent = TRUE
)

# Check validity of args
if (cmd) {
  args = commandArgs(trailingOnly=TRUE)
  if (length(args)<1) {
    stop("Argument for configuration file is missing, try: Rscript download.R path/download.conf")
  } else if (length(args)>1) {
    stop("Too many arguments, try: Rscript download.R path/download.conf")
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

# LOAD CONFIGURATION
GenerateCompressionArgument <- function(compression) {
  if (compression) {
    compressionmethod <- c("COMPRESS=deflate","zlevel=9")
  } else {
    compressionmethod <- "compress=none" 
  }
  return(compressionmethod)
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

getMODISNDVI <- function(date, shapefilepath, dstfolder, hdfstorage=NULL, cloudmask=TRUE, compression=FALSE){
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
  if (!is.null(hdfstorage)) {
    capture.output(capture.output(MODISoptions(quiet=TRUE,localArcPath=hdfstorage,save=FALSE),file='NULL', type="message"),file='NULL') #capture.output to drop all the initiliastion messages of MODIS package
  } else {
    capture.output(capture.output(MODISoptions(quiet=TRUE,localArcPath=tempfolder,save=FALSE),file='NULL', type="message"),file='NULL')
    checkHDFintegrity()
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
  x='MYD13Q1'
  out1=tryCatch(getHdf(product=x,begin=date[[1]],end=date[[2]],tileH=tile@tileH,tileV=tile@tileV, wait=0.5, checkIntegrity=TRUE), error = function(e) {list()})
  
  # Then MODIS Terra
  x='MOD13Q1'
  out2=tryCatch(getHdf(product=x,begin=date[[1]],end=date[[2]],tileH=tile@tileH,tileV=tile@tileV, wait=0.5, checkIntegrity=TRUE), error = function(e) {list()})
  
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
  observationsbydate = data.frame(dates,files)
  observationsbydate = by(as.data.frame(observationsbydate), as.data.frame(observationsbydate)[,"dates"], function(x) x)
  
  # Process all Tiles of each observation date
  for (k in 1:nrow(observationsbydate)) {
    
    filesvalid=TRUE #help variable in case a HDF File is corrupted
    HDFlistbydate =  observationsbydate[[k]]
    GTifflist <- vector(length=nrow(HDFlistbydate))
    GTifflist2 <- vector(length=nrow(HDFlistbydate))

	# Go through each Tile of the current date and: check file integrity, extract EVI layer, cut to shapefile, reproject to EPSG:4326
    for (i in 1:nrow(HDFlistbydate)){

	  cat('Processing ... ',as.character(HDFlistbydate$files[i]),'\n',sep='')
      GTifflist[i]=tempfile('NDVI',tempfolder,fileext = ".tif")
      GTifflist2[i]=tempfile('NDVI',tempfolder, fileext = ".tif")
      
      t <- tryCatch(gdalinfo(HDFlistbydate$files[i]), warning = function(w) {NULL} ,error = function(e) {NULL})
      if (is.null(t)) {
        unlink(as.character(HDFlistbydate$files[i]))
        try(getHdf(HdfName=basename(as.character(HDFlistbydate$files[i])), wait=10))
        t <- tryCatch(gdalinfo(HDFlistbydate$files[i]), warning = function(w) {NULL} ,error = function(e) {NULL})
          if (is.null(t)) {
            cat('Download was not successfull. Try to manually download the following file and check its validity: ', as.character(HDFlistbydate$files[i]),'\n',sep='')
            filesvalid=FALSE
            setwd(oldwd)
            break
          }
      }
      
      sds<-getSds(as.character(HDFlistbydate$files[i]))
      qualityband<-raster(readGDAL(sds$SDS4gdal[12], as.is=TRUE, silent=TRUE))
      mask<-calc(qualityband, fun = function(x) {bitwAnd(2,x)})
      rm(qualityband);gc() # free memory
      evi<-raster(readGDAL(sds$SDS4gdal[2], as.is=TRUE, silent=TRUE))
      
      if (cloudmask) {
        evi[mask==2]<- -10000
        evi[is.na(evi)]<- -10000
      }
      
      rm(mask);gc()
      
      writeRaster(evi,filename=GTifflist[i],format="GTiff",datatype="INT2S")
      rm(evi);gc()
      gdalwarp(srcfile=GTifflist[i],dstfile=GTifflist2[i],cutline=shapefilepath,crop_to_cutline = TRUE, t_srs="EPSG:4326") #Transform GTiff and crop to shapefile

    }
	
	# In case all Tiles of the current date are valid, mosaic them to one Gtiff File
    if (filesvalid) {
    filename=paste(HDFlistbydate$dates[i],'.tif',sep='')
    dstfile <- file.path(dstfolder,filename)
    mosaic_rasters(GTifflist2,dstfile,co=compressionmethod)  #
    outputdates=c(outputdates,as.character(HDFlistbydate$dates[i]))
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

check_available_data <- function(datapath) {
  # find the geotiff folder
  if (!isAbsolutePath(datapath)) {
    abs_path <- file.path(DATASTORAGE_LOC,datapath)
    abs_path <- gsub("//","/",abs_path) # Remove additional slashes
  }
  
  csv_file <- file.path(abs_path,TIMESERIES_DEFAULT_NAME)
  if (file.exists(csv_file)) {
    ts <- read.csv(csv_file)
    ts_dates <- as.data.frame(ts$date)
  } else {
    ts_dates <- NULL
  }
  
  geotiff_list <- list.files(abs_path, pattern="*.tif")
  if (length(geotiff_list>0)) {
    geotiff_dates=as.data.frame(as.Date(sub(".tif","",basename(geotiff_list))))
  } else {
    geotiff_dates <- NULL
  }
  
  
  return(list(TS = unname(ts_dates), GEOTIFF = unname(geotiff_dates)))
}

get_latest_observation <- function(datapath, geotiff=FALSE) {
  available_dates <- check_available_data(datapath)
  if (geotiff) {
    available_dates <- available_dates$GEOTIFF
  } else {
    available_dates <- available_dates$TS
  }
  
  if (is.null(available_dates)) {
    return(NULL)
  } else {
    return(max(as.Date(unlist(available_dates))))
  }
}

UpdateAndProcess <- function(shapefilelist) {
  shapefilelist <- shapefilelist[order(shapefilelist$is_subregion_of, na.last =  FALSE),] # order dataframe such that subregions come last
  
  if (is.na(MODIS_DATASTORAGE)) {
    localArcPath <- file.path(tempdir(), "MODIS")
  } else {
    localArcPath <- MODIS_DATASTORAGE
  }
  
  if (!dir.exists(DATASTORAGE_LOC)) {dir.create(DATASTORAGE_LOC, recursive = TRUE)}
  MODISoptions(MODISserverOrder="LAADS",quiet=TRUE,localArcPath=localArcPath,outDirPath=DATASTORAGE_LOC) 
  
  # Find latest observation for each database entry
  df_dates <- data.frame()
  for (i in 1:nrow(shapefilelist)) {
    date <- get_latest_observation(shapefilelist$datapath[i], geotiff=shapefilelist$store_geotiff[i])
    if (is.null(date)) {
      startdate <- as.Date(shapefilelist$earliestdate[i])
    } else {
      startdate <- date+1
    }
    
    # TODO: Introduce better test for null
    if (shapefilelist$latestdate[i]=="NULL") {
      enddate <- Sys.Date()
    } else {
      enddate <- shapefilelist$latestdate[i]
    }
    df <- data.frame(ID = shapefilelist$ID[i], startdate=startdate, enddate = enddate)
    df_dates <- rbind(df_dates,df)
  }
  rownames(df_dates) <- df_dates$ID
  
  # Create timerange and daterangechunks
  startdate <- min(df_dates$startdate)
  enddate <- max(df_dates$enddate)
  daterange = c(startdate,enddate)
  daterange_days = enddate-startdate
  
  # Split MODIS search window into chunks if the daterange exceed maxDOWNLOADchunk
  if (daterange_days>maxDOWNLOADchunk) {
    chunks_startdate <- seq(startdate,enddate, by=30)
  } else {
    chunks_startdate <- daterange[1] 
  }
  downloadchunks <- data.frame(start=chunks_startdate, end=c(chunks_startdate[-1]-1,enddate))
  
  for (j in 1:nrow(downloadchunks)) {
    removefinally <- c()
    for (i in 1:nrow(shapefilelist)) {
      ID <- as.character(shapefilelist$ID[i])
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
        
        # concentate shapefile path if given path is not absolute
        name=as.character(shapefilelist$name[i])
        shapefilepath=as.character(shapefilelist$shapefile[i])
        if (!isAbsolutePath(shapefilepath)) {
          #TODO: throw warning if path is not correct
          shapefilepath <- file.path(SHAPEFILE_LOC,shapefilepath)
          shapefilepath <- gsub("//","/",shapefilepath)
        }
        
        # concentate data path if given path is not absolute
        datapath=as.character(shapefilelist$datapath[i])
        if (!isAbsolutePath(datapath)) {
          datapath <- file.path(DATASTORAGE_LOC,datapath)
          datapath <- gsub("//","/",datapath)
        }
        if (!dir.exists(datapath)) {
          dir.create(datapath, recursive=TRUE)
        }
        
        # Download and process new MODIS observations
        cat('\n','############### Data for ',name,' are being updated from ',as.character(daterange[1]),' to ',as.character(daterange[2]),' ... ###############','\n',sep='')
        
        if (!is.na(shapefilelist$is_subregion_of[i])) {
          cat('... using data from PARENTREGION with ID ',as.character(shapefilelist$is_subregion_of[i]),' ...','\n',sep='')
          parentregion <- shapefilelist[shapefilelist$ID==as.character(shapefilelist$is_subregion_of[i]),]
          if (nrow(parentregion)==0) {
            warning(paste("The ID",shapefilelist$is_subregion_of[i],"given for is_subregion_of does not have a valid entry", sep=" "))
          } else {
            srcdatapath <- parentregion$datapath
            if (!isAbsolutePath(srcdatapath)) {
              srcdatapath <- file.path(DATASTORAGE_LOC,srcdatapath)
              srcdatapath <- gsub("//","/",srcdatapath)
            }
            rasterimages <- cropFromGeotiff(date = daterange, shapefilepath = shapefilepath, srcfolder = srcdatapath, dstfolder = datapath, compression = GEOTIFF_COMPRESSION)
          }
          
        } else {
          cat('... using data from MODIS FTP server ...','\n',sep='')
          rasterimages <- getMODISNDVI(date = daterange, shapefilepath=shapefilepath, dstfolder=datapath, hdfstorage=MODIS_DATASTORAGE, compression=GEOTIFF_COMPRESSION) #Download&Process MODIS Data
        }
        
        # Add new observations to timeseries file
        if (!is.null(rasterimages)) {
          cat('Updating Time Series for ',name,' ...','\n',sep='')
          
          gtifffiles <- list.files(datapath, pattern = "\\.tif$", full.names = TRUE)
          dates=as.Date(sub(".tif","",basename(gtifffiles)))
          datainstorage = data.frame(gtifffiles, dates)
          
          # Concentating filename for timeseries. Reading existing file or creating an empty data.frame
          csvpath <- file.path(datapath,TIMESERIES_DEFAULT_NAME)
          if (file.exists(csvpath)) {
            ts <- read.csv(csvpath,stringsAsFactors = FALSE, header = TRUE)
            newtsdata <- datainstorage[!as.Date(datainstorage$dates) %in% as.Date(ts$date),]
          } else {
            ts <- data.frame()
            newtsdata <- datainstorage
          }
          
          if (nrow(newtsdata)>0) {
            values <- vector(mode="numeric", length=length(newtsdata[,1]))
            dates <- vector(mode='character',length=length(newtsdata[,1]))
            for (k in 1:length(values)) {
              r <- raster(as.character(newtsdata[k,'gtifffiles']))*0.0001
              r[r==-1]=NA
              values[k]=mean(values(r), na.rm=TRUE)
              dates[k]=as.character(newtsdata[k,'dates'])
            }
            ts <- rbind(ts,data.frame(date=dates,value=values))
            ts = ts[order(ts$date, decreasing=FALSE),]
            write.csv(ts,file=csvpath,row.names=FALSE) 
          }
          
            if (!shapefilelist$store_geotiff[i]) {
              if (shapefilelist$cloud_correct[i]) {
                datainstorage2remove <- datainstorage[!datainstorage$dates %in% max(datainstorage$dates),] #Excludes the most recent files
                removefinally <- c(removefinally,as.character(datainstorage2remove$gtifffiles))  
              } else {
                removefinally <- c(removefinally,list.files(datapath, pattern = "\\.tif$", full.names = TRUE))
              }
            } else {
              if (!is.na(shapefilelist$store_length[i]) && is.numeric(shapefilelist$store_length[i]) && shapefilelist$store_length[i]>0) {
                alldates <- datainstorage$dates
                alldates <- alldates[order(alldates,decreasing=TRUE)]
                dates2keep <- alldates[1:round(shapefilelist$store_length[i])] 
                datainstorage2remove <- datainstorage[!datainstorage$dates %in% dates2keep,]
                removefinally <- c(removefinally,as.character(datainstorage2remove$gtifffiles)) 
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
database <- read.csv(DATABASE_LOC, comment.char='#', stringsAsFactors = TRUE)

if (any(duplicated(database$ID))) {
  stop("The ID entries in the given database are not unique")
}

UpdateAndProcess(database)
  
  

