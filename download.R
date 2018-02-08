setwd("/srv/shiny-server/IrrigationCalc-Guantao")
source('downloadconf.R')
# load necessary libraries
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

getMODISNDVI <- function(date, shapefile, dstfolder, hdfstorage=NULL){
  # Input: 
  # date=c(begindate,enddate)
  # shapefile = path to a valid shapefile
  # dstfolder = path to the folder, where output GeoTiff Files shall be saved
  # path to a Folder for persistant MODIS HDF storage (when there is a chance that the HDFs are used again). NULL when 
  # no persistent storage should be used. All downloaded MODIS HDF Files will then be deleted after processing.
		

  tempfolder = file.path(tempdir(),"processing")
  
  if (!dir.exists(tempfolder)) {
    dir.create(tempfolder)
  } else {
    do.call(unlink, list(list.files(tempfolder, full.names = TRUE),recursive=TRUE))
  }

  oldwd = getwd()
  setwd(tempfolder)
  if (!is.null(hdfstorage)) {
    capture.output(MODISoptions(localArcPath=hdfstorage,save=FALSE,quiet=TRUE),file='NULL')
  } else {
	capture.output(MODISoptions(localArcPath=tempfolder,save=FALSE,quiet=TRUE),file='NULL')
  } 
  
  if (!dir.exists(dstfolder)) {
    dir.create(dstfolder)
  }
  
  outputdates=c()
  outputfiles=c()

  # Get vector of required tiles for the specified shapefile
  myshp <- readOGR(shapefile, verbose=FALSE)
  e <- extent(myshp)
  tile <- getTile(e)

  # Try MODIS Aqua first
  x='MYD13Q1'
  out1=tryCatch(getHdf(product=x,begin=date[[1]],end=date[[2]],tileH=tile@tileH,tileV=tile@tileV, wait=0.5), error = function(e) {list()})
  
  # Then MODIS Terra
  x='MOD13Q1'
  out2=tryCatch(getHdf(product=x,begin=date[[1]],end=date[[2]],tileH=tile@tileH,tileV=tile@tileV, wait=0.5), error = function(e) {list()})
  
  # Merge lists of downloaded HDF Tiles
  files <- c(unname(unlist(out1)),unname(unlist(out2)))

  # abort when nothing has been downloaded or the number of downlaoded tiles does not coincide with the number of required tiles for this shapefile
  if ((length(files) %% length(tile@tileH)*length(tile@tileV)) > 0) {
    return(NULL)
  } else if (length(files)==0) {
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
            break
          }
      }
      
      sds<-getSds(as.character(HDFlistbydate$files[i]))
      evi<-raster(readGDAL(sds$SDS4gdal[2], as.is=TRUE, silent=TRUE))
      qualityband<-raster(readGDAL(sds$SDS4gdal[12], as.is=TRUE, silent=TRUE))
      mask<-calc(qualityband, fun = function(x) {bitwAnd(2,x)})
      if (cloudmask) {
        evi[mask==2]<- -10000
        evi[is.na(evi)]<- -10000
      }
      writeRaster(evi,filename=GTifflist[i],format="GTiff",datatype="INT2S")
      gdalwarp(srcfile=GTifflist[i],dstfile=GTifflist2[i],cutline=shapefile,crop_to_cutline = TRUE, t_srs="EPSG:4326") #Transform GTiff and crop to shapefile

    }
	
	# In case all Tiles of the current date are valid, mosaic them to one Gtiff File
    if (filesvalid) {
    filename=paste(HDFlistbydate$dates[i],'.tif',sep='')
    mosaic_rasters(GTifflist2,file.path(dstfolder,filename))  #
    outputdates=c(outputdates,as.character(HDFlistbydate$dates[i]))
    outputfiles=c(outputfiles, file.path(dstfolder,filename))
    }
  }

  output=data.frame(outputfiles,outputdates)
  names(output)[1] <- 'file'
  names(output)[2] <- 'date'
  
  # Delete all temporaty working data and HDF Files if argument hdfstorage is NULL (no persistent storage)
  do.call(unlink, list(list.files(tempfolder, full.names = TRUE),recursive=TRUE))
  
  if (is.null(hdfstorage)) {
    delHdf("MOD13Q1",ask=FALSE)
    delHdf("MYD13Q1",ask=FALSE)
  }
  
  setwd(oldwd)
  return(output)
}

setwd(ROOT)

#LOGIN into Nasa Server: 
#setNASAauth(NASAusername,NASApassword)
MODISoptions(MODISserverOrder="LAADS") #BUGFIX to exclude other server which does cause error

# Read list of shapefiles
shapefilelist <- read.csv(shapefileCSV, comment.char='#')

#For each shapefile do
for (i in 1:nrow(shapefilelist)) {
  newdata = FALSE
  name=as.character(shapefilelist[i,1])
  cat('\n','############### Data for ',name,' are being updated... ###############','\n',sep='')
  shapefile=as.character(shapefilelist[i,2])
  if (!isAbsolutePath(shapefile)) {
    shapefile=file.path(ROOT,shapefile)
  }
  datapath=as.character(shapefilelist[i,3])
  if (!isAbsolutePath(datapath)) {
    datapath=file.path(ROOT,datapath)
  }
  files = list.files(datapath, pattern = "\\.tif$", full.names = TRUE)
  dates=as.Date(sub(".tif","",basename(files)))
  data = data.frame(files, dates)
  data = data[order(data[,2], decreasing=TRUE),]

  # When there are already data, set begindate to last data point, otherwise set it to the begindate specified in downloadconfig.R
  if (is.na(data$dates[1])) {
    begindate = earliestdate
  }
  else {
    begindate=as.Date(data$dates[1])+days(1)
  }
  daterange = c(begindate,as.Date(Sys.Date()))
  daterange_days = daterange[[2]]-daterange[[1]]

  # Split MODIS search window into chunks if the daterange exceed maxDOWNLOADchunk
  if (daterange_days>maxDOWNLOADchunk) {
    daterange= seq(daterange[[1]],daterange[[2]], length.out=as.numeric(ceiling(daterange_days/maxDOWNLOADchunk)))
  }
  for (i in 1:(length(daterange)-1)) {
    daterangechunk = c(daterange[[i]],daterange[[i+1]])
    rasterimages = getMODISNDVI(daterangechunk, shapefile, datapath, MODIS_DATASTORAGE) #Download&Process MODIS Data
    if (!is.null(rasterimages)) {
      newdata=TRUE
    }
  }
  
  # Generate timeseries.csv when new MODIS data has been processed
  if (newdata) {
      cat('Computing Time Series for ',name,' ...','\n',sep='')
      gtifffiles_all = list.files(datapath, pattern = "\\.tif$", full.names = TRUE)
      dates=as.Date(sub(".tif","",basename(gtifffiles_all)))
      availabledata = data.frame(gtifffiles_all, dates)
      availabledata = availabledata[order(availabledata[,2], decreasing=TRUE),]
      values <- vector(mode="numeric", length=length(availabledata[,1]))
      dates <- vector(mode='character',length=length(availabledata[,1]))
      for (i in 1:length(values)) {
        r <- raster(as.character(availabledata[i,'gtifffiles_all']))*0.0001
        r[r==-1]=NA
        values[i]=mean(values(r), na.rm=TRUE)
        dates[i]=as.character(availabledata[i,'dates'])
      }
      timeseries <- data.frame(values,dates)
      colnames(timeseries)<-c('EVIaverage','Date')
      write.csv(timeseries,file=file.path(datapath,'timeseries.csv'))
  } else {
    cat('No new data were found for ',name,'\n',sep='')
  }
}


