
# config.R

#### MODIS_DATASTORAGE
The location where the raw MODIS tiles should be stored. Warning: This requires a lot of disk space. Set value to NULL when no persistent storage should be used.

#### DATASTORAGE_LOC
The path to a folder where the processed data are going to be stored

#### DATABASE_LOC
The path to the database file. A database will be created if the specified file does not exist

#### DOWNLOAD_TRIGGER
A system command that triggers the execution of the downloader. The specified call is done by the server app. Therefore it is recommended to use a daemon service like supervisor in order to seperate the application context from the call. Otherwise, upon closing the server application context, the downloader will be halted.

#### maxDOWNLOADchunk 
A number of days as Integer. This defines the timeperiod of one step for which MODIS tiles are downloaded and processed. This setting is especially important if MODIS_DATASTORAGE is set to NULL. Here, after every step resp. DOWNLOADchunk disk space is freed by removing all temporary data like MODIS tiles. Thus, setting maxDOWNLOADchunk to a large value will increase the amount of temproary data that need to be stored simultaneously and a larger disk volume will be required. A value of 3 to 10 is recommended.

#### GEOTIFF_COMPRESSION
TRUE or FALSE: Enables lossless geotiff compression. --> Less diskspace is required, but processing takes slightly longer.

#### GEOTIFF_PROCESSOR
The path to the R file, that includes the function raw2geotiff(). Currently only the files modis_10A1.R (snow) and modis_13Q1.R (ndvi) are supported. However, by defining a new function raw2geotiff that accepts the same arguments and returns the same output as both the above mentioned examples, the data-server can be adopted to any other satellite product.

#### MASTERREGION_SHAPEFILE
By design, a masterregion has to be defined by providing a shapefile, that contains one(!) polygon. The masterregion defines the maximum extent for which data can be provided. Any catchment that is added via server API that does not lay within the masterregion is rejected. The reason behind this design decision is to reduce the amount of repeated data transfer from the MODIS servers when a new region is added to the catchment index. Instead of fetching the same MODIS tile for each newly added catchment again and again, geotiffs of the masterregion are produced and stored from the very beginning. All catchments data is then derived from that masterregion data record. This behaviour can be bypassed with the setting is_subregion_of=null in the catchment index (default is = 1 = ID of masterregion)

#### MASTERREGION_EARLIEST_DATE, MASTERREGION_LATEST_DATE
This sets the earliest day resp. latest day for which the data record should be kept up-to-date. format is "YYYY-MM-DD". Set to NULL, for -inf resp. +inf

#### APP_USER, APP_PW
A username and password that shall be used to protect part of the server API.

#### EXAMPLE config.R

```R
MODIS_DATASTORAGE = NULL
DATASTORAGE_LOC = "/home/jules/liveSNOWdata/" 
DATABASE_LOC = "/home/jules/Desktop/Hydromet/MODISsnow_server/MODISsnow-server/downloader/examplefiles/database"
DOWNLOAD_TRIGGER = "supervisorctl start modis-snow-download"
maxDOWNLOADchunk = 3
GEOTIFF_COMPRESSION = TRUE
GEOTIFF_PROCESSOR = "/home/jules/Desktop/Hydromet/MODISsnow_server/MODISsnow-server/downloader/modis_10A1.R"
MASTERREGION_SHAPEFILE = "/home/jules/Desktop/Hydromet/MODISsnow_server/MODISsnow-server/downloader/examplefiles/shapefiles/border_vahsh.shp"
MASTERREGION_EARLIEST_DATE = "2000-01-01"
MASTERREGION_LATEST_DATE = NULL
APP_USER = "admin"
APP_PW = "i9df3478"


```