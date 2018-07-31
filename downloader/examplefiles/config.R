#file:downloadconf.R: settings for download.R,
MODIS_DATASTORAGE = "/home/jules/MODISStorage/"
DATASTORAGE_LOC = "/home/jules/liveSNOWdata/" 
DATABASE_LOC = "/home/jules/Desktop/Hydromet/MODISsnow_server/MODISsnow-server/downloader/examplefiles/database"
DOWNLOAD_TRIGGER = "supervisorctl start modis-snow-download"
maxDOWNLOADchunk = 3
GEOTIFF_COMPRESSION = TRUE
GEOTIFF_PROCESSOR = "/home/jules/Desktop/Hydromet/MODISsnow_server/MODISsnow-server/downloader/modis_10A1.R"
MASTERREGION_SHAPEFILE = "/home/jules/Desktop/Hydromet/MODISsnow_server/MODISsnow-server/downloader/examplefiles/shapefiles/border_vahsh.shp"
MASTERREGION_EARLIEST_DATE = "2018-01-01"
MASTERREGION_LATEST_DATE = "2018-01-02"
APP_USER = "USER"
APP_PW = "TESTPASSWORD"
STOP_ON_DOWNLOAD_ERROR = TRUE

