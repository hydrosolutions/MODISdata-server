#file:downloadconf.R: settings for download.R
NASAusername = 'hydrosolutions'
NASApassword = 'hydro1Sol'
DATASTORAGE = "/home/jules/MODIS_Service/data/" # Set to NULL when no persistent storage should be used. This is not recommended when several shapefiles are on the same MODIS Tile, because the Tile will then be downloaded and deleted for every shapefile. Better use an temporary Folder as MODIS_DATASTORAGE and delete it after every run of the script download.R
DATABASE = "/home/jules/MODIS_Service/database.csv


