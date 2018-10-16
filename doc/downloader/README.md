
# Data processor

## Initialisation

The initialisation is required before the server can be started. It will create the database and download the required data for the masterregion specified in the configuration. The initiliastion may take a long time, depending on the amount of tiles required and the earliest and latestdate. Make sure that enough disk space is available in the path of DATASTORAGE_LOC. It is recommended to set STOP_ON_DOWNLOAD_ERROR to TRUE. In this case, the script must be restarted on error until all available data have been successfully processed. Otherwise data gaps may result if the MODIS data server is temporarily offline. 

