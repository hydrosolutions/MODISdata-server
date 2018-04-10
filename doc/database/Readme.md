
# Database, SQLITE


#### Tables

Tables: settings, geotiffs, timeseries

#### settings

This tables is the catchment index.
rem. TYPE BOOLEAN is actually type INTEGER restricted to 0 and 1

** ID ** : INTEGER: automatically set
** name ** : TEXT: catchment name
** geojson ** : TEXT: geojson string of the catchment boundary
** is_subregion_of ** : INTEGER: ID of the parentregion or null.
** store_geotiff ** : BOOLEAN: shall produced geotiffs be stored? Files are only deleted after the timeseries have been updated. 
** store_length ** : INTEGER > 0: how long shall the record of geotiff data be in days. Older files will be deleted. This does not affect the computation of timeseries. null for an infinite storage length. 
** cloud_correct ** : BOOLEAN: shall geotiffs be cloud corrected?
** timeseries ** : BOOLEAN: shall timeseries be generated from the geotiff data
** elev_split ** : INTEGER>0: the stepsize in meters between elevation zones. For every elevation zone a seperate timeseries will be created. A value of null disables seperation by elevation zones.
** earliestdate, latestdate **: TEXT in format "YYYY-MM-DD": the first resp. latest day for which data shall be generated. 
** deletion ** : BOOLEAN: serves as a deletion flag. The downloader will delete every entry with deletion=1 and all its attached data from the database and filesystem.
** last_obs_ts ** : TEXT in format "YYYY-MM-DD": the date of the most recent timeseries datapoint
** last_obs_gtif ** : TEXT in format "YYYY-MM-DD": the date of the most recent geotiff datapoint

#### geotiffs

** ID **: INTEGER: automatically set
** filepath **: TEXT: the path to the corresponding geotiff
** catchmentid **: INTEGER: The ID of the corresponding catchment entry
** date **: TEXT in format "YYYY-MM-DD": The date of the observation

#### timeseries

** ID **: INTEGER: automatically set
** filepath **: TEXT: the path to the corresponding timeseries csv file
** catchmentid **: INTEGER: The ID of the corresponding catchment entry
** elev_zone **: INTEGER: an identifier of the elevation zone for which this timeseries is valid
** min_elev **: INTEGER: The minimum elevation in meters of the  zone for which this timeseries is valid
** max_elev **: INTEGER: The maximum elevation in meters of the zone for which this timeseries is valid
- 