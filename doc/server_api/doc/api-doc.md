
# modis-snow-server API-DOC

## Open Endpoints

Open endpoints require no Authentication.

### Catchment Information
* [Catchment List](doc/catlist.md) : `GET /catchments`
* [Catchment](doc/cat.md) : `GET /catchments/<catchmentid>`

### Catchment Data
* [Timeseries List](doc/tslist.md) : `GET  /catchments/<catchmentid>/timeseries`
* [Timeseries](doc/ts.md) : `GET /catchments/<catchmentid>/timeseries/<timeseriesid>`
* [Geotiff List](doc/gtiflist.md) : `GET /catchments/<catchmentid>/geotiffs`
* [Geotiff](doc/gtif.md) : `GET /catchments/<catchmentid>/geotiffs/<geotiffid>`
* [Geojson](doc/geojson.md) : `GET /catchments/<catchmentid>/geojson`

### Data Processor
* [View Data Processor Status](doc/dpstat.md) : `GET /data_processor`

### Shapefile conversion
* [Shapefile2json](doc/shp2json.md) : `POST /shapefile2geojson`


## Endpoints that require Authentication

Closed endpoints require basic authentication, e.g. curl -u \<user>:\<password> \<url>

### Manipulating Catchments

* [Add Catchment](doc/addcat.md) : `POST /catchments`
* [Delete Catchment](doc/delcat.md) : `DELETE /catchments/<catchmentid>`

### Data Processor

* [Trigger Execution](doc/dpexec.md) : `PUT /data_processor`

## Errors
Errors always go along with a json in the response body. The field "message" might help to narrow down the problem, e.g.:
```json
{
  "message": "data processor is already running. Try again later"
}
```

