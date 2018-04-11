
# Add Catchment

Add a new catchment to the catchment list. Data will be generated with the next execution of the data processor

**URL** : `/catchments`

**Method** : `POST`

**Auth required** : YES

**Data constraints**
Provide a name for the catchment and the geojson, that defines the spatial extent of the catchment.

```json
{
    "name": "<catchment name>",
    "geojson": <geojson>
}
```

**Additional Data**
* store_length: the number of days for which geotiffs will be stored (int, default=365)
* elev_split: stepsize in meters between elevation zones (int, default=500). A separate timeseries will be generated for every elevation zone. elev_split=null disables elevation splitting.
* earliestdate: date of the first observation (string YYYY-MM-DD, default=null). Null is evaluated as the earliest available date.
* latestdate: date of the last observation (string YYYY-MM-DD, default=null). Null is evaluated as the latest available date.

**Data example**

```json
{
    "name": "Kyrgyzstan",
    "geojson": <geojson>,
    "elev_split" : 1000,
    "earliestdate": "2014-01-01"
}
```

## Success Response

**Code** : `201 CREATED`

**Content example**

```json
{
  "ID": 6, 
  "cloud_correct": 1, 
  "deletion": 0, 
  "earliestdate": "2014-01-01", 
  "elev_split": 1000, 
  "geojson": {
    "href": "/catchments/5/geojson"
  }, 
  "geotiff": {
    "href": "/catchments/5/geotiffs"
  }, 
  "is_subregion_of": 1, 
  "last_obs_gtif": "2014-07-31", 
  "last_obs_ts": "2014-07-31", 
  "latestdate": null, 
  "name": "Kyrgyzstan", 
  "store_geotiff": 1, 
  "store_length": 365, 
  "timeseries": {
    "href": "/catchments/5/timeseries"
  }
}
```

## Error Response

**Condition** : Invalid data in POST request. 

**Code** : `400 BAD REQUEST`