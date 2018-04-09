
# Catchment

Retrieve detailed catchment information. 

**URL** : `/catchments/<catchmentid>`

**Method** : `GET`

**Auth required** : NO

## Success Response

**Code** : `200 OK`

**Content example**

```json
{
  "ID": 5, 
  "cloud_correct": 1, 
  "deletion": 0, 
  "earliestdate": null, 
  "elev_split": 500, 
  "geojson": {
    "href": "/catchments/5/geojson"
  }, 
  "geotiff": {
    "href": "/catchments/5/geotiffs"
  }, 
  "is_subregion_of": 1, 
  "last_obs_gtif": "2000-12-31", 
  "last_obs_ts": "2000-12-31", 
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

**Condition** : If \<catchmentid> does not exist

**Code** : `404 NOT FOUND`

**Content** :

```json
{
  "message": "there is no catchment with the requested id"
}
```
