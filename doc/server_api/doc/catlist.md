
# Catchment List

Retrieve a list of all catchments with some overview information and links (href) to the resources for geojson, geotiffs and timeseries

**URL** : `/catchments`

**Method** : `GET`

**Auth required** : NO


## Success Response

**Code** : `200 OK`

**Content example**

```json
[
  {
    "ID": 1, 
    "geojson": {
      "href": "/catchments/1/geojson"
    }, 
    "geotiff": {
      "href": "/catchments/1/geotiffs"
    }, 
    "href": "/catchments/1", 
    "last_obs_gtif": "2000-12-30", 
    "last_obs_ts": null, 
    "name": "MASTERREGION", 
    "timeseries": {
      "href": "/catchments/1/timeseries"
    }
  }, 
  {
    "ID": 5, 
    "geojson": {
      "href": "/catchments/5/geojson"
    }, 
    "geotiff": {
      "href": "/catchments/5/geotiffs"
    }, 
    "href": "/catchments/5", 
    "last_obs_gtif": "2000-12-30", 
    "last_obs_ts": "2000-12-30", 
    "name": "Kyrgyzstan", 
    "timeseries": {
      "href": "/catchments/5/timeseries"
    }
  }
]
```


