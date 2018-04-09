# Geojson

Retrieve the geojson (catchment shape) of the specified catchment

**URL** : `/catchments/<catchmentid>/geojson`

**Method** : `GET`

**Auth required** : NO


## Success Response

**Code** : `200 OK`

**Content example**

```
geojson: specifications @ http://geojson.org/
```

## Error Response

* **Condition** : If \<catchmentid> or \<timeseriesid> does not exist

	**Code** : `404 NOT FOUND`
