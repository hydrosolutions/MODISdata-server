
# Geotiff

Download the requested geotiff image file.

**URL** : `/catchments/<catchmentid>/geotiffs/<geotiffid>`

**Method** : `GET`

**Auth required** : NO

## Success Response

**Code** : `200 OK`

**Content example**

```
BLOB (Binary File) of MIME type image/geotiffint16
```


## Error Response

* **Condition** : If \<catchmentid> or \<timeseriesid> does not exist

	**Code** : `404 NOT FOUND`

