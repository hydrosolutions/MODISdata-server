
# Shapefile2json

Convert vector data from shapefile format to geojson format. Projects coordinates to EPSG:4326. Required files: .shp, .shx, .dbf, .prj

**URL** : `/shapefile2geojson`

**Method** : `POST` (`GET` will response with a html upload form)

**Auth required** : NO

**Data constraints**

Four files as form data with field names "files"

**Request example**

```bash
curl -i -X POST -F files=@<path.shp> -F files=@<path.shx> -F files=@<path.dbf> -F files=@<path.prj> <this:url>
```

## Success Response

**Code** : `200 OK`

**Content example**

```
geojson: specifications @ http://geojson.org/
```

## Error Response


* **Condition** : Input files are not valid or incomplete

	**Code** : `400 BAD REQUEST ERROR`
