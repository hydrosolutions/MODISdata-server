
# Geotiff List

Used to collect a Token for a registered User.

**URL** : `/catchments/<catchmentid>/geotiffs`

**Method** : `GET`

**Auth required** : NO

**URL query parameters** : from, to = YYYY-MM-DD

**URL query example**

```
/catchments/5/geotiffs?from=2000-03-01&to=2000-03-04
```


## Success Response

**Code** : `200 OK`

**Content example**

```json
[
  {
    "ID": 63, 
    "catchmentid": 5, 
    "date": "2000-03-01", 
    "href": "/catchments/5/geotiffs/63"
  }, 
  {
    "ID": 64, 
    "catchmentid": 5, 
    "date": "2000-03-02", 
    "href": "/catchments/5/geotiffs/64"
  }, 
  {
    "ID": 65, 
    "catchmentid": 5, 
    "date": "2000-03-03", 
    "href": "/catchments/5/geotiffs/65"
  }, 
  {
    "ID": 66, 
    "catchmentid": 5, 
    "date": "2000-03-04", 
    "href": "/catchments/5/geotiffs/66"
  }
]
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
