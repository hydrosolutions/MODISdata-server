# Timeseries List

Used to collect a Token for a registered User.

**URL** : `/catchments/<catchmentid>/timeseries`

**Method** : `GET`

**Auth required** : NO

## Success Response

**Code** : `200 OK`

**Content example**

```json
[
  {
    "ID": 1, 
    "catchmentid": 5, 
    "elev_zone": 1, 
    "href": "/catchments/5/timeseries/1", 
    "max_elev": 500, 
    "min_elev": 0
  }, 
  {
    "ID": 2, 
    "catchmentid": 5, 
    "elev_zone": 2, 
    "href": "/catchments/5/timeseries/2", 
    "max_elev": 1000, 
    "min_elev": 500
  }, 
  {
    "ID": 3, 
    "catchmentid": 5, 
    "elev_zone": 3, 
    "href": "/catchments/5/timeseries/3", 
    "max_elev": 1500, 
    "min_elev": 1000
  }, 
  {
    "ID": 4, 
    "catchmentid": 5, 
    "elev_zone": 4, 
    "href": "/catchments/5/timeseries/4", 
    "max_elev": 2000, 
    "min_elev": 1500
  }]
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
