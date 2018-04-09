# Timeseries

Retrieve a specific timeseries

**URL** : `/catchments/<catchmentid>/timeseries/<timeseriesid>`

**Method** : `GET`

**Auth required** : NO

**URL query parameters** : from, to = YYYY-MM-DD

**URL query example**

```
/catchments/5/timeseries/1?from=2000-02-26&to=2000-03-07
```

## Success Response

**Code** : `200 OK`

**Content example**

```json
{
  "2000-02-26": 47.3465683236854, 
  "2000-02-27": 47.0618977296459, 
  "2000-02-28": 46.7636538734989, 
  "2000-02-29": 46.565789049036, 
  "2000-03-01": 48.0729027235611, 
  "2000-03-02": 44.8457247561137, 
  "2000-03-03": 43.9263472816218, 
  "2000-03-04": 43.2096833196563, 
  "2000-03-05": 43.3000039311267, 
  "2000-03-06": 42.3627851186906, 
  "2000-03-07": 41.5630523624023
}
```

## Error Response

* **Condition** : If \<catchmentid> or \<timeseriesid> does not exist

	**Code** : `404 NOT FOUND`


* **Condition** : When URL query parameternames or -arguments can not be recognized

	**Code** : `400 BAD REQUEST ERROR`

