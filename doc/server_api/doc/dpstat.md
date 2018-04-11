
# View Data Processor Status

Retrieve information about the current status of the data processor

**URL** : `/data_processor`

**Method** : `GET`

**Auth required** : NO

## Success Response

**Code** : `200 OK`

**Content example**

```json
{
  "output": "<data processor messages>", 
  "status": "running", 
  "uptime": "11:05:46.059370"
}
```

or 
```json
{
  "output": null, 
  "status": "idle", 
  "uptime": "None"
}
```
