
# Trigger Execution of Data Processor

Triggers the immediate execution of the data processor. Can be used e.g. when new catchments have been added.

**URL** : `/data_processor`

**Method** : `PUT`

**Auth required** : YES

## Success Response

**Code** : `202 ACCEPTED`

**Content example**

```json
{
  "output": "<data processor messages>", 
  "status": "RUNNING", 
  "uptime": "0:00:00.665268"
}

```
## Error Responses


**Condition** : If data processor is already running.

**Code** : `409 CONFLICT`
