# Delete Catchment

Delete a catchment. This effectively only tags the catchment for deletion. The catchment data are deleted by the next execution of the data processor. However, catchments that are tagged for deletion as well as their related data do not show up in any request and are thus invisible.

**URL** : `/catchments/<catchmentid>`

**Method** : `DELETE`

**Auth required** : YES

## Success Response

**Code** : `204 NO CONTENT`

## Error Responses


**Condition** : If \<catchmentid> does not exist

**Code** : `404 NOT FOUND`
	
### Or

**Condition** : The catchment with ID=1 (masterregion) can not be deleted.

**Code** : `418 I'M A TEAPOT`

