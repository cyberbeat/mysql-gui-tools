/*
 *  MGRTObject.cpp
 *  MySQL GRT
 *
 *  Created by Alfredo Kojima on 05/8/17.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#include "MGRTObject.h"


MGRTObject::MGRTObject(MYX_GRT *grt, MYX_GRT_VALUE *object)
: _grt(grt), _original(object), _object(myx_grt_value_dup(object))
{
  myx_grt_value_release(_object.grtValue());
}

MGRTObject::~MGRTObject()
{
}




MGRTValue MGRTObject::ownerSchema()
{
  return lookupObject(_object["owner"].asString());
}


MGRTValue MGRTObject::ownerCatalog()
{
  MGRTValue schema(ownerSchema());
  if (schema.isValid())
    return lookupObject(schema["owner"].asString());
  else
    return schema;
}


MGRTValue MGRTObject::lookupObject(const char *typeId)
{
  return MGRTValue(myx_grt_reference_cache_lookup(_grt, typeId));
}


void MGRTObject::revert()
{
  _object.assign(MGRTValue(myx_grt_value_dup(_original.grtValue())));
  myx_grt_value_release(_object.grtValue());
}


void MGRTObject::commit()
{
  MYX_GRT_VALUE *diff;
  
  myx_grt_value_print(_grt, _object);
  
  diff= myx_grt_value_diff_make(_grt, _original.grtValue(), _object.grtValue());
  
  myx_grt_value_diff_apply(_grt, _original.grtValue(), diff);
  
  myx_grt_value_release(diff);
}

const char *MGRTObject::name() const
{
  return _object["name"].asString();
}

void MGRTObject::setName(const char *name)
{
  _object.set("name", name);
}


const char *MGRTObject::comment() const
{
  return _object["comment"].asString();
}


void MGRTObject::setComment(const char *comment)
{
  _object.set("comment", comment);
}

/*
std::vector<const char*>MGRTObject::getCollations()
{
  std::vector<const char*> collations;
  MGRTValue catalog(ownerCatalog());
  if (catalog.isValid())
  {
    MGRTValue charsets(catalog["characterSets"]);
    if (charsets.isValid())
    {
      int i, c= charsets.count();
      
      for (i= 0; i < c; i++)
      {
        MGRTValue chs(MGRTValue::refObject(_grt, charsets[i].asString()));    
        MGRTValue colls(chs["collations"]);
        for (int j= 0; j < colls.count(); j++)
          collations.push_back(colls[j].asString());
      }
    }
  }
  return collations;
}

*/
