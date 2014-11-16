/*
 *  MGRTSchema.cpp
 *  MySQL GRT
 *
 *  Created by Alfredo Kojima on 05/9/11.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#include "MGRTSchema.h"
#include <string>

MGRTSchema::MGRTSchema(MYX_GRT *grt, MYX_GRT_VALUE *value)
: MGRTObject(grt, value)
{
}


const char *MGRTSchema::collation()
{
  return _object["defaultCollationName"].asString();
}

void MGRTSchema::setCollation(const char *code)
{
  if (code && *code)
  {
    _object.set("defaultCollationName", code);
    _object.set("defaultCharacterSetName", std::string(code, (const char*)strchr(code, '_')).c_str());
  }
  else
  {
    _object.set("defaultCollationName", "");
    _object.set("defaultCharacterSetName", "");
  }
}


MGRTValue MGRTSchema::ownerSchema()
{
  return _object;
}
