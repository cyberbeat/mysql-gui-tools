/*
 *  MGRTRoutine.cpp
 *  MySQL GRT
 *
 *  Created by Alfredo Kojima on 05/9/12.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#include "MGRTRoutine.h"


MGRTRoutineGroup::MGRTRoutineGroup(MYX_GRT *grt, MYX_GRT_VALUE *value)
: MGRTObject(grt, value)
{
  for (int i= 0; i < _object["routines"].count(); i++)
  {
    _routines[_object["routines"][i].asString()]= getRoutine(i);
  }
}


MGRTValue MGRTRoutineGroup::ownerSchema()
{
  return lookupObject(_object["owner"].asString());
}


void MGRTRoutineGroup::addRoutine(const MGRTValue &routine)
{
  _object["routines"].append(routine.dictId());
}


void MGRTRoutineGroup::removeRoutine(const MGRTValue &routine)
{
  myx_grt_list_item_del_as_string(_object["routines"].grtValue(),
                                  routine.dictId());
}


int MGRTRoutineGroup::routineCount()
{
  return _object["routines"].count();
}


MGRTValue MGRTRoutineGroup::getRoutine(int i)
{
  if (_routines.find(_object["routines"][i].asString()) == _routines.end())
    return _routines[_object["routines"][i].asString()];
  
  return MGRTValue::refObject(_grt, _object["routines"][i]);
}

