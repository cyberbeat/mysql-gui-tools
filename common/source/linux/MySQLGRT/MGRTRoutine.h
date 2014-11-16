/*
 *  MGRTRoutine.h
 *  MySQL GRT
 *
 *  Created by Alfredo Kojima on 05/9/12.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */


#ifndef __MGRTROUTINE_H__
#define __MGRTROUTINE_H__

#include "MGRTObject.h"

#include <string>
#include <map>

class MGRTRoutineGroup : public MGRTObject {
  std::map<std::string,MGRTValue> _routines;
  
public:
  MGRTRoutineGroup(MYX_GRT *grt, MYX_GRT_VALUE *value);

  MGRTValue ownerSchema();
  
  void addRoutine(const MGRTValue &routine);
  void removeRoutine(const MGRTValue &routine);
  
  int routineCount();
  MGRTValue getRoutine(int i);
};

#endif
