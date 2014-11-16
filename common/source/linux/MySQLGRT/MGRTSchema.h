/*
 *  MGRTSchema.h
 *  MySQL GRT
 *
 *  Created by Alfredo Kojima on 05/9/11.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#ifndef __MGRTSCHEMA_H__
#define __MGRTSCHEMA_H__

#include "MGRTObject.h"

class MGRTSchema : public MGRTObject {
  
  virtual MGRTValue ownerSchema();
public:
  MGRTSchema(MYX_GRT *grt, MYX_GRT_VALUE *value);
  
  const char *collation();
  void setCollation(const char *code);
};


#endif
