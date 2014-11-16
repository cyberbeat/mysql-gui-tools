/*
 *  MGRTObject.h
 *  MySQL GRT
 *
 *  Created by Alfredo Kojima on 05/8/17.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */


#ifndef __MGRTOBJECT_H__
#define __MGRTOBJECT_H__

#include <MySQLGRT/MGRTValue.h>
#include <vector>

class MGRTObject 
{
protected:
  MYX_GRT *_grt;
  MGRTValue _original;
  MGRTValue _object;
  
  MGRTValue lookupObject(const char *typeId);
  
public:
  MGRTObject(MYX_GRT *grt, MYX_GRT_VALUE *object);
  virtual ~MGRTObject();
  
  virtual MGRTValue ownerSchema();
  virtual MGRTValue ownerCatalog();
  
  inline MGRTValue value() const { return _object; };
  
  virtual const char *name() const;
  virtual void setName(const char *name);

  virtual const char *comment() const;
  virtual void setComment(const char *comment);

  virtual void revert();
  virtual void commit();
};

#endif
