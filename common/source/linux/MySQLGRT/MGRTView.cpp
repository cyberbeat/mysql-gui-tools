/*
 *  MGRTView.cpp
 *  MySQL GRT
 *
 *  Created by Alfredo Kojima on 05/9/11.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#include "MGRTView.h"

MGRTView::MGRTView(MYX_GRT *grt, MYX_GRT_VALUE *value)
: MGRTObject(grt, value)
{
}


const char *MGRTView::code()
{
  return _object["queryExpression"].asString();
}


void MGRTView::setCode(const char *code)
{
  _object.set("queryExpression", code);
}




