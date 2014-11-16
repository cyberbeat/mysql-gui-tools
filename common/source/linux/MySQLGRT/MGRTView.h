/*
 *  MGRTView.h
 *  MySQL GRT
 *
 *  Created by Alfredo Kojima on 05/9/11.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#ifndef __MGRTVIEW_H__
#define __MGRTVIEW_H__

#include "MGRTObject.h"


class MGRTView : public MGRTObject {
public:
  MGRTView(MYX_GRT *grt, MYX_GRT_VALUE *value);
  
  const char *code();
  void setCode(const char *code);
};


#endif
