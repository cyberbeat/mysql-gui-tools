//
//  MPtrArray.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Sun Jul 11 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MPtrArray.h"
#include <glib.h>

@implementation MPtrArray

- (id)init
{
  self= [super init];
  if (self)
  {
    values= NULL;
    count= 0;
  }
  return self;
}

- (void)dealloc
{
  g_free(values);
  [super dealloc];
}

- (void)addPointer:(void*)ptr
{
  values= g_realloc(values, sizeof(void*)*++count);
  values[count-1]= ptr;
}

- (void*)pointerAtIndex:(int)index
{
  return values[index];
}

- (int)count
{
  return count;
}

@end
