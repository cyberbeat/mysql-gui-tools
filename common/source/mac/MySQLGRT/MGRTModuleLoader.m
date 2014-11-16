//
//  MGRTModuleLoader.m
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/8/23.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MGRTModuleLoader.h"
#include "myx_grt_private.h"
#include <objc/objc-class.h>

typedef struct MYX_GRT_FUNCTION_PRIVATE
{
  id target;
  SEL selector;
} MYX_OBJC_FUNCTION;


@implementation MGRTModuleLoader

static MYX_GRT_ERROR objc_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval);


- (id)initWithGRT:(MYX_GRT*)grt
{
  self = [super init];
  if (self != nil) 
  {
    _grt= grt;
  }
  return self;
}



- (MYX_GRT_MODULE_LOADER*)initLoader
{
  MYX_GRT_MODULE_LOADER *loader= g_new0(MYX_GRT_MODULE_LOADER, 1);
  
  loader->grt= _grt;
  loader->loader_type= MYX_OBJC_MODULE_TYPE;
  loader->priv= NULL;
  loader->init_module= NULL;
  loader->call_function= objc_call_function;
  loader->extensions_num= 0;
  loader->extensions= NULL;
  
  return loader;
}


static MYX_GRT_FUNCTION *gatherObjectFunctions(MYX_GRT_MODULE *module, id object, unsigned int *count)
{
  MYX_GRT_FUNCTION *funcList= 0;
  void *iterator = 0;
  struct objc_method_list *mlist;
  
  *count= 0;
  
  mlist = class_nextMethodList([object class], &iterator);
  while (mlist != NULL)
  {
    int i;
    for (i= 0; i < mlist->method_count; i++)
    {
      SEL selector= mlist->method_list[i].method_name;
      if (method_getNumberOfArguments(mlist->method_list+i) == 3 &&
          [object performSelector:@selector(exportsMethod:)
                       withObject:NSStringFromSelector(selector)])
      {
        MYX_OBJC_FUNCTION *priv= g_new0(MYX_OBJC_FUNCTION, 1);
        char *sel= g_strdup([NSStringFromSelector(selector) UTF8String]);
        char *ptr= strchr(sel, ':');
        if (ptr) *ptr= 0;
        
        priv->selector= selector;
        priv->target= object;
        
        *count= *count+1;
        funcList= g_realloc(funcList, sizeof(MYX_GRT_FUNCTION)**count);
        memset(funcList+*count-1, 0, sizeof(MYX_GRT_FUNCTION));
        funcList[*count-1].module= module;
        funcList[*count-1].name= sel;
        funcList[*count-1].priv= priv;
      }
    }
    mlist = class_nextMethodList([object class], &iterator);
  }
  
  return funcList;
}

- (MYX_GRT_MODULE*)registerObject:(id)anObject
                         asModule:(NSString*)moduleName
{
  MYX_GRT_MODULE *module;
  MYX_GRT_ERROR error;
  
  GRT_ENTER(_grt);
  
  // init internal module descriptor
  module= g_new0(MYX_GRT_MODULE, 1);
  
  module->loader= myx_grt_get_loader_of_type(_grt, MYX_OBJC_MODULE_TYPE);
  module->priv= NULL;
  module->name= g_strdup([moduleName UTF8String]);
  module->path= NULL;
  module->functions_num= 0;
  module->functions= gatherObjectFunctions(module, anObject, &module->functions_num);
  module->extends= NULL;
    
  error= myx_grt_add_module(_grt, module);
  if (error != MYX_GRT_NO_ERROR)
    GRT_RETURN(_grt, NULL, MYX_GRT_MODULE *);
  else
    GRT_RETURN(_grt, module, MYX_GRT_MODULE *);
}


static MYX_GRT_ERROR objc_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval)
{
  MYX_OBJC_FUNCTION *bfunc= (MYX_OBJC_FUNCTION*)function->priv;
  MYX_GRT_ERROR error= MYX_GRT_NO_ERROR;
  
  // check if a parameter list was submitted
  if ((myx_grt_value_get_type(value) == MYX_LIST_VALUE) && (myx_grt_bridge_list_item_count(value) > 0))
  {
    unsigned int i;
    NSMutableArray *newArgs= [[NSMutableArray alloc] init];
    
    for (i= 0; i < myx_grt_bridge_list_item_count(value); i++)
    {
      MYX_GRT_VALUE *arg= myx_grt_bridge_list_item_get(value, i, 0);
      
      // if the arg is a string, check if the string starts "global::"
      if ( (myx_grt_value_get_type(arg) == MYX_STRING_VALUE) &&
           str_beginswith(myx_grt_bridge_value_as_string(arg), "global::") )
      {
        // if so, take the "real" grt global value instead of the string
        const char *path= myx_grt_bridge_value_as_string(arg)+8;
        
        arg= myx_grt_dict_item_get_by_path((MYX_GRT *)(function->module->priv),
                                           myx_grt_get_root((MYX_GRT *)(function->module->priv)), path);
      }
      
      switch (myx_grt_value_get_type(arg))
      {
        case MYX_STRING_VALUE:
          [newArgs addObject:[NSString stringWithUTF8String:myx_grt_value_as_string(arg)]];
          break;
        case MYX_INT_VALUE:
          [newArgs addObject:[NSNumber numberWithInt:myx_grt_value_as_int(arg)]];
          break;
        case MYX_REAL_VALUE:
          [newArgs addObject:[NSNumber numberWithDouble:myx_grt_value_as_real(arg)]];
          break;
        default:
          [newArgs addObject:[NSValue valueWithPointer:arg]];
          break;
      }
    }
    *retval= (MYX_GRT_VALUE*)[bfunc->target performSelector:bfunc->selector withObject:newArgs];
    [newArgs release];    
  }
  else
  {
    id theArg= nil;
    
    if (value)
    {
      switch (myx_grt_value_get_type(value))
      {
        case MYX_STRING_VALUE:
          theArg= [NSString stringWithUTF8String:myx_grt_value_as_string(value)];
          break;
        case MYX_INT_VALUE:
          theArg= [NSNumber numberWithInt:myx_grt_value_as_int(value)];
          break;
        case MYX_REAL_VALUE:
          theArg= [NSNumber numberWithDouble:myx_grt_value_as_real(value)];
          break;
        default:
          theArg= [NSValue valueWithPointer:value];
          break;
      }
    }
    
    *retval= (MYX_GRT_VALUE*)[bfunc->target performSelector:bfunc->selector withObject:theArg];
  }
  return error;
}


@end
