/* Copyright (c) 2005 MySQL AB
  
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
  
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
  
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
 */

#ifndef __GRT_JAVA_H_
#define __GRT_JAVA_H_

#include "myx_grt_private.h"
#ifdef __APPLE__
#include <JavaVM/jni.h>
#else
#include <jni.h>
#endif

typedef struct MYX_GRT_MODULE_LOADER_PRIVATE
{
  JavaVM *jvm;
  JNIEnv *env;
  jclass grt_class;
  jmethodID grt_call_func;

  jclass java_class, java_method;
  jmethodID java_class_getName, java_class_getMethods, java_method_getName, java_method_getReturnType;

  jclass java_string, java_integer, java_double;
  jmethodID java_integer_init, java_integer_intValue, java_double_init, java_double_doubleValue;

  jclass java_grtobject;
  jmethodID java_grtobject_get_id;

  jclass java_grtlist;
  jmethodID java_grtlist_size, java_grtlist_getObject, java_grtlist_getContentType, 
    java_grtlist_getContentStructName;

  jclass java_grthashmap;
  jmethodID java_grthashmap_getKeys, java_grthashmap_getObject, java_grthashmap_getContentType, 
    java_grthashmap_getContentStructName;
} MYX_JAVA_LOADER;


typedef struct MYX_GRT_MODULE_PRIVATE
{
  jclass class_ref;
} MYX_JAVA_MODULE;

typedef struct MYX_GRT_FUNCTION_PRIVATE
{
  char *java_signature;
} MYX_JAVA_FUNCTION;


#endif
