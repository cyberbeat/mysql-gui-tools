/* Copyright (C) 2004 MySQL AB

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */

/**
 * @file myx_gc_variant.h 
 * @brief Implementation of a general purpose variant data type.
 * 
 */

#ifndef __GC_VARIANT_H__
#define __GC_VARIANT_H__

#include "myx_gc.h"

//----------------------------------------------------------------------------------------------------------------------

class CGCBase;

/**
 * Implementation of a variant data type that can carry values of different type.
 */
typedef enum tagGCVariantType
{
  GC_VAR_UNKNOWN,       // The value type is unknown (e.g. because a property does not exist).
  GC_VAR_BOOL,          // The value is a boolean.
  GC_VAR_INT,           // The value is an integer number.
  GC_VAR_FLOAT,         // The value is a floating point number.
  GC_VAR_STRING,        // The value is a sequence of characters.
  GC_VAR_OBJECT         // The value is an object with subproperties (e.g. a figure instance).
} TGCVariantType;

typedef struct GENERIC_CANVAS_API tagGCVariant
{
private:
  TGCVariantType type;
  bool b;
  int i;                          // Values are not in a union because of the string entry.
  float f;
  string s;
  CGCBase* reference;

public:
  tagGCVariant()
  {
    type = GC_VAR_UNKNOWN;
    b = false;
    i = 0;
    f = 0;
    reference = NULL;
  };

  tagGCVariant& operator = (const tagGCVariant& other);
  tagGCVariant& operator = (const bool& b);
  operator bool();
  tagGCVariant& operator = (const int& i);
  operator int();
  tagGCVariant& operator = (const float& f);
  operator float();
  tagGCVariant& operator = (const string& s);
  operator string();
  tagGCVariant& operator = (const char* s);
  tagGCVariant& operator = (CGCBase* object);
  operator CGCBase*();
  template <class T> T* cast(void)
  {
    if (type == GC_VAR_OBJECT)
      return (T*) reference;
    else
      return NULL;
  };
  CGCBase* operator -> ();
  tagGCVariant& operator += (const string& s);
  tagGCVariant& operator += (const char* s);

  bool isNonNullReference(void) { return type == GC_VAR_OBJECT && reference != NULL; };
  bool isReference(void) { return type == GC_VAR_OBJECT; };
  bool isString(void) { return type == GC_VAR_STRING; };
  bool isValid(void) { return type != GC_VAR_UNKNOWN; };
} TGCVariant;


//----------------------------------------------------------------------------------------------------------------------

#endif // __GC_VARIANT_H__
