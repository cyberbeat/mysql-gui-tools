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
 * @file myx_gc_variant.cpp 
 * @brief Implementation of a general purpose variant data type.
 * 
 */

#include "myx_gc_variant.h"
#include "myx_gc_datatypes.h"

//----------------------------------------------------------------------------------------------------------------------

tagGCVariant& tagGCVariant::operator = (const tagGCVariant& other)
{
  type = other.type;
  b = other.b;
  i = other.i;
  f = other.f;
  s = other.s;
  reference = other.reference;

  return (*this);
};

//----------------------------------------------------------------------------------------------------------------------

tagGCVariant& tagGCVariant::operator = (const bool& b)
{
  type = GC_VAR_BOOL;
  this->b = b;

  return (*this);
};

//----------------------------------------------------------------------------------------------------------------------

tagGCVariant::operator bool()
{
  bool result = false;

  switch (type)
  {
    case GC_VAR_UNKNOWN:
      {
        result = false;
        break;
      };
    case GC_VAR_BOOL:
      {
        result = b;
        break;
      };
    case GC_VAR_INT:
      {
        result = i > 0;
        break;
      };
    case GC_VAR_FLOAT:
      {
        result = f > 0;
        break;
      };
    case GC_VAR_STRING:
      {
        if (s == "true" || s == "yes" || s == "t" || s == "y")
          result = true;
        else
        {
          float value = (float) atof(s.c_str());
          result = value > 0;
        };
        break;
      };
    case GC_VAR_OBJECT:
      {
        result = reference != NULL;
        break;
      };
  };

  return result;
};

//----------------------------------------------------------------------------------------------------------------------

tagGCVariant& tagGCVariant::operator = (const int& i)
{
  type = GC_VAR_INT;
  this->i = i;

  return (*this);
};

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Converts a GC variant to an integer.
 *
 * @param variant The variant to convert.
 * @return The value of the variant as integer.
 */
tagGCVariant::operator int()
{
  int result = 0;

  switch (type)
  {
    case GC_VAR_UNKNOWN:
      {
        result = 0;
        break;
      };
    case GC_VAR_BOOL:
      {
        result = b ? 1 : 0;
        break;
      };
    case GC_VAR_INT:
      {
        result = i;
        break;
      };
    case GC_VAR_FLOAT:
      {
        result = ROUND(f);
        break;
      };
    case GC_VAR_STRING:
      {
        result = atoi(s.c_str());
        break;
      };
    case GC_VAR_OBJECT:
      {
        result = 0;
        break;
      };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

tagGCVariant& tagGCVariant::operator = (const float& f)
{
  type = GC_VAR_FLOAT;
  this->f = f;

  return (*this);
};

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Converts a GC variant to a float.
 *
 * @param variant The variant to convert.
 * @return The value of the variant as float.
 */
tagGCVariant::operator float()
{
  float result = 0;

  switch (type)
  {
    case GC_VAR_UNKNOWN:
      {
        result = 0;
        break;
      };
    case GC_VAR_BOOL:
      {
        result = b ? 1.0f : 0.0f;
        break;
      };
    case GC_VAR_INT:
      {
        result = (float) i;
        break;
      };
    case GC_VAR_FLOAT:
      {
        result = f;
        break;
      };
    case GC_VAR_STRING:
      {
        result = (float) atof(s.c_str());
        break;
      };
    case GC_VAR_OBJECT:
      {
        result = 0;
        break;
      };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

tagGCVariant& tagGCVariant::operator = (const string& s)
{
  type = GC_VAR_STRING;
  this->s = s;

  return (*this);
};

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Converts a GC variant to a string.
 *
 * @param variant The variant to convert.
 * @return The value of the variant as string.
 */
tagGCVariant::operator string()
{
  string result;

  switch (type)
  {
    case GC_VAR_UNKNOWN:
      {
        result = "unknown";
        break;
      };
    case GC_VAR_BOOL:
      {
        result = b ? "true" : "false";
        break;
      };
    case GC_VAR_INT:
      {
        char buffer[12];
        sprintf(buffer, "%i", i);
        result = buffer;
        break;
      };
    case GC_VAR_FLOAT:
      {
        char buffer[100];
        #ifdef _WINDOWS
          // Microsoft has confirmed that there is no guaranteed NULL termination (see MSDN for details).
          // Hence we better add one.
          _snprintf(buffer, sizeof(buffer), "%gi\0", f);
        #else
          snprintf(buffer, sizeof(buffer), "%g", f);
        #endif // #ifdef _WINDOWS

        result = buffer;
        break;
      };
    case GC_VAR_STRING:
      {
        result = s;
        break;
      };
    case GC_VAR_OBJECT:
      {
        result = "object";
        break;
      };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

tagGCVariant& tagGCVariant::operator = (const char* s)
{
  type = GC_VAR_STRING;
  this->s = s;

  return (*this);
};

//----------------------------------------------------------------------------------------------------------------------

tagGCVariant& tagGCVariant::operator = (CGCBase* object)
{
  type = GC_VAR_OBJECT;
  this->reference = object;

  return (*this);
};

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Converts a GC variant to a reference.
 *
 * @param variant The variant to convert.
 * @return The value of the variant as reference or NULL if the variant does not represent a reference.
 */
tagGCVariant::operator CGCBase* ()
{
  if (type == GC_VAR_OBJECT)
    return reference;
  else
    return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Converts a GC variant to a reference.
 *
 * @param variant The variant to convert.
 * @return The value of the variant as reference or NULL if the variant does not represent a reference.
 */
CGCBase* tagGCVariant::operator -> ()
{
  if (type == GC_VAR_OBJECT)
    return reference;
  else
    return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Defines the concatination operator for strings and variants. The variant is implicitly converted to a string.
 */
tagGCVariant& tagGCVariant::operator += (const string& s)
{
  string temp = *this;
  temp += s;
  type = GC_VAR_STRING;
  this->s = temp;

  return (*this);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Defines the concatination operator for strings and variants. The variant is implicitly converted to a string.
 */
tagGCVariant& tagGCVariant::operator += (const char* s)
{
  string temp = *this;
  temp += s;
  type = GC_VAR_STRING;
  this->s = temp;

  return (*this);
}

//----------------------------------------------------------------------------------------------------------------------

