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
 * @file myx_gc_style.cpp 
 * @brief Implementation of the base style class.
 * 
 */

#include "myx_gc_style.h"

//----------------- CGCStyle -------------------------------------------------------------------------------------------

CGCStyle::CGCStyle(CGCModel* model, wstring name): CGCBase(model->canvas())
{
  FModel = model;
  FName = name;
  FDisplayList = 0;
  _className = "CGCStyle";
}

//----------------------------------------------------------------------------------------------------------------------

CGCStyle::~CGCStyle(void)
{
  if (FDisplayList != 0)
    glDeleteLists(FDisplayList, 1);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by path. The path syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property to return.
 * @param index The index of the sub property to return if it is located in a list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CGCStyle::propertyGet(const char* name, unsigned int index)
{
  TGCVariant result;

  switch (getContainerID(name))
  {
    case GC_CONTAINER_UNKNOWN:
      {
        switch (getPropertyID(name))
        {
          case GC_PROPERTY_NAME:
            {
              result = utf16ToUtf8(FName);
              break;
            };
          case GC_PROPERTY_OWNER:
            {
              result = FModel;
              break;
            };
        };

        break;
      };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the value of the given property, which must be a simple property.
 *
 * @param name The name of the property.
 * @param index The index of the sub property to return if it is located in a list.
 * @param value The new value of the property. Automatic conversion is performed where possible.
 */
void CGCStyle::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  // There are currently no properties that could be changed. The name is a unique identifier and must not be changed.
}

//----------------------------------------------------------------------------------------------------------------------
