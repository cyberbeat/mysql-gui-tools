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
 * @file myx_gc_style.h 
 * @brief Implementation of the style class.
 * 
 */

#ifndef __GC_STYLE_H__
#define __GC_STYLE_H__

#include "myx_gc_base.h"
#include "myx_gc_figure.h"
#include "myx_gc_model.h"

//----------------------------------------------------------------------------------------------------------------------

class CSVGParser;
class CGCModel;

/** A compiled style with its associated bounding box. */
class CGCStyle: public CGCBase
{
  friend class CSVGParser;
private:
  CGCModel* FModel;
  wstring FName;                  // The name of the style.
  TBoundingBox FBoundingBox;      // The box that covers the whole content of this style.
  GLuint FDisplayList;            // An OpenGL display list to render this style element.
public:
  CGCStyle(CGCModel* model, wstring name);
  virtual ~CGCStyle(void);

  const TBoundingBox& boundingBox(void) { return FBoundingBox; };
  GLuint displayList(void) { return FDisplayList; };
  wstring name(void) { return FName; };
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // __GC_STYLE_H__
