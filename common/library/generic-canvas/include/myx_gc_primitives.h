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
 * @file myx_gc_primitives.h 
 * @brief Implementation of the primitive layer and its associated classes and structures.
 * 
 */

#ifndef __GC_PRIMITIVES_H__
#define __GC_PRIMITIVES_H__

#include "myx_gc_base.h"
#include "myx_gc_layer.h"

/*
#include <hash_set>

using namespace stdext;
*/
//----------------- Primitive layer and associated structures ----------------------------------------------------------

class CPrimitiveLayer;

/**
 * Abstract ancestor class for all primitives.
 */
class GENERIC_CANVAS_API CPrimitive
{
  friend class CPrimitiveLayer;
private:
  CPrimitiveLayer* FLayer;    // The owner of the primitive.
protected:
  GLuint FListId;

  TBoundingBox FBounds;
  GLfloat FColor[4];
  bool FFilled;

  virtual GLuint renderList(void);
  virtual void render(void) = 0;
public:
  CPrimitive(const TBoundingBox& bounds, GLfloat* color, bool filled);
  virtual ~CPrimitive(void);

  virtual void __cdecl release(void) { delete this; };
  virtual void __cdecl setEndPoint(const TVertex& end);
};

typedef vector<CPrimitive*> CPrimitives;

class GENERIC_CANVAS_API CLine: public CPrimitive
{
private:
  float FWidth;
  GLushort FStipple;
protected:
  virtual void render(void);
public:
  CLine(const TVertex& begin, const TVertex& end, GLfloat* color, GLfloat width, GLushort stipple);
};

class GENERIC_CANVAS_API CPolyLine: public CPrimitive
{
protected:
  float FWidth;
  vector<TVertex> FPoints;
  virtual void render(void);
public:
  CPolyLine(GLfloat* color, GLfloat width);
  void addPoint(const TVertex &point);
};

class GENERIC_CANVAS_API CRectangle: public CPrimitive
{
protected:
  virtual void render(void);
public:
  CRectangle(const TBoundingBox& bounds, GLfloat* color, bool filled);
};

class GENERIC_CANVAS_API CCircle: public CPrimitive
{
private:
  TVertex FCenter;
  float FOuterRadius;
  float FInnerRadius;
protected:
  virtual void render(void);
public:
  CCircle(const TVertex& center, float outerRadius, float innerRadius, GLfloat* color, bool filled);

  virtual void __cdecl setCenter(const TVertex& center);
  virtual void __cdecl setInnerRadius(float radius);
  virtual void __cdecl setOuterRadius(float radius);
};

class GENERIC_CANVAS_API CTextLine: public CPrimitive
{
private:
  wstring FText;
  string FFont;
protected:
  virtual void render(void);
public:
  CTextLine(const TVertex& start, const wstring &text, GLfloat* color, const string &fontId);
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * The primitive layer is a special layer variant that renders simple graphical elements like lines and circles.
 * This way it is not necessary to build a full figure. Once a primitive has been created it cannot be modified
 * anymore. For changable graphic elements use figures.
 */
class GENERIC_CANVAS_API CPrimitiveLayer: public CLayer
{
private:
  CPrimitives FPrimitives;        // The primitives to render.
  vector<GLuint> FLists;
protected:
  virtual void renderLayerContent(TBoundingBox bounds);
public:
  CPrimitiveLayer(CGenericCanvas* canvas);
  virtual ~CPrimitiveLayer(void);

  void addPrimitive(CPrimitive* primitive);
  virtual void __cdecl clear(void);
  void removePrimitive(CPrimitive* primitive);
  void updatePrimitive(CPrimitive* primitive);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // __GC_PRIMITIVES_H__
