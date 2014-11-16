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
 * @file myx_gc_primitives.cpp 
 * @brief Implementation of the primitive layer and its associated classes and structures.
 * 
 */

#include "myx_gc_primitives.h"
#include "myx_gc_canvas.h"

//----------------- CPrimitive -----------------------------------------------------------------------------------------

const TBoundingBox empty;

CPrimitive::CPrimitive(const TBoundingBox& bounds, GLfloat* color, bool filled)
{
  FLayer = NULL;
  FBounds = bounds;
  if (color != NULL)
  {
    FColor[0] = color[0];
    FColor[1] = color[1];
    FColor[2] = color[2];
    FColor[3] = color[3];
  }
  else
  {
    memset(FColor, 0, sizeof(FColor));
  };
  FFilled = filled;
  FListId = 0;
}


//----------------------------------------------------------------------------------------------------------------------

CPrimitive::~CPrimitive(void)
{
  if (FLayer != NULL)
    FLayer->removePrimitive(this);

  if (FListId != 0)
    glDeleteLists(FListId, 1);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates the display list for this primitive.
 *
 * @param A newly created display list.
 */
GLuint CPrimitive::renderList(void)
{
  if (FListId == 0)
    FListId = glGenLists(1);
  glNewList(FListId, GL_COMPILE);
  render();
  glEndList();

  return FListId;
}

//----------------------------------------------------------------------------------------------------------------------

void CPrimitive::setEndPoint(const TVertex& end)
{
  FBounds.lower = end;
}

//----------------- CLine ----------------------------------------------------------------------------------------------

CLine::CLine(const TVertex& begin, const TVertex& end, GLfloat* color, GLfloat width, GLushort stipple): 
  CPrimitive(empty, color, false)
{
  FBounds.upper = begin;
  FBounds.lower = end;
  FWidth = width;
  FStipple = stipple;
}

//----------------------------------------------------------------------------------------------------------------------

void CLine::render(void)
{
  glColor4fv(FColor);

  glPushAttrib(GL_ENABLE_BIT | GL_LINE_BIT);
  glLineWidth(FWidth);
  if (FStipple == 0 || FStipple == 0xFFFF)
    glDisable(GL_LINE_STIPPLE);
  else
  {
    glEnable(GL_LINE_STIPPLE);
    glLineStipple(1, FStipple);
  };
  glBegin(GL_LINES);
    glVertex3f(FBounds.upper.x, FBounds.upper.y, FBounds.upper.z);
    glVertex3f(FBounds.lower.x, FBounds.lower.y, FBounds.lower.z);
  glEnd();
  glPopAttrib();
}

//----------------- CPolyLine ------------------------------------------------------------------------------------------

CPolyLine::CPolyLine(GLfloat* color, GLfloat width): CPrimitive(empty, color, false)
{
  FWidth= width;
}

//----------------------------------------------------------------------------------------------------------------------

void CPolyLine::addPoint(const TVertex &point)
{
  FPoints.push_back(point);
}

//----------------------------------------------------------------------------------------------------------------------

void CPolyLine::render(void)
{
  if (FPoints.size() > 0)
  {
    glColor4fv(FColor);

    glPushAttrib(GL_ENABLE_BIT | GL_LINE_BIT);
    glLineWidth(FWidth);  
    glDisable(GL_LINE_STIPPLE);

    glBegin(GL_LINE_STRIP);
      for (vector<TVertex>::const_iterator iterator = FPoints.begin(); iterator != FPoints.end(); ++iterator)
        glVertex3f(iterator->x, iterator->y, iterator->z);
    glEnd();
    glPopAttrib();
  };
}

//----------------- CRectangle -----------------------------------------------------------------------------------------

CRectangle::CRectangle(const TBoundingBox& bounds, GLfloat* color, bool filled): CPrimitive(bounds, color, filled)
{
}

//----------------------------------------------------------------------------------------------------------------------

void CRectangle::render(void)
{
  glColor4fv(FColor);
  if (FFilled)
    glRectf(FBounds.upper.x, FBounds.upper.y, FBounds.lower.x, FBounds.lower.y);
  else
  {
    glBegin(GL_LINE_LOOP);
      glVertex2f(FBounds.upper.x, FBounds.upper.y);
      glVertex2f(FBounds.lower.x, FBounds.upper.y);
      glVertex2f(FBounds.lower.x, FBounds.lower.y);
      glVertex2f(FBounds.upper.x, FBounds.lower.y);
    glEnd();
  };
}

//----------------- CCircle --------------------------------------------------------------------------------------------

CCircle::CCircle(const TVertex& center, float outerRadius, float innerRadius, GLfloat* color, bool filled): 
  CPrimitive(empty, color, filled)
{
  FOuterRadius = outerRadius;
  FInnerRadius = innerRadius;
  setCenter(center);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Moves the circle to a new location by modifying its center.
 *
 * @param center The new center point for the circle.
 */
void CCircle::setCenter(const TVertex& center)
{
  FCenter = center;
  FBounds.upper.x = center.x - FOuterRadius;
  FBounds.upper.y = center.y - FOuterRadius;
  FBounds.lower.x = center.x + FOuterRadius;
  FBounds.lower.y = center.y + FOuterRadius;
}


//----------------------------------------------------------------------------------------------------------------------

/**
 * Changes the size of the hole in the circle.
 *
 * @param radius The new inner radius for the circle.
 */
void CCircle::setInnerRadius(float radius)
{
  FInnerRadius = radius;
}


//----------------------------------------------------------------------------------------------------------------------

/**
 * Changes the overall size of the circle.
 *
 * @param radius The new outer radius for the circle.
 */
void CCircle::setOuterRadius(float radius)
{
  FOuterRadius= radius;
  setCenter(FCenter);
}

//----------------------------------------------------------------------------------------------------------------------

void CCircle::render(void)
{
  glColor4fv(FColor);
  GLUquadricObj *quadric = gluNewQuadric();

  float startAngle = 0;
  float sweepAngle = 360;
  GLint slices = 40;
  GLint loops = 4;

  gluQuadricOrientation(quadric, GLU_INSIDE);
  glPushMatrix();
  glTranslatef(FCenter.x, FCenter.y, FCenter.z);
  if (FFilled)
    gluQuadricDrawStyle(quadric, GLU_FILL);
  else
    gluQuadricDrawStyle(quadric, GLU_SILHOUETTE);
  gluQuadricNormals(quadric, GLU_SMOOTH);
  gluQuadricTexture(quadric, GL_FALSE);

  gluPartialDisk(quadric, FInnerRadius, FOuterRadius, slices, loops, startAngle, sweepAngle);

  glPopMatrix();
  gluDeleteQuadric(quadric);
}

//----------------- CTextLine ------------------------------------------------------------------------------------

CTextLine::CTextLine(const TVertex& start, const wstring &text, GLfloat *color, const string &fontId)
  : CPrimitive(TBoundingBox(start, start), color, true)
{
  FText= text;
  FFont= fontId;
}

//----------------------------------------------------------------------------------------------------------------------

void CTextLine::render(void)
{
  glColor4fv(FColor);

  glRasterPos2f(FBounds.lower.x, FBounds.lower.y);
  fontManager()->textOut(FText, FFont);
}

//----------------- CPrimitiveLayer ------------------------------------------------------------------------------------

CPrimitiveLayer::CPrimitiveLayer(CGenericCanvas* canvas):CLayer("primitives", canvas)
{
}

//----------------------------------------------------------------------------------------------------------------------

CPrimitiveLayer::~CPrimitiveLayer(void)
{
  clear();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders all primitives in the layer.
 *
 * @param bounds The area which must be rendered.
 */
void CPrimitiveLayer::renderLayerContent(TBoundingBox bounds)
{
  glListBase(0);
  glCallLists((GLsizei) FLists.size(), GL_UNSIGNED_INT, &FLists.front());
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds an existing primitive to the internal list.
 *
 * @param primitive The primitive to add.
 */
void CPrimitiveLayer::addPrimitive(CPrimitive* primitive)
{
  CPrimitives::iterator iterator = find(FPrimitives.begin(), FPrimitives.end(), primitive);
  if (iterator == FPrimitives.end())
  {
    if (primitive->FLayer != NULL)
      primitive->FLayer->removePrimitive(primitive);
    primitive->FLayer = this;
    FPrimitives.push_back(primitive);

    // Create a display list for the new primitive and keep it for quick rendering.
    FLists.push_back(primitive->renderList());
    
    makeDirty();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Destroys all primitives.
 */
void CPrimitiveLayer::clear(void)
{
  CLayer::clear();

  beginUpdate();
  for (CPrimitives::const_iterator iterator = FPrimitives.begin(); iterator != FPrimitives.end(); ++iterator)
    delete *iterator;
  FPrimitives.clear();
  FLists.clear();
  endUpdate();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given primitive from the internal list without destroying it.
 *
 * @param primitive The primitive to remove.
 */
void CPrimitiveLayer::removePrimitive(CPrimitive* primitive)
{
  if (!updating())
  {
    CPrimitives::iterator iterator = find(FPrimitives.begin(), FPrimitives.end(), primitive);
    if (iterator != FPrimitives.end())
    {
      vector<GLuint>::difference_type offset = iterator - FPrimitives.begin();
      (*iterator)->FLayer = NULL;
      FPrimitives.erase(iterator);

      vector<GLuint>::iterator iterator = FLists.begin();
      FLists.erase(iterator + offset);

      makeDirty();
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Updates the display list of the given primitive (e.g. because its content changed).
 *
 * @param primitive The primitive to update.
 */
void CPrimitiveLayer::updatePrimitive(CPrimitive* primitive)
{
  primitive->renderList();
  makeDirty();
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

