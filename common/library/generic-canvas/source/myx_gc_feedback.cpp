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
 * @file myx_gc_feedback.cpp 
 * @brief Implementation of the feedback layer and its related classes and structures.
 * 
 */

#include "myx_gc_feedback.h"
#include "myx_gc_canvas.h"

// Fixed floater decoration data.
const GLfloat romanNumber1[] = {
  4.0f, // Number of vertices
  4.5f, 0, 5.438f, 0, 5.438f, 10.0f, 4.5f, 10.0f,
  0     // Number of vertices => 0 == end marker
};
const GLfloat romanNumber2[] = {
  4.0f,
  3.5f, 0, 4.438f, 0, 4.438f, 10.0f, 3.5f, 10.0f, 
  4.0f,
  5.5f, 0, 6.438f, 0, 6.438f, 10.0f, 5.5f, 10.0f,
  0
};
const GLfloat romanNumber3[] = {
  4.0f,
  2.5f, 0, 3.438f, 0, 3.438f, 10.0f, 2.5f, 10.0f, 
  4.0f,
  4.5f, 0, 5.438f, 0, 5.438f, 10.0f, 4.5f, 10.0f,
  4.0f,
  6.5f, 0, 7.438f, 0, 7.438f, 10.0f, 6.5f, 10.0f,
  0
};
const GLfloat romanNumber4[] = {
  4.0f,
  8.59f, 0, 6.6f, 10.0f, 6.1f, 7.486f, 7.59f, 0, 
  5.0f,
  6.6f, 10.0f, 5.6f, 10.0f, 3.611f, 0, 4.611f, 0, 6.1f, 7.486f,  
  4.0f,
  1.6f, 0.0f, 2.538f, 0.0f, 2.538f, 10.0f, 1.6f, 10.0f,
  0
};
const GLfloat romanNumber5[] = {
  4.0f,
  6.479f, 0, 4.989f, 7.486f, 5.489f, 10.0f, 7.479f, 0,
  5.0f,
  4.989f, 7.486f, 3.5f, 0, 2.5f, 0, 4.489f, 10.0f, 5.489f, 10.0f,
  0
};
const GLfloat romanNumber6[] = {
  4.0f,
  5.479f, 0, 3.989f, 7.486f, 4.489f, 10.0f, 6.479f, 0,
  5.0f,
  3.989f, 7.486f, 2.5f, 0, 1.5f, 0, 3.489f, 10.0f, 4.489f, 10.0f,
  4.0f,
  7.5f, 0.0f, 8.438f, 0.0f, 8.438f, 10.0f, 7.5f, 10.0f,
  0
};
const GLfloat romanNumber7[] = {
  4.0f,
  4.479f, 0, 2.989f, 7.486f, 3.489f, 10.0f, 5.479f, 0,
  5.0f,
  2.989f, 7.486f, 1.5f, 0, 0.5f, 0, 2.489f, 10.0f, 3.489f, 10.0f,
  4.0f,
  6.563f, 0.0f, 7.501f, 0.0f, 7.501f, 10.0f, 6.563f, 10.0f,
  4.0f,
  8.563f, 0.0f, 9.501f, 0.0f, 9.501f, 10.0f, 8.563f, 10.0f,
  0
};
const GLfloat romanNumber8[] = {
  4.0f,
  3.979f, 0, 2.489f, 7.486f, 2.989f, 10.0f, 4.979f, 0,
  5.0f,
  2.489f, 7.486f, 1.0f, 0, 0, 0, 1.989f, 10.0f, 2.989f, 10.0f,
  4.0f,
  5.25f, 0.0f, 6.188f, 0.0f, 6.188f, 10.0f, 5.25f, 10.0f,
  4.0f,
  7.188f, 0.0f, 8.126f, 0.0f, 8.126f, 10.0f, 7.188f, 10.0f,
  4.0f,
  9.0f, 0.0f, 9.938f, 0.0f, 9.938f, 10.0f, 9.0f, 10.0f,
  0
};
const GLfloat romanNumber9[] = {
  4.0f,
  8.5f, 10.0f, 4.5f, 0, 3.5f, 0, 7.5f, 10.0f,
  4.0f,
  6.5f, 5.0f, 8.5f, 0, 7.5f, 0, 6.0f, 3.75f,
  4.0f,
  5.5f, 5.0f, 3.5f, 10.0f, 4.5f, 10.0f, 6.0f, 6.25f,
  4.0f,
  1.5f, 0.0f, 2.5f, 0.0f, 2.5f, 10.0f, 1.5f, 10.0f,
  0
};
const GLfloat romanNumber10[] = {
  4.0f,
  7.5f, 10.0f, 3.5f, 0, 2.5f, 0, 6.5f, 10.0f,
  4.0f,
  5.5f, 5.0f, 7.5f, 0, 6.5f, 0, 5.0f, 3.75f, 
  4.0f,
  4.5f, 5.0f, 2.5, 10.0f, 3.5f, 10.0f, 5.0f, 6.25f, 6.5f, 10.0f,
  0
};
const GLfloat* romanNumbers[] = {
  romanNumber1, romanNumber2, romanNumber3, romanNumber4, romanNumber5, 
  romanNumber6, romanNumber7, romanNumber8, romanNumber9, romanNumber10 
};

//----------------- CFloater -------------------------------------------------------------------------------------------

CFloater::CFloater(CGenericCanvas* canvas, const TBoundingBox& bounds, GLfloat* color1, GLfloat* color2, 
                   TFloaterDecoration decoration):CGraphicElement(canvas)
{
  _className = "CFloater";
  FDisplayList = 0;

  FTranslation[0] = bounds.upper.x;
  FTranslation[1] = bounds.upper.y;
  FTranslation[2] = bounds.upper.z;
  FWidth = bounds.lower.x - bounds.upper.x;
  FHeight = bounds.lower.y - bounds.upper.y;
  FDecoration = decoration;
  FColor1[0] = color1[0];
  FColor1[1] = color1[1];
  FColor1[2] = color1[2];
  FColor1[3] = color1[3];
  FColor2[0] = color2[0];
  FColor2[1] = color2[1];
  FColor2[2] = color2[2];
  FColor2[3] = color2[3];
}

//----------------------------------------------------------------------------------------------------------------------

CFloater::~CFloater(void)
{
  if (FDisplayList != 0)
    glDeleteLists(FDisplayList, 1);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Floaters have their own feedback flag, which is returned if the given coordinates are with the floater's bounds.
 *
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The coordinates to check against the floater's bounds in layer space.
 * @result A flag telling either the coordinates are within the floater's bounds or not.
 */
TFeedbackInfo CFloater::getFeedbackInfo(int modifiers, const TVertex& coords)
{
  if (containsPoint(coords.x, coords.y))
    return GC_FI_FLOATER;
  else
    return GC_FI_NONE;
}


//----------------------------------------------------------------------------------------------------------------------

/**
 * Moves the floater to a new position.
 *
 * @param dX The horizontal amount to move.
 * @param d> The vertical amount to move.
 */
void CFloater::move(float dX, float dY)
{
  FTranslation[0] += dX;
  FTranslation[1] += dY;
  makeDirty();
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by path. The path syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property to return.
 * @param index If the property is a list then this is the index into that list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CFloater::propertyGet(const char* name, unsigned int index)
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
              result = "";
              break;
            };
        };
      };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the value of the given property, which must be a simple property.
 *
 * @param name The name of the property.
 * @param index If the property is a list then this is the index into that list.
 * @param Value The new value of the property. Automatic conversion is performed where possible.
 */
void CFloater::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  switch (getContainerID(name))
  {
    case GC_CONTAINER_UNKNOWN:
      {
        switch (getPropertyID(name))
        {
          case GC_PROPERTY_NAME:
            {
              // place holder
              break;
            };
        };
        break;
      };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders this floater.
 */
void CFloater::render(void)
{
  validate();

  glTranslatef(FTranslation[0], FTranslation[1], FTranslation[2]);
  glCallList(FDisplayList);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Validates this floater by creating its display list.
 */
void CFloater::validate(void)
{
  if (dirty())
  {
    CGraphicElement::validate();

    boundsNew();
    boundsAdd(TVertex(FTranslation[0], FTranslation[1], 0));
    boundsAdd(TVertex(FTranslation[0] + FWidth, FTranslation[1] + FHeight, 0));
    boundsFinished();

    if (FDisplayList == 0)
      FDisplayList = glGenLists(1);
    glNewList(FDisplayList, GL_COMPILE);

    if (canvas()->supportsExtension(GC_OE_MULTISAMPLING))
      glDisable(GL_MULTISAMPLE_ARB);

    // Except for the checker board style the floater is always filled.
    if (FDecoration != GC_FLOATER_DECO_CHECKER_BOARD)
    {
      // The interior.
      glColor4fv(FColor1);
      glRectf(0, 0, FWidth, FHeight);
    };

    switch (FDecoration)
    {
      case GC_FLOATER_DECO_CHECKER_BOARD:
        {
          glLineWidth(1);
          float increment = 100;

          bool oddRow = false; 
          float y1 = 0;
          float y2;
          while (y1 < FHeight)
          {
            y2 = y1 + increment;
            if (y2 > FHeight)
              y2 = FHeight;
            bool oddCell = oddRow;
            float x1 = 0;
            float x2;
            while (x1 < FWidth)
            {
              x2 = x1 + increment;
              if (x2 > FWidth)
                x2 = FWidth;
              if (oddCell)
                glColor4fv(FColor1);
              else
                glColor4fv(FColor2);
              glBegin(GL_POLYGON);
                glVertex2f(x1, y1);
                glVertex2f(x2, y1);
                glVertex2f(x2, y2);
                glVertex2f(x1, y2);
              glEnd();

              oddCell = !oddCell;
              x1 = x2;
            };
            oddRow = !oddRow;
            y1 = y2;
          };

          break;
        };
      case GC_FLOATER_DECO_CUT_MARKERS:
        {
          // The transparent rectangle is decorated with 8 short lines two of them at each corner.
          glColor4fv(FColor2);
          glBegin(GL_LINES);
            glVertex2f(-120,    0);
            glVertex2f( -40,    0);
            glVertex2f(   0,  -40);
            glVertex2f(   0, -120);

            glVertex2f(FWidth,        -40);
            glVertex2f(FWidth,       -120);
            glVertex2f(FWidth +  40,    0);
            glVertex2f(FWidth + 120,    0);

            glVertex2f(FWidth +  40, FHeight);
            glVertex2f(FWidth + 120, FHeight);
            glVertex2f(FWidth,       FHeight +  40);
            glVertex2f(FWidth,       FHeight + 120);

            glVertex2f(-120, FHeight);
            glVertex2f( -40, FHeight);
            glVertex2f(   0, FHeight +  40);
            glVertex2f(   0, FHeight + 120);
          glEnd();

          break;
        };
      case GC_FLOATER_DECO_ROMAN_1:
      case GC_FLOATER_DECO_ROMAN_2:
      case GC_FLOATER_DECO_ROMAN_3:
      case GC_FLOATER_DECO_ROMAN_4:
      case GC_FLOATER_DECO_ROMAN_5:
      case GC_FLOATER_DECO_ROMAN_6:
      case GC_FLOATER_DECO_ROMAN_7:
      case GC_FLOATER_DECO_ROMAN_8:
      case GC_FLOATER_DECO_ROMAN_9:
      case GC_FLOATER_DECO_ROMAN_10:
        {
          glPushMatrix();

          // Scale the number so that it fills the entire floater and center it.
          // All numbers are specified in the range (0, 0) - (10, 10).
          float scaleFactor = (FWidth < FHeight ? FWidth : FHeight) / 12; // 12, to have a small border.
          float dX = (FWidth - 10 * scaleFactor) / 2;
          float dY = (FHeight - 10 * scaleFactor) / 2;
          glTranslatef(dX, dY, 0);
          glScalef(scaleFactor, scaleFactor, 1);

          glColor4fv(FColor2);
          int index = 0;
          const GLfloat* vertices = romanNumbers[FDecoration - GC_FLOATER_DECO_ROMAN_1];
          int count = (int) vertices[index++];
          while (count > 0)
          {
            glBegin(GL_POLYGON);
            while (count-- > 0)
            {
              glVertex2f(vertices[index], vertices[index + 1]);
              index += 2;
            };
            glEnd();

            count = (int) vertices[index++];
          };
          glPopMatrix();

          break;
        };
    };

    // The border, always drawn.
    glColor4fv(FColor2);
    glBegin(GL_LINE_LOOP);
      glVertex2f(0, 0);
      glVertex2f(FWidth, 0); 
      glVertex2f(FWidth, FHeight);
      glVertex2f(0, FHeight); 
    glEnd();

    if (canvas()->supportsExtension(GC_OE_MULTISAMPLING))
      glEnable(GL_MULTISAMPLE_ARB);

    glEndList();

    canvas()->checkError();
  };
}

//----------------- CSelectionEnumerator -------------------------------------------------------------------------------

/**
 * Constructor of the enumerator class.
 *
 * @param layer The feedback layer which contains the selection.
 */
CSelectionEnumerator::CSelectionEnumerator(CFeedbackLayer* layer)
{
  FLayer = layer;
  reset();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines if there is a next figure instance to enumerate.
 *
 * @return True if there is still a figure instance otherwise false.
 */
bool CSelectionEnumerator::hasNext(void)
{
  return FIterator != FLayer->FSelection.end();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the next figure instance in the sequence.
 *
 * @return The next figure instance.
 */
CFigureInstance* CSelectionEnumerator::next(void)
{
  CFigureInstance* result = FIterator->first;

  // Advance to next instance.
  ++FIterator;

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Frees this enumerator instance. Usually called by non-C++ languages as memory is managed by the C++ runtime.
 */
void CSelectionEnumerator::release(void)
{
  delete this;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Resets the enumerator to the first figure instance in the canvas.
 */
void CSelectionEnumerator::reset(void)
{
  FIterator = FLayer->FSelection.begin();
}

//----------------- CFeedbackInstanceListener --------------------------------------------------------------------------

void CFeedbackLayer::CFeedbackInstanceListener::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  layer->invalidateBounds((CFigureInstance*) sender);
}

//----------------------------------------------------------------------------------------------------------------------

void CFeedbackLayer::CFeedbackInstanceListener::onDestroy(CGCBase* object)
{
  if (object->classIs("CFloater"))
    layer->removeFloater((CFloater*) object);
  else
    layer->removeFromSelection((CFigureInstance*) object);
}

//----------------- CFeedbackLayer ------------------------------------------------------------------------------------

CFeedbackLayer::CFeedbackLayer(CGCView* view): CLayer("feedback", view->canvas())
{
  _className = "CFeedbackLayer";
  FView = view;
  FHandleSize = 5;
  FRubberRectDisplayList = 0;
  FStates = 0;
  FSelectionDecoration = 0;
  FListener.layer = this;
  FVisibleParts = GC_FBPART_ALL;
}

//----------------------------------------------------------------------------------------------------------------------

CFeedbackLayer::~CFeedbackLayer(void)
{
  clearFloaters();
  clearSelection();
  if (FSelectionDecoration != 0)
    glDeleteLists(FSelectionDecoration, 1);
  if (FRubberRectDisplayList != 0)
    glDeleteLists(FRubberRectDisplayList, 1);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates the display list for the selection decoration, which is shared among all selection entries.
 */
void CFeedbackLayer::createSelectionDecoration(void)
{
  GLubyte Color[4];
  if (!colorByName("Highlight", Color))
  {
    // If the system's highlight color could not be found then use a default one.
    Color[0] = 64;
    Color[1] = 64;
    Color[2] = 128;
  };

  FSelectionDecoration = glGenLists(1);
  glNewList(FSelectionDecoration, GL_COMPILE);

  // The interior.
  /*
  Color[3] = 30;
  glColor4ubv(Color);
  glRectd(0, 1, 1, 0);
  */
  glPushAttrib(GL_LINE_BIT);
  glDisable(GL_LINE_SMOOTH);

  // The border.
  Color[3] = 200;
  glColor4ubv(Color);
  glBegin(GL_LINE_LOOP);
    glVertex2d(0, 1);
    glVertex2d(1, 1); 
    glVertex2d(1, 0);
    glVertex2d(0, 0); 
  glEnd();

  // The handles.
  Color[3] = 100;
  glColor4ubv(Color);
  glPointSize(FHandleSize);
  glBegin(GL_POINTS);
    glVertex2d(0, 1);
    glVertex2d(0.5, 1);
    glVertex2d(1, 1);

    glVertex2d(0, 0.5);
    glVertex2d(1, 0.5);

    glVertex2d(0, 0);
    glVertex2d(0.5, 0);
    glVertex2d(1, 0);
  glEnd();

  glPopAttrib();

  glEndList();

  checkError();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Helper method to add a figure instance to the selection list. No change event is triggered.
 *
 * @param instance The instance to add.
 * @return If the instance was added (because it wasn't already there) then true is returned, otherwise false.
 */
bool CFeedbackLayer::internalAddToSelection(CFigureInstance* instance)
{
  bool result = !instance->FSelected;
  if (result)
  {
    instance->FSelected = true;
    TSelectionEntry* entry = new TSelectionEntry;
    entry->instance = instance;
    entry->dirty = true;
    FSelection[instance] = entry;

    instance->addListener(&FListener);
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Helper method to remove a figure instance from the selection list. No change event is triggered.
 *
 * @param instance The instance to add.
 */
void CFeedbackLayer::internalRemoveFromSelection(CFigureInstance* instance)
{
  if (instance->FSelected)
  {
    if (!instance->destroying())
    {
      instance->FSelected = false;
      instance->removeListener(&FListener);
    };
    CSelectionIterator iterator = FSelection.find(instance);
    if (iterator != FSelection.end())
    {
      delete iterator->second;
      FSelection.erase(iterator);
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders the decorations for all figure instances that are currently selected.
 */
void CFeedbackLayer::renderLayerContent(TBoundingBox bounds)
{
  // Selection decorations.
  if ((GC_FBPART_SELECTION & FVisibleParts) != 0)
  {
    for (CSelectionIterator iterator = FSelection.begin(); iterator != FSelection.end(); ++iterator)
    {
      TSelectionEntry* Entry = iterator->second;
      TBoundingBox localBounds;
      viewToLayer(bounds.lower, localBounds.lower);
      viewToLayer(bounds.upper, localBounds.upper);
      if (boundsIntersect(Entry->bounds, localBounds))
      {
        glPushMatrix();
        glTranslatef(Entry->bounds.upper.x, Entry->bounds.upper.y, 1);
        glScalef(Entry->bounds.lower.x - Entry->bounds.upper.x, Entry->bounds.lower.y - Entry->bounds.upper.y, 1);
        glCallList(FSelectionDecoration);
        glPopMatrix();
      };
    };
  };

  // Floaters.
  if ((GC_FBPART_FLOATERS & FVisibleParts) != 0)
  {
    for (CFloaters::const_iterator iterator = FFloaters.begin(); iterator != FFloaters.end(); ++iterator)
    {
      glPushMatrix();
      (*iterator)->render();
      glPopMatrix();
    };
  };

  // Rubber rectangle.
  if ((GC_FBPART_RUBBERRECT & FVisibleParts) != 0 && (FStates & GC_FBSTATE_RUBBERRECT) != 0)
  {
    // In order to avoid backface culling if the coordinates of the rubber band do not form a counter-clock-wise
    // face we simply disable face culling for the moment.
    glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_CULL_FACE);
    glTranslatef(FRubberBounds.upper.x, FRubberBounds.upper.y, 0);
    glScalef(FRubberBounds.lower.x - FRubberBounds.upper.x, FRubberBounds.lower.y - FRubberBounds.upper.y, 1);
    glCallList(FRubberRectDisplayList);
    glPopAttrib();
  };

  // Rubber band.
  if ((GC_FBPART_RUBBERBAND & FVisibleParts) != 0 && (FStates & GC_FBSTATE_RUBBERBAND) != 0)
  {
    glTranslatef(FRubberBounds.upper.x, FRubberBounds.upper.y, 0);
    glScalef(FRubberBounds.lower.x - FRubberBounds.upper.x, FRubberBounds.lower.y - FRubberBounds.upper.y, 1);
    glCallList(FRubberBandDisplayList);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the number of currently selected items.
 *
 * @result The number of currently selected items.
 */
int CFeedbackLayer::selectionCount(void)
{
  return (int) FSelection.size();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates display lists for all invalid decorations.
 */
void CFeedbackLayer::validateLayerContent(void)
{
  if (FSelectionDecoration == 0)
    createSelectionDecoration();

  for (CSelectionIterator iterator = FSelection.begin(); iterator != FSelection.end(); iterator++)
    if (iterator->second->dirty)
    {
      iterator->second->dirty = false;

      // Update the bounding box of the figure instance.
      TBoundingBox bounds = iterator->second->instance->bounds();

      // Give a small border around the figure instance.
      bounds.upper.x -= 1;
      bounds.upper.y -= 1;
      bounds.lower.x += 1;
      bounds.lower.y += 1;
      iterator->second->bounds = bounds;
    };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds the given figure instance to the current selection.
 *
 * @param instance The instance to be added to the selection. If it is already in the set it won't be added again.
 */
void CFeedbackLayer::addToSelection(CFigureInstance* instance)
{
  if (internalAddToSelection(instance))
  {
    change(instance, GC_CHANGE_SELECTION_ADD);
    if (!updating())
      makeDirty();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes all floaters.
 */
void CFeedbackLayer::clearFloaters(void)
{
  if (FFloaters.size() > 0)
  {
    for (CFloaters::iterator iterator = FFloaters.begin(); iterator != FFloaters.end(); ++iterator)
    {
      CFloater* floater = *iterator;
      floater->removeListener(&FListener);
      delete floater;
    };

    FFloaters.clear();
    if (!updating())
      canvas()->refresh();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes all figure instances from the selection set, making it empty.
 */
void CFeedbackLayer::clearSelection(void)
{
  if (FSelection.size() > 0)
  {
    for (CSelectionIterator iterator = FSelection.begin(); iterator != FSelection.end(); ++iterator)
    {
      CFigureInstance* instance = iterator->second->instance;
      instance->removeListener(&FListener);
      instance->FSelected = false;
      
      delete iterator->second;
    };

    FSelection.clear();
    change(NULL, GC_CHANGE_SELECTION_CLEAR);
    if (!updating())
      makeDirty();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new floater object. The new floater is automatically added to the internal floaters list.
 *
 * @param bounds The bounds of the new floater.
 * @param color1 The normal interior color.
 * @param color2 The border color (also used as alternating color in the checker board).
 * @param decoration The style or additional decoration of the floater.
 * @return The reference of the new floater.
 */
CFloater* CFeedbackLayer::createFloater(const TBoundingBox& bounds, GLfloat* color1, GLfloat* color2, 
                                        TFloaterDecoration decoration)
{
  CFloater* result = new CFloater(canvas(), bounds, color1, color2, decoration);
  result->addListener(&FListener);
  FFloaters.push_back(result);
  makeDirty();

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines what feedback action could be executed at the given position. The returned info is usually used to 
 * set an indicator (e.g. the mouse pointer) to a certain state to reflect what is possible at that point.
 *
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The target position in layer space.
 * @param instance [out] Returns the instance that was hit, if any.
 * @return A flag indicating the possible action state.
 */
TFeedbackInfo CFeedbackLayer::getFeedbackInfo(int modifiers, const TVertex& coords, float zoom, 
                                              CFigureInstance** instance)
{
  TFeedbackInfo result = GC_FI_NONE;

  for (CFloaters::reverse_iterator iterator = FFloaters.rbegin(); iterator != FFloaters.rend(); ++iterator)
  {
    result = (*iterator)->getFeedbackInfo(modifiers, coords);
    if (result != GC_FI_NONE)
      break;
  };

  if (result == GC_FI_NONE)
  {
    // Try to find a decoration that is located at the given position.
    // We examine only those that are not dirty. dirty decorations cannot be in view.
    // Iteration happens backwards to find decorations on top earlier than others.
    float distance = 4 / zoom;
    for (CSelectionIteratorReverse iterator = FSelection.rbegin(); iterator != FSelection.rend(); ++iterator)
    {
      if (!iterator->second->dirty)
      {
        TBoundingBox bounds = iterator->second->bounds;
        if ((coords.x >= (bounds.upper.x - distance)) && (coords.x <= (bounds.lower.x + distance)) &&
          (coords.y > (bounds.upper.y - distance)) && (coords.y <= (bounds.lower.y + distance)))
        {                                                
          // Found a decoration. Check if a handle is hit.
          *instance = iterator->first;

          bool leftColumn = ::fabs(bounds.upper.x - coords.x) <= distance;
          bool middleColumn = ::fabs((bounds.upper.x + bounds.lower.x) / 2 - coords.x) <= distance;
          bool rightColumn = ::fabs(bounds.lower.x - coords.x) <= distance;
          bool topRow = ::fabs(bounds.upper.y - coords.y) <= distance;
          bool middleRow = ::fabs((bounds.upper.y + bounds.lower.y) / 2 - coords.y) <= distance;
          bool bottomRow = ::fabs(bounds.lower.y - coords.y) <= distance;

          if (leftColumn)
          {
            if (topRow)
              result = GC_FI_RESIZE_NORTH_WEST;
            else
              if (middleRow)
                result = GC_FI_RESIZE_WEST;
              else
                if (bottomRow)
                  result = GC_FI_RESIZE_SOUTH_WEST;
          }
          else
            if (middleColumn)
            {
              if (topRow)
                result = GC_FI_RESIZE_NORTH;
              else
                if (bottomRow)
                  result = GC_FI_RESIZE_SOUTH;
            }
            else
              if (rightColumn)
              {
                if (topRow)
                  result = GC_FI_RESIZE_NORTH_EAST;
                else
                  if (middleRow)
                    result = GC_FI_RESIZE_EAST;
                  else
                    if (bottomRow)
                      result = GC_FI_RESIZE_SOUTH_EAST;
              };

          break;
        };
      };
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates and returns a selection entry enumerator.
 *
 * @return A new selection enumerator instance. The caller is responsible for freeing it!
 */
CSelectionEnumerator* CFeedbackLayer::getSelectionEnumerator(void)
{
  return new CSelectionEnumerator(this);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Invalidates the selection decoration of the given instance (or all instances if instance is NULL) so they are recomputed
 * next time the selection layer draws them.
 *
 * @param instance The figure instance whose bounds need recomputation. If this parameter is NULL then all bounds are invalidated.
 */
void CFeedbackLayer::invalidateBounds(CFigureInstance* instance)
{
  if (instance == NULL)
  {
    for (CSelectionIterator iterator = FSelection.begin(); iterator != FSelection.end(); ++iterator)
      iterator->second->dirty = true;
    makeDirty();
  }
  else
    if (instance->FSelected)
    {
      CSelectionIterator iterator = FSelection.find(instance);
      if (iterator != FSelection.end())
      {
        iterator->second->dirty = true;
        makeDirty();
      };
    };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Translates all currently selected figure instances by the given amount.
 *
 * @param x The horizontal amount for moving.
 * @param y The vertical amount for moving.
 * @param z The depth amount for moving.
 * @param accumulative Relative or absolute move.
 */
void CFeedbackLayer::moveSelectedInstances(float x, float y, float z, bool accumulative)
{
  if (FSelection.size() > 0)
  {
    for (CSelectionIterator iterator = FSelection.begin(); iterator != FSelection.end(); iterator++)
    {
      CFigureInstance* instance = iterator->second->instance;
      instance->translate(x, y, z, accumulative);
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given floater from the floater list.
 *
 * @param floater The floater to be removed.
 */
void CFeedbackLayer::removeFloater(CFloater* floater)
{
  CFloaters::iterator iterator = find(FFloaters.begin(), FFloaters.end(), floater);
  if (iterator != FFloaters.end())
    FFloaters.erase(iterator);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given figure instance from the current selection.
 *
 * @param instance The instance to be removed. If it isn't actually selected then nothing happens.
 */
void CFeedbackLayer::removeFromSelection(CFigureInstance* instance)
{
  if (instance->FSelected)
  {
    internalRemoveFromSelection(instance);
    change(instance, GC_CHANGE_SELECTION_REMOVE);
    if (!updating())
      makeDirty();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Method from CLayer overriden to also remove the instance from the current selection if necessary.
 *
 * @param instance The instance to be removed.
 */
void CFeedbackLayer::removeInstance(CFigureInstance* instance)
{
  if (updating())
    internalRemoveFromSelection(instance);
  else
    removeFromSelection(instance);
  CLayer::removeInstance(instance);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * When in rubber band mode then this function extends the current rubber band from the start point to
 * the given coordinates.
 *
 * @param coords The coordinate of the new endpoint in view space.
 */
void CFeedbackLayer::rubberBandResize(const TVertex& coords)
{
  if ((FStates & GC_FBSTATE_RUBBERBAND) != 0)
  {
    viewToLayer(coords, FRubberBounds.lower);
    canvas()->refresh();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Starts the rubber band if none is active currently. Otherwise it does nothing.
 *
 * @param style Determines the visible style of the rubber band.
 * @param coords The coordinates of the start point in view space.
 */
void CFeedbackLayer::rubberBandStart(TRubberBandStyle style, const TVertex& coords)
{
  if ((FStates & GC_FBSTATE_RUBBERBAND) != 0)
    rubberBandStop();

  FStates |= GC_FBSTATE_RUBBERBAND;

  viewToLayer(coords, FRubberBounds.upper);
  FRubberBounds.lower = FRubberBounds.upper;

  if (FRubberBandDisplayList == 0 || FLastRBStyle != style)
  {
    FLastRBStyle = style;
    if (FRubberBandDisplayList == 0)
      FRubberBandDisplayList = glGenLists(1);
    glNewList(FRubberBandDisplayList, GL_COMPILE);

    glPushAttrib(GL_ENABLE_BIT | GL_LINE_BIT);
    glEnable(GL_LINE_SMOOTH);
    glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);

    glColor4f(0, 0, 0, 0.5);
    switch (style)
    {
      case GC_RBSTYLE_SOLID_THIN:   // A one pixel thick solid black line.
        {
          glLineWidth(1);
          break;
        };
      case GC_RBSTYLE_SOLID_THICK:  // A three pixels thick solid black line.
        {
          glLineWidth(3);
          break;
        };
      case GC_RBSTYLE_DOTTED_THIN:  // A one pixel thick dotted black line.
        {
          glLineWidth(1);
          glEnable(GL_LINE_STIPPLE);
          glLineStipple(1, 0xFF);
          break;
        };
      case GC_RBSTYLE_DOTTED_THICK: // A three pixels thick solid black line.
        {
          glLineWidth(3);
          glEnable(GL_LINE_STIPPLE);
          glLineStipple(1, 0xFF);
          break;
        };
    };

    glBegin(GL_LINES);
      glVertex3d(0, 0, 1);
      glVertex3d(1, 1, 1);
    glEnd();

    if (canvas()->supportsExtension(GC_OE_MULTISAMPLING))
      glEnable(GL_MULTISAMPLE_ARB);

    glPopAttrib();
    glEndList();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Stops the rubber band mode if it is active currently. Does nothing if not.
 */
void CFeedbackLayer::rubberBandStop(void)
{
  if ((FStates & GC_FBSTATE_RUBBERBAND) != 0)
  {
    FStates &= ~GC_FBSTATE_RUBBERBAND;
    canvas()->refresh();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * When in rubber rectangle mode then this function extends the current rubber rectangle from the start point to
 * the given coordinates and handles selection/deselection of figure instances.
 *
 * @param coords The coordinate of the new corner in view space.
 * @param Action Determines if and how figure instance selection is to be handled. See TRRSelectionAction for
 *               a description of the various modes.
 */
void CFeedbackLayer::rubberRectResize(const TVertex& coords, TRRSelectionAction Action)
{
  if ((FStates & GC_FBSTATE_RUBBERRECT) != 0)
  {
    TBoundingBox oldBounds = sortBounds(FRubberBounds);
    viewToLayer(coords, FRubberBounds.lower);
    TBoundingBox newBounds = sortBounds(FRubberBounds);

    bool haveSelectionRemoved = false;
    bool haveSelectionAdded = false;

    if (Action != GC_RRACTION_NONE)
    {
      CFigureInstanceEnumerator* enumerator = FView->getFigureInstanceEnumerator();
      switch (Action)
      {
        case GC_RRACTION_SELECT:
          {
            // Select all figure instances if they intersect. Leave all others alone.
            while (enumerator->hasNext())
            {
              CFigureInstance* instance = enumerator->next();
              TBoundingBox instanceBounds = instance->bounds();
              if (!boundsAreEmpty(instanceBounds))
              {
                //if (boundsIntersect(newBounds, instanceBounds))
                if (boundsContainBounds(newBounds, instanceBounds))
                {
                  internalAddToSelection(instance);
                  haveSelectionAdded = true;
                };
              };
            };
            if (haveSelectionAdded)
              change(NULL, GC_CHANGE_SELECTION_ADD);

            break;
          };
        case GC_RRACTION_SELECT_REMOVE:
          {
            // Select figure instances, which intersect, unselect all others.
            while (enumerator->hasNext())
            {
              CFigureInstance* instance = enumerator->next();
              TBoundingBox instanceBounds = instance->bounds();
              if (!boundsAreEmpty(instanceBounds))
              {
                if (instance->FSelected)
                {
                  // instance is selected. See if it is still within the rubberband bounds.
                  // Remove it from the selection list if not.
                  //if (!boundsIntersect(newBounds, instanceBounds))
                  if (!boundsContainBounds(newBounds, instanceBounds))
                  {
                    internalRemoveFromSelection(instance);
                    haveSelectionRemoved = true;
                  };
                }
                else
                {
                  // instance is not selected. Add it to the current selection if its bounds
                  // fall within the rubberband bounds.
                  //if (boundsIntersect(newBounds, instanceBounds))
                  if (boundsContainBounds(newBounds, instanceBounds))
                  {
                    internalAddToSelection(instance);
                    haveSelectionAdded = true;
                  };
                };
              };
            };

            if (haveSelectionRemoved)
              change(NULL, GC_CHANGE_SELECTION_REMOVE);

            if (haveSelectionAdded)
              change(NULL, GC_CHANGE_SELECTION_ADD);

            break;
          };
        case GC_RRACTION_TOGGLE:
          {
            // Switch figure instance selection state if their bounds intersect either
            // the new rubber band bounds or the old, but not both.
            while (enumerator->hasNext())
            {
              CFigureInstance* instance = enumerator->next();
              TBoundingBox instanceBounds = instance->bounds();

              if (!boundsAreEmpty(instanceBounds))
              {
                //if (boundsIntersect(newBounds, instanceBounds) != boundsIntersect(oldBounds, instanceBounds))
                if (boundsContainBounds(newBounds, instanceBounds) != boundsContainBounds(oldBounds, instanceBounds))
                {
                  if (instance->FSelected)
                  {
                    internalRemoveFromSelection(instance);
                    haveSelectionRemoved = true;
                  }
                  else
                  {
                    internalAddToSelection(instance);
                    haveSelectionAdded = true;
                  };
                };
              };
            };

            if (haveSelectionRemoved)
              change(NULL, GC_CHANGE_SELECTION_REMOVE);

            if (haveSelectionAdded)
              change(NULL, GC_CHANGE_SELECTION_ADD);

            break;
          };
      };
      delete enumerator;
    };
    
    if (haveSelectionAdded || haveSelectionRemoved)
    {
      makeDirty();
      change(this, GC_CHANGE_SELECTION_CHANGE);
    }
    else
      canvas()->refresh();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Starts the rubber rectangle if none is active currently. Otherwise it does nothing.
 *
 * @param style Determines the visible style of the rubber rectangle.
 * @param coords The coordinates of the start point in view space.
 * @param removeSelection If true then the current selection will be cleared.
 */
void CFeedbackLayer::rubberRectStart(TRubberRectStyle style, const TVertex& coords, bool removeSelection)
{
  if ((FStates & GC_FBSTATE_RUBBERRECT) != 0)
    rubberRectStop();

  FStates |= GC_FBSTATE_RUBBERRECT;

  if (removeSelection)
    clearSelection();

  viewToLayer(coords, FRubberBounds.upper);
  FRubberBounds.lower = FRubberBounds.upper;

  if (FRubberRectDisplayList == 0 || FLastRRStyle != style)
  {
    FLastRRStyle = style;
    if (FRubberRectDisplayList == 0)
      FRubberRectDisplayList = glGenLists(1);
    glNewList(FRubberRectDisplayList, GL_COMPILE);
    glPushAttrib(GL_LINE_BIT);
    glDisable(GL_LINE_SMOOTH);

    switch (style)
    {
      case GC_RRSTYLE_SOLID_THIN:   // A simple black rectangle with a one pixel wide border.
        {
          GLubyte Color[4];

          if (!colorByName("Highlight", Color))
          {
            // If the system's highlight color could not be found then use a default one.
            Color[0] = 64;
            Color[1] = 64;
            Color[2] = 128;
          };

          Color[3] = 80;
          glColor4ubv(Color);
          glBegin(GL_POLYGON);
            glVertex3d(0, 0, 1);
            glVertex3d(1, 0, 1);
            glVertex3d(1, 1, 1);
            glVertex3d(0, 1, 1);
          glEnd();

          Color[3] = 255;
          glColor4ubv(Color);
          glLineWidth(1);
          glBegin(GL_LINE_LOOP);
            glVertex3d(0, 0, 1);
            glVertex3d(1, 0, 1);
            glVertex3d(1, 1, 1);
            glVertex3d(0, 1, 1);
          glEnd();

          break;
        };
      case GC_RRSTYLE_SOLID_THICK:  // A simple black rectangle with a 3 pixel wide border.
        {
          GLubyte Color[4];

          if (!colorByName("Highlight", Color))
          {
            // If the system's highlight color could not be found then use a default one.
            Color[0] = 64;
            Color[1] = 64;
            Color[2] = 128;
          };

          Color[3] = 80;
          glColor4ubv(Color);
          glBegin(GL_POLYGON);
            glVertex3d(0, 0, 1);
            glVertex3d(1, 0, 1);
            glVertex3d(1, 1, 1);
            glVertex3d(0, 1, 1);
          glEnd();

          Color[3] = 255;
          glColor4ubv(Color);
          glLineWidth(3);
          glBegin(GL_LINE_LOOP);
            glVertex3d(0, 0, 1);
            glVertex3d(1, 0, 1);
            glVertex3d(1, 1, 1);
            glVertex3d(0, 1, 1);
          glEnd();

          break;
        };
      case GC_RRSTYLE_DOTTED_THIN:  // A simple black rectangle with a one pixel wide dotted border.
        {
          glColor3f(0, 0, 0);
          glLineWidth(1);
          glEnable(GL_LINE_STIPPLE);
          glLineStipple(1, 0xFF);
          glBegin(GL_LINE_LOOP);
            glVertex3d(0, 0, 1);
            glVertex3d(1, 0, 1);
            glVertex3d(1, 1, 1);
            glVertex3d(0, 1, 1);
          glEnd();
          break;
        };
      case GC_RRSTYLE_DOTTED_THICK: // A simple black rectangle with a 3 pixel wide dotted border.
        {
          glColor3f(0, 0, 0);
          glLineWidth(3);
          glEnable(GL_LINE_STIPPLE);
          glLineStipple(1, 0xFF);
          glBegin(GL_LINE_LOOP);
            glVertex3d(0, 0, 1);
            glVertex3d(1, 0, 1);
            glVertex3d(1, 1, 1);
            glVertex3d(0, 1, 1);
          glEnd();
          break;
        };
      case GC_RRSTYLE_BLENDED_CHECKERBOARD: // A filled rectangle with a one pixel border and a translucent interior.
        {
          GLubyte Color[4];
          if (!colorByName("Highlight", Color))
          {
            // If the system's highlight color could not be found then use a default one.
            Color[0] = 64;
            Color[1] = 64;
            Color[2] = 128;
          };

          glLineWidth(1);
          for (int row = 0; row < 10; row++)
          {
            for (int cell = 0; cell < 10; cell++)
            {
              if (((row + cell) & 1) == 0)
                Color[3] = 60;
              else
                Color[3] = 70;
              glColor4ubv(Color);
              glBegin(GL_POLYGON);
                glVertex3d(cell * 0.1, row * 0.1, 1);
                glVertex3d(cell * 0.1, (row + 1) * 0.1, 1);
                glVertex3d((cell + 1) * 0.1, (row + 1) * 0.1, 1);
                glVertex3d((cell + 1) * 0.1, row * 0.1, 1);
              glEnd();
            };
          };

          Color[3] = 200;
          glColor4ubv(Color);
          glBegin(GL_LINE_LOOP);
            glVertex3d(0, 0, 1);
            glVertex3d(1, 0, 1);
            glVertex3d(1, 1, 1);
            glVertex3d(0, 1, 1);
          glEnd();
          break;
        };
      case GC_RRSTYLE_BLENDED_DIAGONALS: // A filled rectangle with a one pixel border and a translucent interior.
        {
          GLubyte Color[4];
          if (!colorByName("Highlight", Color))
          {
            // If the system's highlight color could not be found then use a default one.
            Color[0] = 64;
            Color[1] = 64;
            Color[2] = 128;
          };

          glLineWidth(1);

          // Interior.
          int Steps = 50;
          double dStep = 1.0 / Steps;

          // left lower half.
          for (int x = 0; x < Steps; x++)
          {
            if (x % 2 == 0)
              Color[3] = 60;
            else
              Color[3] = 100;

            glColor4ubv(Color);
            glBegin(GL_POLYGON);
              glVertex3d(x * dStep, 0, 1);
              glVertex3d(0, x * dStep, 1);
              glVertex3d(0, (x + 1) * dStep, 1);
              glVertex3d((x + 1) * dStep, 0, 1);
            glEnd();
          };

          // right upper half.
          for (int y = 0; y < Steps; y++)
          {
            if (y % 2 == 0)
              Color[3] = 60;
            else
              Color[3] = 100;

            glColor4ubv(Color);
            glBegin(GL_POLYGON);
              glVertex3d(1, y * dStep, 1);
              glVertex3d(y * dStep, 1, 1);
              glVertex3d((y + 1) * dStep, 1, 1);
              glVertex3d(1, (y + 1) * dStep, 1);
            glEnd();
          };

          // Border.
          Color[3] = 200;
          glColor4ubv(Color);
          glBegin(GL_LINE_LOOP);
            glVertex3d(0, 0, 1);
            glVertex3d(1, 0, 1);
            glVertex3d(1, 1, 1);
            glVertex3d(0, 1, 1);
          glEnd();
          break;
        };
      case GC_RRSTYLE_BLENDED_SOLID: // A filled rectangle with a one pixel border and a translucent interior
      {
        GLubyte Color[4];
        if (!colorByName("Highlight", Color))
        {
          // If the system's highlight color could not be found then use a default one.
          Color[0] = 100;
          Color[1] = 100;
          Color[2] = 100;
        };
        Color[3] = 100;
        
        glLineWidth(1);
        glColor4ubv(Color);
        glBegin(GL_POLYGON);
        glVertex3d(0, 0, 1);
        glVertex3d(1, 0, 1);
        glVertex3d(1, 1, 1);
        glVertex3d(0, 1, 1);
        glEnd();
        
        Color[3] = 200;
        glColor4ubv(Color);
        glBegin(GL_LINE_LOOP);
        glVertex3d(0, 0, 1);
        glVertex3d(1, 0, 1);
        glVertex3d(1, 1, 1);
        glVertex3d(0, 1, 1);
        glEnd();
        break;
      };
    };
    glPopAttrib();
    glEndList();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Stops the rubber rectangle if it is active currently. Does nothing if not.
 */
void CFeedbackLayer::rubberRectStop(void)
{
  if ((FStates & GC_FBSTATE_RUBBERRECT) != 0)
  {
    FStates &= ~GC_FBSTATE_RUBBERRECT;
    canvas()->refresh();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Used to set which part is to be displayed and which remains hidden.
 */
void CFeedbackLayer::visibleParts(TFeedbackParts parts)
{
  FVisibleParts = parts;
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

