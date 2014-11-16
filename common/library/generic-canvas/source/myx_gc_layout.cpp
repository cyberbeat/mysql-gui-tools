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
 * @file myx_gc_layout.cpp 
 * @brief Implementation of the layout computation classes.
 * 
 */

#include "myx_gc_layout.h"

//----------------- CLayouter ------------------------------------------------------------------------------------------

CLayouter::CLayouter(CFigureElement* Element)
{
  FElement = Element;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the layouted bounding box of the given element, which must be a child element of the owner of this layouter.
 *
 * @result The bounding box of the child element after layouting. If this layouter is not owned by a figure element or
 *         the given child does not belong to the owner then the result is an empty box.
 * @note This method resets the current layout chain. This means before using any of the next* functions call reset first.
 */
TBoundingBox CLayouter::boundingBoxForChild(CFigureElement* child)
{
  TBoundingBox result;
  reset();
  if (FElement != NULL)
  {
    CElementList* children = FElement->children();
    while (FIterator != children->end())
    {
      if (*FIterator == child)
      {
        nextBoundingBox(&result);
        break;
      }
      else
      {
        TBoundingBox localBox;
        nextBoundingBox(&localBox);
      };
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the figure element that contains the given point.
 *
 * @param coords The coordinates for the hit test expressed in the coordinate system of the parent element.
 * @return NULL or the element at the given position.
 */
CFigureElement* CLayouter::elementFromPoint(TVertex coords)
{
  if (FIterator != FElement->children()->end())
  {
    CFigureElement* child = *FIterator;

    TBoundingBox localBox;
    nextBoundingBox(&localBox);

    float localX = coords.x - localBox.upper.x;
    float localY = coords.y - localBox.upper.y;
    return child->elementFromPoint(TVertex(localX, localY, 0));
  };
  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Executes the actions of the current element in the layout order.
 * For this to work the given coordinates must be transformed to local coordinates.
 *
 * @param button The mouse button which triggered the call.
 * @param state The button's state.
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The mouse coordinates in element space.
 * @return The last executed action or NULL if none.
 */
TAction* CLayouter::executeAssociatedActions(TMouseButton button, TMouseEvent event, int modifiers, const TVertex& coords)
{
  if (FIterator != FElement->children()->end())
  {
    CFigureElement* child = *FIterator;

    TBoundingBox localBox;
    nextBoundingBox(&localBox);

    float localX = coords.x - localBox.upper.x;
    float localY = coords.y - localBox.upper.y;
    
    TVertex vertex(localX, localY, 0);
    
    return child->executeAssociatedActions(button, event, modifiers, vertex);
  };
  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves feedback info from the current element in layout order.
 *
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The coordinates for the hit test expressed in the coordinate system of the parent element.
 * @return A feedback value telling about possible actions etc. for the element.
 */
TFeedbackInfo CLayouter::getFeedbackInfo(int modifiers, const TVertex& coords)
{
  if (FIterator != FElement->children()->end())
  {
    CFigureElement* child = *FIterator;

    TBoundingBox localBox;
    nextBoundingBox(&localBox);

    float localX = coords.x - localBox.upper.x;
    float localY = coords.y - localBox.upper.y;
    return child->getFeedbackInfo(modifiers, TVertex(localX, localY, 0));
  };
  return GC_FI_NONE;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Tells the caller whether there is still a next value available.
 *
 * @return True, if there is a next value, otherwise false.
 */
bool CLayouter::hasNext(void)
{
  if (FElement == NULL)
    return false;
  else
    return FIterator != FElement->children()->end();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders the current child element and moves on to the next in the list.
 */
void CLayouter::renderNext(void)
{
  if (FIterator != FElement->children()->end())
  {
    CFigureElement* child = *FIterator;

    TBoundingBox localBox;
    nextBoundingBox(&localBox);

    glTranslatef(localBox.upper.x, localBox.upper.y, localBox.upper.z);
    
#if 0 // Render bounding box (for debugging)
    TBoundingBox temp = child->bounds();
    glPushAttrib(GL_CURRENT_BIT | GL_POLYGON_BIT);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glColor3f(0, 0.5, 0);
    glRectf(temp.upper.x, temp.upper.y, temp.lower.x, temp.lower.y);
    glPopAttrib();
#endif 

    child->render();
    glTranslatef(-localBox.upper.x, -localBox.upper.y, -localBox.upper.z);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Resets layout computation to start over from origin.
 */
void CLayouter::reset(void)
{
  FIterator = FElement->children()->begin();
  FX = 0;
  FY = 0;
};

//----------------- CColumnLayouter ------------------------------------------------------------------------------------

/**
 * Distributes the given values equally over all child elements, which can be resized.
 *
 * @paramn dX The horizontal size change part.
 * @paramn dY The vertical size change part.
 * @param info A flag telling more about the resize directions. Used by the figure elements.
 * @return true if any element was actually resized, otherwise false.
 */
bool CColumnLayouter::distributeSizeChange(float dX, float dY, TFeedbackInfo info)
{
  bool result = false;
  int resizeCount = 0;

  // First collect all resizable child elements.
  CElementList* children = FElement->children();
  for (CElementList::iterator iterator = children->begin(); iterator != children->end(); ++iterator)
    if ((*iterator)->canResize(true))
      ++resizeCount;

  // Now compute the partial size change for all resizable child elements.
  float sizePart = 0;
  if (resizeCount > 0)
    sizePart = dY / resizeCount;

  for (CElementList::iterator iterator = children->begin(); iterator != children->end(); ++iterator)
    if ((*iterator)->resize(dX, sizePart, info))
      result = true;

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
  * Returns the transformed bounding box of the next element.
  *
  * @param BoundingBox The bounding box to fill with the new values.
  */
void CColumnLayouter::nextBoundingBox(TBoundingBox* BoundingBox)
{
  TBoundingBox localBox = (*FIterator)->bounds();

  BoundingBox->upper.x = localBox.upper.x;
  BoundingBox->upper.y = FY + localBox.upper.y;
  BoundingBox->upper.z = 0;

  if (FElement->expanded())
    FY += localBox.lower.y;

  BoundingBox->lower.x = localBox.lower.x;
  BoundingBox->lower.y = FY;
  BoundingBox->lower.z = 0;

  if (FIterator != FElement->children()->end())
    ++FIterator;
}

//----------------- CRowLayouter ---------------------------------------------------------------------------------------

/**
 * Distributes the given values equally over all child elements, which can be resized.
 *
 * @paramn dX The horizontal size change part.
 * @paramn dY The vertical size change part.
 * @param info A flag telling more about the resize directions. Used by the figure elements.
 * @return true if any element was actually resized, otherwise false.
 */
bool CRowLayouter::distributeSizeChange(float dX, float dY, TFeedbackInfo info)
{
  bool result = false;
  int resizeCount = 0;

  // First collect all resizable child elements.
  CElementList* children = FElement->children();
  for (CElementList::iterator iterator = children->begin(); iterator != children->end(); ++iterator)
    if ((*iterator)->canResize(false))
      ++resizeCount;

  // Now compute the partial size change for all resizable child elements.
  float sizePart = 0;
  if (resizeCount > 0)
    sizePart = dX / resizeCount;

  for (CElementList::iterator iterator = children->begin(); iterator != children->end(); ++iterator)
    if ((*iterator)->resize(sizePart, dY, info))
      result = true;

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
  * Returns the transformed bounding box of the next element.
  *
  * @param BoundingBox The bounding box to fill with the new values.
  */
void CRowLayouter::nextBoundingBox(TBoundingBox* BoundingBox)
{
  TBoundingBox localBox = (*FIterator)->bounds();

  BoundingBox->upper.x = FX + localBox.upper.x;
  BoundingBox->upper.y = localBox.upper.y;
  BoundingBox->upper.z = 0;

  if (FElement->expanded())
    FX += localBox.lower.x;

  BoundingBox->lower.x = FX;
  BoundingBox->lower.y = localBox.lower.y;
  BoundingBox->lower.z = 0;

  if (FIterator != FElement->children()->end())
    ++FIterator;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Static helper method to create a concrete layouter class for a figure element.
 *
 * @param Element The element for which an instance of a layouter is returned.
 * @return An instance of a new layouter class (or NULL if not supported).
 * @note The caller is responsible to free the returned instance.
 */
CLayouter* LayoutMapper::layouterForElement(CFigureElement* Element)
{
  CLayouter* result = NULL;

  switch (Element->layout())
  {
    case GC_LAYOUT_ROW:
      {
        result = new CRowLayouter(Element);
        break;
      };
    case GC_LAYOUT_COLUMN:
      {
        result = new CColumnLayouter(Element);
        break;
      };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

