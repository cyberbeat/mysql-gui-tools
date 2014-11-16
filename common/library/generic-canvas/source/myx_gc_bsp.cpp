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
 * @file myx_gc_bsp.cpp 
 * @brief Implementation of a special BSP (binary space partitioning) tree.
 * 
 */

#include "myx_gc_bsp.h"

//----------------- CBspTree -------------------------------------------------------------------------------------------

/**
 * Constructor of the tree class.
 *
 * @param width The width of the area we have to cover.
 * @param height The height of the area we have to cover.
 */
CBspTree::CBspTree(float width, float height)
{
  // The min size is our recursion stopper. No BSP entry is created with a width or height
  // smaller than the minimum size.
  FMinSize = 32;

  // The first split is vertical because usually a scene has a larger width than height.
  FRoot.isVertical = true;
  FRoot.bounds.lower.x = width;
  FRoot.bounds.lower.y = height;
  FRoot.splitPoint = width / 2;
}

//----------------------------------------------------------------------------------------------------------------------

CBspTree::~CBspTree(void)
{
  clear();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Classifies the element's bounding box against the given BSP entry, that is, determines whether the bounds of the element
 * lie entirely on the left or on the right side of splitting line or on both sides.
 * Note: left/right can actually be above/below, depending on the splitter orientation, but for classification it does not matter.
 *
 * @param entry The entry of the BSP tree against which we classify.
 * @param element The element to classify.
 */
CBspTree::TClassification CBspTree::classify(TBspEntry* entry, CGraphicElement* element)
{
  TClassification result = IS_UNKNOWN;

  const TBoundingBox& bounds = element->bounds();
  if (entry->isVertical)
  {
    int leftCount = 0;
    int rightCount = 0;
    if (bounds.upper.x <= entry->splitPoint)
      ++leftCount;
    else
      ++rightCount;
    if (bounds.lower.x <= entry->splitPoint)
      ++leftCount;
    else
      ++rightCount;
    if (leftCount == 0)
      result = IS_RIGHT;
    else
      if (rightCount == 0)
        result = IS_LEFT;
      else
        result = IS_BOTH;
  }
  else
  {
    int aboveCount = 0;
    int belowCount = 0;
    if (bounds.upper.y <= entry->splitPoint)
      ++aboveCount;
    else
      ++belowCount;
    if (bounds.lower.y <= entry->splitPoint)
      ++aboveCount;
    else
      ++belowCount;
    if (aboveCount == 0)
      result = IS_RIGHT;
    else
      if (belowCount == 0)
        result = IS_LEFT;
      else
        result = IS_BOTH;
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given entry and all its child entries from the tree.
 *
 * @param entry The entry to be removed.
 */
void CBspTree::deleteEntry(TBspEntry* entry)
{
  if (entry->left != NULL)
    deleteEntry(entry->left);
  if (entry->right != NULL)
    deleteEntry(entry->right);

  delete entry;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Recurses down the subtree given by entry to find an element that occupies the given position.
 *
 * @param entry The top element of the subtree to search through.
 * @param point The position to look for.
 * @param singleHit True if only one hit is requested.
 */
CGraphicElement* CBspTree::findElement(TBspEntry* entry, TVertex point)
{
  CGraphicElement* result = NULL;

  if (entry != NULL)
  {
    if (entry->left != NULL || entry->right != NULL)
    {
      if (entry->isVertical)
      {
        if (point.x <= entry->splitPoint)
          result = findElement(entry->left, point);
        else
          result = findElement(entry->right, point);
      }
      else
      {
        if (point.y <= entry->splitPoint)
          result = findElement(entry->left, point);
        else
          result = findElement(entry->right, point);
      };
    }
    else
    {
      // Found a leaf. Return the last entry that fits as it is the top most.
      for (CGraphicElementList::reverse_iterator iterator = entry->elements.rbegin(); iterator != entry->elements.rend(); ++iterator)
      {
        CGraphicElement* element = *iterator;
        if (element->visible() && element->containsPoint(point.x, point.y))
        {
          result = element;
          break;
        };
      };
    };
  };
  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Recurses down the subtree given by entry to find elements that occupy the given position.
 *
 * @param entry The top element of the subtree to search through.
 * @param point The position to look for.
 * @param singleHit True if only one hit is requested.
 */
void CBspTree::findElements(TBspEntry* entry, TVertex point, CHitResults* hits)
{
  if (entry != NULL)
  {
    if (entry->left != NULL || entry->right != NULL)
    {
      if (entry->isVertical)
      {
        if (point.x <= entry->splitPoint)
          findElements(entry->left, point, hits);
        else
          findElements(entry->right, point, hits);
      }
      else
      {
        if (point.y <= entry->splitPoint)
          findElements(entry->left, point, hits);
        else
          findElements(entry->right, point, hits);
      };
    }
    else
    {
      // Found a leaf. Check its elements. Go backwards through the list to get the latest element
      // (which is rendered as top most) first.
      for (CGraphicElementList::reverse_iterator iterator = entry->elements.rbegin(); iterator != entry->elements.rend(); ++iterator)
      {
        CGraphicElement* element = *iterator;
        if (element->visible() && element->containsPoint(point.x, point.y))
          hits->addHit(*iterator);
      };
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Main method to add an element to the tree. It calls itself recursively.
 *
 * @param entry The current entry used for classification.
 * @param element The element to instert.
 */
void CBspTree::insertElement(TBspEntry* entry, CGraphicElement* element)
{
  // Check if we have reached the smallest splitting size. If so add the element to the given entry,
  // otherwise recurse down.
  float width = entry->bounds.lower.x - entry->bounds.upper.x;
  float height = entry->bounds.lower.y - entry->bounds.upper.y;
  if (width <= FMinSize || height <= FMinSize)
    entry->elements.push_back(element);
  else
  {
    TClassification classification = classify(entry, element);

    if (classification == IS_LEFT || classification == IS_BOTH)
    {
      // Go down the left sub tree. Create it if not yet done.
      if (entry->left == NULL)
      {
        TBspEntry* newEntry = new TBspEntry();
        entry->left = newEntry;
        newEntry->isVertical = !entry->isVertical;
        newEntry->bounds = entry->bounds;
        if (entry->isVertical)
        {
          newEntry->bounds.lower.x = (newEntry->bounds.lower.x + newEntry->bounds.upper.x) / 2;
          newEntry->splitPoint = (newEntry->bounds.lower.y + newEntry->bounds.upper.y) / 2;
        }
        else
        {
          newEntry->bounds.lower.y = (newEntry->bounds.lower.y + newEntry->bounds.upper.y) / 2;
          newEntry->splitPoint = (newEntry->bounds.lower.x + newEntry->bounds.upper.x) / 2;
        };
      };
      insertElement(entry->left, element);
    };

    if (classification == IS_RIGHT || classification == IS_BOTH)
    {
      // Go down the right sub tree. Create it if not yet done.
      if (entry->right == NULL)
      {
        TBspEntry* newEntry = new TBspEntry();
        entry->right = newEntry;
        newEntry->isVertical = !entry->isVertical;
        newEntry->bounds = entry->bounds;
        if (entry->isVertical)
        {
          newEntry->bounds.upper.x = (newEntry->bounds.lower.x + newEntry->bounds.upper.x) / 2;
          newEntry->splitPoint = (newEntry->bounds.lower.y + newEntry->bounds.upper.y) / 2;
        }
        else
        {
          newEntry->bounds.upper.y = (newEntry->bounds.lower.y + newEntry->bounds.upper.y) / 2;
          newEntry->splitPoint = (newEntry->bounds.lower.x + newEntry->bounds.upper.x) / 2;
        };
      };

      insertElement(entry->right, element);
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Used as debugging aid. Renders the given entry via OpenGL.
 *
 * @param entry The subtree to render.
 */
void CBspTree::renderEntry(TBspEntry* entry)
{
  static GLubyte colorVerticalLeft[4] = {255, 0, 0, 10};
  static GLubyte colorVerticalRight[4] = {0, 255, 0, 10};
  static GLubyte colorHorizontalLeft[4] = {0, 0, 255, 10};
  static GLubyte colorHorizontalRight[4] = {255, 255, 0, 10};

  if (entry->left != NULL)
  {
    if (entry->isVertical)
      glColor4ubv(colorVerticalLeft);
    else
      glColor4ubv(colorHorizontalLeft);

    glBegin(GL_POLYGON);
      glVertex2f(entry->bounds.upper.x, entry->bounds.upper.y);
      glVertex2f(entry->bounds.lower.x, entry->bounds.upper.y);
      glVertex2f(entry->bounds.lower.x, entry->bounds.lower.y);
      glVertex2f(entry->bounds.upper.x, entry->bounds.lower.y);
    glEnd();

    renderEntry(entry->left);
  };

  if (entry->right != NULL)
  {
    if (entry->isVertical)
      glColor4ubv(colorVerticalRight);
    else
      glColor4ubv(colorHorizontalRight);

    glBegin(GL_POLYGON);
      glVertex2f(entry->bounds.upper.x, entry->bounds.upper.y);
      glVertex2f(entry->bounds.lower.x, entry->bounds.upper.y);
      glVertex2f(entry->bounds.lower.x, entry->bounds.lower.y);
      glVertex2f(entry->bounds.upper.x, entry->bounds.lower.y);
    glEnd();

    renderEntry(entry->right);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds the given element to the tree according to its bounds.
 *
 * @param element The element to add.
 */
void CBspTree::addElement(CGraphicElement* element)
{
  insertElement(&FRoot, element);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes all entries from the tree.
 */
void CBspTree::clear(void)
{
  if (FRoot.left != NULL)
  {
    deleteEntry(FRoot.left);
    FRoot.left = NULL;
  };
  if (FRoot.right != NULL)
  {
    deleteEntry(FRoot.right);
    FRoot.right = NULL;
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the first (top most) element in the cache at the given position or NULL if there is none.
 *
 * @param point The position to look at (view space).
 * @return NULL or the top most element at that position.
 */
CGraphicElement* CBspTree::findElement(TVertex point)
{
  return findElement(&FRoot, point);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Searches the tree for a given position and returns the elements located there.
 *
 * @param point The point at which we look for elements.
 * @param singleHit True if only hit is required.
 * @param hits A list that takes all found elements.
 */
void CBspTree::findElements(TVertex point, CHitResults* hits)
{
  findElements(&FRoot, point, hits);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders the areas defined currently in the tree as an debug aid using normal OpenGL calls.
 * A rendering context must be active and setup correctly for the output.
 */
void CBspTree::render(void)
{
  renderEntry(&FRoot);
}

//----------------------------------------------------------------------------------------------------------------------

