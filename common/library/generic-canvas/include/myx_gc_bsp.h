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
 * @file myx_gc_bsp.h 
 * @brief Implementation of a special BSP (binary space partitioning) tree.
 * 
 */

#ifndef __GC_BSP_H__
#define __GC_BSP_H__

#include "myx_gc_base.h"

class CGraphicElement;
class CHitResults;

//----------------------------------------------------------------------------------------------------------------------
/** 
 * A BSP (binary space partitioning) tree is a useful tool to greatly improve hit tests in complex scenes. This BSP class
 * implements a special BSP tree that takes advantage of certain given aspects like always axis-aligned bounding boxes
 * and 2D-only layout. This allows us to use an axis aligned partition, which always separates space into two equal halfs.
 * Additionally, since we only need the BSP tree for hit tests it is not necessary to split the bounding boxes.
 */
class CBspTree
{
private:
  // The element the tree consists of.
  typedef struct tagBspEntry
  {
    bool isVertical;              // Indicates whether the splitting straight line is vertical or horizontal. With this flag
                                  // we can speed up classification as we only have to compare one value per vertex instead
                                  // solving a line equation.
    TBoundingBox bounds;          // The bounds that this entry covers.
    float splitPoint;             // The actual partition value.
    tagBspEntry* left;            // Pointer to the left subtree.
    tagBspEntry* right;           // Pointer to the right subtree.
    CGraphicElementList elements; // The list of elements convered by this BSP entry.
    
    tagBspEntry()
    {
      isVertical = true;
      left = NULL;
      right = NULL;
    };
  } TBspEntry;

  // Result for the bounding box classification.
  typedef enum tagClassifiction
  {
    IS_UNKNOWN,
    IS_LEFT,
    IS_RIGHT,
    IS_BOTH
  } TClassification;

  TBspEntry FRoot;
  float FMinSize;
protected:
  TClassification classify(TBspEntry* entry, CGraphicElement* element);
  void deleteEntry(TBspEntry* entry);
  CGraphicElement* findElement(TBspEntry* entry, TVertex point);
  void findElements(TBspEntry* entry, TVertex point, CHitResults* hits);
  void insertElement(TBspEntry*, CGraphicElement* element);
  void renderEntry(TBspEntry* entry);
public:
  CBspTree(float width, float height);
  virtual ~CBspTree(void);

  void addElement(CGraphicElement* element);
  void clear(void);
  CGraphicElement* findElement(TVertex point);
  void findElements(TVertex point, CHitResults* hits);
  void render(void);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // __GC_BSP_H__
