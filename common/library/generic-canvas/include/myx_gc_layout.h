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
 * @file myx_gc_layout.h 
 * @brief Implementation of the layouter classes.
 * 
 */

#ifndef __GC_LAYOUT_H__
#define __GC_LAYOUT_H__

#include "myx_gc_figure.h"

//----------------------------------------------------------------------------------------------------------------------

/** Abstract base class for all layouter classes. */
class CLayouter
{
protected:
  float FX, FY;                       // The current coordinates.
  CFigureElement* FElement;            // The element we are layouting.
  CElementList::iterator FIterator;    // The iterator used to go through the child list of the element to layout.
public:
  CLayouter(CFigureElement* Element);
  virtual ~CLayouter(void) {};

  TBoundingBox boundingBoxForChild(CFigureElement* child);
  virtual bool distributeSizeChange(float dX, float dY, TFeedbackInfo info) = 0;
  CFigureElement* elementFromPoint(TVertex coords);
  TAction* executeAssociatedActions(TMouseButton button, TMouseEvent event, int modifiers, const TVertex& coords);
  TFeedbackInfo getFeedbackInfo(int modifiers, const TVertex& coords);
  virtual bool hasNext(void);
  virtual void nextBoundingBox(TBoundingBox* BoundingBox) = 0;
  virtual void renderNext(void);
  void reset(void);
};

class CColumnLayouter: public CLayouter
{
public:
  CColumnLayouter(CFigureElement* Element): CLayouter(Element) {};
  virtual ~CColumnLayouter(void) {};
  virtual bool distributeSizeChange(float dX, float dY, TFeedbackInfo info);
  virtual void nextBoundingBox(TBoundingBox* BoundingBox);
};

class CRowLayouter: public CLayouter
{
public:
  CRowLayouter(CFigureElement* Element): CLayouter(Element) {};

  virtual bool distributeSizeChange(float dX, float dY, TFeedbackInfo info);
  virtual void nextBoundingBox(TBoundingBox* BoundingBox);
};

//----------------- Layout mapper --------------------------------------------------------------------------------------

/** The layout mapper provides a simple way of getting a layouter class for a particular layout. */
class LayoutMapper
{
public:
  static CLayouter* layouterForElement(CFigureElement* Element);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // __GC_LAYOUT_H__
