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
 * @file myx_gc_feedback.h 
 * @brief Implementation of the feedback layer and its related classes and structures.
 * 
 */

#ifndef __GC_FEEDBACK_H__
#define __GC_FEEDBACK_H__

#include "myx_gc_layer.h"

//----------------------------------------------------------------------------------------------------------------------

typedef struct tagSelectionEntry
{
  CFigureInstance* instance;
  bool dirty;
  TBoundingBox bounds;
} TSelectionEntry;

typedef map<CFigureInstance*, TSelectionEntry*> CSelection;
typedef map<CFigureInstance*, TSelectionEntry*>::iterator CSelectionIterator;
typedef map<CFigureInstance*, TSelectionEntry*>::reverse_iterator CSelectionIteratorReverse;

/** Interal states of the feedback layer. */
#define GC_FBSTATE_RUBBERRECT 0x0001
#define GC_FBSTATE_RUBBERBAND 0x0002

/** Possible part switches for the feedback layer. */
#define GC_FBPART_SELECTION  0x0001
#define GC_FBPART_FLOATERS   0x0002
#define GC_FBPART_RUBBERRECT 0x0004
#define GC_FBPART_RUBBERBAND 0x0008
#define GC_FBPART_ALL 0xFFFF

typedef unsigned int TFeedbackParts;

class CFeedbackLayer;

//----------------- CSelectionEnumerator -------------------------------------------------------------------------------

/**
 * Helper class to allow enumeration of the currently selected figure instances. An instance is created by the feedback layer's
 * getSelectionEnumerator method and must be freed by the caller. Non C++ callers must use the enumerator's release method to free
 * the class.
 */
class GENERIC_CANVAS_API CSelectionEnumerator
{
private:
  CFeedbackLayer* FLayer;
  CSelectionIterator FIterator;
public:
  CSelectionEnumerator(CFeedbackLayer* layer);

  virtual bool __cdecl hasNext(void);
  virtual CFigureInstance* __cdecl next(void);
  virtual void __cdecl release(void);
  virtual void __cdecl reset(void);
};

//----------------- CFloater -------------------------------------------------------------------------------------------

/**
 * CFloater is a helper class for the feedback layer to display.
 */
class GENERIC_CANVAS_API CFloater: public CGraphicElement
{
private:
  GLuint FDisplayList;
  GLfloat FTranslation[3];
  float FWidth;
  float FHeight;
  GLfloat FColor1[4];   // Normal interior color. Also used as one color in the checker board.
  GLfloat FColor2[4];   // Normal border color. Also used as the other color in the checkboard.
  TFloaterDecoration FDecoration;
protected:
public:
  CFloater(CGenericCanvas* canvas, const TBoundingBox& bounds, GLfloat* color1, GLfloat* color2, 
    TFloaterDecoration decoration);
  virtual ~CFloater(void);

  HIDESBASE virtual TFeedbackInfo __cdecl getFeedbackInfo(int modifiers, const TVertex& coords);
  void move(float dX, float dY);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
  void render(void);
  virtual void __cdecl validate(void);
};

typedef vector<CFloater*> CFloaters;

//----------------- CFeedbackLayer -------------------------------------------------------------------------------------

/**
 * The feedback layer is a special layer variant that renders decorations for selected figures and can be queried
 * for quick hit tests and lists of selected figures. Additionally it serves as means for various general decorations
 * used to give user feedback.
 */
class GENERIC_CANVAS_API CFeedbackLayer: public CLayer
{
  friend class CSelectionEnumerator;
  friend class CGCView;
private:
  class CFeedbackInstanceListener: public CGCBaseListener
  {
    friend class CFeedbackLayer;
  protected:
    CFeedbackLayer* layer;
  public:
    virtual void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
    virtual void __cdecl onDestroy(CGCBase* object);
  };

  CGCView* FView;                 // The view to which this layer belongs.
  CSelection FSelection;          // The list of currently selected figure instances.
  float FHandleSize;              // The size of the decoration handles.
  unsigned int FStates;          // A number of flags holding transient states of the layer.
  TBoundingBox FRubberBounds;     // The current bounds of the rubber rectangle/band (if active).
  GLuint FRubberRectDisplayList;  // The display list for the rubber rectangle (if active).
  GLuint FRubberBandDisplayList;  // The display list for the rubber band (if active).
  GLuint FSelectionDecoration;    // The display list for the selection decoration.
  CFloaters FFloaters;
  TFeedbackParts FVisibleParts;   // Indicates, which feedback is to be displayed.
  CFeedbackInstanceListener FListener;
  TRubberRectStyle FLastRRStyle;  // Cache for the last rubber rect style to avoid annecessary rebuild of the display list.
  TRubberBandStyle FLastRBStyle;  // Dito for the rubber band.
protected:
  void createSelectionDecoration(void);
  bool internalAddToSelection(CFigureInstance* instance);
  void internalRemoveFromSelection(CFigureInstance* instance);
  virtual void renderLayerContent(TBoundingBox bounds);
  int selectionCount(void);
  virtual void validateLayerContent(void);
public:
  CFeedbackLayer(CGCView* view);
  virtual ~CFeedbackLayer(void);

  void addToSelection(CFigureInstance* instance);
  void clearFloaters(void);
  void clearSelection(void);
  CFloater* createFloater(const TBoundingBox& bounds, GLfloat* color1, GLfloat* color2, TFloaterDecoration decoration);
  virtual TFeedbackInfo getFeedbackInfo(int modifiers, const TVertex& coords, float zoom, CFigureInstance** instance);
  CSelectionEnumerator* getSelectionEnumerator(void);
  void invalidateBounds(CFigureInstance* instance);
  void moveSelectedInstances(float x, float y, float z, bool accumulative);
  virtual void __cdecl removeInstance(CFigureInstance* instance);
  void removeFloater(CFloater* floater);
  void removeFromSelection(CFigureInstance* instance);
  void rubberBandResize(const TVertex& coords);
  void rubberBandStart(TRubberBandStyle style, const TVertex& coords);
  void rubberBandStop(void);
  TBoundingBox rubberBounds(void) { return FRubberBounds; };
  void rubberRectResize(const TVertex& coords, TRRSelectionAction action);
  void rubberRectStart(TRubberRectStyle style, const TVertex& coords, bool removeSelection);
  void rubberRectStop(void);
  void visibleParts(TFeedbackParts parts);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // __GC_FEEDBACK_H__
