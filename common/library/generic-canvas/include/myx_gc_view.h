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
 * @file myx_gc_view.h
 * @brief Implementation of the view class.
 *
 */

#ifndef MYX_GC_VIEW_H
#define MYX_GC_VIEW_H

#include "myx_gc_base.h"
#include "myx_gc_figure.h"
#include "myx_gc_primitives.h"

class CGenericCanvas;
class CPaperLayer;
class CGridLayer;
class CFeedbackLayer;
class CConnectionLayer;
class CGCView;
class CConnection;
class CConnectionInstance;
class CSelectionEnumerator;
class CFloater;

//----------------- CFigureInstanceEnumerator --------------------------------------------------------------------------

/**
 * The CFigureInstanceEnumerator class is for quick access to all figure instances on all (common) layers in a view.
 * Enumeration happens depth-first. That means for each layer first all instances are enumerated before the next layer
 * is taken.
 */
class GENERIC_CANVAS_API CFigureInstanceEnumerator
{
private:
  CGCView* FView;
  CLayers::iterator FLayerIterator;
  CFigureInstances::iterator FFigureInstanceIterator;
public:
  CFigureInstanceEnumerator(CGCView* view);
  virtual ~CFigureInstanceEnumerator(void) {};

  virtual bool __cdecl hasNext(void);
  virtual CFigureInstance* __cdecl next(void);
  virtual void __cdecl release(void);
  virtual void __cdecl reset(void);
};

//----------------------------------------------------------------------------------------------------------------------

/** Certain states a view can enter. */
#define GC_STATE_DRAG_PENDING        0x00000001
#define GC_STATE_DRAGGING            0x00000002
#define GC_STATE_LBUTTON_DOWN        0x00000004
#define GC_STATE_MBUTTON_DOWN        0x00000008
#define GC_STATE_RBUTTON_DOWN        0x00000010
#define GC_STATE_RUBBER_RECTANGLE    0x00000020
#define GC_STATE_RUBBER_BAND         0x00000040
#define GC_STATE_CLEAR_PENDING       0x00000080
#define GC_STATE_RESIZING            0x00000100
#define GC_STATE_SELECTION_RECTANGLE 0x00000200
#define GC_STATE_OVERVIEW            0x00000400
#define GC_STATE_OVERVIEW_DRAG       0x00000800

/** 
 * A view implements an association between a set of layers and their visual representation on screen.
 * Views can have individual zoom and offset values, viewports and other properties.
 * There can always be only one active view. Views are managed by the canvas class.
 */
class GENERIC_CANVAS_API CGCView: public CGCBase
{
  friend class CGenericCanvas;
  friend class CFigureInstanceEnumerator;
  friend class CViewZoomAnimation;
  friend class CViewOffsetAnimation;
private:
  // A private listener class.
  class CLayerListener: private CGCBaseListener
  {
    friend class CGCView;
  protected:
    CGCView* view;
  public:
    virtual void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action);
    virtual void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
    virtual void __cdecl onDestroy(CGCBase* object);
    virtual void __cdecl onError(CGCBase* sender, CGCBase* origin, const char* message);
  };

  CGridLayer* FGrid;                   // A grid rendered on top of all content.
  CConnectionLayer* FConnectionLayer;  // A special layer only for figure instance connections.
  CFeedbackLayer* FFeedbackLayer;      // The feedback layer is rendered above everything else including the grid.
  CPrimitiveLayer* FPrimitiveLayer;    // The layer for the simple stuff.
  wstring FName;                        // An identifier for this view.
  GLfloat FColor[4];                   // The background color of the viewer.
  TGCViewport FViewport;               // Coordinates of the output area within the viewer window.
  CLayers FLayers;                     // A list of layers that are visible in this view.
  float FZoom;                         // The current zoom factor. It's a normal scaling factor but for the 
                                       // whole view (scales everything).
  float FOffsetX;                      // A translation offset to enable scrolling (panning) in a zoomed scene.
  float FOffsetY;                      // The vertical translation offset.
  float FPrintOffsetX;                 // Translation offsets for printing.
  float FPrintOffsetY;
  float FWorkspaceWidth;               // Width and height of the virtual workspace.
  float FWorkspaceHeight;
  float FNearPlane;                    // The cut off plane that is closest to the eye.
  float FFarPlane;                     // The cut off plane that is farthest from the eye.
  float FJitter;                       // Little offset to add to the viewport values to improve display quality on some boards.
  CLayerListener FListener;            // A listener to get notified if a layer is being destroyed.
  unsigned int FStates;               // Various states for the work of the view.
  CElementLookupCache* FCache;         // Used for quick hit info determination.
  float FOrigin[3];                    // Determines the center position (the virtual (0, 0, 0) position) of the view.

  // Info related to mouse input.
  CFigureInstance* FLastHit;           // The figure instance under the mouse during the last mouse down action.
  int FLastWindowX;                    // Last hit's window coordinates.
  int FLastWindowY;  
  float FLastViewX;                    // Last hit's view coordinates (window coords transformed to view space).
  float FLastViewY;
  bool FDragSelection;                 // If true then the entire selection is dragged, otherwise only the last hit.
  TFeedbackInfo FResizeInfo;           // Keeps the current resize direction if resizing is active.

  // Exposé mode stuff.
  float FLastZoom;                     // The zoom level that was active before exposé mode was activated.
  float FLastOffsetX;                  // Backup for the last offset.
  float FLastOffsetY;
  CFloater* FExposeFloater;            // A floater marking the original display area.
  bool FDirty;
  bool FInAnimation;
protected:
  void activate(void);
  void animationStarted(void);
  void animationStopped(bool zoom, bool offset);
  void applyTransformations(void);
  void drawFocusRect(void);
  TAction* handleAction(TMouseButton button, TMouseEvent event, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords, 
    TFeedbackInfo feedbackInfo, CFigureInstance* instance);
	bool handleMouseDblClick(TMouseButton button, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords);
	bool handleMouseDown(TMouseButton button, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords);
	bool handleMouseMove(TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords);
	bool handleMouseUp(TMouseButton button, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords);
	void internalRender(TGCRenderContent content, const TBoundingBox& visibleBounds);
  void notifyZoomChange(void);
	void render(TGCRenderContent content);
  void renderToMemory(TGCColorFormat format, TGCRenderContent content, float zoom, TGCViewport& bounds, unsigned char* memory);
	void updateCache(void);
  void validate(void);
public:
	CGCView(CGenericCanvas* canvas, string name);
	virtual ~CGCView(void);

	virtual void __cdecl addLayer(CLayer* layer);
	virtual void __cdecl addToSelection(CFigureInstance* instance);
	virtual void __cdecl clearContent(bool removeLayers);
	virtual void __cdecl clearPrimitives(void);
	virtual void __cdecl clearSelection(void);
	virtual void __cdecl color(float red, float green, float blue, float alpha);
	virtual void __cdecl colorV(GLfloat* newColor);
	virtual bool __cdecl contains(CLayer* layer);
	virtual CPrimitive* __cdecl createCircle(const TVertex& center, float ouiterRadius, float innerRadius,
		GLfloat* color, bool filled);
	virtual CConnectionInstance* __cdecl createConnectionInstance(CConnection* connection, CFigureInstance* endPoint1,
		CFigureInstance* endPoint2);
	virtual CFloater* __cdecl createFloater(const TBoundingBox& bounds, GLfloat* color1, GLfloat* color2,
		TFloaterDecoration decoration);
	virtual CPrimitive* __cdecl createLine(const TVertex& start, const TVertex& end, GLfloat* color, GLfloat width,
		GLushort stipple);
	virtual CPrimitive* __cdecl createRectangle(const TBoundingBox& bounds, GLfloat* color, bool filled);
	virtual TFeedbackInfo __cdecl getFeedbackInfo(TModifiers modifiers, int windowX, int windowY);
	virtual CFigureInstanceEnumerator* __cdecl getFigureInstanceEnumerator(void);
	virtual CHitResults* __cdecl getHitTestInfoAt(TVertex point, bool singleHit);
	virtual void __cdecl getLastRubberBounds(TBoundingBox& box);
	virtual void __cdecl getOrigin(float* x, float* y, float* z);
	virtual CSelectionEnumerator* __cdecl getSelectionEnumerator(void);
	virtual void __cdecl getWorkspace(float* width, float* height);
	virtual CGridLayer* __cdecl grid(void) { return FGrid; };
  void handleChange(CGCBase* origin, TGCChangeReason reason);
  virtual bool __cdecl handleMouseInput(TMouseEvent event, TMouseButton button, TModifiers modifiers, int x, int y);
  bool inAnimation(void) { return FInAnimation; };
  virtual float __cdecl jitterGet(void);
  virtual void __cdecl jitterSet(float value);
  virtual float __cdecl offsetXGet(void);
  virtual void __cdecl offsetXSet(float value);
  virtual float __cdecl offsetYGet(void);
  virtual void __cdecl offsetYSet(float value);
  virtual bool __cdecl overviewActive(void);
  virtual void __cdecl overviewStart(bool animated);
  virtual void __cdecl overviewStop(bool returnToZoomed, bool animated);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
  virtual void __cdecl removeFromSelection(CFigureInstance* instance);
  virtual void __cdecl removeLayer(CLayer* layer);
  virtual void __cdecl rubberBandStart(TRubberBandStyle style, const TVertex& coords);
  virtual void __cdecl rubberBandStop(void);
  virtual void __cdecl rubberRectStart(TRubberRectStyle style, const TVertex& coords, bool removeSelection);
  virtual void __cdecl rubberRectStop(void);
  virtual int __cdecl selectionCount(void);
  virtual void __cdecl setOrigin(float x, float y, float z);
  virtual void __cdecl showSelection(bool Visible);
  TGCViewport viewportGet(void) { return FViewport; };
  virtual void __cdecl viewportSet(const TGCViewport& newViewport);
  virtual void __cdecl viewToWindow(const TVertex& coords, int& x, int& y);
  virtual void __cdecl windowToView(int x, int y, TVertex& coords);
  virtual void __cdecl workspaceGet(float &width, float &height);
  virtual void __cdecl workspaceSet(const float width, const float height);
  virtual float __cdecl zoomGet(void);
  virtual void __cdecl zoomSet(float value);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // MYX_GC_VIEW_H
