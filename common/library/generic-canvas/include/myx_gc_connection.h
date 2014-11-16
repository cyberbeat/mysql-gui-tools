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
 * @file myx_gc_connection.h 
 * @brief Implementation of the connectionclass.
 * 
 */

#ifndef __GC_CONNECTION_H__
#define __GC_CONNECTION_H__

#include "myx_gc_figure.h"
#include "myx_gc_utilities.h"
#include "myx_gc_layer.h"

//----------------------------------------------------------------------------------------------------------------------

class CGenericCanvas;
class CConnection;
class CConnectionInstance;
class CConnectionLayer;
class CGCModel;

typedef enum tagConnectionDirection
{
  GC_DIR_NONE,
  GC_DIR_SELF,
  GC_DIR_NORTH,
  GC_DIR_EAST,
  GC_DIR_SOUTH,
  GC_DIR_WEST
} TConnectionDirection;

//----------------- CConnection ----------------------------------------------------------------------------------------

/**
 * CConnectionDecor is a description of what a connection can do (e.g. actions). It is loaded from a description file
 * and created by the figure parser.
 */
class CConnectionDecor: public CBaseTemplate, public CGCBase
{
  friend class CFigureParser;
private:
  wstring FType;                       // The type for which this template can be used.
  wstring FClass;                      // A string describing the (figure) layout the decor is used for.
  CGCModel* FModel;
public:
  CConnectionDecor(CGCModel* model, wstring type, wstring layoutClass);
  virtual ~CConnectionDecor(void);

  wstring layoutClass(void) { return FClass; };
  wstring type(void) { return FType; };
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
};

//----------------------------------------------------------------------------------------------------------------------

/** A class comprising data for a connection. */
class GENERIC_CANVAS_API CConnection: public CGraphicElement
{
  friend class CConnectionInstance;
private:
  // A private listener class for the figures of a connection.
  class CFigureListener: private CGCBaseListener
  {
    friend class CConnection;
  protected:
    CConnection* connection;
  public:
    virtual void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action);
    virtual void __cdecl onDestroy(CGCBase* sender);
    virtual void __cdecl onError(CGCBase* sender, CGCBase* origin, const char* message);
  };

  CFigure* FEnd1;
  CFigure* FEnd2;
  TConnectionDecorations FDecorations;
  CFigureListener FListener;
  CConnectionDecor* FDecor;  // Defines certain abilities (actions etc.) of the connection.
protected:
  TAction* doAction(TAction* action);
public:
  CConnection(CGenericCanvas* canvas, CConnectionDecor* decor, CFigure* endPoint1, CFigure* endPoint2);
  virtual ~CConnection(void);

  virtual TConnectionDecorations __cdecl decorationsGet(void) { return FDecorations; };
  virtual void __cdecl decorationsSet(const TConnectionDecorations& data);
  TAction* executeAssociatedActions(TMouseButton button, TMouseEvent event, TModifiers modifiers, const TVertex& coords);
  void figureDestroyed(CFigure* figure);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
};

//----------------- CConnectionInstance --------------------------------------------------------------------------------

typedef enum tagConnectionPart
{
  GC_CONNECTION_PART_NONE,
  GC_CONNECTION_PART_1,
  GC_CONNECTION_PART_CENTER,
  GC_CONNECTION_PART_2
} TConnectionPart;

/** A concrete instance for a connection. */
class GENERIC_CANVAS_API CConnectionInstance: public CGraphicElement
{
  friend class CConnectionLayer;
private:
  // A private listener class for the parts of a connection instance.
  class CConnectionListener: private CGCBaseListener
  {
    friend class CConnectionInstance;
  protected:
    CConnectionInstance* instance;
  public:
    virtual void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action);
    virtual void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
    virtual void __cdecl onDestroy(CGCBase* sender);
  };

  CConnectionLayer* FLayer;            // The owner layer.
  CConnection* FConnection;
  CFigureInstance* FEnd1;
  CFigureInstance* FEnd2;
  CConnectionListener FListener;
  GLuint FDisplayList;
  TVertex FEndVertex1;
  TVertex FEndVertex2;
  TConnectionDirection FDirection1;    // Direction of the connection with regard to end point 1.
  TConnectionDirection FDirection2;    // Direction of the connection with regard to end point 2.
  TConnectionLineStyle FLineStyle;     // Style of the connection line.
  GLfloat FColor[4];
  GLfloat FHotColor[4];

  // Decorations and labels.
  CFigureInstance* FEnd1Decoration;
  TVertex FLabel1Offset;
  CFigureInstance* FEnd1Label;
  CFigureInstance* FCenterDecoration;
  TVertex FCenterLabelOffset;
  CFigureInstance* FCenterLabel;
  CFigureInstance* FEnd2Decoration;
  TVertex FLabel2Offset;
  CFigureInstance* FEnd2Label;
  float FCenter;                       // A percentual value reaching from 0 through 1 specifying the entire range 
                                        // from start to end point.
protected:
  void applyTransformations(CFigureInstance* target, TVertex* position, TConnectionDirection direction);
  float computeCenterPosition(void);
  TConnectionDirection determineCenterDirection(void);
  TAction* executeAssociatedActions(TMouseButton button, TMouseEvent event, TModifiers modifiers, const TVertex& coords);
  void renderDecoration(TConnectionPart part);
  void updateDecoration(TConnectionPart part);
  void updateFigureInstance(CFigureInstance** instance, CFigure* figure);
  void zoomChanged(float newZoom);
public:
  CConnectionInstance(CConnectionLayer* layer, CConnection* connection, CFigureInstance* endPoint1, 
    CFigureInstance* endPoint2);
  virtual ~CConnectionInstance(void);

  virtual bool __cdecl containsPoint(const float x, const float y);
  virtual float __cdecl center(void) { return FCenter; };
  void computeCoordinates(CFigureInstance* point, TConnectionDirection direction, float slot, bool alternative);
  virtual CFigureInstance* __cdecl endPoint1(void) { return FEnd1; };
  virtual CFigureInstance* __cdecl endPoint2(void) { return FEnd2; };
  TConnectionDirection getDirection(CFigureInstance* point);
  virtual TFeedbackInfo __cdecl getFeedbackInfo(TModifiers modifiers, const TVertex& coords);
  TConnectionPart labelPartFromPoint(float x, float y);
  virtual TConnectionLineStyle __cdecl lineStyleGet(void) { return FLineStyle; };
  virtual void __cdecl lineStyleSet(TConnectionLineStyle newStyle);
  void moveCenterTo(TVertex position);
  void moveLabel(TConnectionPart part, float dX, float dY);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
  void render(bool isHot);
  void updateDecorations(void);
  virtual void __cdecl validate(void);
};

//----------------- Connection layer and associated structures ---------------------------------------------------------

typedef set<CConnectionInstance*> CConnectionInstanceList;
typedef map<CFigureInstance*, CConnectionInstanceList> CFigureConnectionList;

#define GC_CLAYER_STATE_CENTER_DRAGGING     0x0001   // A center line of a connection is being dragged.
#define GC_CLAYER_STATE_VDRAGGING           0x0002   // A vertical center line of a connection is being dragged.
#define GC_CLAYER_STATE_HDRAG_PENDING       0x0004   // A horizontal center line dragging is about to start.
#define GC_CLAYER_STATE_VDRAG_PENDING       0x0008   // A vertical center line dragging is about to start.
#define GC_CLAYER_STATE_DRAG_LABEL_PENDING  0x0010   // A decoration label is about to be dragged.
#define GC_CLAYER_STATE_DRAG_LABEL          0x0020   // A decoration label is being dragged.

/**
 * The connection layer is a special layer variant that renders connections between figures.
 */
class GENERIC_CANVAS_API CConnectionLayer: public CLayer
{
  friend class CGCView;
private:
  // A private listener class.
  class CConnectionInstanceListener: public CGCBaseListener
  {
    friend class CConnectionLayer;
  protected:
    CConnectionLayer* layer;
  public:
    virtual void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
    virtual void __cdecl onDestroy(CGCBase* object);
    virtual void __cdecl onError(CGCBase* sender, CGCBase* origin, const char* message);
  };

  unsigned int FStates;                         // Contains active states of the layer.
  CConnectionInstanceList FConnections;          // All defined connections.
  CFigureConnectionList FConnectionPointList;    // A list of figure instances with their associated connection instances.
  CConnectionInstanceListener FListener;
  CConnectionInstance* FHotConnection;           // When hot tracking is active then this member contains the connection under the mouse pointer.
  CGCView* FView;                                // The view to which this layer belongs.
  CElementLookupCache* FCache;                   // Used for quick hit info determination.

  // Info related to mouse input.
  CConnectionInstance* FLastHit;                 // The figure instance under the mouse during the last mouse down action.
  int FLastWindowX;                              // Last hit's window coordinates.
  int FLastWindowY;  
  float FLastViewX;                              // Last hit's view coordinates (window coords transformed to view space).
  float FLastViewY;
  bool FDragSelection;                           // If true then the entire selection is dragged, otherwise only the last hit.
  TFeedbackInfo FCurrentFeedback;                // Used when a mouse action is in progress to avoid validating the display info 
                                                 // frequently.
  TConnectionPart FLabelPart;                    // If dragging a decoration label the part to which it belongs is kept here.
protected:
  bool handleAction(TMouseButton button, TMouseEvent event, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords);
  void invalidateEndPoint(CFigureInstance* point);
  virtual void renderLayerContent(TBoundingBox bounds);
  void updateCache(void);
  virtual void validateLayerContent(void);
  virtual void zoomChanged(float newZoom);
public:
  CConnectionLayer(CGCView* view);
  virtual ~CConnectionLayer(void);

  HIDESBASE CConnectionInstance* createInstance(CConnection* connection, CFigureInstance* endPoint1, CFigureInstance* endPoint2);
  TFeedbackInfo getFeedbackInfo(TModifiers modifiers, TVertex& coords);
  void getHitTestInfoAt(CHitResults* hits, TVertex point, bool singleHit);
  void handleChange(CConnectionInstance* instance, TGCChangeReason reason);
  bool handleMouseDown(TMouseButton button, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords);
  bool handleMouseMove(TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords);
  bool handleMouseUp(TMouseButton button, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords);
  HIDESBASE void removeInstance(CConnectionInstance* instance);
  void validateEndPoint(CFigureInstance* point, bool handleSelfReferences);
  void workspaceChanged(void);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // __GC_CONNECTION_H__
