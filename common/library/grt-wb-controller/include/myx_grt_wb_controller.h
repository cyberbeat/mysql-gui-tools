
// private header

#ifndef __MYX_GRT_WB_CONTROLLER_H__
#define __MYX_GRT_WB_CONTROLLER_H__


#include <myx_public_interface.h>
#ifdef __APPLE__
#include <MySQLGRT/myx_grt_public_interface.h>
#else
#include <myx_grt_public_interface.h>
#endif
#include <myx_grt_wb_public_interface.h>
#include <myx_gc_canvas.h>
#include <myx_gc_figure.h>

#include <list>

#include "GraphRenderer.h"

class BaseController;
class ModelController;
class LayerController;
class CanvasViewController;
class ElementController;
class WorkbenchController;

class RemakableObject
{
private:
  WorkbenchController *_wbench;
  bool _pendingRemake;
public:
  RemakableObject(WorkbenchController *wb);
  virtual ~RemakableObject();
  virtual void setPendingRemake();
  virtual void remake() = 0;
  virtual void remakeDone();
};


class WorkbenchController : public CGCBaseListener
{
  MYX_GRT *_grt;
  CGenericCanvas *_canvas;
  ModelController *_modelCtl;
  
  map<string,BaseController*> _objectTable; // _id to controller
  list<RemakableObject*> _pendingRemakes;
  
  void *_callbackData;
  void (*_viewChanged)(MYX_GRT *Runtime, MYX_GRT_VALUE *view, CGCView *gcView, MYX_WB_GC_CHANGE change, void *data);
  void (*_layerChanged)(MYX_GRT *Runtime, MYX_GRT_VALUE *layer, CFigureInstance *gcLayer, MYX_WB_GC_CHANGE change, void *data);
  void (*_elementChanged)(MYX_GRT *Runtime, MYX_GRT_VALUE *elemen, CFigureInstance *gcElem, MYX_WB_GC_CHANGE change, void *data);

  void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
 
public:
  bool initializing;
    
  WorkbenchController(MYX_GRT *grt);
  ~WorkbenchController();
  MYX_GRT *grt() { return _grt; };
  ModelController *model() { return _modelCtl; };

  void setCanvas(CGenericCanvas *canvas);
  CGenericCanvas *canvas() { return _canvas; };

  void setCallbacks(void *data,
                    void (*view_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *view, CGCView *gcView, MYX_WB_GC_CHANGE change, void *data),
                    void (*layer_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *layer, CFigureInstance *gcLayer, MYX_WB_GC_CHANGE change,  void *data),
                    void (*element_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *element, CFigureInstance *gcElem, MYX_WB_GC_CHANGE change, void *data));
  
  void notifyView(CanvasViewController *view, MYX_WB_GC_CHANGE change);
  void notifyLayer(LayerController *layer, MYX_WB_GC_CHANGE change);
  void notifyElement(ElementController *elem, MYX_WB_GC_CHANGE change);

  void setPendingRemake(RemakableObject *element);
  void processPendingRemakes();
 
  bool initObject(MYX_GRT_VALUE *object);
  CanvasViewController *initObjectForView(CGCView *view);

  BaseController* objectForId(string oid);
  void rememberObject(BaseController *object);
  
  void getObjectAtPoint(float x, float y, MYX_GRT_VALUE *&figure, MYX_GRT_VALUE *&object, MYX_GRT_VALUE *&detail);
};


class BaseController
{
  friend class WorkbenchController;
  
protected:
  WorkbenchController *_wb;
  ModelController *_ownerModel;
  MYX_GRT_VALUE *_value;
  bool _valueRetained;
  const char *_type;
  
public:
  BaseController(WorkbenchController *wb, MYX_GRT_VALUE *value);
  BaseController(WorkbenchController *wb);
  virtual ~BaseController();
  virtual void postCreate() {};
  
  const char *type() { return _type; };

  virtual bool valueFromGrt(const char *key, MYX_GRT_VALUE *value);
  virtual bool updateFromGrt(const char *key, MYX_GRT_VALUE *value);
  virtual bool updateToGrt(const char *key, MYX_GRT_VALUE *value);
  
  void setModel(ModelController *model);
  
  void release();
  inline MYX_GRT_VALUE *value() { return _value; };
  
  const char *getStringValue(const char *key);
  int setStringValue(const char *key, const char *s);
  int getIntValue(const char *key);
  MYX_GRT_VALUE *getValue(const char *key);
  MYX_GRT_VALUE *getRefValue(const char *key);
  double getDoubleValue(const char *key);
  bool referenceIsSet(const char *key);

  string description();
};


class ModelController : public BaseController
{
  CGenericCanvas *_canvas;

  map<string,CanvasViewController*> _viewControllers;
  
public:
  ModelController(WorkbenchController *wb, CGenericCanvas *canvas, MYX_GRT_VALUE *value);
  virtual bool valueFromGrt(const char *key, MYX_GRT_VALUE *value);
  virtual bool updateFromGrt(const char *key, MYX_GRT_VALUE *value);
  CGenericCanvas *canvas() { return _canvas; };
    
  void changedCurrentView(CanvasViewController *view);
  CanvasViewController* currentView();

  void addViewRef(CanvasViewController *view);
  void removeViewRef(CanvasViewController *view);
  
  void deleteView(CanvasViewController *view);
  
  void rememberViewController(CanvasViewController *object);  
  CanvasViewController *controllerForCanvasView(CGCView *object);
  
  ElementController *controllerForElement(CGCBase *element);
};


class CanvasViewController : public BaseController, public CGCBaseListener
{
  friend class LayerController;
  
  CGCView *_gcView; // only set once added to the model
  CLayer *_gcLayer;

  std::list<ElementController*> _pendingElementRelocations;
  std::list<LayerController*> _pendingLayerRelocations;
  
  LayerController *_rootLayer;
  
  float _rubberX, _rubberY;
  bool _rubberStartPending, _rubberStopPending;
    
  CFigure *_connectionParts[4];
  CFigure *_connectionCenterParts[4];

  LayerController *findEnclosingLayer(const TBoundingBox &bounds);
  bool _needRealization;

  void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);

public:
  CanvasViewController(WorkbenchController *wb, MYX_GRT_VALUE *value);
  CanvasViewController(WorkbenchController *wb, CGCView *gcView);
  virtual ~CanvasViewController();
  virtual bool updateFromGrt(const char *key, MYX_GRT_VALUE *value);
  virtual bool valueFromGrt(const char *key, MYX_GRT_VALUE *value);
  virtual void postCreate();

  void changeRelationshipStyle(const char *style);
  void updateRelationships();

  void decorateConnection(CConnection *connection,
                          bool sourceMandatory, bool sourceMany,
                          bool destMandatory, bool destMany,
                          CFigure *end1Label, CFigure *end2Label,
                          CFigure *centerLabel);

  void setupForRubberbandStart(float x, float y);
  void setupForRubberbandStop();
  void doPendingViewTasks();

  void notifyTaskComplete();
  void notifyTaskCanceled();
  
  void addPendingElementRelocation(ElementController *elem);
  void addPendingLayerRelocation(LayerController *layer);
  
  CGCView *gcView() { return _gcView; };
  CLayer *gcLayer() { return _gcLayer; };
  void realizeView();

  void relocateElementToContainerLayer(ElementController *element);
  void relocateLayerToContainerLayer(LayerController *layer);
  void relocateElementsToLayer(LayerController *layer);
  
  void raiseLayer(LayerController *layer);
  
  void relocatePendingObjects();

  void autoArrangeElements(bool selectedOnly);

  void updateSelectionList();

  void clear();

  bool isObjectUnder(CFigureInstance *object1, CFigureInstance *object2);
    
  void deleteElement(ElementController *elem);
  void deleteLayer(LayerController *layer);

  void rememberLayerController(LayerController *object);  
  LayerController *controllerForCanvasLayer(CLayer *object);
  
  static BaseController* create(WorkbenchController *wb, MYX_GRT_VALUE *value)
  {
    return new CanvasViewController(wb, value);
  }
};


// Note that the WB LayerController is not related to the GC layer.
// All WB layers of the same view are in a single GC layer.
class LayerController : public BaseController, public CGCBaseListener, public RemakableObject
{
  enum {
    ApplySize=1,
    ApplyPos=2,
    ApplyColor=4,
    
    ApplyAll= (ApplySize|ApplyPos|ApplyColor)
  };
  
  friend class CanvasViewController;
  CanvasViewController *_view;

  LayerController *_parent;
  list<LayerController*> _layers;
  
  CFigure *_gcFigure;
  CFigureInstance *_gcFigureInstance;
  
  bool _moved;
  bool _pendingDestroy;
  
  LayerController *layerAbove();
  LayerController *layerUnder();

  void applyFrameProperties(int flags);
  void createFrame();

  virtual void setPendingRemake();
  virtual void setPendingDestroy();
  virtual void remake();
  virtual void remakeDone();
  
  void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
  
  LayerController *sublayerContainingBounds(const TBoundingBox &bounds);
  void moveTo(float newX, float newY, bool dragging=false);
public:
  LayerController(WorkbenchController *wb, MYX_GRT_VALUE *value);
  virtual ~LayerController();
  virtual bool updateFromGrt(const char *key, MYX_GRT_VALUE *value);
  virtual bool valueFromGrt(const char *key, MYX_GRT_VALUE *value);
  void setView(CanvasViewController *view);
  inline CanvasViewController *view() { return _view; };
  inline CFigureInstance *layerFrame() { return _gcFigureInstance; };
  inline LayerController *getParent() { return _parent; };

  void setElementStyleColor(CFigureElement *element, const std::string &color);
  
  ElementController *realizedElementBelow(ElementController *elem);
  ElementController *frontmostElement(bool realizedOnly=false);
  ElementController *backmostElement(bool realizedOnly=false);
    
  void clearPendingRelocation() { _moved= false; };
  
  TBoundingBox grtBounds();
  TBoundingBox gcBounds();
  
  void addSublayer(LayerController *layer);
  void removeSublayer(LayerController *layer);
  
  LayerController *findEnclosingLayer(const TBoundingBox &bounds);
  bool containsBounds(const TBoundingBox &bounds, bool totally=true);

  void restackElements();
  
  void addElementRef(ElementController *element);
  void removeElementRef(ElementController *element);

  void raiseElement(ElementController *element, ElementController *above);
  void lowerElement(ElementController *element, ElementController *under);
  
  static BaseController* create(WorkbenchController *wb, MYX_GRT_VALUE *value)
  {
    return new LayerController(wb, value);
  }

};


class ElementController : public BaseController, public CGCBaseListener, public RemakableObject
{
  friend class LayerController;

  char *formatColumn(MYX_GRT_VALUE *column);
  
  void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action);
  void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
  
protected:
  CFigure *_gcFigure;
  CFigureInstance *_gcFigureInstance;
  LayerController *_layer;
  
  bool _initialSizeSet;
  
  bool _pendingChange;
  bool _pendingDestroy;
  bool _moved;
  
  static int grtListenerCallback(MYX_GRT *grt, MYX_GRT_VALUE *value, MYX_GRT_VALUE_CALLBACK_REASON reason, void *data);
  void setupListener(MYX_GRT_VALUE *value);

  virtual void onElementChange(CFigureElement *element);
  
  virtual void fixupElementStyle(CFigureElement *element, MYX_GRT_VALUE *data);
  
  virtual CFigureTemplate* layoutType();
  
  void addPartsFromDict(CFigureElement *content, MYX_GRT_VALUE *source, const string &path);
  void addPartsFromList(CFigureElement *content, MYX_GRT_VALUE *source, const string &path);
    
  void updatePartsFromDict(CFigureElement *content, MYX_GRT_VALUE *source, const string &path);
  void updatePartsFromList(CFigureElement *content, MYX_GRT_VALUE *source, const string &path);
public:
  ElementController(WorkbenchController *wb, MYX_GRT_VALUE *value);
  virtual void assembleFigure();
  virtual bool realizeFigure();
  virtual void unrealizeFigure();
  virtual bool realizeConditionsMet();
  virtual void remakeDone();
  virtual void remake();
  virtual void applyProperties();
  virtual void moveTo(double x, double y);
  virtual bool valueFromGrt(const char *key, MYX_GRT_VALUE *value);
  void setPendingDestroy();

  virtual void setPendingRemake();
  
  void clearPendingRelocation() { _moved= false; };
  
  TBoundingBox grtBounds();
  TBoundingBox gcBounds();
  
  virtual TBoundingBox detailFrame(MYX_GRT_VALUE *detail) { return grtBounds(); };

  virtual MYX_GRT_VALUE *objectData();

  inline CFigure *gcFigure() { return _gcFigure; };
  inline CFigureInstance* gcInstance() { return _gcFigureInstance; };
  
  void setLayer(LayerController *lctl, bool fromLayer);
  LayerController* layer() { return _layer; };
  
  static BaseController* create(WorkbenchController *wb, MYX_GRT_VALUE *value)
  {
    return new ElementController(wb, value);
  }
};


class RelationshipController : public BaseController, public RemakableObject, public CGCBaseListener
{
  CConnection *_gcConnection;
  CConnectionInstance *_gcConnectionInstance;
  CFigure *_gcLabel1, *_gcLabel2, *_gcCenter;

  CanvasViewController *_view;

  void __cdecl onDestroy(CGCBase *sender);
  void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
  void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action);

public:
  RelationshipController(WorkbenchController *wb, MYX_GRT_VALUE *value);
  virtual bool valueFromGrt(const char *key, MYX_GRT_VALUE *value);
  virtual void remake();
  virtual void applyProperties();
  virtual void updateProperties();
  void redecorate();
  ElementController *getElement1();
  ElementController *getElement2();
  static BaseController* create(WorkbenchController *wb, MYX_GRT_VALUE *value)
  {
    return new RelationshipController(wb, value);
  }
};


class TableController : public ElementController
{
  bool _applying;
  
  bool checkIfPK(MYX_GRT_VALUE *column);
  bool checkIfFK(MYX_GRT_VALUE *column);
  virtual MYX_GRT_VALUE *objectData();
  virtual void onElementChange(CFigureElement *element);
  virtual void fixupElementStyle(CFigureElement *element, MYX_GRT_VALUE *data);

public:
  TableController(WorkbenchController *wb, MYX_GRT_VALUE *value);
  virtual ~TableController();
  virtual bool valueFromGrt(const char *key, MYX_GRT_VALUE *value);
  virtual void applyProperties();
  virtual bool realizeConditionsMet();
  virtual bool realizeFigure();
  virtual TBoundingBox detailFrame(MYX_GRT_VALUE *detail);
  static BaseController* create(WorkbenchController *wb, MYX_GRT_VALUE *value)
  {
    return new TableController(wb, value);
  }
};


class ViewController : public ElementController
{
  virtual void onElementChange(CFigureElement *element);
public:
  ViewController(WorkbenchController *wb, MYX_GRT_VALUE *value);
  virtual MYX_GRT_VALUE *objectData();
  virtual void applyProperties();
  virtual bool valueFromGrt(const char *key, MYX_GRT_VALUE *value);
  static BaseController* create(WorkbenchController *wb, MYX_GRT_VALUE *value)
  {
    return new ViewController(wb, value);
  }
};


class RoutineController : public ElementController
{
  virtual void onElementChange(CFigureElement *element);
  virtual void assembleFigure();
public:
  RoutineController(WorkbenchController *wb, MYX_GRT_VALUE *value);
  virtual MYX_GRT_VALUE *objectData();
  virtual void applyProperties();
  virtual bool valueFromGrt(const char *key, MYX_GRT_VALUE *value);
  static BaseController* create(WorkbenchController *wb, MYX_GRT_VALUE *value)
  {
    return new RoutineController(wb, value);
  }
};


class NoteController : public ElementController
{
  virtual void applyProperties();
public:
  NoteController(WorkbenchController *wb, MYX_GRT_VALUE *value);
  bool valueFromGrt(const char *key, MYX_GRT_VALUE *value);
  static BaseController* create(WorkbenchController *wb, MYX_GRT_VALUE *value)
  {
    return new NoteController(wb, value);
  }
};


class ImageController : public ElementController
{
  virtual void applyProperties();
public:
  ImageController(WorkbenchController *wb, MYX_GRT_VALUE *value);
  static BaseController* create(WorkbenchController *wb, MYX_GRT_VALUE *value)
  {
    return new ImageController(wb, value);
  }
};


#endif
