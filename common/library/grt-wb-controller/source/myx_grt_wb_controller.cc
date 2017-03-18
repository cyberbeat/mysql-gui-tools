 /* Copyright (C) 2005 MySQL AB

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


// Notes:
//
// To add a view to the model:
// - create the view object
// - the view will be automatically added to the model
//
// To add a layer to the model:
// - create the layer object
// - setup coordinates and other stuff
// - add it to the layers list of the view
//
// To add an element to the model:
// - create the element
// - setup all its attributes
// - add it to the elementList of the rootLayer, it will be relocated to the correct
//   sublayer
//

#include <myx_gc_model.h>
#include <myx_gc_layer.h>


#include "myx_grt_wb_controller.h"
#include "myx_grt_wb_public_interface.h"

#define INTERNAL_ERROR(msg) g_warning("INTERNAL ERROR: %s", msg)

static inline MYX_GRT_VALUE *grtObjectFromValue(MYX_GRT *grt, MYX_GRT_VALUE *value)
{
  if (myx_grt_value_get_type(value) == MYX_STRING_VALUE)
    return myx_grt_reference_cache_lookup(grt, myx_grt_value_as_string(value));
  else
    return value;
}


static inline LayerController *layerFromValueObject(MYX_GRT_VALUE *value)
{
  if (myx_grt_value_get_type(value) == MYX_DICT_VALUE
      && strcmp2(myx_grt_dict_struct_get_name(value), "db.workbench.Layer")==0)
    return (LayerController*)myx_grt_value_bridge_data_object_get(value);
  return NULL;
}

static inline CanvasViewController *viewFromValueObject(MYX_GRT_VALUE *value)
{
  if (myx_grt_value_get_type(value) == MYX_DICT_VALUE
      && strcmp2(myx_grt_dict_struct_get_name(value), "db.workbench.View")==0)
    return (CanvasViewController*)myx_grt_value_bridge_data_object_get(value);
  return NULL;
}

static inline ElementController *elementFromValueObject(MYX_GRT *grt, MYX_GRT_VALUE *value)
{
  if (myx_grt_value_get_type(value) == MYX_DICT_VALUE)
  {
    MYX_GRT_STRUCT *gstruct= myx_grt_dict_struct_get(grt, value);
    if (!gstruct)
      return NULL;
      
    if (strcmp2(myx_grt_struct_get_parent_name(gstruct), "model.Element")==0)
      return (ElementController*)myx_grt_value_bridge_data_object_get(value);
  }
  else if (myx_grt_value_get_type(value) == MYX_STRING_VALUE)
  {
    return elementFromValueObject(grt, myx_grt_reference_cache_lookup(grt, myx_grt_value_as_string(value)));
  }
  return NULL;
}

static inline TableController *tableFromValueObject(MYX_GRT *grt, MYX_GRT_VALUE *value)
{
  if (myx_grt_value_get_type(value) == MYX_DICT_VALUE
      && strcmp2(myx_grt_dict_struct_get_name(value), "db.workbench.TableElement")==0)
    return (TableController*)myx_grt_value_bridge_data_object_get(value);
  return NULL;
}


static inline RelationshipController *relationshipFromValueObject(MYX_GRT *grt, MYX_GRT_VALUE *value)
{
  if (myx_grt_value_get_type(value) == MYX_DICT_VALUE
      && strcmp2(myx_grt_dict_struct_get_name(value), "db.workbench.Relationship")==0)
    return (RelationshipController*)myx_grt_value_bridge_data_object_get(value);
  return NULL;
}


static bool listContainsObject(MYX_GRT_VALUE *list, MYX_GRT_VALUE *value)
{
  int i, c;
  c= myx_grt_bridge_list_item_count(list);
  for (i= c-1; i >= 0; i--)
  {
    if (myx_grt_bridge_list_item_get(list, i, 0) == value)
      return true;
  }
  return false;
}


static int listIndexOfValue(MYX_GRT_VALUE *list, MYX_GRT_VALUE *value)
{
  int i, c;
  c= myx_grt_bridge_list_item_count(list);
  for (i= c-1; i >= 0; i--)
  {
    if (myx_grt_bridge_list_item_get(list, i, 0) == value)
      return i;
  }
  return -1;
}


static void removeFromList(MYX_GRT_VALUE *list, MYX_GRT_VALUE *value)
{
  int i, c;
  c= myx_grt_bridge_list_item_count(list);
  for (i= c-1; i >= 0; i--)
  {
    if (myx_grt_bridge_list_item_get(list, i, 0) == value)
    {
      myx_grt_bridge_list_item_del(list, i, 0);
      break;
    }
  }
}

/*
static void removeRefFromList(MYX_GRT_VALUE *list, MYX_GRT_VALUE *value)
{
  int i, c;
  const char *id= myx_grt_dict_id_item_as_string(value);

  c= myx_grt_bridge_list_item_count(list);
  for (i= c-1; i >= 0; i--)
  {
    MYX_GRT_VALUE *item= myx_grt_bridge_list_item_get(list, i, 0);
    if (strcmp(myx_grt_value_as_string(item), id)==0)
    {
      myx_grt_bridge_list_item_del(list, i, 0);
      break;
    }
  }
}
*/

RemakableObject::RemakableObject(WorkbenchController *wb)
: _wbench(wb), _pendingRemake(false)
{
}


RemakableObject::~RemakableObject()
{
}


void RemakableObject::setPendingRemake()
{
  if (!_pendingRemake)
  {
    _pendingRemake= true;
    _wbench->setPendingRemake(this);
  }
}


void RemakableObject::remakeDone()
{
  _pendingRemake= false;
}



BaseController::BaseController(WorkbenchController *wb, MYX_GRT_VALUE *value)
: _wb(wb)
{
  _type= "base";
  _value= myx_grt_value_retain(value);
  _valueRetained= true;
  _ownerModel= NULL;
}


BaseController::BaseController(WorkbenchController *wb)
: _wb(wb)
{
  _type= "base";
  _value= NULL;
  _valueRetained= false;
  _ownerModel= NULL;
}


BaseController::~BaseController()
{
  if (_valueRetained)
    myx_grt_value_release(_value);
}

string BaseController::description()
{
  return _type;
}


void BaseController::release()
{
  if (_valueRetained)
  {
    _valueRetained= false;
    myx_grt_value_release(_value);
  }
}


bool BaseController::updateFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  return false;
}


bool BaseController::updateToGrt(const char *key, MYX_GRT_VALUE *value)
{
  return false;
}


bool BaseController::valueFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  if (strcmp2(key, "_id")==0)
  {
    _wb->rememberObject(this);
  }
  else
    return false;
  return true;
}

const char *BaseController::getStringValue(const char *key)
{
  MYX_GRT_VALUE *value= myx_grt_bridge_dict_item_get_value(_value, key, 0);
  if (value)
    return myx_grt_value_as_string(value);
  return NULL;
}


MYX_GRT_VALUE *BaseController::getRefValue(const char *key)
{
  const char *id= getStringValue(key);
  if (id)
    return myx_grt_reference_cache_lookup(_wb->grt(), key);
  return NULL;
}


int BaseController::setStringValue(const char *key, const char *s)
{
  myx_grt_bridge_dict_item_set_value_from_string(_value, key, s, 0);

  return 0;
}

int BaseController::getIntValue(const char *key)
{
  MYX_GRT_VALUE *value= myx_grt_bridge_dict_item_get_value(_value, key, 0);
  if (value)
    return myx_grt_value_as_int(value);
  return 0;
}

double BaseController::getDoubleValue(const char *key)
{
  MYX_GRT_VALUE *value= myx_grt_bridge_dict_item_get_value(_value, key, 0);
  if (value)
    return myx_grt_value_as_real(value);
  return 0.0;
}

MYX_GRT_VALUE *BaseController::getValue(const char *key)
{
  return myx_grt_bridge_dict_item_get_value(_value, key, 0);
}

bool BaseController::referenceIsSet(const char *key)
{
  const char *ref= getStringValue(key);
  if (!ref || !*ref)
    return false;
  return true;
}



void BaseController::setModel(ModelController *model)
{
  _ownerModel= model;
}


ModelController::ModelController(WorkbenchController *wb, CGenericCanvas *canvas, MYX_GRT_VALUE *value)
: BaseController(wb, value), _canvas(canvas)
{
  _type= "model";
}


CanvasViewController* ModelController::currentView()
{
  CGCView *gcView= _canvas->currentViewGet();
  
  return controllerForCanvasView(gcView);
}


void ModelController::addViewRef(CanvasViewController *view)
{
  myx_grt_bridge_list_item_insert(getValue("views"),
                                  -1,
                                  view->value(),
                                  0);
}


void ModelController::removeViewRef(CanvasViewController *view)
{
  removeFromList(getValue("views"), view->value());
}


void ModelController::deleteView(CanvasViewController *view)
{
  view->clear();
  
  removeViewRef(view);
  
  //XXX
}


ElementController *ModelController::controllerForElement(CGCBase *element)
{
  return (ElementController*)(BaseController*)element->userDataGet();
}


void ModelController::changedCurrentView(CanvasViewController *view)
{
  const char *_id= view->getStringValue("_id");
  MYX_GRT_VALUE *value= getValue("currentView");
  if (_id)
    myx_grt_bridge_value_change_string(value, _id);
  else
  {
    char *new_id= myx_grt_get_guid();

    view->setStringValue("_id", new_id);
    myx_grt_bridge_value_change_string(value, new_id);
  }  
}


bool ModelController::updateFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  if (strcmp2(key, "views")==0)
  {
    MYX_GRT_VALUE *viewL= getValue("views");

    // check what happened to the views list
    for (map<string,CanvasViewController*>::iterator iter= _viewControllers.begin();
         iter != _viewControllers.end(); ++iter)
    {
      if (!listContainsObject(viewL, iter->second->value()))
      {
        CanvasViewController* controller = iter->second;
        _wb->notifyView(controller, MYX_WBGC_REMOVED);
        controller->release();
        _viewControllers.erase(iter);

        break;
      }
    }
  }
  else if (strcmp2(key, "elements")==0)
  {
    
  }
  else
    return BaseController::updateFromGrt(key, value);
  return true;
}


bool ModelController::valueFromGrt(const char *key, MYX_GRT_VALUE *value)
{
//  g_message("value from grt, key=%s type=%s", key,
//            myx_get_value_type_as_string(myx_grt_value_get_type(value)));
  
  if (strcmp2(key, "views")==0)
  {
    INTERNAL_ERROR("Views are added to the model automatically.");
    removeFromList(getValue("views"), value);
  }
  else if (strcmp2(key, "elements")==0)
  {
    if (getValue("elements") != value)
    {
      INTERNAL_ERROR("Cannot add element directly to view");
      removeFromList(getValue("elements"), value);
    }
  }
  else if (strcmp2(key, "currentView")==0)
  {
    CanvasViewController *view= viewFromValueObject(grtObjectFromValue(_wb->grt(), value));
    
    if (view != NULL && _canvas->currentViewGet() != view->gcView())
    {
      _canvas->currentViewSet(view->gcView());      
    }
  }
  else if (strcmp2(key, "markers")==0)
  {
  }
  return true;
}


void ModelController::rememberViewController(CanvasViewController *object)
{
  _viewControllers[object->getStringValue("name")]= object;
}


CanvasViewController *ModelController::controllerForCanvasView(CGCView *object)
{
  if (object != NULL)
  {
    string name= object->propertyGet("name", 0);
  
    if (_viewControllers.find(name) != _viewControllers.end())
      return _viewControllers[name];
  };

  return NULL;  
}


// ==============================================================================

CanvasViewController::CanvasViewController(WorkbenchController *wb, MYX_GRT_VALUE *value)
: BaseController(wb, value), _gcView(0), _gcLayer(0), _rootLayer(0), _needRealization(true)
{
  _type= "view";
  
  _rubberStartPending= false;
  _rubberStopPending= false;
  _rubberX= -1;
  _rubberY= -1;
  
  memset(_connectionParts, 0, sizeof(CFigure*)*4);
  memset(_connectionCenterParts, 0, sizeof(CFigure*)*4);

  changeRelationshipStyle("default");
}


CanvasViewController::CanvasViewController(WorkbenchController *wb, CGCView *gcView)
: BaseController(wb), _gcView(gcView), _gcLayer(0), _rootLayer(0), _needRealization(true)
{
  _type= "view";
  string name= gcView->propertyGet("name", 0);
  _rubberStartPending= false;
  _rubberStopPending= false;
  _rubberX= -1;
  _rubberY= -1;
  
  _value= myx_grt_bridge_dict_new(_wb->grt(), "db.workbench.View", this);

  myx_grt_dict_init_obj(_wb->grt(), _value, name.c_str(), NULL, NULL);
  
  memset(_connectionParts, 0, sizeof(CFigure*)*4);
  memset(_connectionCenterParts, 0, sizeof(CFigure*)*4);

  changeRelationshipStyle("default");
}


CanvasViewController::~CanvasViewController()
{
  _wb->canvas()->removeLayer(_gcLayer);
  _wb->canvas()->removeView(_gcView);
  delete _gcView;

  /*
  for (int i= 0; i < 4; i++)
  {
    delete _connectionParts[i];
    delete _connectionCenterParts[i];
  }
   */
}


void CanvasViewController::changeRelationshipStyle(const char *style)
{
  CGenericCanvas *canvas= _wb->canvas();

  /* 
  for (int i= 0; i < 4; i++)
  {
    delete _connectionParts[i];
    delete _connectionCenterParts[i];
  }
   */

  if (!style || !*style)
    style= "default";

#define DECORATION_INDEX(many, mand) ((mand ? 1 : 0) | (many ? 1 : 0)<<1)
#define CENTER_TYPE_INDEX(leftMany,rightMany) ((leftMany ? 1 : 0) | (rightMany ? 1 : 0)<<1)

  _connectionParts[DECORATION_INDEX(false,false)]= canvas->createFigure("db.Connection.End.Single.Optional", style);
  _connectionParts[DECORATION_INDEX(false,true)]= canvas->createFigure("db.Connection.End.Single.Mandatory", style);
  _connectionParts[DECORATION_INDEX(true,false)]= canvas->createFigure("db.Connection.End.Multiple.Optional", style);
  _connectionParts[DECORATION_INDEX(true,true)]= canvas->createFigure("db.Connection.End.Multiple.Mandatory", style);
 
  _connectionCenterParts[CENTER_TYPE_INDEX(false,false)]= canvas->createFigure("db.Connection.Center.One.One", style);
  _connectionCenterParts[CENTER_TYPE_INDEX(false,true)]= canvas->createFigure("db.Connection.Center.One.Many", style);
  _connectionCenterParts[CENTER_TYPE_INDEX(true,false)]= canvas->createFigure("db.Connection.Center.One.Many", style);
  _connectionCenterParts[CENTER_TYPE_INDEX(true,true)]= canvas->createFigure("db.Connection.Center.Many.Many", style);
}



void CanvasViewController::decorateConnection(CConnection *connection,
                                         bool sourceMandatory, bool sourceMany,
                                         bool destMandatory, bool destMany,
                                         CFigure *end1Label, CFigure *end2Label,
                                         CFigure *centerLabel)
{
  TConnectionDecorations decoration;
  
  decoration.end1Decoration= _connectionParts[DECORATION_INDEX(sourceMany,sourceMandatory)];
  decoration.end1Label = end1Label;

  decoration.centerDecoration = _connectionCenterParts[CENTER_TYPE_INDEX(sourceMany,destMany)];
  decoration.centerLabel = centerLabel;
  
  decoration.end2Decoration = _connectionParts[DECORATION_INDEX(destMany,destMandatory)];
  decoration.end2Label = end2Label;
  
  connection->decorationsSet(decoration);
}



bool CanvasViewController::updateFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  if (strcmp2(key, "selection")==0)
  {
  }
  else if (strcmp2(key, "layers")==0)
  {
  }
  else if (strcmp2(key, "elements")==0)
  {
  }
  else
    return BaseController::updateFromGrt(key, value);
  return true;
}


void CanvasViewController::postCreate()
{
}


void CanvasViewController::setupForRubberbandStart(float x, float y)
{
  _rubberX= x;
  _rubberY= y;
  _rubberStartPending= true;
  _rubberStopPending= false;
  
  _wb->notifyView(this, MYX_WBGC_RUBBERBAND_STARTED);
}


void CanvasViewController::setupForRubberbandStop()
{
  _rubberStartPending= false;
  _rubberStopPending= true;
  _wb->notifyView(this, MYX_WBGC_RUBBERBAND_STOPPED);
}


void CanvasViewController::doPendingViewTasks()
{
  if (_rubberStartPending)
  {
    TVertex pt;
    pt.x= _rubberX;
    pt.y= _rubberY;
    pt.z= 0;
    pt.w= 0;
    _gcView->rubberBandStart(GC_RBSTYLE_DOTTED_THICK, pt);
  }
  else if (_rubberStopPending)
  {
    _gcView->rubberBandStop();
  }
}


void CanvasViewController::notifyTaskComplete()
{
  _wb->notifyView(this, MYX_WBGC_TASK_COMPLETED);
}


void CanvasViewController::notifyTaskCanceled()
{
  _wb->notifyView(this, MYX_WBGC_TASK_CANCELED);
}


void CanvasViewController::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  if (reason == GC_CHANGE_SELECTION_CHANGE
      || reason == GC_CHANGE_SELECTION_ADD
      || reason == GC_CHANGE_SELECTION_REMOVE
      || reason == GC_CHANGE_SELECTION_CLEAR)
    _wb->notifyView(this, MYX_WBGC_SELECTION_CHANGE);
  else if (reason == GC_CHANGE_VIEW_WORKSPACE)
  {
    float width, height;
    
    if (_gcView)
    {
      _gcView->workspaceGet(width, height);

      myx_grt_bridge_value_change_real(getValue("width"), width);
      myx_grt_bridge_value_change_real(getValue("height"), height);
      _wb->notifyView(this, MYX_WBGC_MODIFIED);
    }
  }
}

bool CanvasViewController::valueFromGrt(const char *key, MYX_GRT_VALUE *value)
{
//  g_message("VIEW valueFromGrt %s", key);
  if (strcmp2(key, "rootLayer")==0)
  {
    if (getValue("rootLayer") != value)
      INTERNAL_ERROR("Not possible to change root layer");
    else
      setStringValue("rootLayer", _rootLayer->getStringValue("_id"));
  }
  else if (strcmp2(key, "layers")==0)
  {
    if (getValue("layers") != value)
    {
      LayerController *lctl= layerFromValueObject(value);
      if (lctl)
      {
        if (_rootLayer != lctl)
        {
          // relocate must be called before setView() because it relies
          // on the frame not existing yet to know it's a new layer
          relocateLayerToContainerLayer(lctl);
          lctl->setView(this);
          _wb->notifyLayer(lctl, MYX_WBGC_ADDED);
        }
        else // add it back
          myx_grt_bridge_list_item_insert(getValue("layers"), -1, lctl->value(), 0);
      }
      else
      {
        INTERNAL_ERROR("Adding invalid layer object to view");
        removeFromList(getValue("layers"), value);
      }
    }
    else
    {
      // the layers list is being initialized, auto-add the root layer to it

      if (_rootLayer == NULL)
      {
        // Initialize root layer instance if not yet done.
        MYX_GRT_VALUE *root_layer= myx_grt_dict_new_obj(_wb->grt(), "db.workbench.Layer", "root", "", 
                                                        _wb->model()->getStringValue("_id"));
        _rootLayer= layerFromValueObject(root_layer);
        _rootLayer->_parent= 0;
        _rootLayer->setView(this);
      };
      myx_grt_bridge_list_item_insert(getValue("layers"), -1, _rootLayer->value(), 0);
      
      // don't notify the app yet, as the creation of the view itself
      // has not happened yet. do it after the view notify is sent
      
      //_wb->notifyLayer(_rootLayer->value(), MYX_WBGC_ADDED);
    }
  }
  else if (strcmp2(key, "elements")==0)
  {  
    if (getValue("elements") != value)
    {
      ElementController *ectl= elementFromValueObject(_wb->grt(), value);
      
      if (ectl)
      {
        relocateElementToContainerLayer(ectl);      
      }
      else
      {
        INTERNAL_ERROR("Adding invalid element to elements list of view");
        removeFromList(getValue("elements"), value);
      }
    }
  }
  else if (strcmp2(key, "name")==0)
  {
    if (_ownerModel)
    {
      if (_ownerModel->canvas()->viewByName(getStringValue("name")))
      {
        INTERNAL_ERROR("Trying to create a view with a duplicate name");
      }
      else
      {
        if (!_gcView)
        {
          _ownerModel->rememberViewController(this);
          realizeView();
        }
      }
      _wb->notifyView(this, MYX_WBGC_MODIFIED);
    }
  }
  else if (strcmp2(key, "selection")==0)
  {
  }
  else if (strcmp2(key, "connectionLayoutClass")==0)
  {
    changeRelationshipStyle(getStringValue("connectionLayoutClass"));
   
    updateRelationships();
    
    _wb->notifyView(this, MYX_WBGC_LAYOUT_CHANGED);
  }
  else if (strcmp2(key, "elementLayoutClass")==0)
  {    
    _wb->notifyView(this, MYX_WBGC_LAYOUT_CHANGED);
  }
  else if (strcmp2(key, "width")==0)
  {
    if (_gcView)
    {
      float w, h;
      _gcView->workspaceGet(w, h);
      _gcView->workspaceSet((float) getDoubleValue("width"), h);
      _wb->notifyView(this, MYX_WBGC_MODIFIED);
    }
  }
  else if (strcmp2(key, "height")==0)
  {
    if (_gcView)
    {
      float w, h;
      _gcView->workspaceGet(w, h);
      _gcView->workspaceSet(w, (float) getDoubleValue("height"));
      _wb->notifyView(this, MYX_WBGC_MODIFIED);
    }
  }
  else
    return BaseController::valueFromGrt(key, value);
  return true;
}

void CanvasViewController::clear()
{
}


void CanvasViewController::updateRelationships()
{
  MYX_GRT_VALUE *relList= getValue("relationships");
  unsigned int i, c;

  c= myx_grt_bridge_list_item_count(relList);
  for (i= 0; i < c; i++)
  {
    RelationshipController *rel= relationshipFromValueObject(_wb->grt(),
                                                    myx_grt_bridge_list_item_get(relList, i, 0));

    if (rel)
      rel->redecorate();
  }
}


void CanvasViewController::deleteLayer(LayerController *layer)
{
  layer->_parent->removeSublayer(layer);

  removeFromList(getValue("layers"), layer->value());
  removeFromList(getValue("selection"), layer->value());
  
  layer->setPendingDestroy();
}


void CanvasViewController::deleteElement(ElementController *elem)
{
  removeFromList(getValue("elements"), elem->value());
  removeFromList(getValue("selection"), elem->value());
  elem->layer()->removeElementRef(elem);
  
  elem->setPendingDestroy();
}


void CanvasViewController::realizeView()
{
  if (_needRealization)
  {
    _needRealization= false;
    _ownerModel->rememberViewController(this);
    CGenericCanvas *canvas= _ownerModel->canvas();

    if (!_gcView)
    {
      canvas->beginUpdate();  
      _gcView= canvas->createView(getStringValue("name"));
      _gcView->userDataSet(this);
      _gcView->addListener(this);
    };
      
    // create canvas layer (not related to workbench layers)
    _gcLayer= canvas->createLayer("Main", false);
    _gcLayer->userDataSet(this);
    _gcView->addLayer(_gcLayer);
    canvas->endUpdate();
    
    _wb->notifyView(this, MYX_WBGC_ADDED);
    _wb->notifyLayer(_rootLayer, MYX_WBGC_ADDED);
    _wb->notifyView(this, MYX_WBGC_SWITCHED);
  };
}


LayerController *CanvasViewController::findEnclosingLayer(const TBoundingBox &bounds)
{
  return _rootLayer->sublayerContainingBounds(bounds);
}


void CanvasViewController::relocateElementsToLayer(LayerController *layer)
{
  LayerController *parent= layer->getParent();
  TBoundingBox bounds= layer->grtBounds();
  ElementController *elem;
  MYX_GRT *grt= _wb->grt();
  MYX_GRT_VALUE *elements;
  int i, c;
  
  // check if any of the elements in this layer fall out of it
  elements= layer->getValue("elements");
  c= myx_grt_list_item_count(elements);
  for (i= c-1; i >= 0; --i)
  {
    MYX_GRT_VALUE *elemId= myx_grt_list_item_get(elements, i);

    elem= elementFromValueObject(grt, elemId);
    if (elem)
    {
      relocateElementToContainerLayer(elem);
    }
  }


  if (parent)
  {
    // get all elements in parent layer and check if they should be in the
    // given layer
    // 
    elements= parent->getValue("elements");
    
    c= myx_grt_list_item_count(elements);
    for (i= c-1; i >= 0; --i)
    {
      MYX_GRT_VALUE *elemId= myx_grt_list_item_get(elements, i);
      
      elem= elementFromValueObject(grt, elemId);
      if (elem)
        relocateElementToContainerLayer(elem);
    }
  }
}


void CanvasViewController::relocateLayerToContainerLayer(LayerController *layer)
{
  TBoundingBox layerBounds= layer->grtBounds();

  // 0- if there's no frame created for the layer, it means it's a new layer.
  // put it inside the first sublayer that fully contains it, unless there's none
  // 1- look for sibling layers that are smaller than this one and fully contain it
  // 2- if there's no sibling layer, look for sibling layers of the parent layer 
  // (including parent itself)
  // 3- look for siblings of grandparent and so on
  // 4- if root layer is reached, then just relocate as child of root

  if (!layer->layerFrame())
  {
    bool found= false;
    // 0- this is a newcomer
    
    for (std::list<LayerController*>::reverse_iterator iter= _rootLayer->_layers.rbegin();
         iter != _rootLayer->_layers.rend(); ++iter)
    {
      if ((*iter)->containsBounds(layerBounds))
      {
        (*iter)->addSublayer(layer);
        found= true;
        break;
      }
    }
    if (!found)
      _rootLayer->addSublayer(layer);
  }
  else
  {
    LayerController *parent= layer->_parent;
    bool found= false;
    while (parent && !found)
    { 
      // if this parent contains ourselves
      if (parent->containsBounds(layerBounds))
      {
        found= true;
        break;
      }
      parent= parent->_parent;
    }
    if (!found)
      parent= _rootLayer;

    // look up which sublayer contains us
    do
    {
      found= false;
      
      for (std::list<LayerController*>::reverse_iterator iter= parent->_layers.rbegin();
           iter != parent->_layers.rend(); ++iter)
      {
        // this sublayer contains us, try to see if there's another sub-sublayer that contains
        if (*iter != layer && (*iter)->containsBounds(layerBounds))
        {
          parent= *iter;
          found= true;
          break;
        }
      }
    } while (found);

    if (layer->_parent != parent)
    {
      myx_grt_value_retain(layer->value());
      layer->_parent->removeSublayer(layer);
      
      if (!parent)
        _rootLayer->addSublayer(layer);
      else
        parent->addSublayer(layer);
      myx_grt_value_release(layer->value());
      
      _wb->notifyLayer(layer, MYX_WBGC_MODIFIED);
    }
  }
}


void CanvasViewController::relocateElementToContainerLayer(ElementController *element)
{
  TBoundingBox bounds= element->grtBounds();
  LayerController *layer;
  
  if (element->layer() && element->layer()->containsBounds(bounds))
  {    
    // now check if some sublayer of the current layer contains this
    layer= element->layer()->sublayerContainingBounds(bounds);
    
    if (!layer)
      layer= element->layer();
  }
  else
    layer= findEnclosingLayer(bounds);

  // if it's not totally in any layer, then leave it where it is
  if (!layer)
  {
    // unless it's not in it's old layer either
    if (!element->layer() || (element->layer() != _rootLayer && !element->layer()->containsBounds(bounds, false)))
      element->setLayer(_rootLayer, false); // then move it to the root layer
    return;
  }
  
  // no changes
  if (layer == element->layer())
    return;
  
  // remove from current layer and add to new one
  element->setLayer(layer, false);
}



bool CanvasViewController::isObjectUnder(CFigureInstance *object1,
                                         CFigureInstance *object2)
{ // object1 under object2?
  const CFigureInstances *instList= _gcLayer->instancesGet();

  for (CFigureInstances::const_iterator iter= instList->begin(); iter != instList->end(); ++iter)
  {
    if (*iter == object1) // object1 seen first, so its under object2
      return true;
    if (*iter == object2) 
      return false;
  }
  puts("!!!! isObjectUnder() unreachable place");
  return false; // shouldnt reach here
}


void CanvasViewController::addPendingLayerRelocation(LayerController *layer)
{
  // insert ordered by stacking
  for (std::list<LayerController*>::iterator iter= _pendingLayerRelocations.begin();
       iter != _pendingLayerRelocations.end(); ++iter)
  {
    if (*iter == layer)
      return;
    else if (isObjectUnder((*iter)->layerFrame(), layer->layerFrame()))
    {
      _pendingLayerRelocations.insert(iter, layer);
      break;
    }
  }
  _pendingLayerRelocations.push_back(layer);
}



void CanvasViewController::addPendingElementRelocation(ElementController *elem)
{
  // insert ordered by stacking
  for (std::list<ElementController*>::iterator iter= _pendingElementRelocations.begin();
       iter != _pendingElementRelocations.end(); ++iter)
  {
    if (*iter == elem)
      return;
    else if (isObjectUnder((*iter)->gcInstance(), elem->gcInstance()))
    {
      _pendingElementRelocations.insert(iter, elem);
      break;
    }
  }
  _pendingElementRelocations.push_back(elem);
}


void CanvasViewController::relocatePendingObjects()
{
  // relocate layers
  for (std::list<LayerController*>::iterator iter= _pendingLayerRelocations.begin();
       iter != _pendingLayerRelocations.end(); ++iter)
  {
    relocateLayerToContainerLayer(*iter);
    relocateElementsToLayer(*iter);
    (*iter)->clearPendingRelocation();    
  }
  _pendingLayerRelocations.clear();

  // relocate elements
  for (std::list<ElementController*>::iterator iter= _pendingElementRelocations.begin();
       iter != _pendingElementRelocations.end(); ++iter)
  {
    relocateElementToContainerLayer(*iter);
    (*iter)->clearPendingRelocation();
  }
  _pendingElementRelocations.clear();
}


void CanvasViewController::raiseLayer(LayerController *layer)
{
  MYX_GRT_VALUE *layerList= getValue("layers");
  MYX_GRT_VALUE *gtopLayer;
  int c;
  c= myx_grt_list_item_count(layerList);
  if (c > 1 && (gtopLayer= myx_grt_list_item_get(layerList, c-1)) != layer->value())
  {
    MYX_GRT_VALUE *elemList;
    LayerController *topLayer= layerFromValueObject(gtopLayer);
    ElementController *topElem, *elem;

    myx_grt_value_retain(layer->value());
    removeFromList(layerList, layer->value());
    myx_grt_list_item_add(layerList, layer->value());
    myx_grt_value_release(layer->value());

    _gcLayer->bringToFront(layer->layerFrame(), topLayer->layerFrame());
    
    // start putting on top of the topmost element of the layer
    topElem= topLayer->frontmostElement(true);
    
    elemList= layer->getValue("elements");
    for (unsigned int i= 0; i < myx_grt_list_item_count(elemList); i++)
    {
      MYX_GRT_VALUE *e= myx_grt_list_item_get_reference_value(_wb->grt(), elemList, i);
      
      if (e && (elem= elementFromValueObject(_wb->grt(), e)))
      {
        _gcLayer->bringToFront(elem->gcInstance(), topElem->gcInstance());
        topElem= elem;
      }
    }
  }
}


void CanvasViewController::updateSelectionList()
{
  CFigureInstanceEnumerator *fenum= _gcView->getFigureInstanceEnumerator();
  MYX_GRT_VALUE *list= getValue("selection");
  
  myx_grt_list_clear(list);
  
  while (fenum->hasNext())
  {
    CFigureInstance *instance= fenum->next();
    ElementController *element;
    MYX_GRT_VALUE *item;

    element= _ownerModel->controllerForElement(instance);
    if (!element)
    {
      continue;
    }
    if (instance->selected())
    {
      item= element->value();
      myx_grt_bridge_list_item_insert(list, -1, item, 0);
    }
  }
}


void CanvasViewController::autoArrangeElements(bool selectedOnly)
{
  MYX_GRT_VALUE *elements= getValue("elements");
  MYX_GRT_VALUE *relationships= getValue("relationships");
  unsigned int i, j;
  std::set<ElementController*> fkTables;
  std::map<ElementController*,GraphNode*> nodes;
  std::list<ElementController*> tableList, viewList, otherList;
  double left_x= 0, right_x= 0, bottom_y= 0;
  double row_height;

  float workspaceWidth, workspaceHeight;
  _gcView->getWorkspace(&workspaceWidth, &workspaceHeight);
  GraphRenderer arranger(10, (int) workspaceWidth, (int) workspaceHeight);

  // go through relationships and remember all nodes that are connected
  unsigned int edgeCount= myx_grt_list_item_count(relationships);
  for (i= 0; i < edgeCount; i++)
  {
    MYX_GRT_VALUE *relv= myx_grt_list_item_get(relationships, i);
    RelationshipController *rel= relationshipFromValueObject(_wb->grt(), relv);
    ElementController *elem1, *elem2;

    elem1= rel->getElement1();
    elem2= rel->getElement2();

    fkTables.insert(elem1);
    fkTables.insert(elem2);
  }

  // Pass coordinates to arrange code.
  unsigned int nodeCount= myx_grt_list_item_count(elements);
  for (i= 0; i < nodeCount; i++)
  {
    MYX_GRT_VALUE *elemv= myx_grt_list_item_get(elements, i);
    ElementController *elem= elementFromValueObject(_wb->grt(), elemv);
    TBoundingBox bounds;
    
    if (elem->gcInstance())
      bounds= elem->gcBounds();
    else
      bounds= elem->grtBounds();
    
    if (bounds.width() < 1.0)
      bounds.lower.x= bounds.upper.x + 150;

    if (bounds.height() < 1.0)
      bounds.lower.y= bounds.upper.y + 150;

    // do not move non-table elements
    if (!myx_grt_dict_struct_is_or_inherits_from(_wb->grt(), elemv, "db.workbench.TableElement"))
    {
      if (myx_grt_dict_struct_is_or_inherits_from(_wb->grt(), elemv, "db.workbench.ViewElement"))
        viewList.push_back(elem);
      else
        otherList.push_back(elem);
      continue;
    }
    // do not move tables with no relationships
    if (fkTables.find(elem) == fkTables.end())
    {
      tableList.push_back(elem);
      continue;
    }
    nodes[elem]= arranger.add_node(bounds.upper.x, bounds.upper.y, bounds.width(), bounds.height());
    if (selectedOnly && !(elem->gcInstance() && elem->gcInstance()->selected()))
      nodes[elem]->set_movable(false);

    //if (!selectedOnly || (elem->gcInstance() && elem->gcInstance()->selected()))
    //  nodes[elem]= arranger.add_node(bounds.upper.x, bounds.upper.y, bounds.width(), bounds.height());
  };

  edgeCount= myx_grt_list_item_count(relationships);
  for (i= 0; i < edgeCount; i++)
  {
    MYX_GRT_VALUE *relv= myx_grt_list_item_get(relationships, i);
    RelationshipController *rel= relationshipFromValueObject(_wb->grt(), relv);
    ElementController *elem1, *elem2;

    elem1= rel->getElement1();
    GraphNode* node1 = nodes[elem1];
    elem2= rel->getElement2();
    GraphNode* node2 = nodes[elem2];

    if ((node1 != NULL) && (node2 != NULL))
      arranger.add_edge(node1, node2);

    //if (!selectedOnly || ((node1 != NULL) && (node2 != NULL)))
    //  arranger.add_edge(node1, node2);
  }

  // Do the magic.
  if (nodeCount > 0)
    arranger.recalc();

  bottom_y= 0.0;
  right_x= 0.0;
  left_x= 10000000000.0;
  
  // update positions
  _gcView->canvas()->beginUpdate();
  for (std::map<ElementController*,GraphNode*>::iterator iter= nodes.begin();
       iter != nodes.end(); ++iter)
  {
    GraphNode* node = iter->second;
    if (node != NULL)
    {
      double newx= iter->second->newleft();
      double newy= iter->second->newtop();

      bottom_y= std::max(newy + iter->second->height(), bottom_y);
      left_x= std::min(newx, left_x);
      right_x= std::max(newx + iter->second->width(), right_x);

      iter->first->moveTo(newx, newy);
    };
  };
  
  if (right_x < left_x)
  {
    left_x= 0;
    right_x= 1000;
  }
  else if (right_x - left_x < 1000)
    right_x= left_x + 1000;

  // arrange non-moved objects below everything else
  double x, y;
  double spacing= 20.0;
  y= bottom_y + spacing;
  for (j= 0; j < 3; j++)
  {
    std::list<ElementController*> elemList;
    
    switch (j)
    {
    case 0: elemList= tableList; break;
    case 1: elemList= viewList; break;
    case 2: elemList= otherList; break;
    }
    row_height= 0;
    x= left_x;
    i= 0;
    for (std::list<ElementController*>::iterator iter= elemList.begin();
         iter != elemList.end(); ++iter)
    {
      TBoundingBox bounds;
      if ((*iter)->gcInstance())
        bounds= (*iter)->gcBounds();
      else
        bounds= (*iter)->grtBounds();

      (*iter)->moveTo(x, y);
      x+= bounds.width() + spacing;
      row_height= std::max((double)bounds.height(), row_height);
      i++;
      if (x >= right_x)
      {
        i= 0;
        x= left_x;
        y+= row_height + spacing;
        row_height= 0;
      }
    }
    if (i > 0)
    {
      x= left_x;
      y+= row_height + spacing;
    }
  }

  _gcView->canvas()->endUpdate();
  _gcView->canvas()->refresh();
}


// ==============================================================================


LayerController::LayerController(WorkbenchController *wb, MYX_GRT_VALUE *value)
: BaseController(wb, value), RemakableObject(wb), _view(0), _parent(0), _gcFigure(0), _gcFigureInstance(0)
{
  _moved= false;
  _pendingDestroy= false;
  _type= "layer";
}


LayerController::~LayerController()
{
}


void LayerController::setPendingRemake()
{
  RemakableObject::setPendingRemake();
  _wb->notifyLayer(this, MYX_WBGC_MODIFIED);
}



void LayerController::setPendingDestroy()
{
  _pendingDestroy= true;
  RemakableObject::setPendingRemake();
  
  _wb->notifyLayer(this, MYX_WBGC_REMOVED);
}


void LayerController::remake()
{
  if (_pendingDestroy)
  {
    _gcFigureInstance->removeListener(this);
    _view->gcLayer()->removeInstance(_gcFigureInstance);
    _gcFigureInstance= 0;
    
    delete _gcFigure;
    _gcFigure= 0;  
  }
  else
    createFrame();
  
  
  // relocate stuff that fall inside us
  _view->relocateElementsToLayer(this);
}


void LayerController::remakeDone()
{
//  if (_pendingDestroy)
//    release();
  RemakableObject::remakeDone();
}


TBoundingBox LayerController::grtBounds()
{
  TBoundingBox box;
  box.upper.x= (float) getDoubleValue("left");
  box.upper.y= (float) getDoubleValue("top");
  box.lower.x= box.upper.x + (float) getDoubleValue("width");
  box.lower.y= box.upper.y + (float) getDoubleValue("height");
  return box;
}


TBoundingBox LayerController::gcBounds()
{
  return _gcFigureInstance->bounds();
}


void LayerController::addSublayer(LayerController *layer)
{
  if (layer->layerFrame())
  {
    // find the topmost layer and put the new one just above it
    if (_layers.begin() != _layers.end() || !layerFrame())
      _view->gcLayer()->bringToFront(layer->layerFrame(), (*_layers.rbegin())->layerFrame());
    else
      _view->gcLayer()->bringToFront(layer->layerFrame(), layerFrame());
  }
  myx_grt_bridge_list_item_insert(getValue("subLayers"), -1, 
                                  layer->getValue("_id"), 0);
  _layers.push_back(layer);
  layer->_parent= this;

  layer->restackElements();
}


void LayerController::removeSublayer(LayerController *layer)
{
  list<LayerController*>::iterator iter= std::find(_layers.begin(), _layers.end(), layer);
  
  layer->_parent= 0;
  if (iter != _layers.end())
    _layers.erase(iter);
  removeFromList(getValue("subLayers"), layer->getValue("_id"));

  // go through elements in sublayer and add to us
  MYX_GRT_VALUE *elements= layer->getValue("elements");
  MYX_GRT *grt= _wb->grt();
  int c= myx_grt_bridge_list_item_count(elements);
  for (int i = 0; i < c; i++)
  {
    MYX_GRT_VALUE *item= myx_grt_bridge_list_item_get(elements, i, 0);
    ElementController *elem= elementFromValueObject(grt, item);

    elem->setLayer(this, true);
  }
}


void LayerController::moveTo(float newX, float newY, bool dragging)
{
  double oldX, oldY;
  double dx, dy;
  MYX_GRT_VALUE *elements= getValue("elements");
  MYX_GRT *grt= _wb->grt();
  
  oldX= getDoubleValue("left");
  oldY= getDoubleValue("top");
  
  dx= newX - oldX;
  dy= newY - oldY;

  _ownerModel->canvas()->beginUpdate();
  
  // TODO: if the movement is the result of dragging, relocation of the contents
  // should be disabled
  
  // move the elements
  int c= myx_grt_bridge_list_item_count(elements);
  for (int i = 0; i < c; i++)
  {
    MYX_GRT_VALUE *item= myx_grt_bridge_list_item_get(elements, i, 0);
    ElementController *elem= elementFromValueObject(grt, item);

    if (elem && elem->gcInstance() && (!dragging || !elem->gcInstance()->selected()))
    {
      double x= elem->getDoubleValue("left") + dx;
      double y= elem->getDoubleValue("top") + dy;

      if (elem->gcInstance())
        elem->gcInstance()->translate((float)x, (float)y, 0, false);

      myx_grt_bridge_value_change_real(getValue("left"), x);
      myx_grt_bridge_value_change_real(getValue("top"), y);
    }
  }

  // move the sublayers
  for (std::list<LayerController*>::const_iterator iter= _layers.begin();
       iter != _layers.end(); ++iter)
  {
    double x= (*iter)->getDoubleValue("left") + dx;
    double y= (*iter)->getDoubleValue("top") + dy;
    if ((*iter)->_gcFigureInstance)
      (*iter)->_gcFigureInstance->translate((float)x, (float)y, 0, false);
  }
  _ownerModel->canvas()->endUpdate();
  
  myx_grt_bridge_value_change_real(getValue("left"), newX);
  myx_grt_bridge_value_change_real(getValue("top"), newY);
}


void LayerController::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  if (reason == GC_CHANGE_FINSTANCE_PROPERTY)
  {
    TGCVariant gcValue = _gcFigureInstance->propertyGet("translation", 0);
    float newX, newY;
    
    newX= gcValue;
    gcValue = _gcFigureInstance->propertyGet("translation", 1);
    newY= gcValue;
    
    if (getDoubleValue("left") != newX ||
        getDoubleValue("top") != newY)
    {
      moveTo(newX, newY, true);
      if (!_moved)
      {
        _moved= true;
        _view->addPendingLayerRelocation(this);
      }
    }  
  }
  else if (reason == GC_CHANGE_FIGURE_RESIZE)
  {
    bool resized= false;
    
    if (getDoubleValue("width") != _gcFigureInstance->width())
    {
      myx_grt_bridge_value_change_real(getValue("width"), _gcFigureInstance->width());
      resized= true;
    }
    if (getDoubleValue("height") != _gcFigureInstance->height())
    {
      myx_grt_bridge_value_change_real(getValue("height"), _gcFigureInstance->height());
      resized= true;
    }
    if (resized)
      _view->addPendingLayerRelocation(this);
  }
}


bool LayerController::updateFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  return BaseController::updateFromGrt(key, value);
}


bool LayerController::valueFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  if (strcmp2(key, "description")==0)
  {
  }
  else if (strcmp2(key, "name")==0)
  {    
    // apply name
    if (_gcFigure)
    {
      const char* name = getStringValue("name");
      _gcFigure->elementFromKey("/name")->setCaption(name != NULL ? name : "");
      _wb->canvas()->refresh();
      _wb->notifyLayer(this, MYX_WBGC_MODIFIED);
    }
  }
  else if (strcmp2(key, "color")==0)
  {
    _wb->canvas()->beginUpdate();
    applyFrameProperties(ApplyColor);
    _wb->canvas()->endUpdate();
    _wb->notifyLayer(this, MYX_WBGC_MODIFIED);
    _wb->canvas()->refresh();
  }
  else if (strcmp2(key, "top")==0)
  {
    applyFrameProperties(ApplyPos);
  }
  else if (strcmp2(key, "left")==0)
  {
    applyFrameProperties(ApplyPos);
  }
  else if (strcmp2(key, "width")==0)
  {
    applyFrameProperties(ApplySize);
  }
  else if (strcmp2(key, "height")==0)
  {
    applyFrameProperties(ApplySize);
  }
  else if (strcmp2(key, "subLayers")==0)
  {
    if (getValue("subLayers") != value)
    {
      INTERNAL_ERROR("Adding layer directly to subLayers list. Add to the layers list of the view instead");
      removeFromList(getValue("subLayers"), value);
    }
  }
  else if (strcmp2(key, "elements")==0)
  {
    if (getValue("elements") != value)
    {
      INTERNAL_ERROR("Adding element directly to a layer is not permitted. Add to the view instead");
      removeFromList(getValue("elements"), value);
    }
  }
  else
    return BaseController::valueFromGrt(key, value);
  
  return true;
}


void LayerController::addElementRef(ElementController *element)
{
  myx_grt_bridge_list_item_insert(getValue("elements"),
                                  -1,
                                  element->getValue("_id"),
                                  0);
}


void LayerController::removeElementRef(ElementController *element)
{
  removeFromList(getValue("elements"), element->getValue("_id"));
}


ElementController *LayerController::backmostElement(bool realizedOnly)
{
  MYX_GRT_VALUE *list= getValue("elements");
  int i, count;
  if ((count= myx_grt_bridge_list_item_count(list)) > 0)
  {
    for (i= 0; i < count; i++)
    {
      MYX_GRT_VALUE *item= myx_grt_bridge_list_item_get(list, i, 0);
      ElementController *elem= elementFromValueObject(_wb->grt(), item);
      
      if (!realizedOnly || elem->gcInstance())
        return elem;
    }
  }
  return 0;
}


ElementController *LayerController::realizedElementBelow(ElementController *elem)
{
  MYX_GRT_VALUE *list= getValue("elements");
  int i, count;

  if ((count= myx_grt_bridge_list_item_count(list)) > 0)
  {
    ElementController *prev= 0;
    
    for (i= 0; i < count; ++i)
    {
      MYX_GRT_VALUE *item= myx_grt_bridge_list_item_get(list, i, 0);
      ElementController *elemItem= elementFromValueObject(_wb->grt(), item);
      
      if (elemItem == elem)
        break;
      
      if (elemItem->gcInstance())
        prev= elemItem;
    }
    return prev;
  }  
  return 0;
}


ElementController *LayerController::frontmostElement(bool realizedOnly)
{
  MYX_GRT_VALUE *list= getValue("elements");
  int i, count;
  if ((count= myx_grt_bridge_list_item_count(list)) > 0)
  {
    for (i= count-1; i >= 0; i--)
    {
      MYX_GRT_VALUE *item= myx_grt_bridge_list_item_get(list, i, 0);
      ElementController *elem= elementFromValueObject(_wb->grt(), item);

      if (!realizedOnly || elem->gcInstance())
        return elem;
    }
  }  
  return 0;
}


void LayerController::raiseElement(ElementController *element, ElementController *above)
{
  // 1- put the element just above the one indicated in "above".
  // 2- if above is 0, then it will be put as the topmost element in this layer.
  // 3- if there's no elements in this layer, it will be put right above the topmost
  // element from topmost sublayer
  // 5- move to top for now
  
  //g_message("raising element %s in layer", element->getStringValue("name"),
   //         getStringValue("name"));
  if (element->gcInstance() && element->layer()==this)
  {
    // the element to be put above must be in the same layer, otherwise
    // just put the element as the topmost in the current layer
    if (above && above->layer() != this)
      above= 0;
    
    // rearrange in canvas
    if (_view && _view->gcLayer())
    {
      CLayer *gcLayer= _view->gcLayer();

      // 1-
      if (above)
        gcLayer->bringToFront(element->gcInstance(), above->gcInstance());
      else
      {
        // 2- if above is 0, then make it the topmost element in its layer
        ElementController *elemUnder;

        // find the element directly under this
        elemUnder= frontmostElement(true);
        if (elemUnder == element)  // if we're already the frontmost element
          // find the element just below us
          elemUnder= realizedElementBelow(element);
        
        if (elemUnder)
          gcLayer->bringToFront(element->gcInstance(), elemUnder->gcInstance());
        else 
        {
          // 3- find the topmost of the sublayers in this layer
          if (_layers.begin() != _layers.end() && (*_layers.rbegin())->frontmostElement(true))
          {
            gcLayer->bringToFront(element->gcInstance(),
                                  (*_layers.rbegin())->frontmostElement(true)->gcInstance());
          }
          else
          {
            // 4- put on top of all
            gcLayer->bringToFront(element->gcInstance());
          }
        }
      }
    }
  }
  // rearrange in layer list
  if (_view)
  {
    myx_grt_value_retain(element->value());
    removeFromList(getValue("elements"), element->getValue("_id"));
    if (above)
    {
      int i= listIndexOfValue(getValue("elements"), above->value());
      myx_grt_bridge_list_item_insert(getValue("elements"), i+1,
                                      element->getValue("_id"), 0);
    }
    else
    {
      // add to the end of the list
      myx_grt_bridge_list_item_insert(getValue("elements"), -1,
                                      element->getValue("_id"), 0);
    }
    myx_grt_value_release(element->value());
  }
}


void LayerController::lowerElement(ElementController *element, ElementController *under)
{
  if (element->layer()==this && (!under || under->layer()==this))
  {
    // rearrange in canvas
    if (_view && _view->gcLayer())
    {
      if (under)
        _view->gcLayer()->sendToBack(element->gcInstance(), under->gcInstance());
      else
      {
        LayerController *layer= element->layer();
        ElementController *under2= 0;
        while (layer)
        {
          under2= layer->backmostElement();
          if (under2)
            break;
          layer= layer->layerUnder();
        }
        
        if (!under2)
          _view->gcLayer()->bringToFront(element->gcInstance(), 0);
        else
          _view->gcLayer()->bringToFront(element->gcInstance(),
                                         under2->gcInstance());
      }
    }
    
    // rearrange in layer list
    if (_view)
    {
      myx_grt_value_retain(element->value());
      removeFromList(getValue("elements"), element->value());
      if (under)
      {
        int i= listIndexOfValue(getValue("elements"), under->value());
        myx_grt_bridge_list_item_insert(getValue("elements"), i,
                                        element->getValue("_id"), 0);
      }
      else
      {
        // add to the start of the list
        myx_grt_bridge_list_item_insert(getValue("elements"), 0,
                                        element->getValue("_id"), 0);
      }
      myx_grt_value_release(element->value());
    }    
  }  
}


LayerController *LayerController::layerAbove()
{
  if (_parent)
  {
    list<LayerController*>::const_iterator iter= find(_parent->_layers.begin(), _parent->_layers.end(), this);
    if (iter != _parent->_layers.end())
    {
      ++iter;
      if (iter != _parent->_layers.end())
        return *iter;
    }
  }
  return 0;
}


LayerController *LayerController::layerUnder()
{
  if (_parent)
  {
    list<LayerController*>::const_iterator iter, prev;
    iter= _parent->_layers.begin();
    prev= _parent->_layers.end();
    while (iter != _parent->_layers.end())
    {
      if (*iter == this)
      {
        if (prev == _parent->_layers.end())
          return 0;
        else
          return *prev;
      }
      prev= iter;
      ++iter;
    }
  }
  return 0;
}


LayerController *LayerController::sublayerContainingBounds(const TBoundingBox &bounds)
{
  LayerController *layer;
  
  // traverse from topmost layer to bottom
  for (list<LayerController*>::reverse_iterator iter= _layers.rbegin();
       iter != _layers.rend(); ++iter)
  {
    layer= (*iter)->sublayerContainingBounds(bounds);
    if (layer)
      return layer;
    
    if ((*iter)->containsBounds(bounds))
      return *iter;
  }
  return 0;
}


void LayerController::setElementStyleColor(CFigureElement *element, const std::string &color)
{
  // set new color style for all child elements
  CElementList* elements= element->children();
  for (CElementList::iterator iterator= elements->begin();
    iterator != elements->end(); 
    ++iterator)
  {
    CFigureElement* child_element = *iterator;

    setElementStyleColor(child_element, color);
  }

  if (element->style())
  {
    // set new color style to the element itself
    std::wstring style_name= element->style()->name();
    std::wstring::size_type pos= style_name.find_last_of(L":");

    if (pos != std::wstring::npos)
    {
      std::wstring new_style_name= style_name.substr(0, pos + 1) + utf8ToUtf16(color);

      CGCModel *model= _wb->canvas()->getModel();
      if (model->styleExists(new_style_name))
      {
        CGCStyle *style= model->style(new_style_name);
        element->style(style);
      }
    }
  }
}


void LayerController::applyFrameProperties(int flags)
{
  _wb->canvas()->beginUpdate();

  if (_gcFigureInstance)
  {
    _gcFigureInstance->disableEvents();
    if (flags & ApplyColor)
    {
      // apply style
      const char *color= getStringValue("color");
      if (color && color[0])
      {
        setElementStyleColor(_gcFigure->content(), color);
      }
    }
    if (flags & ApplyPos)
    {
      // apply geometry
      _gcFigureInstance->translate((float)getDoubleValue("left"),
                                   (float)getDoubleValue("top"),
                                   0.0f);
    }
    if (flags & ApplySize)
    {
      _gcFigure->setSize((float)getDoubleValue("width"),
                         (float)getDoubleValue("height"));
    }
    _gcFigureInstance->enableEvents();

    if (flags & (ApplyPos|ApplySize))
      _view->relocateLayerToContainerLayer(this);
    
    _wb->notifyLayer(this, MYX_WBGC_MODIFIED);
  }
  
  _wb->canvas()->endUpdate();
}


void LayerController::createFrame()
{
  CGCModel *model= _wb->canvas()->getModel();
  const char *layoutClass= "default";
  CFigureTemplate *layout;
  layout= model->layout("db.workbench.LayerFrame", layoutClass);

  if (layout != NULL)
  {
    CLayer *gcLayer= _view->gcLayer();
    if (!gcLayer)
    {
      // realize view if it's not yet
      view()->realizeView(); 
      gcLayer= _view->gcLayer();
    };

    _wb->canvas()->beginUpdate();

    _gcFigure = new CFigure(model, layout);

    CFigureElement* subElement = _gcFigure->elementFromKey("/name");
    const char* description = getStringValue("name");
    subElement->setCaption(description != NULL ? description : "Layer");
    
    _gcFigureInstance= gcLayer->createInstance(_gcFigure);
    _gcFigureInstance->addListener(this);
    _gcFigureInstance->userDataSet((BaseController*)this);

    applyFrameProperties(ApplyAll);

    // put it in the right stacking order
    
    // 1- place above the sibling layer immediately under this one
    // 2- if there's no layer under, then put it right above the parent layer's frame
    // 3- if parent is root, then just put it in the bottom
    
    LayerController *under= layerUnder();
    if (under)
      gcLayer->sendToBack(_gcFigureInstance);
      //gcLayer->bringToFront(_gcFigureInstance, under->layerFrame());
    else
    {
      if (_parent && _parent->layerFrame())
        gcLayer->bringToFront(_gcFigureInstance, _parent->layerFrame());
      else
        gcLayer->sendToBack(_gcFigureInstance);
    }
    
    _wb->canvas()->endUpdate();
    _wb->canvas()->refresh();
  }
}

void LayerController::setView(CanvasViewController *view)
{
  _view= view;
  
  if (_parent)
  {
    setPendingRemake();
//    // setup and bind a frame for the layer if this is not the root layer
//    createFrame();
  }
}


bool LayerController::containsBounds(const TBoundingBox &bounds,
                                     bool totally)
{
  double lx1, ly1, lx2, ly2;
  double fx1, fy1, fx2, fy2;
  
  lx1= getDoubleValue("left");
  ly1= getDoubleValue("top");
  lx2= lx1 + getDoubleValue("width");
  ly2= ly1 + getDoubleValue("height");
  
  fx1= bounds.upper.x;
  fy1= bounds.upper.y;
  fx2= bounds.lower.x;
  fy2= bounds.lower.y;
  
  if (totally)
  {
    if (fx1 >= lx1 && fy1 >= ly1 && fx2 <= lx2 && fy2 <= ly2)
      return true;
  }
  else
  {
    if (((fx1 >= lx1 && fx1 <= lx2) || (fx2 >= lx1 && fx2 <= lx2))
        && ((fy1 >= ly1 && fy1 <= ly2) || (fy2 >= ly1 && fy2 <= ly2)))
      return true;
  }

  return false;
}


void LayerController::restackElements()
{
  int i, c;
  MYX_GRT_VALUE *list= getValue("elements");
  MYX_GRT *grt= _wb->grt();
  CLayer *gcLayer;
  LayerController *aboveL;
  CFigureInstance *topFigure;
  
  if (!_view) return;
  
  gcLayer= _view->gcLayer();
  
  _ownerModel->canvas()->beginUpdate();
  
  
  aboveL= layerAbove();
  if (aboveL)
  {
    ElementController *elem= aboveL->frontmostElement(true);
    if (elem)
      topFigure= elem->gcInstance();
    else
      topFigure= 0;
  }
  else
    topFigure= 0;
  
  c= myx_grt_bridge_list_item_count(list);
  for (i = 0; i < c; i++)
  {
    MYX_GRT_VALUE *item= myx_grt_bridge_list_item_get(list, i, 0);
    ElementController *elem= elementFromValueObject(grt, item);
    if (elem && elem->gcInstance())
    {
      gcLayer->bringToFront(elem->gcInstance(), topFigure);
      topFigure= elem->gcInstance();
    }
  }

  for (std::list<LayerController*>::const_iterator iter= _layers.begin();
       iter != _layers.end(); ++iter)
  {
    (*iter)->restackElements();
  }
  _ownerModel->canvas()->endUpdate();
}


// ==============================================================================

ElementController::ElementController(WorkbenchController *wb, MYX_GRT_VALUE *value)
: BaseController(wb, value), RemakableObject(wb), _gcFigure(0), _gcFigureInstance(0), _layer(0)
{
  _moved= false;
  _pendingChange= false;
  _pendingDestroy= false;
  _type= "element";
  _initialSizeSet= false;
}


TBoundingBox ElementController::grtBounds()
{
  TBoundingBox box;
  box.upper.x= (float) getDoubleValue("left");
  box.upper.y= (float) getDoubleValue("top");
  box.lower.x= box.upper.x + (float) getDoubleValue("width");
  box.lower.y= box.upper.y + (float) getDoubleValue("height");
  return box;
}


TBoundingBox ElementController::gcBounds()
{
  return _gcFigureInstance->bounds();
}


void ElementController::remakeDone()
{
  RemakableObject::remakeDone();
  if (_pendingDestroy)
  {
    RemakableObject::remakeDone();
    release();
  }
  else
    RemakableObject::remakeDone();
}


void ElementController::remake()
{
  if (!_pendingDestroy && !realizeConditionsMet())
  {
    INTERNAL_ERROR("trying to remake object in invalid state");
    return;
  }
   
  if (_layer)
  {
    bool firstTime= true;
    MYX_GRT_VALUE *object= objectData();
    if (!object)
      object= _value;

    // destroy the old table
    if (_gcFigureInstance)
    {
      firstTime= false;
      unrealizeFigure();
    }
    
    if (!_pendingDestroy)
    {
      // create new and add to canvas
      realizeFigure();
      
      if (firstTime && object != _value)
      {
        // this is the 1st time we're being called, setup the listeners
        setupListener(object);
      }
    }
  }
}


void ElementController::setPendingDestroy()
{
  _pendingDestroy= true;
  RemakableObject::setPendingRemake();
  
  _wb->notifyElement(this, MYX_WBGC_REMOVED);
}


void ElementController::setPendingRemake()
{
  if (getIntValue("visible")!=0)
  {
    RemakableObject::setPendingRemake();
    
    // this will notify the app that something has changed and it should
    // call the pending flush callback whenever it can
    _wb->notifyElement(this, MYX_WBGC_MODIFIED);
  }
}


int ElementController::grtListenerCallback(MYX_GRT *grt, MYX_GRT_VALUE *value, MYX_GRT_VALUE_CALLBACK_REASON reason, void *data)
{
  ElementController *self= (ElementController*)data;;

  if (!self->_pendingChange)
  {
    MYX_GRT_VALUE *data= self->objectData();

    self->setPendingRemake();
    if (reason != MYX_GVCR_DELETE)
      self->setupListener(value);
    
    // make the element name and the object name match
    if (data)
    {
      const char *name= myx_grt_dict_item_get_as_string(data, "name");
      if (name && strcmp(self->getStringValue("name"), name)!=0)
      {
        MYX_GRT_VALUE *nname= myx_grt_value_from_string(name);
        myx_grt_dict_item_set_value(self->_value, "name", nname);
        myx_grt_value_release(nname);
      }
    }
  }
  return 0;
}


void ElementController::setupListener(MYX_GRT_VALUE *value)
{
  unsigned int i, count;
  MYX_GRT_VALUE *item;
  MYX_GRT_VALUE_TYPE type;
    
  switch (myx_grt_value_get_type(value))
  {
    case MYX_DICT_VALUE:
    {
      // duped listeners are checked in listener_add
      myx_grt_value_listener_add(_wb->grt(), value, this, &ElementController::grtListenerCallback);

      count= myx_grt_bridge_dict_item_count(value, 0);
      for (i= 0; i < count; i++)
      {
        item= myx_grt_bridge_dict_item_value_by_index(value, i, 0);

        type= myx_grt_value_get_type(item);
        if (type == MYX_DICT_VALUE || type == MYX_LIST_VALUE)
        {
          if (!myx_grt_value_listener_get(_wb->grt(), item, this, &ElementController::grtListenerCallback))
            setupListener(item);
        }
      }
      break;
    }
    case MYX_LIST_VALUE:
    {
      myx_grt_value_listener_add(_wb->grt(), value, this, &ElementController::grtListenerCallback);

      count= myx_grt_bridge_list_item_count(value);
      for (i= 0; i < count; i++)
      {
        item= myx_grt_bridge_list_item_get(value, i, 0);
        
        type= myx_grt_value_get_type(item);
        if (type == MYX_DICT_VALUE || type == MYX_LIST_VALUE)
        {
          if (!myx_grt_value_listener_get(_wb->grt(), item, this, &ElementController::grtListenerCallback))
            setupListener(item);
        }
      }
      break;
    }
    default:
      break;
  }
}


void ElementController::moveTo(double x, double y)
{ 
  if (!_gcFigureInstance)
  {
    myx_grt_bridge_value_change_real(getValue("left"), x);
    myx_grt_bridge_value_change_real(getValue("top"), y);
  }
  else
    _gcFigureInstance->translate((float)x, (float)y, 0, false);
}


void ElementController::applyProperties()
{
  _gcFigureInstance->translate((float)getDoubleValue("left"),
                            (float)getDoubleValue("top"),
                            0);
/*
  if (!_gcFigure->setSize((float)getDoubleValue("width"),
                          (float)getDoubleValue("height")+50))
    g_message("set size failed");
*/
  _gcFigureInstance->setSize((float)getDoubleValue("width"),
                             (float)getDoubleValue("height"));
  
  TGCVariant var;
  var= getIntValue("expanded");
  CFigureElement* element = _gcFigure->elementFromKey("/");
  if (element != NULL && element->expanded())
  {
    element->executeAction(GC_ACTION_TOGGLE);
  }
}


void ElementController::addPartsFromDict(CFigureElement* element, MYX_GRT_VALUE *source, const string &path)
{
//  const char *_id= myx_grt_dict_item_get_as_string(source, "_id");
//  myx_grt_reference_cache_lookup(_wb->grt(), _id);
  
  unsigned int count = myx_grt_dict_item_count(source);
  for (unsigned int i = 0; i < count; ++i)
  {
    MYX_GRT_VALUE* child = myx_grt_dict_item_value_by_index(source, i);
    string localPath = path + "/" + myx_grt_dict_item_key_by_index(source, i);
    CFigureElement* subElement = element->figure()->elementFromKey(localPath.c_str());
    if (subElement != NULL)
    {
      switch (myx_grt_value_get_type(child))
      {
        case MYX_LIST_VALUE:
        {
          addPartsFromList(subElement, child, localPath);
          break;
        };
        case MYX_DICT_VALUE:
        {
          addPartsFromDict(subElement, child, localPath);
          break;
        };
        default:
        {
          const char* text= 0;
          
          if (localPath == "/indices/index/columns/column/name")
          {
            const char *refColumn= myx_grt_dict_item_get_as_string(source, "referedColumn");
            if (refColumn)
            {
              MYX_GRT_VALUE *column;
              // hack to take the proper name for the index column
              column= myx_grt_reference_cache_lookup(_wb->grt(), refColumn);
              if (column)
                text= myx_grt_dict_item_get_as_string(column, "name");
            }
          }
          if (!text)
            text= myx_grt_value_as_string(child);

          if (text != NULL)
            subElement->setCaption(text);
          break;
        };
      };
    };
  };
}


char *ElementController::formatColumn(MYX_GRT_VALUE *column)
{
  const char *name;
  const char *typeName;
  const char *typeParams;
  
  name= myx_grt_dict_name_item_as_string(column);
  typeName= myx_grt_dict_item_get_as_string(column, "datatypeName");
  typeParams= myx_grt_dict_item_get_as_string(column, "datatypeExplicitParams");
  
  if (typeParams && *typeParams)
  {
    if (*typeParams == '(')
      return g_strdup_printf("%s: %s%s", name, typeName, typeParams);
    else
      return g_strdup_printf("%s: %s(%s)", name, typeName, typeParams);
  }
  else
    return g_strdup_printf("%s: %s", name, typeName);
}


void ElementController::fixupElementStyle(CFigureElement *element, MYX_GRT_VALUE *data)
{
}


void ElementController::addPartsFromList(CFigureElement* element, MYX_GRT_VALUE* source, const string &path)
{
  if (element != NULL)
  {
    // The sub element key that addresses the sub element in the figure element list is determined
    // by the type of the GRT values in the list.
    string localPath = path;
    bool isDbColumn = false;
    const char* elementType = myx_grt_list_content_get_struct_name(source);
    if (myx_grt_struct_is_or_inherits_from(_wb->grt(), elementType, "db.Column"))
    {
      localPath += "/column";
      isDbColumn = true;
    }
    else if (myx_grt_struct_is_or_inherits_from(_wb->grt(), elementType, "db.ForeignKey"))
      localPath += "/foreignKey";
    else if (myx_grt_struct_is_or_inherits_from(_wb->grt(), elementType, "db.Index"))
      localPath += "/index";
    else if (myx_grt_struct_is_or_inherits_from(_wb->grt(), elementType, "db.IndexColumn"))
      localPath += "/column";
    else if (myx_grt_struct_is_or_inherits_from(_wb->grt(), elementType, "db.Trigger"))
      localPath += "/trigger";

    element->userDataSet(source);
      
    unsigned int count = myx_grt_list_item_count(source);
    switch (myx_grt_list_content_get_type(source))
    {
      case MYX_LIST_VALUE:
      {
        for (unsigned int i = 0; i < count; ++i)
        {
          MYX_GRT_VALUE* item = myx_grt_list_item_get(source, i);
          CFigureElement* child = element->addSubElement();
          addPartsFromList(child, item, localPath);
        };
        break;
      };
      case MYX_DICT_VALUE:
      {
        for (unsigned int i = 0; i < count; ++i)
        {
          MYX_GRT_VALUE* item = myx_grt_list_item_get(source, i);
          CFigureElement* child = element->addSubElement();

          fixupElementStyle(child, item);
          // Format normal column name output differently.
          if (isDbColumn)
          {
            char* caption = formatColumn(item);
            if (caption)
            {
              string captionPath = localPath + "/name";
              CFigureElement* captionElement = element->figure()->elementFromKey(captionPath.c_str());
              if (captionElement != NULL)
                captionElement->setCaption(caption);
              
              captionElement->userDataSet(item);
            };
            g_free(caption);
            
            child->userDataSet(item);
          }
          else
            addPartsFromDict(child, item, localPath);
        };
        break;
      };
      case MYX_STRING_VALUE:
      {
        // String list elements are usually reference ids.
        for (unsigned int i = 0; i < count; ++i)
        {
          MYX_GRT_VALUE* item = myx_grt_list_item_get(source, i);
          CFigureElement* child = element->addSubElement();

          const char* objectId = myx_grt_value_as_string(item);
          if (objectId != NULL)
          {
//            string captionPath = localPath + "/name";
//            CFigureElement* captionElement = element->figure()->elementFromKey(captionPath.c_str());

            // This might be a normal property or a reference to another value. Check this.
            MYX_GRT_VALUE* target = myx_grt_reference_cache_lookup(_wb->grt(), objectId);
            if (target != NULL)
            {
              addPartsFromDict(child, target, localPath);
              
              const char* targetCaption = myx_grt_dict_name_item_as_string(target);
              
              string captionPath = localPath + "/name";
              CFigureElement* captionElement = element->figure()->elementFromKey(captionPath.c_str());
              if (captionElement != NULL)
              {
                captionElement->setCaption(targetCaption);
              
                captionElement->userDataSet(target);
              }
            };
          };
        };
        break;
      };
      default:
      {
        // Not handled. Not sure what to do here.
        break;
      };
    };
  };
}




void ElementController::updatePartsFromList(CFigureElement* element, MYX_GRT_VALUE* source, const string &path)
{
  if (element != NULL)
  {
    // The sub element key that addresses the sub element in the figure element list is determined
    // by the type of the GRT values in the list.
    string localPath = path;
    bool isDbColumn = false;
    const char* elementType = myx_grt_list_content_get_struct_name(source);
    if (myx_grt_struct_is_or_inherits_from(_wb->grt(), elementType, "db.Column"))
    {
      localPath += "/column";
      isDbColumn = true;
    }
    else if (myx_grt_struct_is_or_inherits_from(_wb->grt(), elementType, "db.ForeignKey"))
      localPath += "/foreignKey";
    else if (myx_grt_struct_is_or_inherits_from(_wb->grt(), elementType, "db.Index"))
      localPath += "/index";
    else if (myx_grt_struct_is_or_inherits_from(_wb->grt(), elementType, "db.IndexColumn"))
      localPath += "/column";
    else if (myx_grt_struct_is_or_inherits_from(_wb->grt(), elementType, "db.Trigger"))
      localPath += "/trigger";

    element->userDataSet(source);
      
    unsigned int count = myx_grt_list_item_count(source);
    switch (myx_grt_list_content_get_type(source))
    {
      case MYX_LIST_VALUE:
      {
        for (unsigned int i = 0; i < count; ++i)
        {
          MYX_GRT_VALUE* item = myx_grt_list_item_get(source, i);
          CFigureElement* child = element->addSubElement();
          addPartsFromList(child, item, localPath);
        };
        break;
      };
      case MYX_DICT_VALUE:
      {
        for (unsigned int i = 0; i < count; ++i)
        {
          MYX_GRT_VALUE* item = myx_grt_list_item_get(source, i);
          CFigureElement* child = element->addSubElement();

          fixupElementStyle(child, item);
          // Format normal column name output differently.
          if (isDbColumn)
          {
            char* caption = formatColumn(item);
            if (caption)
            {
              string captionPath = localPath + "/name";
              CFigureElement* captionElement = element->figure()->elementFromKey(captionPath.c_str());
              if (captionElement != NULL)
                captionElement->setCaption(caption);
              
              captionElement->userDataSet(item);
            };
            g_free(caption);
            
            child->userDataSet(item);
          }
          else
            addPartsFromDict(child, item, localPath);
        };
        break;
      };
      case MYX_STRING_VALUE:
      {
        // String list elements are usually reference ids.
        for (unsigned int i = 0; i < count; ++i)
        {
          MYX_GRT_VALUE* item = myx_grt_list_item_get(source, i);
          CFigureElement* child = element->addSubElement();

          const char* objectId = myx_grt_value_as_string(item);
          if (objectId != NULL)
          {
//            string captionPath = localPath + "/name";
//            CFigureElement* captionElement = element->figure()->elementFromKey(captionPath.c_str());

            // This might be a normal property or a reference to another value. Check this.
            MYX_GRT_VALUE* target = myx_grt_reference_cache_lookup(_wb->grt(), objectId);
            if (target != NULL)
            {
              addPartsFromDict(child, target, localPath);
              
              const char* targetCaption = myx_grt_dict_name_item_as_string(target);
              
              string captionPath = localPath + "/name";
              CFigureElement* captionElement = element->figure()->elementFromKey(captionPath.c_str());
              if (captionElement != NULL)
              {
                captionElement->setCaption(targetCaption);
              
                captionElement->userDataSet(target);
              }
            };
          };
        };
        break;
      };
      default:
      {
        // Not handled. Not sure what to do here.
        break;
      };
    };
  };
}


MYX_GRT_VALUE *ElementController::objectData()
{
  return NULL;
}


CFigureTemplate* ElementController::layoutType()
{
  CGCModel* model = _wb->canvas()->getModel();
  MYX_GRT_STRUCT* grtType = myx_grt_dict_struct_get(_wb->grt(), _value);
  const char *layoutClass;
  bool retry= true;

  layoutClass= getStringValue("layoutClass");
  if (!layoutClass || !*layoutClass)
    layoutClass= layer()->view()->getStringValue("elementLayoutClass");
    
  if (!layoutClass || !*layoutClass)
    layoutClass= "default";
  
again:  
  CFigureTemplate* layout;
  do
  {
    const char* typeName = myx_grt_struct_get_name(grtType);
    layout = model->layout(typeName, layoutClass);
    if (layout != NULL)
      break;
    
    const char* parentName = myx_grt_struct_get_parent_name(grtType);
    // Break out if we reached the top of the hierarchy.
    if (parentName == NULL)
      break;
    grtType = myx_grt_struct_get(_wb->grt(), parentName);
    
  } while (layout == NULL); 
  
  if (layout == NULL && strcmp(layoutClass, "default")!=0 && retry)
  {
    retry= false;
    goto again;
  }

  if (layout == NULL)
  {
    // If layout is NULL then we did not find a layout yet for the given type and class.
    // In this case we fall back to the icon class of the top element. This must
    // always be defined.
    const char* typeName = myx_grt_struct_get_name(grtType);
    layout = model->layout(typeName, "icon");
  }

  return layout;
}


void ElementController::assembleFigure()
{
  // setup the figure
  CFigureElement *content= _gcFigure->content();
  MYX_GRT_VALUE *data= objectData();
  if (!data) data= _value;

  addPartsFromDict(content, data, "");
}


bool ElementController::realizeFigure()
{
  CGCModel* model = _wb->canvas()->getModel();
  CFigureTemplate* layout = layoutType();
  
  // If there is still no layout then we can't do anything.
  if (layout != NULL)
  {
    CLayer *gcLayer= _layer->view()->gcLayer();

    if (!gcLayer)
    {
      // realize view if it's not yet
      _layer->view()->realizeView();
      gcLayer= _layer->view()->gcLayer();
    };
    
    _gcFigure = new CFigure(model, layout);
    _gcFigure->userDataSet((BaseController*)this);
    
    _gcFigure->beginUpdate();
        
    assembleFigure();
    
    // create an instance of the figure for the current view
    _gcFigureInstance= gcLayer->createInstance(_gcFigure);
    _gcFigureInstance->userDataSet((BaseController*)this);
    _gcFigureInstance->addListener(this);
      
    // put the element in the right stacking order
    _layer->restackElements();

    if (!_initialSizeSet)
    {
      // get initial size
      myx_grt_bridge_value_change_real(getValue("width"), _gcFigureInstance->width());
    
      myx_grt_bridge_value_change_real(getValue("height"), _gcFigureInstance->height());
    }
 
    applyProperties();

    _gcFigure->validate();

    _gcFigure->endUpdate();

//    myx_grt_value_listener_add(_wb->grt(), _value, this, valueCallback);
    return true;
  }

  return false;
}


void ElementController::unrealizeFigure()
{
  _gcFigureInstance->removeListener(this);
  _layer->view()->gcLayer()->removeInstance(_gcFigureInstance);
  _gcFigureInstance= 0;
  
  // canvas will be notified automatically 
  delete _gcFigure;
  _gcFigure= 0;  
}


void ElementController::setLayer(LayerController *lctl, bool fromLayer)
{
  if (lctl != _layer)
  {
    if (!fromLayer)
    {
      if (_layer)
      {
        //g_message("moving element %s from layer %s to %s",
        //          getStringValue("name"), 
         //         _layer->getStringValue("name"), lctl->getStringValue("name"));

        _layer->removeElementRef(this);
      }
      lctl->addElementRef(this);
    }
    myx_grt_bridge_value_change_string(getValue("layer"), lctl->getStringValue("_id"));

    _layer= lctl;

    _wb->notifyElement(this, MYX_WBGC_CHANGED_LAYER);

    // always place at the top of the new layer
    _layer->raiseElement(this, 0);
  }
  
  if (!_gcFigureInstance && realizeConditionsMet())
    setPendingRemake();
}


bool ElementController::valueFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  if (strcmp2(key, "layer")==0)
  {
    LayerController *lctl= (LayerController*)_wb->objectForId(myx_grt_value_as_string(value));
    if (lctl)
    {
      setLayer(lctl, false);
      if (!_gcFigureInstance && realizeConditionsMet())
        setPendingRemake();
    }
  }
  else if (strcmp2(key, "top")==0)
  {
    if (_gcFigureInstance)
    {
      double y= myx_grt_value_as_real(value);
      double x= getDoubleValue("left");
      _gcFigureInstance->translate((float)x, (float)y, 0);
      _layer->view()->addPendingElementRelocation(this);
    }
  }
  else if (strcmp2(key, "left")==0)
  {
    if (_gcFigureInstance)
    {
      double x= myx_grt_value_as_real(value);
      double y= getDoubleValue("top");
      _gcFigureInstance->translate((float)x, (float)y, 0);
      _layer->view()->addPendingElementRelocation(this);
    }
  }
  else if (strcmp2(key, "width")==0)
  {
    _initialSizeSet= true; 
    if (_gcFigureInstance)
    {
      applyProperties();
      // write the actual final size
      myx_grt_bridge_value_change_real(value, _gcFigureInstance->width());
      _layer->view()->addPendingElementRelocation(this);
    }
  }
  else if (strcmp2(key, "height")==0)
  {
    _initialSizeSet= true;
    if (_gcFigureInstance)
    {
      applyProperties();
      // write the actual final size
      myx_grt_bridge_value_change_real(value, _gcFigureInstance->height());
      _layer->view()->addPendingElementRelocation(this);
    }
  }
  else if (strcmp2(key, "visible")==0)
  {
    if (!_gcFigureInstance && realizeConditionsMet())
      setPendingRemake();
  }
  else if (strcmp2(key, "sql")==0)
  {
  }
  else if (strcmp2(key, "layoutClass")==0)
  {
    setPendingRemake();
  }
  else
    return BaseController::valueFromGrt(key, value);
  return true;
}


void ElementController::onAction(CGCBase* sender, CGCBase* origin, TAction** action)
{
  MYX_GRT_ERROR error;
  MYX_GRT_VALUE *args;
  MYX_GRT_VALUE *result;
  MYX_GRT_VALUE *arg;

  if ((*action)->type >= GC_ACTION_APPLICATION)
  {
    args= myx_grt_list_new(MYX_ANY_VALUE, NULL);
    myx_grt_list_item_add(args, objectData());
    myx_grt_list_item_add(args, _value);
    arg= myx_grt_value_from_int((*action)->type);
    myx_grt_list_item_add(args, arg);
    myx_grt_value_release(arg);
    
    result= myx_grt_function_get_and_call(_wb->grt(),
                                          "Base","editObj",
                                          0, args, &error);
    if (result && myx_grt_dict_item_get_value(result, "error"))
    {
      g_message("error: %s", myx_grt_dict_item_get_as_string(result, "error"));
    }
    
    if (result) myx_grt_value_release(result);
    if (args) myx_grt_value_release(args);
  }
}


void ElementController::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  switch (reason)
  {
  case GC_CHANGE_FINSTANCE_PROPERTY:
    {
      TGCVariant gcValue = _gcFigureInstance->propertyGet("translation", 0);
      float newX, newY;

      newX= gcValue;
      gcValue = _gcFigureInstance->propertyGet("translation", 1);
      newY= gcValue;
      
      if (getDoubleValue("left") != newX ||
          getDoubleValue("top") != newY)
      {
        myx_grt_bridge_value_change_real(getValue("left"), newX);
        
        myx_grt_bridge_value_change_real(getValue("top"), newY);
        
        if (!_moved)
        {
          _moved= true;
          _layer->view()->addPendingElementRelocation(this);
        }
      }
    }
    break;
  
  case GC_CHANGE_FIGURE_RESIZE:
    myx_grt_bridge_value_change_real(getValue("width"), _gcFigureInstance->width());
    
    myx_grt_bridge_value_change_real(getValue("height"), _gcFigureInstance->height());
    break;
    
  case GC_CHANGE_ELEMENT_PROPERTY:
    onElementChange((CFigureElement*)origin);
    break;

  default:
    break;
  }
}


void ElementController::onElementChange(CFigureElement *element)
{
}


bool ElementController::realizeConditionsMet()
{
  if (_layer && getIntValue("visible")!=0)
    return true;
  return false;
}

//---------------------------------------------------------------------------

TableController::TableController(WorkbenchController *wb, MYX_GRT_VALUE *value)
: ElementController(wb, value)
{
  _type= "table";
  _applying= false;
}


TableController::~TableController()
{
}


MYX_GRT_VALUE *TableController::objectData()
{
  const char *id= getStringValue("table");
  return myx_grt_reference_cache_lookup(_wb->grt(), id);
}


bool TableController::realizeConditionsMet()
{
  if (referenceIsSet("table") && ElementController::realizeConditionsMet())
    return true;
  return false;
}


static CFigureElement *subelementWithObject(CFigureElement *element, MYX_GRT_VALUE *detail)
{
  if (element->userDataGet() == detail)
    return element;
  
  CElementList *elist= element->children();
  
  for (CElementList::const_iterator iter= elist->begin(); iter != elist->end(); ++iter)
  {
    CFigureElement *elem= subelementWithObject(*iter, detail);
    if (elem)
      return elem;
  }
  return 0;
}


TBoundingBox TableController::detailFrame(MYX_GRT_VALUE *detail)
{
  TBoundingBox box;
  
  box.upper.x= 0;
  box.upper.y= 0;
  box.lower.x= 0;
  box.lower.y= 0;
  
  if (myx_grt_struct_is_or_inherits_from(_wb->grt(), myx_grt_dict_struct_get_name(detail), "db.Column"))
  {
    CFigureElement *elem= subelementWithObject(_gcFigure->content(), detail);
    if (elem)
    {
      TBoundingBox ebox= elem->bounds();
      
      ebox.upper= elem->elementToFigure(ebox.upper);
      ebox.lower= elem->elementToFigure(ebox.lower);
      _gcFigure->figureToView(ebox.upper, _gcFigureInstance, box.upper);
      _gcFigure->figureToView(ebox.lower, _gcFigureInstance, box.lower);
    }
  }

  return box;
}


bool TableController::checkIfPK(MYX_GRT_VALUE *column)
{
  MYX_GRT_VALUE *pk= myx_grt_dict_item_get_reference_value(_wb->grt(), objectData(), "primaryKey");
  MYX_GRT_VALUE *pkColumns;
  const char *columnId= myx_grt_dict_id_item_as_string(column);
  unsigned int i;
  
  if (!pk)
    return false;
  
  pkColumns= myx_grt_dict_item_get_value(pk, "columns");
  
  for (i= 0; i < myx_grt_list_item_count(pkColumns); i++)
  {
    MYX_GRT_VALUE *column= myx_grt_list_item_get(pkColumns, i);
    MYX_GRT_VALUE *refColumn= myx_grt_dict_item_get_reference_value(_wb->grt(), column, "referedColumn");

    if ((refColumn != NULL) && strcmp(myx_grt_dict_id_item_as_string(refColumn), columnId)==0)
      return true;
  }
  return false;
}


bool TableController::checkIfFK(MYX_GRT_VALUE *column)
{
  MYX_GRT_VALUE *fkList= myx_grt_dict_item_get_value(objectData(), "foreignKeys");
  const char *columnId= myx_grt_dict_id_item_as_string(column);
  unsigned int i;
  
  if (!fkList)
    return false;
  
  for (i= 0; i < myx_grt_bridge_list_item_count(fkList); i++)
  {
    MYX_GRT_VALUE *fk= myx_grt_bridge_list_item_get(fkList, i, 0);
    MYX_GRT_VALUE *columns= myx_grt_bridge_dict_item_get_value(fk, "columns", 0);
    unsigned int j;
    for (j= 0; j < myx_grt_bridge_list_item_count(columns); j++)
    {
      MYX_GRT_VALUE *column= myx_grt_bridge_list_item_get(columns, j, 0);

      if (strcmp(myx_grt_value_as_string(column), columnId)==0)
        return true;
    }
  }
  return false;  
}


void TableController::fixupElementStyle(CFigureElement *element, MYX_GRT_VALUE *data)
{
  std::wstring styleName;
  if (checkIfPK(data))
    styleName= L"db.table.column.left.pk";
  else if (checkIfFK(data))
    styleName= L"db.table.column.left.fk";

  if (!styleName.empty())
  {
    CGCModel *model= _wb->canvas()->getModel();
    
    CElementList *list= element->children();
    
    if (list)
    {
      for (CElementList::const_iterator iter= list->begin();
           iter != list->end(); ++iter)
      {
        if ((*iter)->template_()->key() == L"/columns/column/icon")
        {
          (*iter)->style(model->style(styleName));
          break;
        }
      }
    }
  }
}


void TableController::applyProperties()
{
  TGCVariant var;
  CFigureElement *elem, *toggle;

  _applying= true;
  
  if ((elem = _gcFigure->elementFromId(L"/table-header/table-header-right")))
  {
    if (_gcFigure->elementFromId(L"/table-body")->expanded() != (getIntValue("expanded")!=0))
    {
      elem->executeAction(GC_ACTION_TOGGLE);
    }
  }
  
  if ((elem = _gcFigure->elementFromKey("/columns")) && _gcFigureInstance)
  {
    if ((toggle = _gcFigure->elementFromKey("columns-toggler")))
    {
      if (elem->expanded() != (getIntValue("columnsExpanded")!=0))
        toggle->executeAction(GC_ACTION_TOGGLE);
    }
  }
  
  if ((elem = _gcFigure->elementFromKey("/indices")) && _gcFigureInstance)
  {
    if ((toggle = _gcFigure->elementFromKey("indexes-toggler")))
    {
      if (elem->expanded() != (getIntValue("indicesExpanded")!=0))
        toggle->executeAction(GC_ACTION_TOGGLE);
    }    
  }

  if ((elem = _gcFigure->elementFromKey("/foreignKeys")) && _gcFigureInstance)
  {
    if ((toggle = _gcFigure->elementFromKey("foreignKeys-toggler")))
    {
      if (elem->expanded() != (getIntValue("foreignKeysExpanded")!=0))
        toggle->executeAction(GC_ACTION_TOGGLE);
    }
  }
  
  if ((elem = _gcFigure->elementFromKey("/triggers")) && _gcFigureInstance)
  {
    if ((toggle = _gcFigure->elementFromKey("trigger-toggler")))
    {
      if (elem->expanded() != (getIntValue("triggersExpanded")!=0))
        toggle->executeAction(GC_ACTION_TOGGLE);
    }
  }
  
  
  const char *color= getStringValue("color");
  if (color && color[0])
  {
    _layer->setElementStyleColor(_gcFigure->content(), color);
  }      
  
  ElementController::applyProperties();
  
  _applying= false;
}


bool TableController::valueFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  bool handled= true;
  TGCVariant var;
  CFigureElement *elem;
  
  _applying= true;
  
  if (strcmp2(key, "table")==0)
  {
    if (myx_grt_value_get_type(value) == MYX_STRING_VALUE &&
        *myx_grt_value_as_string(value))
      
      if (!_gcFigureInstance && realizeConditionsMet())
        setPendingRemake();
  }  
  else if (strcmp2(key, "expanded")==0)
  {
    if (_gcFigureInstance)
    {
      if ((elem = _gcFigure->elementFromKey("table-toggler")))
      {
        if (_gcFigure->elementFromKey("/table-body")->expanded() != (getIntValue("expanded")!=0))
        {
          elem->executeAction(GC_ACTION_TOGGLE);
        }
      }
    }
  }    
  else if (strcmp2(key, "columnsExpanded")==0)
  {
    if (_gcFigureInstance)
    {
      if ((elem = _gcFigure->elementFromKey("columns-toggler")))
      {
        CFigureElement *columnsPart= _gcFigure->elementFromKey("/columns");
        if (columnsPart && columnsPart->expanded() != (getIntValue("columnsExpanded")!=0))
          elem->executeAction(GC_ACTION_TOGGLE);
      }
    }
  }
  else if (strcmp2(key, "indicesExpanded")==0)
  {
    if (_gcFigureInstance)
    {
      if ((elem = _gcFigure->elementFromKey("indexes-toggler")))
      {
        CFigureElement *indicesPart= _gcFigure->elementFromKey("/indices");
        if (indicesPart && indicesPart->expanded() != (getIntValue("indicesExpanded")!=0))
          elem->executeAction(GC_ACTION_TOGGLE);
      }
    }    
  }
  else if (strcmp2(key, "foreignKeysExpanded")==0)
  {
    if (_gcFigureInstance)
    {
      if ((elem = _gcFigure->elementFromKey("foreignKeys-toggler")))
      {
        CFigureElement *fkPart= _gcFigure->elementFromKey("/foreignKeys");
        if (fkPart && fkPart->expanded() != (getIntValue("foreignKeysExpanded")!=0))
          elem->executeAction(GC_ACTION_TOGGLE);
      }
    }
  }
  else if (strcmp2(key, "triggersExpanded")==0)
  {
    if (_gcFigureInstance)
    {
      if ((elem = _gcFigure->elementFromKey("trigger-toggler")))
      {
        CFigureElement *triggerPart= _gcFigure->elementFromKey("/triggers");
        if (triggerPart && triggerPart->expanded() != (getIntValue("triggersExpanded")!=0))
          elem->executeAction(GC_ACTION_TOGGLE);
      }
    }
  }  
  else if (strcmp2(key, "color")==0)
  {
    if (_gcFigureInstance)
    {
      _wb->canvas()->beginUpdate();

      const char *color= getStringValue("color");
      if (color && color[0])
      {
        _layer->setElementStyleColor(_gcFigure->content(), color);
      }
      _wb->canvas()->endUpdate();
      _wb->canvas()->refresh();
    }
  }
  else
    handled= false;

  if (!handled)
    handled= ElementController::valueFromGrt(key, value);

  _applying= false;
  
  return handled;
}


void TableController::onElementChange(CFigureElement *element)
{
  if (_gcFigureInstance && !_applying)
  {
    CFigureElement* element = _gcFigure->elementFromId(L"/table-body");
    if (element != NULL)
      myx_grt_bridge_value_change_int(getValue("expanded"), element->expanded());
    element = _gcFigure->elementFromKey("/columns");
    if (element != NULL)
      myx_grt_bridge_value_change_int(getValue("columnsExpanded"), element->expanded());
    element = _gcFigure->elementFromKey("/indices");
    if (element != NULL)
      myx_grt_bridge_value_change_int(getValue("indicesExpanded"), element->expanded());
    element = _gcFigure->elementFromKey("/foreignKeys");
    if (element != NULL)
      myx_grt_bridge_value_change_int(getValue("foreignKeysExpanded"), element->expanded());
    element = _gcFigure->elementFromKey("/triggers");
    if (element != NULL)
      myx_grt_bridge_value_change_int(getValue("triggersExpanded"), element->expanded());
  }
  
  //ElementController::onElementChange(element);
}


bool TableController::realizeFigure()
{
  MYX_GRT_VALUE *rels= _layer->view()->getValue("relationships");
  int i, c= myx_grt_bridge_list_item_count(rels);
  const char *id= getStringValue("_id");
  
  if (!ElementController::realizeFigure())
    return false;
  
  // go through all relationships and check whether there's any that must be recreated
  for (i= 0; i < c; i++)
  {
    MYX_GRT_VALUE *rel= myx_grt_bridge_list_item_get(rels, i, 0);
    RelationshipController *rc= relationshipFromValueObject(_wb->grt(), rel);
    
    if (rc && (strcmp2(rc->getStringValue("startTable"), id)==0
               || strcmp2(rc->getStringValue("endTable"), id)==0))
      rc->setPendingRemake();
  }
  return true;
}


//----------------------------------------------------------------------------

ViewController::ViewController(WorkbenchController *wb, MYX_GRT_VALUE *value)
: ElementController(wb, value)
{
  _type= "view";
}


MYX_GRT_VALUE *ViewController::objectData()
{
  const char *id= getStringValue("view");
  return myx_grt_reference_cache_lookup(_wb->grt(), id);
}


void ViewController::applyProperties()
{
  TGCVariant var;
  CFigureElement *elem;
  
  if ((elem = _gcFigure->elementFromId(L"/view-header/view-header-right/view-header-right-tools/view-header-header-right")))
  {
    if (_gcFigure->elementFromId(L"/view-body")->expanded() != (getIntValue("expanded")!=0))
    {
      elem->executeAction(GC_ACTION_TOGGLE);
    }
  }
  
  ElementController::applyProperties();
}


bool ViewController::valueFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  if (strcmp2(key, "expanded")==0)
  {
    if (_gcFigure)
      applyProperties();
    return true;
  }
  
  return ElementController::valueFromGrt(key, value);
}


void ViewController::onElementChange(CFigureElement *element)
{
  TGCVariant gcValue = _gcFigure->elementFromId(L"/view-body")->propertyGet("expanded", 0);

  if ((getIntValue("expanded")!=0) != (bool)gcValue)
  {
    myx_grt_bridge_value_change_int(getValue("expanded"), (int)gcValue);
  }
}


//----------------------------------------------------------------------------

RelationshipController::RelationshipController(WorkbenchController *wb, MYX_GRT_VALUE *value)
: BaseController(wb, value), RemakableObject(wb), _gcConnection(0), _gcConnectionInstance(0)
{
  _type= "relationship";
  _view= 0;
}


void RelationshipController::applyProperties()
{
  const char *caption;
  
  _wb->canvas()->beginUpdate();
  
  caption= getStringValue("caption");
  CFigureElement* captionElement = _gcCenter->elementFromKey("/caption");
  if (captionElement != NULL)
    captionElement->setCaption(caption);
  
  caption= getStringValue("startCaption");
  captionElement = _gcLabel1->elementFromKey("/caption");
  if (captionElement != NULL)
    captionElement->setCaption(caption);

  caption= getStringValue("endCaption");
  captionElement = _gcLabel2->elementFromKey("/caption");
  if (captionElement != NULL)
    captionElement->setCaption(caption);
  
  _gcConnectionInstance->moveLabel(GC_CONNECTION_PART_1, 
                                   (float)getDoubleValue("startCaptionXOffs"),
                                   (float)getDoubleValue("startCaptionYOffs"));

  _gcConnectionInstance->moveLabel(GC_CONNECTION_PART_2, 
                                   (float)getDoubleValue("endCaptionXOffs"),
                                   (float)getDoubleValue("endCaptionYOffs"));

  _gcConnectionInstance->moveLabel(GC_CONNECTION_PART_CENTER, 
                                   (float)getDoubleValue("captionXOffs"),
                                   (float)getDoubleValue("captionYOffs"));
  
  _wb->canvas()->refresh();
  _wb->canvas()->endUpdate();
}


void RelationshipController::updateProperties()
{
  double newx, newy;

  newx= (float)_gcConnectionInstance->propertyGet("label1OffsetX", 0);
  newy= (float)_gcConnectionInstance->propertyGet("label1OffsetY", 0);
  if (newx != getDoubleValue("startCaptionXOffs"))
    myx_grt_bridge_value_change_real(getValue("startCaptionXOffs"), newx);
  if (newy != getDoubleValue("startCaptionYOffs"))
    myx_grt_bridge_value_change_real(getValue("startCaptionYOffs"), newy);
  
  newx= (float)_gcConnectionInstance->propertyGet("label2OffsetX", 0);
  newy= (float)_gcConnectionInstance->propertyGet("label2OffsetY", 0);
  if (newx != getDoubleValue("endCaptionXOffs"))
    myx_grt_bridge_value_change_real(getValue("endCaptionXOffs"), newx);
  if (newy != getDoubleValue("endCaptionYOffs"))
    myx_grt_bridge_value_change_real(getValue("endCaptionYOffs"), newy);
  
  newx= (float)_gcConnectionInstance->propertyGet("labelCenterOffsetX", 0);
  newy= (float)_gcConnectionInstance->propertyGet("labelCenterOffsetY", 0);
  if (newx != getDoubleValue("captionXOffs"))
    myx_grt_bridge_value_change_real(getValue("captionXOffs"), newx);
  if (newy != getDoubleValue("captionYOffs"))
    myx_grt_bridge_value_change_real(getValue("captionYOffs"), newy);
}


void RelationshipController::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  switch (reason)
  {
    case GC_CHANGE_CONNECTION_DECORATION:
      break;
    case GC_CHANGE_CONNECTION_INSTANCE:
      updateProperties();
      break;
    default:
      break;
  }
}


void RelationshipController::onAction(CGCBase* sender, CGCBase* origin, TAction** action)
{
  MYX_GRT_ERROR error;
  MYX_GRT_VALUE *args;
  MYX_GRT_VALUE *result;
  MYX_GRT_VALUE *arg;

  if (origin != _gcConnection || (*action)->type != 1003)
  {
    return;
  }

  args= myx_grt_list_new(MYX_ANY_VALUE, NULL);
  myx_grt_list_item_add(args, _value);
  arg= myx_grt_value_from_int((*action)->type);
  myx_grt_list_item_add(args, arg);
  myx_grt_value_release(arg);

  result= myx_grt_function_get_and_call(_wb->grt(),
                                        "WorkbenchUi","popupConnectionMenu",
                                        0, args, &error);
  if (result && myx_grt_dict_item_get_value(result, "error"))
  {
    g_message("error: %s", myx_grt_dict_item_get_as_string(result, "error"));
  }

  if (result) myx_grt_value_release(result);
  if (args) myx_grt_value_release(args);
}


void RelationshipController::onDestroy(CGCBase *sender)
{
  _gcConnection= 0;
  _gcConnectionInstance= 0;
}


void RelationshipController::redecorate()
{
  if (_gcConnection)
  {
    _view->decorateConnection(_gcConnection, 
                              (getIntValue("startMandatory") != 0), (getIntValue("startMany") != 0),
                              (getIntValue("endMandatory") != 0), (getIntValue("endMany") != 0),
                              _gcLabel1, _gcLabel2, _gcCenter);
  }
}


bool RelationshipController::valueFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  if (strcmp2(key, "startTable")==0)
  {
  }
  else if (strcmp2(key, "endTable")==0)
  {
  }
  else if (strcmp2(key, "startMandatory")==0)
  {
    redecorate();
  }
  else if (strcmp2(key, "endMandatory")==0)
  {
    redecorate();
  }
  else if (strcmp2(key, "startMany")==0)
  {
    redecorate();
  }
  else if (strcmp2(key, "endMany")==0)
  {
    redecorate();
  }
  else if (strcmp2(key, "startCaption")==0)
  {
    if (_gcConnection)
    {
      const char *caption= getStringValue("startCaption");
      CFigureElement* captionElement = _gcLabel1->elementFromKey("/caption");
      if (captionElement != NULL)
        captionElement->setCaption(caption);
    }
  }
  else if (strcmp2(key, "endCaption")==0)
  {
    if (_gcConnection)
    {
      const char *caption= getStringValue("endCaption");
      CFigureElement* captionElement = _gcLabel2->elementFromKey("/caption");
      if (captionElement != NULL)
        captionElement->setCaption(caption);
    }
  }
  else if (strcmp2(key, "caption")==0)
  {
    if (_gcConnection)
    {
      const char *caption= getStringValue("caption");
      CFigureElement* captionElement = _gcCenter->elementFromKey("/caption");
      if (captionElement != NULL)
        captionElement->setCaption(caption);
    }
  }
  else if (strcmp2(key, "startCaptionXOffs")==0
           || strcmp2(key, "startCaptionYOffs")==0)
  {
    if (_gcConnectionInstance)
      _gcConnectionInstance->moveLabel(GC_CONNECTION_PART_1, 
                                       (float)getDoubleValue("startCaptionXOffs"),
                                       (float)getDoubleValue("startCaptionYOffs"));
  }
  else if (strcmp2(key, "endCaptionXOffs")==0
           || strcmp2(key, "endCaptionYOffs")==0)
  {
    if (_gcConnectionInstance)
      _gcConnectionInstance->moveLabel(GC_CONNECTION_PART_2, 
                                       (float)getDoubleValue("endCaptionXOffs"),
                                       (float)getDoubleValue("endCaptionYOffs"));
  }
  else if (strcmp2(key, "captionXOffs")==0
           || strcmp2(key, "captionYOffs")==0)
  {
    if (_gcConnectionInstance)
    {
      _gcConnectionInstance->moveLabel(GC_CONNECTION_PART_CENTER, 
                                       (float)getDoubleValue("captionXOffs"),
                                       (float)getDoubleValue("captionYOffs"));
    }
  }
  else
    return BaseController::valueFromGrt(key, value);
  
  if (referenceIsSet("startTable") && referenceIsSet("endTable"))
  {
    if (!_gcConnection)
    {
      _wb->setPendingRemake(this);
    }
  }

  return true;
}


void RelationshipController::remake()
{
  TableController *startTable= (TableController*)_wb->objectForId(getStringValue("startTable"));
  TableController *endTable= (TableController*)_wb->objectForId(getStringValue("endTable"));

  if (!startTable || !endTable)
    return;
  
  LayerController *lctl= startTable->layer();
  if (!lctl)
    return;

  _view= lctl->view();

  // destroy the old one
  if (_gcConnection)
  {
    _gcConnectionInstance->removeListener(this);
    _gcLabel1->removeListener(this);
    _gcLabel2->removeListener(this);
    _gcCenter->removeListener(this);
    delete _gcConnectionInstance;
    delete _gcConnection;
  }
  
  if (!startTable->gcFigure() || !endTable->gcFigure())
    return;

  // create new
  
  _gcConnection = _wb->canvas()->createConnection("db.connection.decor", NULL, startTable->gcFigure(), 
                                                  endTable->gcFigure());
  
  _gcLabel1 = _wb->canvas()->createFigure("db.Connection.End1.Text", NULL);
  _gcLabel2 = _wb->canvas()->createFigure("db.Connection.End2.Text", NULL);
  _gcCenter = _wb->canvas()->createFigure("db.Connection.Center.Text", NULL);

  _view->decorateConnection(_gcConnection, 
                            (getIntValue("startMandatory") != 0), (getIntValue("startMany") != 0),
                            (getIntValue("endMandatory") != 0), (getIntValue("endMany") != 0),
                            _gcLabel1, _gcLabel2, _gcCenter);

  _gcConnectionInstance = _view->gcView()->createConnectionInstance(_gcConnection, 
                                                                    startTable->gcInstance(),
                                                                    endTable->gcInstance());
  _gcConnectionInstance->addListener(this);
  _gcConnectionInstance->lineStyleSet(GC_CONNECTION_STYLE_SOLID);
  _gcConnectionInstance->userDataSet(this);
  
  applyProperties();
}


ElementController *RelationshipController::getElement1()
{
  TableController *startTable= (TableController*)_wb->objectForId(getStringValue("startTable"));

  return startTable;
}


ElementController *RelationshipController::getElement2()
{
  TableController *endTable= (TableController*)_wb->objectForId(getStringValue("endTable"));

  return endTable;
}

//----------------------------------------------------------------------------

RoutineController::RoutineController(WorkbenchController *wb, MYX_GRT_VALUE *value)
: ElementController(wb, value)
{
  _type= "routine";
}


void RoutineController::onElementChange(CFigureElement *element)
{
  TGCVariant gcValue = _gcFigure->elementFromId(L"/package-routines")->propertyGet("expanded", 0);
  
  if ((getIntValue("expanded")!=0) != (bool)gcValue)
  {
    myx_grt_bridge_value_change_int(getValue("expanded"), (int)gcValue);
  }
}


void RoutineController::assembleFigure()
{
  // setup the figure
  const char *_id= myx_grt_value_as_string(getValue("routineGroup"));
  MYX_GRT_VALUE *data= myx_grt_reference_cache_lookup(this->_wb->grt(), _id);
  MYX_GRT_VALUE *routine_list= myx_grt_dict_item_get_value(data, "routines");

  if (!data) 
    data= _value;
  
  CFigureElement* captionElement = _gcFigure->elementFromKey("/name");
  if (captionElement != NULL)
    captionElement->setCaption(getStringValue("name"));

  addPartsFromList(_gcFigure->elementFromKey("/routines"), routine_list, "/routines/routine");
}


MYX_GRT_VALUE *RoutineController::objectData()
{
  const char *id= getStringValue("routineGroup");
  return myx_grt_reference_cache_lookup(_wb->grt(), id);
}


void RoutineController::applyProperties()
{
  TGCVariant var;
  CFigureElement *elem;
  
  if ((elem = _gcFigure->elementFromId(L"/package-header/package-header-right/package-header-right-tools/package-header-header-right")))
  {
    if (_gcFigure->elementFromId(L"/package-routines")->expanded() != (getIntValue("expanded")!=0))
    {
      elem->executeAction(GC_ACTION_TOGGLE);
    }
  }
  ElementController::applyProperties();
}


bool RoutineController::valueFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  if (strcmp2(key, "expanded")==0)
  {
    if (_gcFigure)
      applyProperties();
    return true;
  }
  else if (strcmp2(key, "name")==0)
  {
    g_message("name changed to %s", getStringValue("name"));
    if (_gcFigure)
      _gcFigure->elementFromKey("/name")->setCaption(getStringValue("name"));
    return true;
  }
/*  else if (strcmp2(key, "routines")==0)
  {
    const char *_id= myx_grt_value_as_string(getValue("routineGroup"));
    MYX_GRT_VALUE *data= myx_grt_reference_cache_lookup(this->_wb->grt(), _id);
    MYX_GRT_VALUE *routine_list= myx_grt_dict_item_get_value(data, "routines");

    if (!data) 
      data= _value;

    updatePartsFromList(_gcFigure->elementFromKey("/routines"), routine_list, "/routines/routine");
  }
*/
  return ElementController::valueFromGrt(key, value);
}

//----------------------------------------------------------------------------

NoteController::NoteController(WorkbenchController *wb, MYX_GRT_VALUE *value)
: ElementController(wb, value)
{
  _type= "note";
}


bool NoteController::valueFromGrt(const char *key, MYX_GRT_VALUE *value)
{
  if (strcmp2(key, "text")==0)
  {
    if (_gcFigureInstance)
    {
      _gcFigure->elementFromKey("/text")->setCaption(getStringValue("text"));
    }
  }
  else if (strcmp2(key, "color")==0)
  {
    if (_gcFigureInstance)
    {
      _wb->canvas()->beginUpdate();
      if (_gcFigureInstance)
      {
        const char *color= getStringValue("color");
        if (color && color[0])
        {
          _layer->setElementStyleColor(_gcFigure->content(), color);
        }      
      }
      _wb->canvas()->endUpdate();
    }
  }
  else
  {
    bool ret= ElementController::valueFromGrt(key, value);

    if (!_gcFigureInstance && realizeConditionsMet())
      setPendingRemake();

    return ret;
  }
  return true;
}


void NoteController::applyProperties()
{
  _wb->canvas()->beginUpdate();
  if (_gcFigureInstance)
  {
    // apply style
    const char *color= getStringValue("color");
    if (color && color[0])
    {
      _layer->setElementStyleColor(_gcFigure->content(), color);
    }
  }
  _wb->canvas()->endUpdate();
  
  ElementController::applyProperties();
}

//----------------------------------------------------------------------------

ImageController::ImageController(WorkbenchController *wb, MYX_GRT_VALUE *value)
: ElementController(wb, value)
{
  _type= "image";
}


void ImageController::applyProperties()
{
  _wb->canvas()->beginUpdate();
  if (_gcFigureInstance)
  {
//    const char *file= getStringValue("filename");
    
    
  }
  _wb->canvas()->endUpdate();
}



// ======================================================================


static struct {
  const char *name;
  BaseController *(*createController)(WorkbenchController *ctl, MYX_GRT_VALUE *value);
} WB_STRUCT_CONTROLLERS[]= {
  { "db.workbench.Model",         NULL },
  { "db.workbench.View",          &CanvasViewController::create },
  { "db.workbench.Layer",         &LayerController::create },
  { "db.workbench.Group",         NULL },
  { "db.workbench.Element",       &ElementController::create },
  { "db.workbench.TableElement",  &TableController::create },
  { "db.workbench.ViewElement",   &ViewController::create },
  { "db.workbench.RoutinesElement",&RoutineController::create },
  { "db.workbench.NoteElement",   &NoteController::create },
  { "db.workbench.ImageElement",  &ImageController::create },
  { "db.workbench.Relationship",  &RelationshipController::create },
  { NULL, NULL }
};


WorkbenchController::WorkbenchController(MYX_GRT *grt)
: _grt(grt)
{  
//  myx_grt_value_bridge_dict_key_set(form_list, "forms");
}


WorkbenchController::~WorkbenchController()
{
  _canvas->removeListener(this);
}


void WorkbenchController::setCallbacks(void *data,
                  void (*view_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *view, CGCView *gcView, MYX_WB_GC_CHANGE change, void *data),
                  void (*layer_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *layer, CFigureInstance *gcLayer, MYX_WB_GC_CHANGE change, void *data),
                  void (*element_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *element, CFigureInstance *gcElem, MYX_WB_GC_CHANGE change, void *data))
{
  _callbackData= data;
  _viewChanged= view_changed;
  _layerChanged= layer_changed;
  _elementChanged= element_changed;
}



BaseController* WorkbenchController::objectForId(string oid)
{
  if (_objectTable.find(oid) != _objectTable.end())
    return _objectTable[oid];
  else
    return NULL;
}


void WorkbenchController::rememberObject(BaseController *object)
{
  _objectTable[myx_grt_dict_id_item_as_string(object->value())]= object;
}


void WorkbenchController::getObjectAtPoint(float x, float y, MYX_GRT_VALUE *&figure, MYX_GRT_VALUE *&object, MYX_GRT_VALUE *&detail)
{
  CFigureInstance *figInstance;
  TVertex point;
  CHitResults *hits;

  figure= 0;
  object= 0;
  detail= 0;
  
  point.x= x;
  point.y= y;
  point.z= 0;
  point.w= 0;
  
  hits= _canvas->currentViewGet()->getHitTestInfoAt(point, true);
  if (hits)
  {
    hits->reset();
    figInstance= (CFigureInstance*)hits->next();
    if (figInstance)
    {
      BaseController *controller= (BaseController*)figInstance->userDataGet();

      if (controller)
      {
        if (strcmp(controller->type(), "relationship") == 0)
        {
          RelationshipController *element= (RelationshipController*)controller;

          figure= element->value();
        }
        else if (strcmp(controller->type(), "layer") != 0)
        {
          ElementController *element= (ElementController*)controller;
          CFigureElement *figElement;
          
          figure= element->value();
          object= element->objectData();
          
          figElement= element->gcInstance()->elementFromPoint(point);
          if (figElement)
          {
            detail= (MYX_GRT_VALUE*)figElement->userDataGet();
          }
        }
        else
        {
          LayerController *layer= (LayerController*)controller;
          
          figure= layer->value();
        }
      }
    }
  }
}


bool WorkbenchController::initObject(MYX_GRT_VALUE *object)
{
  MYX_GRT_STRUCT *gstruct= myx_grt_dict_struct_get(_grt, object);
  unsigned int i;
  BaseController *bctl= 0;

  if (strcmp2(gstruct->name, "db.workbench.Model")==0)
  {
    _modelCtl= new ModelController(this, _canvas, object);
    myx_grt_value_bridge_data_object_set(object, _modelCtl);

    return true;
  }
//  g_message("init object of type %s", gstruct->name);

  bctl= (BaseController*)myx_grt_value_bridge_data_object_get(object);
  if (!bctl)
  {
    for (i= 0; WB_STRUCT_CONTROLLERS[i].name; i++)
    {
      if (strcmp2(gstruct->name, WB_STRUCT_CONTROLLERS[i].name)==0)
      {
        if (WB_STRUCT_CONTROLLERS[i].createController)
          bctl= (*WB_STRUCT_CONTROLLERS[i].createController)(this, object);
        break;
      }
    }
    if (bctl)
    {      
      bctl->setModel(_modelCtl);
      myx_grt_value_bridge_data_object_set(object, bctl);
      
      bctl->postCreate();
      return true;
    }
  }
  else
    g_warning("OBJECT ALREADY HAS DATA SET");

  return false;
}


CanvasViewController *WorkbenchController::initObjectForView(CGCView *gcView)
{
  CanvasViewController *view;
  
  view= new CanvasViewController(this, gcView);
  
  view->setModel(_modelCtl);
      
  return view;
}


void WorkbenchController::setCanvas(CGenericCanvas *canvas)
{
  _canvas= canvas;
  _canvas->addListener(this);
}


void WorkbenchController::notifyView(CanvasViewController *view, MYX_WB_GC_CHANGE change)
{
  if (_viewChanged)
  {
    if (view)
      (*_viewChanged)(_grt, view->value(), view->gcView(), change, _callbackData);
    else
      (*_viewChanged)(_grt, NULL, NULL, change, _callbackData);
  }
}


void WorkbenchController::notifyLayer(LayerController *layer, MYX_WB_GC_CHANGE change)
{
  if (_layerChanged)
  {    
    (*_layerChanged)(_grt, layer->value(), layer->layerFrame(), change, _callbackData);
  }
}


void WorkbenchController::notifyElement(ElementController *element, MYX_WB_GC_CHANGE change)
{
  if (_elementChanged)
  {
    (*_elementChanged)(_grt, element->value(), element->gcInstance(), change, _callbackData);
  }
}

void WorkbenchController::setPendingRemake(RemakableObject *element)
{
  _pendingRemakes.push_back(element);

  //added to make sure app does a refresh when it has time
  notifyView(NULL, MYX_WBGC_REFRESH);
}


void WorkbenchController::processPendingRemakes()
{
  _canvas->beginUpdate();
  try
  {
    for (list<RemakableObject*>::const_iterator iter= _pendingRemakes.begin();
        iter != _pendingRemakes.end(); ++iter)
    {
      (*iter)->remake();
      (*iter)->remakeDone();
    }
    _pendingRemakes.clear();

    if (model()->currentView())
      model()->currentView()->doPendingViewTasks();
    _canvas->endUpdate();
  }  
  catch (...)
  {
    _canvas->endUpdate();
    throw;
  };
}


void WorkbenchController::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  CanvasViewController *view;
  //LayerController *layer;
  
  switch (reason)
  {
    case GC_CHANGE_CANVAS_ADD_VIEW:
    {
      // When Canvas reports a new view, it can mean:
      // - it's a view that we added ourselves, so we just need to add a reference
      // in the model list (in that case, _creatingObject will be true)
      // - it's a view added externally, so we have to create a GRT object for it
      // and add it to the model list
      view= _modelCtl->controllerForCanvasView(static_cast<CGCView*>(origin));
      if (!view)
      {
        view= initObjectForView(static_cast<CGCView*>(origin));
        _modelCtl->rememberViewController(view);
        _modelCtl->addViewRef(view);
      }
      else
      {
        _modelCtl->addViewRef(view);
        _modelCtl->changedCurrentView(view);
      }
      break;
    }
    case GC_CHANGE_CANVAS_REMOVE_VIEW:
      break;
      
    case GC_CHANGE_CANVAS_SWITCH_VIEW:
      view= _modelCtl->controllerForCanvasView(static_cast<CGCView*>(origin));
      if (view)
      {
        _modelCtl->changedCurrentView(view);
      }
      break;

    case GC_CHANGE_SELECTION_ADD:
    case GC_CHANGE_SELECTION_REMOVE:
    case GC_CHANGE_SELECTION_CLEAR:
    case GC_CHANGE_SELECTION_CHANGE:
      if (_modelCtl->currentView())
        _modelCtl->currentView()->updateSelectionList();
      break;

    default:
      break;
  }
}




// -----------------------------------------------------------------------------
extern "C" {
  
MYX_GRT_VALUE * wb_bringToFront(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *controller= (WorkbenchController*)data;
  MYX_GRT_VALUE *figure, *relative;
  ElementController *figureE, *relativeE;
  
  if (myx_grt_list_item_count(param) == 1)
  {
    figure= myx_grt_list_item_get(param, 0);
    relative= NULL;
  }
  else if (myx_grt_list_item_count(param) == 2)
  {
    figure= myx_grt_list_item_get(param, 0);
    relative= myx_grt_list_item_get(param, 1);
  }
  else
    return make_return_value_error("invalid # of arguments to bringToFront({element, relativeTo})", "");

  figureE= elementFromValueObject(controller->grt(), figure);
  if (relative)
    relativeE= elementFromValueObject(controller->grt(), relative);
  else
    relativeE= 0;
  
  if (!figureE)
    return make_return_value_error("invalid argument to bringToFront(): invalid element object", "");

  if (relativeE && relativeE->layer() != figureE->layer())
    return make_return_value_error("invalid argument to bringToFront(): elements are in different layers", "");

  if (figureE->layer())
    figureE->layer()->raiseElement(figureE, relativeE);
  else
    return make_return_value_error("invalid argument to bringToFront(): element is not in a layer", "");
  
  return make_return_value(myx_grt_value_from_int(1));
}


MYX_GRT_VALUE * wb_sendToBack(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *controller= (WorkbenchController*)data;
  MYX_GRT_VALUE *figure, *relative;
  ElementController *figureE, *relativeE;
  
  if (myx_grt_list_item_count(param) == 1)
  {
    figure= myx_grt_list_item_get(param, 0);
    relative= NULL;
  }
  else if (myx_grt_list_item_count(param) == 2)
  {
    figure= myx_grt_list_item_get(param, 0);
    relative= myx_grt_list_item_get(param, 1);
  }
  else
    return make_return_value_error("invalid # of arguments to sendToBack({element, relativeTo})", "");
  
  figureE= elementFromValueObject(controller->grt(), figure);
  if (relative)
    relativeE= elementFromValueObject(controller->grt(), relative);
  else
    relativeE= 0;
  
  if (!figureE)
    return make_return_value_error("invalid argument to sendToBack(): invalid element object", "");
  
  if (relativeE && relativeE->layer() != figureE->layer())
    return make_return_value_error("invalid argument to sendToBack(): elements are in different layers", "");
  
  if (figureE->layer())
    figureE->layer()->lowerElement(figureE, relativeE);
  else
    return make_return_value_error("invalid argument to sendToBack(): element is not in a layer", "");
  return make_return_value(myx_grt_value_from_int(1));
}


MYX_GRT_VALUE * wb_objectAtPoint(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *controller= (WorkbenchController*)data;
  MYX_GRT_VALUE *figure= NULL, *object, *detail= NULL;  
  float x, y;
  
  if (myx_grt_list_item_count(param) == 2)
  {
    x= (float) myx_grt_value_as_real(myx_grt_list_item_get(param, 0));
    y= (float) myx_grt_value_as_real(myx_grt_list_item_get(param, 1));
  }
  else
    return make_return_value_error("invalid # of arguments to objectAtPoint({x, y})", "");
  
  controller->getObjectAtPoint(x, y, figure, object, detail);
  
  if (figure)
  {
    MYX_GRT_VALUE *result;

    result= myx_grt_dict_new(NULL, NULL);
    myx_grt_dict_item_set_value(result, "element", figure);
    myx_grt_dict_item_set_value(result, "object", object);
    myx_grt_dict_item_set_value(result, "detail", detail);

    return make_return_value(result);
  }
  return make_return_value(NULL);
}


MYX_GRT_VALUE *wb_deleteElement(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *wb= (WorkbenchController*)data;
  MYX_GRT_VALUE *elemValue= myx_grt_bridge_list_item_get(param, 0, 0);
  
  if (myx_grt_dict_struct_is_or_inherits_from(wb->grt(), elemValue,"model.Layer"))
  {
    LayerController *layer;
    layer= layerFromValueObject(elemValue);
    if (layer)
      layer->view()->deleteLayer(layer);
  }
  else
  {
    ElementController *elem;
    elem= elementFromValueObject(wb->grt(), elemValue);
    if (elem)
      elem->layer()->view()->deleteElement(elem);
  }
  return make_return_value(myx_grt_value_from_int(1));
}


MYX_GRT_VALUE * wb_selectObject(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *wb= (WorkbenchController*)data;
  if (myx_grt_bridge_list_item_count(param) == 0)
  {
    
  }
  else
  {
    MYX_GRT_VALUE *elemValue= myx_grt_bridge_list_item_get(param, 0, 0);
    
    if (myx_grt_value_get_type(elemValue) == MYX_DICT_VALUE
        && strcmp2(myx_grt_dict_struct_get_name(elemValue), "db.workbench.Layer")==0)
    {
      LayerController *layer;
      
      layer= layerFromValueObject(elemValue);
      if (layer)
        layer->view()->gcView()->addToSelection(layer->layerFrame());
    }
    else
    {
      ElementController *elem;
      
      elem= elementFromValueObject(wb->grt(), elemValue);
      if (elem)
        elem->layer()->view()->gcView()->addToSelection(elem->gcInstance());
    }
  }
  return make_return_value(myx_grt_value_from_int(1));
}


MYX_GRT_VALUE * wb_unselectObject(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *wb= (WorkbenchController*)data;
  if (myx_grt_bridge_list_item_count(param) == 0)
  {
    wb->canvas()->currentViewGet()->clearSelection();
  }
  else
  {
    MYX_GRT_VALUE *elemValue= myx_grt_bridge_list_item_get(param, 0, 0);
    ElementController *elem;
    
    elem= elementFromValueObject(wb->grt(), elemValue);
    if (elem)
      elem->layer()->view()->gcView()->removeFromSelection(elem->gcInstance());
  }
  return make_return_value(myx_grt_value_from_int(1));
}


MYX_GRT_VALUE * wb_rubberbandStart(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *wb= (WorkbenchController*)data;
  float x, y;

  if (myx_grt_list_item_count(param) == 2)
  {
    x= (float) myx_grt_value_as_real(myx_grt_list_item_get(param, 0));
    y= (float) myx_grt_value_as_real(myx_grt_list_item_get(param, 1));
  }
  else
    return make_return_value_error("invalid # of arguments to objectAtPoint({x, y})", "");
  
  wb->model()->currentView()->setupForRubberbandStart(x, y);
  
  return NULL;
}

MYX_GRT_VALUE * wb_rubberbandStop(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *wb= (WorkbenchController*)data;
  
  wb->model()->currentView()->setupForRubberbandStop();
  
  return NULL;
}

MYX_GRT_VALUE * wb_taskCompleted(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *wb= (WorkbenchController*)data;

  wb->model()->currentView()->notifyTaskComplete();
  
  return NULL;
}

MYX_GRT_VALUE * wb_taskCanceled(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *wb= (WorkbenchController*)data;

  wb->model()->currentView()->notifyTaskCanceled();
  
  return NULL;
}


MYX_GRT_VALUE * wb_detailFrame(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *wb= (WorkbenchController*)data;
  MYX_GRT_VALUE *elemValue= myx_grt_bridge_list_item_get(param, 0, 0);
  ElementController *elem;
  TBoundingBox box;
  MYX_GRT_VALUE *result;
  MYX_GRT_VALUE *number;
  
  elem= elementFromValueObject(wb->grt(), elemValue);
  
  box= elem->detailFrame(myx_grt_bridge_list_item_get(param, 1, 0));
  
  result= myx_grt_list_new(MYX_REAL_VALUE, NULL);
  
  number= myx_grt_value_from_real((double)box.upper.x);
  myx_grt_list_item_add(result, number);
  myx_grt_value_release(number);
  
  number= myx_grt_value_from_real((double)box.upper.y);
  myx_grt_list_item_add(result, number);
  myx_grt_value_release(number);
  
  number= myx_grt_value_from_real((double)(box.lower.x - box.upper.x));
  myx_grt_list_item_add(result, number);
  myx_grt_value_release(number);
  
  number= myx_grt_value_from_real((double)(box.lower.y - box.upper.y));
  myx_grt_list_item_add(result, number);
  myx_grt_value_release(number);
  
  return make_return_value(result);
}
  
  
MYX_GRT_VALUE * wb_arrangeObjects(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *wb= (WorkbenchController*)data;
  
  if (myx_grt_bridge_list_item_count(param) == 2)
  {
    MYX_GRT_VALUE *value= myx_grt_bridge_list_item_get(param, 0, 0);
    int selectedOnly= myx_grt_value_as_int(myx_grt_bridge_list_item_get(param, 1, 0));
    CanvasViewController *view= 0;
    
    if (value)
      view= viewFromValueObject(value);
    if (!view)
      view= wb->model()->currentView();

    view->autoArrangeElements(selectedOnly!=0);
  }
  else
    return make_return_value_error("invalid # of arguments to arrangeObjects({view, selectedOnly})", "");
  return NULL;
}


MYX_GRT_VALUE *wb_reconstructViewElements(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *wb= (WorkbenchController*)data;
  CanvasViewController *view;
  
  if (myx_grt_bridge_list_item_count(param) == 1)
  {
    view= viewFromValueObject(myx_grt_bridge_list_item_get(param, 0, 0));
  }
  else if (myx_grt_bridge_list_item_count(param) == 0)
  {
    view= wb->model()->currentView();
  }
  else
    return  make_return_value_error("invalid # of arguments to reconstructViewElements({[view]})", "");
  
  if (!view)
    return make_return_value_error("invalid view argument to reconstructViewElements()", "");
  
  MYX_GRT_VALUE *elemList= view->getValue("elements");
  unsigned int i, c= myx_grt_bridge_list_item_count(elemList);
  
  for (i= 0; i < c; i++)
  {
    ElementController *elem= elementFromValueObject(wb->grt(), 
                                                    myx_grt_bridge_list_item_get(elemList, i, 0));

    if (elem)
      elem->setPendingRemake();
  }

  return NULL;
}
 
};
