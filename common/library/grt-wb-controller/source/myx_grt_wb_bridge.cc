/*
 *  myx_grt_wb_bridge.cc
 *  WBCanvas
 *
 *  Created by Alfredo Kojima on 05/7/21.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#define DECLARE_WB_BRIDGE_MODULE

#include "myx_gc_feedback.h"
#include "myx_grt_wb_bridge.h"
#include "myx_grt_wb_controller.h"
#include "myx_grt_wb_public_interface.h"

extern "C" {
  
MYX_GRT_ERROR myx_register_grt_wb_module(MYX_GRT *grt)
{
  WorkbenchController *ctl= new WorkbenchController(grt);
  
  myx_grt_module_register_builtin(grt, &grt_module_workbench, ctl);
  
  return MYX_GRT_NO_ERROR;
}

void myx_unregister_grt_wb_module(MYX_GRT *Runtime)
{
  // unregister module
}


void myx_grt_wb_bridge_initialize(MYX_GRT *grt, CGenericCanvas *canvas, const char *path)
{
  MYX_GRT_MODULE *module;
  WorkbenchController *ctl;
  MYX_GRT_VALUE *model;
  
  module= myx_grt_module_get(grt, grt_module_workbench.name);
  
  module->dont_bridge_loaded_values= 1;
  
  ctl= (WorkbenchController*)myx_grt_module_get_private_data(module);
  ctl->setCanvas(canvas);
  
  ctl->initializing= 1;
  
  model= myx_grt_dict_new_obj(grt, "db.workbench.Model", g_get_prgname(), NULL, NULL);
  myx_grt_value_bridge_module_set(model, module);
  
  ctl->initializing= 0;
  
  
  myx_grt_value_release(model);
  
  myx_grt_dict_item_set_by_path(myx_grt_get_root(grt), path, model);
}

void myx_grt_wb_bridge_free(MYX_GRT *Runtime, CGenericCanvas *Canvas, const char *Path)
{
  // free the bridge
}


void myx_grt_wb_bridge_set_callbacks(MYX_GRT *Runtime, void *data,
                                     void (*view_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *view, CGCView *gcView, MYX_WB_GC_CHANGE change, void *data),
                                     void (*layer_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *layer, CFigureInstance *gcLayer, MYX_WB_GC_CHANGE change, void *data),
                                     void (*element_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *figure, CFigureInstance *gcEleme, MYX_WB_GC_CHANGE change, void *data))
{
  MYX_GRT_MODULE *module;
  WorkbenchController *ctl;

  module= myx_grt_module_get(Runtime, grt_module_workbench.name);
  ctl= (WorkbenchController*)myx_grt_module_get_private_data(module);

  ctl->setCallbacks(data, view_changed, layer_changed, element_changed);
}


void myx_grt_wb_bridge_process_pending(MYX_GRT *Runtime)
{
  MYX_GRT_MODULE *module;
  WorkbenchController *ctl;
  
  module= myx_grt_module_get(Runtime, grt_module_workbench.name);
  ctl= (WorkbenchController*)myx_grt_module_get_private_data(module);

  ctl->processPendingRemakes();
}


void myx_grt_wb_bridge_process_relocations(MYX_GRT *Runtime)
{
  MYX_GRT_MODULE *module;
  WorkbenchController *ctl;
  
  module= myx_grt_module_get(Runtime, grt_module_workbench.name);
  ctl= (WorkbenchController*)myx_grt_module_get_private_data(module);
  
  CanvasViewController* viewController = ctl->model()->currentView();
  if (viewController != NULL)
    viewController->relocatePendingObjects();
}

  

MYX_GRT_VALUE ** myx_grt_wb_get_selected_objects(MYX_GRT *Runtime, int *count)
{
  MYX_GRT_MODULE *module= myx_grt_module_get(Runtime, grt_module_workbench.name);
  WorkbenchController *wb= (WorkbenchController*)myx_grt_module_get_private_data(module);
  MYX_GRT_VALUE** objects= NULL;
  CSelectionEnumerator *sel;

  sel= wb->canvas()->currentViewGet()->getSelectionEnumerator();
  if (!sel)
    return NULL;

  if (sel)
  {
    int i= 0;
    
    *count= wb->canvas()->currentViewGet()->selectionCount();

    sel->reset();

    objects= g_new(MYX_GRT_VALUE*, *count);

    while (sel->hasNext())
    {
      BaseController *ctl= (BaseController*)(sel->next())->userDataGet();
      objects[i++]= ctl->value();
    }

    sel->release();
  }
  
  return objects;
}


} // extern "C"


MYX_GRT_VALUE *workbench_init_dict(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *ctl= (WorkbenchController*)data;
  
  ctl->initObject(param);
  
  return NULL;
}


MYX_GRT_VALUE *workbench_update_from_grt(MYX_GRT_VALUE *param, void *data)
{
  //WorkbenchController *ctl= (WorkbenchController*)data;
  BaseController *object= (BaseController*)myx_grt_value_bridge_data_object_get(param);
  const char *key= myx_grt_value_bridge_dict_key_get(param);
    
  if (object)
    object->updateFromGrt(key, param);
  else
  {
    object= (BaseController*)myx_grt_value_bridge_data_owner_get(param);
    object->updateFromGrt(key, param);
  }
  return NULL;
}


MYX_GRT_VALUE *workbench_update_to_grt(MYX_GRT_VALUE *param, void *data)
{
//  WorkbenchController *ctl= (WorkbenchController*)data;
  BaseController *object= (BaseController*)myx_grt_value_bridge_data_owner_get(param);
  const char *key= myx_grt_value_bridge_dict_key_get(param);
  
  //  g_message("update %p %s TO grt", object, key);
  
  if (object)
    object->updateToGrt(key, param);

  return NULL;
}


MYX_GRT_VALUE *workbench_value_from_grt(MYX_GRT_VALUE *param, void *data)
{
  WorkbenchController *ctl= (WorkbenchController*)data;
  BaseController *object;
    
  if (ctl->initializing)
    return NULL;
  object= (BaseController*)myx_grt_value_bridge_data_owner_get(param);
  if (!object)
  {
    g_warning("trying to manipulate invalid workbench object [key=%s]", 
              myx_grt_value_bridge_dict_key_get(param));
    return NULL;
  }
  
  object->valueFromGrt(myx_grt_value_bridge_dict_key_get(param), 
                       param);

  return NULL;
}


MYX_GRT_VALUE *workbench_value_to_grt(MYX_GRT_VALUE *param, void *data)
{
  // unused
  return NULL;
}


MYX_GRT_VALUE *workbench_del_value(MYX_GRT_VALUE *param, void *data)
{
//  WorkbenchController *ctl= (WorkbenchController*)data;
  BaseController *object;
    
  object= (BaseController*)myx_grt_value_bridge_data_object_get(param);
  if (object)
  {
    //g_message("DELETE VALUE %s  ", object->type());
      
    delete object;
  }
/*  else
    g_message("destroy bridged unknown value of type %s", 
              myx_get_value_type_as_string(myx_grt_value_get_type(param)));
  */
  return NULL;
}


//--------------------------------------------------------------------------

MYX_GRT_VALUE *myx_grt_wb_object_for_view(CGCView *view)
{
  CanvasViewController *vc= (CanvasViewController*)view->userDataGet();
  if (vc)
    return vc->value();
  else
    return 0;
}


MYX_GRT_VALUE * myx_grt_wb_object_for_element(CGCBase *elem)
{
  ElementController *controller= (ElementController*)elem->userDataGet();
  if (controller)
    return (MYX_GRT_VALUE*)controller->objectData();
  else
    return 0;
}


int myx_grt_wb_load_style_file(CGenericCanvas *canvas, const char *filename, MYX_GRT_VALUE *variables)
{
  map<string, string> vars;
  if (variables)
  {
    int error= 0;
    int j, k= myx_grt_bridge_list_item_count(variables);
    for (j= 0; j < k; j++)
    {
      MYX_GRT_VALUE *item= myx_grt_bridge_list_item_get(variables, j, 0);
      int i, c= myx_grt_bridge_dict_item_count(item, 0);
      for (i= 0; i < c; i++)
      {
        const char *key= myx_grt_bridge_dict_item_key_by_index(item, i, 0);
        MYX_GRT_VALUE *value= myx_grt_bridge_dict_item_value_by_index(item, i, 0);
        
        vars[key]= myx_grt_bridge_value_as_string(value);
      }
      if (canvas->addStylesFromFile(filename, vars) != GC_NO_ERROR)
        error++;
    }
    return error;
  }
  else
    return canvas->addStylesFromFile(filename, vars) ? 1 : 0;
}
