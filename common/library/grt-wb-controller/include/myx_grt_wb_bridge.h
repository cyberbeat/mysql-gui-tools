/*
 *  myx_grt_wb_bridge.h
 *  WBCanvas
 *
 *  Created by Alfredo Kojima on 05/7/21.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#ifndef __MYX_GRT_WB_BRIDGE_H__
#define __MYX_GRT_WB_BRIDGE_H__


#include <myx_public_interface.h>
#ifdef __APPLE__
#include <MySQLGRT/myx_grt_public_interface.h>
#else
#include <myx_grt_public_interface.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif
  
  //MYX_GRT_VALUE *workbench_create(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE *workbench_init_dict(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE *workbench_update_to_grt(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE *workbench_update_from_grt(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE *workbench_value_from_grt(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE *workbench_value_to_grt(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE *workbench_del_value(MYX_GRT_VALUE *param, void *data);
  
  MYX_GRT_VALUE * wb_bringToFront(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE * wb_sendToBack(MYX_GRT_VALUE *param, void *data);
  
  MYX_GRT_VALUE * wb_objectAtPoint(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE * wb_deleteElement(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE * wb_selectObject(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE * wb_unselectObject(MYX_GRT_VALUE *param, void *data);  
  MYX_GRT_VALUE * wb_rubberbandStart(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE * wb_rubberbandStop(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE * wb_taskCompleted(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE * wb_taskCanceled(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE * wb_detailFrame(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE * wb_arrangeObjects(MYX_GRT_VALUE *param, void *data);
  MYX_GRT_VALUE * wb_reconstructViewElements(MYX_GRT_VALUE *param, void *data);

#ifdef DECLARE_WB_BRIDGE_MODULE
  static MYX_GRT_BUILTIN_FUNCTION functions_workbench[]= {
    //  {"create::", workbench_create },
  {"_initDict::", workbench_init_dict },
  {"_updateToGrt::", workbench_update_to_grt },
  {"_updateFromGrt::", workbench_update_from_grt },
  {"_valueFromGrt::", workbench_value_from_grt },
  {"_valueToGrt::", workbench_value_to_grt },
  {"_delValue::", workbench_del_value },
    
  {"bringToFront::", wb_bringToFront },
  {"sendToBack::", wb_sendToBack },
  {"objectAtPoint::", wb_objectAtPoint },
  {"deleteElement::", wb_deleteElement },
  {"selectObject::", wb_selectObject },
  {"unselectObject::", wb_unselectObject },
  {"rubberbandStart::", wb_rubberbandStart },
  {"rubberbandStop::", wb_rubberbandStop },
  {"taskComplete::", wb_taskCompleted },
  {"taskCanceled::", wb_taskCanceled },
  {"detailFrame::", wb_detailFrame },
  {"arrangeObjects::", wb_arrangeObjects },
  {"reconstructViewElements::", wb_reconstructViewElements },
  };
  
  static MYX_GRT_BUILTIN_MODULE grt_module_workbench= {
    "WorkbenchController",
    NULL,
    sizeof(functions_workbench)/sizeof(MYX_GRT_BUILTIN_FUNCTION),
    functions_workbench
  };
#endif // DECLARE_WB_BRIDGE_MODULE

#ifdef __cplusplus
};
#endif


#endif
