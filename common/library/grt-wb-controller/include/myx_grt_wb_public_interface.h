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

#ifndef myx_grt_wb_public_interface_h
#define myx_grt_wb_public_interface_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define __LCC__
#endif

#ifdef __APPLE__
#include <WBCanvas/myx_gc.h>
#include <WBCanvas/myx_gc_canvas.h>
#include <MySQLGRT/myx_grt_public_interface.h>
#else
#include <myx_gc.h>
#include <myx_gc_canvas.h>
#include "myx_grt_public_interface.h"
#endif

#include <stdio.h>

#ifdef __cplusplus
extern "C" {   
#endif /* __cplusplus */

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define MYX_PUBLIC_FUNC __declspec(dllexport)
#else
#define MYX_PUBLIC_FUNC
#endif

/*
 * PUBLIC INTERFACE definition for MYSQLLibInterfaceMapper
 */

/// [SCRIPT::LibInterfaceMapper] -public_interface "libmysqlgrtwbmodule"
#define libmysqlgrtwbmodule_PUBLIC_INTERFACE_VERSION 10000

/// [SCRIPT::LibInterfaceMapper] -add_to_uses "myx_grt_public_interface"
/// [SCRIPT::LibInterfaceMapper] -add_datatypes_from "..\..\base-library\include\myx_public_interface.h"

/*
 * Defines
 */

/*
 * Enums
 */

/*
 * Structs and Enums
 */

typedef enum {
  MYX_WBGC_ADDED,
  MYX_WBGC_REMOVED,
  MYX_WBGC_MODIFIED,
  MYX_WBGC_SWITCHED,
  MYX_WBGC_CHANGED_LAYER, // for elements

  MYX_WBGC_REFRESH,
  
  MYX_WBGC_RUBBERBAND_STARTED, // view only
  MYX_WBGC_RUBBERBAND_STOPPED,

  MYX_WBGC_TASK_COMPLETED,
  MYX_WBGC_TASK_CANCELED,

  MYX_WBGC_SELECTION_CHANGE,
    
  MYX_WBGC_LAYOUT_CHANGED

} MYX_WB_GC_CHANGE;

/*
 * Functions
 */

MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_register_grt_wb_module(MYX_GRT *Runtime);
MYX_PUBLIC_FUNC void myx_unregister_grt_wb_module(MYX_GRT *Runtime);

MYX_PUBLIC_FUNC void myx_grt_wb_bridge_initialize(MYX_GRT *Runtime, CGenericCanvas *Canvas, const char *Path);
MYX_PUBLIC_FUNC void myx_grt_wb_bridge_free(MYX_GRT *Runtime, CGenericCanvas *Canvas, const char *Path);

MYX_PUBLIC_FUNC void myx_grt_wb_bridge_set_callbacks(MYX_GRT *Runtime, void *data,
                                                     void (*view_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *viewObj, CGCView *view, MYX_WB_GC_CHANGE change, void *data),
                                                     void (*layer_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *layerObj, CFigureInstance *layer, MYX_WB_GC_CHANGE change, void *data),
                                                     void (*element_changed)(MYX_GRT *Runtime, MYX_GRT_VALUE *elementObj, CFigureInstance *elem, MYX_WB_GC_CHANGE change, void *data));

MYX_PUBLIC_FUNC void myx_grt_wb_bridge_process_pending(MYX_GRT *Runtime);

MYX_PUBLIC_FUNC void myx_grt_wb_bridge_process_relocations(MYX_GRT *Runtime);

MYX_PUBLIC_FUNC int myx_grt_wb_load_style_file(CGenericCanvas *canvas, const char *filename, MYX_GRT_VALUE *variables);

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_wb_object_for_element(CGCBase *elem);

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_wb_object_for_view(CGCView *view);

MYX_PUBLIC_FUNC MYX_GRT_VALUE ** myx_grt_wb_get_selected_objects(MYX_GRT *Runtime, int *count);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
