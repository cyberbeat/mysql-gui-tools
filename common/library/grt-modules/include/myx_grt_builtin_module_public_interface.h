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

#ifndef myx_grt_builtin_module_public_interface_h
#define myx_grt_builtin_module_public_interface_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define __LCC__
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

/// [SCRIPT::LibInterfaceMapper] -public_interface "libmysqlgrtbuiltinmodule"
#define libmysqlgrtbuiltinmodule_PUBLIC_INTERFACE_VERSION 10100

/// [SCRIPT::LibInterfaceMapper] -add_to_uses "myx_grt_public_interface"
/// [SCRIPT::LibInterfaceMapper] -add_datatypes_from "..\..\base-library\include\myx_public_interface.h"

/*
 * Defines
 */

/*
 * Enums
 */

typedef enum tagEditorAction
{
  EDIT_ACTION    = 1001,
  LOCK_ACTION    = 1002,
  PLUGINS_ACTION = 1003
} EditorAction;

/*
 * Structs and Enums
 */


/*
 * Functions
 */

MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_register_builtin_grt_module_base(MYX_GRT *grt);

MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_register_builtin_grt_module_reverse_engineer_mysql(MYX_GRT *grt);

MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_register_builtin_grt_module_reverse_engineer_mysql_script(MYX_GRT *grt);

MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_register_builtin_grt_module_transformation_mysql(MYX_GRT *grt);

MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_register_builtin_grt_module_query_mysql(MYX_GRT *grt);

#if !defined(__WIN__) && !defined(_WIN32) && !defined(_WIN64)
MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_register_builtin_grt_module_forms(MYX_GRT *grt);


void myx_grt_module_base_set_copy_to_clipboard_callback(int (*callback)(const char *text, void *data), void *data);
#endif



#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
