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

#ifndef myx_grt_base_h
#define myx_grt_base_h

#include <myx_public_interface.h>
#include <myx_grt_public_interface.h>
#include <myx_grt_builtin_module_public_interface.h>

#ifdef __cplusplus
extern "C" {   
#endif /* __cplusplus */

MYX_GRT_VALUE * get_guid(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * get_app_data_dir(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * get_os_type_name(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * pattern_match(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * check_dir(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * create_dir(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * grt_object_editor(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * edit_obj(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * copy_to_clipboard(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * value_from_text(MYX_GRT_VALUE *param, void *data);

// GRT return value functions
static MYX_GRT_BUILTIN_FUNCTION functions_base[]= {
  {"getAppDataDir::", get_app_data_dir},
  {"getGuid::", get_guid},
  {"getOsTypeName::", get_os_type_name},
  {"patternMatch", pattern_match},
  {"checkDir", check_dir},
  {"createDir", create_dir},
  {"getObjEditor", grt_object_editor},
  {"editObj", edit_obj},
  {"copyToClipboard", copy_to_clipboard},
  {"valueFromText", value_from_text}
};

static MYX_GRT_BUILTIN_MODULE grt_module_base= {
  "Base",
  NULL,
  sizeof(functions_base)/sizeof(MYX_GRT_BUILTIN_FUNCTION),
  functions_base
};

MYX_GRT_MODULE* myx_register_builtin_grt_module_base(MYX_GRT *grt);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
