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

#ifndef myx_grt_mysql_h
#define myx_grt_mysql_h

#include <myx_public_interface.h>
#include <myx_grt_public_interface.h>
#include <myx_grt_builtin_module_public_interface.h>

#ifdef __cplusplus
extern "C" {
#endif

MYX_GRT_VALUE *forms_create(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *forms_init_dict(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *forms_update(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *forms_set_value(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *forms_get_value(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *forms_del_value(MYX_GRT_VALUE *param, void *data);

#ifdef ENABLE_FORMS
static MYX_GRT_BUILTIN_FUNCTION functions_forms[]= {
  {"create::", forms_create },
  {"_initDict::", forms_init_dict },
  {"_update::", forms_update },
  {"_setValue::", forms_set_value },
  {"_getValue::", forms_get_value },
  {"_delValue::", forms_del_value },
};

static MYX_GRT_BUILTIN_MODULE grt_module_forms= {
  "Forms",
  NULL,
  sizeof(functions_forms)/sizeof(MYX_GRT_BUILTIN_FUNCTION),
  functions_forms
};
#endif

#ifdef __cplusplus
};
#endif

#endif
