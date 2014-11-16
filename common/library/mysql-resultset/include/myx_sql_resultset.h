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

#ifndef __MYX_GRT_RESULTSET_H_
#define __MYX_GRT_RESULTSET_H_

#ifdef __cplusplus
extern "C" {
#endif

#ifdef RESULTSET_MODULE

MYX_GRT_VALUE * res_close(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * res_move_next(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * res_move_prior(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * res_move_first(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * res_move_last(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * res_field_get(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * res_field_set(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * res_commit(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * res_revert(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * res_current_row_count(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * res_status(MYX_GRT_VALUE *param, void *data);

// GRT return value functions
static MYX_GRT_BUILTIN_FUNCTION functions[]= {
  {"_delete::", res_close},
  {"moveNext::", res_move_next},
  {"movePrior::", res_move_prior},
  {"moveFirst::", res_move_first},
  {"moveLast::", res_move_last},
  {"getField::", res_field_get},
  {"setField::", res_field_set},
  {"commit::", res_commit},
  {"revert::", res_revert},
  {"currentRowCount::", res_current_row_count},
  {"status::", res_status},
};

static MYX_GRT_BUILTIN_MODULE grt_module_result_set= {
  "RdbmsResultSet",
  NULL,
  sizeof(functions)/sizeof(MYX_GRT_BUILTIN_FUNCTION),
  functions
};
  
  
MYX_GRT_VALUE * src_open(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * src_close(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * src_status(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * src_query(MYX_GRT_VALUE *param, void *data);

// GRT return value functions
static MYX_GRT_BUILTIN_FUNCTION src_functions[]= {
  {"open::", src_open},
  {"_delete::", src_close},
  {"status::", src_status},
  {"query::", src_query},
};

static MYX_GRT_BUILTIN_MODULE grt_module_result_set_source= {
  "RdbmsResultSetSource",
  NULL,
  sizeof(src_functions)/sizeof(MYX_GRT_BUILTIN_FUNCTION),
  src_functions
};

#endif
  
MYX_GRT_MODULE* myx_register_builtin_grt_module_result_set(MYX_GRT *grt);
MYX_GRT_MODULE* myx_register_builtin_grt_module_result_set_source(MYX_GRT *grt);


#ifdef __cplusplus
}
#endif

#endif
