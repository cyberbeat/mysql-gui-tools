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

#ifndef __MYX_GRT_MYSQL_QUERY_H_
#define __MYX_GRT_MYSQL_QUERY_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <mysql.h>

MYX_GRT_VALUE * conn_open(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * conn_close(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * query_print(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * query_fetch_result_set(MYX_GRT_VALUE *param, void *data);

// GRT return value functions
static MYX_GRT_BUILTIN_FUNCTION functions[]= {
  {"connOpen::", conn_open},
  {"connClose::", conn_close},
  {"queryPrint::", query_print},
  {"queryFetchResultSet::", query_fetch_result_set}
};

static MYX_GRT_BUILTIN_MODULE grt_module_query_mysql= {
  "QueryMysql",
  NULL,
  sizeof(functions)/sizeof(MYX_GRT_BUILTIN_FUNCTION),
  functions
};

typedef struct
{
  MYSQL *mysql;
  unsigned int ready;
} MYX_GRT_CONNECTION;


MYX_GRT_MODULE* myx_register_builtin_grt_module_query_mysql(MYX_GRT *grt);


#ifdef __cplusplus
}
#endif

#endif
