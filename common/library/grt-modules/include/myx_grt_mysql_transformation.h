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

#ifndef __MYX_GRT_MYSQL_TRANSFORMATION_H_
#define __MYX_GRT_MYSQL_TRANSFORMATION_H_

#ifdef __cplusplus
extern "C" {
#endif

MYX_GRT_VALUE * generate_sql_create_statements(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE * execute_sql_statements(MYX_GRT_VALUE *param, void *data);

MYX_GRT_VALUE *get_sql_script(MYX_GRT_VALUE *param, void *data);

MYX_GRT_VALUE *get_sql_create(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *get_sql_drop(MYX_GRT_VALUE *param, void *data);

MYX_GRT_VALUE *get_script_header(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *get_script_footer(MYX_GRT_VALUE *param, void *data);

MYX_GRT_VALUE *split_sql_commands(MYX_GRT_VALUE *param, void *data);

MYX_GRT_VALUE *get_catalog_changes(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *get_sql_alter(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *get_sql_changes(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *apply_sql_changes(MYX_GRT_VALUE *param, void *data);

static MYX_GRT_BUILTIN_FUNCTION functions_transformation[]= {
  {"generateSqlCreateStatements::", generate_sql_create_statements },
  {"executeSqlStatements::", execute_sql_statements },
  {"getSqlScript::", get_sql_script },
  {"getSqlCreate::", get_sql_create },
  {"getSqlDrop::", get_sql_drop },
  {"getScriptHeader::", get_script_header },
  {"getScriptFooter::", get_script_footer },
  {"splitSqlCommands::", split_sql_commands },
  {"getCatalogsChanges::", get_catalog_changes },
  {"getSqlAlter::", get_sql_alter },
  {"getSqlChanges::", get_sql_changes },
  {"applySqlChanges::", apply_sql_changes },
};

static MYX_GRT_BUILTIN_MODULE grt_module_transformation_mysql= {
  "TransformationMysql",
  NULL,
  sizeof(functions_transformation)/sizeof(MYX_GRT_BUILTIN_FUNCTION),
  functions_transformation
};

#ifdef __cplusplus
}
#endif

#endif
