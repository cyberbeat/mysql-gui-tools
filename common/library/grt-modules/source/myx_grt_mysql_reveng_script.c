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


#include <myx_public_interface.h>
#include <myx_grt_public_interface.h>
#include <myx_grt_builtin_module_public_interface.h>

#include "myx_grt_mysql.h"
#include "myx_grt_mysql_reveng_script.h"
#include "myx_sql_parser_public_interface.h"
#include "myx_grt_mysql_reveng_script_helper.h"

// --------------------------------------------------------------------------
// module registration functions

MYX_GRT_MODULE* myx_register_builtin_grt_module_reverse_engineer_mysql_script(MYX_GRT *grt)
{
  return myx_grt_module_register_builtin(grt, &grt_module_reverse_engineer_mysql_script, grt);
}

/**
 ****************************************************************************
 * @brief Tests the given connection
 *
 *   Connects to a MySQL server using the given connection to test the 
 * connection
 *
 * @param param the connection information stored in a GRT value
 * @param data buildin module private pointer to the GRT struct
 * 
 * @return Returns the list of schema names in a GRT module function return value 
 *****************************************************************************/
MYX_GRT_VALUE *re_script_get_version(MYX_GRT_VALUE *param, void *data)
{
  return NULL;
}

/**
 ****************************************************************************
 * @brief Returns all schemata names of a given database
 *
 *   Connects to a MySQL server using the given connection parameters defined
 * in a GRT value and retrieves a list of schema names.
 *
 * @param param the connection information stored in a GRT value
 * @param data buildin module private pointer to the GRT struct
 * 
 * @return Returns the list of schema names in a GRT module function return value 
 *****************************************************************************/
MYX_GRT_VALUE *re_script_get_schemata(MYX_GRT_VALUE *param, void *data)
{
  return NULL;
}

MYX_GRT_VALUE *re_script_get_character_sets(MYX_GRT_VALUE *param, void *data)
{
  return NULL;
}

static int parse_sql_callback(const char *sql, void *user_data)
{
  HELPER_ARGS *args= (HELPER_ARGS *)user_data;
  myx_set_parser_source(sql);
  myx_parse();
  myx_free_parser_source();
  args->sql= sql;
  convert_parser_dom_to_grt_dom(myx_get_parser_tree(), args);
  return 0; // not used
}

/**
 ****************************************************************************
 * @brief Reverse engineers all database object of the list of given schemata
 *
 *   Connects to a MySQL server using the given connection parameters defined
 * in a GRT value and reverse engineers all database object of the list of given 
 * schemata
 *
 * @param param the connection information stored in a GRT value
 * @param data buildin module private pointer to the GRT struct
 * 
 * @return Returns the list of schema names in a GRT module function return value 
 *****************************************************************************/
MYX_GRT_VALUE *reverse_engineer_script(MYX_GRT_VALUE *param, void *data)
{
  HELPER_ARGS args;
  MYX_GRT *grt= (MYX_GRT *) data;
  const char *sql_filename;
  MYX_GRT_VALUE *sql_script_name;
  MYX_GRT_VALUE *catalog;

  if((!param) || (myx_grt_value_get_type(param) != MYX_LIST_VALUE))
  {
    return make_return_value_error("Bad parameters.", "The first parameter has to be a string list with "
      "one item - script script file name.");
  }
  
  sql_script_name= myx_grt_list_item_get(param, 0);
  if ((myx_grt_value_get_type(sql_script_name) != MYX_STRING_VALUE))
  {
    return make_return_value_error("Bad parameters.", "The first parameter has to be a string list with "
      "one item - script script file name.");
  }

  sql_script_name= myx_grt_list_item_get(param, 0);
  sql_filename= myx_grt_value_as_string(sql_script_name);

  catalog= myx_grt_dict_new_obj(grt, "db.mysql.Catalog", "MySQL Catalog", "", "");

  args.catalog= catalog;
  args.grt= grt;
  args.used_schema= NULL;
  args.sql= NULL;
  myx_process_sql_statements_from_file(sql_filename, parse_sql_callback, &args, MYX_SPM_NORMAL_MODE);

  return make_return_value(catalog);
}

