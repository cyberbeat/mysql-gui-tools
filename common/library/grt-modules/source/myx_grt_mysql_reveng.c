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

#include "myx_grt_mysql_reveng.h"
//#include "myx_grt_mysql_reveng_script.h"
//#include "myx_grt_mysql_transformation.h"

#include "myx_grt_mysql.h"

#define STR_FIELD_TO_DICT_ITEM(i, d, k) if (fi[i] > -1) \
  {\
    char *val= myx_convert_dbstr_utf8(mysql, row[fi[i]], -1);\
    myx_grt_dict_item_set_value_from_string(d, k, val);\
    g_free(val);\
  }

#define STR_FIELD_TO_DICT_ITEM_TRIM_LEADING_SPACES(i, d, k) if (fi[i] > -1) \
  {\
    int j= 0; \
    char *val;  \
    while(row[fi[i]][j] <= ' ') \
      j++;  \
    val= myx_convert_dbstr_utf8(mysql, row[fi[i]]+j, -1);\
    myx_grt_dict_item_set_value_from_string(d, k, val);\
    g_free(val);\
  }

static MYX_GRT_VALUE *reverse_engineer_schema(MYX_GRT *grt, MYSQL *mysql, MYX_GRT_VALUE *catalog, const char *schema_name, const int tables_only);
static MYX_GRT_VALUE *reverse_engineer_tables_views(MYX_GRT *grt, MYSQL *mysql, MYX_GRT_VALUE *catalog, MYX_GRT_VALUE *schema, const int tables_only);
static MYX_GRT_VALUE *reverse_engineer_table_data(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                  MYX_GRT_VALUE *schema, MYX_GRT_VALUE *table);
static MYX_GRT_VALUE *reverse_engineer_table_columns(MYX_GRT *grt, MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                     MYX_GRT_VALUE *schema, MYX_GRT_VALUE *table);
static MYX_GRT_VALUE *reverse_engineer_table_indices(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                     MYX_GRT_VALUE *schema, MYX_GRT_VALUE *table);
static MYX_GRT_VALUE *reverse_engineer_table_foreign_keys(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                          MYX_GRT_VALUE *schema, MYX_GRT_VALUE *table);
static MYX_GRT_VALUE *reverse_engineer_table_triggers(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                     MYX_GRT_VALUE *schema, MYX_GRT_VALUE *table);
static MYX_GRT_VALUE *reverse_engineer_view_data(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                 MYX_GRT_VALUE *schema, MYX_GRT_VALUE *view);
static MYX_GRT_VALUE *reverse_engineer_routines(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                 MYX_GRT_VALUE *schema);
static MYX_GRT_VALUE *reverse_engineer_routine_data(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                               MYX_GRT_VALUE *schema, MYX_GRT_VALUE *sp);
static MYX_GRT_VALUE *reverse_engineer_routine_params(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                               MYX_GRT_VALUE *schema, MYX_GRT_VALUE *sp);
static void reverse_engineer_update_foreign_keys_references(MYX_GRT_VALUE *catalog);

// --------------------------------------------------------------------------
// module registration functions

MYX_GRT_MODULE* myx_register_builtin_grt_module_reverse_engineer_mysql(MYX_GRT *grt)
{
  return myx_grt_module_register_builtin(grt, &grt_module_reverse_engineer_mysql, grt);
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
MYX_GRT_VALUE *get_version(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *value= NULL;
  MYSQL *mysql= grt_mysql_connect(param, &value);
  char *version_string;
  if (!mysql)
  {
    // if the connection was not successful, return the error GRT value from connect_mysql()
    return value;
  }

  // build version information
  value= myx_grt_dict_new(NULL, "db.Version");
  myx_grt_dict_generate_id(value);

  version_string= myx_get_mysql_full_version(mysql);
  myx_grt_dict_item_set_value_from_string(value, "name", version_string);
  g_free(version_string);

  myx_grt_dict_item_set_value_from_int(value, "major", myx_get_mysql_major_version(mysql));
  myx_grt_dict_item_set_value_from_int(value, "minor", myx_get_mysql_minor_version(mysql));
  myx_grt_dict_item_set_value_from_int(value, "release", myx_get_mysql_release(mysql));

  return make_return_value(value);
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
MYX_GRT_VALUE *get_schemata(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *value= NULL;
  MYSQL *mysql;
  MYSQL_RES *res;
  MYSQL_ROW row;

  mysql= grt_mysql_connect(param, &value);
  if (!mysql)
  {
    // if the connection was not successful, return the error GRT value from connect_mysql()
    return value;
  }

  // execute query that will return all schemata names
  if (!(res= mysql_list_dbs(mysql, (char*) NULL)))
  {
    // return error on failure
    return make_return_value_mysql_error_and_close(mysql, "The schemata names could not be retrieved.", NULL);
  }

  // loop over resultset
  value= myx_grt_list_new(MYX_STRING_VALUE, NULL);
  while ((row= mysql_fetch_row(res)))
  {
    char *schema_name= myx_convert_dbstr_utf8(mysql, row[0], -1);

    myx_grt_list_item_add_as_string(value, schema_name);
    g_free(schema_name);
  }
  myx_mysql_close(mysql);

  return make_return_value(value);
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
MYX_GRT_VALUE *reverse_engineer(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *) data;
  MYX_GRT_VALUE *catalog;
  MYX_GRT_VALUE *version_res, *version;
  MYX_GRT_VALUE *simple_datatypes;
  MYX_GRT_VALUE *character_sets;
  MYX_GRT_VALUE *schemata;
  MYX_GRT_VALUE *connection_value;
  MYX_GRT_VALUE *schema_names;
  MYX_GRT_VALUE *error= NULL;
  MYSQL *mysql;
  unsigned int i;
  unsigned int tables_only= 0;
  
  // check number of parameter
  if ((!param) || (myx_grt_value_get_type(param) != MYX_LIST_VALUE) ||
    ((myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) < 2)))
  {
    return make_return_value_error("Bad parameters.", "This function takes two parameters, "
      "the connection information in a dictionary and the list of schema names.");
  }

  schema_names= myx_grt_list_item_get(param, 1);
  if (myx_grt_value_get_type(schema_names) != MYX_LIST_VALUE)
  {
    return make_return_value_error("Bad parameters.", "The second parameter has to be a string list of "
      "schema names.");
  }

  if (myx_grt_list_item_count(param) > 2) {
    tables_only= myx_grt_value_as_int(myx_grt_list_item_get(param, 2));
  }

  connection_value= myx_grt_list_item_get(param, 0);

  mysql= grt_mysql_connect(connection_value, &error);
  if (!mysql)
  {
    // if the connection was not successful, return the error GRT value from connect_mysql()
    return error;
  }

  // build the catalog dict
  //catalog= myx_grt_dict_new(NULL, "db.mysql.Catalog");
  //myx_grt_dict_generate_id(catalog);
  catalog= myx_grt_dict_new_obj(grt, "db.mysql.Catalog", "MySQL Catalog", "", "");
  myx_grt_dict_item_set_value_from_string(catalog, "name", "default");
  myx_grt_dict_item_set_value_from_string(catalog, "oldName", "default");

  // set DB version
  version_res= get_version(connection_value, data);
  version= myx_grt_dict_item_get_value(version_res, "value");
  myx_grt_dict_item_set_value_from_string(version, "owner", myx_grt_dict_item_get_as_string(catalog, "_id"));
  myx_grt_dict_item_set_value(catalog, "version", version);
  myx_grt_value_release(version_res);

  // create list of simple datatypes and collations
  simple_datatypes= myx_grt_dict_item_get_value(catalog, "simpleDatatypes");
  character_sets= myx_grt_dict_item_get_value(catalog, "characterSets");
  /*simple_datatypes= myx_grt_list_new(MYX_STRING_VALUE, "db.SimpleDatatype");
  myx_grt_dict_item_set_value(catalog, "simpleDatatypes", simple_datatypes);

  character_sets= myx_grt_list_new(MYX_STRING_VALUE, "db.CharacterSet");
  myx_grt_dict_item_set_value(catalog, "characterSets", character_sets);*/

  // take from connection -> driver -> rdbms if possible
  if (strcmp2(myx_grt_dict_struct_get_name(connection_value), "db.mgmt.Connection") == 0)
  {
    MYX_GRT_VALUE *driver= myx_grt_dict_item_get_reference_value(grt, connection_value, "driver");
    
    if (driver)
    {
      MYX_GRT_VALUE *rdbms= myx_grt_dict_item_get_reference_value(grt, driver, "owner");

      if (rdbms)
      {
        MYX_GRT_VALUE *rdbms_datatypes= myx_grt_dict_item_get_value(rdbms, "simpleDatatypes");
        MYX_GRT_VALUE *rdbms_character_sets= myx_grt_dict_item_get_value(rdbms, "characterSets");

        if (rdbms_datatypes)
        {
          unsigned int i;

          for (i= 0; i < myx_grt_list_item_count(rdbms_datatypes); i++)
          {
            MYX_GRT_VALUE *datatype= myx_grt_list_item_get(rdbms_datatypes, i);

            myx_grt_list_item_add(simple_datatypes, myx_grt_dict_item_get_value(datatype, "_id"));
          }
        }

        if (rdbms_character_sets)
        {
          unsigned int i;

          for (i= 0; i < myx_grt_list_item_count(rdbms_character_sets); i++)
          {
            MYX_GRT_VALUE *character_set= myx_grt_list_item_get(rdbms_character_sets, i);

            myx_grt_list_item_add(character_sets, myx_grt_dict_item_get_value(character_set, "_id"));
          }
        }
      }
    }
  }

  // if there are no entries in the list, call rdbms function to fill it
  if (myx_grt_list_item_count(simple_datatypes) == 0)
  {
    MYX_GRT_VALUE *arg_list= myx_grt_list_new(MYX_ANY_VALUE, NULL);
    MYX_GRT_VALUE *simple_datatypes_objects= myx_grt_list_new(MYX_DICT_VALUE, "db.SimpleDatatype");
    MYX_GRT_ERROR func_error;
    unsigned int i;

    myx_grt_list_item_add(arg_list, simple_datatypes_objects);
    myx_grt_function_get_and_call(grt, "RdbmsInfoMysql", "getSimpleDatatypes", 0, arg_list, &func_error);
    myx_grt_value_release(arg_list);

    // add the list of simpleDatatypes to the catalog, note that this is not an defined member of the Catalog struct
    myx_grt_dict_item_set_value(catalog, "simpleDatatypesObjects", simple_datatypes_objects);
    myx_grt_value_release(simple_datatypes_objects);

    // fill reference list
    for (i= 0; i < myx_grt_list_item_count(simple_datatypes_objects); i++)
    {
      MYX_GRT_VALUE *datatype= myx_grt_list_item_get(simple_datatypes_objects, i);

      myx_grt_list_item_add(simple_datatypes, myx_grt_dict_item_get_value(datatype, "_id"));
    }
  }

  // if there are no entries in the list, call rdbms function to fill it
  if (myx_grt_list_item_count(character_sets) == 0)
  {
    MYX_GRT_VALUE *arg_list= myx_grt_list_new(MYX_ANY_VALUE, NULL);
    MYX_GRT_VALUE *character_sets_objects= myx_grt_list_new(MYX_DICT_VALUE, "db.CharacterSet");
    MYX_GRT_ERROR func_error;
    unsigned int i;

    myx_grt_list_item_add(arg_list, character_sets_objects);
    myx_grt_function_get_and_call(grt, "RdbmsInfoMysql", "getCharacterSets", 0, arg_list, &func_error);
    myx_grt_value_release(arg_list);

    // add the list of characterSets to the catalog, note that this is not an defined member of the Catalog struct
    myx_grt_dict_item_set_value(catalog, "characterSetsObjects", character_sets_objects);
    myx_grt_value_release(character_sets_objects);

    for (i= 0; i < myx_grt_list_item_count(character_sets_objects); i++)
    {
      MYX_GRT_VALUE *character_set= myx_grt_list_item_get(character_sets_objects, i);

      myx_grt_list_item_add(character_sets, myx_grt_dict_item_get_value(character_set, "_id"));
    }
  }
  

  // loop over all schemata
  for (i= 0; i < myx_grt_list_item_count(schema_names); i++)
  {
    const char *schema_name= myx_grt_list_item_get_as_string(schema_names, i);

    error= reverse_engineer_schema(grt, mysql, catalog, schema_name, tables_only);
    if (error)
      break;
  }

  myx_mysql_close(mysql);

  // make sure the Fks use real references instead of 
  // text names where possible
  reverse_engineer_update_foreign_keys_references(catalog);
  
  // return error or success
  if (error)
  {
    myx_grt_value_release(catalog);
    return error;
  }
  else
    return make_return_value(catalog);
}

// -----------------------------------------------------------------------------------------------------------
// reverse_engineer_schema

static const char * show_create_database_fields[]=
{
  "Database",             // 0
  "Create Database",      // 1
};
static const char ** show_create_database_fields_end=
             show_create_database_fields + sizeof(show_create_database_fields)/sizeof(char*);

static const char *sql_show_create_db= "SHOW CREATE DATABASE %s;";

static const char *regex_character_set= "CHARACTER SET (\\w*)";
static const char *regex_collate= "COLLATE (\\w*)";

static MYX_GRT_VALUE *reverse_engineer_schema(MYX_GRT *grt, MYSQL *mysql, MYX_GRT_VALUE *catalog, const char *schema_name, const int tables_only)
{
  MYX_GRT_VALUE *schamata= myx_grt_dict_item_get_value(catalog, "schemata");
  MYX_GRT_VALUE *schema= myx_grt_dict_new_obj(grt, "db.mysql.Schema", schema_name, "", myx_grt_dict_item_get_as_string(catalog, "_id"));
  //MYX_GRT_VALUE *schema_assets;
  MYX_GRT_VALUE *error= NULL;
  MYSQL_RES *res;
  MYSQL_ROW row;
  MYSQL_FIELD *fields;
  int num_fields;

  // create schema
  /*myx_grt_dict_generate_id(schema);
  myx_grt_dict_item_set_value_from_string(schema, "name", schema_name);*/
  myx_grt_dict_item_set_value_from_string(schema, "oldName", schema_name);
  //myx_grt_dict_item_set_value_from_string(schema, "owner", myx_grt_dict_item_get_as_string(catalog, "_id"));

  // add table list
  /*schema_assets= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.Table");
  myx_grt_dict_item_set_value(schema, "tables", schema_assets);
  myx_grt_value_release(schema_assets);

  // add view list
  schema_assets= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.View");
  myx_grt_dict_item_set_value(schema, "views", schema_assets);
  myx_grt_value_release(schema_assets);

  // add routine list
  schema_assets= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.Routine");
  myx_grt_dict_item_set_value(schema, "routines", schema_assets);
  myx_grt_value_release(schema_assets);*/


  // add schema to schemata list
  myx_grt_list_item_add(schamata, schema);
  myx_grt_value_release(schema);

  // for MySQL Servers 4.1 or later, fetch character set information
  if (mysql_version_is_later_or_equal_than(mysql, 4, 1))
  {    
    // execute SQL
    if (myx_mysql_query_esc(mysql, sql_show_create_db, schema_name) ||
        !(res= mysql_store_result(mysql)))
    {
      // Ignore error (e.g. for information_schema)
      //return make_return_value_mysql_error(mysql, "Cannot fetch the charset information.", NULL);
    }
    else
    {
      if ((row= mysql_fetch_row(res)))
      {
        int fi[3];

        //Depending on the version of the server there might be different columns
        num_fields= mysql_num_fields(res);
        fields= mysql_fetch_fields(res);

        build_field_subst(show_create_database_fields, show_create_database_fields_end,
          fields, fields + num_fields, fi);

        if (fi[1] != -1)
        {
          // use a regex to get charset and collation
          char *row_create_database= myx_convert_dbstr_utf8(mysql, row[fi[1]], -1);
          char *character_set= get_value_from_text(row_create_database, (int)strlen(row_create_database), regex_character_set);
          char *collate= get_value_from_text(row_create_database, (int)strlen(row_create_database), regex_collate);

          if (character_set)
            myx_grt_dict_item_set_value_from_string(schema, "defaultCharacterSetName", character_set);

          if (collate)
            myx_grt_dict_item_set_value_from_string(schema, "defaultCollationName", collate);

          g_free(row_create_database);
          g_free(character_set);
          g_free(collate);
        }

        mysql_free_result(res);
      }
    }
  }


  // get the tables and views
  error= reverse_engineer_tables_views(grt, mysql, catalog, schema, tables_only);

  if (!tables_only) {
    // get the routines
    if (!error && mysql_version_is_later_or_equal_than(mysql, 5, 0))
      error= reverse_engineer_routines(mysql, catalog, schema);
  }

  return error;
}

// -----------------------------------------------------------------------------------------------------------
// reverse_engineer_tables_views

static const char *sql_show_tables= "SHOW TABLES";
static const char *sql_show_tables_views= "SHOW FULL TABLES";

static MYX_GRT_VALUE *reverse_engineer_tables_views(MYX_GRT *grt, MYSQL *mysql, MYX_GRT_VALUE *catalog, MYX_GRT_VALUE *schema, const int tables_only)
{
  MYX_GRT_VALUE *schema_tables= myx_grt_dict_item_get_value(schema, "tables");
  MYX_GRT_VALUE *schema_views= myx_grt_dict_item_get_value(schema, "views");
  MYX_GRT_VALUE *error= NULL;
  MYSQL_RES *res;
  MYSQL_ROW row;
  const char *sql;
  char *old_db;

  // change to the correct schema
  if (!use_schema_store_old_one(mysql, myx_grt_dict_item_get_as_string(schema, "name"), &old_db))
  {
    return make_return_value_mysql_error(mysql, "Could not change to given schema.", myx_grt_dict_item_get_as_string(schema, "name"));
  }

  if (mysql_version_is_later_or_equal_than(mysql, 5, 0))
    sql= sql_show_tables_views;
  else
    sql= sql_show_tables;

  // get SHOW FULL TABLES; result set
  if ((error= grt_mysql_execute(mysql, &res, sql, "Could not execute SHOW FULL TABLES statement.")))
  {
    restore_old_schema(mysql, old_db);
    return error;
  }

  // get SHOW TABLES; result set
  /*if (!(res= mysql_list_tables(mysql, (char*) NULL)))
  {
    restore_old_schema(mysql, old_db);
    return make_return_value_mysql_error(mysql, "Could not list schema tables.", NULL);
  }*/

  while ((row= mysql_fetch_row(res)))
  {
    // check if the object is a table, < 5.0 return only 1 row and >= 5.0 return 2 rows, 
    // the second being the "BASE TABLE" || "VIEW"
    if ((mysql_num_fields(res) == 1) || (strcmp2(row[1], "BASE TABLE") == 0) || (strcmp2(row[1], "TEMPORARY") == 0))
    {
      // create table
      MYX_GRT_VALUE *table= myx_grt_dict_new(NULL, "db.mysql.Table");
      char *table_name= myx_convert_dbstr_utf8(mysql, row[0], -1);

      myx_grt_dict_generate_id(table);
      myx_grt_dict_item_set_value_from_string(table, "name", table_name);
      myx_grt_dict_item_set_value_from_string(table, "oldName", table_name);
      myx_grt_dict_item_set_value_from_string(table, "owner", myx_grt_dict_item_get_as_string(schema, "_id"));

      g_free(table_name);

      // add table to schema
      myx_grt_list_item_add(schema_tables, table);
      myx_grt_value_release(table);


      // retrieve table data
      if (strcmp2(row[1], "TEMPORARY") == 0)
        myx_grt_dict_item_set_value_from_int(table, "isTemporary", 1);
      else
        myx_grt_dict_item_set_value_from_int(table, "isTemporary", 0);

      if ((error= reverse_engineer_table_data(mysql, catalog, schema, table)))
        break;

      // retrieve table columns
      if ((error= reverse_engineer_table_columns(grt, mysql, catalog, schema, table)))
        break;

      // retrieve table indices
      if ((error= reverse_engineer_table_indices(mysql, catalog, schema, table)))
        break;

      // retrieve foreign keys
      if ((error= reverse_engineer_table_foreign_keys(mysql, catalog, schema, table)))
        break;

      // retrieve triggers, only > 5.0.10
      if (mysql_full_version_is_later_or_equal_than(mysql, 5, 0, 10) &&
        (error= reverse_engineer_table_triggers(mysql, catalog, schema, table)))
        break;
    }
    else if (strcmp2(row[1], "VIEW") == 0 && tables_only != 1)
    {
      // create view
      MYX_GRT_VALUE *view= myx_grt_dict_new(NULL, "db.mysql.View");
      char *view_name= myx_convert_dbstr_utf8(mysql, row[0], -1);

      myx_grt_dict_generate_id(view);
      myx_grt_dict_item_set_value_from_string(view, "name", view_name);
      myx_grt_dict_item_set_value_from_string(view, "oldName", view_name);
      myx_grt_dict_item_set_value_from_string(view, "owner", myx_grt_dict_item_get_as_string(schema, "_id"));

      g_free(view_name);

      // add view to schema
      myx_grt_list_item_add(schema_views, view);
      myx_grt_value_release(view);

      // retrieve view data
      if ((error= reverse_engineer_view_data(mysql, catalog, schema, view)))
        break;
    }
  }

  mysql_free_result(res);

  restore_old_schema(mysql, old_db);

  return error;
}

// -----------------------------------------------------------------------------------------------------------
// reverse_engineer_table_data

static const char * show_table_status_fields[]=
{
  "Name",              // 0
  "Engine",            // 1
  "Type",              // 2
  "Version",           // 3
  "Row_format",        // 4
  "Rows",              // 5
  "Avg_row_length",    // 6
  "Data_length",       // 7
  "Max_data_length",   // 8
  "Index_length",      // 9
  "Data_free",         // 10
  "Auto_increment",    // 11
  "Create_time",       // 12
  "Update_time",       // 13
  "Check_time",        // 14
  "Collation",         // 15
  "Checksum",          // 16
  "Create_options",    // 17
  "Comment"            // 18
};
static const char ** show_table_status_fields_end=
             show_table_status_fields + sizeof(show_table_status_fields)/sizeof(char*);

static const char *sql_show_table_create= "SHOW CREATE TABLE %s;";
static const char *sql_show_table_status= "SHOW TABLE STATUS LIKE '%s';";

static MYX_GRT_VALUE *reverse_engineer_table_data(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                  MYX_GRT_VALUE *schema, MYX_GRT_VALUE *table)
{

  char *sql= myx_mysql_esc(mysql, sql_show_table_create, myx_grt_dict_item_get_as_string(table, "name"));
  MYX_GRT_VALUE *error;
  MYSQL_RES *res;
  MYSQL_ROW row;
  char *create_table_statement= NULL;
  char *tbl_options= NULL;

  // get create table statement
  if ((error= grt_mysql_execute_and_free(mysql, &res, sql, "Could not execute SHOW CREATE TABLE statement.")))
    return error;

  if ((row= mysql_fetch_row(res)))
  {
    create_table_statement= myx_convert_dbstr_utf8(mysql, row[1], -1);
    myx_grt_dict_item_set_value_from_string(table, "sql", create_table_statement);
  }

  mysql_free_result(res);

  // get table status
  sql= g_strdup_printf(sql_show_table_status, myx_grt_dict_item_get_as_string(table, "name"));

  if ((error= grt_mysql_execute_and_free(mysql, &res, sql, "Could not execute SHOW TABLE STATUS statement.")))
    return error;

  if ((row= mysql_fetch_row(res)))
  {
    int fi[19];
    MYSQL_FIELD *fields;
    int num_fields;
    unsigned long *lengths;

    //Depending on the version of the server there might be different columns
    num_fields= mysql_num_fields(res);
    fields= mysql_fetch_fields(res);
    lengths = mysql_fetch_lengths(res);

    build_field_subst(show_table_status_fields, show_table_status_fields_end,
      fields, fields + num_fields, fi);

    // tableEngine
    if ((fi[1] > -1) || (fi[2] > -1))
    {
      char *val= (fi[1] > -1) ? myx_convert_dbstr_utf8(mysql, row[fi[1]], -1) : myx_convert_dbstr_utf8(mysql, row[fi[2]], -1);
      myx_grt_dict_item_set_value_from_string(table, "tableEngine", val);
      g_free(val);
    }

    // rowFormat
    STR_FIELD_TO_DICT_ITEM(4, table, "rowFormat");

    // nextAutoInc
    STR_FIELD_TO_DICT_ITEM(11, table, "nextAutoInc");

    // defaultCollationName
    STR_FIELD_TO_DICT_ITEM(15, table, "defaultCollationName");

    // comment
    STR_FIELD_TO_DICT_ITEM(18, table, "comment");

    // table_options
    if (fi[17] > -1)
      tbl_options= myx_convert_dbstr_utf8(mysql, row[fi[17]], -1);
  }

  mysql_free_result(res);

#define STR_REGEX_TO_DICT_ITEM(txt, txtlen, regex, d, k) {\
      char *tmp= get_value_from_text(txt, txtlen, regex);\
      if (tmp)\
        myx_grt_dict_item_set_value_from_string(d, k, tmp);\
      g_free(tmp);\
    }

  if (tbl_options && tbl_options[0])
  {
    unsigned int len= (unsigned int)strlen(tbl_options);
    char *tmp;

    // delayKeyWrite
    tmp= get_value_from_text(tbl_options, len, "DELAY_KEY_WRITE\\s*=\\s*(0|1)");
    if (tmp)
      myx_grt_dict_item_set_value_from_int(table, "delayKeyWrite", (strcmp2(tmp, "1") == 0) ? 1 : 0);
    g_free(tmp);

    // packKeys
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "PACK_KEYS\\s*=\\s*(\\w+)", table, "packKeys");

    // avgRowLength
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "AVG_ROW_LENGTH\\s*=\\s*(\\w+)", table, "avgRowLength");

    // minRows
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "MIN_ROWS\\s*=\\s*(\\w+)", table, "minRows");

    // maxRows
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "MAX_ROWS\\s*=\\s*(\\w+)", table, "maxRows");

    // checksum
    tmp= get_value_from_text(tbl_options, len, "CHECKSUM=\\s*=\\s*(1)");
    if (tmp)
      myx_grt_dict_item_set_value_from_int(table, "checksum", (strcmp2(tmp, "1") == 0) ? 1 : 0);
    g_free(tmp);

    g_free(tbl_options);
  }

  // process CREATE TABLE statement
  if(create_table_statement)
  {
    unsigned int len;

    // mask out strings
    tbl_options= mask_out_string(g_strdup(create_table_statement), '\'', '\'', 'x');
    len= (unsigned int)strlen(tbl_options);

    // defaultCharacterSetName
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "DEFAULT CHARSET\\s*=\\s*(\\w*)", table, "defaultCharacterSetName");

    // mergeUnion
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "UNION\\s*=\\s*\\((.+)\\)", table, "mergeUnion");

    // mergeInsert
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "INSERT_METHOD\\s*=\\s*(\\w+)", table, "mergeInsert");

    // tableDataDir
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "DATA DIRECTORY\\s*=\\s*'(.+)'", table, "tableDataDir");

    // tableIndexDir
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "INDEX DIRECTORY\\s*=\\s*'(.+)'", table, "tableIndexDir");
    
    // raidType
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "RAID_TYPE\\s*=\\s*(\\w+)", table, "raidType");

    // raidChunks
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "RAID_CHUNKS\\s*=\\s*(\\w+)", table, "raidChunks");

    // raidChunkSize
    STR_REGEX_TO_DICT_ITEM(tbl_options, len, "RAID_CHUNKSIZE\\s*=\\s*(\\w+)", table, "raidChunkSize");

    g_free(tbl_options);
    g_free(create_table_statement);
  }

#undef STR_REGEX_TO_DICT_ITEM

  return NULL;
}

// -----------------------------------------------------------------------------------------------------------
// reverse_engineer_table_columns

static const char * show_table_columns_fields[]=
{
  "Field",       // 0
  "Type",        // 1
  "Collation",   // 2
  "Null",        // 3
  "Key",         // 4
  "Default",     // 5
  "Extra",       // 6
  "Privileges",  // 7
  "Comment"      // 8
};
static const char ** show_table_columns_fields_end=
             show_table_columns_fields + sizeof(show_table_columns_fields)/sizeof(char*);

static const char *sql_show_table_columns= "SHOW FULL COLUMNS FROM %s;";

static MYX_GRT_VALUE *reverse_engineer_table_columns(MYX_GRT *grt, MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                     MYX_GRT_VALUE *schema, MYX_GRT_VALUE *table)
{
  MYX_GRT_VALUE *columns= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.Column");
  MYX_GRT_VALUE *error;
  MYX_GRT_VALUE *datatypes= myx_grt_dict_item_get_value(catalog, "simpleDatatypes");
  char *sql= myx_mysql_esc(mysql, sql_show_table_columns, myx_grt_dict_item_get_as_string(table, "name"));
  MYSQL_RES *res;
  MYSQL_ROW row;
  unsigned int i;
  GHashTable *datatype_cache= g_hash_table_new_full(g_str_hash, g_str_equal, NULL, NULL);

  // build datatype cache
  for (i= 0; i < myx_grt_list_item_count(datatypes); i++)
  {
    MYX_GRT_VALUE *datatype= myx_grt_list_item_get_reference_value(grt, datatypes, i);
    g_hash_table_insert(datatype_cache, (char *)myx_grt_dict_item_get_as_string(datatype, "name"), datatype);
  }

  // add column list to table
  myx_grt_dict_item_set_value(table, "columns", columns);
  myx_grt_value_release(columns);

  // execute SQL
  if ((error= grt_mysql_execute_and_free(mysql, &res, sql, "Could not get table columns.")))
    return error;

  while ((row= mysql_fetch_row(res)))
  {
    int fi[9];
    MYX_GRT_VALUE *column= myx_grt_dict_new(NULL, "db.mysql.Column");
    char *val;
    MYSQL_FIELD *fields;
    int num_fields;
    unsigned long *lengths;    

    myx_grt_dict_generate_id(column);

    //Depending on the version of the server there might be different columns
    num_fields= mysql_num_fields(res);
    fields= mysql_fetch_fields(res);
    lengths = mysql_fetch_lengths(res);

    build_field_subst(show_table_columns_fields, show_table_columns_fields_end,
      fields, fields + num_fields, fi);

    // column name
    val= myx_convert_dbstr_utf8(mysql, row[fi[0]], -1);
    myx_grt_dict_item_set_value_from_string(column, "name", val);
    myx_grt_dict_item_set_value_from_string(column, "oldName", val);
    g_free(val);

    // column datatype definition
    val= myx_convert_dbstr_utf8(mysql, row[fi[1]], -1);
    if (val)
    {
      char *datatype_name= get_value_from_text_ex(val, (int)strlen(val), "(\\w*)\\s*(\\((.*)\\)\\s*)?", 1);
      char *explicit_params= get_value_from_text_ex(val, (int)strlen(val), "(\\w*)\\s*(\\((.*)\\)\\s*)?", 3);
      MYX_GRT_VALUE *datatype= NULL;
      MYX_GRT_VALUE* flags = NULL;
      char* flag_names = NULL;

      // Parse additional type attributes (like binary, zerofill etc.).
      flags = myx_grt_list_new(MYX_STRING_VALUE, NULL);
      myx_grt_dict_item_set_value(column, "flags", flags);
      myx_grt_value_release(flags);

      flag_names = get_value_from_text_ex(val, (int)strlen(val), "(\\w*)\\s*(\\((.*)\\)\\s*)?(.*)", 4);
      if (flag_names != NULL)
      {
        char* separators = " \t";
        char* token = strtok(flag_names, separators);
        while (token != NULL)
        {
          myx_grt_list_item_add_as_string(flags, token);
          token = strtok(NULL, separators);
        };
        g_free(flag_names);
      };

      if (datatype_name)
      {
        datatype_name= str_toupper(datatype_name);
        myx_grt_dict_item_set_value_from_string(column, "datatypeName", datatype_name);

        datatype= g_hash_table_lookup(datatype_cache, datatype_name);

        if (datatype)
          myx_grt_dict_item_set_value_from_string(column, "simpleType", myx_grt_dict_id_item_as_string(datatype));
      }

      if (explicit_params && explicit_params[0])
      {
        if (datatype)
        {
          // check if this datatype has a precision
          if (myx_grt_dict_item_get_as_int(datatype, "numericPrecision") > 0)
          {
            char *precision= get_value_from_text_ex(explicit_params, (int)strlen(val), "(\\d+).?(\\d*)", 1);
            char *scale= get_value_from_text_ex(explicit_params, (int)strlen(val), "(\\d+).?(\\d*)", 2);

            if (precision)
              myx_grt_dict_item_set_value_from_int(column, "precision", atoi(precision));

            if (scale)
              myx_grt_dict_item_set_value_from_int(column, "scale", atoi(scale));
            
            g_free(precision);
            g_free(scale);
          }
          // check if this datatype has a Length
          else if (myx_grt_dict_item_get_as_int(datatype, "characterMaximumLength") > 0)
          {
            myx_grt_dict_item_set_value_from_int(column, "length", atoi(explicit_params));
          }
          // else store the info in datatypeExplicitParams
          else
          {
            char *tmp= g_strdup_printf("(%s)", explicit_params);
            myx_grt_dict_item_set_value_from_string(column, "datatypeExplicitParams", tmp);
            g_free(tmp);
          }
        }
        else
        {
          char *tmp= g_strdup_printf("(%s)", explicit_params);
          myx_grt_dict_item_set_value_from_string(column, "datatypeExplicitParams", tmp);
          g_free(tmp);
        }
      };

      g_free(datatype_name);
      g_free(explicit_params);
      g_free(val);
    }



    // collation
    if ((fi[2] > -1) && (row[fi[2]]) && (strcmp2(row[fi[2]], "NULL") != 0))
    {
      val= myx_convert_dbstr_utf8(mysql, row[fi[2]], -1);
      myx_grt_dict_item_set_value_from_string(column, "collationName", val);
      g_free(val);
    }

    // isNullable
    myx_grt_dict_item_set_value_from_int(column, "isNullable", strcmp2(row[fi[3]], "YES") ? 0 : 1);

    // defaultValue
    if (row[fi[5]])
    {
      char *default_value;
      val= myx_convert_dbstr_utf8(mysql, row[fi[5]], -1);
      if (val && (g_ascii_strcasecmp(val, "CURRENT_TIMESTAMP") == 0))
        myx_grt_dict_item_set_value_from_string(column, "defaultValue", val);
      else
      {
        default_value= g_strdup_printf("'%s'", val);
        myx_grt_dict_item_set_value_from_string(column, "defaultValue", default_value);
        g_free(default_value);
      }
      g_free(val);

      myx_grt_dict_item_set_value_from_int(column, "defaultValueIsNull", 0);
    }
    else
      myx_grt_dict_item_set_value_from_int(column, "defaultValueIsNull", 1);

    // autoIncrement, check if auto_increment is in the extra field
    if ((fi[6] > -1) && (row[fi[6]]))
      myx_grt_dict_item_set_value_from_int(column, "autoIncrement", 
        (g_strstr_len(row[fi[6]], lengths[fi[6]], "auto_increment")) ? 1 : 0);

    // comment
    if ((fi[8] > -1) && (row[fi[8]]))
    {
      val= myx_convert_dbstr_utf8(mysql, row[fi[8]], -1);
      myx_grt_dict_item_set_value_from_string(column, "comment", val);
      g_free(val);
    }

    // add the column to the table
    myx_grt_list_item_add(columns, column);
    myx_grt_value_release(column);

    // primary key
    /*if ((row[fi[4]]) && (strcmp2(row[fi[4]], "PRI") == 0))
    {
      MYX_GRT_VALUE *pk= myx_grt_dict_item_get_value(table, "primaryKey");
      MYX_GRT_VALUE *pk_columns;

      if (!pk)
      {
        char *pk_name= g_strdup_printf("%s_primary_key", myx_grt_dict_item_get_as_string(table, "name"));

        //pk= myx_grt_dict_new(NULL, "db.mysql.PrimaryKey");
        pk= myx_grt_dict_new(NULL, "db.PrimaryKey");
        myx_grt_dict_generate_id(pk);
        myx_grt_dict_item_set_value_from_string(pk, "name", pk_name);
        myx_grt_dict_item_set_value_from_string(pk, "owner", myx_grt_dict_item_get_as_string(table, "_id"));

        g_free(pk_name);

        pk_columns= myx_grt_list_new(MYX_STRING_VALUE, "db.Column");
        myx_grt_dict_item_set_value(pk, "columns", pk_columns);
        myx_grt_value_release(pk_columns);

        myx_grt_dict_item_set_value(table, "primaryKey", pk);
        myx_grt_value_release(pk);
      }
      else
        pk_columns= myx_grt_dict_item_get_value(pk, "columns");

      myx_grt_list_item_add_as_string(pk_columns, myx_grt_dict_item_get_as_string(column, "_id"));

      myx_grt_list_item_count(pk_columns);
    }*/
  }

  mysql_free_result(res);

  g_hash_table_destroy(datatype_cache);

  return NULL;
}


// -----------------------------------------------------------------------------------------------------------
// reverse_engineer_table_indices

static const char * show_table_indices_fields[]=
{
  "Table",        // 0
  "Non_unique",   // 1
  "Key_name",     // 2
  "Seq_in_index", // 3
  "Column_name",  // 4
  "Collation",    // 5
  "Cardinality",  // 6
  "Sub_part",     // 7
  "Packed",       // 8
  "Null",         // 9
  "Index_type",   // 10
  "Comment"       // 11
};
static const char ** show_table_indices_fields_end=
             show_table_indices_fields + sizeof(show_table_indices_fields)/sizeof(char*);

static const char *sql_show_table_indices= "SHOW INDEX FROM %s;";

static MYX_GRT_VALUE *reverse_engineer_table_indices(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                     MYX_GRT_VALUE *schema, MYX_GRT_VALUE *table)
{
  MYX_GRT_VALUE *indices= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.Index");
  MYX_GRT_VALUE *index= NULL;
  MYX_GRT_VALUE *index_columns;
  MYX_GRT_VALUE *error;
  MYX_GRT_VALUE *table_columns= myx_grt_dict_item_get_value(table, "columns");
  char *sql= myx_mysql_esc(mysql, sql_show_table_indices, myx_grt_dict_item_get_as_string(table, "name"));
  MYSQL_RES *res;
  MYSQL_ROW row;
  char *current_index_name= NULL;
  int fi[12];
  MYSQL_FIELD *fields;
  int num_fields;
  unsigned long *lengths;
  const char *index_column_name;

  // add index list to table
  myx_grt_dict_item_set_value(table, "indices", indices);
  myx_grt_value_release(indices);

  // execute SQL
  if ((error= grt_mysql_execute_and_free(mysql, &res, sql, "Could not get table indices.")))
    return error;

  //Depending on the version of the server there might be different columns
  num_fields= mysql_num_fields(res);
  fields= mysql_fetch_fields(res);
  lengths = mysql_fetch_lengths(res);

  build_field_subst(show_table_indices_fields, show_table_indices_fields_end,
    fields, fields + num_fields, fi);

  while ((row= mysql_fetch_row(res)))
  {
    MYX_GRT_VALUE *index_column;
    unsigned int i;

    /*if (strcmp2(row[fi[2]], "PRIMARY") == 0)
      continue;*/

    // check if this is a new index (and not just the 2nd or 3rd column of the last index)
    // if it is a new index, create the GRT value for it
    if (strcmp2(current_index_name, row[fi[2]]) != 0)
    {
      char *index_type= row[fi[10]];      

      current_index_name= row[fi[2]];

      index= myx_grt_dict_new(NULL, "db.mysql.Index");
      myx_grt_dict_generate_id(index);

      // name
      STR_FIELD_TO_DICT_ITEM(2, index, "name");
      STR_FIELD_TO_DICT_ITEM(2, index, "oldName");

      // owner
      myx_grt_dict_item_set_value_from_string(index, "owner", myx_grt_dict_item_get_as_string(table, "_id"));


      // comment
      STR_FIELD_TO_DICT_ITEM(11, index, "comment");

      // unique
      if (fi[1] > -1)
        myx_grt_dict_item_set_value_from_int(index, "unique", strcmp2(row[fi[1]], "1") ? 1 : 0);

      // indexKind and indexType
      if (strcmp2(row[fi[2]], "PRIMARY") == 0)
        myx_grt_dict_item_set_value_from_string(index, "indexKind", "PRIMARY");
      else
        myx_grt_dict_item_set_value_from_string(index, "indexKind", "");

      if(strcmp2(index_type, "BTREE") == 0)
      {
        myx_grt_dict_item_set_value_from_string(index, "indexType", "BTREE");
      }
      else if(strcmp2(index_type, "RTREE") == 0)
      {
        myx_grt_dict_item_set_value_from_string(index, "indexType", "RTREE");
      }
      else if(strcmp2(index_type, "HASH") == 0)
      {
        myx_grt_dict_item_set_value_from_string(index, "indexType", "HASH");
      }
      else if(strcmp2(index_type, "FULLTEXT") == 0)
      {
        myx_grt_dict_item_set_value_from_string(index, "indexKind", "");
        myx_grt_dict_item_set_value_from_string(index, "indexType", "FULLTEXT");
      }
      else if(strcmp2(index_type, "SPATIAL") == 0)
      {
        myx_grt_dict_item_set_value_from_string(index, "indexKind", "");
        myx_grt_dict_item_set_value_from_string(index, "indexType", "SPATIAL");
      }

      if ( (myx_grt_dict_item_get_as_int(index, "unique") == 1) && 
        (strcmp2(row[fi[2]], "PRIMARY") != 0) )
      {
        myx_grt_dict_item_set_value_from_string(index, "indexKind", "UNIQUE");
      }

      // create index column list
      index_columns= myx_grt_list_new(MYX_DICT_VALUE, "db.IndexColumn");

      // add index column list to index
      myx_grt_dict_item_set_value(index, "columns", index_columns);
      myx_grt_value_release(index_columns);

      // add index to the table index list
      myx_grt_list_item_add(indices, index);

      // if this is the primary key, set it in the table
      if (strcmp2(row[fi[2]], "PRIMARY") == 0)
      {
        myx_grt_dict_item_set_value_from_int(index, "isPrimary", 1);

        myx_grt_dict_item_set_value_from_string(table, "primaryKey", 
          myx_grt_dict_item_get_as_string(index, "_id"));
      }

      myx_grt_value_release(index);
    }
    
    // create index column
    index_column= myx_grt_dict_new(NULL, "db.mysql.IndexColumn");
    myx_grt_dict_generate_id(index_column);

    // name
    STR_FIELD_TO_DICT_ITEM(4, index_column, "name");

    // owner
    myx_grt_dict_item_set_value_from_string(index_column, "owner", myx_grt_dict_item_get_as_string(index, "_id"));

    // columnLength
    if ((fi[7] > -1) && (row[fi[7]]) && (row[fi[7]][0]))
      myx_grt_dict_item_set_value_from_int(index_column, "columnLength", atoi(row[fi[7]]));

    // descend
    if (fi[5] > -1)
      myx_grt_dict_item_set_value_from_int(index_column, "descend", (strcmp2(row[fi[5]], "A") == 0) ? 0 : 1);
    else
      myx_grt_dict_item_set_value_from_int(index_column, "descend", 0);

    // referedColumn
    index_column_name= myx_grt_dict_item_get_as_string(index_column, "name");
    for (i= 0; i < myx_grt_list_item_count(table_columns); i++)
    {
      MYX_GRT_VALUE *column= myx_grt_list_item_get(table_columns, i);

      if (strcmp2(myx_grt_dict_item_get_as_string(column, "name"), index_column_name) == 0)
      {
        myx_grt_dict_item_set_value_from_string(index_column, "referedColumn", 
          myx_grt_dict_id_item_as_string(column));
        break;
      }
    }

    // add index column to index
    myx_grt_list_item_add(index_columns, index_column);
    myx_grt_value_release(index_column);
  }

  mysql_free_result(res);

  return NULL;
}

// -----------------------------------------------------------------------------------------------------------
// reverse_engineer_table_foreign_keys

#define QUOTED_ID "(\\w+|`.+?`|\".+?\")"
#define QUOTED_ID_ "(?:\\w+|`.+?`|\".+?\")"

static const char *regex_foreign_keys= ".*?CONSTRAINT "QUOTED_ID
    " FOREIGN KEY \\(([^)]+)\\) REFERENCES ((?:"QUOTED_ID_"."QUOTED_ID_")|"QUOTED_ID_")"
    " \\(([^)]+)\\)((?: ON \\w+ (?:NO\\sACTION|SET\\sNULL|\\w+))*)";

static MYX_GRT_VALUE *reverse_engineer_table_foreign_keys(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                          MYX_GRT_VALUE *schema, MYX_GRT_VALUE *table)
{
  MYX_GRT_VALUE *fks= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.ForeignKey");
  MYX_GRT_VALUE *table_columns= myx_grt_dict_item_get_value(table, "columns");
  pcre *pcre_exp;
  const char *error_str;
  int error_offset;
  int o_vector[256];
  int rc;
  int offset;
  static struct {
    char *kw;
    MYX_DBM_FK_ACTION action;
  } fk_actions[]= {
    {"CASCADE", MYX_DBM_FA_CASCADE},
    {"SET NULL", MYX_DBM_FA_SET_NULL},
    {"NO ACTION", MYX_DBM_FA_NO_ACTION},
    {"RESTRICT", MYX_DBM_FA_RESTRICT},
    {NULL, MYX_DBM_FA_NO_ACTION}
  };
  char *table_def= mask_out_string(g_strdup(myx_grt_dict_item_get_as_string(table, "sql")), '\'', '\'', 'x');

  myx_grt_dict_item_set_value(table, "foreignKeys", fks);
  myx_grt_value_release(fks);

  pcre_exp= pcre_compile(regex_foreign_keys, PCRE_CASELESS|PCRE_UTF8|PCRE_DOTALL, &error_str, &error_offset, NULL);
  
  if (pcre_exp)
  {
    offset= 0;
    while ((rc= pcre_exec(pcre_exp, NULL, table_def, (int)strlen(table_def),
                       offset, 0, o_vector, sizeof(o_vector)/sizeof(int))) > 0)
    {
      MYX_GRT_VALUE *fk= myx_grt_dict_new(NULL, "db.mysql.ForeignKey");
      MYX_GRT_VALUE *fk_columns= myx_grt_list_new(MYX_STRING_VALUE, "db.mysql.Column");
      MYX_GRT_VALUE *fk_refered_column_names= myx_grt_list_new(MYX_STRING_VALUE, NULL);
      MYX_GRT_VALUE *fk_refered_columns= myx_grt_list_new(MYX_STRING_VALUE, "db.mysql.Column");
      const char *value;
      char **src;
      char **dst;
      int i;
      char *tmp;
      char *reference_schema_name;
      char *reference_table_name;

      // _id
      myx_grt_dict_generate_id(fk);

      // name
      pcre_get_substring(table_def, o_vector, rc, 1, &value);
      tmp= unquote_identifier(g_strdup(value));
      myx_grt_dict_item_set_value_from_string(fk, "name", tmp);
      myx_grt_dict_item_set_value_from_string(fk, "oldName", tmp);
      g_free(tmp);
      pcre_free_substring(value);

      // add column list
      myx_grt_dict_item_set_value(fk, "columns", fk_columns);
      myx_grt_value_release(fk_columns);

      // add refered column name list
      myx_grt_dict_item_set_value(fk, "referedColumnNames", fk_refered_column_names);
      myx_grt_value_release(fk_refered_column_names);

      // add refered column list
      myx_grt_dict_item_set_value(fk, "referedColumns", fk_refered_columns);
      myx_grt_value_release(fk_refered_columns);

      // add FK to the table's foreign key list
      myx_grt_list_item_add(fks, fk);
      myx_grt_value_release(fk);


      // source columns
      pcre_get_substring(table_def, o_vector, rc, 2, &value);
      src= g_strsplit(value, ", ", 0);
      pcre_free_substring(value);

      // referedTableSchemaName and referedTableName
      pcre_get_substring(table_def, o_vector, rc, 3, &value);
      split_schema_table(g_strdup(value),
                         &reference_schema_name,
                         &reference_table_name);

      myx_grt_dict_item_set_value_from_string(fk, "referedTableSchemaName", reference_schema_name);
      myx_grt_dict_item_set_value_from_string(fk, "referedTableName", reference_table_name);

      g_free(reference_schema_name);
      g_free(reference_table_name);
      
      pcre_free_substring(value);


      // target columns
      pcre_get_substring(table_def, o_vector, rc, 4, &value);
      dst= g_strsplit(value, ", ", 0);
      pcre_free_substring(value);
      
      // check column count
      for (i= 0; src[i] && dst[i]; i++);
      g_assert(!src[i] && !dst[i]);
      
      for (i= 0; src[i]; i++)
      {
        unsigned int j;

        unquote_identifier(src[i]);

        // look for the _id of the source column
        for (j= 0; j < myx_grt_list_item_count(table_columns); j++)
        {
          MYX_GRT_VALUE *table_column= myx_grt_list_item_get(table_columns, j);

          if (strcmp2(myx_grt_dict_item_get_as_string(table_column, "name"), src[i]) == 0)
          {
            myx_grt_list_item_add_as_string(fk_columns, myx_grt_dict_item_get_as_string(table_column, "_id"));
            break;
          }
        }

        // add target column
        myx_grt_list_item_add_as_string(fk_refered_column_names, unquote_identifier(dst[i]));
      }
      g_strfreev(src);
      g_strfreev(dst);

      myx_grt_dict_item_set_value_from_string(fk, "deleteRule", "RESTRICT");
      myx_grt_dict_item_set_value_from_string(fk, "updateRule", "RESTRICT");

      // actions
      pcre_get_substring(table_def, o_vector, rc, 5, &value);
      {
        char *ptr;
        
        ptr= strstr(value, "ON UPDATE ");
        if (ptr)
        {
          ptr += sizeof("ON UPDATE ")-1;
          for (i= 0; fk_actions[i].kw; i++)
          {
            if (strncmp(ptr, fk_actions[i].kw, strlen(fk_actions[i].kw))==0)
            {
              myx_grt_dict_item_set_value_from_string(fk, "updateRule", fk_actions[i].kw);
              break;
            }
          }
        }
        ptr= strstr(value, "ON DELETE ");
        if (ptr)
        {
          ptr += sizeof("ON DELETE ")-1;
          for (i= 0; fk_actions[i].kw; i++)
          {
            if (strncmp(ptr, fk_actions[i].kw, strlen(fk_actions[i].kw))==0)
            {
              myx_grt_dict_item_set_value_from_string(fk, "deleteRule", fk_actions[i].kw);
              break;
            }
          }
        }
      }
     
      pcre_free_substring(value);

      //Move offset
      offset= o_vector[1];
    }
    pcre_free(pcre_exp);
  }

  return NULL;
}

static const char * trigger_fields[]=
{
  "TRIGGER_CATALOG",            //0
  "TRIGGER_SCHEMA",             //1
  "TRIGGER_NAME",               //2
  "EVENT_MANIPULATION",         //3
  "EVENT_OBJECT_CATALOG",       //4
  "EVENT_OBJECT_SCHEMA",        //5
  "EVENT_OBJECT_TABLE",         //6
  "ACTION_ORDER",               //7
  "ACTION_CONDITION",           //8
  "ACTION_STATEMENT",           //9
  "ACTION_ORIENTATION",         //10
  "ACTION_TIMING",              //11
  "ACTION_REFERENCE_OLD_TABLE", //12 
  "ACTION_REFERENCE_NEW_TABLE", //13
  "ACTION_REFERENCE_OLD_ROW",   //14
  "ACTION_REFERENCE_NEW_ROW",   //15
  "CREATED"                     //16
};
static const char ** trigger_fields_end=
             trigger_fields + sizeof(trigger_fields)/sizeof(char*);

static const char *sql_triggers= "SELECT * FROM information_schema.TRIGGERS WHERE EVENT_OBJECT_SCHEMA='%s' AND "
                                 " EVENT_OBJECT_TABLE='%s' ORDER BY ACTION_ORDER";

static MYX_GRT_VALUE *reverse_engineer_table_triggers(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                      MYX_GRT_VALUE *schema, MYX_GRT_VALUE *table)
{
  MYX_GRT_VALUE *triggers= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.Trigger");
  MYX_GRT_VALUE *error;
  char *sql= g_strdup_printf(sql_triggers, 
    myx_grt_dict_item_get_as_string(schema, "name"),
    myx_grt_dict_item_get_as_string(table, "name"));
  MYSQL_RES *res;
  MYSQL_ROW row;
 // char *current_index_name= NULL;
  int fi[17];
  MYSQL_FIELD *fields;
  int num_fields;
  unsigned long *lengths;

  // add trigger list to table
  myx_grt_dict_item_set_value(table, "triggers", triggers);
  myx_grt_value_release(triggers);

  // execute SQL
  if ((error= grt_mysql_execute_and_free(mysql, &res, sql, "Could not get table triggers.")))
    return error;

  //Depending on the version of the server there might be different columns
  num_fields= mysql_num_fields(res);
  fields= mysql_fetch_fields(res);
  lengths = mysql_fetch_lengths(res);

  build_field_subst(trigger_fields, trigger_fields_end,
    fields, fields + num_fields, fi);

  while ((row= mysql_fetch_row(res)))
  {
    // create index column
    MYX_GRT_VALUE *trigger= myx_grt_dict_new(NULL, "db.mysql.Trigger");
    myx_grt_dict_generate_id(trigger);

    // name
    STR_FIELD_TO_DICT_ITEM(2, trigger, "name");
    STR_FIELD_TO_DICT_ITEM(2, trigger, "oldName");

    // owner
    myx_grt_dict_item_set_value_from_string(trigger, "owner", myx_grt_dict_item_get_as_string(table, "_id"));

    // event
    STR_FIELD_TO_DICT_ITEM(3, trigger, "event");

    // condition
    STR_FIELD_TO_DICT_ITEM(8, trigger, "condition");

    // statement
    //STR_FIELD_TO_DICT_ITEM(9, trigger, "statement");
    STR_FIELD_TO_DICT_ITEM_TRIM_LEADING_SPACES(9, trigger, "statement");

    // order
    myx_grt_dict_item_set_value_from_int(trigger, "order", atoi(row[fi[9]]));

    // orientation
    STR_FIELD_TO_DICT_ITEM(10, trigger, "orientation");

    // timing
    STR_FIELD_TO_DICT_ITEM(11, trigger, "timing");

    // referenceNewTable
    STR_FIELD_TO_DICT_ITEM(13, trigger, "referenceNewTable");

    // referenceOldTable
    STR_FIELD_TO_DICT_ITEM(12, trigger, "referenceOldTable");

    // referenceNewTable
    STR_FIELD_TO_DICT_ITEM(15, trigger, "referenceNewRow");

    // referenceOldTable
    STR_FIELD_TO_DICT_ITEM(14, trigger, "referenceOldRow");

    // enabled
    myx_grt_dict_item_set_value_from_int(trigger, "enabled", 1);

    // add new trigger to trigger list
    myx_grt_list_item_add(triggers, trigger);
  }

  mysql_free_result(res);

  return NULL;
}

// -----------------------------------------------------------------------------------------------------------
// reverse_engineer_view_data

static const char *sql_show_view_create= "SHOW CREATE VIEW %s;";

static const char *regex_with_check_option= "WITH\\s*(\\w*)\\s*CHECK OPTION";

static MYX_GRT_VALUE *reverse_engineer_view_data(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                 MYX_GRT_VALUE *schema, MYX_GRT_VALUE *view)
{
  char *sql= myx_mysql_esc(mysql, sql_show_view_create, myx_grt_dict_item_get_as_string(view, "name"));
  MYSQL_RES *res;
  MYSQL_ROW row;

  // execute SQL
  if (myx_mysql_query(mysql, sql) ||
    !(res= mysql_store_result(mysql)))
  {
    MYX_GRT_VALUE *error= make_return_value_mysql_error(mysql, "Could not execute SHOW CREATE VIEW statement.", sql);

    g_free(sql);

    return error;
  }
  g_free(sql);

  if ((row= mysql_fetch_row(res)))
  {
    // set view data
    char *view_sql= myx_convert_dbstr_utf8(mysql, row[1], -1);
    char *check_option= get_value_from_text(view_sql, (int)strlen(view_sql), regex_with_check_option);

    myx_grt_dict_item_set_value_from_string(view, "queryExpression", view_sql);

    // check if the sql contains a check condition
    myx_grt_dict_item_set_value_from_int(view, "withCheckCondition", (check_option) ? 1 : 0);

    g_free(view_sql);
    g_free(check_option);
  }

  mysql_free_result(res);

  return NULL;
}

// -----------------------------------------------------------------------------------------------------------
// reverse_engineer_routines

static const char * sps_names[]=
{
  "Db",              // 0
  "Name",            // 1
  "Type",            // 2
  "Definer",         // 3
  "Modified",        // 4
  "Created",         // 5
  "Security_type",   // 6
  "Comment",         // 7
};
static const char ** sps_names_end=
                 sps_names + sizeof(sps_names)/sizeof(char*);

static MYX_GRT_VALUE *reverse_engineer_routines(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                                 MYX_GRT_VALUE *schema)
{
  MYX_GRT_VALUE *routines= myx_grt_dict_item_get_value(schema, "routines");
  MYSQL_RES *res;
  MYSQL_ROW row;
  MYSQL_FIELD *fields;
  unsigned int num_fields;
  int q;
  static const char *queries[]={
    "SHOW PROCEDURE STATUS;",
    "SHOW FUNCTION STATUS;"
  };

  int fi[8];

  // query procedures and functions
  for (q = 0; q < 2; q++)
  {
    if (myx_mysql_query(mysql, queries[q]) ||
        !(res= mysql_store_result(mysql)))
    {
      return make_return_value_mysql_error(mysql, "Could not get routines.", queries[q]);
    }
    else
    {
      //Depending on the version of the server there might be different columns
      num_fields= mysql_num_fields(res);
      fields= mysql_fetch_fields(res);
      
      build_field_subst(sps_names, sps_names_end,
                        fields, fields+num_fields, fi);
      
      while ((row= mysql_fetch_row(res)))
      {
        MYX_GRT_VALUE *routine;

        //Ignore SPs from wrong DB
        if (fi[0] == -1 ? 1 : (strcmp2(row[fi[0]], myx_grt_dict_item_get_as_string(schema, "name")) != 0))
          continue;

        routine= myx_grt_dict_new(NULL, "db.mysql.Routine");

        // _id
        myx_grt_dict_generate_id(routine);

        // name
        STR_FIELD_TO_DICT_ITEM(1, routine, "name");
        STR_FIELD_TO_DICT_ITEM(1, routine, "oldName");

        // owner
        myx_grt_dict_item_set_value_from_string(routine, "owner", myx_grt_dict_item_get_as_string(schema, "_id"));

        // add to routine list
        myx_grt_list_item_add(routines, routine);
        myx_grt_value_release(routine);

        
        // procedureType (PROCEDURE || FUNCTION)
        STR_FIELD_TO_DICT_ITEM(2, routine, "routineType");

        // security (DEFINER || INVOKER)
        STR_FIELD_TO_DICT_ITEM(6, routine, "security");

        // comment
        STR_FIELD_TO_DICT_ITEM(7, routine, "comment");

        reverse_engineer_routine_data(mysql, catalog, schema, routine);
      }
      mysql_free_result(res);
    }
  }

  return NULL;
}


// -----------------------------------------------------------------------------------------------------------
// reverse_engineer_sp_data

static const char * sp_show_create[]=
{
  "Procedure",       // 0 (col 1)
  "Function",        // 1 (col 1)
  "sql_mode",        // 2 (col 2)
  "Create Procedure",// 3 (col 3)
  "Create Function", // 4 (col 3)
};
static const char ** sp_show_create_end=
                 sp_show_create + sizeof(sp_show_create)/sizeof(char*);


static MYX_GRT_VALUE *reverse_engineer_routine_data(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                               MYX_GRT_VALUE *schema, MYX_GRT_VALUE *sp)
{
  MYX_GRT_VALUE *error= NULL;
  char *sqlcmd, *sp_code;
  MYSQL_RES *res;
  MYSQL_ROW row;
  MYSQL_FIELD *fields;
  unsigned int num_fields;

  //Get SP parameter
  if (strcmp2(myx_grt_dict_item_get_as_string(sp, "routineType"), "PROCEDURE") == 0)
    sqlcmd= myx_mysql_esc(mysql, "SHOW CREATE PROCEDURE %s;", myx_grt_dict_item_get_as_string(sp, "name"));
  else
    sqlcmd= myx_mysql_esc(mysql, "SHOW CREATE FUNCTION %s;", myx_grt_dict_item_get_as_string(sp, "name"));

  if (myx_mysql_query(mysql, sqlcmd) || !(res= mysql_store_result(mysql)))
  {
    MYX_GRT_VALUE *error= make_return_value_mysql_error(mysql, "Could not get routine code.", sqlcmd);

    g_free(sqlcmd);

    return error;
  }
  else
  {
    int fi[5];
    //Depending on the version of the server there might 
    //   be different columns
    num_fields= mysql_num_fields(res);
    fields= mysql_fetch_fields(res);

    build_field_subst(sp_show_create,sp_show_create_end,
                      fields,fields+num_fields,fi);

    if ((row= mysql_fetch_row(res)))
    {
      if (fi[3]!=-1)
        sp_code= myx_convert_dbstr_utf8(mysql, row[fi[3]], -1);
      else
        sp_code= fi[4]==-1 ? 0 : myx_convert_dbstr_utf8(mysql, row[fi[4]], -1);

      if (sp_code)
      {
        MYX_GRT_VALUE *sp_params= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.RoutineParam");

        // add param list to sp
        myx_grt_dict_item_set_value(sp, "params", sp_params);
        myx_grt_value_release(sp_params);


        // routineCode
        myx_grt_dict_item_set_value_from_string(sp, "routineCode", sp_code);

        // analyze params/return type
        error= reverse_engineer_routine_params(mysql, catalog, schema, sp);
      }

      g_free(sp_code);
    }
    mysql_free_result(res);
  }
  g_free(sqlcmd);

  return error;
}


// -----------------------------------------------------------------------------------------------------------
// reverse_engineer_routine_params

///////////////////////////////////////////////////////////////////////////////
/** @brief extracts only the header of a SP code

    @param sp_code      code of SP
    
    @return new allocated header or NULL
*//////////////////////////////////////////////////////////////////////////////

static char *filter_sp_header(char *sp_code)
{
  unsigned int i= 0, len= (unsigned int)strlen(sp_code);
  unsigned int b_count= 0;
  unsigned int header_start= 0;
  char *sp_header= NULL;

  if (!sp_code)
    return 0;

  while (i<len)
  {
    if (sp_code[i]=='(')
    {
      if(b_count==0)
        header_start= i;
      b_count++;
    }
    else if (sp_code[i]==')')
    {
      b_count--;

      if(b_count==0)
        break;
    }

    i++;
  }

  if (header_start>0) 
    sp_header= g_strndup(sp_code+header_start, i-header_start+1);

  g_free(sp_code);

  return sp_header;
}

//use the following regex to get all params of a SP
//(IN|OUT|INOUT)?\s?([\w\d]+)\s+([\w\d\(\)\,]+)\s*(\)|\,)
// 1: param type (IN, OUT, INOUT), IN is not existing
// 2: param name
// 3: param datatype
// 4: param datatype name
// 5: param datatype options in ()
// 6: separator , or )

#define SP_PARAMS "(IN|OUT|INOUT)?\\s?([\\w\\d]+)\\s+([\\w\\d]+)\\s*((\\([\\s\\w\\d,]+\\))?)\\s*(\\)|\\,)"

//use the following regex to get the returntype of a function
//RETURNS\s+([\w\d\(\)\,\s]+)\s+(LANGUAGE|NOT|DETERMINISTIC|SQL|COMMENT|RETURN|BEGIN)
// 1: datatype
// 2: keyword after datatype (not important)

#define SP_RETURN_TYPE "RETURNS\\s+([\\w\\d\\(\\)\\,\\s]+?)\\s+(LANGUAGE|NOT|DETERMINISTIC|SQL|COMMENT|RETURN|BEGIN)"


static MYX_GRT_VALUE *reverse_engineer_routine_params(MYSQL *mysql, MYX_GRT_VALUE *catalog, 
                                               MYX_GRT_VALUE *schema, MYX_GRT_VALUE *sp)
{
  MYX_GRT_VALUE *sp_params= myx_grt_dict_item_get_value(sp, "params");
  char *sp_code= g_strdup(myx_grt_dict_item_get_as_string(sp, "routineCode"));
  pcre *pcre_exp, *pcre_exp2;
  int erroffset, offset;
  const char *error_str;
  const char *param_type, *param_name, *param_datatype;
  int matched[60], rc;

  // compile regular expressions
  pcre_exp= pcre_compile(SP_PARAMS, PCRE_CASELESS, &error_str, &erroffset, NULL);
  if (!pcre_exp)
  {
    return make_return_value_error("Could not compile regular expression for routine retrieval.", SP_PARAMS);
  }

  pcre_exp2= pcre_compile(SP_RETURN_TYPE, PCRE_CASELESS, &error_str, &erroffset, NULL);
  if (!pcre_exp)
  {
    pcre_free(pcre_exp);
    return make_return_value_error("Could not compile regular expression for routine retrieval.", SP_RETURN_TYPE);
  }

  //Get function return type
  if (strcmp2(myx_grt_dict_item_get_as_string(sp, "routineType"), "FUNCTION") == 0)
  {
    const char *return_type;
    int matched[60], rc;

    if((rc= pcre_exec(pcre_exp2, NULL, sp_code, (int)strlen(sp_code), 0, 0, matched, sizeof(matched)/sizeof(*matched)))>=0)
    {
      pcre_get_substring(sp_code, matched, rc, 1, &return_type);
      myx_grt_dict_item_set_value_from_string(sp, "returnDatatype", return_type);
      pcre_free_substring(return_type);
    }
  }

  //Look only at params
  sp_code= filter_sp_header(sp_code);        
  
  offset= 0;

  while((rc= pcre_exec(pcre_exp, NULL, sp_code, (int)strlen(sp_code), offset, 0, matched, sizeof(matched)/sizeof(*matched)))>=0)
  {
    MYX_GRT_VALUE *sp_param= myx_grt_dict_new(NULL, "db.mysql.RoutineParam");

    // _id
    myx_grt_dict_generate_id(sp_param);

    pcre_get_substring(sp_code, matched, rc, 1, &param_type);
    pcre_get_substring(sp_code, matched, rc, 2, &param_name);                
    pcre_get_substring(sp_code, matched, rc, 3, &param_datatype);

    // name
    myx_grt_dict_item_set_value_from_string(sp_param, "name", param_name);

    // owner
    myx_grt_dict_item_set_value_from_string(sp_param, "owner", myx_grt_dict_item_get_as_string(sp, "_id"));

    // add param to param list
    myx_grt_list_item_add(sp_params, sp_param);
    myx_grt_value_release(sp_param);


    // datatype
    myx_grt_dict_item_set_value_from_string(sp_param, "datatype", param_datatype);

    // paramType
    if (strcmp2(param_type, "")==0)
      myx_grt_dict_item_set_value_from_string(sp_param, "paramType", "IN");
    else
      if (g_ascii_strncasecmp(param_type, "OUT", 3)==0)
        myx_grt_dict_item_set_value_from_string(sp_param, "paramType", "OUT");
      else
        if (g_ascii_strncasecmp(param_type, "INOUT", 5)==0)
          myx_grt_dict_item_set_value_from_string(sp_param, "paramType", "INOUT");
        else
          myx_grt_dict_item_set_value_from_string(sp_param, "paramType", "IN");

    pcre_free_substring(param_type);
    pcre_free_substring(param_name);
    pcre_free_substring(param_datatype);

    //Move offset
    offset= matched[1];
  }

  g_free(sp_code);
  pcre_free(pcre_exp2);
  pcre_free(pcre_exp);

  return NULL;
}

// -----------------------------------------------------------------------------------------------------------
// reverse_engineer_update_foreign_keys_references

void reverse_engineer_update_foreign_keys_references(MYX_GRT_VALUE *catalog)
{
  MYX_GRT_VALUE *schemata= myx_grt_dict_item_get_value(catalog, "schemata");
  unsigned int i;

  // do for all schemata
  for (i= 0; i < myx_grt_list_item_count(schemata); i++)
  {
    MYX_GRT_VALUE *schema= myx_grt_list_item_get(schemata, i);
    MYX_GRT_VALUE *tables= myx_grt_dict_item_get_value(schema, "tables");
    unsigned int j;

    // do for all tables
    for (j= 0; j < myx_grt_list_item_count(tables); j++)
    {
      MYX_GRT_VALUE *fks= myx_grt_dict_item_get_value(myx_grt_list_item_get(tables, j), "foreignKeys");
      unsigned int k;

      // do for all foreign keys
      for (k= 0; k < myx_grt_list_item_count(fks); k++)
      {
        MYX_GRT_VALUE *fk= myx_grt_list_item_get(fks, k);
        const char *ref_schema_name= myx_grt_dict_item_get_as_string(fk, "referedTableSchemaName");
        MYX_GRT_VALUE *ref_schema;

        // get the refered schema
        if (ref_schema_name && ref_schema_name[0] && 
          (strcmp(myx_grt_dict_item_get_as_string(schema, "name"), ref_schema_name) != 0))
          ref_schema= myx_grt_list_item_get_by_object_name(schemata, ref_schema_name);
        else
          ref_schema= schema;
        
        if (ref_schema)
        {
          const char *ref_table_name= myx_grt_dict_item_get_as_string(fk, "referedTableName");

          // get the refered table
          MYX_GRT_VALUE *ref_table= myx_grt_list_item_get_by_object_name(myx_grt_dict_item_get_value(ref_schema, "tables"),
              ref_table_name);
          if (ref_table)
          {
            MYX_GRT_VALUE *ref_col_names= myx_grt_dict_item_get_value(fk, "referedColumnNames");
            MYX_GRT_VALUE *ref_table_cols= myx_grt_dict_item_get_value(ref_table, "columns");
            unsigned int l;

            MYX_GRT_VALUE *ref_cols= myx_grt_dict_item_get_value(fk, "referedColumns");
            if (!ref_cols)
            {
              ref_cols= myx_grt_list_new(MYX_STRING_VALUE, "db.mysql.Column");
              myx_grt_dict_item_set_value(fk, "referedColumns", ref_cols);
              myx_grt_value_release(ref_cols);
            }

            // set the table reference in the fk
            myx_grt_dict_item_set_value_from_string(fk, "referedTable", myx_grt_dict_id_item_as_string(ref_table));

            for (l= 0; l < myx_grt_list_item_count(ref_col_names); l++)
            {
              MYX_GRT_VALUE *ref_col= myx_grt_list_item_get_by_object_name(ref_table_cols, 
                myx_grt_list_item_get_as_string(ref_col_names, l));

              if (ref_col)
              {
                // add column reference to the fk column list
                myx_grt_list_item_add_as_string(ref_cols, myx_grt_dict_id_item_as_string(ref_col));
              }
            }
          }
        }
      }
    }
  }
}

// -----------------------------------------------------------------------------------------------------------
// get_character_sets

static const char * show_character_set_fields[]=
{
  "Charset",              // 0
  "Description",          // 1
  "Default collation",    // 2
  "Maxlen"                // 3
};
static const char ** show_character_set_fields_end=
             show_character_set_fields + sizeof(show_character_set_fields)/sizeof(char*);

/*static const char * show_collations_fields[]=
{
  "Collation",             // 0
  "Charset",               // 1
  "Id",                    // 2
  "Default",               // 3
  "Compiled",              // 4
  "Sortlen"                // 5
};
static const char ** show_collations_fields_end=
             show_collations_fields + sizeof(show_collations_fields)/sizeof(char*);*/

static const char *sql_show_character_sets= "SHOW CHARACTER SET";
static const char *sql_show_collations= "SHOW COLLATION LIKE '%s%%'";

/**
 ****************************************************************************
 * @brief Returns all character sets of a given database
 *
 *   Connects to a MySQL server using the given connection parameters defined
 * in a GRT value and retrieves the list of available character sets
 *
 * @param param the connection information stored in a GRT value
 * @param data buildin module private pointer to the GRT struct
 * 
 * @return Returns the list of character sets in a GRT module function return value 
 *****************************************************************************/
MYX_GRT_VALUE *get_character_sets(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *value= NULL;
  MYSQL *mysql;
  MYSQL_RES *res;
  MYSQL_ROW row;

  mysql= grt_mysql_connect(param, &value);
  if (!mysql)
  {
    // if the connection was not successful, return the error GRT value from connect_mysql()
    return value;
  }

  if (myx_mysql_query(mysql, sql_show_character_sets) || !(res= mysql_store_result(mysql)))
  {
    MYX_GRT_VALUE *error= make_return_value_mysql_error(mysql, "Could not get the list character sets.", 
      sql_show_character_sets);

    return error;
  }

  // loop over resultset
  value= myx_grt_list_new(MYX_DICT_VALUE, "db.CharacterSet");
  while ((row= mysql_fetch_row(res)))
  {
    int fi[5];
    MYSQL_FIELD *fields;
    int num_fields;
    unsigned long *lengths;

    char *character_set_name= myx_convert_dbstr_utf8(mysql, row[0], -1);
    MYX_GRT_VALUE *character_set= myx_grt_dict_new_obj(grt, "db.CharacterSet", character_set_name, "", "");
    MYX_GRT_VALUE *collations= myx_grt_dict_item_get_value(character_set, "collations");

    //Depending on the version of the server there might be different columns
    num_fields= mysql_num_fields(res);
    fields= mysql_fetch_fields(res);
    lengths = mysql_fetch_lengths(res);

    build_field_subst(show_character_set_fields, show_character_set_fields_end,
      fields, fields + num_fields, fi);

    // description
    STR_FIELD_TO_DICT_ITEM(1, character_set, "description");

    // defaultCollation
    STR_FIELD_TO_DICT_ITEM(2, character_set, "defaultCollation");


    // add collations
    if (collations)
    {
      MYSQL_RES *res2;
      MYSQL_ROW row2;

      char *sql= g_strdup_printf(sql_show_collations, character_set_name);

      if (myx_mysql_query(mysql, sql) || !(res2= mysql_store_result(mysql)))
      {
        // ignore error
      }
      else
      {
        while ((row2= mysql_fetch_row(res2)))
        {
          char *collation_name= myx_convert_dbstr_utf8(mysql, row2[0], -1);

          myx_grt_list_item_add_as_string(collations, collation_name);
          g_free(collation_name);
        }
      }

      g_free(sql);
    }


    myx_grt_list_item_add(value, character_set);

    g_free(character_set_name);
  }
  myx_mysql_close(mysql);

  return make_return_value(value);
}

#undef STR_FIELD_TO_DICT_ITEM
