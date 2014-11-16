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
#include <myx_sql_parser_public_interface.h>

#include "myx_grt_mysql_transformation.h"
#include "myx_util_functions.h"
#include "myx_grt_mysql.h"

static void generate_sql_create_statements_asset(MYX_GRT_VALUE *asset, int include_drop, void *data);
static void generate_sql_create_statements_assets(MYX_GRT_VALUE *schema, const char *assets_name, int include_drop, void *data);
MYX_GRT_VALUE * execute_sql_statements(MYX_GRT_VALUE *param, void *data);

static char *get_sql_create_script_assets(MYX_GRT_VALUE *schema, const char *assets_name);

static char * get_sql_create_schema(MYX_GRT *grt, MYX_GRT_VALUE *schema);
static char * get_sql_use_schema(MYX_GRT *grt, MYX_GRT_VALUE *schema);
static char * get_sql_create_table(MYX_GRT *grt, MYX_GRT_VALUE *table);
static char * get_sql_create_view(MYX_GRT *grt, MYX_GRT_VALUE *view);
static char * get_sql_create_routine(MYX_GRT *grt, MYX_GRT_VALUE *routine);

static char * get_sql_drop_schema(MYX_GRT *grt, MYX_GRT_VALUE *schema);
static char * get_sql_drop_table(MYX_GRT *grt, MYX_GRT_VALUE *table);
static char * get_sql_drop_view(MYX_GRT *grt, MYX_GRT_VALUE *view);
static char * get_sql_drop_routine(MYX_GRT *grt, MYX_GRT_VALUE *routine);

static char * comment_out(char *sql);

static int process_split_sql_commands(const char *sql, void *user_data);


// --------------------------------------------------------------------------
// module registration functions

MYX_GRT_MODULE* myx_register_builtin_grt_module_transformation_mysql(MYX_GRT *grt)
{
  return myx_grt_module_register_builtin(grt, &grt_module_transformation_mysql, grt);
}


MYX_GRT_VALUE *generate_sql_create_statements(MYX_GRT_VALUE *param, void *data)
{
  //MYX_GRT *grt= (MYX_GRT *) data;
  MYX_GRT_VALUE *catalog= NULL;
  MYX_GRT_VALUE *options= NULL;

  if (!param)
    return myx_grt_function_create_error_result("You have to submit one object as parameter.", NULL);

  if ((myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) >= 1))
    catalog= myx_grt_list_item_get(param, 0);
  else if ( (myx_grt_value_get_type(param) == MYX_DICT_VALUE) &&
    (strcmp2(myx_grt_dict_struct_get_name(param), "db.mysql.Catalog") == 0) )
    catalog= param;

  if ((myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) >= 2))
    options= myx_grt_list_item_get(param, 1);

  if (!catalog)
    return myx_grt_function_create_error_result("The submitted parameter has to be a dict value of the struct db.mysql.catalog.", NULL);
  else
  {
    MYX_GRT_VALUE *schemata= myx_grt_dict_item_get_value(catalog, "schemata");
    unsigned int i;

    for (i= 0; i < myx_grt_list_item_count(schemata); i++)
    {
      MYX_GRT_VALUE *schema= myx_grt_list_item_get(schemata, i);
      int include_drop= 0;

      if (options && (strcmp2(myx_grt_dict_item_get_as_string(options, "KeepSchema"), "yes") != 0))
        generate_sql_create_statements_asset(schema, 1, data);
      else
      {
        generate_sql_create_statements_asset(schema, 0, data);
        include_drop= 1;
      }

      if (options && (strcmp2(myx_grt_dict_item_get_as_string(options, "GenerateUseSchemaCommand"), "yes") == 0)) {
        char *sql= g_strdup(myx_grt_dict_item_get_as_string(schema, "sql"));

        sql= str_g_append(sql, _br);

        sql= str_g_append_and_free(sql, get_sql_use_schema(data, schema));

        myx_grt_dict_item_set_value_from_string(schema, "sql", sql);

        g_free(sql);
      }

      // get the schema assets
      generate_sql_create_statements_assets(schema, "tables", include_drop, data);
      generate_sql_create_statements_assets(schema, "views", include_drop, data);
      generate_sql_create_statements_assets(schema, "routines", include_drop, data);
    }
  }

  return myx_grt_function_create_result(NULL);
}

MYX_GRT_VALUE * execute_sql_statements(MYX_GRT_VALUE *param, void *data)
{
  // to be implemented
  return myx_grt_function_create_result(NULL);
}

static void generate_sql_create_statements_assets(MYX_GRT_VALUE *schema, const char *assets_name, int include_drop, void *data)
{
  MYX_GRT_VALUE *assets= myx_grt_dict_item_get_value(schema, assets_name);
  unsigned int i;

  if (assets)
  {
    for (i= 0; i < myx_grt_list_item_count(assets); i++)
    {
      MYX_GRT_VALUE *asset= myx_grt_list_item_get(assets, i);

      generate_sql_create_statements_asset(asset, include_drop, data);
    }
  }
}

static void generate_sql_create_statements_asset(MYX_GRT_VALUE *asset, int include_drop, void *data)
{
  MYX_GRT_VALUE *asset_create_string;
  char *sql= NULL;

  if (include_drop)
  {
    MYX_GRT_VALUE *asset_drop_string=
      myx_grt_function_extract_value_from_result(
        get_sql_drop(asset, data));

    sql= g_strdup(myx_grt_value_as_string(asset_drop_string));
    sql= str_g_append(sql, _br);

    myx_grt_value_release(asset_drop_string);
  }

  asset_create_string=
      myx_grt_function_extract_value_from_result(
        get_sql_create(asset, data));

  sql= str_g_append(sql, myx_grt_value_as_string(asset_create_string));

  myx_grt_dict_item_set_value_from_string(asset, "sql", sql);

  g_free(sql);

  myx_grt_value_release(asset_create_string);
}

static char *get_sql_create_script_assets(MYX_GRT_VALUE *schema, const char *assets_name)
{
  char *sql= NULL;
  MYX_GRT_VALUE *assets= myx_grt_dict_item_get_value(schema, assets_name);
  unsigned int i;

  for (i= 0; i < myx_grt_list_item_count(assets); i++)
  {
    MYX_GRT_VALUE *asset= myx_grt_list_item_get(assets, i);

    sql= str_g_append(sql, myx_grt_dict_item_get_as_string(asset, "sql"));
    sql= str_g_append(sql, _br _br);
  }

  return sql;
}

MYX_GRT_VALUE *get_sql_script(MYX_GRT_VALUE *param, void *data)
{
//  MYX_GRT *grt= (MYX_GRT *) data;
  MYX_GRT_VALUE *result= NULL;
  MYX_GRT_VALUE *catalog= NULL;
  MYX_GRT_VALUE *options= NULL;
  char *sql;

  if (!param)
    return myx_grt_function_create_error_result("You have to submit one object as parameter.", NULL);

  if ((myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) >= 1))
    catalog= myx_grt_list_item_get(param, 0);
  else if ( (myx_grt_value_get_type(param) == MYX_DICT_VALUE) &&
    (strcmp2(myx_grt_dict_struct_get_name(param), "db.mysql.Catalog") == 0) )
    catalog= param;

  if ((myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) >= 2))
    options= myx_grt_list_item_get(param, 1);

  if (!catalog)
    return myx_grt_function_create_error_result("The submitted parameter has to be a dict value of the struct db.mysql.catalog.", NULL);
  else
  {
    MYX_GRT_VALUE *schemata= myx_grt_dict_item_get_value(catalog, "schemata");
    unsigned int i;
    MYX_GRT_VALUE *header, *footer;
    
    // get header
    header= myx_grt_function_extract_value_from_result(get_script_header(options, data));
    sql= g_strdup(myx_grt_value_as_string(header));
    myx_grt_value_release(header);

    for (i= 0; i < myx_grt_list_item_count(schemata); i++)
    {
      char *sql_part;
      MYX_GRT_VALUE *schema= myx_grt_list_item_get(schemata, i);

      sql= str_g_append(sql, myx_grt_dict_item_get_as_string(schema, "sql"));
      sql= str_g_append(sql, _br);


      // tables
      sql_part= get_sql_create_script_assets(schema, "tables");
      if (sql_part && sql_part[0])
      {
        sql= str_g_append(sql, "-- -------------------------------------" _br 
          "-- Tables" _br _br);

        sql= str_g_append_and_free(sql, sql_part);
        sql= str_g_append(sql, _br _br);
      }
      else
        g_free(sql_part);

      // views
      sql_part= get_sql_create_script_assets(schema, "views");
      if (sql_part && sql_part[0])
      {
        sql= str_g_append(sql, "-- -------------------------------------" _br 
          "-- Views" _br _br);

        sql= str_g_append_and_free(sql, sql_part);
        sql= str_g_append(sql, _br _br);
      }
      else
        g_free(sql_part);


      sql_part= get_sql_create_script_assets(schema, "routines");
      if (sql_part && sql_part[0])
      {
        sql= str_g_append(sql, "-- -------------------------------------" _br
          "-- Routines" _br _br);

        sql= str_g_append_and_free(sql, sql_part);
        sql= str_g_append(sql, _br _br);
      }
      else
        g_free(sql_part);
    }

    // get footer
    footer= myx_grt_function_extract_value_from_result(get_script_footer(options, data));
    sql= str_g_append(sql, myx_grt_value_as_string(footer));
    myx_grt_value_release(footer);
  }

  result= myx_grt_function_create_result(myx_grt_value_from_string(sql));
  g_free(sql);

  return result;
}

MYX_GRT_VALUE *get_sql_create(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *) data;
  MYX_GRT_VALUE *result= NULL;
  const char *struct_name;
  char *sql= NULL;

  if (!param)
    return myx_grt_function_create_error_result("You have to submit one object as parameter.", NULL);

  if (myx_grt_value_get_type(param) != MYX_DICT_VALUE)
    return myx_grt_function_create_error_result("The submitted parameter has to be a dict value.", NULL);

  struct_name= myx_grt_dict_struct_get_name(param);

  if (strcmp2(struct_name, "db.mysql.Schema") == 0)
    sql= get_sql_create_schema(grt, param);
  else if (strcmp2(struct_name, "db.mysql.Table") == 0)
    sql= get_sql_create_table(grt, param);
  else if (strcmp2(struct_name, "db.mysql.View") == 0)
    sql= get_sql_create_view(grt, param);
  else if (strcmp2(struct_name, "db.mysql.Routine") == 0)
    sql= get_sql_create_routine(grt, param);
  else
    return myx_grt_function_create_error_result("The struct of the submitted dict is of a supported type.", NULL);

  result= myx_grt_function_create_result(myx_grt_value_from_string(sql));
  g_free(sql);

  return result;
}

MYX_GRT_VALUE *get_sql_drop(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *) data;
  MYX_GRT_VALUE *result= NULL;
  const char *struct_name;
  char *sql= NULL;

  if (!param)
    return myx_grt_function_create_error_result("You have to submit one object as parameter.", NULL);

  if (myx_grt_value_get_type(param) != MYX_DICT_VALUE)
    return myx_grt_function_create_error_result("The submitted parameter has to be a dict value.", NULL);

  struct_name= myx_grt_dict_struct_get_name(param);

  if (strcmp2(struct_name, "db.mysql.Schema") == 0)
    sql= get_sql_drop_schema(grt, param);
  else if (strcmp2(struct_name, "db.mysql.Table") == 0)
    sql= get_sql_drop_table(grt, param);
  else if (strcmp2(struct_name, "db.mysql.View") == 0)
    sql= get_sql_drop_view(grt, param);
  else if (strcmp2(struct_name, "db.mysql.Routine") == 0)
    sql= get_sql_drop_routine(grt, param);
  else
    return myx_grt_function_create_error_result("The struct of the submitted dict is of a supported type.", NULL);

  result= myx_grt_function_create_result(myx_grt_value_from_string(sql));
  g_free(sql);

  return result;    
}

static char * get_sql_create_schema(MYX_GRT *grt, MYX_GRT_VALUE *schema)
{
  char *quote_char= g_strdup("`");
  const char *schema_name= myx_grt_dict_item_get_as_string(schema, "name");
  const char *charset= myx_grt_dict_item_get_as_string(schema, "defaultCharacterSetName");
  const char *collation= myx_grt_dict_item_get_as_string(schema, "defaultCollationName");

  char *sql= g_strdup_printf(
      "CREATE DATABASE IF NOT EXISTS %s%s%s" _br,
      quote_char, schema_name, quote_char);

  if ((charset) && (charset[0]))
  {
    int skip_collation= 0;

    MYX_GRT_VALUE *catalog= myx_grt_dict_item_get_reference_value(grt, schema, "owner");
    if (catalog)
    {
      MYX_GRT_VALUE *version= myx_grt_dict_item_get_value(catalog, "version");
      if (version)
      {
        if ((myx_grt_dict_item_get_as_int(version, "major") == 4) &&
          (myx_grt_dict_item_get_as_int(version, "minor") == 0))
          skip_collation= 1;
      }
    }

    if (!skip_collation)
    {
      sql = str_g_append_and_free(sql, g_strdup_printf("  CHARACTER SET %s", charset));

      if ((collation) && (collation[0]))
        sql = str_g_append_and_free(sql, g_strdup_printf(" COLLATE %s" _br, collation));
      else
        sql = str_g_append(sql, _br);
    }
  }

  // remove last \n
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  sql[strlen(sql)-2]= 0;
#else
  sql[strlen(sql)-1]= 0;
#endif
  sql= str_g_append(sql, ";");

  g_free(quote_char);

  if (myx_grt_dict_item_get_as_int(schema, "commentedOut") == 1)
    sql= comment_out(sql);

  return sql;
}

static char * get_sql_use_schema(MYX_GRT *grt, MYX_GRT_VALUE *schema)
{
  char *quote_char= g_strdup("`");
  const char *schema_name= myx_grt_dict_item_get_as_string(schema, "name");

  char *sql= g_strdup_printf(
      "USE %s%s%s;",
      quote_char, schema_name, quote_char);

  return sql;
}

static char * get_owner_name(MYX_GRT *grt, MYX_GRT_VALUE *obj, const char *quote_char)
{
  MYX_GRT_VALUE *owner= myx_grt_dict_item_get_reference_value(grt, obj, "owner");

  if (owner)
  {
    const char *owner_name= myx_grt_dict_item_get_as_string(owner, "name");

    return g_strdup_printf("%s%s%s.",
      quote_char, owner_name, quote_char);
  }
  else
    return g_strdup("");
}

static char * get_sql_create_table(MYX_GRT *grt, MYX_GRT_VALUE *table)
{
  char *quote_char= g_strdup("`");
  unsigned int i;
  const char *table_name= myx_grt_dict_item_get_as_string(table, "name");
  char *quoted_schema_name= get_owner_name(grt, table, quote_char);
  MYX_GRT_VALUE *columns= myx_grt_dict_item_get_value(table, "columns");
  MYX_GRT_VALUE *pk= myx_grt_dict_item_get_reference_value(grt, table, "primaryKey");
  MYX_GRT_VALUE *indices= myx_grt_dict_item_get_value(table, "indices");
  MYX_GRT_VALUE *fks= myx_grt_dict_item_get_value(table, "foreignKeys");
  char *sql= NULL;
  char *sql_cols= NULL;
  char *sql_pk= NULL;
  char *sql_indices= NULL;
  char *sql_fk= NULL;
  const char *opt_engine= myx_grt_dict_item_get_as_string(table, "tableEngine");
  //const char *opt_next_auto_inc= myx_grt_dict_item_get_as_string(table, "nextAutoInc");
  const char *opt_password= myx_grt_dict_item_get_as_string(table, "password");
  int opt_delay_key_write= myx_grt_dict_item_get_as_int(table, "delayKeyWrite");
  const char *opt_charset= myx_grt_dict_item_get_as_string(table, "defaultCharacterSetName");
  const char *opt_collation= myx_grt_dict_item_get_as_string(table, "defaultCollationName");
  const char *opt_comment= myx_grt_dict_item_get_as_string(table, "tableComment");
  const char *opt_merge_union= myx_grt_dict_item_get_as_string(table, "mergeUnion");
  const char *opt_merge_insert= myx_grt_dict_item_get_as_string(table, "mergeInsert");
  const char *opt_data_dir= myx_grt_dict_item_get_as_string(table, "tableDataDir");
  const char *opt_index_dir= myx_grt_dict_item_get_as_string(table, "tableIndexDir");
  const char *opt_pack_keys= myx_grt_dict_item_get_as_string(table, "packKeys");
  const char *opt_raid_type= myx_grt_dict_item_get_as_string(table, "raidType");
  const char *opt_raid_chunks= myx_grt_dict_item_get_as_string(table, "raidChunks");
  const char *opt_raid_chunk_size= myx_grt_dict_item_get_as_string(table, "raidChunkSize");
  int opt_checksum= myx_grt_dict_item_get_as_int(table, "checksum");
  const char *opt_row_format= myx_grt_dict_item_get_as_string(table, "rowFormat");
  const char *opt_avg_row_length= myx_grt_dict_item_get_as_string(table, "avgRowLength");
  const char *opt_min_rows= myx_grt_dict_item_get_as_string(table, "minRows");
  const char *opt_max_rows= myx_grt_dict_item_get_as_string(table, "maxRows");
  int is_integer= 0;
  int is_float= 0;

  // build create table sql
  sql= g_strdup_printf(
    "CREATE TABLE %s%s%s%s (" _br, 
    quoted_schema_name,
    quote_char, table_name, quote_char);

  // --------------------------------------------------------------------------------------------------------------------
  // get columns sql
  for (i= 0; i < myx_grt_list_item_count(columns); i++)
  {
    MYX_GRT_VALUE *column= myx_grt_list_item_get(columns, i);
    MYX_GRT_VALUE *column_flags= myx_grt_dict_item_get_value(column, "flags");
    const char *is_nullable= (myx_grt_dict_item_get_as_int(column, "isNullable") == 1) ? "NULL" : "NOT NULL";
    const char *datatype= myx_grt_dict_item_get_as_string(column, "datatypeName");
    const char *datatype_explicit_params= myx_grt_dict_item_get_as_string(column, "datatypeExplicitParams");
    const char *column_charset= myx_grt_dict_item_get_as_string(column, "characterSetName");
    const char *column_collation= myx_grt_dict_item_get_as_string(column, "collationName");
    char *datatype_definition;
    const char *default_value;
    int default_value_is_null;
    char *column_comment;
    char *column_length= myx_grt_dict_item_get_formated_as_string(column, "length");
    char *column_precision= myx_grt_dict_item_get_formated_as_string(column, "precision");
    char *column_scale= myx_grt_dict_item_get_formated_as_string(column, "scale");
    unsigned int j;
    int charset_defined= 0;

    // if there is an explicit parameter definition for the datatype, use it
    if (datatype_explicit_params && datatype_explicit_params[0])
      datatype_definition= g_strdup_printf("%s%s", datatype, datatype_explicit_params);
    else
    {
      // append length / precision / scale to datatype definition
      if ((strcmp2(datatype, "CHAR") == 0) ||
         (strcmp2(datatype, "VARCHAR") == 0) ||
         (strcmp2(datatype, "VARBINARY") == 0) ||
         (strcmp2(datatype, "BINARY") == 0))
      {
        datatype_definition= g_strdup_printf("%s(%s)", 
          datatype, column_length);
      }
      else if ((strcmp2(datatype, "TINYINT") == 0) ||
         (strcmp2(datatype, "SMALLINT") == 0) ||
         (strcmp2(datatype, "MEDIUMINT") == 0) ||
         (strcmp2(datatype, "INT") == 0) ||
         (strcmp2(datatype, "INTEGER") == 0) ||
         (strcmp2(datatype, "BIGINT") == 0))
      {
        is_integer= 1;

        if (strcmp2(column_precision, "0") != 0)
        {
          datatype_definition= g_strdup_printf("%s(%s)", 
            datatype, 
            column_precision);
        }
        else
          datatype_definition= g_strdup(datatype);
      }
      else if ((strcmp2(datatype, "REAL") == 0) ||
         (strcmp2(datatype, "DOUBLE") == 0) ||
         (strcmp2(datatype, "FLOAT") == 0) ||
         (strcmp2(datatype, "DECIMAL") == 0) ||
         (strcmp2(datatype, "NUMERIC") == 0))
      {
        is_float= 1;

        // if the scale holds a negative value like -1, do not use it
        if ((column_scale[0] == '0' && column_scale[1] == 0) &&
          (column_precision[0] == '0' && column_precision[1] == 0))
          datatype_definition= g_strdup_printf("%s", datatype);
        else if (column_scale[0] != '-')
          datatype_definition= g_strdup_printf("%s(%s, %s)", 
            datatype,
            column_precision,
            column_scale);
        // if the precision holds a negative value like -1, do not use it
        else if (column_precision[0] != '-')
          datatype_definition= g_strdup_printf("%s(%s)", 
            datatype,
            column_precision);
        else
          datatype_definition= g_strdup_printf("%s", datatype);
      }
      else
        datatype_definition= g_strdup(datatype);
    }

    // add Character Set and Collation for CHAR | VARCHAR | TEXT columns
    if (((strcmp2(datatype, "CHAR") == 0) ||
        (strcmp2(datatype, "VARCHAR") == 0) ||
        (strcmp2(datatype, "TEXT") == 0)) && 
        (column_charset))
    {
      datatype_definition= str_g_append_and_free(datatype_definition, 
         g_strdup_printf(" CHARACTER SET %s", column_charset));

      charset_defined= 1;

      // also add collation if it is defined
      if (column_collation)
      {
        datatype_definition= str_g_append_and_free(datatype_definition,
          g_strdup_printf(" COLLATE %s", column_collation));
      }
    }

    // add column datatype flags
    if(column_flags != NULL)
      for (j= 0; j < myx_grt_list_item_count(column_flags); j++)
      {
        MYX_GRT_VALUE *flag= myx_grt_list_item_get(column_flags, j);

        if (charset_defined &&
          g_ascii_strncasecmp(myx_grt_value_as_string(flag), "BINARY", 7) == 0)
          continue;

        datatype_definition= str_g_append_and_free(datatype_definition, 
          g_strdup_printf(" %s", myx_grt_value_as_string(flag)));
      }

    sql_cols= str_g_append_and_free(sql_cols, g_strdup_printf("  %s%s%s %s %s", 
      quote_char,
      myx_grt_dict_item_get_as_string(column, "name"), 
      quote_char,
      datatype_definition,
      is_nullable));

    // add default value
    default_value= myx_grt_dict_item_get_as_string(column, "defaultValue");
    default_value_is_null= myx_grt_dict_item_get_as_int(column, "defaultValueIsNull");
    if (default_value && default_value[0] && !default_value_is_null && 
      !((is_integer || is_float) && (strcmp(default_value, "''") == 0)))
    {
      sql_cols= str_g_append_and_free(sql_cols, g_strdup_printf(" DEFAULT %s", default_value));
    }

    // add column auto increment
    if (myx_grt_dict_item_get_as_int(column, "autoIncrement") == 1)
      sql_cols= str_g_append(sql_cols, " AUTO_INCREMENT");

    // add column comment
    column_comment= g_strdup(myx_grt_dict_item_get_as_string(column, "columnComment"));
    if (column_comment && column_comment[0])
    {
      column_comment= str_g_replace(column_comment, "'", "\\'");
      sql_cols= str_g_append_and_free(sql_cols, g_strdup_printf(" COMMENT '%s'", column_comment));
    }

    sql_cols= str_g_append(sql_cols, "," _br);

    g_free(datatype_definition);
    g_free(column_comment);
    g_free(column_length);
    g_free(column_precision);
    g_free(column_scale);
  }

  // --------------------------------------------------------------------------------------------------------------------
  // get primary key sql
  if (pk)
  {
    MYX_GRT_VALUE *pk_columns= myx_grt_dict_item_get_value(pk, "columns");
    char *sql_pk_cols= NULL;

    for (i= 0; i < myx_grt_list_item_count(pk_columns); i++)
    {
      MYX_GRT_VALUE *index_column= myx_grt_list_item_get(pk_columns, i);
      MYX_GRT_VALUE *column= myx_grt_dict_item_get_reference_value(grt, index_column, "referedColumn");
      int index_column_length= myx_grt_dict_item_get_as_int(index_column, "columnLength");

      if (sql_pk_cols)
        sql_pk_cols= str_g_append(sql_pk_cols, ", ");

      sql_pk_cols= str_g_append_and_free(sql_pk_cols, g_strdup_printf("%s%s%s", 
          quote_char,
          myx_grt_dict_item_get_as_string(column, "name"),
          quote_char));

      // add index column length if set
      if (index_column_length > 0)
        sql_pk_cols= str_g_append_and_free(sql_pk_cols, g_strdup_printf("(%d)", index_column_length));
    }

    if (sql_pk_cols)
      sql_pk= str_g_append_and_free(sql_pk, g_strdup_printf("  PRIMARY KEY (%s)," _br, 
        sql_pk_cols));

    g_free(sql_pk_cols);
  }

  // --------------------------------------------------------------------------------------------------------------------
  // get indices sql
  if (indices)
  {
    for (i= 0; i < myx_grt_list_item_count(indices); i++)
    {
      MYX_GRT_VALUE *index= myx_grt_list_item_get(indices, i);
      const char *index_name= myx_grt_dict_item_get_as_string(index, "name");
      const char *unique= (myx_grt_dict_item_get_as_int(index, "unique") == 1) ? "UNIQUE " : "";
      unsigned int j;

      MYX_GRT_VALUE *index_columns= myx_grt_dict_item_get_value(index, "columns");
      char *sql_index_cols= NULL;

      if (myx_grt_dict_item_get_as_int(index, "isPrimary") == 1)
        continue;

      for (j= 0; j < myx_grt_list_item_count(index_columns); j++)
      {
        MYX_GRT_VALUE *index_col= myx_grt_list_item_get(index_columns, j);
        int column_length= myx_grt_dict_item_get_as_int(index_col, "columnLength");
        MYX_GRT_VALUE *column_ref= myx_grt_dict_item_get_reference_value(grt, index_col, "referedColumn");
        const char *column_name= myx_grt_dict_item_get_as_string(column_ref, "name");
        //MYX_GRT_VALUE *simple_datatype= myx_grt_dict_item_get_reference_value(grt, column_ref, "simpleType");

        if (sql_index_cols)
          sql_index_cols= str_g_append(sql_index_cols, ", ");

        sql_index_cols= str_g_append_and_free(sql_index_cols, g_strdup_printf("%s%s%s", 
            quote_char, column_name, quote_char));

        /*if (simple_datatype)
        {
          MYX_GRT_VALUE *group= myx_grt_dict_item_get_reference_value(grt, simple_datatype, "group");
          if (group)
          {
            const char *group_name= myx_grt_dict_name_item_as_string(group);

            // if the group_name is not string, text or blob, there must be no column_length
            if ((strcmp2(group_name, "string") != 0) && (strcmp2(group_name, "text") != 0) && 
              (strcmp2(group_name, "blob") != 0) && (column_length > 0))
              column_length= 0;

            // if the group_name is not text or blob, there has to be a column_length
            if (((strcmp2(group_name, "text") == 0) || 
              (strcmp2(group_name, "blob") == 0)) && (column_length == 0))
              column_length= 10;
          }
        }*/

        if (column_length > 0)
        {
          /*if (column_length > 1024)
            column_length= 1024;*/

          sql_index_cols= str_g_append_and_free(sql_index_cols, g_strdup_printf("(%d)", 
            column_length));
        }
      }

      sql_indices= str_g_append_and_free(sql_indices, 
        g_strdup_printf("  %sINDEX %s%s%s (%s)," _br, 
          unique,
          quote_char, index_name, quote_char,
          sql_index_cols));

      g_free(sql_index_cols);
    }
  }

  // --------------------------------------------------------------------------------------------------------------------
  // get foreign keys sql
  if (fks)
  {
    for (i= 0; i < myx_grt_list_item_count(fks); i++)
    {
      MYX_GRT_VALUE *fk= myx_grt_list_item_get(fks, i);
      const char *fk_name= myx_grt_dict_item_get_as_string(fk, "name");
      MYX_GRT_VALUE *fk_columns= myx_grt_dict_item_get_value(fk, "columns");

      MYX_GRT_VALUE *fk_ref_table= myx_grt_dict_item_get_reference_value(grt, fk, "referedTable");
      const char *fk_ref_schema_name= myx_grt_dict_item_get_as_string(fk, "referedTableSchemaName");
      const char *fk_ref_table_name= myx_grt_dict_item_get_as_string(fk, "referedTableName");

      MYX_GRT_VALUE *fk_ref_columns= myx_grt_dict_item_get_value(fk, "referedColumns");
      MYX_GRT_VALUE *fk_ref_column_names= myx_grt_dict_item_get_value(fk, "referedColumnNames");

      const char *delete_rule= myx_grt_dict_item_get_as_string(fk, "deleteRule");
      const char *update_rule= myx_grt_dict_item_get_as_string(fk, "updateRule");
      char *sql_fk_cols= NULL;
      char *sql_fk_ref_cols= NULL;

      unsigned int j;

      for (j= 0; j < myx_grt_list_item_count(fk_columns); j++)
      {
        MYX_GRT_VALUE *column= myx_grt_list_item_get_reference_value(grt, fk_columns, j);

        if (sql_fk_cols)
          sql_fk_cols= str_g_append(sql_fk_cols, ", ");

        sql_fk_cols= str_g_append_and_free(sql_fk_cols, g_strdup_printf("%s%s%s", 
            quote_char,
            myx_grt_dict_item_get_as_string(column, "name"),
            quote_char));
      }

      // if possible, use real reference to table. If the table is in a different schema, use text names
      if (fk_ref_table)
      {
        MYX_GRT_VALUE *fk_ref_schema= myx_grt_dict_item_get_reference_value(grt, fk_ref_table, "owner");

        fk_ref_schema_name= myx_grt_dict_item_get_as_string(fk_ref_schema, "name");
        fk_ref_table_name= myx_grt_dict_item_get_as_string(fk_ref_table, "name");
      }
      else
      {
        fk_ref_schema_name= myx_grt_dict_item_get_as_string(fk, "referedTableSchemaName");
        fk_ref_table_name= myx_grt_dict_item_get_as_string(fk, "referedTableName");
      }

      // if possible, use real reference to columns. If not, use the columns names stored as text
      if (fk_ref_columns && (myx_grt_list_item_count(fk_ref_columns) > 0))
      {
        for (j= 0; j < myx_grt_list_item_count(fk_ref_columns); j++)
        {
          MYX_GRT_VALUE *fk_ref_col= myx_grt_list_item_get_reference_value(grt, fk_ref_columns, j);

          if (fk_ref_col)
          {
            if (sql_fk_ref_cols)
              sql_fk_ref_cols= str_g_append(sql_fk_ref_cols, ", ");

            sql_fk_ref_cols= str_g_append_and_free(sql_fk_ref_cols, g_strdup_printf("%s%s%s", 
                quote_char,
                myx_grt_dict_item_get_as_string(fk_ref_col, "name"),
                quote_char));
          }
        }
      }
      else
      {
        for (j= 0; j < myx_grt_list_item_count(fk_ref_column_names); j++)
        {
          if (sql_fk_ref_cols)
            sql_fk_ref_cols= str_g_append(sql_fk_ref_cols, ", ");

          sql_fk_ref_cols= str_g_append_and_free(sql_fk_ref_cols, g_strdup_printf("%s%s%s", 
              quote_char,
              myx_grt_list_item_get_as_string(fk_ref_column_names, j),
              quote_char));
        }
      }

      sql_fk= str_g_append_and_free(sql_fk, 
        g_strdup_printf("  CONSTRAINT %s%s%s FOREIGN KEY %s%s%s (%s)" _br
          "    REFERENCES %s%s%s.%s%s%s (%s)", 
          quote_char, fk_name, quote_char,
          quote_char, fk_name, quote_char,
          sql_fk_cols,
          quote_char, fk_ref_schema_name, quote_char,
          quote_char, fk_ref_table_name, quote_char,
          sql_fk_ref_cols));

      if (delete_rule)
        sql_fk= str_g_append_and_free(sql_fk, g_strdup_printf(_br "    ON DELETE %s", delete_rule));

      if (update_rule)
        sql_fk= str_g_append_and_free(sql_fk, g_strdup_printf(_br "    ON UPDATE %s", update_rule));

      sql_fk= str_g_append(sql_fk, "," _br);

      g_free(sql_fk_cols);
      g_free(sql_fk_ref_cols);
    }
  }

  if (sql_cols)
    sql= str_g_append_and_free(sql, sql_cols);

  if (sql_pk)
    sql= str_g_append_and_free(sql, sql_pk);

  if (sql_indices)
    sql= str_g_append_and_free(sql, sql_indices);

  if (sql_fk)
    sql= str_g_append_and_free(sql, sql_fk);

  // remove last ,
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  sql[strlen(sql)-3]= 0;
#else
  sql[strlen(sql)-2]= 0;
#endif

  sql= str_g_append(sql, _br ")" _br);

  // add table options
  if ((opt_engine) && (opt_engine[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("ENGINE = %s" _br, opt_engine));
  else
    sql= str_g_append(sql, "ENGINE=InnoDB" _br);

  /*
  if ((opt_next_auto_inc) && (opt_next_auto_inc[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("AUTO_INCREMENT = %s" _br, opt_next_auto_inc));
  */

  if ((opt_avg_row_length) && (opt_avg_row_length[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("AVG_ROW_LENGTH = %s" _br, opt_avg_row_length));

  if (opt_checksum == 1)
    sql= str_g_append(sql, "CHECKSUM = 1" _br);

  if ((opt_comment) && (opt_comment[0]))
  {
    char *table_comment= str_g_replace(g_strdup(opt_comment), "'", "\\'");
    sql= str_g_append_and_free(sql, g_strdup_printf("COMMENT = '%s'", table_comment));
    g_free(table_comment);
  }

  if ((opt_max_rows) && (opt_max_rows[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("MAX_ROWS = %s" _br, opt_max_rows));

  if ((opt_min_rows) && (opt_min_rows[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("MIN_ROWS = %s" _br, opt_min_rows));

  if ((opt_pack_keys) && (opt_pack_keys[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("PACK_KEYS = %s" _br, opt_pack_keys));

  if ((opt_password) && (opt_password[0]))
  {
    char *table_password= str_g_replace(g_strdup(opt_password), "'", "\\'");
    sql= str_g_append_and_free(sql, g_strdup_printf("PASSWORD = '%s'" _br, table_password));
    g_free(table_password);
  }

  if (opt_delay_key_write == 1)
    sql= str_g_append(sql, "DELAY_KEY_WRITE = 1" _br);

  if ((opt_row_format) && (opt_row_format[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("ROW_FORMAT = %s" _br, opt_row_format));

  if (opt_raid_type && opt_raid_type[0] && 
    opt_raid_chunks && opt_raid_chunks[0] && 
    opt_raid_chunk_size && opt_raid_chunk_size[0])
  {
    sql= str_g_append_and_free(sql, g_strdup_printf("RAID_TYPE = %s" _br, opt_raid_type));
    sql= str_g_append_and_free(sql, g_strdup_printf("  RAID_CHUNKS = %s" _br, opt_raid_chunks));
    sql= str_g_append_and_free(sql, g_strdup_printf("  RAID_CHUNKSIZE = %s" _br, opt_raid_chunk_size));
  }

  if ((opt_merge_union) && (opt_merge_union[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("UNION = %s" _br, opt_merge_union));

  if ((opt_merge_insert) && (opt_merge_insert[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("INSERT_METHOD = %s" _br, opt_merge_insert));

  if ((opt_merge_insert) && (opt_merge_insert[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("INSERT_METHOD = %s" _br, opt_merge_insert));

  if ((opt_data_dir) && (opt_data_dir[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("DATA DIRECTORY = %s" _br, opt_data_dir));

  if ((opt_index_dir) && (opt_index_dir[0]))
    sql= str_g_append_and_free(sql, g_strdup_printf("INDEX DIRECTORY = %s" _br, opt_index_dir));

  if ((opt_charset) && (opt_charset[0]))
  {
    int skip_collation= 0;

    MYX_GRT_VALUE *schema= myx_grt_dict_item_get_reference_value(grt, table, "owner");
    if (schema)
    {
      MYX_GRT_VALUE *catalog= myx_grt_dict_item_get_reference_value(grt, schema, "owner");
      if (catalog)
      {
        MYX_GRT_VALUE *version= myx_grt_dict_item_get_value(catalog, "version");
        if (version)
        {
          if ((myx_grt_dict_item_get_as_int(version, "major") == 4) &&
            (myx_grt_dict_item_get_as_int(version, "minor") == 0))
            skip_collation= 1;
        }
      }
    }

    if (!skip_collation)
    {
      sql= str_g_append_and_free(sql, g_strdup_printf("CHARACTER SET %s", opt_charset));

      if ((opt_collation) && (opt_collation[0]))
        sql= str_g_append_and_free(sql, g_strdup_printf(" COLLATE %s" _br, opt_collation));
      else
        sql= str_g_append(sql, _br);
    }
  }

  // remove last \n
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  sql[strlen(sql)-2]= 0;
#else
  sql[strlen(sql)-1]= 0;
#endif

  sql= str_g_append(sql, ";");

  if (myx_grt_dict_item_get_as_int(table, "commentedOut") == 1)
    sql= comment_out(sql);

  g_free(quote_char);
  g_free(quoted_schema_name);

  return sql;
}

static char * get_sql_create_view(MYX_GRT *grt, MYX_GRT_VALUE *view)
{
  char *quote_char= g_strdup("`");
  const char *view_name= myx_grt_dict_item_get_as_string(view, "name");
  char *quoted_schema_name= get_owner_name(grt, view, quote_char);
  const char *queryExpression= myx_grt_dict_item_get_as_string(view, "queryExpression");
  int with_check_condition= myx_grt_dict_item_get_as_int(view, "withCheckCondition");
  MYX_GRT_VALUE *cols= myx_grt_dict_item_get_value(view, "columns");
  char *sql;
  char *col_names= NULL;
  int i;
  char *query_expr_uppercase= str_toupper(utf8_str_trim(g_strdup(queryExpression)));

  if (str_beginswith(query_expr_uppercase, "CREATE"))
  {
    sql= g_strdup_printf("%s" _br, queryExpression);
  }
  else if (cols && myx_grt_list_item_count(cols) > 0)
  {
    for (i = 0; i < myx_grt_list_item_count(cols); i++)
    {
      if (i > 0)
        col_names= str_g_append(col_names, ", ");

      col_names= str_g_append(col_names,
        myx_grt_list_item_get_as_string(cols, i));
    }

    sql= g_strdup_printf("CREATE OR REPLACE VIEW %s%s%s%s (%s) AS" _br
      "%s" _br,
      quoted_schema_name,
      quote_char, view_name, quote_char,
      col_names,
      queryExpression);
  }
  else
    sql= g_strdup_printf("CREATE OR REPLACE VIEW %s%s%s%s AS" _br
      "%s" _br,
      quoted_schema_name,
      quote_char, view_name, quote_char,
      queryExpression);

  g_free(query_expr_uppercase);

  if (with_check_condition == 1)
    sql= str_g_append(sql, "  WITH CHECK OPTION" _br);

  // remove last \n
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  sql[strlen(sql)-2]= 0;
#else
  sql[strlen(sql)-1]= 0;
#endif

  sql= str_g_append(sql, ";");

  if (myx_grt_dict_item_get_as_int(view, "commentedOut") == 1)
    sql= comment_out(sql);
    
  g_free(quote_char);
  g_free(quoted_schema_name);

  return sql;
}

static char * get_sql_create_routine(MYX_GRT *grt, MYX_GRT_VALUE *routine)
{
  char *sql= g_strdup(myx_grt_dict_item_get_as_string(routine, "routineCode"));

  if (myx_grt_dict_item_get_as_int(routine, "commentedOut") == 1)
    sql= comment_out(sql);

  return sql;
}

static char * get_sql_drop_schema(MYX_GRT *grt, MYX_GRT_VALUE *schema)
{
  char *quote_char= g_strdup("`");
  const char *schema_name= myx_grt_dict_item_get_as_string(schema, "name");

  char *sql= g_strdup_printf("DROP DATABASE IF EXISTS %s%s%s;",
      quote_char, schema_name, quote_char);

  g_free(quote_char);

  if (myx_grt_dict_item_get_as_int(schema, "commentedOut") == 1)
    sql= comment_out(sql);

  return sql;
}

static char * get_sql_drop_table(MYX_GRT *grt, MYX_GRT_VALUE *table)
{
  char *quote_char= g_strdup("`");
  const char *table_name= myx_grt_dict_item_get_as_string(table, "name");
  char *quoted_schema_name= get_owner_name(grt, table, quote_char);

  // build drop table sql
  char *sql= g_strdup_printf(
    "DROP TABLE IF EXISTS %s%s%s%s;", 
    quoted_schema_name,
    quote_char, table_name, quote_char);

  g_free(quote_char);
  g_free(quoted_schema_name);

  if (myx_grt_dict_item_get_as_int(table, "commentedOut") == 1)
    sql= comment_out(sql);

  return sql;
}

static char * get_sql_drop_view(MYX_GRT *grt, MYX_GRT_VALUE *view)
{
  char *quote_char= g_strdup("`");
  const char *view_name= myx_grt_dict_item_get_as_string(view, "name");
  char *quoted_schema_name= get_owner_name(grt, view, quote_char);

  // build drop view sql
  char *sql= g_strdup_printf(
    "DROP VIEW IF EXISTS %s%s%s%s;", 
    quoted_schema_name,
    quote_char, view_name, quote_char);

  g_free(quote_char);
  g_free(quoted_schema_name);

  if (myx_grt_dict_item_get_as_int(view, "commentedOut") == 1)
    sql= comment_out(sql);

  return sql;
}

static char * get_sql_drop_routine(MYX_GRT *grt, MYX_GRT_VALUE *routine)
{
  char *quote_char= g_strdup("`");
  const char *routine_name= myx_grt_dict_item_get_as_string(routine, "name");
  char *quoted_schema_name= get_owner_name(grt, routine, quote_char);
  const char *routine_type= myx_grt_dict_item_get_as_string(routine, "routineType");

  char *sql= g_strdup_printf("DROP %s IF EXISTS %s%s%s%s;",
    routine_type,
    quoted_schema_name,
    quote_char, routine_name, quote_char);
    
  g_free(quote_char);
  g_free(quoted_schema_name);

  if (myx_grt_dict_item_get_as_int(routine, "commentedOut") == 1)
    sql= comment_out(sql);
    
  return sql;
}

static char * comment_out(char *sql)
{
  char *sql_commented_out= g_strdup_printf("-- %s", sql);
  g_free(sql);

  sql_commented_out= str_g_replace(sql_commented_out, "\n", "\n-- ");
  
  return sql_commented_out;
}

MYX_GRT_VALUE *get_script_header(MYX_GRT_VALUE *param, void *data)
{
  //MYX_GRT *grt= (MYX_GRT *) data;
  const char *app= NULL;
  const char *script_type= NULL;
  char *sql;
  MYX_GRT_VALUE *header;
  MYX_GRT_VALUE *options= NULL;

  if (param && (myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) == 1))
    options= myx_grt_list_item_get(param, 0);
  else if (param)
    options= param;

  if (options)
  {
    app= myx_grt_dict_item_get_as_string(options, "AppName");
    script_type= myx_grt_dict_item_get_as_string(options, "ScriptType");
  }

  if (!app)
    app= "MySQL GRT Application";

  if (!script_type)
    script_type= "SQL Script";


  sql= g_strdup_printf(
      "-- ----------------------------------------------------------------------" _br
      "-- %s" _br
		  "-- %s" _br
		  "-- ----------------------------------------------------------------------" _br 
      _br
      "SET FOREIGN_KEY_CHECKS = 0;" _br _br, app, script_type);

  header= myx_grt_value_from_string(sql);

  g_free(sql);

  return myx_grt_function_create_result(header);
}

MYX_GRT_VALUE *get_script_footer(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *) data;
  MYX_GRT_VALUE *footer= myx_grt_dict_new_obj(grt, "db.DatabaseObject", "ScriptFooter", "", "");

  footer= myx_grt_value_from_string(
      "SET FOREIGN_KEY_CHECKS = 1;" _br _br
      "-- ----------------------------------------------------------------------" _br
      "-- EOF" _br _br);

  return myx_grt_function_create_result(footer);
}

int process_split_sql_commands(const char *sql, void *user_data)
{
  MYX_GRT_VALUE *sql_commands= (MYX_GRT_VALUE *) user_data;

  myx_grt_list_item_add_as_string(sql_commands, sql);

  return 1;
}

MYX_GRT_VALUE *split_sql_commands(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *sql_commands= myx_grt_list_new(MYX_STRING_VALUE, "");
  MYX_GRT_VALUE *sql;

  if (!param)
    return myx_grt_function_create_error_result("You have to submit a SQL script string as parameter.", NULL);

  if ((myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) >= 1))
    sql= myx_grt_list_item_get(param, 0);
  else
    sql= param;

  myx_process_sql_statements(myx_grt_value_as_string(sql), &process_split_sql_commands, sql_commands, MYX_SPM_DELIMS_REQUIRED);

  return myx_grt_function_create_result(sql_commands);
}

// -------------------------------------------------------------------------------------------------------

static MYX_GRT_VALUE *new_change_obj(MYX_GRT *grt, MYX_GRT_VALUE *parent, MYX_GRT_VALUE *org_obj, MYX_GRT_VALUE *mod_obj, int changed)
{
  MYX_GRT_VALUE *retval= myx_grt_dict_new(grt, "db.DatabaseSyncObject");

  //myx_grt_dict_item_set_value(retval, "dbObject", org_obj);
  //myx_grt_dict_item_set_value(retval, "modelObject", mod_obj);
  myx_grt_dict_generate_id(retval);
  myx_grt_reference_cache_add(grt, retval);
  if(org_obj != NULL)
    myx_grt_dict_item_set_value_from_string(retval, "dbObject", myx_grt_dict_item_get_as_string(org_obj, "_id"));
  if(mod_obj != NULL)
    myx_grt_dict_item_set_value_from_string(retval, "modelObject", myx_grt_dict_item_get_as_string(mod_obj, "_id"));
  if(parent != NULL)
  {
    myx_grt_dict_item_set_value_from_string(retval, "owner", myx_grt_dict_item_get_as_string(parent, "_id"));
  }

  myx_grt_dict_item_set_value(retval, "changed", myx_grt_value_from_int(changed));
  if(parent != NULL)
  {
    MYX_GRT_VALUE *child_list= myx_grt_dict_item_get_value(parent, "children");
    if(child_list == NULL)
    {
      child_list= myx_grt_list_new(MYX_ANY_VALUE, NULL);
      myx_grt_dict_item_set_value(parent, "children", child_list);
    }
    myx_grt_list_item_add(child_list, retval);
  }
  return retval;
}

static char *irrel_keys[] = {"_id", "owner", "referedColumn", "primaryKey", "sql", "routineExpandedHeights", "routineExpandedStates", NULL};

static void remove_diff_keys(MYX_GRT_VALUE *diff_list, const char **remove_list);
static void remove_irrelevant_fk_changes(MYX_GRT *grt, MYX_GRT_VALUE *existing, MYX_GRT_VALUE *changed, MYX_GRT_VALUE *diff_list);

typedef void (*make_tree_cb)(MYX_GRT *grt, MYX_GRT_VALUE *node, MYX_GRT_VALUE *os, MYX_GRT_VALUE *ms);

typedef struct
{
  const char *name;
  make_tree_cb iter_cb;
  MYX_GRT_VALUE *org_list;
  MYX_GRT_VALUE *mod_list;
} 
SUBITEM_DATA;

static void make_change_tree_for_items(MYX_GRT *grt, MYX_GRT_VALUE *node, MYX_GRT_VALUE *org_object, MYX_GRT_VALUE *mod_object, 
                                      SUBITEM_DATA *subitem_data, int subitem_data_count, char **item_irrel_keys)
{
  int changed, j, k, mcount;
  MYX_GRT_VALUE *org_sublist, *mod_sublist, *diff, *org_subobject, *mod_subobject, *change_obj;
  const char *subobject_name;

  if(item_irrel_keys == NULL)
    item_irrel_keys= irrel_keys;

  for(j= 0; j < subitem_data_count; j++)
  {
    // original sub-object list
    org_sublist= myx_grt_dict_item_get_by_path(grt, org_object, subitem_data[j].name);
    myx_grt_value_retain(org_sublist);
    myx_grt_dict_item_del(org_object, subitem_data[j].name + 1);  // +1 removes the starting "/" 
    subitem_data[j].org_list= org_sublist;
    // modified sub-object list
    mod_sublist= myx_grt_dict_item_get_by_path(grt, mod_object, subitem_data[j].name);
    myx_grt_value_retain(mod_sublist);
    myx_grt_dict_item_del(mod_object, subitem_data[j].name + 1);  // +1 removes the starting "/" 
    subitem_data[j].mod_list= mod_sublist;
  }

  // pure object diff
  diff= myx_grt_value_diff_make(grt, org_object, mod_object);
  remove_diff_keys(diff, (const char**)item_irrel_keys);
  remove_irrelevant_fk_changes(grt, org_object, mod_object, diff);

  changed= (myx_grt_list_item_count(diff) > 0) ? 1 : 0;
  myx_grt_value_release(diff);
  myx_grt_dict_item_set_value(node, "changed", myx_grt_value_from_int(changed));

  // process sub-objects lists
  for(k= 0; k < subitem_data_count; k++)
  {
    org_sublist= subitem_data[k].org_list;
    mod_sublist= subitem_data[k].mod_list;

    if((org_sublist == NULL) || (mod_sublist == NULL))
    {
      continue;
    }
restart:
    while(myx_grt_list_item_count(org_sublist) > 0)
    {
      org_subobject= myx_grt_list_item_get(org_sublist, 0);
      subobject_name= myx_grt_dict_item_get_as_string(org_subobject, "oldName");
      mcount= myx_grt_list_item_count(mod_sublist);
      for(j= 0; j < mcount; j++)
      {
        mod_subobject= myx_grt_list_item_get(mod_sublist, j);
        if(strcmp(subobject_name, myx_grt_dict_item_get_as_string(mod_subobject, "oldName")) == 0)
        {
          change_obj= new_change_obj(grt, node, org_subobject, mod_subobject, 0);
          //make_change_tree_for_schemata(grt, change_obj, org_subobject, mod_subobject);
          subitem_data[k].iter_cb(grt, change_obj, org_subobject, mod_subobject);
          myx_grt_list_item_del(org_sublist, 0);
          myx_grt_list_item_del(mod_sublist, j);
          goto restart;
        }
      }
      // no corresponding modified object
      change_obj= new_change_obj(grt, node, org_subobject, NULL, 1);
      myx_grt_list_item_del(org_sublist, 0);
    }
    // add to the change tree all objects left in modified list
    while(myx_grt_list_item_count(mod_sublist) > 0)
    {
      mod_subobject= myx_grt_list_item_get(mod_sublist, 0);
      new_change_obj(grt, node, NULL, mod_subobject, 1);
      myx_grt_list_item_del(mod_sublist, 0);
    }
  }
}

static void make_change_tree_for_routines(MYX_GRT *grt, MYX_GRT_VALUE *node, MYX_GRT_VALUE *org, MYX_GRT_VALUE *mod)
{
  static char *routine_irrel_keys[] = {"_id", "owner", "name", "routineCode", "routineType", "sql", "params", NULL};

  make_change_tree_for_items(grt, node, org, mod, NULL, 0, routine_irrel_keys);
}

static void make_change_tree_for_leaf(MYX_GRT *grt, MYX_GRT_VALUE *node, MYX_GRT_VALUE *org, MYX_GRT_VALUE *mod)
{
  make_change_tree_for_items(grt, node, org, mod, NULL, 0, NULL);
}

static void make_change_tree_for_tables(MYX_GRT *grt, MYX_GRT_VALUE *node, MYX_GRT_VALUE *os, MYX_GRT_VALUE *ms)
{
  SUBITEM_DATA data[3];
  
  data[0].name= "/columns";
  data[0].iter_cb= make_change_tree_for_leaf;
  data[0].org_list= 0;
  data[0].mod_list= 0;
  data[1].name= "/indices";
  data[1].iter_cb= make_change_tree_for_leaf;
  data[1].org_list= 0;
  data[1].mod_list= 0;
  data[2].name= "/triggers";
  data[2].iter_cb= make_change_tree_for_leaf;
  data[2].org_list= 0;
  data[2].mod_list= 0;

  make_change_tree_for_items(grt, node, os, ms, data, 3, NULL);
}

static void make_change_tree_for_views(MYX_GRT *grt, MYX_GRT_VALUE *node, MYX_GRT_VALUE *os, MYX_GRT_VALUE *ms)
{
  SUBITEM_DATA data[3];
  
  data[0].name= "/columns";
  data[0].iter_cb= make_change_tree_for_leaf;
  data[0].org_list= 0;
  data[0].mod_list= 0;

  make_change_tree_for_items(grt, node, os, ms, data, 1, NULL);
}

static void make_change_tree_for_schemata(MYX_GRT *grt, MYX_GRT_VALUE *node, MYX_GRT_VALUE *os, MYX_GRT_VALUE *ms)
{
  SUBITEM_DATA data[3];
  
  data[0].name= "/tables";
  data[0].iter_cb= make_change_tree_for_tables;
  data[0].org_list= 0;
  data[0].mod_list= 0;
  
  data[1].name= "/views";
  data[1].iter_cb= make_change_tree_for_views;
  data[1].org_list= 0;
  data[1].mod_list= 0;

  data[2].name= "/routines";
  data[2].iter_cb= make_change_tree_for_routines;
  data[2].org_list= 0;
  data[2].mod_list= 0;

  // remove irrelevant subitems
  myx_grt_dict_item_del(os, "routineGroups");
  myx_grt_dict_item_del(ms, "routineGroups");

  make_change_tree_for_items(grt, node, os, ms, data, 3, NULL);
}

static void make_change_tree_for_catalogs(MYX_GRT *grt, MYX_GRT_VALUE *node, MYX_GRT_VALUE *oc, MYX_GRT_VALUE *mc)
{
  static char *catalog_irrel_keys[] = {"_id", "owner", "oldName", "version", NULL};

  SUBITEM_DATA data;
  data.iter_cb= make_change_tree_for_schemata;
  data.org_list= NULL;
  data.mod_list= NULL;
  data.name= "/schemata";

  make_change_tree_for_items(grt, node, oc, mc, &data, 1, catalog_irrel_keys);
}

MYX_GRT_VALUE *get_catalog_changes(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *) data;
  MYX_GRT_VALUE *result= NULL;
  MYX_GRT_VALUE *modelCatalog= NULL;
  MYX_GRT_VALUE *databaseCatalog= NULL;

  if (!((myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) == 2)))
    return myx_grt_function_create_error_result("The submitted parameter has to be a list with two elements.", 
      "TransformationMySQL:getCatalogsChanges(db.Catalog modelCatalog, db.Catalog databaseCatalog);");

  modelCatalog= myx_grt_value_dup(myx_grt_list_item_get(param, 0));
  databaseCatalog= myx_grt_value_dup(myx_grt_list_item_get(param, 1));

  if ((myx_grt_value_get_type(modelCatalog) != MYX_DICT_VALUE) || 
    (!myx_grt_dict_struct_is_or_inherits_from(grt, modelCatalog, "db.Catalog")) ||
    (!myx_grt_dict_struct_is_or_inherits_from(grt, databaseCatalog, "db.Catalog")))
    return myx_grt_function_create_error_result("The parameters have to be db.Catalog.", NULL);

  // build change tree
  result= new_change_obj(grt, NULL, databaseCatalog, modelCatalog, 0);
  make_change_tree_for_catalogs(grt, result, databaseCatalog, modelCatalog);


  return myx_grt_function_create_result(result);
}

static void put_list_into_cache(MYX_GRT *grt, MYX_GRT_VALUE *list)
{
  int i, count;
  for (i= 0, count= (int)myx_grt_list_item_count(list); i < count; i++)
  {
    myx_grt_reference_cache_add(grt, myx_grt_list_item_get(list, i));
  }
}

static char *get_foreign_key_sql(MYX_GRT *grt, MYX_GRT_VALUE *fk)
{
  char *quote_char= g_strdup("`");
  //MYX_GRT_VALUE *fk= myx_grt_list_item_get(fks, i);
  const char *fk_name= myx_grt_dict_item_get_as_string(fk, "name");
  MYX_GRT_VALUE *fk_columns= myx_grt_dict_item_get_value(fk, "columns");
  //MYX_GRT_VALUE *fk_columns= myx_grt_dict_item_get_value(fk, "columnNames");

  MYX_GRT_VALUE *fk_ref_table= myx_grt_dict_item_get_reference_value(grt, fk, "referedTable");
  const char *fk_ref_schema_name= myx_grt_dict_item_get_as_string(fk, "referedTableSchemaName");
  const char *fk_ref_table_name= myx_grt_dict_item_get_as_string(fk, "referedTableName");

  //MYX_GRT_VALUE *fk_ref_columns= myx_grt_dict_item_get_value(fk, "referedColumns");
  MYX_GRT_VALUE *fk_ref_column_names= myx_grt_dict_item_get_value(fk, "referedColumnNames");

  const char *delete_rule= myx_grt_dict_item_get_as_string(fk, "deleteRule");
  const char *update_rule= myx_grt_dict_item_get_as_string(fk, "updateRule");
  char *sql_fk_cols= NULL;
  char *sql_fk_ref_cols= NULL;
  char *sql_fk= NULL;
  //int col_refs_valid= 1;

  unsigned int j;

  for (j= 0; j < myx_grt_list_item_count(fk_columns); j++)
  {
    MYX_GRT_VALUE *column= myx_grt_list_item_get_reference_value(grt, fk_columns, j);

    if (sql_fk_cols)
      sql_fk_cols= str_g_append(sql_fk_cols, ", ");

    sql_fk_cols= str_g_append_and_free(sql_fk_cols, g_strdup_printf("%s%s%s", 
        quote_char,
        column == NULL ? "" : myx_grt_dict_item_get_as_string(column, "name"),
        quote_char));
  }

  // if possible, use real reference to table. If the table is in a different schema, use text names
  if (fk_ref_table)
  {
    MYX_GRT_VALUE *fk_ref_schema= myx_grt_dict_item_get_reference_value(grt, fk_ref_table, "owner");

    fk_ref_schema_name= myx_grt_dict_item_get_as_string(fk_ref_schema, "name");
    fk_ref_table_name= myx_grt_dict_item_get_as_string(fk_ref_table, "name");
  }
  else
  {
    fk_ref_schema_name= myx_grt_dict_item_get_as_string(fk, "referedTableSchemaName");
    fk_ref_table_name= myx_grt_dict_item_get_as_string(fk, "referedTableName");
  }

  // check if _all_ external column refs are valid, otherwise use
  // column names for all columns
  //for (j= 0; j < myx_grt_list_item_count(fk_ref_columns); j++)
  //{
  //  MYX_GRT_VALUE *fk_ref_col= myx_grt_list_item_get_reference_value(grt, fk_ref_columns, j);
  //  if(fk_ref_col == NULL)
  //  {
  //    col_refs_valid= 0;
  //    break;
  //  }
  //}  

  // if possible, use real reference to columns. If not, use the columns names stored as text
  //if (col_refs_valid && fk_ref_columns && (myx_grt_list_item_count(fk_ref_columns) > 0))
  //{
  //  for (j= 0; j < myx_grt_list_item_count(fk_ref_columns); j++)
  //  {
  //    MYX_GRT_VALUE *fk_ref_col= myx_grt_list_item_get_reference_value(grt, fk_ref_columns, j);

  //    if (fk_ref_col)
  //    {
  //      if (sql_fk_ref_cols)
  //        sql_fk_ref_cols= str_g_append(sql_fk_ref_cols, ", ");

  //      sql_fk_ref_cols= str_g_append_and_free(sql_fk_ref_cols, g_strdup_printf("%s%s%s", 
  //          quote_char,
  //          myx_grt_dict_item_get_as_string(fk_ref_col, "name"),
  //          quote_char));
  //    }
  //  }
  //}
  //else
  {
    for (j= 0; j < myx_grt_list_item_count(fk_ref_column_names); j++)
    {
      if (sql_fk_ref_cols)
        sql_fk_ref_cols= str_g_append(sql_fk_ref_cols, ", ");

      sql_fk_ref_cols= str_g_append_and_free(sql_fk_ref_cols, g_strdup_printf("%s%s%s", 
          quote_char,
          myx_grt_list_item_get_as_string(fk_ref_column_names, j),
          quote_char));
    }
  }

  if(fk_ref_schema_name != NULL)
  {
    sql_fk= str_g_append_and_free(sql_fk, 
      g_strdup_printf("CONSTRAINT %s%s%s FOREIGN KEY (%s) \n\tREFERENCES %s%s%s.%s%s%s (%s)", 
        quote_char, fk_name, quote_char,
        sql_fk_cols,
        quote_char, fk_ref_schema_name, quote_char,
        quote_char, fk_ref_table_name, quote_char,
        sql_fk_ref_cols));
  }
  else
  {
    sql_fk= str_g_append_and_free(sql_fk, 
      g_strdup_printf("CONSTRAINT %s%s%s FOREIGN KEY (%s) \n\tREFERENCES %s%s%s (%s)", 
        quote_char, fk_name, quote_char,
        sql_fk_cols,
        quote_char, fk_ref_table_name, quote_char,
        sql_fk_ref_cols));
  }

  if (delete_rule)
    sql_fk= str_g_append_and_free(sql_fk, g_strdup_printf(" \n\tON DELETE %s", delete_rule));

  if (update_rule)
    sql_fk= str_g_append_and_free(sql_fk, g_strdup_printf(" \n\tON UPDATE %s", update_rule));

  //sql_fk= str_g_append(sql_fk, "," _br);

  g_free(sql_fk_cols);
  g_free(sql_fk_ref_cols);

  return sql_fk;
}

static char *get_index_sql(MYX_GRT *grt, MYX_GRT_VALUE *index)
{
  char *quote_char= g_strdup("`");
  int i;
  MYX_GRT_VALUE *pk= index;
  MYX_GRT_VALUE *pk_columns= myx_grt_dict_item_get_value(pk, "columns");
  char *sql_pk_cols= NULL;
  char *sql_pk= NULL;
  char *sql_indices= NULL;

  if(myx_grt_dict_item_get_as_int(index, "isPrimary") == 1)
  {
    for (i= 0; i < (int)myx_grt_list_item_count(pk_columns); i++)
    {
      MYX_GRT_VALUE *index_column= myx_grt_list_item_get(pk_columns, i);
      MYX_GRT_VALUE *column= myx_grt_dict_item_get_reference_value(grt, index_column, "referedColumn");
      int index_column_length= myx_grt_dict_item_get_as_int(index_column, "columnLength");

      //if (sql_pk_cols)
      //  sql_pk_cols= str_g_append(sql_pk_cols, ", ");

      sql_pk_cols= str_g_append_and_free(sql_pk_cols, g_strdup_printf("%s%s%s", 
          quote_char,
          column == NULL ? "" : myx_grt_dict_item_get_as_string(column, "name"),
          quote_char));

      // add index column length if set
      if (index_column_length > 0)
        sql_pk_cols= str_g_append_and_free(sql_pk_cols, g_strdup_printf("(%d)", index_column_length));
    }

    if (sql_pk_cols)
      sql_pk= str_g_append_and_free(sql_pk, g_strdup_printf("PRIMARY KEY (%s)", 
        sql_pk_cols));

    g_free(sql_pk_cols);

    return sql_pk;
  }
  else
  {
    const char *index_name= myx_grt_dict_item_get_as_string(index, "name");
    const char *unique= (myx_grt_dict_item_get_as_int(index, "unique") == 1) ? "UNIQUE " : "";
    unsigned int j;

    MYX_GRT_VALUE *index_columns= myx_grt_dict_item_get_value(index, "columns");
    char *sql_index_cols= NULL;

    //if (myx_grt_dict_item_get_as_int(index, "isPrimary") == 1)
    //  continue;

    for (j= 0; j < myx_grt_list_item_count(index_columns); j++)
    {
      MYX_GRT_VALUE *index_col= myx_grt_list_item_get(index_columns, j);
      int column_length= myx_grt_dict_item_get_as_int(index_col, "columnLength");
      
      //MYX_GRT_VALUE *column_ref= myx_grt_dict_item_get_reference_value(grt, index_col, "referedColumn");
      //const char *column_name= myx_grt_dict_item_get_as_string(column_ref, "name");
      const char *column_name= myx_grt_dict_item_get_as_string(index_col, "name");

      //MYX_GRT_VALUE *simple_datatype= myx_grt_dict_item_get_reference_value(grt, column_ref, "simpleType");

      if (sql_index_cols)
        sql_index_cols= str_g_append(sql_index_cols, ", ");

      sql_index_cols= str_g_append_and_free(sql_index_cols, g_strdup_printf("%s%s%s", 
          quote_char, column_name, quote_char));

      if (column_length > 0)
      {
        sql_index_cols= str_g_append_and_free(sql_index_cols, g_strdup_printf("(%d)", 
          column_length));
      }
    }

    sql_indices= str_g_append_and_free(sql_indices, 
      g_strdup_printf("INDEX %s%s%s%s (%s)",
        unique,
        quote_char, index_name, quote_char,
        sql_index_cols));

    g_free(sql_index_cols);

    return sql_indices;
  }
}

static char *get_column_sql(MYX_GRT_VALUE *column)
{
  char *quote_char= g_strdup("`");
  int is_integer= 0;
  int is_float= 0;

  MYX_GRT_VALUE *column_flags= myx_grt_dict_item_get_value(column, "flags");
  const char *is_nullable= (myx_grt_dict_item_get_as_int(column, "isNullable") == 1) ? "NULL" : "NOT NULL";
  const char *datatype= myx_grt_dict_item_get_as_string(column, "datatypeName");
  const char *datatype_explicit_params= myx_grt_dict_item_get_as_string(column, "datatypeExplicitParams");
  const char *column_charset= myx_grt_dict_item_get_as_string(column, "characterSetName");
  const char *column_collation= myx_grt_dict_item_get_as_string(column, "collationName");
  char *datatype_definition;
  const char *default_value;
  int default_value_is_null;
  char *column_comment;
  char *column_length= myx_grt_dict_item_get_formated_as_string(column, "length");
  char *column_precision= myx_grt_dict_item_get_formated_as_string(column, "precision");
  char *column_scale= myx_grt_dict_item_get_formated_as_string(column, "scale");
  unsigned int j;
  char *sql_cols= NULL;

  // if there is an explicit parameter definition for the datatype, use it
  if (datatype_explicit_params && datatype_explicit_params[0])
    datatype_definition= g_strdup_printf("%s%s", datatype, datatype_explicit_params);
  else
  {
    // append length / precision / scale to datatype definition
    if ((strcmp2(datatype, "CHAR") == 0) ||
        (strcmp2(datatype, "VARCHAR") == 0) ||
        (strcmp2(datatype, "VARBINARY") == 0) ||
        (strcmp2(datatype, "BINARY") == 0))
    {
      datatype_definition= g_strdup_printf("%s(%s)", 
        datatype, column_length);
    }
    else if ((strcmp2(datatype, "TINYINT") == 0) ||
        (strcmp2(datatype, "SMALLINT") == 0) ||
        (strcmp2(datatype, "MEDIUMINT") == 0) ||
        (strcmp2(datatype, "INT") == 0) ||
        (strcmp2(datatype, "INTEGER") == 0) ||
        (strcmp2(datatype, "BIGINT") == 0))
    {
      is_integer= 1;

      if (strcmp2(column_precision, "0") != 0)
      {
        datatype_definition= g_strdup_printf("%s(%s)", 
          datatype, 
          column_precision);
      }
      else
        datatype_definition= g_strdup(datatype);
    }
    else if ((strcmp2(datatype, "REAL") == 0) ||
        (strcmp2(datatype, "DOUBLE") == 0) ||
        (strcmp2(datatype, "FLOAT") == 0) ||
        (strcmp2(datatype, "DECIMAL") == 0) ||
        (strcmp2(datatype, "NUMERIC") == 0))
    {
      is_float= 1;

      // if the scale holds a negative value like -1, do not use it
      if (column_scale[0] != '-')
        datatype_definition= g_strdup_printf("%s(%s, %s)", 
          datatype,
          column_precision,
          column_scale);
      else
        datatype_definition= g_strdup_printf("%s(%s)", 
          datatype,
          column_precision);
    }
    else
      datatype_definition= g_strdup(datatype);
  }

  // add Character Set and Collation for CHAR | VARCHAR | TEXT columns
  if (((strcmp2(datatype, "CHAR") == 0) ||
      (strcmp2(datatype, "VARCHAR") == 0) ||
      (strcmp2(datatype, "TEXT") == 0)) && 
      (column_charset))
  {
    datatype_definition= str_g_append_and_free(datatype_definition, 
        g_strdup_printf(" CHARACTER SET %s", column_charset));

    // also add collation if it is defined
    if (column_collation)
    {
      datatype_definition= str_g_append_and_free(datatype_definition, 
        g_strdup_printf(" COLLATE %s", column_collation));
    }
  }

  // add column datatype flags
  if(column_flags != NULL)
  {
    for (j= 0; j < myx_grt_list_item_count(column_flags); j++)
    {
      MYX_GRT_VALUE *flag= myx_grt_list_item_get(column_flags, j);

      datatype_definition= str_g_append_and_free(datatype_definition, 
          g_strdup_printf(" %s", myx_grt_value_as_string(flag)));
    }
  }

  sql_cols= str_g_append_and_free(sql_cols, g_strdup_printf("  %s%s%s %s %s", 
    quote_char,
    myx_grt_dict_item_get_as_string(column, "name"), 
    quote_char,
    datatype_definition,
    is_nullable));

  // add default value
  default_value= myx_grt_dict_item_get_as_string(column, "defaultValue");
  default_value_is_null= myx_grt_dict_item_get_as_int(column, "defaultValueIsNull");
  if (default_value && default_value[0] && !default_value_is_null && 
    !((is_integer || is_float) && (strcmp(default_value, "''") == 0)))
  {
    sql_cols= str_g_append_and_free(sql_cols, g_strdup_printf(" DEFAULT %s", default_value));
  }

  // add column auto increment
  if (myx_grt_dict_item_get_as_int(column, "autoIncrement") == 1)
    sql_cols= str_g_append(sql_cols, " AUTO_INCREMENT");

  // add column comment
  column_comment= g_strdup(myx_grt_dict_item_get_as_string(column, "columnComment"));
  if (column_comment && column_comment[0])
  {
    column_comment= str_g_replace(column_comment, "'", "\\'");
    sql_cols= str_g_append_and_free(sql_cols, g_strdup_printf(" COMMENT '%s'", column_comment));
  }

  //sql_cols= str_g_append(sql_cols, "," _br);

  g_free(datatype_definition);
  g_free(column_comment);
  g_free(column_length);
  g_free(column_precision);
  g_free(column_scale);

  return sql_cols;
}

typedef struct 
{
  const char *key;
  const char *pattern;
  char *value;
}
ATTRIBUTE_OPTIONS_DATA;

static char * get_sql_alter_schema(MYX_GRT *grt, MYX_GRT_VALUE * existing, MYX_GRT_VALUE * altered)
{
  int i, count, sz= 0;
  MYX_GRT_VALUE *diff_list= myx_grt_value_diff_make(grt, existing, altered);
  char *sql= NULL, *cs_sql= NULL, *coll_sql= NULL;
  const char *name= myx_grt_value_as_string(myx_grt_dict_item_get_value(existing, "name"));

  // remove irrelevant changed keys
  remove_diff_keys(diff_list, (const char**)irrel_keys);
  //remove_irrelevant_fk_changes(grt, existing, altered, diff_list);

  for(i= 0, count= myx_grt_list_item_count(diff_list); i < count; i += 2)
  {
    MYX_GRT_VALUE *path= myx_grt_list_item_get(diff_list, i);
    MYX_GRT_VALUE *value= myx_grt_list_item_get(diff_list, i+1);
    const char *path_str= myx_grt_value_as_string(path);

    if(strcmp("defaultCharacterSetName", path_str + 2) == 0)
    {
      cs_sql= g_malloc(64);
      sz += 64;
      sprintf(cs_sql, "DEFAULT CHARACTER SET %s", myx_grt_value_as_string(value));
    }
    else if(strcmp("defaultCollationName", path_str + 2) == 0)
    {
      coll_sql= g_malloc(64);
      sz += 64;
      sprintf(coll_sql, "DEFAULT COLLATE %s", myx_grt_value_as_string(value));
    }
  }

  sql= g_malloc((int)strlen(name) + sz + 64);
  sprintf(sql, "ALTER DATABASE `%s` %s %s", name, cs_sql ? cs_sql : "", coll_sql ? coll_sql : "");
  if(cs_sql)
  {
    g_free(cs_sql);
  }
  if(coll_sql)
  {
    g_free(coll_sql);
  }

  return sql;
}

static int str_has_suffix(const char *str, const char *suffix)
{
  int d= (int)strlen(str) - (int)strlen(suffix);

  if(d < 0)
    return 0;

  return strcmp(str + d, suffix) == 0;
}

static void remove_diff_keys(MYX_GRT_VALUE *diff_list, const char **remove_list)
{
  int i, j, count;

restart:
  for(i= 0, count= myx_grt_list_item_count(diff_list); i < count; i += 2)
  {
    for(j= 0; remove_list[j] != NULL; j++)
    {
      const char *list_key= myx_grt_list_item_get_as_string(diff_list, i);
      if(str_has_suffix(list_key, remove_list[j]))
      {
        myx_grt_list_item_del(diff_list, i);
        myx_grt_list_item_del(diff_list, i);
        goto restart;
      }
    }
  }
}

static int foreign_key_columns_changed(MYX_GRT *grt, MYX_GRT_VALUE *existing_fk, MYX_GRT_VALUE *changed_fk, const char *colname)
{
  int i, count, j, jcount, retval;
  MYX_GRT_VALUE *ex_cols= NULL, *ch_cols= NULL;

  ex_cols= myx_grt_value_dup(myx_grt_dict_item_get_value(existing_fk, colname));
  ch_cols= myx_grt_value_dup(myx_grt_dict_item_get_value(changed_fk, colname));

  if(!ex_cols || !ch_cols)
    return 0;

restart:
  for(i= 0, count= myx_grt_list_item_count(ex_cols); i < count; i++)
  {
    const char *ex_name;
    MYX_GRT_VALUE *ex_col= myx_grt_list_item_get_reference_value(grt, ex_cols, i);
    if(!ex_col)
      continue;
    ex_name= myx_grt_dict_item_get_as_string(ex_col, "oldName");
    for(j= 0, jcount= myx_grt_list_item_count(ch_cols); j < jcount; j++)
    {
      const char *ch_name;
      MYX_GRT_VALUE *diff;
      MYX_GRT_VALUE *ch_col= myx_grt_list_item_get_reference_value(grt, ch_cols, j);
      if(!ch_col)
        continue;
      ch_name= myx_grt_dict_item_get_as_string(ch_col, "oldName");
      if(strcmp(ex_name, ch_name) != 0)
        continue;
      diff= myx_grt_value_diff_make(grt, ex_col, ch_col);
      remove_diff_keys(diff, (const char**)irrel_keys);
      if(myx_grt_list_item_count(diff) > 0)
      {
        myx_grt_value_release(ex_cols);
        myx_grt_value_release(ch_cols);
        return 1;
      }
      myx_grt_list_item_del(ex_cols, i);
      myx_grt_list_item_del(ch_cols, j);
      goto restart;
    }
  }

  retval= (myx_grt_list_item_count(ex_cols) != 0) || (myx_grt_list_item_count(ch_cols) != 0);
  myx_grt_value_release(ex_cols);
  myx_grt_value_release(ch_cols);
  return retval;
}

static int foreign_key_changed(MYX_GRT *grt, MYX_GRT_VALUE *existing_fk, MYX_GRT_VALUE *changed_fk)
{
  MYX_GRT_VALUE *ex_ref_table= NULL, *ch_ref_table= NULL;
  const char *ex_table_name= NULL, *ch_table_name= NULL;

  int cols= foreign_key_columns_changed(grt, existing_fk, changed_fk, "columns")
    || foreign_key_columns_changed(grt, existing_fk, changed_fk, "referedColumns");
  
  if(cols != 0)
    return 1;

  ex_ref_table= myx_grt_dict_item_get_reference_value(grt, existing_fk, "referedTable");
  ch_ref_table= myx_grt_dict_item_get_reference_value(grt, changed_fk, "referedTable");
  
  ex_table_name= myx_grt_dict_item_get_as_string(ex_ref_table, "oldName");
  ch_table_name= myx_grt_dict_item_get_as_string(ch_ref_table, "oldName");

  return strcmp2(ex_table_name, ch_table_name);
}

static void remove_irrelevant_fk_changes(MYX_GRT *grt, MYX_GRT_VALUE *existing, MYX_GRT_VALUE *changed, MYX_GRT_VALUE *diff_list)
{
  int i, j, jcount, count;
  MYX_GRT_VALUE *existing_fks= NULL;
  MYX_GRT_VALUE *changed_fks= NULL;

restart:
  for(i= 0, count= myx_grt_list_item_count(diff_list); i < count; i += 2)
  {
    const char *path= myx_grt_list_item_get_as_string(diff_list, i);
    const char *next= NULL;
    //int idx= -1;

    if(strncmp(path + 1, "/foreignKeys/", sizeof("/foreignKeys/") - 1) != 0)
      continue;
    next= strchr(path + 1 + sizeof("/foreignKeys/") - 1, '/');
    if((next == NULL) 
      || ((strncmp(next, "/columns/", sizeof("/columns/") - 1) != 0)
          && (strncmp(next, "/referedColumns/", sizeof("/referedColumns/") - 1) != 0)
          && (strncmp(next, "/referedTable", sizeof("/referedTable") - 1) != 0)))
      continue;
    myx_grt_list_item_del(diff_list, i);
    myx_grt_list_item_del(diff_list, i);
    goto restart;
  }

  existing_fks= myx_grt_dict_item_get_value(existing, "foreignKeys");
  changed_fks= myx_grt_dict_item_get_value(changed, "foreignKeys");

  for(i= 0, count= myx_grt_list_item_count(existing_fks); i < count; i++)
  {
    MYX_GRT_VALUE *existing_fk= myx_grt_list_item_get(existing_fks, i);
    const char *existing_fk_name= myx_grt_dict_item_get_as_string(existing_fk, "oldName");

    for(j= 0, jcount= myx_grt_list_item_count(changed_fks); j < jcount; j++)
    {
      MYX_GRT_VALUE *changed_fk= myx_grt_list_item_get(changed_fks, i);
      const char *changed_fk_name= myx_grt_dict_item_get_as_string(changed_fk, "oldName");
      if(strcmp(existing_fk_name, changed_fk_name) != 0)
        continue;
      if(foreign_key_changed(grt, existing_fk, changed_fk))  // add dummy entry to show that fk has changed
      {
        char *p= g_strdup_printf("//foreignKeys/%d", i);
        myx_grt_list_item_add(diff_list, myx_grt_value_from_string(p));
        myx_grt_list_item_add(diff_list, myx_grt_value_from_string("dummy"));
        g_free(p);
      }
    }
  }
}

// create substrings of changed attributes of an object based on diff list and info about the attributes and their text patterns
// for usage examples see get_sql_alter_table() and get_sql_alter_routine() below
static void fill_attributes_by_patterns(MYX_GRT_VALUE *existing, MYX_GRT_VALUE *diff_list, ATTRIBUTE_OPTIONS_DATA *data, int **specials, char *empty_str)
{
  int i, j, count;

  for(i= 0, count= myx_grt_list_item_count(diff_list); i < count; i += 2)
  {
    MYX_GRT_VALUE *path= myx_grt_list_item_get(diff_list, i);
    MYX_GRT_VALUE *value= myx_grt_list_item_get(diff_list, i+1);
    const char *path_str= myx_grt_value_as_string(path);

    for(j= 0; data[j].key != NULL; j++)
    {
      if(strcmp(path_str + 2, data[j].key) == 0)
      {
        char *value_str= myx_grt_value_formated_as_string(value);
        int len= (int)(strlen(data[j].pattern) + strlen(value_str) + 1);
        data[j].value= g_malloc(len);
        sprintf(data[j].value, data[j].pattern, value_str);
        g_free(value_str);
        break;
      }
    }
  }

  for(i= 0; specials[i] != NULL; i++)
  {
    int flag1= (data[specials[i][0]].value == empty_str);
    int flag2= 1;
    for(j= 1; specials[i][j] >= 0; j++)
    {
      flag2 &= ((data[specials[i][j]].value == empty_str) == flag1);
    }
    if(flag2)  // all are assigned or all are unassigned
    {
      continue;
    }
    flag2= 1;
    for(j= 0; specials[i][j] >= 0; j++)
    {
      int idx= specials[i][j];
      if(data[idx].value == empty_str)
      {
        int len;
        MYX_GRT_VALUE *value= myx_grt_dict_item_get_value(existing, data[idx].key);
        const char *value_str= myx_grt_value_as_string(value);
        if(!value_str)
        {
          flag2= 0;
          break;
        }
        len= (int)(strlen(data[idx].pattern) + strlen(value_str) + 1);
        data[idx].value= g_malloc(len);
        sprintf(data[idx].value, data[idx].pattern, value_str);
      }
    }
    if(flag2)  
    {
      continue;
    }
    // failed to assign some values - clear assinged
    for(j= 1; specials[i][j] >= 0; j++)
    {
      int idx= specials[i][j];
      if(data[idx].value != empty_str)
      {
        g_free(data[idx].value);
        data[idx].value= empty_str;
      }      
    }
  }
}

static char * get_sql_alter_table(MYX_GRT *grt, MYX_GRT_VALUE * existing, MYX_GRT_VALUE * altered)
{
  ATTRIBUTE_OPTIONS_DATA data[]= {
    {"tableEngine", "\n\tENGINE = %s", ""},
    {"nextAutoInc", "\n\tAUTO_INCREMENT = %s", ""},
    {"password", "\n\tPASSWORD = '%s'", ""},
    {"delayKeyWrite", "\n\tDELAY_KEY_WRITE = %s", ""},
    {"defaultCharacterSetName", "\n\tDEFAULT CHARACTER SET %s", ""},
    {"defaultCollationName", "\n\tCOLLATE %s", ""},
    {"comment", "\n\tCOMMENT = '%s'", ""},
    {"mergeUnion", "\n\tUNION = %s", ""},
    {"mergeInsert", "\n\tINSERT_METHOD = %s", ""},
    {"tableDataDir", "\n\tDATA DIRECTORY = '%s'", ""},
    {"tableIndexDir", "\n\tINDEX DIRECTORY = '%s'", ""},
    {"packKeys", "\n\tPACK_KEYS = %s", ""},
    {"raidType", "\n\tRAID_TYPE = %s", ""},
    {"raidChunks", "\n\tRAID_CHUNKS = %s", ""},
    {"raidChunkSize", "\n\tRAID_CHUNKSIZE = %s", ""},
    {"checksum", "\n\tCHECKSUM = %s", ""},
    {"rowFormat", "\n\tROW_FORMAT = %s", ""},
    {"avgRowLength", "\n\tAVG_ROW_LENGTH = %s", ""},
    {"minRows", "\n\tMIN_ROWS = %s", ""},
    {"maxRows", "\n\tMAX_ROWS = %s", ""},
    {NULL, NULL, NULL}
  };

  // attributes lised below should be either all (groupwise) assigned a value or be all unassigned
  int s1[]= {4, 5, -1};       // indices of defaultCharacterSetName & defaultCollationName
  int s2[]= {12, 13, 14, -1}; // indices of raidType, raidChunks & raidChunkSize
  int *specials[3];

  int i, count;
  MYX_GRT_VALUE *diff_list= myx_grt_value_diff_make(grt, existing, altered);
  MYX_GRT_VALUE *schema= myx_grt_dict_item_get_reference_value(grt, existing, "owner");
  const char *name= myx_grt_dict_item_get_as_string(existing, "oldName");
  const char *schema_name= myx_grt_dict_item_get_as_string(schema, "oldName");

  char *alter_prefix= g_strdup_printf("ALTER TABLE `%s`.`%s`", schema_name, name);
  char *sql= g_strdup(alter_prefix);
  char *trigger_sql= g_strdup("");

  // diff generates indices so that column adds and dels are performed correctly if the diff list is sequentally
  // applied to the source object. but this is obviously not suitable for the alter sql case, so
  // we keep track how indexing was changed so far, and do appropriate corrections
  int *adds= g_malloc(sizeof(int)*myx_grt_list_item_count(diff_list)),
      *dels= g_malloc(sizeof(int)*myx_grt_list_item_count(diff_list)),
      *indices= g_malloc(sizeof(int)*myx_grt_list_item_count(diff_list)),
      *fks= g_malloc(sizeof(int)*myx_grt_list_item_count(diff_list)),
      *triggers= g_malloc(sizeof(int)*myx_grt_list_item_count(diff_list)),
      *cols= g_malloc(sizeof(int)*myx_grt_list_item_count(diff_list));

  specials[0] = s1;
  specials[1] = s2;
  specials[2] = NULL;

  put_list_into_cache(grt, myx_grt_dict_item_get_value(existing, "columns"));

  // remove irrelevant changed keys
  remove_diff_keys(diff_list, (const char**)irrel_keys);
  remove_irrelevant_fk_changes(grt, existing, altered, diff_list);

  if(strcmp(myx_grt_dict_item_get_as_string(existing, "oldName"), myx_grt_dict_item_get_as_string(altered, "name")) != 0)
  {
    sql= str_g_append_and_free(sql, g_strdup_printf(", RENAME TO `%s`.`%s`", schema_name, myx_grt_dict_item_get_as_string(altered, "name")));
    g_free(alter_prefix);
    alter_prefix= g_strdup_printf("ALTER TABLE `%s`.`%s`", schema_name, myx_grt_dict_item_get_as_string(altered, "name"));
  }

  for(i= 0, count= myx_grt_list_item_count(diff_list); i < count; i++)
  {
    adds[i]= dels[i]= indices[i]= fks[i]= cols[i]= -1;
  }

  fill_attributes_by_patterns(existing, diff_list, data, specials, "");

  // process subsets (columns, keys, fks)
  for(i= 0, count= myx_grt_list_item_count(diff_list); i < count; i += 2)
  {
    MYX_GRT_VALUE *path= myx_grt_list_item_get(diff_list, i);
    MYX_GRT_VALUE *value= myx_grt_list_item_get(diff_list, i+1);
    const char *path_str= myx_grt_value_as_string(path);

    if(strncmp(path_str + 1, "/columns/", sizeof("/columns/") - 1) == 0)
    {
      const char *column_sql;
      MYX_GRT_VALUE *column_obj, *prev_column_obj;
      MYX_GRT_VALUE *existing_column_list= myx_grt_dict_item_get_value(existing, "columns");
      MYX_GRT_VALUE *altered_column_list= myx_grt_dict_item_get_value(altered, "columns");
      int k, idx= atoi(path_str + sizeof("/columns/"));
      int action= (strchr(path_str + sizeof("/columns/"), '/') == NULL) ? path_str[0] : '/';

      for(k= 0; (k < count) && (cols[k] != -1); k++)
        if(cols[k] == idx)
          break;

      if(cols[k] == idx)
        continue;
      else
        cols[k]= idx;

      // preform index correction
      for(k= 0; (k < count) && ((adds[k] != -1) || (dels[k] != -1)); k++)
      {
        if((adds[k] != -1) && (adds[k] < idx))
        {
          --idx;
        }
        if((dels[k] != -1) && (dels[k] < idx))
        {
          ++idx;
        }
      }

      switch(action)
      {
      case '+':
        prev_column_obj= (idx > 0) ? myx_grt_list_item_get(altered_column_list, idx-1) : NULL;
        column_obj= myx_grt_list_item_get(altered_column_list, idx);
        column_sql= get_column_sql(column_obj);
        if(prev_column_obj == NULL)
        {          
          sql= str_g_append_and_free(sql, g_strdup_printf(",\n\tADD COLUMN %s FIRST", column_sql));
        }
        else
        {
          sql= str_g_append_and_free(sql, g_strdup_printf(",\n\tADD COLUMN %s AFTER `%s`", 
            column_sql, myx_grt_value_as_string(myx_grt_dict_item_get_value(prev_column_obj, "oldName"))));
        }
        // update index correction data
        for(k= 0; k < count; k++)
        {
          if(adds[k] == -1)
          {
            adds[k]= idx;
            break;
          }
        }
        break;
      case '-':
        column_obj= myx_grt_list_item_get(existing_column_list, idx);
        sql= str_g_append_and_free(sql, g_strdup_printf(",\n\tDROP COLUMN `%s`", myx_grt_value_as_string(myx_grt_dict_item_get_value(column_obj, "oldName"))));
        // update index correction data
        for(k= 0; k < count; k++)
        {
          if(dels[k] == -1)
          {
            dels[k]= idx;
            break;
          }
        }
        break;
      case '/':
        column_obj= myx_grt_list_item_get(altered_column_list, idx);
        column_sql= get_column_sql(column_obj);
        sql= str_g_append_and_free(sql, g_strdup_printf(",\n\tCHANGE COLUMN `%s` %s", 
          myx_grt_value_as_string(myx_grt_dict_item_get_value(column_obj, "oldName")), column_sql));
        break;
      case '*':
        column_obj= myx_grt_list_item_get(existing_column_list, idx);
        prev_column_obj= (myx_grt_value_as_int(value) > 0) ? myx_grt_list_item_get(existing_column_list, myx_grt_value_as_int(value)) : NULL;
        column_sql= get_column_sql(column_obj);
        if(prev_column_obj == NULL)
        {          
          sql= str_g_append_and_free(sql, g_strdup_printf(",\n\tMODIFY COLUMN %s FIRST", column_sql));
        }
        else
        {
          sql= str_g_append_and_free(sql, g_strdup_printf(",\n\tMODIFY COLUMN %s AFTER `%s`", 
            column_sql, myx_grt_value_as_string(myx_grt_dict_item_get_value(prev_column_obj, "name"))));
        }
        // update index correction data
        for(k= 0; k < count; k++)
        {
          if(dels[k] == -1)
          {
            dels[k]= idx;
            break;
          }
        }
        for(k= 0; k < count; k++)
        {
          if(adds[k] == -1)
          {
            adds[k]= myx_grt_value_as_int(value);
            break;
          }
        }
        break;
      }
    }
    else if(strncmp(path_str + 1, "/indices/", sizeof("/indices/") - 1) == 0)
    {
      int k, changing= 0;
      const char *index_sql;
      const char *index_name;
      MYX_GRT_VALUE *index_obj;
      MYX_GRT_VALUE *existing_index_list= myx_grt_dict_item_get_value(existing, "indices");
      MYX_GRT_VALUE *altered_index_list= myx_grt_dict_item_get_value(altered, "indices");
      int idx= atoi(path_str + sizeof("/indices/"));
      int action= (strchr(path_str + sizeof("/indices/"), '/') == NULL) ? path_str[0] : '/';

      for(k= 0; (k < count) && (indices[k] != -1); k++)
        if(indices[k] == idx)
          break;

      if(indices[k] == idx)
        continue;
      else
        indices[k]= idx;

      switch(action)
      {
      case '/':
        changing= 1;
        // fallthrough
      case '-':
        index_obj= myx_grt_list_item_get(existing_index_list, idx);
        if(myx_grt_dict_item_get_as_int(index_obj, "isPrimary") == 1)
        {
          sql= str_g_append_and_free(sql, g_strdup(",\n\tDROP PRIMARY KEY"));
        }
        else
        {
          index_name= myx_grt_dict_item_get_as_string(index_obj, "name");
          if(index_name != NULL)
          {
            sql= str_g_append_and_free(sql, g_strdup_printf(",\n\tDROP INDEX `%s`", index_name));
          }
        }
        if(!changing)
        {
          break;
        }
      case '+':
        index_obj= myx_grt_list_item_get(altered_index_list, idx);
        index_sql= get_index_sql(grt, index_obj);
        sql= str_g_append_and_free(sql, g_strdup_printf(",\n\tADD %s", index_sql));
        break;
      }
    }
    else if(strncmp(path_str + 1, "/foreignKeys/", sizeof("/foreignKeys/") - 1) == 0)
    {
      int changing= 0;
      const char *fk_sql;
      const char *fk_name;
      MYX_GRT_VALUE *fk_obj;
      MYX_GRT_VALUE *existing_fk_list= myx_grt_dict_item_get_value(existing, "foreignKeys");
      MYX_GRT_VALUE *altered_fk_list= myx_grt_dict_item_get_value(altered, "foreignKeys");
      int k, idx= atoi(path_str + sizeof("/foreignKeys/"));
      int action= (strchr(path_str + sizeof("/foreignKeys/"), '/') == NULL) ? path_str[0] : '/';

      for(k= 0; (k < count) && (fks[k] != -1); k++)
        if(fks[k] == idx)
          break;

      if(fks[k] == idx)
        continue;
      else
        fks[k]= idx;

      switch(action)
      {
      case '/':
        changing= 1;
        // fallthrough
      case '-':
        fk_obj= myx_grt_list_item_get(existing_fk_list, idx);
        fk_name= myx_grt_dict_item_get_as_string(fk_obj, "name");
        sql= str_g_append_and_free(sql, g_strdup_printf(",\n\tDROP FOREIGN KEY `%s`", fk_name));
        if(!changing)
        {
          break;
        }
      case '+':
        fk_obj= myx_grt_list_item_get(altered_fk_list, idx);
        fk_sql= get_foreign_key_sql(grt, fk_obj);
        if(changing)
          sql= str_g_append_and_free(sql, g_strdup_printf(";\n%s\n\tADD %s", alter_prefix, fk_sql));
        else
          sql= str_g_append_and_free(sql, g_strdup_printf(",\n\tADD %s", fk_sql));
        break;
      }
    }
    else if(strncmp(path_str + 1, "/triggers/", sizeof("/triggers/") - 1) == 0)
    {
      int changing= 0;
      const char *trig_name;
      MYX_GRT_VALUE *trig_obj;
      MYX_GRT_VALUE *existing_trig_list= myx_grt_dict_item_get_value(existing, "triggers");
      MYX_GRT_VALUE *altered_trig_list= myx_grt_dict_item_get_value(altered, "triggers");
      int k, idx= atoi(path_str + sizeof("/triggers/"));
      int action= (strchr(path_str + sizeof("/triggers/"), '/') == NULL) ? path_str[0] : '/';

      for(k= 0; (k < count) && (triggers[k] != -1); k++)
        if(triggers[k] == idx)
          break;

      if(triggers[k] == idx)
        continue;
      else
        triggers[k]= idx;

      switch(action)
      {
      case '/':
        changing= 1;
        // fallthrough
      case '-':
        trig_obj= myx_grt_list_item_get(existing_trig_list, idx);
        trig_name= myx_grt_dict_item_get_as_string(trig_obj, "name");
        trigger_sql= str_g_append_and_free(trigger_sql, g_strdup_printf("\nDROP TRIGGER `%s`.`%s`;", schema_name, trig_name));
        if(!changing)
        {
          break;
        }
      case '+':
        trig_obj= myx_grt_list_item_get(altered_trig_list, idx);
        trig_name= myx_grt_dict_item_get_as_string(trig_obj, "name");
        trigger_sql= str_g_append_and_free(trigger_sql, g_strdup_printf("\nDELIMITER |\nCREATE TRIGGER `%s`.`%s` %s %s ON `%s`.`%s` FOR EACH ROW %s|\nDELIMITER ;", 
          schema_name, trig_name, 
          myx_grt_dict_item_get_as_string(trig_obj, "timing"), 
          myx_grt_dict_item_get_as_string(trig_obj, "event"),
          schema_name, 
          myx_grt_dict_name_item_as_string(myx_grt_dict_item_get_reference_value(grt, trig_obj, "owner")),
          myx_grt_dict_item_get_as_string(trig_obj, "statement")
        ));
        break;
      }
    }
  }

  // add changed table options
  for(i= 0; data[i].key != NULL; i++)
  {
    if(strcmp(data[i].value,""))
    {
      sql= str_g_append_and_free(sql, g_strdup_printf(", %s", data[i].value));
      g_free(data[i].value);
    }
  }

  if(strcmp(sql, alter_prefix) == 0) 
  {
    g_free(sql);
    sql= trigger_sql;
  }
  else
  {
    sql= str_g_append_and_free(sql, g_strdup_printf("; %s", trigger_sql));
    g_free(trigger_sql);
  }


  g_free(adds);
  g_free(dels);
  g_free(indices);
  g_free(fks);
  g_free(triggers);
  g_free(alter_prefix);

  return sql;
}

static char * get_sql_alter_view(MYX_GRT *grt, MYX_GRT_VALUE * existing, MYX_GRT_VALUE * altered)
{
  char *quote_char= g_strdup("`");
  MYX_GRT_VALUE * view= altered;
  const char *view_name= myx_grt_dict_item_get_as_string(view, "name");
  char *quoted_schema_name= get_owner_name(grt, view, quote_char);
  const char *queryExpression= myx_grt_dict_item_get_as_string(view, "queryExpression");
  int with_check_condition= myx_grt_dict_item_get_as_int(view, "withCheckCondition");
  
  char *sql= g_strdup_printf("ALTER VIEW %s%s%s%s AS %s",
    quoted_schema_name,
    quote_char, view_name, quote_char,
    queryExpression);      

  if (with_check_condition == 1)
    sql= str_g_append(sql, "\n\tWITH CHECK OPTION" _br);

  g_free(quote_char);
  g_free(quoted_schema_name);

  return sql;
}

static char * get_sql_alter_routine(MYX_GRT *grt, MYX_GRT_VALUE * existing, MYX_GRT_VALUE * altered)
{
  char *quote_char= g_strdup("`");
  MYX_GRT_VALUE *diff_list= myx_grt_value_diff_make(grt, existing, altered);

  ATTRIBUTE_OPTIONS_DATA data[]= {
    {"comment", "\n\tCOMMENT '%s'", ""},
    {"security", "\n\tSQL SECURITY %s", ""},
    {NULL, NULL, NULL}
  };

  int i;
  int *specials[] = {NULL};
  const char *name= myx_grt_dict_item_get_as_string(existing, "oldName");
  char *sql= NULL;
  
  if(strcmp(myx_grt_dict_item_get_as_string(existing, "routineType"), "PROCEDURE") == 0)
  {
    sql= g_strdup_printf("ALTER PROCEDURE `%s`", name);
  }
  else
  {
    sql= g_strdup_printf("ALTER FUNCTION `%s`", name);
  }

  fill_attributes_by_patterns(existing, diff_list, data, specials, "");

  // add changed routine options
  for(i= 0; data[i].key != NULL; i++)
  {
    if(strcmp(data[i].value,""))
    {
      sql= str_g_append_and_free(sql, g_strdup_printf(" %s", data[i].value));
      g_free(data[i].value);
    }
  }

  g_free(quote_char);
  return sql;
}

MYX_GRT_VALUE *get_alter(MYX_GRT_VALUE * existing, MYX_GRT_VALUE * altered, void *data)
{
  MYX_GRT *grt= (MYX_GRT *) data;
  MYX_GRT_VALUE *result= NULL;
  const char *sql= NULL;
  const char *struct_name= NULL;

  struct_name= myx_grt_dict_struct_get_name(existing);

  if (strcmp2(struct_name, "db.mysql.Schema") == 0)
    sql= get_sql_alter_schema(grt, existing, altered);
  else if (strcmp2(struct_name, "db.mysql.Table") == 0)
    sql= get_sql_alter_table(grt, existing, altered);
  else if (strcmp2(struct_name, "db.mysql.View") == 0)
    sql= get_sql_alter_view(grt, existing, altered);
  else if (strcmp2(struct_name, "db.mysql.Routine") == 0)
    sql= get_sql_alter_routine(grt, existing, altered);
  else
    return myx_grt_function_create_error_result("The struct of the submitted dict is of a supported type.", NULL);

  result= myx_grt_function_create_result(myx_grt_value_from_string(sql));
  //g_free(sql);

  return result;    
}

MYX_GRT_VALUE *get_sql_alter(MYX_GRT_VALUE *param, void *data)
{
//  MYX_GRT *grt= (MYX_GRT *) data;
//  MYX_GRT_VALUE *result= NULL;
  MYX_GRT_VALUE *existing= NULL;
  MYX_GRT_VALUE *altered= NULL;
  //char *sql= NULL;

//  if (!((myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) == 2)))
//    return myx_grt_function_create_error_result("The submitted parameter has to be a list with two elements.", 
//      "TransformationMySQL:getSQLAlter(GrtObject alteredObject, GrtObject existingObject);");

  if(myx_grt_list_item_count(param) == 2)
  {
    altered= myx_grt_list_item_get(param, 0);
    existing= myx_grt_list_item_get(param, 1);
  }
  else if(myx_grt_list_item_count(param) == 1)
  {
    existing= myx_grt_list_item_get(param, 0);
  }
  else if(myx_grt_list_item_count(param) == 3)
  {
    altered= myx_grt_list_item_get(param, 0);
  }
  
  //if (myx_grt_value_get_type(altered) != MYX_DICT_VALUE)
  //  return myx_grt_function_create_error_result("The first parameter has to be a DICT.", NULL);

  if (altered && existing)  // There are both existing and modified objects so make an ALTER statement
    return get_alter(existing, altered, data);
  else if(altered)          // There is no existing object, so make a CREATE statement
    return get_sql_create(altered, data);
  else if(existing)         // There is no altered object, so make a DROP statement
    return get_sql_drop(existing, data);

  return NULL;
}

static void process_tree_node(MYX_GRT *grt, MYX_GRT_VALUE *tree_node, MYX_GRT_VALUE *alter_list)
{
  int i, count;
  int changed= myx_grt_dict_item_get_as_int(tree_node, "changed");
  int dir= myx_grt_dict_item_get_as_int(tree_node, "alterDirection");
  MYX_GRT_VALUE *children_list= myx_grt_dict_item_get_value(tree_node, "children");
  MYX_GRT_VALUE *item_to_add= NULL;
  MYX_GRT_VALUE *existing= myx_grt_dict_item_get_reference_value(grt, tree_node, "dbObject");

  myx_grt_dict_item_set_value(existing, "sql", NULL);

  if(changed && !dir) // apply to database 
  {
    //MYX_GRT_VALUE *existing= myx_grt_dict_item_get_reference_value(grt, tree_node, "dbObject");
    //MYX_GRT_VALUE *altered= myx_grt_dict_item_get_reference_value(grt, tree_node, "modelObject");
    const char *struct_name= myx_grt_dict_struct_get_name(existing);
    int found= 0;

    // if this is column, index, fk then add the table to alter list
    if((strcmp2(struct_name, "db.mysql.Column") == 0) || 
      (strcmp2(struct_name, "db.mysql.Index") == 0) || 
      (strcmp2(struct_name, "db.mysql.ForeignKey") == 0) ||
      (strcmp2(struct_name, "db.mysql.Trigger") == 0)) 
    {
      item_to_add= myx_grt_dict_item_get_reference_value(grt, tree_node, "owner");
    }
    else
    {
      item_to_add= tree_node;
    }

    for(i= 0, count= myx_grt_list_item_count(alter_list); i < count; i++)
    {
      if(myx_grt_list_item_get(alter_list, i) == item_to_add)
      {
        found= 1;
        break;
      }
    }
    if(!found)
      myx_grt_list_item_add(alter_list, item_to_add);
  }

  if(children_list)
    for(i= 0, count= myx_grt_list_item_count(children_list); i < count; i++)
      process_tree_node(grt, myx_grt_list_item_get(children_list, i), alter_list);
}

MYX_GRT_VALUE *get_sql_changes(MYX_GRT_VALUE *param, void *data)
{
  int i, count;
  MYX_GRT *grt= (MYX_GRT *) data;
  MYX_GRT_VALUE *changes_tree= NULL;
  MYX_GRT_VALUE *alter_list= myx_grt_list_new(MYX_ANY_VALUE, NULL);
  
  if (!((myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) == 1)))
    return myx_grt_function_create_error_result("The submitted parameter has to be a list with one element.", 
      "TransformationMySQL:getSQLChanges(GrtObject changesTree);");

  changes_tree= myx_grt_list_item_get(param, 0);
  if (myx_grt_value_get_type(changes_tree) != MYX_DICT_VALUE)
    return myx_grt_function_create_error_result("The first parameter has to be a DICT.", NULL);

  // process the tree
  process_tree_node(grt, changes_tree, alter_list);

  for(i= 0, count= myx_grt_list_item_count(alter_list); i < count; i++)
  {
    MYX_GRT_VALUE *tree_node= myx_grt_list_item_get(alter_list, i);
    MYX_GRT_VALUE *existing= myx_grt_dict_item_get_reference_value(grt, tree_node, "dbObject");
    MYX_GRT_VALUE *altered= myx_grt_dict_item_get_reference_value(grt, tree_node, "modelObject");
    MYX_GRT_VALUE *list= myx_grt_list_new(MYX_ANY_VALUE, NULL);
    MYX_GRT_VALUE *sqldict= NULL;
    MYX_GRT_VALUE *sqlstr= NULL;

    // ok there's no NULL
    // so we go the following way:
    // if we have both altered and existing objects we push them both, so list contains 2 items
    // if we have only existing object we push it, so list contains only 1 item
    // if we have only altered object we push it 3 times, so list contains 3 items
    if(existing && altered)
    {
      myx_grt_list_item_add(list, altered);
      myx_grt_list_item_add(list, existing);
    }
    else if(existing)
    {
      myx_grt_list_item_add(list, existing);
    }
    else if(altered)
    {
      myx_grt_list_item_add(list, altered);
      myx_grt_list_item_add(list, altered);
      myx_grt_list_item_add(list, altered);
    }
    sqldict= get_sql_alter(list, grt);
    
    if(sqldict && (sqlstr= myx_grt_dict_item_get_value(sqldict, "value")))
    {
      if(existing)
        myx_grt_dict_item_set_value(existing, "sql", sqlstr);
      else
        myx_grt_dict_item_set_value(altered, "sql", sqlstr);
    }
  }

  return NULL;
}

static void set_changes_tree_error(MYX_GRT *grt, MYX_GRT_VALUE *changes_tree, const char *error)
{
  MYX_GRT_VALUE *log= myx_grt_dict_new(grt, "base.ObjectLog");
  MYX_GRT_VALUE *log_list= myx_grt_list_new(MYX_DICT_VALUE, "base.ObjectLogEntry");
  MYX_GRT_VALUE *log_list_entry= myx_grt_dict_new(grt, "base.ObjectLogEntry");

  myx_grt_dict_item_set_value(log_list_entry, "name", myx_grt_value_from_string(error));
  myx_grt_dict_item_set_value(log_list_entry, "entryType", myx_grt_value_from_int(2));
  myx_grt_list_item_add(log_list, log_list_entry);
  myx_grt_dict_item_set_value(log, "logObject", changes_tree);
  myx_grt_dict_item_set_value(log, "entries", log_list);
  myx_grt_dict_item_set_value(changes_tree, "syncLog", log);
}

void apply_changes_obj(MYX_GRT_VALUE *from, MYX_GRT_VALUE *to)
{
  const char *excl[] = {
    "_id", "owner", NULL
  };
  
  int i, j, count;

  // clean the target
restart:
  for(i= 0, count= myx_grt_dict_item_count(to); i < count; i++) 
  {
    int found = 0;
    const char *key;
    MYX_GRT_VALUE *prop;
    myx_grt_dict_item_by_index(to, i, &key, &prop);
    
    for(j= 0; excl[j] != NULL; j++)
    {
      if(strcmp2(key, excl[j]) == 0) 
      {
        found= 1;
        break;
      }
    }

    if(!found) 
    {
      myx_grt_dict_item_del(to, key);
      goto restart;
    }
  }

  for(i= 0, count= myx_grt_dict_item_count(from); i < count; i++) 
  {
    int found = 0;
    const char *key;
    MYX_GRT_VALUE *prop;
    myx_grt_dict_item_by_index(from, i, &key, &prop);
    
    for(j= 0; excl[j] != NULL; j++)
    {
      if(strcmp2(key, excl[j]) == 0) 
      {
        found= 1;
        break;
      }
    }

    if(!found) 
    {
      myx_grt_dict_item_set_value(to, key, prop);
    }
  }
}

typedef struct _PROCESS_SQL_CB_DATA
{
  MYSQL *mysql;
  MYX_GRT *grt;
  MYX_GRT_VALUE *changes_tree;
} PROCESS_SQL_CB_DATA;

static int process_sql_cb(const char *sql, void *data)
{
  PROCESS_SQL_CB_DATA *cb_data= (PROCESS_SQL_CB_DATA *)data;
  
  if(myx_mysql_query(cb_data->mysql, sql) != 0)
  {
    set_changes_tree_error(cb_data->grt, cb_data->changes_tree, myx_mysql_error(cb_data->mysql));
  }

  return 0;
}

// walk changes tree and apply all changes to schema, changes for inner nodes are applied first
// returns the modification status
static int apply_changes_tree(MYX_GRT *grt, MYX_GRT_VALUE *changes_tree, MYSQL *mysql)
{
  MYX_GRT_VALUE *children= myx_grt_dict_item_get_value(changes_tree, "children");
  MYX_GRT_VALUE *dbobj= NULL;
  MYX_GRT_VALUE *sqlobj= NULL;
  const char *sql= NULL;
  int changed= myx_grt_dict_item_get_as_int(changes_tree, "changed");
  int dir= myx_grt_dict_item_get_as_int(changes_tree, "alterDirection");
  int i, count;
  int children_changed= 0;

  if(children != NULL)
    for(i= 0, count= myx_grt_list_item_count(children); i < count; i++)
    {
      MYX_GRT_VALUE *item= myx_grt_list_item_get(children, i);
      children_changed += apply_changes_tree(grt, item, mysql);
    }

  dbobj= myx_grt_dict_item_get_reference_value(grt, changes_tree, "dbObject");
  sqlobj= myx_grt_dict_item_get_value(dbobj, "sql");
  if(sqlobj == NULL)
  {
    dbobj= myx_grt_dict_item_get_reference_value(grt, changes_tree, "modelObject");
    sqlobj= myx_grt_dict_item_get_value(dbobj, "sql");
  }

  // check if this is a table and there are changes in columns, etc
  if(children_changed) 
  {
    if(!myx_grt_dict_struct_inherits_from(grt, dbobj, "db.Table") &&
      strcmp(myx_grt_dict_struct_get_name(dbobj), "db.Table")) 
    {
      children_changed = 0;  
    }
  }

  if((sqlobj != NULL) && ((changed == 1) || (children_changed != 0))) 
  {
    if(dir == 0) 
    {
      sql= myx_grt_value_as_string(sqlobj);
      if(sql != NULL)
      {
        PROCESS_SQL_CB_DATA cb_data;
        cb_data.mysql= mysql;
        cb_data.grt= grt;
        cb_data.changes_tree= changes_tree;
        myx_process_sql_statements(sql, process_sql_cb, &cb_data, MYX_SPM_NORMAL_MODE);
      }
    }
    else
    {
      MYX_GRT_VALUE *modelobj= myx_grt_dict_item_get_reference_value(grt, changes_tree, "modelObject");
      apply_changes_obj(dbobj, modelobj);
    }
  }

  return changed;
}

MYX_GRT_VALUE *apply_sql_changes(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *) data;
  MYX_GRT_VALUE *changes_tree= NULL;
  MYX_GRT_VALUE *connection= NULL;
  MYSQL *mysql= NULL;
  MYX_GRT_VALUE *error= NULL;
  
  if (!((myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) == 2)))
    return myx_grt_function_create_error_result("The submitted parameter has to be a list with two elements.", 
      "TransformationMySQL:applySQLChanges(GrtObject changesTree, GrtObject connection);");

  changes_tree= myx_grt_list_item_get(param, 0);
  if (myx_grt_value_get_type(changes_tree) != MYX_DICT_VALUE)
    return myx_grt_function_create_error_result("The first parameter has to be a DICT.", NULL);

  connection= myx_grt_list_item_get(param, 1);
  if (myx_grt_value_get_type(connection) != MYX_DICT_VALUE)
    return myx_grt_function_create_error_result("The second parameter has to be a DICT.", NULL);

  mysql= grt_mysql_connect(connection, &error);
  if (!mysql)
  {
    // if the connection was not successful, return the error GRT value from connect_mysql()
    return error;
  }

  // process the tree
  apply_changes_tree(grt, changes_tree, mysql);

  return NULL;
}
