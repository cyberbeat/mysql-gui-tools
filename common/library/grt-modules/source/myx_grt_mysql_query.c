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

#include "myx_grt_mysql_query.h"
#include "myx_grt_mysql.h"


#define MAX_COLUMN_LENGTH 50

// --------------------------------------------------------------------------
// module registration function

MYX_GRT_MODULE* myx_register_builtin_grt_module_query_mysql(MYX_GRT *grt)
{
  return myx_grt_module_register_builtin(grt, &grt_module_query_mysql, grt);
}

// --------------------------------------------------------------------------

MYX_GRT_VALUE * conn_open(MYX_GRT_VALUE *param, void *data)
{
 // MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *value, *error= NULL;
  MYSQL *mysql;
  MYX_GRT_CONNECTION *mysql_conn;

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes a db.mgmt.Connection as parameter.", "");

  value= myx_grt_list_item_get(param, 0);

  mysql= grt_mysql_connect(value, &error);
  // if the connection was not successful, return the error GRT value from connect_mysql()
  if (!mysql)
    return error;

  mysql_conn= g_new0(MYX_GRT_CONNECTION, 1);
  mysql_conn->mysql= mysql;
  mysql_conn->ready= 1;

  // store MYSQL * in the bridge_data_object
  myx_grt_value_bridge_data_object_set(value, mysql_conn);

  return make_return_value(value);
}

MYX_GRT_VALUE * conn_close(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *value;
  MYX_GRT_CONNECTION *mysql_conn;

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes a db.query.mysql.Connection as parameter.", "");

  value= myx_grt_list_item_get(param, 0);

  // retrieve MYSQL * from the bridge_data_object
  mysql_conn= myx_grt_value_bridge_data_object_get(value);

  myx_grt_value_bridge_data_object_set(value, NULL);

  myx_mysql_close(mysql_conn->mysql);

  g_free(mysql_conn);

  return NULL;
}

static void fill_char(char *buffer, int ch, int count)
{
  int i;
  for (i= 0; i < count; i++)
    buffer[i]=ch;
  buffer[count]= 0;
}


MYX_GRT_VALUE * query_print(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *value, *error;
  const char *sql;

  MYX_GRT_CONNECTION *mysql_conn;
  MYSQL_RES *res;

  /*unsigned int i, field_count;
  char *s, *s2;*/

  unsigned int off;
  MYSQL_ROW cur;
  MYSQL_FIELD *field;
  unsigned int line_width= 4;
  char *line_sep;
  char *line;
  char *buf;
  int do_break= FALSE;
  char *tmp_utf8= g_malloc(MAX_COLUMN_LENGTH*2+1);
  unsigned long row_nr= 0;

  if (myx_grt_list_item_count(param) != 2)
    return make_return_value_error("This function takes (db.query.mysql.Connection, sql) as parameters.", "");

  value= myx_grt_list_item_get(param, 0);
  sql= myx_grt_list_item_get_as_string(param, 1);

  // retrieve MYSQL * from the bridge_data_object
  mysql_conn= myx_grt_value_bridge_data_object_get(value);

  if (!mysql_conn)
    return make_return_value_error("The connection has not been established. Use Rdbms:connect() to open the connection.", "");

  error= grt_mysql_execute(mysql_conn->mysql, &res, sql, "Could not execute the given statement.");
  if (error)
  {
    myx_grt_messages_stack_add(grt, 0, myx_grt_dict_item_get_as_string(error, "error"), NULL, 0, -1);
    myx_grt_messages_stack_add(grt, 0, myx_grt_dict_item_get_as_string(error, "detail"), NULL, 0, -1);

    myx_grt_messages_stack_flush(grt, 0);

    return error;
  }

  // ------------------------------------------
  // print fields

  // calc line width
  while ((field= mysql_fetch_field(res)))
  {
#if MYSQL_VERSION_ID < 40100
    unsigned int length= (field->name != 0 ? strnlen(field->name, field->max_length) : 0);
#else
    unsigned int length= field->name_length;
#endif

    length= max(length,field->max_length);
    length*= 2; //consider UTF8 2-byte chars
    if (length < 4 && !IS_NOT_NULL(field->flags))
      length=4;                                 // Room for "NULL"
    field->max_length=length+1;

    line_width+= length+3;
  }

  // build line separator
  line_sep= g_malloc(sizeof(char)*(line_width+1));
  strcpy(line_sep, "+");
  mysql_field_seek(res, 0);
  while ((field= mysql_fetch_field(res)))
  {
    fill_char(line_sep+strlen(line_sep), '-', min((int)field->max_length+1, MAX_COLUMN_LENGTH+1));
    strcat(line_sep,"+");
  }
  myx_grt_messages_stack_add(grt, 0, line_sep, NULL, 0, -1);

  // output column names
  mysql_field_seek(res,0);

  line= g_strdup("|");
  buf= g_malloc(MAX_COLUMN_LENGTH * 2 + 1);

  while ((field= mysql_fetch_field(res)))
  {
    char *field_name= myx_convert_dbstr_utf8(mysql_conn->mysql, field->name, field->name_length);

    sprintf(buf, " %-*s|", min((int)field->max_length, MAX_COLUMN_LENGTH),
              field_name);

    line= str_g_append(line, buf);

    g_free(field_name);
  }

  g_free(buf);

  myx_grt_messages_stack_add(grt, 0, line, NULL, 0, -1);

  g_free(line);

  myx_grt_messages_stack_add(grt, 0, line_sep, NULL, 0, -1);


  line= g_malloc(sizeof(char)*(line_width+1));
  
  // output rows
  while ((cur= mysql_fetch_row(res)) && (!do_break))
  {
    char *line_end= line;
    strcpy(line, "|");
    line_end++;

    mysql_field_seek(res, 0);
    for (off= 0; off < mysql_num_fields(res); off++)
    {
      unsigned int length, u8length, clength;
      char *field_value;
      
      if(cur[off])
        field_value= myx_convert_dbstr_utf8(mysql_conn->mysql, cur[off], -1);
      else
        field_value= g_strdup("NULL");

      field_value= str_g_replace(field_value, "\r\n", "\xc2\xab\xc2\xb6");
      field_value= str_g_replace(field_value, "\n", "\xc2\xb6");

      field= mysql_fetch_field(res);
      length= field->max_length;
      
      // compensage difference between bytecount and utf8 char count
      clength= (unsigned int)strlen(field_value);
      u8length= g_utf8_strlen(field_value, clength);
      length+= clength-u8length;

      if (u8length > MAX_COLUMN_LENGTH)
      {
        //Clear buffer
        memset(tmp_utf8, 0, MAX_COLUMN_LENGTH*2+1);

        tmp_utf8= g_utf8_strncpy(tmp_utf8, field_value, MAX_COLUMN_LENGTH-1);

        strcpy(line_end, " ");
        line_end++;
        g_utf8_strncpy(line_end, field_value, MAX_COLUMN_LENGTH-1);
        line_end+= strlen(tmp_utf8);
        strcpy(line_end, ">|");
        line_end+= 2;
      }
      else
      {
        line_end+= sprintf(line_end, IS_NUM(field->type) ? "%*s |" : " %-*s|",
                min(length, MAX_COLUMN_LENGTH+clength-u8length), field_value);
        //line_end+= strlen(line_end);
      }

      g_free(field_value);
    }

    myx_grt_messages_stack_add(grt, 0, line, NULL, 0, -1);

    row_nr++;

    if (row_nr % 20 == 0)
      myx_grt_messages_stack_flush(grt, 0);
  }

  if(!do_break)
    myx_grt_messages_stack_add(grt, 0, line_sep, NULL, 0, -1);
  else
  {
    myx_grt_messages_stack_add(grt, 0, "Ctrl+C pressed, cleaning up buffer ...\n", NULL, 0, -1);

    //Finish fetching rows
    while ((cur= mysql_fetch_row(res)))
      ;

    myx_grt_messages_stack_add(grt, 0, "Buffer cleaned up.", NULL, 0, -1);
  }
  g_free(line_sep);
  g_free(tmp_utf8);

  myx_grt_messages_stack_flush(grt, 0);

  return NULL;
}

MYX_GRT_VALUE * query_fetch_result_set(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *conn, *query, *result_set, *columns, *args, *rows, *error_val;
  const char *result_set_id;
  MYX_GRT_ERROR error;

  MYX_GRT_CONNECTION *mysql_conn;
  MYSQL_RES *res;
  MYSQL_ROW row;
  MYSQL_FIELD *fields;
  unsigned long *field_lengths;
  unsigned int i, field_count;
  //char *s= NULL;

  char *sql;
  int row_block_size, limit_start;
  

  result_set= myx_grt_list_item_get(param, 0);
  query= myx_grt_dict_item_get_reference_value(grt, result_set, "query");
  conn= myx_grt_dict_item_get_reference_value(grt, query, "connection");

  // retrieve MYSQL * from the bridge_data_object
  mysql_conn= myx_grt_value_bridge_data_object_get(conn);

  if (!mysql_conn || !mysql_conn->mysql)
    return make_return_value_error("The connection has not been established. Use Rdbms:connect() to open the connection.", "");

  // If rowBlockSize is -1, a streaming result set is returned. If set to 0 the complete result set
  // will be cached. If set to a value greater than 0, the row blocksize is set to that value and
  // the first block is fetched
  row_block_size= myx_grt_dict_item_get_as_int(result_set, "rowBlockSize");
  limit_start= myx_grt_dict_item_get_as_int(result_set, "nextBlockStartRow");

  // if this resultset is already at the very end, do not fetch another block
  if (limit_start == -1)
    return NULL;

  // ToDo: only add LIMIT to statements that return a resultset
  sql= g_strdup(myx_grt_dict_item_get_as_string(query, "sql"));
  if (row_block_size > 0)
  {
    sql= str_g_append_and_free(sql, g_strdup_printf(" LIMIT %d, %d", limit_start, row_block_size));
  }

  if ((error_val= grt_mysql_execute(mysql_conn->mysql, &res, sql, "Could not execute the given statement.")))
  {
    g_free(sql);
    return error_val;
  }
  g_free(sql);

  field_count= mysql_num_fields(res);

  // ------------------------------------------
  // get columns
  result_set_id= myx_grt_dict_id_item_as_string(result_set);
  columns= myx_grt_dict_item_get_value(result_set, "columns");
  if (myx_grt_list_item_count(columns) == 0)
  {
    fields= mysql_fetch_fields(res);

    for(i = 0; i < field_count; i++)
    {
      MYX_GRT_VALUE *col;
      char *val= myx_convert_dbstr_utf8(mysql_conn->mysql, fields[i].name, fields[i].name_length);

      col= myx_grt_dict_new_obj(grt, "db.query.ResultsetColumn", val, "", result_set_id);
      g_free(val);

      val= g_strdup_printf("%lu", fields[i].length);
      myx_grt_dict_item_set_value_from_string(col, "length", val);
      g_free(val);

      myx_grt_dict_item_set_value_from_int(col, "displayDecimals", fields[i].decimals);

      if (fields[i].flags & PRI_KEY_FLAG)
        myx_grt_dict_item_set_value_from_int(col, "isPrimaryKey", 1);
      else
        myx_grt_dict_item_set_value_from_int(col, "isPrimaryKey", 0);

      if ((fields[i].type == MYSQL_TYPE_TINY) ||
        (fields[i].type == MYSQL_TYPE_SHORT) ||
        (fields[i].type == MYSQL_TYPE_LONG) ||
        (fields[i].type == MYSQL_TYPE_INT24) ||
        (fields[i].type == MYSQL_TYPE_LONGLONG)
#if MYSQL_VERSION_ID >= 50000
        || (fields[i].type == MYSQL_TYPE_BIT)
#endif
          )
        myx_grt_dict_item_set_value_from_string(col, "columnType", "int");
      else if ((fields[i].type == MYSQL_TYPE_DECIMAL) ||
#if MYSQL_VERSION_ID >= 50000
        (fields[i].type == MYSQL_TYPE_NEWDECIMAL) ||
#endif
        (fields[i].type == MYSQL_TYPE_FLOAT) ||
        (fields[i].type == MYSQL_TYPE_DOUBLE))
        myx_grt_dict_item_set_value_from_string(col, "columnType", "real");
      else if ((fields[i].type == MYSQL_TYPE_TIMESTAMP) ||
        (fields[i].type == MYSQL_TYPE_DATETIME))
        myx_grt_dict_item_set_value_from_string(col, "columnType", "datetime");
      else if ((fields[i].type == MYSQL_TYPE_DATE) ||
          (fields[i].type == MYSQL_TYPE_YEAR))
        myx_grt_dict_item_set_value_from_string(col, "columnType", "date");
      else if (fields[i].type == MYSQL_TYPE_TIME)
        myx_grt_dict_item_set_value_from_string(col, "columnType", "time");
      else if ((fields[i].type == MYSQL_TYPE_STRING) ||
        (fields[i].type == MYSQL_TYPE_VAR_STRING) ||
        (fields[i].type == MYSQL_TYPE_SET) ||
        (fields[i].type == MYSQL_TYPE_ENUM))
        myx_grt_dict_item_set_value_from_string(col, "columnType", "string");
      else if (fields[i].type == MYSQL_TYPE_BLOB)
        myx_grt_dict_item_set_value_from_string(col, "columnType", "blob");
      else if (fields[i].type == MYSQL_TYPE_GEOMETRY)
        myx_grt_dict_item_set_value_from_string(col, "columnType", "geo");

      myx_grt_list_item_add(columns, col);
    }
  }

  // ------------------------------------------
  // get rows

  args= myx_grt_list_new(MYX_ANY_VALUE, "");
  myx_grt_list_item_add(args, result_set);

  rows= myx_grt_list_new(MYX_LIST_VALUE, "");
  myx_grt_list_item_add(args, rows);
  myx_grt_value_release(rows);

  while ((row= mysql_fetch_row(res)))
  {
    MYX_GRT_VALUE *cols= myx_grt_list_new(MYX_LIST_VALUE, "");
    myx_grt_list_item_add(rows, cols);
    myx_grt_value_release(cols);

    field_lengths= mysql_fetch_lengths(res);

    for(i = 0; i < field_count; i++)
    {
      MYX_GRT_VALUE *col= myx_grt_list_new(MYX_ANY_VALUE, "");
      MYX_GRT_VALUE *col_val;
      char *val;
        
      val= myx_convert_dbstr_utf8(mysql_conn->mysql, row[i], -1);
      col_val= myx_grt_value_from_string(val);
      myx_grt_list_item_add(col, col_val);
      myx_grt_value_release(col_val);
      g_free(val);

      val= g_strdup_printf("%ld", field_lengths[i]);
      col_val= myx_grt_value_from_string(val);
      myx_grt_list_item_add(col, col_val);
      myx_grt_value_release(col_val);
      g_free(val);

      myx_grt_list_item_add(cols, col);
      myx_grt_value_release(col);
    }   
  }
  // add rows
  myx_grt_function_get_and_call(grt, "RdbmsResultSet", "rowsAdd", 0, args, &error);

  myx_grt_value_release(args);

  mysql_free_result(res);

  if (error)
    return make_return_value_error("The RdbmsResultSet:rowsAdd() function could not be called", "");

  return NULL;
}
