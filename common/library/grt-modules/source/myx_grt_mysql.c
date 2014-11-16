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


#include "myx_grt_mysql.h"

// --------------------------------------------------------------------------

/**
 ****************************************************************************
 * @brief Creates an error GRT value with the error from the given MYSQL struct
 *
 *   Creates a GRT value of type MYX_GRT_DICT, that contains a error and a detail
 * error message of the MySQL error.
 *
 * @param message the error message
 * @param mysql MYSQL struct that caused the error
 * 
 * @return  A newly created dict value struct containing the error information.
 *****************************************************************************/
MYX_GRT_VALUE *make_return_value_mysql_error(MYSQL *mysql, const char *message, const char *details)
{
  MYX_GRT_VALUE *value;
  char *mysql_error= g_strdup_printf("MySQL Error %d: %s", myx_mysql_errno(mysql), myx_mysql_error(mysql));
  //char *msg= NULL;

  if (!details)
    value= make_return_value_error(message, mysql_error);
  else
  {
    char *detail_str= g_strdup_printf("%s" _br _br "%s", details, mysql_error);

    value= make_return_value_error(message, detail_str);
    g_free(detail_str);
  }
  g_free(mysql_error);

  return value;
}

/**
 ****************************************************************************
 * @brief Creates an error GRT value with the error from the given MYSQL struct
 *
 *   Creates a GRT value of type MYX_GRT_DICT, that contains a error and a detail
 * error message of the MySQL error. Then closes the connection to the MySQL server
 *
 * @param message the error message
 * @param mysql MYSQL struct that caused the error
 * 
 * @return  A newly created dict value struct containing the error information.
 *****************************************************************************/
MYX_GRT_VALUE *make_return_value_mysql_error_and_close(MYSQL *mysql, const char *message, const char *sql)
{
  MYX_GRT_VALUE *value= make_return_value_mysql_error(mysql, message, sql);

  myx_mysql_close(mysql);

  return value;
}


/**
 ****************************************************************************
 * @brief Sets the parameters in a MYX_USER_CONNECTION struct
 *
 *   Sets the parameters in a MYX_USER_CONNECTION struct given in a GRT value.
 *
 * @param value the connection information stored in a GRT value
 * @param conn the connection struct
 *
 *****************************************************************************/
void get_connection_info(MYX_GRT_VALUE *value, MYX_USER_CONNECTION *conn)
{
  MYX_GRT_VALUE *port;
  const char *param;

  // if this is a "db.mgmt.Connection" struct, the parameters are in the parameterValues item
  if (strcmp2(myx_grt_dict_struct_get_name(value), "db.mgmt.Connection") == 0)
    value= myx_grt_dict_item_get_value(value, "parameterValues");

  param= myx_grt_dict_item_get_as_string(value, "username");
  if (param)
    conn->username= g_strdup(param);
  else
    conn->username= g_strdup("root");

  param= myx_grt_dict_item_get_as_string(value, "password");
  if (param)
    conn->password= g_strdup(param);
  else
    conn->password= NULL;

  param= myx_grt_dict_item_get_as_string(value, "host");

  if (param)
    conn->hostname= g_strdup(param);
  else
    conn->hostname= g_strdup("localhost");


  port= myx_grt_dict_item_get_value(value, "port");
  if (port)
  {
    if (myx_grt_value_get_type(port) == MYX_INT_VALUE)
      conn->port= myx_grt_value_as_int(port);
    else if (myx_grt_value_get_type(port) == MYX_STRING_VALUE)
      conn->port= atoi(myx_grt_value_as_string(port));
    else
      conn->port= 3306;
  }
  else
    conn->port= 3306;
}


/**
 ****************************************************************************
 * @brief Connects to a MySQL server
 *
 *   Connects to a MySQL server using the given connection parameters defined
 * in a GRT value.
 *
 * @param param the connection information stored in a GRT value
 * @param retval contains the error GRT value on failure, NULL on success
 * 
 * @return  Returns the MYSQL struct if successful
 *****************************************************************************/
MYSQL *grt_mysql_connect(MYX_GRT_VALUE *param, MYX_GRT_VALUE **retval)
{
  MYX_USER_CONNECTION conn;
  MYX_GRT_VALUE *value;
  MYSQL *mysql;

  memset(&conn, 0, sizeof(MYX_USER_CONNECTION));

  if ((myx_grt_value_get_type(param) == MYX_LIST_VALUE) && (myx_grt_list_item_count(param) == 1))
  {
    value= myx_grt_list_item_get(param, 0);
  }
  else if (myx_grt_value_get_type(param) == MYX_DICT_VALUE)
  {
    value= param;
  }
  else
  {
    *retval= make_return_value_error("Bad parameter submitted.", 
      "The connection parameters have to be submitted as list or dict.");
    return NULL;
  }

  get_connection_info(value, &conn);

  mysql= myx_mysql_init();
  if (!mysql)
  {
    *retval= make_return_value_error("Out of memory.", 
      "The memory for the MySQL connection information could not be created.");
    return NULL;
  }

  DBUG("connecting to mysql");

  if (myx_connect_to_instance(&conn, mysql) < 0)
  {
    *retval= make_return_value_mysql_error_and_close(mysql,
      "Can't connect to server. Please check the connection parameters.", NULL);
    return NULL;
  }

  *retval= NULL;

  return mysql;
}

/**
 ****************************************************************************
 * @brief Executes the given SQL statement and stores the result set
 *
 *   Executes the given SQL statement and stores the result set.
 *
 * @param mysql the connection information stored in a GRT value
 * @param sql the SQL command to execute.
 * @param error_msg the message text that will be returned in case of error
 * 
 * @return Returns the error GRT value on failure, NULL on success
 *****************************************************************************/
MYX_GRT_VALUE * grt_mysql_execute(MYSQL *mysql, MYSQL_RES **res, const char *sql, char *error_msg)
{
  MYX_GRT_VALUE *error= NULL;

  // execute SQL
  if (myx_mysql_query(mysql, sql) || !(*res= mysql_store_result(mysql)))
    error= make_return_value_mysql_error(mysql, error_msg, sql);

  return error;
}

/**
 ****************************************************************************
 * @brief Executes the given SQL statement and stores the result set
 *
 *   Executes the given SQL statement and stores the result set. The sql text
 * is freed after the execution.
 *
 * @param mysql the connection information stored in a GRT value
 * @param sql the SQL command to execute. Will be freed with g_free()
 * @param error_msg the message text that will be returned in case of error
 * 
 * @return Returns the error GRT value on failure, NULL on success
 *****************************************************************************/
MYX_GRT_VALUE * grt_mysql_execute_and_free(MYSQL *mysql, MYSQL_RES **res, char *sql, char *error_msg)
{
  MYX_GRT_VALUE *error= grt_mysql_execute(mysql, res, sql, error_msg);

  g_free(sql);

  return error;
}
