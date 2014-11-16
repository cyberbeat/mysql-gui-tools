/* Copyright (C) 2005 MySQL AB

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

#include "MYXMySQLResultSetSource.h"
/**
 * @file  MYXMySQLResultSetSource.cc
 * @brief 
 */



MYXMySQLResultSetSource::MYXMySQLResultSetSource()
{
  _result= 0;
  _row= 0;
  _connected= false;
  _last_errno= 0;
  _last_error= 0;
#if MYSQL_VERSION_ID >= 50000
  _stmt= 0;
  _stmt_active= true;
  _stmt_results= 0;
#endif
}


MYXMySQLResultSetSource::~MYXMySQLResultSetSource()
{
  g_free(_hostname);
  g_free(_password);
  g_free(_username);
  g_free(_socket_path);
  
#if MYSQL_VERSION_ID >= 50000
  if (_stmt)
    mysql_stmt_close(_stmt);
  g_free(_stmt_results);
#endif
  mysql_free_result(_result);
  if (_connected)
    mysql_close(&_mysql);
  g_free(_last_error);
    
  for (std::list<char*>::iterator iter= _schema_stack.begin();
       iter != _schema_stack.end(); ++iter)
    g_free(*iter);
}
  


bool MYXMySQLResultSetSource::connect(const char *hostname, int port,
                                      const char *username,
                                      const char *password,
                                      const char *socket_path)
{
  _hostname= g_strdup(hostname);
  _password= g_strdup(password);
  _username= g_strdup(username);
  _socket_path= g_strdup(socket_path);
  _port= port;
  
  if (hostname && !_hostname)
    return false;
  
  if (password && !_password)
    return false;
  
  if (username && !_username)
    return false;
  
  if (socket_path && !_socket_path)
    return false;
  
  clear_error();
  
  mysql_init(&_mysql);

  if (mysql_real_connect(&_mysql, hostname, username, password, NULL, port, socket_path, 0) != 0)
  {
    catch_mysql_error();
    return false;
  }
  
  mysql_query(&_mysql, "SET SESSION interactive_timeout=1000000");

  unsigned long version= get_server_version();
  
  //using SET NAMES utf8;
  if ((version >= 40108 && version < 50000) || version >= 50002)
  {
    if (mysql_query(&_mysql, "SET NAMES utf8"))
    {
      catch_mysql_error();
      return false;
    }
  }
  //using SET CHARACTER SET utf8;
  else if (version >= 40100)
  {
    if (mysql_query(&_mysql, "SET CHARACTER SET utf8"))
    {
      catch_mysql_error();
      return false;
    }
  }

  _connected= true;

  return true;
}


void MYXMySQLResultSetSource::clear_error()
{
  g_free(_last_error);
  _last_error= 0;
  _last_errno= 0;
}


void MYXMySQLResultSetSource::catch_mysql_error()
{
  if (_last_error) g_free(_last_error);

  _last_error= g_strdup(mysql_error(&_mysql));
  _last_errno= mysql_errno(&_mysql);
  
  if (getenv("DEBUG") && _last_errno)
    printf("MySQL Error: %s\n", _last_error);
}


bool MYXMySQLResultSetSource::execute(const char *query, size_t length)
{
  clear_error();
  
  if (mysql_real_query(&_mysql, query, length) != 0)
  {
    catch_mysql_error();
    return false;
  }

  _result= mysql_use_result(&_mysql);
  if (!_result)
  {
    if (mysql_field_count(&_mysql) != 0)
    {
      catch_mysql_error();
      return false;
    }
  }

  if (_result)
    _num_columns= mysql_num_fields(_result);
  else
    _num_columns= 0;

  return true;
}


void MYXMySQLResultSetSource::push_schema(const char *schema)
{
  _schema_stack.push_back(g_strdup(schema));
  mysql_select_db(&_mysql, schema);
}


void MYXMySQLResultSetSource::pop_schema()
{
  mysql_select_db(&_mysql, *_schema_stack.begin());
  g_free(*_schema_stack.begin());
  _schema_stack.erase(_schema_stack.begin());
}


static char *field_names[]= {
  "Field",
    "Type",
    "Null",
    "Key",
    "Default",
    "Extra",
    NULL
};



std::vector<MYXResultSetSource::ColumnInfo*> MYXMySQLResultSetSource::get_columns()
{
  return _columns;
}


std::vector<MYXResultSetSource::ColumnInfo*> MYXMySQLResultSetSource::get_columns_stmt()
{
  std::vector<ColumnInfo*> columns;
#if MYSQL_VERSION_ID >= 50000
  MYSQL_ROW row;
  MYSQL mysql;
  MYSQL_RES *res;
  char *query;
  int fields[6];

  g_return_val_if_fail(_stmt != NULL && _stmt_active, columns);
  
  mysql_init(&mysql);
  
  if (mysql_real_connect(&mysql, _hostname, _username, _password, NULL, _port, _socket_path, 0) != 0)
  {
    mysql_close(&mysql);
    return columns;
  }

  query= g_strdup_printf("SHOW COLUMNS FROM `%s`.`%s`",
                         columns[0]->schema, columns[0]->table);
  if (!query)
  {
    mysql_close(&mysql);
    return columns;
  }
  if (mysql_real_query(&mysql, query, strlen(query)) != 0)
  {
    g_free(query);
    mysql_close(&mysql);
    return columns;
  }
  g_free(query);
  res= mysql_store_result(&mysql);
  if (!res)
  {
    mysql_close(&mysql);
    return columns;
  }
  
  for (int i= 0; field_names[i]; i++)
  {
    for (fields[i]= 0; strcmp(field_names[i], res->fields[fields[i]].name)!=0; fields[i]++);
  }

  while ((row= mysql_fetch_row(res)))
  {
    for (unsigned int i= 0; i < columns.size(); i++)
    {
      if (strcmp(columns[i]->name, row[fields[0]])==0)
      {
        if (strcmp2(row[fields[3]], "PRI")==0)
          columns[i]->is_pk= 1;
        else
          columns[i]->is_pk= 0;
        if (strcmp2(row[fields[2]], "YES")==0)
          columns[i]->not_null= 0;
        else
          columns[i]->not_null= 1;
        
        columns[i]->type_name= g_strdup(row[fields[1]]);

        columns[i]->is_autoincrement= strstr(row[fields[5]], "auto_increment")!=0;
        break;
      }
    }
  }
  mysql_free_result(res);
  mysql_close(&mysql);
#endif
  return columns;
}


std::vector<MYXResultSetSource::ColumnInfo*> MYXMySQLResultSetSource::get_columns_normal()
{
  std::vector<ColumnInfo*> columns;
  ColumnInfo *info;
  MYSQL_FIELD *field;
  int fields[6]= {0};

  g_return_val_if_fail(_result!=NULL, columns);

  
  while ((field= mysql_fetch_field(_result)))
  {
    info= new ColumnInfo;
    
#if MYSQL_VERSION_ID >= 50100
    info->catalog= g_strdup(field->catalog);
#else
    info->catalog= NULL;
#endif
    info->schema= g_strdup(field->db);
    info->table= g_strdup(field->table);
    info->name= g_strdup(field->name);

    info->length= field->length;
    info->display_length= field->max_length;
    info->display_decimals= field->decimals;

    if (
#if MYSQL_VERSION_ID >= 50100
        (!info->catalog && field->catalog) ||
#endif
        (!info->schema && field->db) ||
        (!info->table && field->table) ||
        (!info->name && field->name))
    {
      columns.clear();
      _last_errno= -1;
      if (_last_error) g_free(_last_error);
      _last_error= NULL;
      return columns;
    }

    switch (field->type)
    {
      //To be completed
    case FIELD_TYPE_TINY:
    case FIELD_TYPE_SHORT: 
    case FIELD_TYPE_LONG:
    case FIELD_TYPE_LONGLONG:
    case FIELD_TYPE_INT24:
      info->type= MYX_RSCT_INTEGER;
      break;
      //case FIELD_TYPE_DECIMAL: is handled as a string
    case FIELD_TYPE_FLOAT:
    case FIELD_TYPE_DOUBLE:
      info->type= MYX_RSCT_DOUBLE;
      break;
    case FIELD_TYPE_DECIMAL:
      info->type= MYX_RSCT_DECIMAL;
      break;
    case FIELD_TYPE_BLOB:
    case FIELD_TYPE_TINY_BLOB:
    case FIELD_TYPE_MEDIUM_BLOB:
    case FIELD_TYPE_LONG_BLOB:
      if (field->flags & BINARY_FLAG)
        info->type= MYX_RSCT_BLOB;
      else
        info->type= MYX_RSCT_TEXT;
      break;
    case FIELD_TYPE_DATE:
      info->type= MYX_RSCT_DATE;
      break;
    case FIELD_TYPE_TIME:
      info->type= MYX_RSCT_TIME;
      break;
    case FIELD_TYPE_DATETIME:
      info->type= MYX_RSCT_DATETIME;
      break;
    case FIELD_TYPE_ENUM:
      info->type= MYX_RSCT_ENUM;
      break;
    case FIELD_TYPE_SET:
      info->type= MYX_RSCT_SET;
      break;
    default:
      info->type= MYX_RSCT_STRING;
      break;
    }
    columns.push_back(info);
  }

  MYSQL mysql;
  MYSQL_RES *res;
  MYSQL_ROW row;
  char *query;
  
  mysql_init(&mysql);
  
  if (mysql_real_connect(&mysql, _hostname, _username, _password, NULL, _port, _socket_path, 0) != 0)
  {
    mysql_close(&mysql);
    return columns;
  }

  query= g_strdup_printf("SHOW COLUMNS FROM `%s`.`%s`",
                         columns[0]->schema, columns[0]->table);
  if (!query)
  {
    mysql_close(&mysql);
    return columns;
  }
  if (mysql_real_query(&mysql, query, strlen(query)) != 0)
  {
    g_free(query);
    mysql_close(&mysql);
    return columns;
  }
  g_free(query);
  res= mysql_store_result(&mysql);
  if (!res)
  {
    mysql_close(&mysql);
    return columns;
  }
  
  for (int i= 0; field_names[i]; i++)
  {
    for (fields[i]= 0; strcmp(field_names[i], res->fields[fields[i]].name)!=0; fields[i]++);
  }

  while ((row= mysql_fetch_row(res)))
  {
    for (unsigned int i= 0; i < columns.size(); i++)
    {
      if (strcmp(columns[i]->name, row[fields[0]])==0)
      {
        if (strcmp2(row[fields[3]], "PRI")==0)
          columns[i]->is_pk= 1;
        else
          columns[i]->is_pk= 0;
        if (strcmp2(row[fields[2]], "YES")==0)
          columns[i]->not_null= 0;
        else
          columns[i]->not_null= 1;
        
        columns[i]->type_name= g_strdup(row[fields[1]]);

        columns[i]->is_autoincrement= strstr(row[fields[5]], "auto_increment")!=0;
        break;
      }
    }
  }
  mysql_free_result(res);
  mysql_close(&mysql);
  
  return columns;
}


bool MYXMySQLResultSetSource::fetch_stmt_results()
{
  return false;
}


bool MYXMySQLResultSetSource::next()
{
  clear_error();

#if MYSQL_VERSION_ID >= 50000
  if (_stmt_active)
  {
    switch (mysql_stmt_fetch(_stmt))
    {
    case 0:
      if (!fetch_stmt_results())
      {
        _row= 0;
        return false;
      }
      break;
    case 1: // error
      _last_error= g_strdup(mysql_stmt_error(_stmt));
      _last_errno= mysql_stmt_errno(_stmt);
      _row= 0;
      return false;
      break;
    case MYSQL_NO_DATA:
      _row= 0;
      break;
    default:
      _row= 0;
      _last_error= g_strdup(mysql_stmt_error(_stmt));
      _last_errno= mysql_stmt_errno(_stmt);
      break;
    }
  }
  else
#endif
  {
    _row= mysql_fetch_row(_result);
    _row_lengths= mysql_fetch_lengths(_result);
  }
  return _row != 0;
}


bool MYXMySQLResultSetSource::eof()
{
  return _row == 0;
}


bool MYXMySQLResultSetSource::get_row(const char **values, size_t *lengths)
{
  clear_error();
  
  if (_row)
  {
    for (int i= 0; i < _num_columns; i++)
    {
      values[i]= _row[i];
      lengths[i]= _row_lengths[i];
    }
    return true;
  }
  return false;
}


bool MYXMySQLResultSetSource::get_value(int column, const char *&value, size_t &length)
{
  clear_error();

  if (_row && column < _num_columns)
  {
    value= _row[column];
    length= _row_lengths[column];
    return true;
  }
  return false;
}


void MYXMySQLResultSetSource::reset()
{
  clear_error();

  if (_result)
    mysql_free_result(_result);
  _result= 0;
  _row= 0;

  for (unsigned int i= 0; i < _columns.size(); i++)
    delete _columns[i];
  _columns.clear();


#if MYSQL_VERSION_ID >= 50000
  if (_stmt)
    mysql_stmt_close(_stmt);
  _stmt= 0;
  _stmt_active= false;
#endif
}



bool MYXMySQLResultSetSource::prepare(const char *query)
{
#if MYSQL_VERSION_ID >= 50000
  g_return_val_if_fail(_stmt != 0, false);
  
  _stmt= mysql_stmt_init(&_mysql);
  _stmt_active= true;
  
  if (mysql_stmt_prepare(_stmt, query, strlen(query)) != 0)
  {
    catch_mysql_error();
    return false;
  }
  return true;
#else
  return false;
#endif
}


bool MYXMySQLResultSetSource::execute_prepared(QueryParameter *params, int paramCount)
{
#if MYSQL_VERSION_ID >= 50000
  MYSQL_BIND *p;
  MYSQL_TIME timebuf;
  MYSQL_FIELD *field;
  unsigned long length;
  my_bool is_null;

  g_return_val_if_fail(_stmt != 0, false);
  g_return_val_if_fail((int)mysql_stmt_param_count(_stmt) == paramCount, false);
  
  p= new MYSQL_BIND[paramCount];
  
  for (int i= 0; i < paramCount; i++)
  {
    memset(&p, 0, sizeof(p));
    
    is_null= params[i].is_null;
    
    switch (params[i].type)
    {
    case MYX_RSCT_INTEGER:
      p[i].buffer_type= MYSQL_TYPE_LONG;
      if (params[i].master)
      {
      }
      else
      {
        p[i].buffer= (void*)&params[i].value_int;
        p[i].is_null= &is_null;
        p[i].is_unsigned= 0;
        p[i].length= 0;
      }
      break;
    case MYX_RSCT_BIGINT:
      p[i].buffer_type= MYSQL_TYPE_LONGLONG;
      if (params[i].master)
      {
      }
      else
      {
        p[i].buffer= (void*)&params[i].value_bigint;
        p[i].is_null= &is_null;
        p[i].is_unsigned= 0;
        p[i].length= 0;
      }
      break;
    case MYX_RSCT_DOUBLE:
      p[i].buffer_type= MYSQL_TYPE_DOUBLE;
      if (params[i].master)
      {
      }
      else
      {
        p[i].buffer= (void*)&params[i].value_double;
        p[i].is_null= &is_null;
        p[i].length= 0;
      }
      break;
    case MYX_RSCT_STRING:
      p[i].buffer_type= MYSQL_TYPE_STRING;
      if (params[i].master)
      {
      }
      else
      {
        p[i].buffer= (void*)params[i].value;
        length= params[i].value_length;
        p[i].length= &length;
        p[i].is_null= &is_null;
      }
      break;
    case MYX_RSCT_DATE:
      p[i].buffer_type= MYSQL_TYPE_DATE;
      if (params[i].master)
      {
      }
      else
      {
        memset(&timebuf, 0, sizeof(timebuf));
        timebuf.year= params[i].value_time.year;
        timebuf.month= params[i].value_time.month;
        timebuf.day= params[i].value_time.day;
        p[i].buffer= (void*)&timebuf;
        p[i].length= 0;
        p[i].is_null= &is_null;
      }
      break;
    case MYX_RSCT_TIME:
      p[i].buffer_type= MYSQL_TYPE_TIME;
      if (params[i].master)
      {
      }
      else
      {
        memset(&timebuf, 0, sizeof(timebuf));
        timebuf.hour= params[i].value_time.hour;
        timebuf.minute= params[i].value_time.minute;
        timebuf.second= params[i].value_time.second;
        timebuf.second_part= params[i].value_time.second_frac;
        p[i].buffer= (void*)&params[i].value_time;
        p[i].length= 0;
        p[i].is_null= &is_null;
      }
      break;
    case MYX_RSCT_DATETIME:
      p[i].buffer_type= MYSQL_TYPE_DATETIME;
      if (params[i].master)
      {
      }
      else
      {
        memset(&timebuf, 0, sizeof(timebuf));
        timebuf.year= params[i].value_time.year;
        timebuf.month= params[i].value_time.month;
        timebuf.day= params[i].value_time.day;
        timebuf.hour= params[i].value_time.hour;
        timebuf.minute= params[i].value_time.minute;
        timebuf.second= params[i].value_time.second;
        timebuf.second_part= params[i].value_time.second_frac;
        p[i].buffer= (void*)&params[i].value_time;
        p[i].length= 0;
        p[i].is_null= &is_null;
      }
      break;
    case MYX_RSCT_BLOB:
      p[i].buffer_type= MYSQL_TYPE_BLOB;
      if (params[i].master)
      {
      }
      else
      {
        p[i].buffer= (void*)params[i].value;
        length= params[i].value_length;
        p[i].length= &length;
        p[i].is_null= &is_null;
      }
      break;
    case MYX_RSCT_TEXT:
      p[i].buffer_type= MYSQL_TYPE_BLOB;
      if (params[i].master)
      {
      }
      else
      {
        p[i].buffer= (void*)params[i].value;
        length= params[i].value_length;
        p[i].length= &length;
        p[i].is_null= &is_null;
      }
      break;
    case MYX_RSCT_ENUM:
      g_return_val_if_fail(params[i].type != MYX_RSCT_ENUM, false);
      //p[i].buffer_type= 
      break;
    case MYX_RSCT_SET:
      g_return_val_if_fail(params[i].type != MYX_RSCT_SET, false);
      break;
    case MYX_RSCT_DECIMAL:
      g_return_val_if_fail(params[i].type != MYX_RSCT_DECIMAL, false);
      break;
    default:
      g_message("Unsupported parameter type for prepared query");
      return false;
    }
  }
  if (mysql_stmt_bind_param(_stmt, p) != 0)
  {
    delete[] p;
    return false;
  }

  delete[] p;
  
  _stmt_metadata= mysql_stmt_result_metadata(_stmt);
  if (!_stmt_metadata)
    return false;

  int c= mysql_num_fields(_stmt_metadata);
  _stmt_results= new MYSQL_BIND[c];
  int i= 0;
  
  while ((field= mysql_fetch_field(_result)))
  {
    ColumnInfo *info= new ColumnInfo;
    ResultColumnData *cdata= new ResultColumnData;
    
    info->catalog= g_strdup(field->catalog);
    info->schema= g_strdup(field->db);
    info->table= g_strdup(field->table);
    info->name= g_strdup(field->name);

    info->length= field->length;
    info->display_length= field->max_length;
    info->display_decimals= field->decimals;

    if ((!info->catalog && field->catalog) ||
        (!info->schema && field->db) ||
        (!info->table && field->table) ||
        (!info->name && field->name))
    {
      _last_errno= -1;
      if (_last_error) g_free(_last_error);
      _last_error= NULL;
      return false;
    }

    cdata->buffer_length= 0;

    switch (field->type)
    {
      //To be completed
    case FIELD_TYPE_TINY:
    case FIELD_TYPE_SHORT:
    case FIELD_TYPE_LONG:
      
    case FIELD_TYPE_INT24:
      _stmt_results[i].buffer_type= MYSQL_TYPE_LONG;
      info->type= MYX_RSCT_INTEGER;
      break;
    case FIELD_TYPE_LONGLONG:
      _stmt_results[i].buffer_type= MYSQL_TYPE_LONGLONG;
      info->type= MYX_RSCT_BIGINT;
      break;
      //case FIELD_TYPE_DECIMAL: is handled as a string
    case FIELD_TYPE_FLOAT:
    case FIELD_TYPE_DOUBLE:
      _stmt_results[i].buffer_type= MYSQL_TYPE_DOUBLE;
      info->type= MYX_RSCT_DOUBLE;
      break;
    case FIELD_TYPE_DECIMAL:
      _stmt_results[i].buffer_type= MYSQL_TYPE_DECIMAL;
      info->type= MYX_RSCT_DECIMAL;
      break;
    case FIELD_TYPE_BLOB:
    case FIELD_TYPE_TINY_BLOB:
    case FIELD_TYPE_MEDIUM_BLOB:
    case FIELD_TYPE_LONG_BLOB:
      if (field->flags & BINARY_FLAG)
        info->type= MYX_RSCT_BLOB;
      else
        info->type= MYX_RSCT_TEXT;
      _stmt_results[i].buffer_type= MYSQL_TYPE_BLOB;

      cdata->buffer_length= field->max_length + 1;
      cdata->data.buffer_value= (char*)g_malloc(cdata->buffer_length);
      break;
    case FIELD_TYPE_DATE:
      info->type= MYX_RSCT_DATE;
      _stmt_results[i].buffer_type= MYSQL_TYPE_DATETIME;
      break;
    case FIELD_TYPE_TIME:
      info->type= MYX_RSCT_TIME;
      _stmt_results[i].buffer_type= MYSQL_TYPE_DATETIME;
      break;
    case FIELD_TYPE_DATETIME:
      info->type= MYX_RSCT_DATETIME;
      _stmt_results[i].buffer_type= MYSQL_TYPE_DATETIME;
      break;
    case FIELD_TYPE_ENUM:
      info->type= MYX_RSCT_ENUM;
      _stmt_results[i].buffer_type= MYSQL_TYPE_STRING;
     
      cdata->buffer_length= field->max_length + 1;
      cdata->data.buffer_value= (char*)g_malloc(cdata->buffer_length);
      break;
    case FIELD_TYPE_SET:
      info->type= MYX_RSCT_SET;
      _stmt_results[i].buffer_type= MYSQL_TYPE_STRING;

      cdata->buffer_length= field->max_length + 1;
      cdata->data.buffer_value= (char*)g_malloc(cdata->buffer_length);
      break;
    default:
      info->type= MYX_RSCT_STRING;
      _stmt_results[i].buffer_type= MYSQL_TYPE_STRING;
      
      cdata->buffer_length= field->max_length + 1;
      cdata->data.buffer_value= (char*)g_malloc(cdata->buffer_length);
      break;
    }
    
    if (cdata->buffer_length > 0 && !cdata->data.buffer_value)
    {
      delete info;
      delete cdata;
      return false;
    }
    
    _columns.push_back(info);
    _column_data.push_back(cdata);
    
    _stmt_results[i].length= &cdata->length;
    _stmt_results[i].buffer_length= cdata->buffer_length;
    _stmt_results[i].buffer= &cdata->data;
    _stmt_results[i].is_null= &cdata->is_null;
    _stmt_results[i].error= &cdata->error;

    i++;
  }

  
  if (mysql_stmt_bind_result(_stmt, _stmt_results) != 0)
    return false;

  if (mysql_stmt_execute(_stmt) != 0)
  {
    return false;
  }

  _stmt_active= true;

  return true;
#else
  return false;
#endif
}


bool MYXMySQLResultSetSource::close_prepared()
{
#if MYSQL_VERSION_ID >= 50000
  if (_stmt)
    mysql_stmt_close(_stmt);
  _stmt= 0;
  _stmt_active= false;
  g_free(_stmt_results);
  _stmt_results= 0;
  if (_stmt_metadata)
    mysql_free_result(_stmt_metadata);
  _stmt_metadata= 0;

  return true;
#else
  return false;
#endif
}


bool MYXMySQLResultSetSource::supports_prepared()
{
  if (get_server_version() >= 50000)
    return true;
  return false;
}


unsigned long MYXMySQLResultSetSource::get_server_version()
{
  if (mysql_query(&_mysql, "SELECT VERSION()")==0)
  {
    MYSQL_RES *res= mysql_use_result(&_mysql);
    MYSQL_ROW row;

    if (res)
    {
	int major=0, minor=0, pl= 0;
	row= mysql_fetch_row(res);
	sscanf(row[0], "%i.%i.%i", &major, &minor, &pl);
	mysql_free_result(res);
	return major*10000+minor*100+pl;
    }
  }
  return 0;
}



int MYXMySQLResultSetSource::get_error_number()
{
  return _last_errno;
}


const char *MYXMySQLResultSetSource::get_error_message()
{
  return _last_error;
}
