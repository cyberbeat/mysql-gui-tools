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


#include "MYXMySQLResultSetStorage.h"
#include "myx_util_public_interface.h"
/**
 * @file  MYXMySQLResultSetStorage.cc
 * @brief 
 */



void MYXMySQLResultSetStorage::gather_errors(MYXResultSetCommitResult *result, RowChange &row)
{
  if (_source->get_error_number() != 0)
    result->add_error(row.row, row.type, _source->get_error_message(), _source->get_error_number());

#if MYSQL_VERSION_ID >= 50000
  int c;
  MYSQL *mysql= ((MYXMySQLResultSetSource*)_source)->get_mysql();
  MYSQL_RES *res;
  MYSQL_ROW rrow;

  if (_is_mysql5)
  {
    c= mysql_warning_count(mysql);
    if (c > 0 && !mysql_query(mysql, "SHOW WARNINGS") &&
        (res= mysql_use_result(mysql)))
    {
      while ((rrow= mysql_fetch_row(res)))
      {
        if (strcasecmp(rrow[0], "Note")==0)
          result->add_note(row.row, row.type, rrow[2], atoi(rrow[1]));
        else
          result->add_warning(row.row, row.type, rrow[2], atoi(rrow[1]));
      }
      mysql_free_result(res);
    }
  }
#endif // mysql < 5.0
}


size_t MYXMySQLResultSetStorage::min_buffer_size_for_row(RowChange &row)
{
  size_t len= 0;
  
  len= strlen(_columns[0]->schema) + strlen(_columns[0]->table) + 5;
  
  for (unsigned int i= 0; i < _columns.size(); i++)
  {
    if (row.fields[i])
    {
      len+= strlen(_columns[i]->name) + 3; // `col`=
      len+= 2 * row.fields[i]->length + 2; // 'value'
    }
  }
  return len;
}


MYXMySQLResultSetStorage::MYXMySQLResultSetStorage(MYXMySQLResultSetSource *dataSource)
  : MYXResultSetStorage(dataSource)
{
  unsigned int ver= dataSource->get_server_version();
  
  if (ver >= 50000)
    _is_mysql5= true;
  else
    _is_mysql5= false;

  if (ver > 40100)
    _is_mysql41= true;
  else
    _is_mysql41= false;
  
  _buffer_size= 0;
  _buffer= 0;
}


size_t MYXMySQLResultSetStorage::prepare_value(char *buffer, MYXField *field, int column)
{
  if (field->value == NULL)
  {
    strcpy(buffer, "NULL");
    return 4;
  }
  else
  {
    /* locate field name */
    switch (_columns[column]->type)
    {
    case MYX_RSCT_INTEGER:
    case MYX_RSCT_DOUBLE:
    case MYX_RSCT_DECIMAL:
      memcpy(buffer, field->value, field->length);
      buffer[field->length]= 0;

      return field->length;

    case MYX_RSCT_ENUM:
    case MYX_RSCT_SET:

    case MYX_RSCT_TIME:
    case MYX_RSCT_DATE:
    case MYX_RSCT_DATETIME:
      // why *2?
      return g_snprintf(buffer, field->length*2 + 2 + 1, "'%s'", field->value);

    case MYX_RSCT_STRING:
    case MYX_RSCT_TEXT:
    case MYX_RSCT_BLOB:
      {
        int l;
        strcpy(buffer, "'");
        l= mysql_real_escape_string(((MYXMySQLResultSetSource*)_source)->get_mysql(), buffer+1, field->value, field->length);
        strcpy(buffer+1+l, "'");
        
        return l+2;
      }
    }

    return 0;
  }
}
 

bool MYXMySQLResultSetStorage::db_insert_row(RowChange &row, MYXResultSetCommitResult *result)
{
  char *query;
  size_t size;
  bool has_values;
  
  size= min_buffer_size_for_row(row) + 50 + _columns.size()*2;
  if (size > _buffer_size)
  {
    char *old= _buffer;
    _buffer= (char*)g_realloc(_buffer, size);
    if (!_buffer)
    {
      result->out_of_memory();
      _buffer= old;
      return false;
    }
    _buffer_size= size;
  }

  query= _buffer;

  query+= g_snprintf(query, _buffer_size, "INSERT INTO `%s`.`%s` (",
                  _columns[0]->schema,
                  _columns[0]->table);

  has_values= false;
  for (unsigned int i= 0; i < _columns.size(); i++)
  {
    if (row.fields[i])
    {
      if (has_values)
        query= strmov(query, ",");
      query= strmov(query, "`");
      query= strmov(query, _columns[i]->name);
      query= strmov(query, "`");
      has_values= true;
    }
  }

  if (has_values)
  {
    query= strmov(query, ") VALUES (");
    
    has_values= false;
    for (unsigned int i= 0; i < _columns.size(); i++)
    {
      if (row.fields[i])
      {
        if (has_values)
          query= strmov(query, ",");
        query+= prepare_value(query, row.fields[i], i);
        has_values= true;
      }
    }
    query= strmov(query, ")");
  }

  if (!_source->execute(_buffer, query - _buffer))
  {
    gather_errors(result, row);
    return false;
  }
  

  // fetch the autoincrement value, if there is one
  for (unsigned int i= 0; i < _columns.size(); i++)
  {
    if (_columns[i]->is_autoincrement)
    {
      my_ulonglong id= mysql_insert_id(((MYXMySQLResultSetSource*)_source)->get_mysql());
      char buf[100];
      g_snprintf(buf, sizeof(buf), "%lli", id);
      //manually add the id
      if (!set(row.row, i, buf))
      {
        result->out_of_memory();
        return false;
      }
      break;
    }
  }

  gather_errors(result, row);

  return true;
}


bool MYXMySQLResultSetStorage::db_update_row(RowChange &row, MYXResultSetCommitResult *result)
{
  char *query;
  size_t size;
  bool has_values= false;
  
  size= min_buffer_size_for_row(row) + 50 + _columns.size()*5;
  if (_buffer_size < size)
  {
    char *old= _buffer;
    _buffer= (char*)g_realloc(_buffer, size);
    if (!_buffer)
    {
      result->out_of_memory();
      _buffer= old;
      return false;
    }
    _buffer_size= size;
  }

  query= _buffer;
  
  query+= g_snprintf(query, _buffer_size, "UPDATE `%s`.`%s` SET ",
                  _columns[0]->schema, _columns[0]->table);

  for (unsigned int i= 0; i < _columns.size(); i++)
  {
    if (row.fields[i])
    {
      if (has_values)
        query= strmov(query, ",");
      query= strmov(query, "`");
      query= strmov(query, _columns[i]->name);
      query= strmov(query, "`=");
      query+= prepare_value(query, row.fields[i], i);
      has_values= true;
    }
  }

  query= strmov(query, " WHERE ");

  query= prepare_pk_condition(query, row);

  if (!_source->execute(_buffer, query - _buffer))
  {
    gather_errors(result, row);
    return false;
  }

  gather_errors(result, row);

  return true;
}


bool MYXMySQLResultSetStorage::db_delete_row(RowChange &row, MYXResultSetCommitResult *result)
{
  char *query;
  size_t size;
  
  size= min_buffer_size_for_row(row) + 50;
  if (size > _buffer_size)
  {
    char *old= _buffer;
    _buffer= (char*)g_realloc(_buffer, size);
    if (!_buffer)
    {
      result->out_of_memory();
      _buffer= old;
      return false;
    }
    _buffer_size= size;
  }

  query= _buffer;

  query+= g_snprintf(query, _buffer_size, "DELETE FROM `%s`.`%s` WHERE ",
                  _columns[0]->schema,
                  _columns[0]->table);

  query= prepare_pk_condition(query, row);

  if (query)
  {
    bool ok;

    // execute query
    if (!_source->execute(_buffer, query - _buffer))
      ok= false;
    else
      ok= true;

    gather_errors(result, row);

    return ok;
  }
  else
    return false;
}



