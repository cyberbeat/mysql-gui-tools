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


#include "MQQueryParameters.h"



static Glib::ustring get_escaped_value(MYSQL *mysql, MYX_RS_COLUMN *column, MYX_RS_FIELD *field)
{
  Glib::ustring value;
  
  // The query execution code is enclosing anything between ', so don't do it
  // here
  
  switch (column->column_type)
  {
  case MYX_RSCT_STRING:
  case MYX_RSCT_DATE:
  case MYX_RSCT_DATETIME:
    {
      char *buffer= (char*)g_alloca(field->value_length*2+1);
      mysql_real_escape_string(mysql, buffer, field->value, field->value_length);
//      value= "'"+Glib::ustring(buffer)+"'";
      value= buffer;
    }
    break;
  case MYX_RSCT_INTEGER:
  case MYX_RSCT_FLOAT:
    value= Glib::ustring(field->value, field->value_length);
    break;
  case MYX_RSCT_BLOB:
  case MYX_RSCT_TEXT:
    break;
  case MYX_RSCT_ENUM:
  case MYX_RSCT_SET:
    return Glib::ustring(field->value, field->value_length);
  case MYX_RSCT_TIME:
    value= Glib::ustring(field->value, field->value_length);
    break;
  }
  return value;
}



void MQGlobalQueryParameters::set_from_list(const std::list<std::string> &list)
{
  _parameters.clear();
  for (std::list<std::string>::const_iterator iter= list.begin();
       iter != list.end(); ++iter)
  {
    std::string::size_type pos= iter->find('=');
    
    if (pos == std::string::npos)
      _parameters.push_back(MQParameter(*iter, ""));
    else
      _parameters.push_back(MQParameter(iter->substr(0, pos),
                                        iter->substr(pos+1)));
  }
}


std::list<std::string> MQGlobalQueryParameters::get_as_list()
{
  std::list<std::string> list;
  
  // save some stuff
  for (std::list<MQParameter>::const_iterator iter= _parameters.begin();
       iter != _parameters.end(); ++iter)
  {
    list.push_back(iter->first+"="+iter->second);
  }
  
  return list;
}


void MQGlobalQueryParameters::changed(const Glib::ustring &key)
{
  _signal_changed.emit(key);
}



static std::list<MQParameter>::iterator find(std::list<MQParameter> &list,
                                             const Glib::ustring &key)
{
  for (std::list<MQParameter>::iterator iter= list.begin(); iter != list.end(); ++iter)
  {
    if (iter->first == key)
      return iter;
  }
  return list.end();
}


bool find(std::list<MQParameter> &list, const Glib::ustring &key,
          Glib::ustring &value)
{
  std::list<MQParameter>::iterator iter= find(list, key);

  if (iter != list.end())
  {
    value= iter->second;
    return true;
  }
  return false;
}


void MQQueryParameters::set_stringlist_value(const Glib::ustring &key,
                                             const Glib::ustring &value)
{
  bool ok= false;
  for (unsigned int i= 0; i < _data->strings_num; i++)
  {
    if (strncmp(_data->strings[i], key.data(), key.length())==0
         && _data->strings[i][key.length()]=='=')
    {
      if (strcmp(_data->strings[i]+key.length()+1, value.c_str())!=0)
      {
        g_free(_data->strings[i]);
        _data->strings[i]= g_strdup_printf("%s=%s", key.c_str(), value.c_str());
      }
      ok= true;
      break;
    }
  }
  
  _data->strings_num++;
  _data->strings= (char**)g_realloc(_data->strings, sizeof(char*)*_data->strings_num);
  _data->strings[_data->strings_num-1]= g_strdup_printf("%s=%s", key.c_str(), value.c_str());
}


void MQQueryParameters::delete_stringlist_value(const Glib::ustring &key)
{
  for (unsigned int i= 0; i < _data->strings_num; i++)
  {  
    if (strncmp(_data->strings[i], key.data(), key.length())==0
        && _data->strings[i][key.length()]=='=')
    {
      g_free(_data->strings[i]);
      memmove(_data->strings+i, _data->strings+i+1,
              sizeof(char*)*(_data->strings_num-i-1));
      break;
    }
  }
}


MQQueryParameters::MQQueryParameters(MQGlobalQueryParameters *global)
  : _global_params(global)
{
  _data= (MYX_STRINGLIST*)g_malloc0(sizeof(MYX_STRINGLIST));

  _global_params->signal_changed().connect(sigc::mem_fun(*this,&MQQueryParameters::global_changed));
}


MQQueryParameters::~MQQueryParameters()
{
  for (unsigned int i=0; i < _data->strings_num; i++)
  {
    g_free(_data->strings[i]);
  }
  g_free(_data->strings);
  g_free(_data);
}


void MQQueryParameters::process_resultset(MYSQL *mysql, MYX_RESULTSET *rs, MYX_RS_ROW *row)
{
  Glib::ustring value;

  _dynamic.clear();
  
  if (row->fields)
  {
    for (unsigned int i=0; i < rs->columns_num; i++)
    {
      switch (rs->columns[i].column_type)
      {
      case MYX_RSCT_BLOB:
      case MYX_RSCT_TEXT:
        break;
      default:
        value= get_escaped_value(mysql, rs->columns+i, row->fields+i);
        _dynamic.push_back(MQParameter(rs->columns[i].name, value));
        set_stringlist_value(rs->columns[i].name, value);
        break;
      }
    }
  }
}


void MQQueryParameters::delete_local(const Glib::ustring &key)
{
  std::list<MQParameter>::iterator iter= find(_local, key);
  if (iter != _local.end())
    _local.erase(iter);
  
  if (find(_dynamic, key)!=_dynamic.end())
    delete_stringlist_value(key);
}


void MQQueryParameters::delete_global(const Glib::ustring &key)
{
  std::list<MQParameter>::iterator iter= find(_global_params->_parameters, key);
  if (iter != _global_params->_parameters.end())
  {
    _global_params->_parameters.erase(iter);
    _global_params->changed(key);
  }
}


void MQQueryParameters::set_local(const Glib::ustring &key, const Glib::ustring &value)
{
  std::list<MQParameter>::iterator iter= find(_local, key);

  if (iter != _local.end())
    iter->second= value;
  else
    _local.push_back(MQParameter(key,value));

  if (find(_dynamic, key)!=_dynamic.end())
    set_stringlist_value(key, value);
}


void MQQueryParameters::global_changed(const Glib::ustring &key)
{
  Glib::ustring value;
  
  if (!find(_local, key, value) && !find(_dynamic, key, value))
  {
    if (find(_global_params->_parameters, key, value))
      set_stringlist_value(key, value);
    else
      delete_stringlist_value(key);
  }
}


void MQQueryParameters::set_global(const Glib::ustring &key, const Glib::ustring &value)
{
  std::list<MQParameter>::iterator iter= find(_global_params->_parameters, key);

  if (iter != _global_params->_parameters.end())
    iter->second= value;
  else
    _global_params->_parameters.push_back(MQParameter(key,value));

  _global_params->changed(key);
}


std::list<MQParameter> MQQueryParameters::get_local()
{
  return _local;
}


std::list<MQParameter> MQQueryParameters::get_global()
{
  return _global_params->_parameters;
}

  
std::list<MQParameter> MQQueryParameters::get_dynamic()
{
  return _dynamic;
}



