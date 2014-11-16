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


#include "MYXInterface.h"



using namespace MYX;


UserConnection::UserConnection()
  : _options(0), _options_num(0)
{
  storage_type= MYX_HISTORY_USER_CONNECTION;
  connection_type= MYX_MYSQL_CONN;
}

    
UserConnection::UserConnection(const UserConnection &copy)
  : _options(0), _options_num(0)
{
  connection_name= copy.connection_name;
  username= copy.username;
  password= copy.password;
  hostname= copy.hostname;
  port= copy.port;
  schema= copy.schema;
  storage_path= copy.storage_path;
  notes= copy.notes;
  connection_type= copy.connection_type;
  storage_type= copy.storage_type;
  advanced_options= copy.advanced_options;
}


void UserConnection::assign(const UserConnection &copy)
{
  for (unsigned int i= 0; i < _options_num; i++)
    free(_options[i]);
  if (_options)
    free(_options);
  _options= NULL;
  _options_num= 0;

  connection_name= copy.connection_name;
  username= copy.username;
  password= copy.password;
  hostname= copy.hostname;
  port= copy.port;
  schema= copy.schema;
  storage_path= copy.storage_path;
  notes= copy.notes;
  connection_type= copy.connection_type;
  storage_type= copy.storage_type;
  advanced_options= copy.advanced_options;
}

    
UserConnection::UserConnection(MYX_USER_CONNECTION *conn)
  : _options(0), _options_num(0)
{
  connection_name= (char*)conn->connection_name?:"";
  username= (char*)conn->username?:"";
  password= (char*)conn->password?:"";
  hostname= (char*)conn->hostname?:"";
  port= conn->port;
  schema= (char*)conn->schema?:"";
  storage_path= (char*)conn->storage_path?:"";
  notes= (char*)conn->notes?:"";
  connection_type= conn->connection_type;
  storage_type= conn->storage_type;
  myg_convert_string_list(conn->advanced_options,
                          conn->advanced_options_num,
                          advanced_options);
}


UserConnection::~UserConnection()
{
  for (unsigned int i= 0; i < _options_num; i++)
    free(_options[i]);
  if (_options)
    free(_options);
}


void UserConnection::fill(MYX_USER_CONNECTION *conn)
{
  conn->connection_name= (char*)connection_name.c_str();
  conn->username= (char*)username.c_str();
  conn->password= (char*)password.c_str();
  conn->hostname= (char*)hostname.c_str();
  conn->port= port;
  conn->schema= (char*)schema.c_str();
  conn->storage_path= (char*)storage_path.c_str();
  conn->notes= (char*)notes.c_str();
  conn->connection_type= connection_type;
  conn->storage_type= storage_type;
  for (unsigned int i= 0; i < _options_num; i++)
    free(_options[i]);
  if (_options)
    free(_options);
  _options_num= 0;
  _options= NULL;
  myg_convert_string_list(advanced_options, _options, _options_num);
  conn->advanced_options= _options;
  conn->advanced_options_num= _options_num;
}


bool UserConnection::operator ==(const UserConnection &conn)
{
  return ((connection_name == conn.connection_name)
          && (username == conn.username)
          && (password == conn.password)
          && (hostname == conn.hostname)
          && (port == conn.port)
          && (schema == conn.schema)
          && (storage_path == conn.storage_path)
          && (notes == conn.notes)
          && (connection_type == conn.connection_type)
          && (storage_type == conn.storage_type));
}


std::string UserConnection::find_option(const std::string &option)
{
  std::string pref= option+"=";
  
  for (std::list<std::string>::const_iterator iter= advanced_options.begin();
       iter != advanced_options.end(); ++iter)
  {
    if (Glib::str_has_prefix(*iter, pref))
      return iter->substr(pref.size());
  }
  return std::string();
}

//----------------------------------------------------------------------


UserConnectionList::UserConnectionList(MYX_USER_CONNECTIONS *conn)
{
  connections.clear();
  for (unsigned int i= 0; i < conn->user_connections_num; i++)
  {
    connections.push_back(UserConnection(conn->user_connections+i));
  }
  last_connection= conn->last_connection;
  memset(&obj, 0, sizeof(MYX_USER_CONNECTIONS));
}


UserConnectionList::UserConnectionList()
{
  memset(&obj, 0, sizeof(MYX_USER_CONNECTIONS));
  last_connection= -1;
}


UserConnectionList::~UserConnectionList()
{
  if (obj.user_connections)
    g_free(obj.user_connections);
}


MYX_USER_CONNECTIONS *UserConnectionList::get_obj()
{
  if (obj.user_connections)
    g_free(obj.user_connections);
  
  obj.last_connection= last_connection;
  obj.user_connections_num= connections.size();
  obj.user_connections= (MYX_USER_CONNECTION*)g_malloc(sizeof(MYX_USER_CONNECTION)*obj.user_connections_num);
  for (unsigned int i= 0; i < connections.size(); i++)
  {
    connections[i].fill(obj.user_connections+i);
  }

  return &obj;
}


bool UserConnectionList::find_connection(const Glib::ustring &name,
                                         UserConnection &conn)
{
  for (unsigned int i= 0; i < connections.size(); i++)
  {
    if (connections[i].connection_name == name)
    {
      conn= connections[i];
      return true;
    }
  }
  return false;
}


bool UserConnectionList::update_connection(const Glib::ustring &name,
                                           const UserConnection &conn)
{
  for (unsigned int i= 0; i < connections.size(); i++)
  {
    if (connections[i].connection_name == name)
    {
      connections[i]= conn;
      return true;
    }
  }
  return false;
}

