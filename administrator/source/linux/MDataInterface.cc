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

#include "myadmin.h"
#include "MAdministrator.h"
#include "MInstanceInfo.h"
#include "MDataInterface.h"

#include "myg_utils.h"
#include "myg_gtkutils.h"


MDataInterface::MDataInterface(MAdministrator *app, MInstanceInfo *inst)
  : _app(app), _instance(inst), _catalogs(0), _user_names(0)
{
  _instance->signal_disconnect().connect(sigc::mem_fun(*this,&MDataInterface::instance_disconnected));
}


MDataInterface::~MDataInterface()
{
  if (_user_names)
    myx_free_user_names(_user_names);
  
  for (std::map<Glib::ustring,UserData>::iterator it= _user_data.begin();
       it != _user_data.end(); ++it)
  {
    myx_free_user(it->second.user);
  }
}


void MDataInterface::instance_disconnected()
{
  _catalogs.clear();

  if (_user_names)
    myx_free_user_names(_user_names);
  _user_names= NULL;
  
  for (std::map<Glib::ustring,UserData>::iterator it= _user_data.begin();
       it != _user_data.end(); ++it)
  {
    myx_free_user(it->second.user);
  }
  _user_data.clear();
}


void MDataInterface::show_server_error(const Glib::ustring &msg)
{
  myg_show_mysql_error(*_app->window(), msg, _instance->get_mysql());
}


void MDataInterface::refresh_catalogs()
{
  MYX_CATALOGS *cats;
  
  cats= (MYX_CATALOGS*)_instance->perform_data_fetch((MInstanceInfo::DataFetcher)myx_get_catalogs);
  if (!cats)
  {
    show_server_error(_("Could not retrieve catalogs from server"));
  }
  else
  {
    _catalogs= Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> >(new MGPtrWrap<MYX_CATALOGS*>(cats,myx_free_catalogs));
    
    _catalogs_refreshed_signal.emit(_catalogs);
  }
}


Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > MDataInterface::get_catalogs(bool refresh)
{
  if (refresh || !_catalogs)
    refresh_catalogs();

  return _catalogs;
}

MYX_SCHEMA_TABLES *MDataInterface::get_schema_tables(const Glib::ustring &catalog_name,
                                                     const Glib::ustring &schema_name,
                                                     bool refresh)
{
  MYX_SCHEMA_TABLES *tables;
  MYX_SCHEMA *parent_schema= NULL;
  MYX_CATALOGS *cats= _catalogs->ptr();

  for (unsigned int c= 0; c < cats->catalogs_num && parent_schema == NULL; c++)
  {
    MYX_CATALOG *catalog= cats->catalogs+c;

    for (unsigned int s= 0; s < catalog->schemata_num; s++)
    {
      MYX_SCHEMA *schema= catalog->schemata+s;

      if (schema_name.compare(schema->schema_name)==0)
      {
        parent_schema= schema;
        break;
      }
    }
  }

  g_assert(parent_schema!=NULL);

  if (refresh || !parent_schema->schema_tables)
  {
    tables= (MYX_SCHEMA_TABLES*)_instance->perform_data_fetch3((MInstanceInfo::DataFetcher3)myx_get_schema_tables,
                                                               (void*)catalog_name.c_str(),
                                                               (void*)schema_name.c_str());
    if (!tables)
    {
      show_server_error(_("Could not retrieve schema table definitions from server"));
    }
    parent_schema->schema_tables= tables;
  }
  return parent_schema->schema_tables;
}

MYX_SCHEMA_STORED_PROCEDURES *MDataInterface::get_schema_sps(
                                                 const Glib::ustring &catalog_name,
                                                 const Glib::ustring &schema_name,
                                                 bool refresh)
{
  MYX_SCHEMA_STORED_PROCEDURES *procs;
  MYX_SCHEMA *parent_schema= NULL;
  MYX_CATALOGS *cats= _catalogs->ptr();

  for (unsigned int c= 0; c < cats->catalogs_num && parent_schema == NULL; c++)
  {
    MYX_CATALOG *catalog= cats->catalogs+c;

    for (unsigned int s= 0; s < catalog->schemata_num; s++)
    {
      MYX_SCHEMA *schema= catalog->schemata+s;

      if (schema_name.compare(schema->schema_name)==0)
      {
        parent_schema= schema;
        break;
      }
    }
  }

  g_assert(parent_schema!=NULL);

  if (refresh || !parent_schema->schema_sps)
  {
    procs= (MYX_SCHEMA_STORED_PROCEDURES*)_instance->perform_data_fetch3(
                            (MInstanceInfo::DataFetcher3)myx_get_schema_sps,
                            (void*)catalog_name.c_str(),
                            (void*)schema_name.c_str());
    if (!procs)
    {
      show_server_error(_("Could not retrieve schema stored routine definitions from server"));
    }
    parent_schema->schema_sps= procs;
  }
  return parent_schema->schema_sps;
}


MYX_SCHEMA_TABLE_STATUS *MDataInterface::get_schema_table_status(const Glib::ustring &catalog_name,
                                                                 const Glib::ustring &schema_name)
{
  MYX_SCHEMA_TABLE_STATUS *status;
  
  status= (MYX_SCHEMA_TABLE_STATUS*)_instance->perform_data_fetch3((MInstanceInfo::DataFetcher3)myx_get_schema_table_status,
                                                                   (void*)catalog_name.c_str(),
                                                                   (void*)schema_name.c_str());
  if (!status)
  {
    show_server_error(ufmt(_("Could not retrieve table status data for schema '%s.%s'"),
                           catalog_name.c_str(), schema_name.c_str()));
  }
  return status;
}

MYX_SCHEMA_ENTITY_STATUS *MDataInterface::get_schema_entity_status(const Glib::ustring &catalog_name,
                                                                   const Glib::ustring &schema_name)
{
  MYX_SCHEMA_ENTITY_STATUS *status;
  
  status= (MYX_SCHEMA_ENTITY_STATUS*)_instance->perform_data_fetch3((MInstanceInfo::DataFetcher3)myx_get_schema_entity_status,
                                                                   (void*)catalog_name.c_str(),
                                                                   (void*)schema_name.c_str());
  if (!status)
  {
    show_server_error(ufmt(_("Could not retrieve entity status data for schema '%s.%s'"),
                           catalog_name.c_str(), schema_name.c_str()));
  }
  return status;
}

MYX_USER_NAMES *MDataInterface::get_user_names(bool refresh)
{
  if (refresh || !_user_names)
  {
    if (_user_names)
      myx_free_user_names(_user_names);

    _user_names= (MYX_USER_NAMES*)_instance->perform_data_fetch((MInstanceInfo::DataFetcher)myx_get_user_names);
    if (!_user_names)
    {
      show_server_error(_("Could not retrieve user list from server"));
    }
    
    // notify
  }
  return _user_names;
}


MYX_USER *MDataInterface::get_user_if_cached(const Glib::ustring &username)
{  
  if (_user_data.find(username) != _user_data.end())
  {
    return _user_data[username].user;
  }
  return 0;
}
  

MYX_USER *MDataInterface::get_user(const Glib::ustring &username, bool refresh)
{
  MYX_USER *user;
  
  if (_user_data.find(username) == _user_data.end())
  {
    UserData info;

//    user= (MYX_USER*)_instance->perform_data_fetch2((MInstanceInfo::DataFetcher2)myx_get_user_with_privileges,
//                                                    (void*)username.c_str());

    user=myx_get_user_with_privileges(_instance->get_mysql(), username.c_str());
    if (!user)
    {
      show_server_error(ufmt(_("Could not retrieve user information for '%s'"),
                             username.c_str()));
      return NULL;
    }
    info.user= user;
    info.is_new= false;
    _user_data[username]= info;
  }
  else
  {
    user= _user_data[username].user;

    if (refresh)
    {
      myx_free_user(user);

      user= (MYX_USER*)_instance->perform_data_fetch2((MInstanceInfo::DataFetcher2)myx_get_user_with_privileges,
                                                      (void*)username.c_str());
      _user_data[username].user= user;
      if (!user)
      {
        show_server_error(ufmt(_("Could not retrieve user information for '%s'"),
                               username.c_str()));
        return NULL;
      }
    }
  }

  return user;
}


void MDataInterface::add_new_user(MYX_USER *user, const char *new_name)
{
  UserData info;
  
  info.user= user;
  info.is_new= true;
  _user_data[new_name]= info;

  _user_names->user_names_num++;
  _user_names->user_names= (char**)g_realloc(_user_names->user_names,
                                             _user_names->user_names_num*sizeof(char*));
  _user_names->user_names[_user_names->user_names_num-1]= g_strdup(new_name);
  
  //notify
}


bool MDataInterface::save_user(MYX_USER *user, 
                               const Glib::ustring &old_name,
                               const Glib::ustring &new_name)
{
  long res;
  bool is_new;

  is_new= _user_data[old_name].is_new;

  // save user info
  res= (long)_instance->perform_data_fetch4((MInstanceInfo::DataFetcher4)myx_set_user,
                                            user,
                                            (void*)(is_new ? "" : old_name.c_str()),
                                            (is_new ? (void*)1 : (void*)0),
                                            _("Storing user information..."));
    
  if (res < 0)
    return false;
    
  // update internal data
  for (unsigned int i= 0; i < _user_names->user_names_num; i++)
  {
    if (strcmp((char*)_user_names->user_names[i], old_name.c_str())==0)
    {
      g_free(_user_names->user_names[i]);
      _user_names->user_names[i]= g_strdup(new_name.c_str());
      break;
    }
  }

  _user_data.erase(_user_data.find(old_name));
  _user_data[new_name].user= user;
  _user_data[new_name].is_new= false;

  // notify
  return true;
}


void MDataInterface::remove_user_from_name_list(const Glib::ustring &name)
{
  //remove user from name list
  for (unsigned int i= 0; i < _user_names->user_names_num; i++)
  {
    if (name.compare((char*)_user_names->user_names[i])==0)
    {
      g_free(_user_names->user_names[i]);

      memmove(_user_names->user_names+i,
              _user_names->user_names+i+1,
              sizeof(char*)*(_user_names->user_names_num-i-1));
      _user_names->user_names_num--;
      break;
    }
  }
}


void MDataInterface::remove_user(const Glib::ustring &name)
{
    //remove user from server
  _instance->perform_data_fetch2((MInstanceInfo::DataFetcher2)myx_del_user,
                                 (void*)name.c_str(),
                                 _("Removing user..."));

  remove_user_from_name_list(name);

  if (_user_data.find(name) != _user_data.end())
  {
    if (_user_data[name].user)
      myx_free_user(_user_data[name].user);
    _user_data.erase(_user_data.find(name));
  }

  // notify
}


void MDataInterface::forget_user(const Glib::ustring &user)
{
  if (_user_data[user].user)
    myx_free_user(_user_data[user].user);
  if (_user_data[user].is_new)
  {
    remove_user_from_name_list(user.c_str());
  }
  _user_data.erase(_user_data.find(user));
}


