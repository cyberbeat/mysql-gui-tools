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



#include "MGPreferences.h"
#include <glibmm.h>
#include "mygpriv.h"

#include <errno.h>
#include <sys/stat.h>


bool MGOptions::check_directory(const std::string &path, bool create_if_not_exist)
{
  if (!g_file_test(path.c_str(), G_FILE_TEST_IS_DIR))
  {
    if (create_if_not_exist)
    {
      if (mkdir(path.c_str(), 0700) < 0)
      {
        g_warning(_("Could not create directory %s: %s"), 
                  path.c_str(), strerror(errno));
        return false;
      }
    }
    else
    {
      g_warning(_("Directory %s does not exist"), path.c_str());
      return false;
    }
  }
  return true;
}



MGOptions::~MGOptions()
{
}


std::string MGOptions::get_user_config_dir()
{
  std::string path=Glib::get_home_dir()+"/.mysqlgui";
  
  check_directory(path, true);
  
  return path;
}


std::string MGOptions::build_path_to(const std::string &file)
{
  return Glib::build_filename(get_user_config_dir(), file);
}


bool MGOptions::save(const std::string &file)
{
  MYX_APPLICATION_OPTIONS *options;
  std::string path;
  
  
  path= build_path_to("mysqlx_common_options.xml");
  options= prepare_options(true);
  if (options && myx_store_application_options(options, (char*)path.c_str()) < 0)
  {
    return false;
  }

  free_options(options);


  path= build_path_to(file.empty()?_store_path:file);
  options= prepare_options(false);
  if (options && myx_store_application_options(options, (char*)path.c_str()) < 0)
  {
    return false;
  }
  free_options(options);

  return true;
}


bool MGOptions::load(const std::string &file)
{
  MYX_LIB_ERROR err;
  MYX_APPLICATION_OPTIONS *options;
  std::string path= build_path_to(file);
  
  options= myx_get_application_options((char*)build_path_to("mysqlx_common_options.xml").c_str(), &err);
  if (options)
  {
    process_options(options, true);
    free_options(options);
  }

  _store_path= file;

  options= myx_get_application_options((char*)path.c_str(), &err);
  if (!options)
    return false;

  process_options(options, false);
  free_options(options);

  return true;
}


const char *MGOptions::find_value(MYX_OPTION_GROUP *group, const char *name)
{
  for (unsigned int i= 0; i < group->name_value_pairs_num; i++)
  {
    if (g_strcasecmp(group->name_value_pairs[i].name, name)==0)
      return (char*)group->name_value_pairs[i].value;
  }
  return NULL;
}

std::list<const char *>MGOptions::find_values(MYX_OPTION_GROUP *group, const char *name)
{
  std::list<const char*> l;
  for (unsigned int i= 0; i < group->name_value_pairs_num; i++)
  {
    if (g_strcasecmp(group->name_value_pairs[i].name, name)==0)
      l.push_back((char*)group->name_value_pairs[i].value);
  }
  return l;
}

void MGOptions::add_group(MYX_APPLICATION_OPTIONS *options, const char *name,
                          const std::list<ValuePair> &values)
{
  MYX_OPTION_GROUP *group;

  options->option_groups_num++;
  options->option_groups= (MYX_OPTION_GROUP*)g_realloc(options->option_groups,
                                                       sizeof(MYX_OPTION_GROUP)*options->option_groups_num);

  group= options->option_groups+(options->option_groups_num-1);

  group->name= g_strdup(name);

  group->name_value_pairs= (MYX_NAME_VALUE_PAIR*)g_malloc(sizeof(MYX_NAME_VALUE_PAIR)*values.size());
  group->name_value_pairs_num= values.size();
  unsigned int i= 0;
  for (std::list<ValuePair>::const_iterator it= values.begin();
       it != values.end(); ++it, i++)
  {
    group->name_value_pairs[i].name= g_strdup(it->name.c_str());
    group->name_value_pairs[i].value= g_strdup(it->value.c_str());
  }
}


bool MGOptions::value_to_bool(const char *value)
{
  if (g_strcasecmp(value, "yes")==0 
      || g_strcasecmp(value, "1")==0
      || g_strcasecmp(value, "true")==0
      || g_strcasecmp(value, "y")==0
      || g_strcasecmp(value, "t")==0)
    return true;
  return false;
}
 

void MGOptions::free_options(MYX_APPLICATION_OPTIONS *options)
{
  if(options == NULL)
    return;

  for (unsigned int i= 0; i < options->option_groups_num; i++)
  {
    g_free(options->option_groups[i].name);
    for (unsigned int j= 0; j < options->option_groups[i].name_value_pairs_num; j++)
    {
      g_free(options->option_groups[i].name_value_pairs[j].name);
      g_free(options->option_groups[i].name_value_pairs[j].value);
    }
    g_free(options->option_groups[i].name_value_pairs);
  }
  g_free(options->option_groups);
  g_free(options);
}


//----------------------------------------------------------------------

MGPreferences::MGPreferences()
{
  connections_filename= "mysqlx_user_connections.xml";
  password_storage_type= MYX_PASSWORD_NOT_STORED;
}



void MGPreferences::process_options(MYX_APPLICATION_OPTIONS *options, bool shared)
{
  const char *value;

  if (shared)
  {
    for (unsigned int i= 0; i < options->option_groups_num; i++)
    {
      MYX_OPTION_GROUP *group= options->option_groups+i;
      
      if (g_strcasecmp(group->name,"general")==0)
      {
        if ((value= find_value(group, "connections_filename")))
          connections_filename= value;
        if ((value= find_value(group, "password_storage_type")))
          password_storage_type= (MYX_PASSWORD_STORAGE_TYPE)atoi(value);
      }
    }
  }
}



MYX_APPLICATION_OPTIONS *MGPreferences::prepare_options(bool shared)
{
  MYX_APPLICATION_OPTIONS *options= (MYX_APPLICATION_OPTIONS*)g_malloc0(sizeof(MYX_APPLICATION_OPTIONS));
  if (shared)
  {
    std::list<ValuePair> l;
    char buffer[32];
    
    l.clear();
    l.push_back(ValuePair("connections_filename",connections_filename));
    g_snprintf(buffer, sizeof(buffer), "%i", (int)password_storage_type);
    l.push_back(ValuePair("password_storage_type",buffer));
    
    add_group(options, "general", l);
  }
  return options;
}
