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

#include "MAPreferences.h"
#include <glibmm.h>


MAPreferences::MAPreferences()
{
  backup_profiles_directory= "backup_profiles";
  last_backup_directory= Glib::get_home_dir();
  
  start_script_path= "/etc/init.d/mysql";

  show_user_global_privileges= false;
  show_user_table_column_privileges= false;
  disable_backup_timestamps= false;
  connection_list_auto_refresh= 0;
}


void MAPreferences::process_options(MYX_APPLICATION_OPTIONS *options, bool shared)
{
  const char *value;

  if (!shared)
  {
    for (unsigned int i= 0; i < options->option_groups_num; i++)
    {
      MYX_OPTION_GROUP *group= options->option_groups+i;
      
      if (g_strcasecmp(group->name,"administrator")==0)
      {
        if ((value= find_value(group, "show_user_global_privileges")))
          show_user_global_privileges= value_to_bool(value);
        
        if ((value= find_value(group, "show_user_table_column_privileges")))
          show_user_table_column_privileges= value_to_bool(value);
        
        if ((value= find_value(group, "last_backup_directory")))
          last_backup_directory= value;
        
        if ((value= find_value(group, "disable_backup_timestamps")))
          disable_backup_timestamps= value_to_bool(value);
        
        if ((value= find_value(group, "connection_list_auto_refresh")))
          connection_list_auto_refresh= atoi(value);

        if ((value= find_value(group, "start_script_path")))
          start_script_path= value;
      }
    }
  }
  
  MGPreferences::process_options(options, shared);
}



MYX_APPLICATION_OPTIONS *MAPreferences::prepare_options(bool shared)
{
  MYX_APPLICATION_OPTIONS *options= MGPreferences::prepare_options(shared);
  std::list<ValuePair> l;
  char numbuffer[40];

  if (!shared)
  {
    // administrator
    l.clear();
    l.push_back(ValuePair("show_user_global_privileges",show_user_global_privileges?"yes":"no"));
    l.push_back(ValuePair("show_user_table_column_privileges",show_user_table_column_privileges?"yes":"no"));
    l.push_back(ValuePair("last_backup_directory",last_backup_directory));
    l.push_back(ValuePair("disable_backup_timestamps",disable_backup_timestamps?"yes":"no"));
    l.push_back(ValuePair("start_script_path",start_script_path));
    sprintf(numbuffer, "%i", connection_list_auto_refresh);
    l.push_back(ValuePair("connection_list_auto_refresh",numbuffer));
    
    add_group(options, "administrator", l);
  }
  return options;
}
