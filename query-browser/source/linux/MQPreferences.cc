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

#include "MQPreferences.h"
#include "myg_utils.h"

MQPreferences::MQPreferences()
{
  dont_beep= false;
  auto_add_pk_check= false;
  view_type= 'N';
  show_sidebar= true;
  max_query_history= 200;
  max_blob_length= 100;
  default_limit_value= 1000;
}


void MQPreferences::process_options(MYX_APPLICATION_OPTIONS *options, bool shared)
{
  if (!shared)
  {
    const char *value;

    for (unsigned int i= 0; i < options->option_groups_num; i++)
    {
      MYX_OPTION_GROUP *group= options->option_groups+i;
      
      if (g_strcasecmp(group->name,"querybrowser")==0)
      {      
        if ((value= find_value(group, "view_type")))
          view_type= atoi(value);
        
        if ((value= find_value(group, "show_sidebar")))
          show_sidebar= value_to_bool(value);
        
        if ((value= find_value(group, "max_query_history")))
          max_query_history= atoi(value);
        
        if ((value= find_value(group, "max_blob_length")))
          max_blob_length= atoi(value);
        
        if ((value= find_value(group, "dont_beep")))
          dont_beep= value_to_bool(value);
        
        if ((value= find_value(group, "auto_add_pk_check")))
          auto_add_pk_check= value_to_bool(value);
        
        if ((value= find_value(group, "default_limit_value")))
          default_limit_value= atoi(value);
      }
    }
  }
  MGPreferences::process_options(options, shared);
}


MYX_APPLICATION_OPTIONS *MQPreferences::prepare_options(bool shared)
{
  MYX_APPLICATION_OPTIONS *options= MGPreferences::prepare_options(shared);
  if (!shared)
  {
    std::list<ValuePair> l;

    l.clear();
    l.push_back(ValuePair("view_type",tostr(view_type)));
    l.push_back(ValuePair("show_sidebar",show_sidebar?"yes":"no"));
    l.push_back(ValuePair("max_query_history",tostr(max_query_history)));
    l.push_back(ValuePair("max_blob_length",tostr(max_blob_length)));
    l.push_back(ValuePair("dont_beep",dont_beep?"yes":"no"));
    l.push_back(ValuePair("auto_add_pk_check",auto_add_pk_check?"yes":"no"));
    l.push_back(ValuePair("default_limit_value",tostr(default_limit_value)));
    
    add_group(options, "querybrowser", l);
  }
  return options;
}
