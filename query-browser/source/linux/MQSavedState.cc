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

#include "MQSavedState.h"

#include "myqb.h"

#include "myg_utils.h"

#define MAX_RECENT_FILES 10


MQSavedState::MQSavedState()
{
  first_time= true;

  load("query-browser/state.xml");
}


void MQSavedState::process_options(MYX_APPLICATION_OPTIONS *options, bool shared)
{
  if (!shared)
  {
    const char *value;

    recent_files.clear();
    global_params.clear();
    for (unsigned int i= 0; i < options->option_groups_num; i++)
    {
      MYX_OPTION_GROUP *group= options->option_groups+i;
      
      if (g_strcasecmp(group->name,"script_editor")==0)
      {
        for (unsigned int j= 0; j < MAX_RECENT_FILES; j++)
        {
          if ((value= find_value(group, ufmt("recent_file%i", j).c_str())))
            recent_files.push_back(value);
        }
      }
      else if (g_strcasecmp(group->name,"global_parameters")==0)
      {
        std::list<const char*> values= find_values(group, "param");
        
        for (std::list<const char*>::const_iterator iter= values.begin();
             iter != values.end(); ++iter)
        {
          global_params.push_back(*iter);
        }
      }
    }
  }

  first_time= false;
}


MYX_APPLICATION_OPTIONS *MQSavedState::prepare_options(bool shared)
{
  MYX_APPLICATION_OPTIONS *options= (MYX_APPLICATION_OPTIONS*)g_malloc0(sizeof(MYX_APPLICATION_OPTIONS));
  if (!shared)
  {
    std::list<ValuePair> l;

    l.clear();
    int i= 0;
    for (std::list<std::string>::const_iterator iter= recent_files.begin();
         iter != recent_files.end(); ++iter, ++i)
    {
      l.push_back(ValuePair(ufmt("recent_file%i", i), iter->c_str()));
    }
    add_group(options, "script_editor", l);

    l.clear();
    for (std::list<std::string>::const_iterator iter= global_params.begin();
         iter != global_params.end(); ++iter)
    {
      l.push_back(ValuePair("param", iter->c_str()));
    }
    add_group(options, "global_parameters", l);
  }
  return options;
}


void MQSavedState::add_recent_file(const std::string &file)
{
  recent_files.remove(file);

  recent_files.push_front(file);
  while (recent_files.size() > MAX_RECENT_FILES)
    recent_files.pop_back();
  
  save();
}

