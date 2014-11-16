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

#ifndef _MAPREFERENCES_H_
#define _MAPREFERENCES_H_

#include "MGPreferences.h"

class MAPreferences : public MGPreferences {
    virtual void process_options(MYX_APPLICATION_OPTIONS *options, bool shared);
    virtual MYX_APPLICATION_OPTIONS *prepare_options(bool shared);
    
  public:
    MAPreferences();

    std::string start_script_path;
   
    std::string backup_profiles_directory;
    std::string last_backup_directory; //state

    bool show_user_global_privileges;
    bool show_user_table_column_privileges;

    bool disable_backup_timestamps;
    
    int connection_list_auto_refresh;
};

#endif /* _MAPREFERENCES_H_ */
