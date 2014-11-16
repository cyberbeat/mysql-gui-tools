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


#ifndef _MGPREFERENCES_H_
#define _MGPREFERENCES_H_

#include "myx_public_interface.h"
#include <map>
#include <list>
#include <string>


class MGOptions {
  protected:
    struct ValuePair {
      std::string name;
      std::string value;
      
      ValuePair(const std::string &a, const std::string &b) : name(a), value(b) {};
    };


    std::string _store_path;

    virtual void process_options(MYX_APPLICATION_OPTIONS *options, bool shared)= 0;
    virtual MYX_APPLICATION_OPTIONS *prepare_options(bool shared)= 0;

    const char *find_value(MYX_OPTION_GROUP *group, const char *name);
    std::list<const char *> find_values(MYX_OPTION_GROUP *group, const char *name);

    void add_group(MYX_APPLICATION_OPTIONS *options, const char *name,
                   const std::list<ValuePair> &values);

    bool value_to_bool(const char *value);

    void free_options(MYX_APPLICATION_OPTIONS *options);
  public:
    virtual ~MGOptions();
    virtual bool load(const std::string &file);
    virtual bool save(const std::string &file=std::string());
    std::string get_user_config_dir();
    std::string build_path_to(const std::string &file);
    
    bool check_directory(const std::string &path, bool create_if_doesnt_exist=false);
};


class MGPreferences : public MGOptions {
  protected:
    virtual void process_options(MYX_APPLICATION_OPTIONS *options, bool shared);
    virtual MYX_APPLICATION_OPTIONS *prepare_options(bool shared);

  public:
    MGPreferences();
    std::string connections_filename;
    MYX_PASSWORD_STORAGE_TYPE password_storage_type;
};



#endif /* _MGPREFERENCES_H_ */

