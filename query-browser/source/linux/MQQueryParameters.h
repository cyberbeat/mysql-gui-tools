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


#ifndef _MQQUERYPARAMETERS_H_
#define _MQQUERYPARAMETERS_H_

#include <glibmm.h>
#include "myx_public_interface.h"

typedef std::pair<Glib::ustring,Glib::ustring> MQParameter;


class MQGlobalQueryParameters : public Glib::ObjectBase {
    friend class MQQueryParameters;

    std::list<MQParameter> _parameters;

    sigc::signal1<void,const Glib::ustring&> _signal_changed;

    void changed(const Glib::ustring &key);

  public:
    std::list<std::string> get_as_list();
    void set_from_list(const std::list<std::string> &list);
    
    sigc::signal1<void,const Glib::ustring&> signal_changed() { return _signal_changed; };
};


class MQQueryParameters : public Glib::ObjectBase {
    MYX_STRINGLIST *_data;

    MQGlobalQueryParameters *_global_params;
    std::list<MQParameter> _local;
    std::list<MQParameter> _dynamic;

    void global_changed(const Glib::ustring &key);

    void set_stringlist_value(const Glib::ustring &key,
                              const Glib::ustring &value);
    void delete_stringlist_value(const Glib::ustring &key);
    
  public:
    MQQueryParameters(MQGlobalQueryParameters *global);
    ~MQQueryParameters();

    void process_resultset(MYSQL *mysql, MYX_RESULTSET *rs, MYX_RS_ROW *row);
    void set_local(const Glib::ustring &key, const Glib::ustring &value);
    void set_global(const Glib::ustring &key, const Glib::ustring &value);

    void delete_local(const Glib::ustring &key);
    void delete_global(const Glib::ustring &key);
    
    std::list<MQParameter> get_local();
    std::list<MQParameter> get_global();
    std::list<MQParameter> get_dynamic();

    MYX_STRINGLIST *get_all() { return _data; };
};


#endif /* _MQQUERYPARAMETERS_H_ */
