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


#ifndef _MDATAINTERFACE_H_
#define _MDATAINTERFACE_H_

#include "MInstanceInfo.h"
#include "MGPtrWrap.h"

#include <map>

#include "myx_library.h"
#include "myx_admin_library.h"


class MAdministrator;


class MDataInterface : public Glib::ObjectBase {
    struct UserData {
      MYX_USER *user;
      bool is_new;
    };

    sigc::signal1<void,const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> >& > _catalogs_refreshed_signal;

    MAdministrator *_app;
    MInstanceInfo *_instance;

    Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > _catalogs;

    MYX_USER_NAMES *_user_names;
    std::map<Glib::ustring,UserData> _user_data;

    void show_server_error(const Glib::ustring &msg);

    void remove_user_from_name_list(const Glib::ustring &name);
    
    void instance_disconnected();
 public:
    MDataInterface(MAdministrator *app, MInstanceInfo *inst);
    ~MDataInterface();

    MInstanceInfo *get_instance() const { return _instance; };
    
    void show_last_error(const Glib::ustring &msg) { show_server_error(msg); };

    MYX_USER_NAMES *get_user_names(bool refresh= false);
    MYX_USER *get_user_if_cached(const Glib::ustring &username);
    MYX_USER *get_user(const Glib::ustring &user, bool refresh= false);
    void forget_user(const Glib::ustring &user);

    void add_new_user(MYX_USER *user, const char *new_name);
    void remove_user(const Glib::ustring &name);
    bool save_user(MYX_USER *user, 
                   const Glib::ustring &old_name,
                   const Glib::ustring &new_name);

    void refresh_catalogs();
    Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > get_catalogs(bool refresh= false);

    MYX_SCHEMA_TABLES *get_schema_tables(const Glib::ustring &catalog_name,
                                         const Glib::ustring &schema_name,
                                         bool refresh= false);

   MYX_SCHEMA_STORED_PROCEDURES *get_schema_sps(
                                         const Glib::ustring &catalog_name,
                                         const Glib::ustring &schema_name,
                                         bool refresh= false);

    MYX_SCHEMA_TABLE_STATUS *get_schema_table_status(const Glib::ustring &catalog_name,
                                                     const Glib::ustring &schema_name);
    MYX_SCHEMA_ENTITY_STATUS *get_schema_entity_status(
                                                const Glib::ustring &catalog_name,
                                                const Glib::ustring &schema_name);

    sigc::signal1<void,const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> >& > signal_catalogs_refreshed() { return _catalogs_refreshed_signal; };
};

#endif /* _MDATAINTERFACE_H_ */
