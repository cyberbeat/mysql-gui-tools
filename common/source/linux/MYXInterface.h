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



#ifndef _MYX_INTERFACE_H_
#define _MYX_INTERFACE_H_

#include <glibmm.h>
#include "myg_utils.h"
#include "myx_public_interface.h"

namespace MYX {

class UserConnection {
    char **_options;
    unsigned int _options_num;

  public:
    Glib::ustring connection_name;
    Glib::ustring username;
    Glib::ustring password;
    Glib::ustring hostname;
    unsigned int port;
    Glib::ustring schema;
    std::list<std::string> advanced_options;
    Glib::ustring storage_path;
    Glib::ustring notes;
    enum myx_user_connection_type connection_type;
    enum myx_user_connection_storage_type storage_type;
    
    UserConnection();
    UserConnection(const UserConnection &copy);
    UserConnection(MYX_USER_CONNECTION *conn);
    ~UserConnection();

    void fill(MYX_USER_CONNECTION *conn);

    std::string find_option(const std::string &option);
    
    bool operator ==(const UserConnection &conn);
    void assign(const UserConnection &conn);
};


class UserConnectionList {
    MYX_USER_CONNECTIONS obj;

  public:
    std::vector<UserConnection> connections;
    int last_connection;

    UserConnectionList(MYX_USER_CONNECTIONS *conn);
    UserConnectionList();
    ~UserConnectionList();

    bool find_connection(const Glib::ustring &name,
                         UserConnection &conn);
    bool update_connection(const Glib::ustring &name,
                         const UserConnection &conn);
    
    MYX_USER_CONNECTIONS *get_obj();
};

};

#endif /* _MYX_INTERFACE_H_ */
