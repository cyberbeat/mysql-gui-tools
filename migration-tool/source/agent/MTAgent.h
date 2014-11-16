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

#ifndef _MTAGENT_H_
#define _MTAGENT_H_

#include "myx_grt_public_interface.h"

#include <list>
#include <string>

class MNServerSocket;
class MNSocket;

class MTAgent {
    MYX_GRT *_grt;
    
    MNServerSocket *_ssock;

    std::list<std::string> _module_path;
    
    void init_plugins();

    void handle_request(const char *request);

    bool authenticate(MNSocket *sock);
    void serve(MNSocket *sock);

    void reply(MNSocket *sock, const std::string &msg);
    void reply_data(MNSocket *sock, const char *data, size_t size);
    void reply_error(MNSocket *sock, const std::string &error);
    void reply_error(MNSocket *sock, const std::string &error, MYX_GRT_ERROR errcode);
    
  public:
    MTAgent();
    
    void add_module_path(const std::string &path);

    bool init_grt();
      
    bool bind(int port);
    
    void wait();
};


#endif /* _MTAGENT_H_ */
