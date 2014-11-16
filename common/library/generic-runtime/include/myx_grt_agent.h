/* Copyright (c) 2005 MySQL AB
  
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
  
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
  
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
 */

#ifndef _GRTAGENT_H_
#define _GRTAGENT_H_


#include "myx_grt_public_interface.h"
#include <glib.h>

#include <string>

class MNServerSocket;
class MNSocket;

class GRTAgent 
{
    struct Request
    {
      std::string module;
      std::string function;
      MYX_GRT_VALUE *argument;
      
      ~Request() { myx_grt_value_release(argument); };
    };
    
    struct Reply
    {
      Request *request;
      
      MYX_GRT_ERROR error;
      MYX_GRT_VALUE *value;
      
      ~Reply() { delete request; if (value) myx_grt_value_release(value); };
    };
    
    MYX_GRT *_grt;
    MYX_GRT_MSGS *_msgs;
    GMutex *_msgsLock;
    
    GMutex *_queueLock;
    GCond *_queueCond;
    Request* _request;
    
    std::string _processing;
    bool _cancelled;
    
    GAsyncQueue *_results;

    MNServerSocket *_socket;

    
    bool check_busy();
    
    void handle_client(MNSocket *client);
    
    static void store_messages(MYX_GRT_MSGS *msgs, void *user_data);

    static gpointer loop(gpointer data);
    static gpointer handle_requests(gpointer data);
  public:
    GRTAgent(MYX_GRT *grt, int port);
    
    void start();
};


#endif /* _GRTAGENT_H_ */
