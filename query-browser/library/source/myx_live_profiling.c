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


#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
# include <winsock2.h>
# define MSG_DONTWAIT 0
#else
# include <sys/types.h>
# include <sys/time.h>
# include <sys/socket.h>
# include <netinet/in.h>
# include <arpa/inet.h>
# include <sys/fcntl.h>
# include <netdb.h>
# include <unistd.h>
# define closesocket close
#endif

#include "myx_profiling.h"

#define DBG(s,...) g_message("QBPROF:"s, ##__VA_ARGS__)

struct MYX_QP_NET {
  int sock;
  int max_clients;

  unsigned int clients_num;
  MYX_QP_CLIENT *clients;
};





MYX_QP_NET *myx_qp_init_live_profiler()
{
  MYX_QP_NET *prof= g_new0(MYX_QP_NET, 1);
  
  return prof;
}


MYX_QBPROF_ERROR myx_qp_listen_at(MYX_QP_NET *prof, int port,
                                  int max_clients)
{
  struct sockaddr_in addr;
  
  prof->max_clients= max_clients;
  prof->sock= socket(PF_INET, SOCK_STREAM, 0);
  if (prof->sock < 0)
  {
    DBG("can't create socket: %s", strerror(errno));
    return MYX_QBP_SYSTEM_ERROR;
  }
  
  memset(&addr, 0, sizeof(struct sockaddr_in));
  addr.sin_family= AF_INET;
  addr.sin_port= htons(port);
  addr.sin_addr= //;
  
  if (bind(prof->sock, (struct sockaddr*)&addr, sizeof(addr)) < 0)
  {
    DBG("can't bind on port %i: %s", port, strerror(errno));
    return MYX_QBP_NETWORK_ERROR;
  }

  return MYX_QBP_NO_ERROR;
}


static int qp_accept_connection(MYX_QP_NET *prof)
{
  struct sockaddr_in addr;
  int nsock;
  MYX_QB_PROFILING_CLIENT *client;

  if ((nsock= accept(prof->sock, &addr, sizeof(addr))) < 0)
  {
    DBG("error accepting connection %s", strerror(errno));
    return -1;
  }

  if (prof->clients_num >= prof->max_clients)
  {    
    DBG("can't accept connection, too many clients. will drop connection");
    close(nsock);
    return -1;
  }

  prof->clients_num++;
  prof->clients= g_realloc(prof->clients, sizeof(MYX_QB_PROFILING_CLIENT)*prof->clients_num);
  client= prof->clients + prof->clients_num-1;

  client->host= g_strdup(inet_ntoa(addr.sin_addr));
  

  return 1;
}


int myx_qp_process_pending(MYX_QP_NET *prof)
{
  
}
