/* Copyright (c) 2004, 2005 MySQL AB
  
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

#include "MNSocket.h"
#include "MNServerSocket.h"

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
# include <winsock2.h>
# define socklen_t int
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
#include <glib.h>

#include <fcntl.h>
#include <errno.h>
#include <string.h>




MNServerSocket::MNServerSocket()
  : _socket(-1)
{
  MNSocket::initialize();
}


MNServerSocket::~MNServerSocket()
{
  if (_socket >= 0)
    closesocket(_socket);
}

    
bool MNServerSocket::bind(int port)
{
  struct sockaddr_in addr;
  int one= 1;
  int res;

  /* get a socket */
  _socket= socket(PF_INET, SOCK_STREAM, 0);
  if (_socket == (MYX_SOCKET)-1)
  {
    g_warning("could not create socket: %s", g_strerror(errno));
    return false;
  }

  // ignore fails of setsockopt
  setsockopt(_socket, SOL_SOCKET, SO_REUSEADDR, (const char *)&one, sizeof(one));

  memset(&addr, 0, sizeof(struct sockaddr_in));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  memset(&addr.sin_addr, 0, sizeof(addr.sin_addr));
  
  res = ::bind(_socket, (struct sockaddr*)&addr, sizeof(addr));
  if (res < 0)
  {
    g_warning("could not bind socket: %s", g_strerror(errno));
    closesocket(_socket);
    _socket= -1;
    return false;
  }

  return true;
}


bool MNServerSocket::listen()
{
  if (::listen(_socket, 5) < 0)
  {
    g_warning("could not listen to socket: %s", g_strerror(errno));
    return false;
  }
  return true;
}


MNSocket *MNServerSocket::accept()
{
  struct sockaddr_in addr;
  socklen_t addrlen= sizeof(addr);
  MYX_SOCKET s;
  MNSocket *sok;

  s= ::accept(_socket, (struct sockaddr*)&addr, &addrlen);
  if (s == (MYX_SOCKET)-1)
  {
    g_warning("error accepting connection: %s", g_strerror(errno));
    return NULL;
  }

  sok= new MNSocket();
  sok->wrap_socket(s, inet_ntoa(addr.sin_addr), ntohs(addr.sin_port));

  return sok;
}


