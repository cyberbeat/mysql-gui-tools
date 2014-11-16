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


#define MIN_BUFFER_SIZE 1024


#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
//# include <winsock2.h>
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


MNSocket::MNSocket()
  : _socket(-1), _next_offset(0), _state(MNSDisconnected)
{
  _buffer_used= 0;
  _buffer_size= 1024;
  _buffer= (char*)g_malloc((gulong) _buffer_size);
  
  initialize();
}


MNSocket::~MNSocket()
{
  g_free(_buffer);

  disconnect();
}


void MNSocket::initialize()
{
  static int done= 0;
  if (!done)
  {
    done= 1;
    
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
    WSADATA wsaData;
    int iResult = WSAStartup(MAKEWORD(2,2), &wsaData);
    if (iResult != NO_ERROR)
    {
      g_error("Error at WSAStartup()\n");
    }
#endif
  }
}


void MNSocket::wrap_socket(MYX_SOCKET socket, const std::string &host, int port)
{
  _socket= socket;
  _state= MNSConnected;
  
  _host= host;
  _port= port;
}


bool MNSocket::connect(const std::string &host, int port)
{
  struct sockaddr_in addr;
  struct hostent *hptr;
  struct in_addr **aptr;

  _host= host;
  _port= port;
  
  /* get a socket */
  _socket= socket(PF_INET, SOCK_STREAM, 0);
  if (_socket == (MYX_SOCKET)-1)
  {
    g_warning("could not create socket: %s", g_strerror(errno));
    return false;
  }

  /* resolve name */
  memset(&addr, 0, sizeof(struct sockaddr_in));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);

  hptr = gethostbyname(host.c_str());
  if (!hptr)
  {
    closesocket(_socket);
    _socket= -1;
    _state= MNSResolveError;
    return false;
  }

  /* try all addresses */
  for (aptr = (struct in_addr**)hptr->h_addr_list; *aptr != NULL; aptr++) {
    memcpy(&addr.sin_addr, *aptr, sizeof(**aptr));
    if (::connect(_socket, (struct sockaddr*)&addr, sizeof(addr)) == 0) {
      break;
    }
  }

  if (!*aptr)
  {
    g_warning("could not connect socket to %s:%i: %s", 
            host.c_str(), port, strerror(errno));
    closesocket(_socket);
    _socket= -1;
    _state= MNSConnectError;
    return false;
  }

  _state= MNSConnected;
  
  return true;
}


void MNSocket::disconnect()
{
  if (_state != MNSDisconnected && _socket != -1)
    closesocket(_socket);
  _state= MNSDisconnected;
  _socket= -1;
}


void MNSocket::fixup_offset()
{
  if (_next_offset > 0)
  {
    if (_buffer_used > 0)
    {
      memmove(_buffer, _buffer+_next_offset, _buffer_used-_next_offset);
      _buffer_used-= _next_offset;
    }
    _next_offset= 0;
  }
}


void MNSocket::send_data(const char *data, size_t size)
{
  size_t count, offset= 0;
  
  do
  {
    count= send(_socket, data+offset, (int)(size-offset), 0);
    if (count < 0)
    {
      if (errno == EINTR)
        continue;
      _state= MNSSendError;
      break;
    }
    if (count == 0)
    {
      disconnect();
      break;
    }
    offset += count;
  }
  while (size > offset);
}
  

void MNSocket::send_line(const std::string &line)
{
  size_t count, offset= 0;
  
  do
  {
    count= send(_socket, line.c_str()+offset, (int)(line.size()-offset), 0);
    if (count < 0)
    {
      if (errno == EINTR)
        continue;
      _state= MNSSendError;
      break;
    }
    if (count == 0)
    {
      disconnect();
      break;
    }
    offset += count;
  }
  while (line.size() > offset);
}


std::string MNSocket::get_block(int sep)
{
  size_t count;
  char *ptr;
  
  if (_state != MNSConnected)
    return "";

  fixup_offset();

  for (;;)
  {
    ptr= (char*)memchr(_buffer, sep, _buffer_used);
    if (ptr)
    {
      ptr++;
      std::string line= std::string(_buffer, ptr-_buffer);
      
      _buffer_used-= (ptr-1-_buffer);

      memmove(_buffer, ptr, _buffer_used);
      return line;
    }

    if (_buffer_size - _buffer_used < MIN_BUFFER_SIZE)
    {
      _buffer_size += MIN_BUFFER_SIZE;
      _buffer= (char*)g_realloc(_buffer, (gulong)_buffer_size);
    }

again:
    count= recv(_socket, _buffer+_buffer_used, (int)(_buffer_size-_buffer_used), 0);
    if (count == (size_t)-1)
    {
      if (errno == EINTR)
        goto again;
      _state= MNSReadError;
      break;
    }
    else if (count == 0)
    {
      disconnect();
      break;
    }

    _buffer_used+= count;
  }

  return "";
}


std::string MNSocket::get_line()
{
  return get_block('\n');
}


std::string MNSocket::get_zstring()
{
  return get_block(0);
}


bool MNSocket::get_data(const char *&data, size_t &size)
{
  size_t count;

  fixup_offset();
  
  for (;;)
  {
    if (_buffer_used >= size)
    {
      data= _buffer;

      _next_offset= size;

      return true;
    }
    else
    {
      if (_buffer_size < size)
      {
        _buffer_size = size;
        _buffer= (char*)g_realloc(_buffer, (int)_buffer_size);
      }

again:
      count= recv(_socket, _buffer+_buffer_used, (int)(_buffer_size-_buffer_used), 0);

      if (count < 0)
      {
        if (errno == EINTR)
          goto again;
        _state= MNSReadError;
        break;
      }
      else if (count == 0)
      {
        disconnect();
        break;
      }

      _buffer_used+= count;
    }
  }

  return false;
}


bool MNSocket::is_connected() const
{
  return _state == MNSConnected;
}


