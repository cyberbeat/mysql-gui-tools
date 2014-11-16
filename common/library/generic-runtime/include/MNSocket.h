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

#ifndef _MNSOCKET_H_
#define _MNSOCKET_H_

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define MYX_SOCKET SOCKET
#include <winsock2.h>
#else
# define MYX_SOCKET int
#endif
#include <string>
#include <glib.h>

typedef enum {
  MNSDisconnected,
  MNSResolveError,
  MNSConnectError,
  MNSReadError,
  MNSSendError,
  MNSConnected
} MNSocketState;


class MNSocket {
  protected:
    MYX_SOCKET _socket;
    std::string _host;
    int _port;

    char *_buffer;
    size_t _buffer_used;
    size_t _buffer_size;
    size_t _next_offset;

    MNSocketState _state;
    
    void fixup_offset();
  public:
    MNSocket();
    virtual ~MNSocket();

    static void initialize();
    
    void wrap_socket(MYX_SOCKET socket, const std::string &host, int port);
    
    bool connect(const std::string &host, int port);
    void disconnect();

    void send_line(const std::string &line);
    std::string get_line();
    std::string get_zstring();
    std::string get_block(int sep);

    void send_data(const char *data, size_t size);
    
    bool get_data(const char *&data, size_t &size);

    bool is_connected() const;
    
    inline MNSocketState get_state() const { return _state; };
    std::string get_host() { return _host; };
};


#endif /* _MNSOCKET_H_ */
