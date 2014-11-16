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


#ifndef _MNSOCKET_H_
#define _MNSOCKET_H_

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
    int _socket;
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

    void wrap_socket(int socket, const std::string &host, int port);
    
    bool connect(const std::string &host, int port);
    void disconnect();

    void send_line(const std::string &line);
    std::string get_line();
    
    void send_data(const char *data, size_t size);
    
    bool get_data(const char *&data, size_t &size);

    bool is_connected() const;
    
    inline MNSocketState get_state() const { return _state; };
};


#endif /* _MNSOCKET_H_ */
