#ifndef _TCP_HELPERS_H_
#define _TCP_HELPERS_H_

#ifdef _WINDOWS
#include <winsock2.h>
#include <Ws2tcpip.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#define SOCKET int
#define SOCKET_ERROR -1
#endif

#include <glib.h>
#include <stdexcept>

class TCP_socket
{
  SOCKET sock;

  TCP_socket(SOCKET s) : sock(s) {}

  friend class TCP_listener;

public:

  TCP_socket(const char *host, unsigned short port)
  {
    sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));

    addr.sin_family= AF_INET;
    addr.sin_addr.s_addr= inet_addr(host);
    addr.sin_port= htons(port);

    sock= socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

    if(connect(sock, (sockaddr *) &addr, sizeof(addr)) == SOCKET_ERROR)
    {
      char itoabuf[16];
      sprintf(itoabuf, "%i", port);

      throw std::logic_error(
        std::string("TCP_socket: connect error host: ")
        .append(host)
        .append(" port: ")
        .append(itoabuf)
        .c_str());
    }
  }

  ~TCP_socket()
  {
    ::closesocket(sock);
  }

  int send(const char *buf, int len)
  {
    int result= ::send(sock, buf, len, 0);

    if(result == SOCKET_ERROR)
    {
      throw std::logic_error(
        std::string("TCP_socket: data send error")
      );
    }

    return result;
  }

  int send(const std::wstring& str)
  {
    glong bytes_written= 0;

    gchar* cbuf= g_utf16_to_utf8((const unsigned short *)str.data(),
      str.length(), NULL, &bytes_written, NULL);

    glong bytes_written_n= htonl(bytes_written);

    int retval=
        send((const char *)&bytes_written_n, sizeof(bytes_written_n))
      + send(cbuf, bytes_written);

    g_free(cbuf);

    return retval;
  }

  int recv(char *buf, int len)
  {
    int result= ::recv(sock, buf, len, 0);

    if(result == SOCKET_ERROR)
    {
      throw std::logic_error(
        std::string("TCP_socket: data recv error")
      );
    }

    return result;
  }

  void recv(std::wstring& str)
  {
    glong bytes_to_read_n= 0;
    recv((char *)&bytes_to_read_n, sizeof(bytes_to_read_n));

    glong bytes_to_read= ntohl(bytes_to_read_n);
    gchar *cbuf= (gchar *)g_malloc(bytes_to_read);
    recv(cbuf, bytes_to_read);

    glong items_written= 0;
    gunichar2* c2= g_utf8_to_utf16(cbuf, bytes_to_read, NULL,
      &items_written, NULL);

    str.append((const wchar_t*)c2, items_written);
    g_free(cbuf);
    g_free(c2);
  }
};

class TCP_listener
{
  SOCKET sock;

public:

  TCP_listener(const char *addr_string, unsigned short port)
  {
    sock= socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

    sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));

    addr.sin_family= AF_INET;
    addr.sin_addr.s_addr= inet_addr(addr_string);
    addr.sin_port= htons(port);

    if(::bind(sock, (sockaddr *) &addr, sizeof(addr)) == SOCKET_ERROR)
    {
      throw std::logic_error(
        std::string("TCP_listener: socket bind error")
      );
    }

    if(::listen(sock, SOMAXCONN) == SOCKET_ERROR)
    {
      throw std::logic_error(
        std::string("TCP_listener: listen error")
      );
    }
  }

  ~TCP_listener()
  {
    ::closesocket(sock);
  }

  TCP_socket *listen()
  {
    SOCKET newsock= SOCKET_ERROR;

    do
    {
      newsock= accept(sock, NULL, 0);
    }
    while(newsock == SOCKET_ERROR);

    return new TCP_socket(newsock);
  }
};

#endif // _TCP_HELPERS_H_
