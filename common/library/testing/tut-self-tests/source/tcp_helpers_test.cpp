//---------------------------------------------------------------------------

#pragma hdrstop

#include "test.h"
#include <glib.h>

//---------------------------------------------------------------------------

/*
  Test suite for routines from test_helpers.h
*/

namespace tut
{

} // namespace tut

TEST_MODULE(Tcp_helpers_test, "Test TCP helper classes");

/*
  Test byte array transmission

  DESCRIPTION
    Create listening socket, connect to it,
    transmit data, check result

*/

static gpointer listener_func1(gpointer arg);
static gpointer caller_func1(gpointer arg);

static volatile bool listener_inited= false;
static volatile bool tx_error= false;
static volatile int tx_error_pos= 0;

TEST_FUNCTION(1)
{
  g_thread_init(NULL);

  char itoabuf[16];

  GThread *listener= g_thread_create(listener_func1, NULL, true, NULL);

  while(!listener_inited)
  {
    g_thread_yield();
  }

  GThread *caller= g_thread_create(caller_func1, NULL, true, NULL);

  g_thread_join(listener);
  g_thread_join(caller);

  ensure(
    std::string("TCP Transaction With Byte Buffers (error pos: ")
    .append(itoa(tx_error_pos, itoabuf, 10)).append(")"), !tx_error);
}

static gpointer listener_func1(gpointer arg)
{
  const int buffer_size= 10;
  char buffer[buffer_size] = { 0,0,0,0,0,0,0,0,0,0 };
  char ref_buffer[buffer_size] = { 1,2,3,4,5,6,7,8,9,0 };

  TCP_listener listener("127.0.0.1", 2345);
  listener_inited= true;
  TCP_socket *sock= listener.listen();
  sock->recv(buffer, buffer_size);
  delete sock;

  tx_error= (memcmp(buffer, ref_buffer, buffer_size) != 0);

  if(tx_error)
  {
    for(int i= 0; i < buffer_size; i++)
    {
      if(ref_buffer[i] != buffer[i])
      {
        tx_error_pos= i;
        break;
      }
    }
  }

  return NULL;
}

static gpointer caller_func1(gpointer arg)
{
  const int buffer_size= 10;
  char buffer[buffer_size] = { 1,2,3,4,5,6,7,8,9,0 };

  TCP_socket sock("127.0.0.1", 2345);
  sock.send(buffer, buffer_size);

  return NULL;
}

static gpointer listener_func2(gpointer arg);
static gpointer caller_func2(gpointer arg);

TEST_FUNCTION(2)
{
  listener_inited= false;
  tx_error= false;
  tx_error_pos= 0;

  char itoabuf[16];

  GThread *listener= g_thread_create(listener_func2, NULL, true, NULL);

  while(!listener_inited)
  {
    g_thread_yield();
  }

  GThread *caller= g_thread_create(caller_func2, NULL, true, NULL);

  g_thread_join(listener);
  g_thread_join(caller);

  ensure(
    std::string("TCP Transaction With WStrings (error pos: ")
    .append(itoa(tx_error_pos, itoabuf, 10)).append(")"), !tx_error);
}

static gpointer listener_func2(gpointer arg)
{
  TCP_listener listener("127.0.0.1", 2345);
  listener_inited= true;
  TCP_socket *sock= listener.listen();

  std::wstring str;
  sock->recv(str);

  delete sock;

  static const wchar_t ref_str[]= L"some utf16 text";
  int buffer_size= sizeof(ref_str);

  tx_error= str != ref_str;

  if(tx_error)
  {
    for(int i= 0; i < buffer_size; i++)
    {
      if(str[i] != ref_str[i])
      {
        tx_error_pos= i;
        break;
      }
    }
  }

  return NULL;
}

static gpointer caller_func2(gpointer arg)
{
  std::wstring str(L"some utf16 text");

  TCP_socket sock("127.0.0.1", 2345);
  sock.send(str);

  return NULL;
}

END_TESTS
