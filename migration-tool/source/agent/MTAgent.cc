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

#include "MTAgent.h"
#include "MNServerSocket.h"
#include "MNSocket.h"

#include "myx_grt_public_interface.h"
#include "myx_grt_lua.h"


MTAgent::MTAgent()
  : _grt(0), _ssock(0)
{
}


bool MTAgent::bind(int port)
{
  _ssock= new MNServerSocket();
  return _ssock->bind(port);
}



bool MTAgent::authenticate(MNSocket *sock)
{
  //TODO client authentication
  
  return true;
}


void MTAgent::reply(MNSocket *sock, const std::string &msg)
{
  sock->send_line("OK "+msg+"\n");
}


void MTAgent::reply_data(MNSocket *sock, const char *data, size_t size)
{
  sock->send_data(data, size);
}


void MTAgent::reply_error(MNSocket *sock, const std::string &error)
{
  sock->send_line("ER "+error+"\n");
}


void MTAgent::reply_error(MNSocket *sock, const std::string &error, MYX_GRT_ERROR errcode)
{
  //TODO put error code
  sock->send_line("ER "+error+"\n");
}


static std::string nexttok(std::string &str)
{
  std::string::size_type i;
  std::string s;
  
  i= 0;
  while (str[i]== ' ') i++;
  str= str.substr(i);

  i= str.find(' ');
  
  if (i == std::string::npos)
  {
    s= str;
    str= "";
    return s;
  }
  s= str.substr(0, i);
  str= str.substr(i);
  return s;
}


void MTAgent::serve(MNSocket *client)
{
  std::string request, line;
  std::string module;
  std::string token;

  g_message("handling client");

  for (;;)
  {
    request= client->get_line();

    if (client->get_state() != MNSConnected)
    {
      g_warning("socket error talking to client");
      break;
    }

    if (request[request.size()-1]=='\n')
      request.resize(request.size()-1);
    if (request[request.size()-1]=='\r')
      request.resize(request.size()-1);

    printf("client-> %s\n", request.c_str());

    line= request;

    token= nexttok(line);
    
    if (token == "shutdown")
    {
      g_message("shutting down agent");
      exit(0);
    }
    else if (token == "module")
    {
      std::string tmp;
      
      // select default module
      tmp= nexttok(line);

      if (myx_grt_find_module(_grt, tmp.c_str()))
      {
        reply(client, "module changed");
        module= tmp;
      }
      else
        reply_error(client, "invalid module");
    }
    else if (token == "call")
    {
      MYX_GRT_ERROR error;
      MYX_GRT_VALUE *argument, *result;
      std::string function= nexttok(line);
      std::string argsize= nexttok(line);
      const char *data;
      size_t size;

      if (module.empty())
      {
        reply_error(client, "module not selected");
        continue;
      }

      if (function.empty() || argsize.empty())
      {
        reply_error(client, "invalid request");
        continue;
      }

      size= atoi(argsize.c_str());
      if (size < 0)
      {
        reply_error(client, "invalid size");
        continue;
      }

      if (!client->get_data(data, size))
      {
        reply_error(client, "error retrieving argument data");
        continue;
      }

      if (size > 0)
        argument= myx_grt_unserialize_value(data, size);
      else
        argument= 0;
      result= 0;

      g_message("calling %s.%s", module.c_str(), function.c_str());

      error= myx_grt_find_call_function(_grt, module.c_str(), function.c_str(), argument, &result);
      if (error != MYX_GRT_NO_ERROR)
        reply_error(client, "function call error", error);
      else
      {
        if (result)
        {
          char *str= myx_grt_serialize_value(result);

          reply_data(client, str, strlen(str));
          
          g_free(str);
        }
        else
          reply(client, "no data");
      }

      if (argument) myx_grt_free_value(argument);
      if (result) myx_grt_free_value(result);
    }
    else if (token == "quit")
    {
      g_message("disconnecting client");
      break;
    }
    else
    {
      reply_error(client, "invalid request");
    }
  }
}


void MTAgent::wait()
{
  g_message("Listening for connections...");

  for (;;)
  {
    MNSocket *sock;
    
    if (!_ssock->listen())
      return;

    sock= _ssock->accept();
    if (!sock)
    {
      g_warning("error accepting connection");
      continue;
    }

    if (authenticate(sock))
    {
      serve(sock);
    }
    sock->disconnect();
    delete sock;
  }
}



bool MTAgent::init_grt()
{
  MYX_GRT_MODULE_LOADER *loader;
  MYX_GRT_ERROR error;
  
  static MYX_GRT_BUILTIN_FUNCTION commands[]=
  {
//    {"shutdown", MTAgent::cmd_shutdown}
  };
  static MYX_GRT_BUILTIN_MODULE agent_module=
  {
    "agent",
      NULL,
      1,
      commands
  };

  _grt= myx_grt_initialize();
  if (!_grt)
  {
    g_warning("Error initializing GRT");
    return false;
  }

//  myx_grt_register_builtin_module(_grt, &agent_module, this);

  loader= myx_lua_init_loader(_grt, &error);
  if (!loader)
    g_warning("Error initializing Lua module loader (%i)", error);
  else
  {
    if (myx_grt_register_module_loader(_grt, loader) < 0)
      g_warning("Could not register Java module loader.");
  }

  
  int c= 0;
  for (std::list<std::string>::const_iterator it= _module_path.begin();
       it != _module_path.end(); ++it)
  {
    c+= myx_grt_scan_for_modules(_grt, it->c_str(), &error);
  }
  g_message("Initialized %i modules", c);
  return true;
}


void MTAgent::add_module_path(const std::string &path)
{
  _module_path.push_back(path);
}
