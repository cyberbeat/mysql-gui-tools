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

#include "myx_grt_agent.h"
#include "MNServerSocket.h"
#include "MNSocket.h"

#include <zlib.h>


static bool zdata_from_value(MYX_GRT *grt, 
                             MYX_GRT_VALUE *value, 
                             char *&data, size_t &size,
                             size_t &orig_size)
{
  char *xml= myx_grt_value_to_xml(grt, value);
  bool ok= true;
  
  if (!xml)
    return false;
  
  orig_size= strlen(xml)+1;
#if ZLIB_VERNUM >= 0x1220
  size= compressBound((uLong)orig_size);
#else
  size= orig_size + (orig_size/10) + 12;
#endif
  data= (char*)g_malloc((gulong)size);
  
  uLongf dlen= (uLongf)size;
  
  if (compress((Bytef*)data, &dlen, (const Bytef*)xml, (uLong)orig_size) != Z_OK)
  {
    ok= false;
    g_free(data);
    data= 0;
  }
  size= dlen;
  
  g_free(xml);
  
  return ok;
}


static MYX_GRT_VALUE *value_from_zdata(MYX_GRT *grt, const char *data, size_t size,
                                       size_t orig_size)
{
  Bytef *udata;
  uLongf usize;

  usize= (uLongf)orig_size;
  udata= (Bytef*)g_malloc(usize);

  if (uncompress(udata, &usize, (const Bytef*)data, (uLong)size) != Z_OK)
  {
    g_free(udata);
    return NULL;
  }

  MYX_GRT_VALUE *value= myx_grt_value_from_xml(grt, (char*)udata, usize);

  g_free(udata);

  return value;
}


static char **get_message(MNSocket *client)
{
  std::string line= client->get_line();
  if (line.empty())
    return NULL;

  line= line.substr(0, line.size()-1);

  return g_strsplit(line.c_str(), " ", -1);
}


GRTAgent::GRTAgent(MYX_GRT *grt, int port)
  : _grt(grt), _msgs(0)
{
  myx_grt_set_message_callback(grt, this, &store_messages);
  
  _socket= new MNServerSocket();
  _socket->bind(port);

  _request= 0;
  _results= g_async_queue_new();

  _msgsLock= g_mutex_new();
  
  _queueCond= g_cond_new();
  _queueLock= g_mutex_new();
}



void GRTAgent::store_messages(MYX_GRT_MSGS *msgs, void *user_data)
{
  unsigned int i, j;
  GRTAgent *me= (GRTAgent*)user_data;

  g_mutex_lock(me->_msgsLock);
  
  if (!me->_msgs)
    me->_msgs= g_new0(MYX_GRT_MSGS, 1);

  j= me->_msgs->msgs_num;
  me->_msgs->msgs_num+= msgs->msgs_num;
  me->_msgs->msgs= (MYX_GRT_MSG*)g_realloc(me->_msgs->msgs, sizeof(MYX_GRT_MSG)*me->_msgs->msgs_num);

  for (i= 0; i < msgs->msgs_num; i++)
  {
    me->_msgs->msgs[j+i]= msgs->msgs[i];
  }

  g_free(msgs->msgs);
  g_free(msgs);

  
  g_mutex_unlock(me->_msgsLock);
}


bool GRTAgent::check_busy()
{
  if (!g_mutex_trylock(_queueLock))
    return true;

  if (_processing.empty() && !_request)
    return false;

  g_mutex_unlock(_queueLock);
  return true;
}


void GRTAgent::handle_client(MNSocket *client)
{
  // do authentication

  while (client->is_connected())
  {
    // perform requests
    MYX_GRT_VALUE *value;
    
    gchar **tokens= get_message(client);

    if (!tokens)
      continue;
    
    if (!tokens[0])
    {
      g_strfreev(tokens);
      continue;
    }

    if ((strcasecmp(tokens[0], "zcall")==0 && tokens[1] && tokens[2] && !tokens[3])
        ||
        (strcasecmp(tokens[0], "call")==0 && tokens[1] && !tokens[2]))
    {
      size_t size, orig_size;
      const char *data;

      size= atoi(tokens[1]);

      if (!client->get_data(data, size))
        continue;

      if (*tokens[0] == 'z')
      {
        orig_size= atoi(tokens[2]);
        value= value_from_zdata(_grt, data, size, orig_size);
      }
      else
        value= myx_grt_value_from_xml(_grt, data, size);

      if (!value || myx_grt_value_get_type(value) != MYX_DICT_VALUE)
      {
        g_warning("Invalid request received from client.");
      }
      else
      {
        Request *request;

        const char *module= myx_grt_dict_item_get_as_string(value, "module");
        const char *function= myx_grt_dict_item_get_as_string(value, "function");
        MYX_GRT_VALUE *argument= myx_grt_dict_item_get_value(value, "argument");

        if (!module || !function || !argument)
          g_warning("Request received from client is incomplete.");
        else
        {
          g_mutex_lock(_queueLock);
          if (_request || !_processing.empty())
          {
            if (std::string(module)+"::"+std::string(function) == _processing)
              client->send_line("RUNNING\n");
            else
              client->send_line("BUSY\n");

            g_mutex_unlock(_queueLock);
          }
          else
          {
            request= new Request;
            request->module= module;
            request->function= function;
            request->argument= myx_grt_value_retain(argument);

            client->send_line("OK\n");

            _request= request;

            g_cond_signal(_queueCond);
            g_mutex_unlock(_queueLock);
          }
        }
      }
      if (value)
        myx_grt_value_release(value);
    }
    else if (strcasecmp(tokens[0], "getroot")==0 && !tokens[1]
             ||
             strcasecmp(tokens[0], "zgetroot")==0 && !tokens[1])
    {
      MYX_GRT_VALUE *value;
      char *data;
      size_t size, orig_size;
      char buffer[100];

      if (check_busy())
      {
        client->send_line("BUSY\n");
        continue;
      }
      
      value= myx_grt_get_root(_grt);

      g_mutex_unlock(_queueLock);

      if (!value)
      {
        client->send_line("NODATA\n");
      }
      else if (*tokens[0] == 'z')
      {
        if (zdata_from_value(_grt, value, data, size, orig_size))
        {
          sprintf(buffer, "OK %li %li\n", size, orig_size);
          client->send_line(buffer);
          client->send_data(data, size);
          g_free(data);
        }
        else
          client->send_line("ERROR\n");
      }
      else
      {
        char *xml= myx_grt_value_to_xml(_grt, value);
        
        if (xml)
        {
          sprintf(buffer, "OK %li\n", size);
          client->send_line(buffer);
          client->send_data(xml, strlen(xml));
          g_free(xml);
        }
        else
          client->send_line("ERROR\n");
      }
    }
    else if (strcasecmp(tokens[0], "setroot")==0 && tokens[1] && !tokens[2]
             ||
             strcasecmp(tokens[0], "zsetroot")==0 && tokens[1] && tokens[2] && !tokens[3])
    {
      size_t size, orig_size;
      const char *data;

      size= atoi(tokens[1]);

      if (!client->get_data(data, size))
        continue;
      
      if (check_busy())
      {
        client->send_line("BUSY\n");
        continue;
      }

      if (*tokens[0] == 'z')
      {
        orig_size= atoi(tokens[2]);
        value= value_from_zdata(_grt, data, size, orig_size);
      }
      else
        value= myx_grt_value_from_xml(_grt, data, size);

      if (!value)
      {
        client->send_line("ERROR\n");
        g_mutex_unlock(_queueLock);
        continue;
      }
      
      myx_grt_set_root(_grt, value);
      
      myx_grt_value_release(value);

      client->send_line("OK\n");
      
      g_mutex_unlock(_queueLock);
    }
    else if (strcasecmp(tokens[0], "status")==0 && !tokens[1])
    {
      g_mutex_lock(_queueLock);
      
      if (!_processing.empty())
        client->send_line("PROCESSING "+_processing+"\n");
      else if (g_async_queue_length(_results) > 0)
      {
        if (!_cancelled)
          client->send_line("FINISHED\n");
        else
          client->send_line("CANCELLED\n");
      }
      else
        client->send_line("IDLE\n");

      g_mutex_unlock(_queueLock);
    }
    else if (strcasecmp(tokens[0], "cancel")==0 && !tokens[1])
    {
      _cancelled= true;
      client->send_line("OK\n");
    }
    else if (strcasecmp(tokens[0], "messages")==0 && !tokens[1])
    {
      g_mutex_lock(_msgsLock);
      MYX_GRT_MSGS *msgs= _msgs;
      _msgs= NULL;
      g_mutex_unlock(_msgsLock);

      client->send_line("OK\n");

      if (msgs)
      {
        for (unsigned int m= 0; m < msgs->msgs_num; m++)
        {
          char *tmp= g_strdup_printf("%i %i %s\n", msgs->msgs[m].msg_type, msgs->msgs[m].progress, msgs->msgs[m].msg);
          client->send_line(tmp);
          g_free(tmp);
          for (unsigned int d= 0; d < msgs->msgs[m].msg_detail->strings_num; d++)
          {
            client->send_line(std::string(msgs->msgs[m].msg_detail->strings[d])+"\n");
          }
          client->send_line("\n");
        }
        myx_grt_messages_free(msgs);
      }
      client->send_line("\n");
    }
    else if (strcasecmp(tokens[0], "finish")==0 && !tokens[1])
    {
      Reply *reply;
      
      g_mutex_lock(_queueLock);
      
      reply= (Reply*)g_async_queue_try_pop(_results);
      
      if (_processing.empty() && !reply)
        client->send_line("NOFUNCTION\n");
      else if (!reply) //
        client->send_line("PROCESSING\n");
      else
      {
        MYX_GRT_VALUE *value;
        char *cdata;
        size_t csize, orig_size;
        
        value= myx_grt_dict_create(NULL, NULL,
                                   "module", MYX_STRING_VALUE, reply->request->module.c_str(),
                                   "function", MYX_STRING_VALUE, reply->request->function.c_str(),
                                   "error", MYX_INT_VALUE, reply->error,
                                   "value", MYX_ANY_VALUE, reply->value,
                                   NULL);
        if (value && zdata_from_value(_grt, value, cdata, csize, orig_size))
        {          
          char *tmp= g_strdup_printf("FINISHED %li %li\n", csize, orig_size);
          client->send_line(tmp);
          g_free(tmp);
          client->send_data(cdata, csize);
          g_free(cdata);
        }
        else
        {
          g_warning("Error replying to client.");
          client->send_line("ERROR: Insufficient resources\n");
        }

        if (value)
          myx_grt_value_release(value);

        delete reply;
      }
      g_mutex_unlock(_queueLock);
    }
    else
    {
      client->send_line("ERROR: Invalid command\n");
      g_warning("Invalid command from client: %s", tokens[0]);
    }
    g_strfreev(tokens);
  }
}


void GRTAgent::start()
{
  GError *gerror= NULL;
  
  g_thread_create(loop, this, FALSE, &gerror);
  if (gerror)
  {
    g_error("Could not create thread: %s", gerror->message);
    exit(1);
  }

  handle_requests(this);
}


gpointer GRTAgent::loop(gpointer data)
{
  GRTAgent *me= (GRTAgent*)data;

  for (;;)
  {
    if (me->_socket->listen())
    {
      MNSocket *client= me->_socket->accept();

      g_message("Received connection from %s", client->get_host().c_str());

      if (client)
      {
        me->handle_client(client);

        client->disconnect();
        g_message("Client disconnected.");

        delete client;
      }
      else
        g_warning("Error accepting connection from client.");
    }
  }
  return 0;
}



gpointer GRTAgent::handle_requests(gpointer data)
{
  GRTAgent *me= (GRTAgent*)data;

  for (;;)
  {
    Request *request;

    g_mutex_lock(me->_queueLock);
    
    while (!me->_request)
      g_cond_wait(me->_queueCond, me->_queueLock);  

    request= me->_request;
    me->_request= NULL;

    me->_cancelled= false;
    me->_processing= request->module+":"+request->function;

    g_mutex_unlock(me->_queueLock);

    g_message("Will execute %s", me->_processing.c_str());

    // process request
    Reply *result= 0;

    if (request->module.empty() && request->function.empty())
    {
      // special message to set the root value
      myx_grt_set_root(me->_grt, request->argument);

      delete request;
    }
    else
    {
      MYX_GRT_ERROR error= MYX_GRT_NO_ERROR;
      MYX_GRT_VALUE *value;
      
      value= myx_grt_function_get_and_call(me->_grt, 
                                           request->module.c_str(),
                                           request->function.c_str(),
                                           1,
                                           request->argument,
                                           &error);

      g_message("Execution finished (error= %s).", myx_grt_error_string(error));
      result= new Reply;
      result->request= request;
      result->error= error;
      result->value= value;
    }

    g_mutex_lock(me->_queueLock);
      
    // push back the result to the main thread
    if (result)
        g_async_queue_push(me->_results, result);

    me->_processing= "";
    
    g_mutex_unlock(me->_queueLock);
    
    g_message("Finished handling request, waiting for new one...");
  }
  
  return 0;
}

//======================================================================

extern "C" {

// server side
  
void myx_grt_start_agent(MYX_GRT *grt, int port, 
                 char **allowed_modules, unsigned int allowed_modules_num)
{  
  GRTAgent agent(grt, port);
  
  agent.start();
}

  
struct MYX_GRT_AGENT_SESSION {
  MNSocket *socket;
  // currently executing stuff
  std::string module;
  std::string function;
};

// client side

MYX_GRT_AGENT_SESSION *myx_grt_remote_connect(const char *host, int port,
                                              const char *passkey)
{
  MYX_GRT_AGENT_SESSION *sess= new MYX_GRT_AGENT_SESSION;

  sess->socket= new MNSocket();
  if (!sess->socket->connect(host, port))
  {
    delete sess->socket;
    delete sess;
    return NULL;
  }
  return sess;
}


MYX_GRT_AGENT_STATUS myx_grt_remote_function_invoke(MYX_GRT_AGENT_SESSION *sess,
                                                    const char *module, const char *function_name,
                                                    MYX_GRT_VALUE *argument)
{
  MYX_GRT_VALUE *value;
  MYX_GRT_AGENT_STATUS error= MYX_GRTA_OK;

  value= myx_grt_dict_create(NULL, NULL,
                             "module", MYX_STRING_VALUE, module,
                             "function", MYX_STRING_VALUE, function_name,
                             "argument", MYX_ANY_VALUE, argument,
                             NULL);

  char *data;
  size_t size, osize;
  if (zdata_from_value(NULL, value, data, size, osize))
  {
    char buffer[100];
    sprintf(buffer, "zcall %li %li\n", size, osize);

    sess->socket->send_line(buffer);

    sess->socket->send_data(data, size);

    g_free(data);
  }
  myx_grt_value_release(value);

  char **tokens= get_message(sess->socket);

  if (tokens && tokens[0])
  {
    if (strcmp(tokens[0], "OK")==0)
    {
      error= MYX_GRTA_OK;
      sess->module= module;
      sess->function= function_name;
    }
    else if (strcmp(tokens[0], "BUSY")==0)
      error= MYX_GRTA_BUSY;
    else if (strcmp(tokens[0], "RUNNING")==0)
    {
      error= MYX_GRTA_EXECUTING;
      sess->module= module;
      sess->function= function_name;
    }
    else
    {
      error= MYX_GRTA_REMOTE_ERROR;
      g_warning("Invalid response from agent: %s", tokens[0]);
    }
  }
  else
  {
    g_warning("Error receiving response from agent.");
    error= MYX_GRTA_REMOTE_ERROR;
  }
  if (tokens) g_strfreev(tokens);

  return error;
}
  
  
MYX_GRT_AGENT_STATUS myx_grt_remote_function_check(MYX_GRT_AGENT_SESSION *sess)
{
  sess->socket->send_line("status\n");
  MYX_GRT_AGENT_STATUS status;
  
  char **tokens= get_message(sess->socket);
  
  if (!tokens)
    return MYX_GRTA_REMOTE_ERROR;

  if (tokens[0])
  {
    if (strcmp(tokens[0], "PROCESSING")==0)
      status= MYX_GRTA_EXECUTING;
    else if (strcmp(tokens[0], "FINISHED")==0)
      status= MYX_GRTA_FINISHED;
    else if (strcmp(tokens[0], "IDLE")==0)
      status= MYX_GRTA_OK;
    else
      status= MYX_GRTA_REMOTE_ERROR;
  }
  else
    status= MYX_GRTA_REMOTE_ERROR;

  g_strfreev(tokens);

  return status;
}


MYX_GRT_VALUE *myx_grt_remote_function_finish(MYX_GRT_AGENT_SESSION *sess,
                                              MYX_GRT_ERROR *error,
                                              MYX_GRT_AGENT_STATUS *status)
{
  MYX_GRT_VALUE *result= NULL;

  sess->socket->send_line("finish\n");

  char **tokens= get_message(sess->socket);

  if (!tokens)
  {
    *status= MYX_GRTA_REMOTE_ERROR;
    return NULL;
  }

  *status= MYX_GRTA_OK;

  if (tokens[0])
  {
    if (strcmp(tokens[0], "NOFUNCTION")==0)
      *status= MYX_GRTA_ERROR;
    else if (strcmp(tokens[0], "CANCELLED")==0)
      *status= MYX_GRTA_CANCELLED;
    else if (strcmp(tokens[0], "FINISHED")==0 && tokens[1] && tokens[2] && !tokens[3])
    {
      size_t size, osize;
      const char *data;

      *status= MYX_GRTA_FINISHED;
      size= atoi(tokens[1]);
      osize= atoi(tokens[2]);
      
      if (sess->socket->get_data(data, size))
      {
        MYX_GRT_VALUE *value= value_from_zdata(NULL, data, size, osize);
        if (value)
        {
          result= myx_grt_dict_item_get_value(value, "value");
          if (result)
            myx_grt_value_retain(result);
          *error= (MYX_GRT_ERROR)myx_grt_dict_item_get_as_int(value, "error");

          myx_grt_value_release(value);
        }
        else
          *status= MYX_GRTA_ERROR;
      }
      else
        *status= MYX_GRTA_ERROR;
    }
    else if (strcmp(tokens[0], "PROCESSING")==0)
      *status= MYX_GRTA_EXECUTING;
    else
      *status= MYX_GRTA_REMOTE_ERROR;
  }
  else
    *status= MYX_GRTA_REMOTE_ERROR;

  g_strfreev(tokens);

  return result;  
}

  
MYX_GRT_AGENT_STATUS myx_grt_remote_function_cancel(MYX_GRT_AGENT_SESSION *sess)
{
  MYX_GRT_AGENT_STATUS status;
  
  sess->socket->send_line("cancel\n");

  char **tokens= get_message(sess->socket);
  if (!tokens)
    return MYX_GRTA_REMOTE_ERROR;
  
  if (tokens[0] && strcmp(tokens[0], "OK")==0)
    status= MYX_GRTA_OK;
  else
    status= MYX_GRTA_REMOTE_ERROR;
  g_strfreev(tokens);
  return status;
}
  
  
MYX_GRT_MSGS *myx_grt_remote_get_messages(MYX_GRT_AGENT_SESSION *sess)
{
  sess->socket->send_line("messages\n");

  char **tokens= get_message(sess->socket);
  if (!tokens || !tokens[0])
  {
    if (tokens)
      g_strfreev(tokens);
    return NULL;
  }
  
  MYX_GRT_MSGS *msgs= NULL;
  
  if (strcmp(tokens[0], "OK")==0)
  {
    msgs= g_new0(MYX_GRT_MSGS, 1);
    
    for (;;)
    {
      std::string line= sess->socket->get_line();
      if (line.empty() || line == "\n")
        break;
      MYX_GRT_MSG msg;
      
      msg.msg= (char*)g_malloc((gulong)line.size());

      sscanf(line.c_str(), "%i %i %s\n", &msg.msg_type, &msg.progress, msg.msg);
      msg.msg_detail= g_new0(MYX_STRINGLIST, 1);
      for (;;)
      {
        line= sess->socket->get_line();
        if (line.empty() || line == "\n")
          break;
        
        msg.msg_detail->strings_num++;
        msg.msg_detail->strings= (char**)g_realloc(msg.msg_detail->strings,
                                           sizeof(char*)*msg.msg_detail->strings_num);
        msg.msg_detail->strings[msg.msg_detail->strings_num-1]= g_strdup(line.c_str());
      }
      if (msg.msg_detail->strings_num == 0)
      {
        g_free(msg.msg_detail);
        msg.msg_detail= 0;
      }
      msgs->msgs_num++;
      msgs->msgs= (MYX_GRT_MSG*)g_realloc(msgs->msgs, sizeof(MYX_GRT_MSG)*msgs->msgs_num);
      msgs->msgs[msgs->msgs_num-1]= msg;
    }
  }

  return msgs;
}

  
MYX_GRT_VALUE *myx_grt_remote_get_tree(MYX_GRT_AGENT_SESSION *sess, MYX_GRT_AGENT_STATUS *status)
{
  std::string line;
  MYX_GRT_VALUE *tree= NULL;

  sess->socket->send_line("zgetroot\n");
  
  char **tokens= get_message(sess->socket);
  if (tokens)
  {    
    if (tokens[0] && strcmp(tokens[0], "OK")==0 && tokens[1] && tokens[2] && !tokens[3])
    {
      size_t size, orig_size;
      const char *data;

      size= atoi(tokens[1]);
      orig_size= atoi(tokens[2]);

      if (sess->socket->get_data(data, size))
      {
        tree= value_from_zdata(NULL, data, size, orig_size);

        *status= MYX_GRTA_OK;
      }
      else
        *status= MYX_GRTA_ERROR;
    }
    else if (tokens[0] && strcmp(tokens[0], "BUSY")==0)
      *status= MYX_GRTA_BUSY;
    else if (tokens[0] && strcmp(tokens[0], "ERROR")==0)
      *status= MYX_GRTA_REMOTE_ERROR;
    else if (tokens[0] && strcmp(tokens[0], "NODATA")==0)
      *status= MYX_GRTA_OK;
    else
      *status= MYX_GRTA_REMOTE_ERROR;

    g_strfreev(tokens);
  }
  else
    *status= MYX_GRTA_REMOTE_ERROR;
  
  return tree;
}


void myx_grt_remote_set_tree(MYX_GRT_AGENT_SESSION *sess, MYX_GRT_VALUE *tree, MYX_GRT_AGENT_STATUS *status)
{
  MNSocket *client= sess->socket;
  char *data;
  size_t size, orig_size;
  char buffer[100];

  if (zdata_from_value(NULL, tree, data, size, orig_size))
  {
    sprintf(buffer, "zsetroot %li %li\n", size, orig_size);
    client->send_line(buffer);
    client->send_data(data, size);
    g_free(data);

    std::string line= client->get_line();
    if (line == "OK\n")
      *status= MYX_GRTA_OK;
    else if (line == "BUSY\n")
      *status= MYX_GRTA_BUSY;
    else
      *status= MYX_GRTA_REMOTE_ERROR;
  }
  else
    *status= MYX_GRTA_ERROR;
}


int myx_grt_remote_session_close(MYX_GRT_AGENT_SESSION *sess)
{
  sess->socket->disconnect();
  delete sess->socket;
  delete sess;

  return 0;
}

}
