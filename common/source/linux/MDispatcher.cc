/* Copyright (C) 2003, 2004 MySQL AB

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



#include "MDispatcher.h"
#include <gtkmm.h>
#include <unistd.h>
#include <mysql.h>

#define MX_LOCK(mx) { if (0) g_message("[%i]:%s: LOCK %s", (int)pthread_self(), __FUNCTION__, #mx); pthread_mutex_lock(mx); }
#define MX_UNLOCK(mx) { if (0) g_message("[%i]:%s: UNLOCK %s", (int)pthread_self(), __FUNCTION__, #mx); pthread_mutex_unlock(mx); }


void *MDispatcher::thread_func(Fetcher *self)
{
  void *result;
 
  mysql_thread_init();

  // allow thread to be immediately cancelled
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
  // make sure we've got no races
  pthread_testcancel();

  result= self->function(self->user_data);

  // this will signal that the thread is done
  self->sig_dispatcher->emit();

  mysql_thread_end();

  return result;
}


void *MDispatcher::async_thread_func(AsyncFetcher *self)
{
  mysql_thread_init();

  MX_LOCK(&self->disp->_async_mutex);
  void *result;
  void *(*function)(void*)= self->function;
  void *user_data= self->user_data;
  long delay= self->delay;
  MX_UNLOCK(&self->disp->_async_mutex);
  Glib::Dispatcher *dispatcher= self->sig_dispatcher;

  // allow thread to be immediately cancelled
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
  // make sure we've got no races
  pthread_testcancel();

  do
  {
    MX_LOCK(&self->disp->_async_mutex);
//    self->fetch_locked= true;
//    MX_LOCK(&self->disp->_fetch_mutex);
    MX_UNLOCK(&self->disp->_async_mutex);

    result= (*function)(user_data);
//    MX_UNLOCK(&self->disp->_fetch_mutex);

    MX_LOCK(&self->disp->_async_mutex);

//    self->fetch_locked= false;

    self->data= result;
    self->data_ready= true;
    MX_UNLOCK(&self->disp->_async_mutex);
    
    dispatcher->emit();

    if (delay > 0)
    {
      ::usleep(delay*1000);
    }
  } while (delay >= 0 && !self->cancelled);

  if (self->free_user_data)
  {
    (*self->free_user_data)(self->user_data);
    self->user_data= NULL;
  }

  dispatcher->emit();

  mysql_thread_end();

  return NULL;
}


MDispatcher::MDispatcher()
  : _done(false), _cancelled(false), _checking_instances(false), _fetcher(0)
{
  pthread_mutex_init(&_fetch_mutex, NULL);
  pthread_mutex_init(&_async_mutex, NULL);
  
  _sync_thread_signal.connect(sigc::mem_fun(*this,&MDispatcher::sync_thread_finished));
  _async_thread_signal.connect(sigc::mem_fun(*this,&MDispatcher::async_thread_finished));
}


void MDispatcher::sync_thread_finished()
{
  // check sync fetcher
  if (_fetcher)
  {
    // get out of loop in dispatch()
    Gtk::Main::instance()->quit();
  }
}


void MDispatcher::async_thread_finished()
{
  MX_LOCK(&_async_mutex);
  _checking_instances= true;

  std::map<const char*,AsyncFetcher*>::iterator it= _async_fetchers.begin();
  while (it != _async_fetchers.end())
  {
    bool stop= false;
    std::map<const char*,AsyncFetcher*>::iterator next= it;
    ++next;

    // check if data available
    if (it->second->data_ready)
    {
      it->second->data_ready= false;
      if (it->second->callback)
      {
        stop= !(*it->second->callback)(it->second->data,
                                       it->second->cb_user_data);
      }
      it->second->data= NULL;
    }
    // check if the thread finished its job
    if (stop || it->second->cancelled)
    {
      if (it->second->free_user_data && it->second->user_data)
          (*it->second->free_user_data)(it->second->user_data);

      _async_fetchers.erase(it);
      delete it->second;
    }
    it= next;
  }

  _checking_instances= false;
  MX_UNLOCK(&_async_mutex);
}



void *MDispatcher::dispatch(void *(*func)(void*), void *udata)
{  
  if (getenv("DEBUG_DONT_SPAWN_FETCHES"))
    return func(udata);
  void *result;

  g_return_val_if_fail(_fetcher == 0, NULL);

  _fetcher= new Fetcher;

  _fetcher->disp= this;
  
  _fetcher->cancelled= false;

  _fetcher->function= func;
  _fetcher->user_data= udata;

  _fetcher->sig_dispatcher= &_sync_thread_signal;

  _work_signal.emit(true);

  MX_LOCK(&_fetch_mutex);

  // create thread
  pthread_create(&_fetcher->tid, NULL, (void*(*)(void*))thread_func, _fetcher);

  // loop gui until thread finishes
  Gtk::Main::instance()->run();
  
  MX_UNLOCK(&_fetch_mutex);
  
  _work_signal.emit(false);

  // join the thread
  pthread_join(_fetcher->tid, &result);

  if (_fetcher->cancelled)
    result= NULL;

  delete _fetcher;
  _fetcher= 0;
 
  return result;
}


void MDispatcher::cancel()
{
  g_return_if_fail(_fetcher != 0);

  _cancelled= true;
  pthread_cancel(_fetcher->tid);
}


void MDispatcher::dispatch_async(const char *name, 
                                 void *(*func)(void*), void *udata, void (*free_data)(void*),
                                 bool (*callback)(void*,void*), void *cbdata,
                                 long repeat_delay)
{
  g_return_if_fail(_async_fetchers.find(name)==_async_fetchers.end());

  AsyncFetcher *fetcher= new AsyncFetcher;

  fetcher->disp= this;
  
  fetcher->cancelled= false;
  fetcher->data_ready= false;
  fetcher->sig_dispatcher= &_async_thread_signal;

  fetcher->delay= repeat_delay;

  fetcher->data= NULL;

  fetcher->function= func;
  fetcher->user_data= udata;
  fetcher->free_user_data= free_data;

  fetcher->callback= callback;
  fetcher->cb_user_data= cbdata;

  MX_LOCK(&_async_mutex);
  _async_fetchers[name]= fetcher;

  // create thread
  pthread_create(&fetcher->tid, NULL, (void*(*)(void*))async_thread_func, fetcher);
  MX_UNLOCK(&_async_mutex);
}


void MDispatcher::cancel_async(const char *name)
{
  void *res;

  // would mean we're being called from the callback, which we dont support
  g_assert(_checking_instances==false);

  MX_LOCK(&_async_mutex);

  pthread_cancel(_async_fetchers[name]->tid);

  pthread_join(_async_fetchers[name]->tid, &res);
  
//  if (_async_fetchers[name]->fetch_locked)
//    MX_UNLOCK(&_fetch_mutex);

  _async_fetchers[name]->cancelled= true;

  MX_UNLOCK(&_async_mutex);

  async_thread_finished();
}

