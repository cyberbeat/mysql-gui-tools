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


#ifndef _MDISPATCHER_H_
#define _MDISPATCHER_H_

#include <glibmm.h>
#include <map>
#include <pthread.h>

class MDispatcher : public Glib::ObjectBase {
  public:
    typedef sigc::signal1<void,bool> WorkStartStopSignal;
    
  private:
    struct Fetcher {
      MDispatcher *disp;

      pthread_t tid;

      bool cancelled;

      void *(*function)(void*);
      void *user_data;
      
      Glib::Dispatcher *sig_dispatcher;
    };
    struct AsyncFetcher {
      MDispatcher *disp;

      pthread_t tid;

      bool cancelled;
      bool data_ready;
      
      long delay;

      void *data;
      
      bool fetch_locked;
      
      void *(*function)(void*);
      void *user_data;
      void (*free_user_data)(void*);
      
      bool (*callback)(void *data,void *user_data);
      void *cb_user_data;
      
      Glib::Dispatcher *sig_dispatcher;
    };

    WorkStartStopSignal _work_signal;

    pthread_mutex_t _fetch_mutex;

    bool _done;

    bool _cancelled;

    bool _checking_instances;
    pthread_mutex_t _async_mutex;
    std::map<const char*,AsyncFetcher*> _async_fetchers;

    Fetcher *_fetcher;
    
    Glib::Dispatcher _sync_thread_signal;
    Glib::Dispatcher _async_thread_signal;
    
    static void *thread_func(Fetcher *self);
    static void *async_thread_func(AsyncFetcher *self);

    void sync_thread_finished();
    void async_thread_finished();    
    
  public:
    MDispatcher();

    void *dispatch(void *(*func)(void*), void *udata);
    void cancel();
    bool was_cancelled() { return _cancelled; };

    void dispatch_async(const char *name, 
                        void *(*func)(void*), void *udata, void(*free_data)(void*),
                        bool (*callback)(void*,void*), void *cbdata,
                        long repeat_delay);
    void cancel_async(const char *name);

    WorkStartStopSignal signal_work_start_stop() { return _work_signal; };
};

   

#endif /* _MDISPATCHER_H_ */
