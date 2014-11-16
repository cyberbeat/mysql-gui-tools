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

   
#include "myadmin.h"
#include "MAServerConnectionsPanel.h"
#include "MInstanceInfo.h"
#include "MDataInterface.h"
#include "MAdministrator.h"

#include "myg_utils.h"
#include "myg_gtkutils.h"

#define MAX_COMMAND_LENGTH 128


static Glib::ustring crop_string(const char *str, int max_length)
{
  char *tmp= g_strndup(str, max_length);
  Glib::ustring res= tmp;
  g_free(tmp);
  return res;
}


MAServerConnectionsPanel::MAServerConnectionsPanel(MAdministrator *app, MDataInterface *data)
    : MAPanel(app, data), _processes(0), _current_page(0)
{
  _instance= data->get_instance();
}


MAServerConnectionsPanel::~MAServerConnectionsPanel()
{
  if (_processes)
    myx_free_process_list(_processes);
}



void MAServerConnectionsPanel::setup_thread_list(Gtk::TreeView *tree)
{
  Gtk::TreeView::Column *column;

  column= new Gtk::TreeView::Column(_("Id"));
  column->pack_start(_thr_columns._col_icon, false);
  column->pack_start(_thr_columns._col_pid);
  tree->append_column(*Gtk::manage(column));

  column= new Gtk::TreeView::Column(_("User"));
  column->pack_start(_thr_columns._col_user_icon, false);
  column->pack_start(_thr_columns._col_user);
  tree->append_column(*Gtk::manage(column));

  tree->append_column(_("Host"), _thr_columns._col_host);
  tree->append_column(_("DB"), _thr_columns._col_db);
  tree->append_column(_("Command"), _thr_columns._col_command);
  tree->append_column(_("Time"), _thr_columns._col_time);
  tree->append_column(_("State"), _thr_columns._col_state);
  tree->append_column(_("Info"), _thr_columns._col_info);

  for (int i= 0; i < 8; i++)
  {
    column= tree->get_column(i);
    column->set_clickable();
    column->set_resizable(true);
    column->signal_clicked().connect(sigc::bind<Gtk::TreeView*,int>
                                     (sigc::mem_fun(*this,&MAServerConnectionsPanel::list_column_clicked),
                                      tree, i));
  }
}


void MAServerConnectionsPanel::setup_user_list(Gtk::TreeView *tree)
{
  Gtk::TreeView::Column *column;

  column= new Gtk::TreeView::Column(_("Username"));
  column->pack_start(_user_columns._col_user_icon, false);
  column->pack_start(_user_columns._col_user);
  tree->append_column(*Gtk::manage(column));

  tree->append_column(_("Threads"), _user_columns._col_nthreads);
  tree->append_column(_("Full Name"), _user_columns._col_full_name);
  tree->append_column(_("Description."), _user_columns._col_description);
  
  for (int i= 0; i < 4; i++)
  {
    column= tree->get_column(i);
    column->set_clickable();
    column->set_resizable(true);
    column->signal_clicked().connect(sigc::bind<Gtk::TreeView*,int>
                                     (sigc::mem_fun(*this,&MAServerConnectionsPanel::list_column_clicked),
                                      tree, i));
  }
}


bool MAServerConnectionsPanel::init()
{
  Gtk::TreeView *tree;
  
  if (_xml)
    return true;

  if (!MAPanel::init_from_glade(get_glade_file(GLADE_SERVERCONNECTIONS_FILE), "panel_frame"))
    return false;

  _thread_icon= PIXCACHE->load("16x16_Thread.png");
  
  
  ((Gtk::Notebook*)get_widget("notebook"))->signal_switch_page().connect(sigc::mem_fun(*this,&MAServerConnectionsPanel::notebook_switched_page));
  get_button("kill_user_button")->signal_clicked().connect(sigc::mem_fun(*this,&MAServerConnectionsPanel::kill_user_clicked));
  get_button("kill_thread_button")->signal_clicked().connect(sigc::mem_fun(*this,&MAServerConnectionsPanel::kill_thread_clicked));
  get_button("refresh_button")->signal_clicked().connect(sigc::mem_fun(*this,&MAServerConnectionsPanel::refresh_thread_list));
  get_button("copy_button")->signal_clicked().connect(sigc::mem_fun(*this,&MAServerConnectionsPanel::copy_info_clicked));
  
  _thread_list= Gtk::ListStore::create(_thr_columns);
  _user_list= Gtk::ListStore::create(_user_columns);
  _user_thread_list= Gtk::ListStore::create(_thr_columns);

  _thread_list->set_default_sort_func(sigc::bind<Glib::RefPtr<Gtk::ListStore>*>
                                      (sigc::mem_fun(*this, &MAServerConnectionsPanel::compare_thr_row),
                                       &_thread_list));
  _user_thread_list->set_default_sort_func(sigc::bind<Glib::RefPtr<Gtk::ListStore>*>
                                           (sigc::mem_fun(*this, &MAServerConnectionsPanel::compare_thr_row),
                                            &_user_thread_list));

  _thread_tree= tree= (Gtk::TreeView*)get_widget("threads_tree");
  tree->set_model(_thread_list);
  setup_thread_list(tree);
  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MAServerConnectionsPanel::update_sensitivity));

  _user_tree= tree= (Gtk::TreeView*)get_widget("users_tree");
  tree->set_model(_user_list);
  setup_user_list(tree);
  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MAServerConnectionsPanel::user_list_clicked));

  _user_thread_tree= tree= (Gtk::TreeView*)get_widget("user_threads_tree");
  tree->set_model(_user_thread_list);
  setup_thread_list(tree);
  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MAServerConnectionsPanel::update_sensitivity));

  _default_user_icon= PIXCACHE->load("16x16_User.png");
  
  if (prefs.connection_list_auto_refresh > 0)
    _timer= Glib::signal_timeout().connect(sigc::mem_fun(*this,&MAServerConnectionsPanel::refresh_thread_list_b), prefs.connection_list_auto_refresh*1000);

  _app->signal_prefs_changed().connect(sigc::mem_fun(*this,&MAServerConnectionsPanel::prefs_changed));

  return true;
}


void MAServerConnectionsPanel::prefs_changed()
{
  if (_timer)
    _timer.disconnect();

  if (prefs.connection_list_auto_refresh > 0)
    _timer= Glib::signal_timeout().connect(sigc::mem_fun(*this,&MAServerConnectionsPanel::refresh_thread_list_b), prefs.connection_list_auto_refresh*1000);
}


void MAServerConnectionsPanel::update_sensitivity()
{
  bool user_ok= false, thread_ok= false;
  bool copy_ok= false;
  Gtk::TreeIter iter;

  if (_current_page == 0)
  {   
    if ((iter= _thread_tree->get_selection()->get_selected()))
    {
      Gtk::TreeModel::Row row= *iter;
      Glib::ustring s= row[_thr_columns._col_pid];
      if (atol(s.c_str())!=_instance->get_mysql_connection_id())
        thread_ok= true;
      
      copy_ok= true;
    }
  }
  else
  {
    if (_user_tree->get_selection()->get_selected())
      user_ok= true;

    if ((iter= _user_thread_tree->get_selection()->get_selected()))
    {
      Gtk::TreeModel::Row row= *iter;
      Glib::ustring s= row[_thr_columns._col_pid];
      if (atol(s.c_str())!=_instance->get_mysql_connection_id())
        thread_ok= true;
      
      copy_ok= true;
    }
  }

  get_widget("kill_user_button")->set_sensitive(user_ok);

  get_widget("kill_thread_button")->set_sensitive(thread_ok);

  get_widget("copy_button")->set_sensitive(copy_ok);
}


MAServerConnectionsPanel::UserInfo &MAServerConnectionsPanel::get_user_info(const char *name)
{
  UserInfo info;

  if (_user_info_cache.find(name) == _user_info_cache.end())
  {
    MYX_USER *data= (MYX_USER*)_instance->perform_data_fetch2((MInstanceInfo::DataFetcher2)myx_get_user,
                                                              (void*)name,
                                                              _("Retrieving user info..."));
    if (data)
    {
          
      if (data->icon && data->icon_length > 0)
        info.icon= make_pixbuf_from_data(data->icon, data->icon_length);
      info.fullname= (char*)data->full_name?:"";
          
      _user_info_cache[name]= info;
          
      myx_free_user(data);
    }
    else
    {
      _user_info_cache[name]= info;
    }
  }

  return _user_info_cache[name];
}


bool MAServerConnectionsPanel::refresh_thread_list_b()
{
  refresh_thread_list();
  return true;
}

void MAServerConnectionsPanel::refresh_thread_list()
{
  MYX_PROCESS_LIST *plist;
  Glib::ustring old_sel_user;
  Gtk::TreeIter old_sel_user_iter;
  
//  plist= (MYX_PROCESS_LIST*)_instance->perform_data_fetch((MInstanceInfo::DataFetcher)myx_get_process_list);

  plist= myx_get_process_list(_inst->get_mysql());
  if (plist)
  {
    Gtk::TreeIter iter;
    Gtk::TreeModel::Row row;
    std::map<Glib::ustring,Gtk::TreeIter> users;
    
    iter= _user_tree->get_selection()->get_selected();
    if (iter)
      old_sel_user= ((Gtk::TreeModel::Row)*iter)[_user_columns._col_user];

    _user_thread_list->clear();
    _user_list->clear();
    _thread_list->clear();

    for (unsigned int i= 0; i < plist->process_infos_num; i++)
    { 
      char *uname= (char*)plist->process_infos[i].user;
      UserInfo info= get_user_info(uname);

      // fill thread list
      iter= _thread_list->append();
      row= *iter;

      row[_thr_columns._col_icon]= _thread_icon;
      row[_thr_columns._col_pid]= (char*)plist->process_infos[i].id;
      row[_thr_columns._col_user_icon]= info.icon ? info.icon: _default_user_icon;
      row[_thr_columns._col_user]= (char*)plist->process_infos[i].user;
      row[_thr_columns._col_host]= (char*)plist->process_infos[i].host;
      row[_thr_columns._col_db]= (char*)plist->process_infos[i].db?:"";
      row[_thr_columns._col_command]= crop_string((char*)plist->process_infos[i].command?:"", MAX_COMMAND_LENGTH);
      row[_thr_columns._col_time]= (char*)plist->process_infos[i].time?:"";
      row[_thr_columns._col_state]= (char*)plist->process_infos[i].state?:"";
      row[_thr_columns._col_info]= crop_string((char*)plist->process_infos[i].info?:"", MAX_COMMAND_LENGTH);
      row[_thr_columns._col_data]= plist->process_infos+i;
     
      if (users.find((char*)plist->process_infos[i].user)==users.end())
      {
        Gtk::TreeIter jter= _user_list->append();
        Gtk::TreeModel::Row jrow= *jter;
        
        users[(char*)plist->process_infos[i].user]= jter;

        jrow[_user_columns._col_user_icon]= info.icon ?info.icon: _default_user_icon;
        jrow[_user_columns._col_user]= (char*)plist->process_infos[i].user;
        jrow[_user_columns._col_full_name]= info.fullname;
        jrow[_user_columns._col_nthreads]= 1;
        jrow[_user_columns._col_description]= "";
        
        if (old_sel_user == jrow[_user_columns._col_user])
        {
          old_sel_user_iter= jter;
        }
      }
      else
      {
        Gtk::TreeModel::Row jrow= *users[(char*)plist->process_infos[i].user];
        
        jrow[_user_columns._col_nthreads]= jrow[_user_columns._col_nthreads]+1;
      }
    }
  }
  
  if (_processes)
    myx_free_process_list(_processes);
  _processes= plist;

  if (old_sel_user_iter)
  {
    _user_tree->get_selection()->select(old_sel_user_iter);
  }
  
  update_sensitivity();
}


int MAServerConnectionsPanel::compare_thr_row(const Gtk::TreeModel::iterator &a, 
                                              const Gtk::TreeModel::iterator &b,
                                              const Glib::RefPtr<Gtk::ListStore> *model)
{
  int col;
  Gtk::SortType order;
  Gtk::TreeModel::Row rowa= *a;
  Gtk::TreeModel::Row rowb= *b;
  Glib::ustring sa, sb;
  
  (*model)->get_sort_column_id(col, order);
  
  switch (col)
  {
   case 0:
    sa= rowa[_thr_columns._col_pid];
    sb= rowb[_thr_columns._col_pid];
    break;

   case 1:
    sa= rowa[_thr_columns._col_user];
    sb= rowb[_thr_columns._col_user];
    break;

   case 2:
    sa= rowa[_thr_columns._col_host];
    sb= rowb[_thr_columns._col_host];
    break;

   case 3:
    sa= rowa[_thr_columns._col_db];
    sb= rowb[_thr_columns._col_db];
    break;

   case 4:
    sa= rowa[_thr_columns._col_command];
    sb= rowb[_thr_columns._col_command];
    break;

   case 5:
    sa= rowa[_thr_columns._col_time];
    sb= rowb[_thr_columns._col_time];
    break;

   case 6:
    sa= rowa[_thr_columns._col_state];
    sb= rowb[_thr_columns._col_state];
    break;

   case 7:
    sa= rowa[_thr_columns._col_info];
    sb= rowb[_thr_columns._col_info];
    break;
  }

  return order == Gtk::SORT_ASCENDING ? sa.compare(sb) : -sa.compare(sb);
}




void MAServerConnectionsPanel::show()
{
  MAPanel::show();
  refresh_thread_list();
}


bool MAServerConnectionsPanel::before_show()
{
  return true;
}


bool MAServerConnectionsPanel::before_hide()
{

  return true;
}



/***********************************************************************/

void MAServerConnectionsPanel::list_column_clicked(Gtk::TreeView *tree,
                                                   int column)
{
  Gtk::SortType order= tree->get_column(column)->get_sort_order();
  Glib::RefPtr<Gtk::ListStore> model= Glib::RefPtr<Gtk::ListStore>::cast_static(tree->get_model());
  
  model->set_sort_column_id(column, 
                            order==Gtk::SORT_ASCENDING?Gtk::SORT_DESCENDING:Gtk::SORT_ASCENDING);
}

void MAServerConnectionsPanel::notebook_switched_page(GtkNotebookPage *page,
                                                      guint num)
{
  _current_page= num;

  update_sensitivity();
}


void MAServerConnectionsPanel::user_list_clicked()
{
  MYX_PROCESS_LIST *plist= _processes;
  Gtk::TreeIter iter;
  Gtk::TreeModel::Row row;
  Glib::ustring user;
  
  iter= _user_tree->get_selection()->get_selected();
  if (iter)
  {
    row= *iter;
    user= row[_user_columns._col_user];

    _user_thread_list->clear();

    for (unsigned int i= 0; i < plist->process_infos_num; i++)
    {
      if (user.compare((char*)plist->process_infos[i].user)==0)
      {
        UserInfo info= get_user_info((char*)plist->process_infos[i].user);

        // fill thread list
        iter= _user_thread_list->append();
        row= *iter;
        
        row[_thr_columns._col_icon]= _thread_icon;
        row[_thr_columns._col_pid]= (char*)plist->process_infos[i].id;
        row[_thr_columns._col_user_icon]= info.icon?info.icon:_default_user_icon;
        row[_thr_columns._col_user]= (char*)plist->process_infos[i].user;
        row[_thr_columns._col_host]= (char*)plist->process_infos[i].host;
        row[_thr_columns._col_db]= (char*)plist->process_infos[i].db;
        row[_thr_columns._col_command]= crop_string((char*)plist->process_infos[i].command,MAX_COMMAND_LENGTH);
        row[_thr_columns._col_time]= (char*)plist->process_infos[i].time;
        row[_thr_columns._col_state]= (char*)plist->process_infos[i].state;
        row[_thr_columns._col_info]= crop_string((char*)plist->process_infos[i].info,MAX_COMMAND_LENGTH);
        row[_thr_columns._col_data]= plist->process_infos+i;
      }
    }
  }
  update_sensitivity();
}


void MAServerConnectionsPanel::copy_info_clicked()
{
  Gtk::TreeIter iter;
  Gtk::TreeModel::Row row;

  if (_current_page==0)
    iter= _thread_tree->get_selection()->get_selected();
  else
    iter= _user_thread_tree->get_selection()->get_selected();
  
  if (iter)
  {
    row= *iter;
  
    MYX_PROCESS_INFO *info= row[_thr_columns._col_data];

    Gtk::Clipboard::get()->set_text((char*)info->info);
  }
}


void MAServerConnectionsPanel::kill_user_clicked()
{
  MYX_PROCESS_LIST *plist= _processes;
  Gtk::TreeIter iter= _user_tree->get_selection()->get_selected();
  Gtk::TreeModel::Row row= *iter;
  Glib::ustring user= row[_user_columns._col_user];

  for (unsigned int i= 0; i < plist->process_infos_num; i++)
  {
    long id= strtoul((char*)plist->process_infos[i].id, NULL, 10);

    if (id == _instance->get_mysql_connection_id())
      continue;

    if (_instance->perform_data_fetch2((MInstanceInfo::DataFetcher2)myx_kill_thread, (void*)id) != (void*)0)
    {
      myg_show_mysql_error(*_app->window(), ufmt(_("Could not kill thread %i."), id),
                           _instance->get_mysql());
    }
  }
  
  refresh_thread_list();
}


void MAServerConnectionsPanel::kill_thread_clicked()
{
  Gtk::TreeIter iter;

  if (_current_page==0)
    iter= _thread_tree->get_selection()->get_selected();
  else
    iter= _user_thread_tree->get_selection()->get_selected();

  Gtk::TreeModel::Row row= *iter;
  Glib::ustring ids= row[_thr_columns._col_pid];
  long id= strtoul(ids.c_str(), NULL, 10);
  
  if (id != _instance->get_mysql_connection_id())
  {
    if (_instance->perform_data_fetch2((MInstanceInfo::DataFetcher2)myx_kill_thread, (void*)id) != (void*)0)
    {
      myg_show_mysql_error(*_app->window(), ufmt(_("Could not kill thread %i."), id),
                           _instance->get_mysql());
    }
  }
  
  refresh_thread_list();
}



/***********************************************************************/

MAPanel *create_server_connections_panel(MAdministrator *app, MDataInterface *data)
{
  return new MAServerConnectionsPanel(app, data);
}
