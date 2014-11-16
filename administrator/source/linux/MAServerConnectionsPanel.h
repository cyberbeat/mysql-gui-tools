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


#ifndef _MASERVERCONNECTIONSPANEL_H_
#define _MASERVERCONNECTIONSPANEL_H_

#include "MAPanel.h"
#include <gtkmm.h>
#include <map>

#include <myx_admin_library.h>


class MInstanceInfo;

class MAServerConnectionsPanel : public MAPanel {
    struct UserInfo {
      Glib::RefPtr<Gdk::Pixbuf> icon;
      Glib::ustring fullname;
    };
    
    class ThreadsColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        ThreadsColumns() { add(_col_icon); add(_col_pid);
          add(_col_user_icon); add(_col_user);
          add(_col_host); add(_col_db); add(_col_command);
          add(_col_time); add(_col_state); add(_col_info); add(_col_data);
        }

        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _col_icon;
        Gtk::TreeModelColumn<Glib::ustring> _col_pid;
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _col_user_icon;
        Gtk::TreeModelColumn<Glib::ustring> _col_user;
        Gtk::TreeModelColumn<Glib::ustring> _col_host;
        Gtk::TreeModelColumn<Glib::ustring> _col_db;
        Gtk::TreeModelColumn<Glib::ustring> _col_command;
        Gtk::TreeModelColumn<Glib::ustring> _col_time;
        Gtk::TreeModelColumn<Glib::ustring> _col_state;
        Gtk::TreeModelColumn<Glib::ustring> _col_info;
        Gtk::TreeModelColumn<MYX_PROCESS_INFO*> _col_data;
    };
    ThreadsColumns _thr_columns;
    
    class UsersColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        UsersColumns() { add(_col_user_icon); add(_col_user); 
          add(_col_full_name); add(_col_nthreads);
          add(_col_description);
        }
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _col_user_icon;
        Gtk::TreeModelColumn<Glib::ustring> _col_user;
        Gtk::TreeModelColumn<Glib::ustring> _col_full_name;
        Gtk::TreeModelColumn<int> _col_nthreads;
        Gtk::TreeModelColumn<Glib::ustring> _col_description;
    };

    UsersColumns _user_columns;
    
    MInstanceInfo *_instance;

    std::map<Glib::ustring,UserInfo> _user_info_cache;
    Glib::RefPtr<Gdk::Pixbuf> _default_user_icon;

    MYX_PROCESS_LIST *_processes;
    
    Glib::RefPtr<Gtk::ListStore> _thread_list;
    Glib::RefPtr<Gtk::ListStore> _user_list;
    Glib::RefPtr<Gtk::ListStore> _user_thread_list;
    
    Gtk::TreeView *_thread_tree;
    Gtk::TreeView *_user_tree;
    Gtk::TreeView *_user_thread_tree;

    Glib::RefPtr<Gdk::Pixbuf> _thread_icon;

    sigc::connection _timer;
    
    int _current_page;

    UserInfo &get_user_info(const char *name);
    
    void prefs_changed();
    
    void update_sensitivity();
    bool refresh_thread_list_b();
    void refresh_thread_list();
    
    void setup_thread_list(Gtk::TreeView *tree);
    void setup_user_list(Gtk::TreeView *tree);

    void notebook_switched_page(GtkNotebookPage *page, guint num);

    void kill_user_clicked();
    void kill_thread_clicked();
    void copy_info_clicked();
    
    void list_column_clicked(Gtk::TreeView *tree, int column);
    void user_list_clicked();

    int compare_thr_row(const Gtk::TreeModel::iterator &a,
                        const Gtk::TreeModel::iterator &b,
                        const Glib::RefPtr<Gtk::ListStore> *model);
  public:
    MAServerConnectionsPanel(MAdministrator *app, MDataInterface *data);
    virtual ~MAServerConnectionsPanel();

    virtual void show();
    virtual bool before_show();
    virtual bool before_hide();

    virtual bool init();

    virtual bool is_local_only() { return false; };
    virtual bool needs_connection() { return true; };
};

extern MAPanel *create_server_connections_panel(MAdministrator *app, MDataInterface *data);

#endif /* _MASERVERCONNECTIONSPANEL_H_ */
