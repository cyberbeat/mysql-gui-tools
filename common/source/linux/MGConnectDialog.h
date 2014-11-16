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

#ifndef _MGCONNECTDIALOG_H_
#define _MGCONNECTDIALOG_H_

#include <stdio.h>
#include <gtkmm.h>
#include <pthread.h>

#include <mysql.h>

#include "MGGladeXML.h"

#include "MYXInterface.h"

#include <myx_util_functions.h>
#include <myx_public_interface.h>
//#include <wmyx_public_interface.h>


class MGPreferences;

class MGConnectDialog : public Glib::ObjectBase {
  public:
    enum {
      MYG_FLAG_PICK_SCHEMA= (1<<0),
      MYG_FLAG_ALLOW_SKIP= (1<<1)
    };

 protected:
    MGPreferences *_prefs;
    
    MGGladeXML *_xml;

    MYSQL *_mysql;

    MYX::UserConnectionList *_connections;

    MYX::UserConnection _cur_conn;

    Glib::ustring _app_title;

    int _conn_result;
    pthread_t _conn_thread;

    Gtk::TextView *_ping_view;
    FILE *_ping_output;
    Gtk::MessageDialog *_ping_dialog;

    bool _advanced_shown;
    bool _pick_schema;
    bool _connecting;
    bool _ignore_connection_change;
    int _ignore_changes;
    
    bool _skip_connect;
    bool _cancel_clicked;

    int _current_selected_item;
 private:
    bool init(int flags, const std::string &header_image_file="");

    void load_connection_list(const std::string &clpath);

    Gtk::TextView *add_text_to_ping_dialog();
    void append_text_to_ping_view(const Glib::ustring &msg);

    void start_connection();
    void stop_connection();
    void update_connect_sensitivity();
    
    void show_connection_failed();

    void reset_connection_values();
    void display_connection_values(const MYX::UserConnection &conn);
    void fetch_connection_values(MYX::UserConnection &conn);

    static void* connection_thread(void *data);
    bool process_ping_output(Glib::IOCondition cond);

    void save_connection();
    void open_connection_editor();
    void preferences_changed();
    void preferences_closed();
    
    bool monitor_connection_thread();
    void on_connect_button_clicked();
    void on_clear_button_clicked();
    void on_cancel_button_clicked();
    void on_check_entry();
    void on_details_button_clicked();
    void on_connection_list_changed();

    bool on_window_close(GdkEventAny *ev);

    bool on_key_pressed(GdkEventKey *key);
    bool on_key_released(GdkEventKey *key);
public:
    MGConnectDialog(MGPreferences *prefs);
    ~MGConnectDialog();

    virtual bool run_with_defaults(const Glib::ustring &title,
                                   MYX::UserConnection *default_conn,
                                   int flags, const std::string &header_image_file="");
    virtual bool run(const Glib::ustring &title, int flags, const std::string &header_image_file="");

    MYSQL *get_connection() const { return _mysql; };
    MYX::UserConnection get_user_connection() const { return _cur_conn; };
};


#endif /* _MGCONNECTDIALOG_H_ */
