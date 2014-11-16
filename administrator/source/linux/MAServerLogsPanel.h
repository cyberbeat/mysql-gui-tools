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


#ifndef _MASERVERLOGSPANEL_H_
#define _MASERVERLOGSPANEL_H_

#include "MAPanel.h"

class MDataInterface;


class MAServerLogsPanel : public MAPanel {
    class Columns : public Gtk::TreeModel::ColumnRecord {
       public:
        Columns() { add(_icon); add(_time); add(_text); add(_data); add(_line); };

        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _time;
        Gtk::TreeModelColumn<Glib::ustring> _text;
        Gtk::TreeModelColumn<Glib::ustring> _data;
        Gtk::TreeModelColumn<int> _line;
    } _columns;

    class LogColumns : public Gtk::TreeModel::ColumnRecord {
       public:
        LogColumns() { add(_text); };

        Gtk::TreeModelColumn<Glib::ustring> _text;
    } _log_columns;

    enum LogType {
      LError,
      LBinary,
      LSlow,
      LGeneral,
      LInnoDB
    };

    Glib::RefPtr<Gdk::Pixbuf> _start_icon;
    Glib::RefPtr<Gdk::Pixbuf> _stop_icon; 
    Glib::RefPtr<Gdk::Pixbuf> _error_icon;
    Glib::RefPtr<Gdk::Pixbuf> _warning_icon;
   
    Glib::RefPtr<Gtk::ListStore> _store[5];
    Gtk::Label *_label[5];
    Gtk::Widget *_frame[5];
    Gtk::TreeView *_tree[5];
//    Gtk::OptionMenu *_menu[5];
    Gtk::TreeView *_list[5];
    Glib::RefPtr<Gtk::ListStore> _list_store[5];
    Gtk::Scrollbar *_sbar[5];
    Gtk::Button *_saveb[5];

    void set_page_sensitive(LogType type, bool flag);

    void setup_panel(LogType type);
    
    bool process_logs(LogType type);
    
    void selected_event(LogType type);
    void scroll_log(LogType type);

    void search_log(LogType type);
    void select_log(LogType type);
    void save_log(LogType type);
    
    void display_log(LogType type, int page);
    
  public:
    MAServerLogsPanel(MAdministrator *app, MDataInterface *data);

    virtual bool before_show();
    virtual bool before_hide();

    virtual bool init();

    virtual bool is_local_only() { return true; };
    virtual bool needs_connection() { return false; };
};

extern MAPanel *create_server_logs_panel(MAdministrator *app, MDataInterface *data);

#endif /* _MASERVERLOGSPANEL_H_ */
