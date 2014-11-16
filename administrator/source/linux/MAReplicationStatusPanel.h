/* Copyright (C) 2003 MySQL AB

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


#ifndef _MAREPLICATIONSTATUSPANEL_H_
#define _MAREPLICATIONSTATUSPANEL_H_

#include "MAPanel.h"

#include "myx_admin_public_interface.h"

class MDataInterface;


class MAReplicationStatusPanel : public MAPanel {
    class InfoColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        InfoColumns() {
          add(_icon); add(_server); add(_port); add(_id); add(_kind); add(_status);
          add(_logfile); add(_logpos); add(_is_known);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _server;
        Gtk::TreeModelColumn<int> _port;
        Gtk::TreeModelColumn<int> _id;
        Gtk::TreeModelColumn<Glib::ustring> _kind;
        Gtk::TreeModelColumn<Glib::ustring> _status;
        Gtk::TreeModelColumn<Glib::ustring> _logfile;
        Gtk::TreeModelColumn<Glib::ustring> _logpos;
        Gtk::TreeModelColumn<bool> _is_known;
    } _info_columns;

    Glib::RefPtr<Gdk::Pixbuf> _running_icon;
    Glib::RefPtr<Gdk::Pixbuf> _stopped_icon;
    Glib::RefPtr<Gdk::Pixbuf> _new_host_icon;

    Glib::RefPtr<Gtk::ListStore> _info_store;

    MYX_USER_REPL_HOSTS *_replist;
    
    void update_list();
    
    void update_sensitivity();
    void add_host();
    void remove_host();
    
  public:
    MAReplicationStatusPanel(MAdministrator *app, MDataInterface *data);

    virtual bool before_show();
    virtual bool before_quit();

    virtual bool init();

    virtual bool is_local_only() { return false; };
    virtual bool needs_connection() { return true; };
};

extern MAPanel *create_replication_status_panel(MAdministrator *app, MDataInterface *data);

#endif /* _MAREPLICATIONSTATUSPANEL_H_ */
