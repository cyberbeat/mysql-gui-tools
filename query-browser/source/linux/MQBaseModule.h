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


// modules are the Query Browser and Script Editor


#ifndef _MQBASEMODULE_H_
#define _MQBASEMODULE_H_

#include <gtkmm/box.h>
#include <gtkmm/notebook.h>
#include <gtkmm/menu.h>
#include <gtkmm/window.h>
#include <gtkmm/treeview.h>

namespace Gtk {
  class MenuBar;
};

#include "myx_public_interface.h"
#include "MGPtrWrap.h"
#include "MQSavedState.h"

class MQBookmarks;

class MQMainWindowInterface : public Gtk::Window {
  public:
    enum ViewType {
      VNone,
      VQuery,
      VScript
    };

    MQSavedState *_state;
    MQMainWindowInterface(GtkWindow *win) : Gtk::Window(win), _state(0) {};

    virtual ~MQMainWindowInterface() {};
    
    virtual void set_status(const Glib::ustring &text)= 0;
    virtual void set_cursor(int line= -1, int column= -1)= 0;
    
    virtual void quit()= 0;
    
    //XXX move back to MQWorkArea?
    virtual void bookmark_query(const Glib::ustring &catalog,
                                const Glib::ustring &schema,
                                const Glib::ustring &query)= 0;
    virtual MQBookmarks *get_bookmarks()= 0;
};


class MQQueryDispatcher;
class MQBaseTab;

class MQBaseModule : public Gtk::VBox {
    class SchemaColumns : public Gtk::TreeModel::ColumnRecord
    {
      public:
        SchemaColumns()
        {
          add(icon); add(text); add(schema);
        }
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> text;
        Gtk::TreeModelColumn<MYX_SCHEMA*> schema;
    } _schema_columns;

  protected:
    MQQueryDispatcher *_dispatcher;

    Gtk::Notebook *_note;
    
    Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > _catalogs;

    Glib::RefPtr<Gtk::AccelGroup> _accel_group;
    
    MQMainWindowInterface *_mainw;

    virtual void set_dispatcher(MQQueryDispatcher *dispatcher);

    virtual MQBaseTab *get_tab(int index=-1);

    virtual void add_tab(MQBaseTab *tab);
    virtual bool close_tab(MQBaseTab *tab);
    virtual bool close_other_tabs(MQBaseTab *tab);
    
    bool tab_clicked(GdkEventButton *ev,MQBaseTab *tab);

    void ok_schema_selection(Gtk::TreeView *tree);
    void cancel_schema_selection(GdkEventAny *a);

    virtual void catalogs_refreshed() {};

    virtual Gtk::Menu *get_tab_menu()= 0;
    
  public:
    bool schemata_fetch_tables(const Glib::ustring &catalog,const Glib::ustring&schema,MYX_SCHEMA_TABLES *&tables);
    bool schemata_fetch_sps(const Glib::ustring &catalog,const Glib::ustring&schema,MYX_SCHEMA_STORED_PROCEDURES *&sps);

    void select_schema();
    
    void refresh_catalogs();

  public:
    MQBaseModule(GtkVBox *vbox);

    Glib::RefPtr<Gtk::AccelGroup> get_accel_group() { return _accel_group; };

    virtual void setup()= 0;

    virtual void show()= 0;
    virtual void hide()= 0;

    virtual Gtk::Widget *get_widget()= 0;
};



#endif /* _MQBASEMODULE_H_ */
