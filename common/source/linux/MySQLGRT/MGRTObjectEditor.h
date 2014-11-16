/* Copyright (C) 2005 MySQL AB

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

#ifndef _MGRTOBJECTEDITOR_H_
#define _MGRTOBJECTEDITOR_H_

#include <MySQLGRT/MGRT.h>

#include <gtkmm/window.h>
#include <gtkmm/treeview.h>
#include <gtkmm/liststore.h>

namespace Gtk {
class ComboBox;
};

class MGRTObjectEditor : public Gtk::Window {
  protected:
    class CharsetColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        CharsetColumns() { add(text); add(collation); };

        Gtk::TreeModelColumn<Glib::ustring> text;
        Gtk::TreeModelColumn<Glib::ustring> collation;
    } _charset_columns;
    
    MGRT *_grt;
    MGRTValue *_catalogs;

    sigc::signal<void> _notify_saved;
    sigc::signal<void> _notify_closed;

    MGRTObjectEditor(GtkWindow *window);
    virtual void set_grt(MGRT *grt);
    virtual void set_catalog(MGRTValue catalog);

    virtual MGRTValue edited_object()= 0;

    Glib::RefPtr<Gtk::ListStore> make_collation_list();
    Gtk::TreeIter collation_iter(Glib::RefPtr<Gtk::ListStore> list,
                                 const Glib::ustring &coll);

    virtual bool commit()= 0;
    virtual void revert()= 0;
    virtual void show_object()= 0;
    virtual void setup();
    
    virtual bool delete_event(GdkEventAny *ev);

  public:
    virtual ~MGRTObjectEditor();

    virtual void apply_changes();
    virtual void discard_changes();
    virtual void close();
    const char *object_id();
    
    sigc::signal<void> signal_saved() { return _notify_saved; };
    sigc::signal<void> signal_closed() { return _notify_closed; };
};


#endif /* _MGRTOBJECTEDITOR_H_ */
