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


#include <MGGladeXML.h>

#include "myx_migration_public_interface.h"

class GRTEnvironment;

class ObjectShell : public Glib::ObjectBase {
    class OTColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        OTColumns() {
          add(icon); add(text); add(object); add(type);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> text;
        Gtk::TreeModelColumn<int> type;
        Gtk::TreeModelColumn<void*> object;
    } _otcols;

    class OIColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        OIColumns() {
          add(name); add(value);
        };
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<Glib::ustring> value;
    } _oicols;

    MGGladeXML *_xml;
    
    Glib::RefPtr<Gtk::TreeStore> _otree;
    Glib::RefPtr<Gtk::ListStore> _ilist;

    std::vector<Glib::ustring> _history;
    int _history_index;
    
    bool _publicOnly;
    
    GRTEnvironment *_env;
    
    void object_selected();
    void print_shell(const Glib::ustring &text);
    bool cursor_move(Gtk::MovementStep step, int count, bool extend_selection);

    void put_prompt();
    bool shell_key_press(GdkEventKey *ev);
    
    void build_object_tree();
    void build_object_tree(MYX_GRT_OBJ *obj,const Gtk::TreeIter &parent);
    
  public:
    ObjectShell(GRTEnvironment *env);

    void show();
};
