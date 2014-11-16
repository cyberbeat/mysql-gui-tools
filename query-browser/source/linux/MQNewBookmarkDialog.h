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

#ifndef _MQNEWBOOKMARKDIALOG_H_
#define _MQNEWBOOKMARKDIALOG_H_

#include "MQBookmarks.h"


#include <gtkmm/window.h>
#include <gtkmm/treemodel.h>
#include <gtkmm/treestore.h>
#include <gtkmm/liststore.h>


class MGGladeXML;

class MQNewBookmarkDialog : public Gtk::Window {
    class Columns : public Gtk::TreeModel::ColumnRecord
    {
      public:
        Columns()
        {
          add(icon);
          add(name);
          add(group);
        }
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<MQBookmarks::BookmarkGroup*> group;
    } _columns;

    
    MGGladeXML *_xml;

    Glib::RefPtr<Gtk::TreeStore> _store;
    Glib::RefPtr<Gtk::ListStore> _lstore;
    
    bool _details_shown;
    
    MQBookmarks::BookmarkGroup *_bookmarks;

    bool _needs_save;
    bool _added;
    
    void name_changed();
    void add_clicked();
    void cancel_clicked();
    void new_folder_clicked();
    void folder_selected();

    void renamed_folder(const Glib::ustring &path, const Glib::ustring &new_value);
    void toggle_detail();

    void add_group_to_tree(const Gtk::TreeModel::Children &children,
                           MQBookmarks::BookmarkGroup *group);

  public:
    MQNewBookmarkDialog(GtkWindow *win, MGGladeXML *xml);
    
    void set_bookmarks(MQBookmarks::BookmarkGroup *bm);
    
    bool run(const Glib::ustring &catalog,
             const Glib::ustring &schema,
             const Glib::ustring &query);
};



#endif /* _MQNEWBOOKMARKDIALOG_H_ */
