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




#ifndef _MGUSERBROWSERLIST_H_
#define _MGUSERBROWSERLIST_H_

#include "MGBrowserList.h"
#include "myx_admin_library.h"
#include <map>
#include <list>

class MGUserBrowserList : public MGBrowserList {
  public:
    class Columns : public Gtk::TreeModel::ColumnRecord {
      public:
        Columns() { add(_icon); add(_text); add(_populated); };

        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _text;
        Gtk::TreeModelColumn<bool> _populated;
    };

    Columns _columns;

  public:
    typedef sigc::slot<bool,MGUserBrowserList*,const Gtk::TreeIter&,std::list<Glib::ustring>&> PopulateSlot;

  protected:
    virtual void refresh_list(const Glib::ustring &filter);

    MYX_USER_NAMES *_user_list;
    Glib::RefPtr<Gdk::Pixbuf> _default_icon;

    PopulateSlot _populate_slot;
    
    Gtk::TreeIter find_user(const Glib::ustring &user); 

  public:

    MGUserBrowserList(const Glib::ustring &caption);
    virtual ~MGUserBrowserList();

    void set_default_icon(const Glib::RefPtr<Gdk::Pixbuf> &icon);

    void set_user_list(MYX_USER_NAMES *users);

    void get_row_user(const Gtk::TreeIter &node, 
                      Glib::ustring &user, Glib::ustring &host);

    bool exists_user(const Glib::ustring &user);

    bool select_user(const Glib::ustring &user, 
                     const Glib::ustring &host=Glib::ustring(),
                     bool expand= false);

    void populate_node(const Gtk::TreeIter &node);

    void set_populate_func(const PopulateSlot &pslot);
};


#endif /* _MGUSERBROWSERLIST_H_ */
