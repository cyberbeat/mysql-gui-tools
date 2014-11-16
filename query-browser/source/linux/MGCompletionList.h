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

#ifndef _MGCOMPLETIONLIST_H_
#define _MGCOMPLETIONLIST_H_

#include <gtkmm/scrolledwindow.h>
#include <gtkmm/window.h>
#include <gtkmm/treeview.h>
#include <gtkmm/liststore.h>
#include <gtkmm/textview.h>

class MGCompletionList : public Gtk::Window {
  public:
    typedef sigc::signal2<bool,const Gtk::TextIter&,std::list<Glib::ustring>& > RequestSuggestionsSignal;

  protected:
    class Columns : public Gtk::TreeModel::ColumnRecord {
      public:
        Columns()
        {
//          add(icon);
          add(text);
        };
//        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> text;
    } _columns;

    Gtk::ScrolledWindow _swin;
    Gtk::TreeView _tree;
    Glib::RefPtr<Gtk::ListStore> _store;

    Gtk::TextView *_text_view;
    Glib::RefPtr<Gtk::TextBuffer> _buffer;

    RequestSuggestionsSignal _request_suggestions;
    
    bool _activated;
    
    void reset_size();
    
    bool query_text_key_press(GdkEventKey *ev);
    bool query_text_key_release(GdkEventKey *ev);
    
    void update(bool insert_if_unique= false);
    void insert_selection();
    void insert_word(const Glib::ustring &word);
    
    std::list<Glib::ustring> get_suggestions(const Gtk::TextIter &iter);
  public:
    MGCompletionList(Gtk::TextView *text_view);

    void set_font(const Pango::FontDescription &font);

    void set_list(const std::list<Glib::ustring> &list,
                  const Glib::ustring &input);

    void popup(int x, int y);

    RequestSuggestionsSignal signal_request_suggestions() { return _request_suggestions; };
};


#endif /* _MGCOMPLETIONLIST_H_ */
