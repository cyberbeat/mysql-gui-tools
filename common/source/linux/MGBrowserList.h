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



#ifndef _MGBROWSERLIST_H_
#define _MGBROWSERLIST_H_


#include <gtkmm.h>
#include <map>


class MGBrowserList : public Gtk::VBox {
  public:
    typedef sigc::signal<bool,MGBrowserList*,const Gtk::TreeIter&> SelectionChangeDelegate;

    typedef sigc::signal<void,MGBrowserList*,const Gtk::TreeIter&> SelectionSignal;

  protected:
    class CustomTreeView : public Gtk::TreeView {        
      protected:
        virtual bool on_button_press_event(GdkEventButton *ev);
      public:
        CustomTreeView() : Gtk::TreeView(), popup(0) {};
        Gtk::Menu *popup;
    };

  private:
    void menu_activate_handler(int index);
    bool popup_handler(GdkEventButton *event);
    void entry_changed_handler();
    //void list_row_expanded_handler(const Gtk::TreeIter &iter,
    //                               const Gtk::TreePath &path);
        
    void open_popup(GdkEventButton *event);

    bool emit_when_idle();
  protected:
    SelectionSignal _selection_signal;
    SelectionChangeDelegate _selection_change_delegate;

    Gtk::Entry *_entry;
    Gtk::Menu *_menu;
    
    CustomTreeView *_tree;
    Glib::RefPtr<Gtk::TreeStore> _store;

    bool _refreshing;
    bool _selkluge;
    
    bool _queued_idle_selection;
    
    int _selected_menu_item;

    Gtk::Widget *create_search_entry(bool with_popup);

    virtual void refresh_list(const Glib::ustring &filter)=0;

    virtual bool list_selection_function(const Glib::RefPtr<Gtk::TreeModel> &model,
                                         const Gtk::TreeModel::Path &path,
                                         bool path_currently_selected);
    
    void list_selection_changed();
  public:
    MGBrowserList(bool with_popup, const Glib::ustring &caption);
    ~MGBrowserList();

//    void set_allow_multiple_selection(bool flag);

    virtual void clear();

    void refresh();

    void set_popup_menu(Gtk::Menu *menu);

    void set_store(const Glib::RefPtr<Gtk::TreeStore> &store);
    void set_menu_items(std::vector<Glib::ustring> &items);

    void unselect();
    Gtk::TreeIter get_selected();
    void set_selection(const Gtk::TreeIter &iter);
    
    Gtk::TreeIter get_iter(const Gtk::TreePath &path);

    bool find_node(const Gtk::TreeIter &parent, const Glib::ustring &text,
                   const Gtk::TreeModelColumn<Glib::ustring>& column,
                   Gtk::TreeIter &node_ret);

    bool find_node(const Glib::ustring &text,
                   const Gtk::TreeModelColumn<Glib::ustring>& column,
                   Gtk::TreeIter &node_ret);

    void mark_node(const Gtk::TreeIter &node,
                   const Gtk::TreeModelColumn<Gdk::Color>& column,
                   bool flag, bool follow_parents);

    void expand_node(const Gtk::TreeIter &node);

    Gtk::TreeView *get_tree() { return _tree; };
    
    void clear_filter() { _entry->set_text(Glib::ustring("")); refresh_list(Glib::ustring("")); }

  public:
    SelectionSignal signal_selected();
    SelectionChangeDelegate signal_selection_will_change() { return _selection_change_delegate; };
};

#endif /* _MGBROWSERLIST_H_ */
