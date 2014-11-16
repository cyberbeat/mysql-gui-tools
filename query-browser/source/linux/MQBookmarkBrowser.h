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


#ifndef _MQBOOKMARKBROWSER_H_
#define _MQBOOKMARKBROWSER_H_

#include "MGBrowserList.h"
#include "MQBookmarks.h"
#include "MGTreeTooltip.h"

class MQBookmarkBrowser : public MGBrowserList {
  public:
    typedef sigc::signal1<void,MQBookmarks::BookmarkItem*> BookmarkActivateSignal;

  protected:
    class Columns : public Gtk::TreeModel::ColumnRecord
    {
      public:
        Columns()
        {
          add(object);
          add(icon);
          add(text);
          add(disabled);
        }
        Gtk::TreeModelColumn<MQBookmarks::Bookmark*> object;
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> text;
        Gtk::TreeModelColumn<bool> disabled;
    } _columns;

    MQBookmarks *_bookmarks;
    
    Glib::RefPtr<Gdk::Pixbuf> _folder_icon;
    Glib::RefPtr<Gdk::Pixbuf> _query_icon;

    BookmarkActivateSignal _signal_activate;

    Gtk::Menu _menu;
    
    MGTreeTooltip _tooltip;

    bool tooltip_get(const Gtk::TreeModel::Path &path);
    
    void bookmark_activate(const Gtk::TreeModel::Path &path, Gtk::TreeViewColumn *column);
    void bookmark_select();
    bool key_pressed(GdkEventKey *ev);

    bool tree_motion_notify_event(GdkEventMotion* event);
    
    void drag_data_get(const Glib::RefPtr<Gdk::DragContext>& context, 
      const Gtk::SelectionData& selection_data, guint info, guint time);
    
    int add_group(const Gtk::TreeModel::Children &children,
                  MQBookmarks::BookmarkGroup *group,
                  const Glib::ustring &filter);
    
    virtual void refresh_list(const Glib::ustring &filter);
    bool set_activatable(const Gtk::TreeIter &iter, 
                         const Glib::ustring &catalog, const Glib::ustring &schema);

    void commit_reordered();
    
    void open_mi();
    void new_folder_mi();
    void copy_query_mi();
    void delete_mi();
    void rename_mi();
  public:
    MQBookmarkBrowser(MQBookmarks *bookmarks);

    void refresh_activatable(const Glib::ustring &catalog, const Glib::ustring &schema);
    
    BookmarkActivateSignal signal_activate() { return _signal_activate; };
};


#endif /* _MQBOOKMARKBROWSER_H_ */
