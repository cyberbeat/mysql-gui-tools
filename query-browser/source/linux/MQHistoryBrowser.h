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


#ifndef _MQHISTORYBROWSER_H_
#define _MQHISTORYBROWSER_H_

#include "MGBrowserList.h"
#include "MGTreeTooltip.h"

class MQHistory;

class MQHistoryBrowser : public MGBrowserList {
  public:
    typedef sigc::signal1<void,std::string> ActivateSignal;

  protected:
   class Columns : public Gtk::TreeModel::ColumnRecord
    {
      public:
        Columns()
        {
          add(id);
          add(icon);
          add(text);
        }
        Gtk::TreeModelColumn<std::string> id;
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> text;
    } _columns;

    MQHistory *_history;
    
    Glib::RefPtr<Gdk::Pixbuf> _folder_icon;
    Glib::RefPtr<Gdk::Pixbuf> _query_icon;

    Gtk::Menu _menu;
    
    ActivateSignal _signal_activate;
    
    MGTreeTooltip _tooltip;

    void history_activate(const Gtk::TreeModel::Path &path, Gtk::TreeViewColumn *column);
    void history_select();
    
    bool will_show_tip(const Gtk::TreeModel::Path &path);
    
    bool key_pressed(GdkEventKey *ev);

    virtual void refresh_list(const Glib::ustring &filter);

    void delete_mi();
    void open_mi();
    void copy_query_mi();

    void drag_data_get(const Glib::RefPtr<Gdk::DragContext>& context, const Gtk::SelectionData& selection_data, guint info, guint time);
  public:
    MQHistoryBrowser(MQHistory *history);

    ActivateSignal signal_activate() { return _signal_activate; };
};


#endif /* _MQHISTORYBROWSER_H_ */
