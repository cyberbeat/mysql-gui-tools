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

#ifndef _MQFUNCTIONBROWSER_H_
#define _MQFUNCTIONBROWSER_H_

#include "MGBrowserList.h"
#include "myx_public_interface.h"
#include "myx_qb_public_interface.h"

class MQFunctionBrowser : public MGBrowserList {
  public:
    typedef sigc::signal1<void,const std::string&> FunctionActivateSignal;

  private:
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

    MYX_SQL_FUNCTIONINDEX *_functions;

    Glib::RefPtr<Gdk::Pixbuf> _folder_icon;
    Glib::RefPtr<Gdk::Pixbuf> _node_icon;

    FunctionActivateSignal _signal_activate;

    void function_select();
    void function_activate(const Gtk::TreeModel::Path &path, Gtk::TreeViewColumn *column);

    virtual void refresh_list(const Glib::ustring &filter);

  public:
    MQFunctionBrowser(MYX_SQL_FUNCTIONINDEX *findex, const std::string &icon_file);
    
    FunctionActivateSignal signal_activate() { return _signal_activate; };
};


#endif /* _MQFUNCTIONBROWSER_H_ */
