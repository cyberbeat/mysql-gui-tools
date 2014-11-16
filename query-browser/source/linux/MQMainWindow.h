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

#ifndef _MQMAINWINDOW_H_
#define _MQMAINWINDOW_H_

#include "myqb.h"
#include <gtkmm/window.h>
#include <gtkmm/tooltips.h>

#include "MQBookmarks.h"

#include "MYXInterface.h"

#include "MQBaseModule.h"

class MQWorkArea;

class MQQueryDispatcher;

class MGGladeXML;

class MQMainWindow : public MQMainWindowInterface {
    friend class MQWorkArea;

    MGGladeXML *_xml;

    MQWorkArea *_workarea;
    
    Gtk::Tooltips _tips;
    
    MQQueryDispatcher *_dispatcher;
    
    MQBookmarks _bookmarks;

    void about();
    void quit();
    bool close_clicked(GdkEventAny *ev);

    void bookmarks_changed();
    
    void new_connection();
    void show_preferences(bool conn_only= false);

    void start_console();

    bool _has_status;
  
    void open_url(const Glib::ustring &url);
    
  public:
    MQMainWindow(GtkWindow *win);

    bool init(MYSQL *mysql, const MYX::UserConnection &conn);
    
    virtual void set_status(const Glib::ustring &text);
    virtual void set_cursor(int line= -1, int column= -1);

    virtual void bookmark_query(const Glib::ustring &catalog,
                                const Glib::ustring &schema,
                                const Glib::ustring &query);
    virtual MQBookmarks *get_bookmarks() { return &_bookmarks; };

    virtual void show();
	
    void execute_query_raw(const Glib::ustring &query);
    void execute_script_file_new_window(const Glib::ustring &filename);
    
    static MQMainWindow *create();
};

#endif /* _MQMAINWINDOW_H_ */
