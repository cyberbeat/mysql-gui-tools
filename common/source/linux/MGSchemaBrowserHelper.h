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

#ifndef _MGSCHEMAHELPER_H_
#define _MGSCHEMAHELPER_H_

#include "MGTableBrowserList.h"

class MGTableEditor;

class MGSchemaBrowserHelper : public Glib::ObjectBase {
    MGTableEditor *_editor;
    
    MGTableBrowserList *_browser;
    MYSQL *_mysql;

    sigc::signal0<void> _signal_refresh;
    
    bool confirm(const Glib::ustring &message);

    void table_editor_closed();

    void edit_table();
  public:
    MGSchemaBrowserHelper(MGTableBrowserList *browser);

    void set_mysql(MYSQL *mysql);

    void create_schema();
    void create_table();

    void edit_object();
    void drop_object();

    sigc::signal0<void>signal_changed() { return _signal_refresh; };
};


#endif /* _MGSCHEMABROWSERHELPER_H_ */
