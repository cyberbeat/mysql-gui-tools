/* Copyright (C) 2005 MySQL AB

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

#ifndef _MGRTSCHEMAEDITOR_H_
#define _MGRTSCHEMAEDITOR_H_

#include <MySQLGRT/MGRTObjectEditor.h>
#include <MySQLGRT/MGRTSchema.h>

class MGGladeXML;

class MGRTSchemaEditor : public MGRTObjectEditor {
    MGGladeXML *_xml;
    
    MGRTSchema *_schema_data;

    virtual MGRTValue edited_object();
    virtual bool commit();
    virtual void revert();
    virtual void show_object();

  public:
    MGRTSchemaEditor(GtkWindow *window);
    static MGRTSchemaEditor *create(MGRT *grt, MGRTValue catalog);
    
    void edit_object(MGRTValue object);
    void create_new();
};

#endif /* _MGRTSCHEMAEDITOR_H_ */
