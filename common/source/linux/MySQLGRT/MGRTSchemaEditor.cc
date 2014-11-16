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

#include "MGRTSchemaEditor.h"
/**
 * @file  MGRTSchemaEditor.cc
 * @brief 
 */

#include "MGGladeXML.h"
#include "mygpriv.h"


MGRTSchemaEditor::MGRTSchemaEditor(GtkWindow *window)
  : MGRTObjectEditor(window)
{
  _schema_data= 0;
}


MGRTSchemaEditor *MGRTSchemaEditor::create(MGRT *grt, MGRTValue catalog)
{
  MGRTSchemaEditor *editor= 0;
  MGGladeXML *xml;
  
  xml= new MGGladeXML(myg_datadir+"/grt_schema_editor.glade", "editor_window");
  
  xml->get_widget_derived("editor_window", editor);
  
  editor->_xml= xml;
  editor->set_grt(grt);
  editor->set_catalog(catalog);

  editor->setup();
  
  xml->get_button("apply_button")->signal_clicked().connect(sigc::mem_fun(*editor,
                                                                          &MGRTSchemaEditor::apply_changes));
  xml->get_button("close_button")->signal_clicked().connect(sigc::mem_fun(*editor,
                                                                          &MGRTSchemaEditor::close));

  xml->get_tree("charset_tree")->append_column("", editor->_charset_columns.text);

  return editor;
}


void MGRTSchemaEditor::edit_object(MGRTValue object)
{
  _schema_data= new MGRTSchema(_grt->grt(), object);

  show_object();
}


void MGRTSchemaEditor::create_new()
{
  const char *schemaStruct= (*_catalogs)["schemata"].listContentStruct();
  _schema_data= new MGRTSchema(_grt->grt(),
                               MGRTValue::createObject(_grt->grt(), schemaStruct, "newSchema", *_catalogs).grtValue());

  (*_catalogs)["schemata"].append(_schema_data->value().grtValue());

  show_object();
}


MGRTValue MGRTSchemaEditor::edited_object()
{
  return _schema_data->value();
}


bool MGRTSchemaEditor::commit()
{
  _schema_data->setName(_xml->get_entry("schema_entry")->get_text().c_str());
  _schema_data->setComment(_xml->get_text("comment")->get_buffer()->get_text().c_str());
  
  Gtk::TreeIter iter= _xml->get_tree("charset_tree")->get_selection()->get_selected();
  Gtk::TreeRow row= *iter;
  _schema_data->setCollation(((Glib::ustring)row[_charset_columns.collation]).c_str());
  
  _schema_data->commit();

  return true;
}


void MGRTSchemaEditor::revert()
{
  _schema_data->revert();
}


void MGRTSchemaEditor::show_object()
{
  _xml->get_entry("schema_entry")->set_text(_schema_data->name());
  
  Glib::RefPtr<Gtk::ListStore> list= make_collation_list();

  _xml->get_text("comment")->get_buffer()->set_text(_schema_data->comment());
  
  _xml->get_tree("charset_tree")->set_model(list);

  _xml->get_tree("charset_tree")->get_selection()->select(collation_iter(list, _schema_data->collation()));
}

