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

#include "MGRTViewEditor.h"
#include "MGGladeXML.h"
#include "mygpriv.h"
/**
 * @file  MGRTViewEditor.cc
 * @brief 
 */



bool MGRTViewEditor::commit()
{
  _view_data->setName(_xml->get_entry("name_entry")->get_text().c_str());
  _view_data->setComment(_xml->get_text("comment_text")->get_buffer()->get_text().c_str());

  _view_data->setCode(_xml->get_text("view_text")->get_buffer()->get_text().c_str());

  _view_data->commit();
  
  return true;
}

void MGRTViewEditor::revert()
{
  _view_data->revert();
}


void MGRTViewEditor::setup()
{
  MGRTObjectEditor::setup();
  
  _xml->get_button("apply_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGRTObjectEditor::apply_changes));
  _xml->get_button("close_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGRTObjectEditor::close));
}


void MGRTViewEditor::show_object()
{
  _xml->get_entry("name_entry")->set_text(_view_data->name());
  _xml->get_text("comment_text")->get_buffer()->set_text(_view_data->comment());
  _xml->get_text("view_text")->get_buffer()->set_text(_view_data->code());
}


MGRTViewEditor::MGRTViewEditor(GtkWindow *window)
  : MGRTObjectEditor(window)
{

}


MGRTViewEditor *MGRTViewEditor::create(MGRT *grt, MGRTValue catalog)
{
  MGRTViewEditor *editor= 0;
  MGGladeXML *xml;
  
  xml= new MGGladeXML(myg_datadir+"/grt_view_editor.glade", "editor_window");
  
  xml->get_widget_derived("editor_window", editor);
  
  editor->_xml= xml;
  editor->set_grt(grt);
  editor->set_catalog(catalog);

  editor->setup();

  return editor;
}


MGRTValue MGRTViewEditor::edited_object()
{
  return _view_data->value();
}

void MGRTViewEditor::edit_object(MGRTValue object)
{
  _view_data= new MGRTView(_grt->grt(), object);
  
  show_object();
}


void MGRTViewEditor::create_new()
{

}


