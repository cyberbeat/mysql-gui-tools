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

#include "MGRTRoutineGroupEditor.h"
#include "MGGladeXML.h"
#include "mygpriv.h"
/**
 * @file  MGRTRoutineGroupEditor.cc
 * @brief 
 */



bool MGRTRoutineGroupEditor::commit()
{
  for (std::list<MGRTRoutineEmbeddedEditor*>::iterator iter= _editors.begin();
       iter != _editors.end(); ++iter)
  {
    (*iter)->apply_changes();
  }

  _group_data->setName(_xml->get_entry("name_entry")->get_text().c_str());

  _group_data->commit();

  return true;
}


void MGRTRoutineGroupEditor::revert()
{
}


void MGRTRoutineGroupEditor::add_editor(MGRTValue *routine)
{
  MGRTRoutineEmbeddedEditor *editor= new MGRTRoutineEmbeddedEditor(_grt, *routine);
  
  _xml->get_box("content_box")->pack_start(*editor, false, true);

  editor->show_object();
  
  _editors.push_back(editor);
}


void MGRTRoutineGroupEditor::add_routine(bool function)
{
  MGRTValue routine;
  MGRTValue args(MGRTValue::createList());

  args.append(_group_data->ownerSchema());
  args.append(MGRTValue("test"));
  routine= _grt->call_function("Workbench", "addRoutine", args);

  if (routine.isValid())
  {
    MGRTValue grtRoutine(routine);

    _group_data->addRoutine(grtRoutine);

    if (!function)
    {
      grtRoutine.set("routineCode",
                     "-- Comment\n"
                     "-- \n\n"
                     "CREATE PROCEDURE test()\n"
                     "BEGIN\n\n"
                     "END");
    }
    else
    {
      grtRoutine.set("routineCode",
                     "-- Comment\n"
                     "-- \n\n"
                     "CREATE FUNCTION test() RETURNS INT\n"
                     "BEGIN\n"
                     "  RETURN 0;\n"
                     "END");
    }

    add_editor(&grtRoutine);
  }
  else
    g_warning("Error calling addRoutine: %s", _grt->last_error_description().c_str());
}


void MGRTRoutineGroupEditor::show_object()
{
  for (int i= 0; i < _group_data->routineCount(); i++)
  {
    MGRTValue routine(_group_data->getRoutine(i));

    add_editor(&routine);
  }

  _xml->get_entry("name_entry")->set_text(_group_data->name());
}


MGRTValue MGRTRoutineGroupEditor::edited_object()
{
  return _group_data->value();
}


MGRTRoutineGroupEditor::MGRTRoutineGroupEditor(GtkWindow *window)
  : MGRTObjectEditor(window)
{

}


MGRTRoutineGroupEditor *MGRTRoutineGroupEditor::create(MGRT *grt, MGRTValue catalog)
{
  MGRTRoutineGroupEditor *editor= 0;
  MGGladeXML *xml;
  
  xml= new MGGladeXML(myg_datadir+"/grt_routines_editor.glade", "editor_window");
  
  xml->get_widget_derived("editor_window", editor);
  
  editor->_xml= xml;
  editor->set_grt(grt);
  editor->set_catalog(catalog);

  editor->setup();

  return editor;
}


void MGRTRoutineGroupEditor::setup()
{
  MGRTObjectEditor::setup();
  
  _xml->get_button("apply_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGRTObjectEditor::apply_changes));
  _xml->get_button("close_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGRTObjectEditor::close));
  
  _xml->get_button("add_procedure_button")->signal_clicked().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MGRTRoutineGroupEditor::add_routine),false));
  _xml->get_button("add_function_button")->signal_clicked().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MGRTRoutineGroupEditor::add_routine),true));
}


void MGRTRoutineGroupEditor::edit_object(MGRTValue object)
{
  _group_data= new MGRTRoutineGroup(_grt->grt(), object.grtValue());
  show_object();
}


void MGRTRoutineGroupEditor::create_new()
{
}

