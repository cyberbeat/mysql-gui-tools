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

#include "MGRTObjectEditor.h"
/**
 * @file  MGRTObjectEditor.cc
 * @brief 
 */

#include <gtkmm/combobox.h>


MGRTObjectEditor::MGRTObjectEditor(GtkWindow *window)
  : Gtk::Window(window), _grt(0), _catalogs(0)
{
}


void MGRTObjectEditor::set_grt(MGRT *grt)
{
  _grt= grt;
}


void MGRTObjectEditor::set_catalog(MGRTValue catalog)
{
  _catalogs= new MGRTValue(catalog);
}


MGRTObjectEditor::~MGRTObjectEditor()
{
  delete _catalogs;
}


const char *MGRTObjectEditor::object_id()
{
  return edited_object().dictId();
}



Gtk::TreeIter MGRTObjectEditor::collation_iter(Glib::RefPtr<Gtk::ListStore> list,
                                               const Glib::ustring &coll)
{
  Gtk::TreeIter iter;
  
  for (iter= list->children().begin(); iter != list->children().end(); ++iter)
  {
    Gtk::TreeRow row= *iter;
    if (row[_charset_columns.collation] == coll)
      return iter;
  }
  return Gtk::TreeIter();
}


Glib::RefPtr<Gtk::ListStore> MGRTObjectEditor::make_collation_list()
{
  Glib::RefPtr<Gtk::ListStore> list= Gtk::ListStore::create(_charset_columns);
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  
  iter= list->append();
  row= *iter;
  
  row[_charset_columns.text]= "";
  row[_charset_columns.collation]= "";

  //XXX add most recently used items

  if (_catalogs && _catalogs->isValid())
  {
    MGRTValue charsets((*_catalogs)["characterSets"]);
    if (charsets.isValid())
    {
      int i, c= charsets.count();
      
      for (i= 0; i < c; i++)
      {
        MGRTValue chs(MGRTValue::refObject(_grt->grt(), charsets[i].asString()));    
        MGRTValue colls(chs["collations"]);
        for (unsigned int j= 0; j < colls.count(); j++)
        {
          iter= list->append();
          row= *iter;
          
          row[_charset_columns.text]= colls[j].asString();
          row[_charset_columns.collation]= colls[j].asString();
        }
      }
    }
  }

  return list;
}


void MGRTObjectEditor::apply_changes()
{
  if (commit())
    _notify_saved.emit();
}


void MGRTObjectEditor::discard_changes()
{
  revert();
  show_object();
}


void MGRTObjectEditor::close()
{
  revert();

  hide();

  _notify_closed.emit();
}


bool MGRTObjectEditor::delete_event(GdkEventAny *ev)
{
  close();
  return true;
}


void MGRTObjectEditor::setup()
{
  signal_delete_event().connect(sigc::mem_fun(*this,&MGRTObjectEditor::delete_event));
}
