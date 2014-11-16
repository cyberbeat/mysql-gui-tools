/* Copyright (C) 2006 MySQL AB

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

#include "MGRT.h"
#include "MGRTPropertiesList.h"
/**
 * @file  MGRTPropertiesList.cc
 * @brief 
 */



MGRTPropertiesList::MGRTPropertiesList(GtkTreeView *tree)
  : Gtk::TreeView(tree), _grt(0)
{
  _store= Gtk::ListStore::create(_columns);
  set_model(_store);
  
  append_column("Property", _columns.name);
  append_column_editable("Value", _columns.value);
  
  get_column(1)->add_attribute(((Gtk::CellRendererText*)get_column(1)->get_first_cell_renderer())->property_editable(),
                               _columns.editable);
  ((Gtk::CellRendererText*)get_column(1)->get_first_cell_renderer())->signal_edited().connect(sigc::mem_fun(*this,&MGRTPropertiesList::value_edited));
  get_column(0)->set_resizable();
  get_column(1)->set_resizable();
}


void MGRTPropertiesList::set_grt(MGRT *grt)
{
  _grt= grt;
}


void MGRTPropertiesList::value_edited(const Glib::ustring &path, const Glib::ustring &value)
{
  Gtk::TreeIter iter= _store->get_iter(Gtk::TreePath(path));
  Gtk::TreeRow row= *iter;
  Glib::ustring key= row[_columns.name];
  int i;
  float f;
  char buffer[100];
  
  switch (_object[key.c_str()].type())
  {
  case MYX_STRING_VALUE:
    row[_columns.value]= value;
    _object.set(key.c_str(), value.c_str());
    break;
  case MYX_INT_VALUE:
    if (sscanf(value.c_str(), "%i", &i) == 1)
    {
      _object.set(key.c_str(), i);
      sprintf(buffer, "%i", _object[key.c_str()].asInt());
      row[_columns.value]= buffer;
    }
    break;
  case MYX_REAL_VALUE:
    if (sscanf(value.c_str(), "%f", &f) == 1)
    {
      _object.set(key.c_str(), f);
      sprintf(buffer, "%.2lf", _object[key.c_str()].asDouble());
      row[_columns.value]= value;
    }
    break;
  default:
    break;
  }
}


void MGRTPropertiesList::display_object(const MGRTValue &object)
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  unsigned int i, c;

  _object= object;
  
  _store->clear();
  
  if (!object.isValid())
    return;
  
  c= object.count();
  for (i= 0; i < c; i++)
  {
    const char *key;
    char *str;
    MGRTValue value;
    
    _object.dictItemByIndex(i, key, value);

    iter= _store->append();
    row= *iter;
    row[_columns.name]= key;
    str= myx_grt_value_formated_as_string(value.grtValue());
    row[_columns.value]= str;

    if ((value.type() == MYX_STRING_VALUE && !_object.memberIsRef(_grt->grt(), key) && strcmp(key, "_id")!=0)
        || value.type() == MYX_INT_VALUE || value.type() == MYX_REAL_VALUE)
      row[_columns.editable]= true;
    else
      row[_columns.editable]= false;
    g_free(str);
  }  
}

