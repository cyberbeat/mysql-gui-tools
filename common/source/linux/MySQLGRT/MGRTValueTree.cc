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

#include "MGRTValueTree.h"
#include "myg_gtkutils.h"
#include "myg_utils.h"
#include "mygpriv.h"


MGRTValueTree::MGRTValueTree(MGRT *grt, const Glib::ustring &root_path,
                             const Glib::ustring &title)
  : _grt(grt)
{
  Gtk::TreeViewColumn *column;

  _store= Gtk::TreeStore::create(_columns);
  set_model(_store);

  column= new Gtk::TreeViewColumn(title);
  column->pack_start(_columns.icon, false);
  column->pack_start(_columns.text);
  column->set_resizable(true);
  append_column(*Gtk::manage(column));
  
  _simple_icon= PIXCACHE->load("grt_value_simple.png");
  _dict_icon= PIXCACHE->load("grt_value_dict.png");
  _list_icon= PIXCACHE->load("grt_value_list.png");

  set_root_path(root_path);
  
  refresh();
}


MGRTValueTree::MGRTValueTree(GtkTreeView *tree)
  : Gtk::TreeView(tree), _grt(0)
{
  Gtk::TreeViewColumn *column;

  _store= Gtk::TreeStore::create(_columns);
  set_model(_store);

  column= new Gtk::TreeViewColumn("");
  column->pack_start(_columns.icon, false);
  column->pack_start(_columns.text);
  column->set_resizable(true);
  append_column(*Gtk::manage(column));
  
  _simple_icon= PIXCACHE->load("grt_value_simple.png");
  _dict_icon= PIXCACHE->load("grt_value_dict.png");
  _list_icon= PIXCACHE->load("grt_value_list.png");
}


void MGRTValueTree::set_grt(MGRT *grt)
{
  _grt= grt;
}


void MGRTValueTree::set_root_path(const Glib::ustring &root_path)
{
  _root_path= root_path;
}


void MGRTValueTree::set_title(const Glib::ustring &title)
{
  get_column(0)->set_title(title);
}



void MGRTValueTree::set_icon(Gtk::TreeRow row, MGRTValue value)
{
  std::string path;

  switch (value.type())
  {
  case MYX_DICT_VALUE:
    {
      MYX_GRT_STRUCT *vstr= myx_grt_dict_struct_get(_grt->grt(), value.grtValue());

      if (vstr)
        path= myx_grt_struct_get_icon_path(_grt->grt(), vstr, MYX_IT_SMALL);
      else
        path= "";
      if (!path.empty())
      {
        Glib::RefPtr<Gdk::Pixbuf> icon;
        try {
          icon= PIXCACHE->load(path);
        } catch (Glib::Error &exc) {
          icon.clear();
        };
        if (_icon_info.find(myx_grt_struct_get_name(vstr))!=_icon_info.end())
          icon= _icon_info[myx_grt_struct_get_name(vstr)];
        if (!icon)
        {
          try {
            icon= PIXCACHE->load("GrtObject.16x16.png");
          } catch (Glib::Error &exc) {
            icon.clear();
          }
        }
        if (!icon)
          icon= _dict_icon;

        row[_columns.icon]= icon;
      }
      else
        row[_columns.icon]= _dict_icon;
      break;
    }

  case MYX_LIST_VALUE:
    row[_columns.icon]= _list_icon;
    break;

  default:
    row[_columns.icon]= _simple_icon;
    break;
  }
}


void MGRTValueTree::add_list_to_store(MGRTValue list, Gtk::TreeRow *parent, std::list<MYX_GRT_VALUE*> *expanded_list)
{
  unsigned int c= list.count();
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  Gtk::TreePath path;
  
  if (parent)
    path= Gtk::TreePath(*parent);
  
  for (unsigned int i= 0; i < c; i++)
  {
    MGRTValue value(list[i]);
    
    if (parent)
      iter= _store->append(parent->children());
    else
      iter= _store->append();
    row= *iter;

    switch (value.type())
    {
    case MYX_STRING_VALUE:
      if (list.listContentStruct())
      {
        MGRTValue rvalue= MGRTValue::refObject(_grt->grt(), value.asString());

        set_icon(row, rvalue);
        row[_columns.text]= rvalue["name"].asString();
        row[_columns.data]= rvalue;
        add_dict_to_store(rvalue, row, expanded_list);
      }
      break;
      
    case MYX_LIST_VALUE:
      set_icon(row, value);
      row[_columns.text]= "";
      row[_columns.data]= value;
      add_list_to_store(value, &row, expanded_list);
      break;

    case MYX_DICT_VALUE:
      set_icon(row, value);
      row[_columns.text]= value["name"].isValid() ? value["name"].asString() : "";
      row[_columns.data]= value;
      add_dict_to_store(value, row, expanded_list);
      break;

    default:
      break;
    }
  }
  
  if (expanded_list && std::find(expanded_list->begin(), expanded_list->end(), list.grtValue()) != expanded_list->end())
    expand_to_path(path);
}


void MGRTValueTree::add_dict_to_store(MGRTValue dict, Gtk::TreeRow &parent, std::list<MYX_GRT_VALUE*> *expanded_list)
{
  unsigned int c= dict.countComplex();

  Gtk::TreePath path(parent);

  Gtk::TreeIter iter;
  
  for (unsigned int i= 0; i < c; i++)
  {
    MGRTValue value;
    const char *key;
    
    dict.complexDictItemByIndex(i, key, value);
    
    if (!_display_filter || !_display_filter(value))
      continue;

    iter= _store->append(parent.children());
    Gtk::TreeRow row= *iter;

    set_icon(row, value);
    row[_columns.text]= key;
    row[_columns.data]= value;
    
    switch (value.type())
    {
    case MYX_DICT_VALUE:
      add_dict_to_store(value, row, expanded_list);
      break;
    case MYX_LIST_VALUE:
      add_list_to_store(value, &row, expanded_list);
      break;
    default:
      break;
    }
  }
  if (expanded_list && std::find(expanded_list->begin(), expanded_list->end(), dict.grtValue()) != expanded_list->end())
    expand_to_path(path);
}


void MGRTValueTree::remember_path(Gtk::TreeView *tree, const Gtk::TreeModel::Path &path, std::list<MYX_GRT_VALUE*> &list)
{
  Gtk::TreeIter iter= _store->get_iter(path);
  Gtk::TreeRow row= *iter;

  MGRTValue value= row[_columns.data];
  list.push_back(value.grtValue());
}


void MGRTValueTree::refresh()
{
  std::list<MYX_GRT_VALUE*> expand_state;
  
  // remember expanded
  map_expanded_rows(sigc::bind(sigc::mem_fun(*this,&MGRTValueTree::remember_path), expand_state));

  // rebuild list
  _store->clear();
  
  MGRTValue root(_grt->global_value(_root_path.empty() ? "/" : _root_path));
  
  if (root.isValid() && root.type() == MYX_LIST_VALUE)
  {
    add_list_to_store(root, 0, &expand_state);
  }
  else
  {
    Gtk::TreeIter iter;
    Gtk::TreeRow row;
    iter= _store->append();
    row= *iter;
    row[_columns.icon]= _dict_icon;
    if (_root_path.empty())
      row[_columns.text]= "root";
    else
      row[_columns.text]= myx_grt_dict_name_item_as_string(root.grtValue());
    row[_columns.data]= root;
    add_dict_to_store(root, row, &expand_state);
  }
  
  if (!_store->children().empty())
    expand_row(Gtk::TreePath(*_store->children().begin()), false);
}


void MGRTValueTree::set_icon_info(const std::map<std::string, Glib::RefPtr<Gdk::Pixbuf> > &icon_info)
{
  _icon_info= icon_info;
}


void MGRTValueTree::set_display_filter(sigc::slot<bool,MGRTValue> filter)
{
  _display_filter= filter;
}


MGRTValue MGRTValueTree::value_at_row(const Gtk::TreePath &path)
{
  Gtk::TreeIter iter= _store->get_iter(path);
  if (iter)
  {
    Gtk::TreeRow row= *iter;
  
    return row[_columns.data];
  }
  return MGRTValue();
}
