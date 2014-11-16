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

#include "MQFunctionBrowser.h"
#include "myg_gtkutils.h"


MQFunctionBrowser::MQFunctionBrowser(MYX_SQL_FUNCTIONINDEX *findex,
                                     const std::string &icon_file)
  : MGBrowserList(false, ""), _functions(findex)
{
  Gtk::TreeViewColumn *column= new Gtk::TreeView::Column("");
  column->pack_start(_columns.icon, false);
  column->pack_start(_columns.text);
  _tree->append_column(*Gtk::manage(column));

  _tree->signal_row_activated().connect(sigc::mem_fun(*this,&MQFunctionBrowser::function_activate));
  
  set_store(Gtk::TreeStore::create(_columns));

  _folder_icon= PIXCACHE->load("folder_open_14x14.png");
  _node_icon= PIXCACHE->load(icon_file);
}


void MQFunctionBrowser::refresh_list(const Glib::ustring &filter)
{
  if (!_functions)
    return;
  
  _store->clear();
  
  for (unsigned int i= 0; i < _functions->groups_num; i++)
  {
    MYX_SQL_FUNCTIONGROUP *group= _functions->groups+i;
    Gtk::TreeIter giter= _store->append();
    Gtk::TreeRow grow= *giter;
    bool empty= true;

    grow[_columns.icon]= _folder_icon;
    grow[_columns.text]= group->caption?:"";

    for (unsigned int j= 0; j < group->functions_num; j++)
    {      
      MYX_SQL_FUNCTION *func= group->functions+j;
      
      if (filter.empty() || myx_match_pattern(func->caption, filter.c_str(), 0, 0)>0)
      {
        Gtk::TreeIter iter= _store->append(grow.children());
        Gtk::TreeRow row= *iter;

        row[_columns.icon]= _node_icon;
        row[_columns.text]= func->caption?:"";
        row[_columns.id]= func->id?:"";
        
        empty= false;
      }
    }
    
    if (!empty)
      _tree->expand_row(_store->get_path(giter), true);
  }
}


void MQFunctionBrowser::function_activate(const Gtk::TreeModel::Path &path, Gtk::TreeViewColumn *column)
{
  Gtk::TreeIter iter= _store->get_iter(path);
  Gtk::TreeRow row= *iter;
  std::string id= row[_columns.id];

  if (!id.empty())
    _signal_activate.emit(id);
  else
  {
    if (_tree->row_expanded(_store->get_path(iter)))
      _tree->collapse_row(_store->get_path(iter));
    else
      _tree->expand_row(_store->get_path(iter), false);
  }
}

