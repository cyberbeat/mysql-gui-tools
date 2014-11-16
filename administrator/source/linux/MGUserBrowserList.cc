/* Copyright (C) 2003, 2004 MySQL AB

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


#include "MGUserBrowserList.h"
#include "myg_gtk_utils.h"

MGUserBrowserList::MGUserBrowserList(const Glib::ustring &caption)
    : MGBrowserList(false, caption), _user_list(0)//,
//      _populate_handler(0)
{
  Gtk::TreeView::Column *column;

  set_store(Gtk::TreeStore::create(_columns));

  // icon column
  column= new Gtk::TreeView::Column("");
  column->pack_start(_columns._icon, false);
  column->pack_start(_columns._text);
  _tree->append_column(*Gtk::manage(column));
}


MGUserBrowserList::~MGUserBrowserList()
{
}


void MGUserBrowserList::set_default_icon(const Glib::RefPtr<Gdk::Pixbuf> &icon)
{
  if (icon->get_height()!=16)
      _default_icon= icon->scale_simple(16, 16, Gdk::INTERP_BILINEAR);
  else
      _default_icon= icon;
}


void MGUserBrowserList::refresh_list(const Glib::ustring &filter)
{
  if (_user_list)
  {
    std::map<Glib::ustring,bool> expanded_nodes;

    // get list of expanded nodes
    for (Gtk::TreeIter it= _store->children().begin();
         it != _store->children().end(); ++it)
    {
      if (_tree->row_expanded(Gtk::TreePath(it)))
      {
        Gtk::TreeModel::Row row= *it;
        expanded_nodes[row[_columns._text]]= true;
      }
    }

    Gtk::TreeIter iter;
    Gtk::TreeIter old_user_iter, old_host_iter;
    Glib::ustring old_user_buf, old_host;
    Glib::ustring *old_user;

    _refreshing= true;

    iter= get_selected();
    if (iter)
    {
      get_row_user(iter, old_user_buf, old_host);
      old_user= &old_user_buf;
    }
    else
      old_user=0;

    clear();

    for (unsigned int i= 0; i < _user_list->user_names_num; i++)
    {
      if (myx_match_pattern(_user_list->user_names[i], filter.c_str(), 0, 0)!=0)
      {
        iter= _store->append();

        Gtk::TreeModel::Row row= *iter;

        row[_columns._icon]= _default_icon;//get_icon_for_user((char*)_user_list->user_names[i]);
        row[_columns._text]= (char*)_user_list->user_names[i];

        populate_node(iter);

        if (old_user && old_user->compare((char*)_user_list->user_names[i])==0)
        {
          old_user_iter= iter;

          if (!old_host.empty())
          {
            Gtk::TreeModel::Children hosts= row.children();
            // locate old host node
            for (Gtk::TreeIter jter= hosts.begin(); jter != hosts.end(); ++jter)
            {
              Gtk::TreeModel::Row jrow= *jter;
              
              if (old_host.compare(jrow[_columns._text])==0)
              {
                old_host_iter= jter;
                break;
              }
            }
          }
        }
        // expand node if it was expanded before
        if (expanded_nodes.find((char*)_user_list->user_names[i])!=expanded_nodes.end())
        {
          expand_node(iter);
        }
      }
    }

    if (old_host_iter)
    {
      expand_node(old_user_iter);
      set_selection(old_host_iter);
    }
    else if (old_user_iter)
    {
      set_selection(old_user_iter);
    }

    _refreshing= false;

    // old selected item was removed from list
    if (!old_user_iter && old_user!=0)
      list_selection_changed();
  }
  else
  {
    clear();
  }
}


void MGUserBrowserList::get_row_user(const Gtk::TreeIter &node, 
                                     Glib::ustring &user, Glib::ustring &host)
{
  Glib::ustring text;
  Gtk::TreeModel::Row row= *node;
  Gtk::TreeIter parent;

  text= row[_columns._text];

  parent= row.parent();

  if (parent)
  {
    row= *parent;

    host= text;
    user= row[_columns._text];
  }
  else
  {
    user= text;
    host.clear();
  }
}

void MGUserBrowserList::set_user_list(MYX_USER_NAMES *users)
{  
  _user_list= users;
  refresh_list(_entry->get_text());
}

/**
 * @brief checks whether a user with given name exists
 * 
 * @param user name to check
 * @return true if user with such name exists
 */
bool MGUserBrowserList::exists_user(const Glib::ustring &user)
{
  return find_user(user) != _store->children().end();
}

Gtk::TreeIter MGUserBrowserList::find_user(const Glib::ustring &user)
{
  for (Gtk::TreeIter it= _store->children().begin();
       it != _store->children().end(); ++it)
  {
    Gtk::TreeModel::Row row= *it;

    if (row[_columns._text] == user)
    {
      return it;
    }
  }
  return _store->children().end();
}

bool MGUserBrowserList::select_user(const Glib::ustring &user, 
                                    const Glib::ustring &host,
                                    bool expand)
{
  Gtk::TreeIter iter;
  bool found= false;
  Glib::ustring filter= _entry->get_text();

  //if (myx_match_string(user.c_str(), filter.c_str(), 0)==0)
  if (user.find(filter)!=Glib::ustring::npos)
  {
    _entry->set_text("");
  }

  //for (Gtk::TreeIter it= _store->children().begin();
  //     it != _store->children().end(); ++it)
  //{
    //Gtk::TreeModel::Row row= *it;

    //if (row[_columns._text] == user)
    //{
    do
    {
      Gtk::TreeIter it= find_user(user);

      if(it == _store->children().end())
        return false;   // did not find user

      Gtk::TreeModel::Row row= *it;
      Gtk::TreeIter parent= it;

      // look for host
      if (!host.empty())
      {
        for (Gtk::TreeIter jter= row.children().begin(); 
             jter != row.children().end(); ++jter)
        {
          Gtk::TreeModel::Row row2= *jter;
          Glib::ustring tmp;

          tmp= row2[_columns._text];

          if (!tmp.empty() && tmp == host)
          {
            iter= jter;
            found= true;
            break;
          }
        }
        if (!found)
        {
          iter= parent;
          found= true;
        }
      }
      else  // no host
      {
        iter= it;
        found= true;
      }
      break;
    }
    while(0);
  //}

  if (found)
  {
    if (expand)
      expand_node(iter);

    set_selection(iter);

    myx_tree_make_row_visible(_tree->gobj(), iter.gobj());
  }

  return found;
}


void MGUserBrowserList::populate_node(const Gtk::TreeIter &node)
{
  Gtk::TreeModel::Row prow= *node;
  std::list<Glib::ustring> list;
  Glib::RefPtr<Gdk::Pixbuf> icon;
  bool was_expanded= false;
  Gtk::TreeRowReference old_sel;

  icon= prow[_columns._icon];

  {
    if (_tree->row_expanded(Gtk::TreePath(node)))
      was_expanded= true;
    Gtk::TreeIter tmp;
    while ((tmp= prow->children().begin())!= prow->children().end())
    {
      if (_tree->get_selection()->is_selected(tmp))
        old_sel= Gtk::TreeRowReference(_store, _store->get_path(tmp));
      _store->erase(tmp);
    }
  }

  if (_populate_slot(this, node, list))
  {
    for (std::list<Glib::ustring>::const_iterator i= list.begin(); 
         i != list.end(); ++i)
    {
      Gtk::TreeIter iter= _store->append(prow->children());
      Gtk::TreeModel::Row row= *iter;

      row[_columns._icon]= icon;
      row[_columns._text]= *i;
    }
    prow[_columns._populated]= true;

    if (was_expanded)
      expand_node(node);
    
    if (old_sel)
      set_selection(_store->get_iter(old_sel.get_path()));
  }
  else
  {
    prow[_columns._populated]= false;
  }
}


void MGUserBrowserList::set_populate_func(const PopulateSlot &pslot)
{
  _populate_slot= pslot;
}
