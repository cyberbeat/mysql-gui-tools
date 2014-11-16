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

#include "myqb.h"

#include "MQBookmarks.h"

#include "myg_gtkutils.h"
#include "MGGladeXML.h"
#include "MQNewBookmarkDialog.h"
#include "MQBookmarkBrowser.h"

#include "myg_utils.h"


MQBookmarks::MQBookmarks(MGGladeXML *add_xml)
  : _addxml(add_xml), _root_group(0), _add_dialog(0), _browser(0)
{
  load();

  _browser= new MQBookmarkBrowser(this);
  _browser->refresh();
}


MQBookmarks::~MQBookmarks()
{
  delete _addxml;
  delete _root_group;
  delete _browser;
}


MQBookmarks::BookmarkGroup *MQBookmarks::load_group(MYX_BOOKMARK_GROUP *bkms)
{
  BookmarkGroup *group= new BookmarkGroup;
  std::vector<Bookmark*> tmp;

  group->name= bkms->caption?:"";
  
  tmp.resize(bkms->bookmark_groups_num+bkms->bookmarks_num);
  
  //XXX rewrite this using sort
  
  for (unsigned int i= 0; i < bkms->bookmark_groups_num; i++)
    tmp[bkms->bookmark_groups[i].pos]= load_group(bkms->bookmark_groups+i);

  for (unsigned int i= 0; i < bkms->bookmarks_num; i++)
    tmp[bkms->bookmarks[i].pos]= load_item(bkms->bookmarks+i);

  for (unsigned int i= 0; i < tmp.size(); i++)
    group->items.push_back(tmp[i]);

  return group;
}


MQBookmarks::BookmarkItem *MQBookmarks::load_item(MYX_BOOKMARK *bmk)
{
  BookmarkItem *item= new BookmarkItem;
  
  item->name= bmk->caption?:"";
  item->catalog= bmk->catalog?:"";
  item->schema= bmk->schema?:"";
  item->query= bmk->sql?:"";
  //bmk->query_type
  
  item->access_count= bmk->access_count;
  item->ctime= atol(bmk->date_created?:"");
  item->mtime= atol(bmk->date_modified?:"");
  item->atime= atol(bmk->date_last_access?:"");

  return item;
}


void MQBookmarks::load()
{
  MYX_LIB_ERROR err;
  MYX_BOOKMARKS *bmks= myx_bookmarks_load(prefs.build_path_to("query-browser/bookmarks.xml").c_str(), &err);

  if (bmks)
  {
    if (bmks->bookmark_groups_num == 1)
    {
      _root_group= load_group(bmks->bookmark_groups);
    }
    else
    {
      _root_group= new BookmarkGroup;
      _root_group->name= _("Bookmarks");
      for (unsigned int i= 0; i < bmks->bookmark_groups_num; i++)
      {
        _root_group->items.push_back(load_group(bmks->bookmark_groups+i));
      }
    }
    myx_bookmarks_free(bmks);
  }
  else
  {
    _root_group= new BookmarkGroup;
    _root_group->name= _("Bookmarks");
  }
}


void MQBookmarks::store_group(BookmarkGroup *bgroup, MYX_BOOKMARK_GROUP *group)
{
  unsigned int g= 0, b= 0;
  for (std::list<Bookmark*>::const_iterator iter= bgroup->items.begin();
       iter != bgroup->items.end(); ++iter)
  {
    if ((*iter)->is_group())
      g++;
    else
      b++;
  }

  group->caption= g_strdup(bgroup->name.c_str());

  group->bookmark_groups_num= g;
  group->bookmark_groups= (MYX_BOOKMARK_GROUP*)g_malloc0(sizeof(MYX_BOOKMARK_GROUP)*g);

  group->bookmarks_num= b;
  group->bookmarks= (MYX_BOOKMARK*)g_malloc0(sizeof(MYX_BOOKMARK)*b);

  b= 0;
  g= 0;
  unsigned int i= 0;
  for (std::list<Bookmark*>::const_iterator iter= bgroup->items.begin();
       iter != bgroup->items.end(); ++iter)
  {
    if ((*iter)->is_group())
    {
      group->bookmark_groups[g].pos= i++;
      store_group(static_cast<BookmarkGroup*>(*iter), group->bookmark_groups + g++);
    }
    else
    {
      group->bookmarks[b].pos= i++;
      store_item(static_cast<BookmarkItem*>(*iter), group->bookmarks + b++);
    }
  }
}


void MQBookmarks::store_item(BookmarkItem *item, MYX_BOOKMARK *bmk)
{  
  bmk->caption= g_strdup(item->name.c_str());
  bmk->catalog= g_strdup(item->catalog.c_str());
  bmk->schema= g_strdup(item->schema.c_str());
  bmk->sql= g_strdup(item->query.c_str());
  //bmk->query_type
  
  bmk->access_count= item->access_count;
//  bmk->date_created= item->ctime;
//  bmk->date_modified= item->mtime;
//  bmk->date_last_access= item->atime;
}


void MQBookmarks::save()
{
  MYX_BOOKMARKS *groups= (MYX_BOOKMARKS*)g_malloc0(sizeof(MYX_BOOKMARKS));
  MYX_LIB_ERROR err;
  
  groups->bookmark_groups_num= 1;
  groups->bookmark_groups= (MYX_BOOKMARK_GROUP*)g_malloc0(sizeof(MYX_BOOKMARK_GROUP)*groups->bookmark_groups_num);

  /*
  unsigned int i= 0;
  for (std::list<Bookmark*>::const_iterator iter= _root_group->items.begin();
       iter != _root_group->items.end(); ++iter)
  {
    g_assert((*iter)->is_group());

    groups->bookmark_groups[i].pos= i;
    store_group(static_cast<BookmarkGroup*>(*iter), groups->bookmark_groups + ++i);
  }
   */
  store_group(_root_group, groups->bookmark_groups);

  err= myx_bookmarks_store(prefs.build_path_to("query-browser/bookmarks.xml").c_str(),
                           groups);

  myx_bookmarks_free(groups);
  
  if (err != MYX_NO_ERROR)
    myg_show_xlib_error(*static_cast<Gtk::Window*>(_addxml->get_widget("add_bookmark_dialog")),
                        _("Error saving bookmarks."), err);
}


std::list<Glib::ustring> MQBookmarks::get_bookmark_folder_list()
{
  std::list<Glib::ustring> folders;
  std::list<Bookmark*> l;

  if (_root_group->items.size()==1)
    l= static_cast<BookmarkGroup*>(*_root_group->items.begin())->items;
  else
    l= _root_group->items;

  for (std::list<Bookmark*>::const_iterator iter= l.begin(); iter!=l.end(); ++iter)
  {
    if ((*iter)->is_group())
      folders.push_back((*iter)->name);
  }

  return folders;
}


void MQBookmarks::interactive_add(const Glib::ustring &catalog,
                                  const Glib::ustring &schema,
                                  const Glib::ustring &query)
{
  if (!_add_dialog)
  {
    _add_dialog= 0;
    _addxml->get_widget_derived2("add_bookmark_dialog", _add_dialog);
  }
  
  _add_dialog->set_bookmarks(_root_group);

  if (_add_dialog->run(catalog, schema, query))
  {
    save();

    _signal_changed.emit();
    _browser->refresh();
  }
}


void MQBookmarks::add_bookmark_group_to_menu(Gtk::Menu *menu, BookmarkGroup *group,
                                             const MQBookmarks::OpenBookmarkSlot &slot)
{
  Gtk::MenuItem *item;

  for (std::list<Bookmark*>::const_iterator iter= group->items.begin();
       iter != group->items.end(); ++iter)
  {
    item= Gtk::manage(new Gtk::MenuItem((*iter)->name));
    menu->append(*item);

    if ((*iter)->is_group())
    {
      Gtk::Menu *submenu= Gtk::manage(new Gtk::Menu);
      item->set_submenu(*submenu);
      add_bookmark_group_to_menu(submenu, static_cast<BookmarkGroup*>(*iter), slot);
    }
    else
    {
      BookmarkItem *bookmark= static_cast<BookmarkItem*>(*iter);

      if (bookmark->catalog.empty())
        _tips.set_tip(*item, ufmt("Schema: %s\n%s",
                                  bookmark->schema.c_str(),
                                  bookmark->query.c_str()));
      else
        _tips.set_tip(*item, ufmt("Schema: %s.%s\n%s",
                                  bookmark->catalog.c_str(),
                                  bookmark->schema.c_str(),
                                  bookmark->query.c_str()));

      item->signal_activate().connect(sigc::bind<BookmarkItem*>(slot, static_cast<BookmarkItem*>(*iter)));
    }
    item->show();
  }
}


void MQBookmarks::update_bookmark_menu(Gtk::Menu *menu, int index,
                                       const MQBookmarks::OpenBookmarkSlot &slot)
{
  Gtk::Menu::MenuList items= menu->items();

  if (!items.empty())
  {
    Gtk::Menu::MenuList::iterator beg= items.begin();
    while (index-- > 0)
      ++beg;

    if (beg != items.end())
      items.erase(beg, items.end());
  }
  add_bookmark_group_to_menu(menu, _root_group, slot);
}


void MQBookmarks::bookmarks_changed()
{
  save();
  _signal_changed.emit();
}


MQBookmarks::BookmarkGroup *MQBookmarks::create_group(BookmarkGroup *group, const Glib::ustring &name)
{
  BookmarkGroup *ngroup= new BookmarkGroup();
  ngroup->name= name;

  group->items.push_back(ngroup);

  bookmarks_changed();
  
  return ngroup;
}


bool MQBookmarks::delete_item(Bookmark *bm, BookmarkGroup *group)
{
  if (group==0)
    group= _root_group;
  
  std::list<Bookmark*>::iterator iter= 
    std::find(group->items.begin(), group->items.end(), bm);
  
  if (iter != group->items.end())
  {
    group->items.erase(iter);
    delete bm;
    bookmarks_changed();
    return true;
  }
  else
  {
    for (std::list<Bookmark*>::iterator iter= group->items.begin();
         iter != group->items.end(); ++iter)
    {
      if ((*iter)->is_group())
        if (delete_item(bm, static_cast<BookmarkGroup*>(*iter)))
          return true;
    }
    return false;
  }
}
