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
#include "MQBookmarkBrowser.h"
#include "MQBookmarks.h"

#include <sys/time.h>

#include "myg_gtkutils.h"
#include "myg_utils.h"

MQBookmarkBrowser::MQBookmarkBrowser(MQBookmarks *bookmarks)
  : MGBrowserList(false, ""), _bookmarks(bookmarks), _tooltip(_tree)
{
  Gtk::TreeViewColumn *column= new Gtk::TreeView::Column(_("Table Name"));
  column->pack_start(_columns.icon, false);
  column->pack_start(_columns.text);
  std::vector<Gtk::CellRenderer*> rends= column->get_cell_renderers();
  Gtk::CellRendererText *cell= dynamic_cast<Gtk::CellRendererText*>(rends[1]);
  column->add_attribute(cell->property_foreground_set(), _columns.disabled);
  cell->property_foreground()= "#aaaaaa";
  _tree->append_column(*Gtk::manage(column));
  
  _tree->set_reorderable();
  
  _tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MQBookmarkBrowser::bookmark_select));
  _tree->signal_row_activated().connect(sigc::mem_fun(*this,&MQBookmarkBrowser::bookmark_activate));
  _tree->signal_key_press_event().connect(sigc::mem_fun(*this,&MQBookmarkBrowser::key_pressed));
//  _tree->signal_motion_notify_event().connect(sigc::mem_fun(*this,&MQBookmarkBrowser::tree_motion_notify_event), false);
//  _tree->add_events(Gdk::POINTER_MOTION_HINT_MASK);

  set_store(Gtk::TreeStore::create(_columns));

  _folder_icon= PIXCACHE->load("folder_open_14x14.png");
  _query_icon= PIXCACHE->load("bookmark_14x14.png");

  
  Gtk::MenuItem *item;
  item= Gtk::manage(new Gtk::MenuItem(_("_Open Bookmark"), true));
  item->signal_activate().connect(sigc::mem_fun(*this,&MQBookmarkBrowser::open_mi));
  _menu.append(*item);
  item= Gtk::manage(new Gtk::MenuItem(_("_Copy to Clipboard"), true));
  item->signal_activate().connect(sigc::mem_fun(*this,&MQBookmarkBrowser::copy_query_mi));
  _menu.append(*item);
  item= Gtk::manage(new Gtk::SeparatorMenuItem());
  _menu.append(*item);
  item= Gtk::manage(new Gtk::MenuItem(_("_New Folder..."), true));
  item->set_name("new_folder_mi");
  item->signal_activate().connect(sigc::mem_fun(*this,&MQBookmarkBrowser::new_folder_mi));
  _menu.append(*item);
  item= Gtk::manage(new Gtk::MenuItem(_("_Rename..."), true));
  item->signal_activate().connect(sigc::mem_fun(*this,&MQBookmarkBrowser::rename_mi));
  _menu.append(*item);
  item= Gtk::manage(new Gtk::SeparatorMenuItem());
  _menu.append(*item);
  item= Gtk::manage(myg_make_stock_image_item(Gtk::Stock::DELETE, _("_Delete")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MQBookmarkBrowser::delete_mi));
  _menu.append(*item);
  _menu.show_all();

  // enable dnd
  std::list<Gtk::TargetEntry> targets;
  //targets.push_back(Gtk::TargetEntry("x-mysqlgui-query", 0, 0));
  targets.push_back(Gtk::TargetEntry("x-mysqlgui-query", Gtk::TargetFlags(0), 0));
  _tree->drag_source_set(targets, Gdk::ModifierType(GDK_BUTTON1_MASK),
                         Gdk::DragAction(GDK_ACTION_COPY));
  _tree->signal_drag_data_get().connect(sigc::mem_fun(*this, &MQBookmarkBrowser::drag_data_get));
  set_popup_menu(&_menu);

  bookmark_select();
  
  _tooltip.signal_will_show().connect(sigc::mem_fun(*this,&MQBookmarkBrowser::tooltip_get));
}


bool MQBookmarkBrowser::tooltip_get(const Gtk::TreeModel::Path &path)
{
  Gtk::TreeIter iter= _tree->get_model()->get_iter(path);
  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    MQBookmarks::Bookmark *bitem= row[_columns.object];

    if (!bitem->is_group())
    {
      MQBookmarks::BookmarkItem *item= (MQBookmarks::BookmarkItem*)bitem;

      _tooltip.set_text(ufmt("<b>%s</b>\n"
                             "%s\n"
                             "<b>Schema: </b>%s",
                             item->name.c_str(),
                             item->query.c_str(),
                             item->schema.c_str()));
      return true;
    }
  }
  
  return false;
}


bool MQBookmarkBrowser::tree_motion_notify_event(GdkEventMotion* event)
{
  Gtk::TreePath path;
  Gtk::TreeViewColumn *column;
  int xx, yy;
  
  if (_tree->get_path_at_pos((int)event->x, (int)event->y, path, column, xx, yy))
  {
    Gtk::TreeRow row= *_store->get_iter(path);
    MQBookmarks::Bookmark *item= row[_columns.object];
    Glib::ustring text;
    
    if (item->is_group())
    {
      text= "";
    }
    else
    {
      MQBookmarks::BookmarkItem *bk= static_cast<MQBookmarks::BookmarkItem*>(item);

      text= "Query: "+bk->query;
    }
  }
  
  return false;
}


bool MQBookmarkBrowser::key_pressed(GdkEventKey *ev)
{
  if (ev->keyval == GDK_Delete)
  {
    Gtk::TreeIter iter= _tree->get_selection()->get_selected();
    if (iter)
    {    
      Gtk::TreeRow row= *iter;
      MQBookmarks::Bookmark *item= row[_columns.object];
      
      if (_bookmarks->delete_item(item))
        _store->erase(iter);
    }
    return true;
  }
  return false;
}


void MQBookmarkBrowser::drag_data_get(const Glib::RefPtr<Gdk::DragContext>& context, 
  const Gtk::SelectionData& selection_data, guint info, guint time)
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    MQBookmarks::Bookmark *bm= row[_columns.object];
    
    if (!bm->is_group())
    {
      MQBookmarks::BookmarkItem *entry= static_cast<MQBookmarks::BookmarkItem*>(bm);
      char *s;

      entry->touch();

      s= g_strdup_printf("C:%s\nS:%s\nQ:%s",
                         entry->catalog.c_str(),
                         entry->schema.c_str(),
                         entry->query.c_str());
      gtk_selection_data_set(const_cast<GtkSelectionData*>(selection_data.gobj()), 
        const_cast<GtkSelectionData*>(selection_data.gobj())->target, 8, 
        (const guchar*)s, strlen(s)+1);
      g_free(s);
      
      static_cast<MQBookmarks::BookmarkItem*>(bm)->touch();
    }
  }
}


int MQBookmarkBrowser::add_group(const Gtk::TreeModel::Children &children,
                                  MQBookmarks::BookmarkGroup *group,
                                  const Glib::ustring &filter)
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  int mcount= 0;
  
  for (std::list<MQBookmarks::Bookmark*>::const_iterator it= group->items.begin();
       it != group->items.end(); ++it)
  {
    if ((*it)->is_group())
    {
      int count;
      
      iter= _store->append(children);
      row= *iter;
      row[_columns.text]= (*it)->name;
      row[_columns.icon]= _folder_icon;
      row[_columns.object]= *it;
      row[_columns.disabled]= false;
      count= add_group(row.children(), static_cast<MQBookmarks::BookmarkGroup*>(*it),
                       filter);
      if (!filter.empty() && count > 0)
        _tree->expand_row(_store->get_path(iter), true);
      mcount += count;
    }
    else
    {
      if (filter.empty() || (*it)->name.find(filter)!=Glib::ustring::npos)
      {
        iter= _store->append(children);
        row= *iter;
        row[_columns.text]= (*it)->name;
        row[_columns.icon]= _query_icon;
        row[_columns.object]= *it;
        row[_columns.disabled]= false;
        mcount++;
      }
    }
  }
  return mcount;
}


void MQBookmarkBrowser::refresh_list(const Glib::ustring &filter)
{
  _store->clear();

  if (_bookmarks->get_bookmarks())
    add_group(_store->children(), _bookmarks->get_bookmarks(), filter);
}


bool MQBookmarkBrowser::set_activatable(const Gtk::TreeIter &iter, 
                                        const Glib::ustring &catalog, const Glib::ustring &schema)
{
  Gtk::TreeRow row= *iter;
  MQBookmarks::Bookmark *bm= row[_columns.object];
  
  if (!bm->is_group())
  {
    MQBookmarks::BookmarkItem *entry= static_cast<MQBookmarks::BookmarkItem*>(bm);

    if (schema.empty() || entry->schema.empty())
      row[_columns.disabled]= false;
    else if ((!catalog.empty() && entry->catalog != catalog) || entry->schema != schema)
      row[_columns.disabled]= true;
    else
      row[_columns.disabled]= false;
  }
  return false;
}


void MQBookmarkBrowser::refresh_activatable(const Glib::ustring &catalog, const Glib::ustring &schema)
{
  _store->foreach_iter(sigc::bind(sigc::mem_fun(*this,&MQBookmarkBrowser::set_activatable), catalog, schema));
}


void MQBookmarkBrowser::bookmark_select()
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    MQBookmarks::Bookmark *bm= row[_columns.object];

    for (unsigned int i=0; i < _menu.items().size(); i++)
      _menu.items()[i].set_sensitive(true);
    
    if (bm->is_group())
    {
      _menu.items()[0].set_sensitive(false);
      _menu.items()[1].set_sensitive(false);
    }
  }
  else
  {
    for (unsigned int i=0; i < _menu.items().size(); i++)
    {
      if (_menu.items()[i].get_name() != "new_folder_mi")
        _menu.items()[i].set_sensitive(false);
      else
        _menu.items()[i].set_sensitive(true);
    }
  }
}


void MQBookmarkBrowser::bookmark_activate(const Gtk::TreeModel::Path &path, Gtk::TreeViewColumn *column)
{
  Gtk::TreeIter iter= _store->get_iter(path);
  Gtk::TreeRow row= *iter;
  MQBookmarks::Bookmark *bookmark= row[_columns.object];

  if (!bookmark->is_group())
  {
    static_cast<MQBookmarks::BookmarkItem*>(bookmark)->touch();
    _signal_activate.emit(static_cast<MQBookmarks::BookmarkItem*>(bookmark));
  }
  else
  {
    if (_tree->row_expanded(_store->get_path(iter)))
      _tree->collapse_row(_store->get_path(iter));
    else
      _tree->expand_row(_store->get_path(iter), false);
  }
}


void MQBookmarkBrowser::commit_reordered()
{
  //XXX
}


void MQBookmarkBrowser::open_mi()
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    MQBookmarks::Bookmark *item= row[_columns.object];

    if (!item->is_group())
      _signal_activate.emit(static_cast<MQBookmarks::BookmarkItem*>(item));
  }
}


void MQBookmarkBrowser::new_folder_mi()
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  Gtk::TreeRow row;
  MQBookmarks::Bookmark *parent= 0;

  if (iter)
  {
    row = *iter;
    parent= row[_columns.object];
  }
  else
    parent= _bookmarks->get_bookmarks();

  if (!parent->is_group())
  {
    iter= row.parent();
    if (iter)
    {
      row= *iter;
      parent= row[_columns.object];
    }
    else
      parent= _bookmarks->get_bookmarks();
  }

  Glib::ustring name;
  if (!myg_ask_string(*static_cast<Gtk::Window*>(get_toplevel()), _("Create Group"), _("Type a name for the new bookmark group."), name) || name.empty())
    return;

  _bookmarks->create_group(static_cast<MQBookmarks::BookmarkGroup*>(parent), name);
  refresh();
}


void MQBookmarkBrowser::delete_mi()
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    MQBookmarks::Bookmark *item= row[_columns.object];

    if (_bookmarks->delete_item(item))
      _store->erase(iter);
  }
}


void MQBookmarkBrowser::rename_mi()
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    MQBookmarks::Bookmark *item= row[_columns.object];
    
    Glib::ustring name;
    if (!myg_ask_string(*static_cast<Gtk::Window*>(get_toplevel()), 
                        item->is_group() ? _("Rename Group") : _("Rename Bookmark"),
                        item->is_group() ? _("Type a new name for the group.") : _("Type a new name for the bookmark."),
                        name) || name.empty())
      return;
    
    item->name= name;
    _bookmarks->bookmarks_changed();
  }
}


void MQBookmarkBrowser::copy_query_mi()
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    MQBookmarks::Bookmark *item= row[_columns.object];

    if (item && !item->is_group())
    {
      Gtk::Clipboard::get()->set_text(static_cast<MQBookmarks::BookmarkItem*>(item)->query);
    }
  }
}
