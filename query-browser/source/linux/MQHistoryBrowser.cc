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
#include "MQHistoryBrowser.h"
#include "MQHistory.h"

#include "myg_utils.h"
#include "myg_gtkutils.h"


MQHistoryBrowser::MQHistoryBrowser(MQHistory *history)
  : MGBrowserList(false, ""), _history(history), _tooltip(_tree)
{
  Gtk::TreeViewColumn *column= new Gtk::TreeView::Column(_("Table Name"));
  column->pack_start(_columns.icon, false);
  column->pack_start(_columns.text);
  _tree->append_column(*Gtk::manage(column));

  _tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MQHistoryBrowser::history_select));
  _tree->signal_row_activated().connect(sigc::mem_fun(*this,&MQHistoryBrowser::history_activate));

  set_store(Gtk::TreeStore::create(_columns));

  _folder_icon= PIXCACHE->load("history_interval_14x14.png");
  _query_icon= PIXCACHE->load("query_14x14.png");
  
  Gtk::MenuItem *item;
  item= Gtk::manage(new Gtk::MenuItem(_("_Execute Query"), true));
  item->signal_activate().connect(sigc::mem_fun(*this,&MQHistoryBrowser::open_mi));
  _menu.append(*item);
  item= Gtk::manage(new Gtk::MenuItem(_("_Copy to Clipboard"), true));
  item->signal_activate().connect(sigc::mem_fun(*this,&MQHistoryBrowser::copy_query_mi));
  _menu.append(*item);
  item= Gtk::manage(new Gtk::SeparatorMenuItem());
  _menu.append(*item);
  item= Gtk::manage(myg_make_stock_image_item(Gtk::Stock::DELETE, _("_Delete")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MQHistoryBrowser::delete_mi));
  _menu.append(*item);

  _menu.show_all();
  set_popup_menu(&_menu);
  
  _tree->signal_key_press_event().connect(sigc::mem_fun(*this,&MQHistoryBrowser::key_pressed), false);
  
  _tooltip.signal_will_show().connect(sigc::mem_fun(*this,&MQHistoryBrowser::will_show_tip));
  
  // enable dnd
  std::list<Gtk::TargetEntry> targets;
  //targets.push_back(Gtk::TargetEntry("x-mysqlgui-query", 0, 0));
  targets.push_back(Gtk::TargetEntry("x-mysqlgui-query", Gtk::TargetFlags(0), 0));
  _tree->drag_source_set(targets, Gdk::ModifierType(GDK_BUTTON1_MASK),
                         Gdk::DragAction(GDK_ACTION_COPY));
  _tree->signal_drag_data_get().connect(sigc::mem_fun(*this,&MQHistoryBrowser::drag_data_get));
}


bool MQHistoryBrowser::will_show_tip(const Gtk::TreeModel::Path &path)
{
  Gtk::TreeIter iter= _tree->get_model()->get_iter(path);
  if (iter)
  {
    Gtk::TreeRow row= *iter;

    if (!row.parent())
      return false;
    else
    {
      std::string id= row[_columns.id];
      MYX_HISTORY_ENTRY *entry= _history->find_entry(id);
      if (entry)
      {
        char *ts= g_strdup(entry->date_last_access);
        char *tmp;
        tmp= strchr(ts, 'T');
        if (tmp) *tmp= ' ';
        tmp= strchr(ts, 'Z');
        if (tmp) *tmp= ' ';
        _tooltip.set_text(ufmt("%s\n"
                               "<b>Schema: </b>%s\n"
                               "<b>Last Used: </b>%s",
                               entry->sql, 
                               entry->schema,
                               ts));
        g_free(ts);
        return true;
      }
    }
  }
  
  return false;
}


bool MQHistoryBrowser::key_pressed(GdkEventKey *ev)
{
  if (ev->keyval == GDK_Delete)
  {
    delete_mi();
    return true;
  }
  return false;
}



void MQHistoryBrowser::drag_data_get(const Glib::RefPtr<Gdk::DragContext>& context, const Gtk::SelectionData& selection_data, guint info, guint time)
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    std::string id= row[_columns.id];
    MYX_HISTORY_ENTRY *entry= _history->find_entry(id);

    if (entry)
    {
      char *s;
      _history->mark_entry(entry);

      s= g_strdup_printf("C:%s\nS:%s\nQ:%s",
                         entry->catalog,
                         entry->schema,
                         entry->sql);
      gtk_selection_data_set(const_cast<GtkSelectionData*>(selection_data.gobj()), 
        const_cast<GtkSelectionData*>(selection_data.gobj())->target, 
        8, (const guchar*)s, strlen(s)+1);
      g_free(s);
    }
  }
}


void MQHistoryBrowser::history_activate(const Gtk::TreeModel::Path &path, Gtk::TreeViewColumn *column)
{
  Gtk::TreeIter iter= _store->get_iter(path);
  Gtk::TreeRow row= *iter;
  std::string index= row[_columns.id];

  if (row.parent())
    open_mi();
  else
  {
    if (_tree->row_expanded(_store->get_path(iter)))
      _tree->collapse_row(_store->get_path(iter));
    else
      _tree->expand_row(_store->get_path(iter), false);
  }
}


void MQHistoryBrowser::history_select()
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;

    if (!row.parent())
    {
      _menu.items()[0].set_sensitive(false);
      _menu.items()[1].set_sensitive(false);
    }
    else
    {
      _menu.items()[0].set_sensitive(true);
      _menu.items()[1].set_sensitive(true);
    }
  }
}


void MQHistoryBrowser::refresh_list(const Glib::ustring &filter)
{
  MYX_HISTORY_TREE *tree= _history->get_tree();
  MYX_HISTORY_INTERVAL_TYPE order[]= {
    MYX_HIT_TODAY,
      MYX_HIT_YESTERDAY,
      MYX_HIT_MONDAY,
      MYX_HIT_TUESDAY,
      MYX_HIT_WEDNESDAY,
      MYX_HIT_THURSDAY,
      MYX_HIT_FRIDAY,
      MYX_HIT_SATURDAY,
      MYX_HIT_SUNDAY,
      MYX_HIT_LAST_WEEK,
      MYX_HIT_BEFORE_LAST_WEEK
  };

  _store->clear();

  for (unsigned int i= 0; i < sizeof(order)/sizeof(MYX_HISTORY_INTERVAL_TYPE); i++)
  {
    MYX_HISTORY_INTERVAL *interv= 0;

    for (unsigned int j= 0; j < tree->history_intervals_num; j++)
      if (tree->history_intervals[j].interval_type < sizeof(order)/sizeof(MYX_HISTORY_INTERVAL_TYPE)
          && tree->history_intervals[j].interval_type == (unsigned int)order[i])
      {
        interv= tree->history_intervals+j;
        break;
      }
    if (!interv)
      continue;

    Gtk::TreeIter iter= _store->append();
    Gtk::TreeRow row= *iter;
    bool has_stuff= false;

    row[_columns.icon]= _folder_icon;
    switch (interv->interval_type)
    {
    case MYX_HIT_TODAY: row[_columns.text]= _("Today"); break;
    case MYX_HIT_YESTERDAY: row[_columns.text]= _("Yesterday"); break;
    case MYX_HIT_MONDAY: row[_columns.text]= _("Monday"); break;
    case MYX_HIT_TUESDAY: row[_columns.text]= _("Tuesday"); break;
    case MYX_HIT_WEDNESDAY: row[_columns.text]= _("Wednesday"); break;
    case MYX_HIT_THURSDAY: row[_columns.text]= _("Thursday"); break;
    case MYX_HIT_FRIDAY: row[_columns.text]= _("Friday"); break;
    case MYX_HIT_SATURDAY: row[_columns.text]= _("Saturday"); break;
    case MYX_HIT_SUNDAY: row[_columns.text]= _("Sunday"); break;
    case MYX_HIT_LAST_WEEK: row[_columns.text]= _("Last Week"); break;
    case MYX_HIT_BEFORE_LAST_WEEK: row[_columns.text]= _("Before Last Week"); break;
    }

    for (unsigned int c= 0; c < interv->catalogs_num; c++)
    {
      MYX_HISTORY_CATALOG *cat= interv->catalogs+c;
      for (unsigned int s= 0; s < cat->schemata_num; s++)
      {
        MYX_HISTORY_SCHEMA *sche= cat->schemata+s;
        for (unsigned int e= 0; e < sche->entries_num; e++)
        {
          MYX_HISTORY_ENTRY *item= sche->entries[e];

          if (!filter.empty() && Glib::ustring(item->sql).find(filter)==Glib::ustring::npos)
            continue;

          Gtk::TreeIter eiter= _store->insert(row.children().begin());
          Gtk::TreeRow erow= *eiter;

          erow[_columns.icon]= _query_icon;
          erow[_columns.text]= Glib::ustring(item->sql);//.substr(0, 32)+"...";
          erow[_columns.id]= _history->get_id_for(item);
          has_stuff= true;
        }
      }
    }
    
    _tree->expand_row(_store->get_path(iter), true);
  }
  
  myx_history_free_tree(tree);
}


void MQHistoryBrowser::delete_mi()
{

  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    if (row.parent()) // a query entry
    {
      std::string id= row[_columns.id];
      MYX_HISTORY_ENTRY *entry= _history->find_entry(id);
      _history->remove_entry(entry);
      refresh();
    }
    else  // date (parent) entry
    {
      while(row.children().size() > 0)
      {
        Gtk::TreeRow nextrow= *row.children().begin();
        std::string id= nextrow[_columns.id];
        MYX_HISTORY_ENTRY *entry= _history->find_entry(id);
        _history->remove_entry(entry);
        refresh();
      }
    }
  }
}


void MQHistoryBrowser::open_mi()
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    std::string id= row[_columns.id];

    _signal_activate.emit(id);
  }
}


void MQHistoryBrowser::copy_query_mi()
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    std::string id= row[_columns.id];
    MYX_HISTORY_ENTRY *entry= _history->find_entry(id);

    if (entry)
      Gtk::Clipboard::get()->set_text(entry->sql);
  }
}
