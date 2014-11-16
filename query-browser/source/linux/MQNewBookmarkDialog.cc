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

#include "MQNewBookmarkDialog.h"
#include "MGGladeXML.h"
#include "myg_gtkutils.h"

MQNewBookmarkDialog::MQNewBookmarkDialog(GtkWindow *win, MGGladeXML *xml)
  : Gtk::Window(win), _xml(xml), _details_shown(false)
{
  _xml->get_button("add_button")->signal_clicked().connect(sigc::mem_fun(*this,&MQNewBookmarkDialog::add_clicked));
  _xml->get_button("cancel_button")->signal_clicked().connect(sigc::mem_fun(*this,&MQNewBookmarkDialog::cancel_clicked));
  _xml->get_entry("name_entry")->signal_changed().connect(sigc::mem_fun(*this,&MQNewBookmarkDialog::name_changed));
  ((Gtk::Entry*)_xml->get_combo_entry("folder_combo")->get_child())->signal_changed().connect(sigc::mem_fun(*this,&MQNewBookmarkDialog::name_changed));
  _xml->get_button("expand_button")->signal_clicked().connect(sigc::mem_fun(*this,&MQNewBookmarkDialog::toggle_detail));
  _xml->get_button("new_folder_button")->signal_clicked().connect(sigc::mem_fun(*this,&MQNewBookmarkDialog::new_folder_clicked));

  Gtk::TreeView *tree= _xml->get_tree("folder_tree");

  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MQNewBookmarkDialog::folder_selected));

  tree->append_column("", _columns.icon);
  tree->append_column_editable("", _columns.name);
  
  
  std::vector<Gtk::CellRenderer*> crs= tree->get_column(1)->get_cell_renderers();
  Gtk::CellRendererText *textrend= (Gtk::CellRendererText*)crs[0];
  textrend->signal_edited().connect(sigc::mem_fun(*this,&MQNewBookmarkDialog::renamed_folder));
  
  _store= Gtk::TreeStore::create(_columns);
  _lstore= Gtk::ListStore::create(_columns);
  
  tree->set_model(_store);
  
  _xml->get_combo_entry("folder_combo")->set_model(_lstore);
//  _xml->get_combo_entry("folder_combo")->clear();
//  _xml->get_combo_entry("folder_combo")->pack_start(_columns.name);
  _xml->get_combo_entry("folder_combo")->set_text_column(_columns.name);

  toggle_detail();
  toggle_detail();
  
  folder_selected();
}



void MQNewBookmarkDialog::name_changed()
{
  bool ok= false;
  
  if (!_xml->get_entry("name_entry")->get_text().empty())
    ok= true;
  
  if (!((Gtk::Entry*)_xml->get_combo_entry("folder_combo")->get_child())->get_text().empty())
    ok= true;

  _xml->get_button("add_button")->set_sensitive(ok);
}


void MQNewBookmarkDialog::add_clicked()
{
  _added= true;

  Gtk::Main::instance()->quit();
}


void MQNewBookmarkDialog::cancel_clicked()
{
  _added= false;

  Gtk::Main::instance()->quit();
}


void MQNewBookmarkDialog::new_folder_clicked()
{
  Glib::RefPtr<Gtk::TreeSelection> selection= _xml->get_tree("folder_tree")->get_selection();
  Gtk::TreeIter piter= selection->get_selected();
  if (!piter)
    return;

  _needs_save= true;

  Gtk::TreeRow row= *piter;
  MQBookmarks::BookmarkGroup *group, *ngroup;
  Gtk::TreeIter iter;

  group= row[_columns.group];

  iter= _store->append(row.children());
  row= *iter;
    
  ngroup= new MQBookmarks::BookmarkGroup;
  ngroup->name= _("New Folder");
  
  group->items.push_back(ngroup);

  row[_columns.icon]= PIXCACHE->load("folder_16x16.png");
  row[_columns.name]= ngroup->name;
  row[_columns.group]= ngroup;

  _xml->get_tree("folder_tree")->expand_row(_store->get_path(piter), true);
  
  selection->select(iter);
}


void MQNewBookmarkDialog::add_group_to_tree(const Gtk::TreeModel::Children &children,
                                            MQBookmarks::BookmarkGroup *group)
{
  Gtk::TreeIter iter= _store->append(children);
  Gtk::TreeRow row= *iter;
  Gtk::TreeRow lrow;

  row[_columns.icon]= PIXCACHE->load("folder_16x16.png");
  row[_columns.name]= group->name;
  row[_columns.group]= group;

  iter= _lstore->append();
  lrow= *iter;
  lrow[_columns.name]= group->name;
  lrow[_columns.group]= group;

  for (std::list<MQBookmarks::Bookmark*>::const_iterator iter= group->items.begin();
       iter != group->items.end(); ++iter)
  {
    if ((*iter)->is_group())
      add_group_to_tree(row.children(), static_cast<MQBookmarks::BookmarkGroup*>(*iter));
  }
}



void MQNewBookmarkDialog::set_bookmarks(MQBookmarks::BookmarkGroup *bm)
{
  _bookmarks= bm;

  _store->clear();
  _lstore->clear();
  add_group_to_tree(_store->children(), _bookmarks);
  _xml->get_tree("folder_tree")->expand_all();
}


bool MQNewBookmarkDialog::run(const Glib::ustring &catalog,
                              const Glib::ustring &schema,
                              const Glib::ustring &query)
{
  _xml->get_entry("name_entry")->set_text("New Bookmark");
  _xml->get_entry("name_entry")->select_region(0, -1);

  _xml->get_combo_entry("folder_combo")->set_active(0);

  if (((Gtk::Entry*)_xml->get_combo_entry("folder_combo")->get_child())->get_text().empty())
    _xml->get_button("add_button")->set_sensitive(false);
  else
    _xml->get_button("add_button")->set_sensitive(true);

  _added= false;
  _needs_save= false;

  _xml->get_entry("name_entry")->grab_focus();  

  show();

  Gtk::Main::instance()->run();

  hide();
  
  if (_added)
  {
    Glib::ustring name= _xml->get_entry("name_entry")->get_text();
    Glib::ustring folder= ((Gtk::Entry*)_xml->get_combo_entry("folder_combo")->get_child())->get_text();
    MQBookmarks::BookmarkGroup *group;

    // find the folder group
    group= _bookmarks->find_group(folder);
    g_return_val_if_fail(group!=0, false);

    _needs_save= true;

    MQBookmarks::BookmarkItem *item= new MQBookmarks::BookmarkItem;
  
    item->name= name;

    item->query= query;
    item->catalog= catalog;
    item->schema= schema;
    item->access_count= 0;
    item->mtime= item->atime= item->ctime= time(NULL);

    group->items.push_back(item);
  }

  return _needs_save;
}


void MQNewBookmarkDialog::toggle_detail()
{
  _details_shown= !_details_shown;
  if (_details_shown)
  {
    _xml->get_widget("tree_scroll")->show();
    ((Gtk::Image*)_xml->get_widget("expand_image"))->set(Gtk::Stock::GO_UP,Gtk::ICON_SIZE_MENU);
    _xml->get_widget("new_folder_button")->show();
  }
  else
  {
    _xml->get_widget("tree_scroll")->hide();
    ((Gtk::Image*)_xml->get_widget("expand_image"))->set(Gtk::Stock::GO_DOWN,Gtk::ICON_SIZE_MENU);
    _xml->get_widget("new_folder_button")->hide();
  }
}


void MQNewBookmarkDialog::folder_selected()
{
  Glib::RefPtr<Gtk::TreeSelection> sel= _xml->get_tree("folder_tree")->get_selection();
  Gtk::TreeIter iter;
  
  iter= sel->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;

    MQBookmarks::BookmarkGroup *group= row[_columns.group];
    
    ((Gtk::Entry*)_xml->get_combo_entry("folder_combo")->get_child())->set_text(group->name);
    
    _xml->get_button("new_folder_button")->set_sensitive(true);
  }
  else
    _xml->get_button("new_folder_button")->set_sensitive(false);
}


void MQNewBookmarkDialog::renamed_folder(const Glib::ustring &path, const Glib::ustring &new_value)
{
  Gtk::TreeIter iter= _store->get_iter(path);
  Gtk::TreeRow row= *iter;

  _needs_save= true;

  row[_columns.name]= new_value;
  (*row[_columns.group]).name= new_value;

  folder_selected();
}

