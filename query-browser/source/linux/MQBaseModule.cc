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

#include "MQBaseModule.h"

#include "MQBaseTab.h"

#include "MQQueryDispatcher.h"
#include "myqb.h"
#include "myg_gtkutils.h"

#include <gtkmm.h>

MQBaseModule::MQBaseModule(GtkVBox *vbox)
  : Gtk::VBox(vbox), _dispatcher(0)
{
}


bool MQBaseModule::schemata_fetch_tables(const Glib::ustring &catalog,const Glib::ustring&schema,MYX_SCHEMA_TABLES *&tables)
{
  tables= _dispatcher->get_tables(catalog,schema);
  return tables != 0;
}

bool MQBaseModule::schemata_fetch_sps(const Glib::ustring &catalog,const Glib::ustring&schema,MYX_SCHEMA_STORED_PROCEDURES *&sps)
{
  sps= _dispatcher->get_sps(catalog,schema);
  return sps != 0;
}


void MQBaseModule::set_dispatcher(MQQueryDispatcher *dispatcher)
{
  _dispatcher= dispatcher;
  _catalogs= Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> >(new MGPtrWrap<MYX_CATALOGS*>(_dispatcher->get_catalogs()));
}


void MQBaseModule::refresh_catalogs()
{
  _catalogs= Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> >(new MGPtrWrap<MYX_CATALOGS*>(_dispatcher->get_catalogs()));
  
  catalogs_refreshed();
}


MQBaseTab *MQBaseModule::get_tab(int tab)
{
  if (tab < 0)
    return static_cast<MQBaseTab*>(_note->get_nth_page(_note->get_current_page()));
  else
    return static_cast<MQBaseTab*>(_note->get_nth_page(tab));
}


void MQBaseModule::add_tab(MQBaseTab *tab)
{
  Gtk::EventBox *evbox= Gtk::manage(new Gtk::EventBox);
  evbox->add(*tab->get_tab_widget());

  // popup menu when right click on the tab
  evbox->signal_button_press_event().connect(sigc::bind<MQBaseTab*>(sigc::mem_fun(*this,
                                                        &MQBaseModule::tab_clicked),tab));
  evbox->show_all();
  evbox->show();
  tab->show();
  _note->append_page(*tab, *evbox);
  
  _note->set_current_page(_note->get_n_pages()-1);

  _note->set_show_tabs(_note->get_n_pages()>1);
  
}


bool MQBaseModule::close_tab(MQBaseTab *tab)
{
  if (_note->get_n_pages()>1 && tab->tab_will_close())
  {
    tab->reference();

    _note->remove_page(*tab);

    tab->tab_did_close();

    tab->unreference();

    if (_note->get_n_pages()==1)
      _note->set_show_tabs(false);
    
    return true;
  }
  return false;
}


bool MQBaseModule::close_other_tabs(MQBaseTab *tab)
{
  bool flag= true;
  for (int i= _note->get_n_pages()-1; i >= 0; i--)
  {
    if (i == _note->get_current_page())
      continue;
    
    MQBaseTab *wtab= get_tab(i);
    if (wtab->tab_will_close())
    {
      wtab->reference();
      _note->remove_page(*wtab);

      wtab->tab_did_close();
      wtab->unreference();
    }
    else
      flag= false;
  }
  if (_note->get_n_pages()==1)
    _note->set_show_tabs(false);

  return flag;
}


bool MQBaseModule::tab_clicked(GdkEventButton *ev,MQBaseTab *tab)
{
  if (ev->button == 3)
  {
    // set current page to the tab, so that when a tab related item is activated
    // the action will apply to this tab.
    _note->set_current_page(_note->page_num(*tab));
    get_tab_menu()->popup(ev->button, ev->time);
  }
  else
    return false;
  return true;
}


void MQBaseModule::cancel_schema_selection(GdkEventAny *a)
{
  Gtk::Main::instance()->quit();
}

void MQBaseModule::ok_schema_selection(Gtk::TreeView *tree)
{
  Gtk::TreeIter iter= tree->get_selection()->get_selected();
  Gtk::TreeRow row;
  if (iter)
  {
    row= *iter;
    MYX_SCHEMA *schema= row[_schema_columns.schema];

    Gtk::Main::instance()->quit();

    _dispatcher->select_schema(schema->catalog_name, schema->schema_name);
  }
}


void MQBaseModule::select_schema()
{
  Gtk::Window window(Gtk::WINDOW_TOPLEVEL);
  Gtk::VBox vbox(false, 8);
  Gtk::HButtonBox bbox(Gtk::BUTTONBOX_END, 8);
  Gtk::Button okb(Gtk::Stock::OK);
  Gtk::Button cancelb(Gtk::Stock::CANCEL);
  Gtk::Label label(_("Select the new default schema:"));
  Gtk::ScrolledWindow swin;
  Gtk::TreeView tree;
  Glib::RefPtr<Gtk::ListStore> store;
  Glib::RefPtr<Gdk::Pixbuf> icon= PIXCACHE->load("16x16_Database.png");
  MYX_CATALOGS *cats;

  window.set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
  window.set_modal(true);
  window.set_title(_("Select Default Schema"));
  window.set_transient_for(*_mainw);
  window.add(vbox);
  vbox.set_border_width(12);
  
  vbox.pack_start(label, false, false);
  
  store= Gtk::ListStore::create(_schema_columns);
  tree.set_headers_visible(false);
  tree.append_column("", _schema_columns.icon);
  tree.append_column("", _schema_columns.text);
  tree.set_model(store);
  tree.set_size_request(-1, 200);
  vbox.pack_start(swin, true, true);
  swin.add(tree);
  swin.set_shadow_type(Gtk::SHADOW_IN);
  swin.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  
  cats= _catalogs->ptr();
  for (unsigned int i= 0; i < cats->catalogs_num; i++)
  {
    MYX_CATALOG *cat= cats->catalogs+i;
    Gtk::TreeIter iter;
    Gtk::TreeRow row;
    
    for (unsigned int j= 0; j < cat->schemata_num; j++)
    {
      MYX_SCHEMA *schema= cat->schemata+j;
      
      iter= store->append();
      row= *iter;
      row[_schema_columns.icon]= icon;
      row[_schema_columns.text]= schema->schema_name;
      row[_schema_columns.schema]= schema;
    }
  }

  vbox.pack_start(bbox, false, false);
  bbox.add(cancelb);
  bbox.add(okb);

  cancelb.signal_clicked().connect(sigc::bind<GdkEventAny*>(sigc::mem_fun(*this,&MQBaseModule::cancel_schema_selection),0));
  okb.signal_clicked().connect(sigc::bind<Gtk::TreeView*>(sigc::mem_fun(*this,&MQBaseModule::ok_schema_selection),&tree));
  window.signal_delete_event().connect_notify(sigc::mem_fun(*this,&MQBaseModule::cancel_schema_selection));
  
  window.show_all();
  
  window.show();
  
  Gtk::Main::instance()->run();
}

