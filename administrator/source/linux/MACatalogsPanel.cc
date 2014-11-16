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

   
#include "myadmin.h"

#include "MAdministrator.h"
#include "MDataInterface.h"
#include "MACatalogsPanel.h"

#include "MGTableEditor.h"
#include "MGSchemaBrowserList.h"

#include "myg_utils.h"
#include "myg_gtkutils.h"

#include <gtk/gtkdialog.h>
#include <gtkmm/messagedialog.h>
#include <gtkmm/textbuffer.h>
#include <gtkmm/cellrenderertext.h>

static void *execute_query(MYSQL *mysql, const char *query, MYX_LIB_ERROR *error);

class CellRendererProgress : public Gtk::CellRendererText
{
  public:
    CellRendererProgress()
      :
    Glib::ObjectBase(typeid(CellRendererProgress)),
    Gtk::CellRendererText (),
    _line_color(*this, "line_color", Gdk::Color()),
    _back_color(*this, "back_color", Gdk::Color()),
    _percent(*this, "percent", 0)
    {
    }

    virtual ~CellRendererProgress()
    {
    }
    
    Glib::PropertyProxy<Gdk::Color> property_back_color()
    {
      return _back_color.get_proxy();
    }
    
    Glib::PropertyProxy<Gdk::Color> property_line_color()
    {
      return _line_color.get_proxy();
    }
    
    Glib::PropertyProxy<int> property_percent()
    {
      return _percent.get_proxy();
    }
    
  protected:    
    void render_vfunc(const Glib::RefPtr<Gdk::Window>& window,
                                              Gtk::Widget& widget,
                                              const Gdk::Rectangle& bg_area,
                                              const Gdk::Rectangle& cell_area,
                                              const Gdk::Rectangle& rect2,
                                              Gtk::CellRendererState flags)
    {      
      if (!_gc)
        _gc= Gdk::GC::create(window);

      _gc->set_foreground(_back_color.get_value());
      window->draw_rectangle(_gc, true,
                             bg_area.get_x(),
                             bg_area.get_y(),
                             ((bg_area.get_width()-4)*_percent)/100, bg_area.get_height()-2);

      _gc->set_foreground(_line_color.get_value());
      window->draw_rectangle(_gc, false,
                             bg_area.get_x(),
                             bg_area.get_y(),
                             bg_area.get_width()-4, bg_area.get_height()-2);

      Gtk::CellRendererText::render_vfunc(window, widget, bg_area, cell_area,
                                          rect2, flags);
    }

  private:    
    Glib::Property<Gdk::Color> _line_color;
    Glib::Property<Gdk::Color> _back_color;
    Glib::Property<int> _percent;
    Glib::RefPtr<Gdk::GC> _gc;
};



//-----------------------------------------------------------------------------

MACatalogsPanel::MACatalogsPanel(MAdministrator *app, MDataInterface *data)
    : MAPanel(app, data), _tables(0), _entity_status(0),
      _details_shown(false)
{
}


MACatalogsPanel::~MACatalogsPanel()
{
  _schema_browser->unreference();

  if (_entity_status)
    myx_free_schema_entity_status(_entity_status);
  
  delete _maint_dlg_xml;
}


void MACatalogsPanel::setup_trees()
{
  Gtk::TreeView *tree;
  Gtk::TreeView::Column *column;
  Gdk::Color data_fg("#8c96d6"), data_bg("#a5b2e7");
  Gdk::Color index_fg("#bda2e7"), index_bg("#cebeef");

  _panel->get_colormap()->alloc_color(data_fg);
  _panel->get_colormap()->alloc_color(data_bg);
  _panel->get_colormap()->alloc_color(index_fg);
  _panel->get_colormap()->alloc_color(index_bg);

  // table listing
  _table_store= Gtk::ListStore::create(_table_columns);
  tree= get_tree("table_tree");
  tree->set_model(_table_store);
  tree->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);
  tree->signal_button_press_event().connect_notify(sigc::mem_fun(*this,&MACatalogsPanel::table_button_press));
  
  column= new Gtk::TreeView::Column(_("Table Name"));
  column->pack_start(_table_columns._icon, false);
  column->pack_start(_table_columns._name);
  tree->append_column(*Gtk::manage(column));
  tree->append_column(_("Type"), _table_columns._type);
  tree->append_column(_("Row Format"), _table_columns._format);
  tree->append_column(_("Rows"), _table_columns._rows);
  
  CellRendererProgress *prog;
  prog= new CellRendererProgress();
  column= new Gtk::TreeView::Column(_("Data Length"), *Gtk::manage(prog));
  tree->append_column(*Gtk::manage(column));
  column->add_attribute(prog->property_percent(), _table_columns._data_percent);
  column->add_attribute(prog->property_text(), _table_columns._length);
  prog->property_line_color()= data_bg;
  prog->property_back_color()= data_bg;

  prog= new CellRendererProgress();
  column= new Gtk::TreeView::Column(_("Index Length"), *Gtk::manage(prog));
  tree->append_column(*Gtk::manage(column));
  column->add_attribute(prog->property_percent(), _table_columns._index_percent);
  column->add_attribute(prog->property_text(), _table_columns._ilength);
  prog->property_line_color()= index_bg;
  prog->property_back_color()= index_bg;
  
  tree->append_column(_("Update Time"), _table_columns._time);
  for (int i= 0; i < 7; i++)
    tree->get_column(i)->set_resizable(true);
  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MACatalogsPanel::selected_table));

  tree->signal_row_activated().connect(sigc::mem_fun(*this,&MACatalogsPanel::table_dbl_clicked));

  // column listing
  _column_store= Gtk::TreeStore::create(_column_columns);
  tree= get_tree("column_tree");
  tree->set_model(_column_store);
  
  column= new Gtk::TreeView::Column(_("Column Name"));
  column->pack_start(_column_columns._icon, false);
  column->pack_start(_column_columns._name);
  tree->append_column(*Gtk::manage(column));
  tree->append_column(_("Type"), _column_columns._type);
  tree->append_column(_("Default"), _column_columns._default);
  tree->append_column(_("Primary"), _column_columns._key);
  tree->append_column(_("Null Allowed"), _column_columns._null);
  tree->append_column(_("Other"), _column_columns._extra);
  for (int i= 0; i < 6; i++)
    tree->get_column(i)->set_resizable(true);

  // table index listing
  _tindex_store= Gtk::TreeStore::create(_index_columns);
  tree= get_tree("tindex_tree");
  tree->set_model(_tindex_store);

  column= new Gtk::TreeView::Column(_("Name/Columns"));
  column->pack_start(_index_columns._icon, false);
  column->pack_start(_index_columns._name);
  tree->append_column(*Gtk::manage(column));
  tree->append_column(_("Index Type"), _index_columns._type);
  tree->append_column(_("Unique"), _index_columns._unique);
  tree->append_column(_("Not NULL"), _index_columns._null);
  tree->append_column(_("Seq. in Index"), _index_columns._seq);
  tree->append_column(_("Collation"), _index_columns._coll);
  for (int i= 0; i < 6; i++)
  {
    Gtk::TreeView::Column *col= tree->get_column(i);
    col->set_resizable(true);
    std::vector<Gtk::CellRenderer*> rends= col->get_cell_renderers();
    if (i == 0)
      col->add_attribute(static_cast<Gtk::CellRendererText*>(rends[1])->property_weight(),
                         _index_columns._bold);
    else
      col->add_attribute(static_cast<Gtk::CellRendererText*>(rends[0])->property_weight(),
                         _index_columns._bold);
  }

  // index listing
  _index_store= Gtk::TreeStore::create(_index_columns);
  tree= get_tree("index_tree");
  tree->set_model(_index_store);
  
  column= new Gtk::TreeView::Column(_("Table Name/Column"));
  column->pack_start(_index_columns._icon, false);
  column->pack_start(_index_columns._name);
  tree->append_column(*Gtk::manage(column));
  tree->append_column(_("Name"), _index_columns._key);
  tree->append_column(_("Index Type"), _index_columns._type);
  tree->append_column(_("Unique"), _index_columns._unique);
  tree->append_column(_("Null Allowed"), _index_columns._null);
  tree->append_column(_("Seq. in Index"), _index_columns._seq);
  tree->append_column(_("Collation"), _index_columns._coll);
  for (int i= 0; i < 7; i++)
  {
    Gtk::TreeView::Column *col= tree->get_column(i);
    col->set_resizable(true);

    std::vector<Gtk::CellRenderer*> rends= col->get_cell_renderers();
    if (i == 0)
      col->add_attribute(static_cast<Gtk::CellRendererText*>(rends[1])->property_weight(),
                         _index_columns._bold);
    else
      col->add_attribute(static_cast<Gtk::CellRendererText*>(rends[0])->property_weight(),
                         _index_columns._bold);
  }

  // view listing
  _view_store= Gtk::ListStore::create(_view_columns);
  tree= get_tree("view_tree");
  tree->set_model(_view_store);
  
  column= new Gtk::TreeView::Column(_("View Name"));
  column->pack_start(_view_columns._icon, false);
  column->pack_start(_view_columns._name);
  tree->append_column(*Gtk::manage(column));
  tree->append_column(_("Comment"), _view_columns._comment);
  tree->append_column(_("Updatable"), _view_columns._updatable);
  for (int i= 0; i < 3; i++)
    tree->get_column(i)->set_resizable(true);

  
  // user listing
  _user_store= Gtk::ListStore::create(_user_columns);
  tree= get_tree("user_tree");
  tree->set_model(_user_store);
  
  column= new Gtk::TreeView::Column(_("User"));
  column->pack_start(_user_columns._icon, false);
  column->pack_start(_user_columns._name);
  tree->append_column(*Gtk::manage(column));
  tree->append_column(_("Full Name"), _user_columns._fullname);
  tree->append_column(_("Privileges"), _user_columns._privileges);
  for (int i= 0; i < 3; i++)
    tree->get_column(i)->set_resizable(true);

  // stored procedures
  _procedure_store= Gtk::ListStore::create(_procedure_columns);
  tree= get_tree("procedure_tree");
  tree->set_model(_procedure_store);
  
  column= new Gtk::TreeView::Column(_("Routine Name"));
  column->pack_start(_procedure_columns._icon, false);
  column->pack_start(_procedure_columns._name);
  tree->append_column(*Gtk::manage(column));
  tree->append_column(_("Definer"), _procedure_columns._definer);
  tree->append_column(_("Create Time"), _procedure_columns._created);
  tree->append_column(_("Update Time"), _procedure_columns._modified);
  tree->append_column(_("Return Data Type"), _procedure_columns._return_datatype);
  tree->append_column(_("Comment"), _procedure_columns._comment);
  for (int i= 0; i < 6; i++)
    tree->get_column(i)->set_resizable(true);

  // events
  _event_store= Gtk::ListStore::create(_event_columns);
  tree= get_tree("event_tree");
  tree->set_model(_event_store);
  
  column= new Gtk::TreeView::Column(_("Event Name"));
  column->pack_start(_event_columns._icon, false);
  column->pack_start(_event_columns._name);
  tree->append_column(*Gtk::manage(column));
  tree->append_column(_("Type"), _event_columns._type);
  tree->append_column(_("Create Time"), _event_columns._ctime);
  tree->append_column(_("Update Time"), _event_columns._utime);
  for (int i= 0; i < 4; i++)
    tree->get_column(i)->set_resizable(true);

  // doc listing
  _doc_store= Gtk::TreeStore::create(_doc_columns);
  tree= get_tree("doc_tree");
  tree->set_model(_doc_store);
  
  column= new Gtk::TreeView::Column(_("Developer Documents"));
  column->pack_start(_doc_columns._icon, false);
  column->pack_start(_doc_columns._name);
  tree->append_column(*Gtk::manage(column));
  tree->append_column(_("Type"), _doc_columns._type);
  tree->append_column(_("Version"), _doc_columns._version);
  tree->append_column(_("User"), _doc_columns._user);
  tree->append_column(_("Checked Out"), _doc_columns._checked_out);
  tree->append_column(_("Description"), _doc_columns._description);
  for (int i= 0; i < 6; i++)
    tree->get_column(i)->set_resizable(true);
}


bool MACatalogsPanel::init()
{
  if (_xml)
    return true;

  if (!MAPanel::init_from_glade(get_glade_file(GLADE_CATALOGS_FILE), "panel_frame"))
    return false;

  _maint_dlg_xml= new MGGladeXML(get_glade_file(GLADE_CATALOGS_FILE), "maintenance_window");
  
  _schema_icon= PIXCACHE->load("16x16_Database.png");
  _sys_schema_icon= _schema_icon;
  _table_icon= PIXCACHE->load("16x16_Table.png");
  _column_icon= PIXCACHE->load("16x16_Field.png");
  _key_column_icon= PIXCACHE->load("16x16_KeyColumn.png");
  _index_icon= PIXCACHE->load("16x16_Index.png");
  _doc_icon= PIXCACHE->load("16x16_DBModel.png");
  _view_icon= PIXCACHE->load("16x16_View.png");
  _proc_icon= PIXCACHE->load("16x16_StoredProc.png");

  setup_trees();
  
  _schema_browser= new MGTableBrowserList(_("Schemata"), MGTableBrowserList::Schema);
  _schema_browser->signal_selected().connect(sigc::mem_fun(*this,&MACatalogsPanel::schema_selected));
//  _schema_browser->set_mark_delegate(schema_mark_delegate, this);
 //XXX _schema_browser->set_icons(_schema_icon, _sys_schema_icon, _table_icon, _column_icon);
  _schema_browser->show();
  {
    Gtk::MenuItem *item;
    item= myg_make_stock_image_item(Gtk::Stock::REFRESH, _("Refresh Schemata"));
    item->signal_activate().connect(sigc::mem_fun(*_data,&MDataInterface::refresh_catalogs));
    _schemata_menu.append(*item);
   
    _schemata_menu.append(*myg_make_separator_item());
    
    item= myg_make_stock_image_item(Gtk::Stock::NEW, _("Create Schema"));
    item->signal_activate().connect(sigc::mem_fun(*this,&MACatalogsPanel::create_schema));
    _schemata_menu.append(*item);

    item= myg_make_stock_image_item(Gtk::Stock::DELETE, _("Drop Schema"));
    item->signal_activate().connect(sigc::mem_fun(*this,&MACatalogsPanel::drop_schema));
    _schemata_menu.append(*item);
    
    _schemata_menu.show_all();
  }
  _schema_browser->set_popup_menu(&_schemata_menu);
  _data->signal_catalogs_refreshed().connect(sigc::mem_fun(*this,&MACatalogsPanel::schemata_reloaded));

  get_button("details_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::toggle_details));
  
  get_button("create_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::create_table));
  get_button("edit_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::edit_table));

  get_button("maintenance_button")->signal_clicked().connect(sigc::bind<MaintenanceCommand>(sigc::mem_fun(*this,&MACatalogsPanel::table_maintenance),Select));

  get_button("refresh_tables_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::reload_table_list));

  ((Gtk::Image*)get_widget("ts_image"))->set(_table_icon);
  ((Gtk::Image*)get_widget("rs_image"))->set(_table_icon);
  
  get_button("create_button")->set_sensitive(false);
  update_button_sensitivity();

  get_button("add_procedure_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::create_procedure));
  get_button("add_function_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::create_function));
  get_button("edit_procedure_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::edit_procedure));
  get_button("drop_procedure_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::drop_procedure));

  get_button("add_view_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::create_view));
  get_button("edit_view_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::edit_view));
  get_button("drop_view_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::drop_view));

  _details_shown= true;
  toggle_details();
  
  // maintenance dialog
  _maint_dlg_xml->get_button("next_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::maint_next_page));
  _maint_dlg_xml->get_button("cancel_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::maint_cancel));
  _maint_dlg_xml->get_button("finish_button")->signal_clicked().connect(sigc::mem_fun(*this,&MACatalogsPanel::maint_next_page));

  Gtk::TreeView *tree= _maint_dlg_xml->get_tree("list_tree");
  _maint_table_store= Gtk::ListStore::create(_maint_table_columns);
  tree->append_column("", _maint_table_columns._table);
  tree->set_model(_maint_table_store);
  
  // table menu
  {
    Gtk::MenuItem *item;


    myg_menu_add(_table_menu, Gtk::Stock::PROPERTIES, _("_Edit Table"),
                 sigc::mem_fun(*this,&MACatalogsPanel::edit_table), "edit");
    myg_menu_add(_table_menu, Gtk::Stock::PROPERTIES, _("Edit _Table Data"),
                 sigc::mem_fun(*this,&MACatalogsPanel::edit_table_data), "edit_data");
    myg_menu_add(_table_menu, _("Show Create Script"), 
                 sigc::mem_fun(*this,&MACatalogsPanel::show_create_table), "show_create");
    
    myg_menu_add(_table_menu);
    
    myg_menu_add(_table_menu, Gtk::Stock::PROPERTIES, _("_Check Table"),
                 sigc::bind<MaintenanceCommand>(sigc::mem_fun(*this,&MACatalogsPanel::table_maintenance), Check), "check");
    myg_menu_add(_table_menu, Gtk::Stock::PROPERTIES, _("_Repair Table"),
                 sigc::bind<MaintenanceCommand>(sigc::mem_fun(*this,&MACatalogsPanel::table_maintenance), Repair), "repair");
    myg_menu_add(_table_menu, Gtk::Stock::PROPERTIES, _("_Optimize Table"),
                 sigc::bind<MaintenanceCommand>(sigc::mem_fun(*this,&MACatalogsPanel::table_maintenance), Optimize), "optimize");
    myg_menu_add(_table_menu);
    myg_menu_add(_table_menu, Gtk::Stock::DELETE, _("Drop Table"),
                 sigc::mem_fun(*this,&MACatalogsPanel::drop_table), "drop");
    _table_menu.show_all();
  }

  update_table_menu_sensitivity();

  return true;
}

void MACatalogsPanel::schemata_reloaded(const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &schemata)
{
  _schema_browser->set_catalogs(schemata);
}


void MACatalogsPanel::create_schema()
{
  Glib::ustring name;
  
  if (myg_ask_string(*_app->window(), _("Create Schema"),
                     _("<b>Create Schema</b>\nEnter a name for the new schema."), name))
  {
    if (_inst->perform_query(ufmt("CREATE DATABASE %s;",name.c_str()),
                             _("Creating new schema...")))
      _data->refresh_catalogs();
  }
}


void MACatalogsPanel::drop_schema()
{
  Glib::ustring catalog;
  Glib::ustring schema;
  Gtk::TreeIter iter= _schema_browser->get_selected();
  if (iter)
  {
    catalog= _schema_browser->get_catalog(iter);
    schema= _schema_browser->get_schema(iter);
    {
      Gtk::MessageDialog dlg(*_app->window(),
                             ufmt(_("Drop schema '%s.%s'?\nAll data stored in it will be permanently deleted."),
                                  catalog.c_str(),schema.c_str()),
                             false,
                             Gtk::MESSAGE_WARNING,
                             Gtk::BUTTONS_OK_CANCEL,
                             true);
      if (dlg.run()!=Gtk::RESPONSE_OK)
        return;
    }

    if (_inst->perform_query(ufmt("DROP DATABASE %s;",schema.c_str()),
                             _("Dropping schema...")))
      _data->refresh_catalogs();
  }
}


void MACatalogsPanel::show()
{
  MAPanel::show();
  _schema_browser->set_catalogs(_data->get_catalogs());
  _xml->get_widget("note")->set_sensitive(false);
}


bool MACatalogsPanel::before_show()
{
  _app->add_side_panel(_schema_browser);
  return true;
}


bool MACatalogsPanel::before_hide()
{
  _app->remove_side_panel(_schema_browser);
  _xml->get_widget("note")->set_sensitive(false);
  return true;
}


void MACatalogsPanel::table_button_press(GdkEventButton *ev)
{
  if ((ev->type == GDK_BUTTON_PRESS) && (ev->button == 3))
    _table_menu.popup(ev->button, ev->time);
  else
    _table_menu.popdown();
}


void MACatalogsPanel::refresh_table_info(const Glib::ustring &catalog,const Glib::ustring &schema)
{
  Gtk::TreeIter iter;
  Gtk::TreeModel::Row row;
  MYX_TABLE_STATUS *ts;

  get_label("table_label")->set_markup(ufmt(_("<b>%s</b>\n"
                                              "All tables of the '%s' schema"),
                                            schema.c_str(),
                                            schema.c_str()));

  _table_store->clear();

  if (_entity_status)
  {
    _sum_total_tables= 0;
    _sum_rows= 0;
    _sum_data= 0;
    _sum_index= 0;
    long long total_data= 0, total_index= 0;

    for (unsigned int i= 0; i < _entity_status->schema_entities_num; ++i)
    {
      if (_entity_status->schema_entities[i].entity_type != MYX_ENTITY_TABLE) 
      {
        continue;
      }
      ts= (MYX_TABLE_STATUS *)_entity_status->schema_entities[i].entity;
      total_data+=strtoll((char*)ts->data_length, NULL, 10);
      total_index+=strtoll((char*)ts->index_length, NULL, 10);
    }

    // fill table list
    for (unsigned int i= 0; i < _entity_status->schema_entities_num; ++i)
    {
      if (_entity_status->schema_entities[i].entity_type != MYX_ENTITY_TABLE) 
      {
        continue;
      }
      ts= (MYX_TABLE_STATUS *)_entity_status->schema_entities[i].entity;

      long long val;
      iter= _table_store->append();
      row= *iter;
      
      _sum_total_tables++;

      row[_table_columns._icon]= _table_icon;
      row[_table_columns._name]= (char*)ts->table_name;
      row[_table_columns._type]= (char*)ts->table_type?:"?";
      row[_table_columns._format]= (char*)ts->row_format?:"?";
      row[_table_columns._rows]= (char*)ts->rows?:"?";
      _sum_rows+= strtoll((char*)ts->rows, NULL, 10);
      row[_table_columns._length]= format_value(val=strtoll((char*)ts->data_length,NULL,10));
      _sum_data+= val;
      if (total_data)
        row[_table_columns._data_percent]= val*100 / total_data;
      else
        row[_table_columns._data_percent]= 0;
      row[_table_columns._ilength]= format_value(val=strtoll((char*)ts->index_length,NULL,10));
      _sum_index+= val;
      if (total_index)
        row[_table_columns._index_percent]= val*100 / total_index;
      else
        row[_table_columns._index_percent]= 0;
      row[_table_columns._time]= (char*)ts->update_time?:"?";
      row[_table_columns._status]= ts;
      //row[_table_columns._status]= _table_status->schema_tables+i;
      
      // find the table data for this one
      for (unsigned int j= 0; j < _tables->schema_tables_num; ++j)
      {
        if (strcmp((char*)_tables->schema_tables[j].table_name, 
                    (char*)ts->table_name)==0)
        {
          row[_table_columns._table]= _tables->schema_tables+j;
          break;
        }
      }
    }
    get_label("ntables_label")->set_label(ufmt(_("Number of Tables: %i"), _sum_total_tables));
    get_label("rows_label")->set_label(ufmt(_("Rows: %i"), _sum_rows));
    get_label("datalen_label")->set_label(ufmt(_("Data Len.: %s"), format_value(_sum_data).c_str()));
    get_label("indexlen_label")->set_label(ufmt(_("Index Len.: %s"), format_value(_sum_index).c_str()));
  }
}


void MACatalogsPanel::refresh_index_info(const Glib::ustring &catalog,const Glib::ustring &schema)
{
  MYX_TABLE_STATUS *ts1/*, *ts2*/;
  
  get_label("index_label")->set_markup(ufmt(_("<b>%s</b>\n"
                                              "All indices in schema '%s'"),
                                            schema.c_str(),
                                            schema.c_str()));
  _index_store->clear();
  if (_entity_status)
  {
    // fill table list
    for (unsigned int i= 0; i < _entity_status->schema_entities_num; ++i)
    {
      if (_entity_status->schema_entities[i].entity_type != MYX_ENTITY_TABLE) 
      {
        continue;
      }
      ts1= (MYX_TABLE_STATUS *)_entity_status->schema_entities[i].entity;

      for (unsigned int j= 0; j < ts1->indexes_num; j++)
      {
        Gtk::TreeIter iter;
        Gtk::TreeModel::Row row;

        // fix of #13628
        /*
        if (_entity_status->schema_entities[j].entity_type != MYX_ENTITY_TABLE) 
        {
          continue;
        }
        ts2= (MYX_TABLE_STATUS *)_entity_status->schema_entities[j].entity;

        MYX_TABLE_INDEX *index= ts2->indexes+j;
        */
        MYX_TABLE_INDEX *index= ts1->indexes+j;

        iter= _index_store->append();
        row= *iter;

        row[_index_columns._icon]= _index_icon;
        row[_index_columns._name]= (char*)ts1->table_name;
        row[_index_columns._key]= index->key_name?:"?";
        row[_index_columns._type]= index->index_type?:"?";
        row[_index_columns._unique]= index->unique?_("UNIQUE"):"";
        row[_index_columns._null]= index->not_null?"":_("Yes");
        row[_index_columns._bold]= Pango::WEIGHT_BOLD;

        for (unsigned int k= 0; k < index->index_columns_num; k++)
        {
          Gtk::TreeIter kter= _index_store->append(row.children());
          Gtk::TreeModel::Row crow= *kter;
        
          crow[_index_columns._icon]= _column_icon;
          crow[_index_columns._name]= index->index_columns[k].column_name?:"?";
          crow[_index_columns._seq]= index->index_columns[k].seq_in_index?:"?";
          crow[_index_columns._coll]= strcmp2(index->index_columns[k].collation,"A")==0?_("Ascending"):"";
          crow[_index_columns._bold]= Pango::WEIGHT_NORMAL;
        }
      }
    }
    get_tree("index_tree")->expand_all();
  }
}

void MACatalogsPanel::refresh_event_info(const Glib::ustring &catalog,const Glib::ustring &schema)
{
  get_label("event_label")->set_markup(ufmt(_("<b>%s</b>\n"
                                              "All events in schema '%s'"),
                                            schema.c_str(),
                                            schema.c_str()));
}

void MACatalogsPanel::refresh_view_info(const Glib::ustring &catalog,const Glib::ustring &schema)
{
  Gtk::TreeIter iter;
  Gtk::TreeModel::Row row;
  MYX_VIEW_STATUS *vs;

  get_label("view_label")->set_markup(ufmt(_("<b>%s</b>\n"
                                             "All views in schema '%s'"),
                                           schema.c_str(),
                                           schema.c_str()));

  _view_store->clear();

  if (_entity_status)
  {
    // fill view list
    for (unsigned int i= 0; i < _entity_status->schema_entities_num; ++i)
    {
      if (_entity_status->schema_entities[i].entity_type != MYX_ENTITY_VIEW) 
      {
        continue;
      }
      vs= (MYX_VIEW_STATUS *)_entity_status->schema_entities[i].entity;

      iter= _view_store->append();
      row= *iter;

      row[_view_columns._icon]= _view_icon;
      row[_view_columns._name]= (char*)vs->view_name;
      row[_view_columns._comment]= (char*)vs->comment;
      row[_view_columns._updatable]= "n/a";
    }
  }
}

void MACatalogsPanel::refresh_procedure_info(const Glib::ustring &catalog,const Glib::ustring &schema)
{
  Gtk::TreeIter iter;
  Gtk::TreeModel::Row row;
  MYX_SCHEMA_STORED_PROCEDURE *ps;

  get_label("procedure_label")->set_markup(ufmt(_("<b>%s</b>\n"
                                                  "All stored procedures in schema '%s'"),
                                                schema.c_str(),
                                                schema.c_str()));

  _procedure_store->clear();

  if (_entity_status)
  {
    _sum_rows= 0;
    _sum_data= 0;
    _sum_index= 0;

    // fill sp list
    for (unsigned int i= 0; i < _entity_status->schema_entities_num; ++i)
    {
      if ((_entity_status->schema_entities[i].entity_type != MYX_ENTITY_PROC) &&
        (_entity_status->schema_entities[i].entity_type != MYX_ENTITY_FUNC))
      {
        continue;
      }
      ps= (MYX_SCHEMA_STORED_PROCEDURE *)_entity_status->schema_entities[i].entity;

      iter= _procedure_store->append();
      row= *iter;

      row[_procedure_columns._icon]= _proc_icon;
      row[_procedure_columns._name]= (char*)ps->name;
      row[_procedure_columns._definer]= (char*)ps->definer;
      row[_procedure_columns._created]= (char*)ps->created;
      row[_procedure_columns._modified]= (char*)ps->modified;
      row[_procedure_columns._return_datatype]= (char*)ps->return_datatype?:"";
      row[_procedure_columns._comment]= (char*)ps->comment;
    }
  }
}

void MACatalogsPanel::refresh_doc_info(const Glib::ustring &catalog,const Glib::ustring &schema)
{
  Gtk::TreeIter iter, jter;
  Gtk::TreeModel::Row row;
  
  get_label("doc_label")->set_markup(ufmt(_("<b>%s</b>\n"
                                            "Developer documents stored in schema '%s'"),
                                          schema.c_str(),
                                          schema.c_str()));

  _doc_store->clear();

  get_tree("doc_tree")->expand_all();
}


void MACatalogsPanel::refresh_user_info(const Glib::ustring &catalog,const Glib::ustring &schema)
{
  get_label("user_label")->set_markup(ufmt(_("<b>%s</b>\n"
                                             "Users with privileges to connect to '%s'"),
                                           schema.c_str(),
                                           schema.c_str()));
}


void MACatalogsPanel::update_button_sensitivity()
{
  int selection_count= get_tree("table_tree")->get_selection()->count_selected_rows();
  if (selection_count == 0)
  {
    get_widget("edit_button")->set_sensitive(false);

    get_widget("maintenance_button")->set_sensitive(false);
  }
  else if (selection_count == 1)
  {
    get_widget("edit_button")->set_sensitive(true);

    get_widget("maintenance_button")->set_sensitive(true);
  }
  else
  {
    get_widget("edit_button")->set_sensitive(false);

    get_widget("maintenance_button")->set_sensitive(true);
  }  
}


void MACatalogsPanel::update_table_menu_sensitivity()
{
  Gtk::TreeView *tree= get_tree("table_tree");
  Gtk::TreeIter iter;

  if (tree->get_selection()->count_selected_rows()==1)
  {
    iter= _table_store->get_iter(*tree->get_selection()->get_selected_rows().begin());

    _table_menu.set_sensitive(true);
    myg_menu_set_sensitive(_table_menu, "edit", true);
    myg_menu_set_sensitive(_table_menu, "edit_data", true);
    myg_menu_set_sensitive(_table_menu, "show_create", true);
    myg_menu_set_sensitive(_table_menu, "check", true);
    myg_menu_set_sensitive(_table_menu, "repair", true);
    myg_menu_set_sensitive(_table_menu, "optimize", true);
    myg_menu_set_sensitive(_table_menu, "drop", true);
  }
  else if (tree->get_selection()->count_selected_rows()>1)
  {
    _table_menu.set_sensitive(true);
    myg_menu_set_sensitive(_table_menu, "edit", false);
    myg_menu_set_sensitive(_table_menu, "edit_data", false);
    myg_menu_set_sensitive(_table_menu, "show_create", false);
    myg_menu_set_sensitive(_table_menu, "check", true);
    myg_menu_set_sensitive(_table_menu, "repair", true);
    myg_menu_set_sensitive(_table_menu, "optimize", true);
    myg_menu_set_sensitive(_table_menu, "drop", true);
  }
  else
  {
    _table_menu.set_sensitive(false);
  }
}

void MACatalogsPanel::selected_table()
{
  //Gtk::TreeView *tree= get_tree("table_tree");
  Gtk::TreeIter iter;
  Gtk::TreeModel::Row row;
  MYX_TABLE_STATUS *status= NULL;
  MYX_SCHEMA_TABLE *table= NULL;
  Gtk::Label *label= get_label("status_label");

  update_table_menu_sensitivity();

  update_button_sensitivity();

  if (iter)
  {
    get_widget("table_notebook")->set_sensitive(true);
    row= *iter;
    status= row[_table_columns._status];
    table= row[_table_columns._table];
  }
  else
  {
    get_widget("table_notebook")->set_sensitive(false);
    status= 0;
  }
  
  get_label("table_status_label")->set_markup(ufmt("<b>%s</b>\nDetailed table status information",
                                                   table?(char*)table->table_name:""));

  get_label("row_status_label")->set_markup(ufmt("<b>%s</b>\nDetailed table row status information",
                                                   table?(char*)table->table_name:""));

  if (status)
    label->set_markup(ufmt("<b>%s\n%s\n%s\n%s</b>",
                           (char*)status->table_type?:"",
                           (char*)status->row_format?:"",
                           (char*)status->auto_increment?:"",
                           (char*)status->create_options?:""));
  else
    label->set_text(_("Multiple selection"));
  
  label= get_label("comment_label");
  if (status)
    label->set_text(Glib::ustring((char*)status->comment?:""));
  else
    label->set_text("");

  
  label= get_label("data_label");
  if (status)
    label->set_markup(ufmt("<b>%s\n%s\n%s\n%s\n%s\n%s</b>",
                           (char*)status->rows?:"",
                         (char*)status->avg_row_length?:"",
                         (char*)status->data_length?:"",
                         (char*)status->max_data_length?:"",
                         (char*)status->index_length?:"",
                         (char*)status->data_free?:""));
  else
    label->set_text(_("Multiple selection"));

  label= get_label("time_label");
  if (status)
    label->set_markup(ufmt("<b>%s\n%s\n%s</b>",
                           (char*)status->create_time?:"",
                           (char*)status->update_time?:"",
                           (char*)status->check_time?:""));
  else
    label->set_text(_("Multiple selection"));

#if 0
  // fill columns
  _column_store->clear();

  if (table)
  {
    for (unsigned int i= 0; i < table->columns_num; i++)
    {
      iter= _column_store->append();
      row= *iter;
      row[_column_columns._icon]= table->columns[i].is_primary_key ? _key_column_icon : _column_icon;
      row[_column_columns._name]= (char*)table->columns[i].column_name;
      row[_column_columns._type]= (char*)table->columns[i].column_type?:"?";
      row[_column_columns._key]= table->columns[i].is_primary_key ? _("Yes") : "";
      row[_column_columns._null]= table->columns[i].is_null_allowed ? _("Yes") : "";
      row[_column_columns._extra]= (char*)table->columns[i].extra ? : "";
      row[_column_columns._default]= (char*)table->columns[i].default_value ? : "";
    }
  }
  
  // fill index
  _tindex_store->clear();
  
  if (status)
  {
    for (unsigned int j= 0; j < status->indexes_num; j++)
    {
      Gtk::TreeIter iter;
      Gtk::TreeModel::Row row;

      MYX_TABLE_INDEX *index= status->indexes+j;
      
      iter= _tindex_store->append();
      row= *iter;

      row[_index_columns._icon]= _index_icon;
      row[_index_columns._name]= index->key_name;
      row[_index_columns._type]= index->index_type?:"?";
      row[_index_columns._unique]= index->is_non_unique?"":_("UNIQUE");
      row[_index_columns._null]= index->is_null_allowed?_("Yes"):"";
      row[_index_columns._bold]= Pango::WEIGHT_BOLD;

      for (unsigned int k= 0; k < index->index_columns_num; k++)
      {
        Gtk::TreeIter kter= _tindex_store->append(row.children());
        Gtk::TreeModel::Row crow= *kter;
        
        crow[_index_columns._icon]= _column_icon;
        crow[_index_columns._name]= index->index_columns[k].column_name;
        crow[_index_columns._seq]= index->index_columns[k].seq_in_index?:"?";
        crow[_index_columns._coll]= strcmp2(index->index_columns[k].collation,"A")==0?_("Ascending"):"";
        crow[_index_columns._bold]= Pango::WEIGHT_NORMAL;
      }
    }
    get_tree("tindex_tree")->expand_all();
  }
#endif
}


void MACatalogsPanel::schema_selected(MGBrowserList *sender,
                                      const Gtk::TreeIter &node)
{
  Glib::ustring catalog;
  Glib::ustring schema;

  if (node)
  {
    catalog= _schema_browser->get_schema(node);
    schema= _schema_browser->get_schema(node);
    
    _xml->get_button("create_button")->set_sensitive(true);
    _xml->get_widget("note")->set_sensitive(true);
  }
  else
  {
    _xml->get_button("create_button")->set_sensitive(false);
    _xml->get_widget("note")->set_sensitive(false);
  }

  display_schema(catalog, schema, false);
  myx_use_schema(_data->get_instance()->get_mysql(), schema.c_str());

  update_button_sensitivity();
}


void MACatalogsPanel::display_schema(const Glib::ustring &catalog, const Glib::ustring &schema, bool refresh)
{
  if (_entity_status)
  {
    myx_free_schema_entity_status(_entity_status);
    _entity_status= NULL;
  }

  if (!schema.empty())
  {
    // this is owned by MDataInterface
    _tables= _data->get_schema_tables(catalog.c_str(), schema.c_str(), refresh);

    _entity_status= _data->get_schema_entity_status(catalog.c_str(), schema.c_str());
  }

  refresh_table_info(catalog, schema);
  refresh_index_info(catalog, schema);
  refresh_view_info(catalog, schema);
  refresh_procedure_info(catalog, schema);
  refresh_event_info(catalog, schema);
  refresh_doc_info(catalog, schema);
  refresh_user_info(catalog, schema);
}


std::list<Glib::ustring> MACatalogsPanel::get_selected_tables()
{
  Gtk::TreeView *tree= get_tree("table_tree");
  std::list<Gtk::TreePath> selected;
  Gtk::TreeIter iter;
  Gtk::TreeModel::Row row;
  std::list<Glib::ustring> objects;
  Glib::ustring catalog;
  Glib::ustring schema;

  iter= _schema_browser->get_selected();
  catalog= _schema_browser->get_catalog(iter);
  schema= _schema_browser->get_schema(iter);

  selected= tree->get_selection()->get_selected_rows();

  for (std::list<Gtk::TreePath>::const_iterator i= selected.begin();
       i != selected.end(); ++i)
  {
    Glib::ustring table;

    iter= _table_store->get_iter(*i);
    row= *iter;

    table= row[_table_columns._name];

    objects.push_back("`"+schema+"`.`"+table+"`");
  }

  return objects;
}


void MACatalogsPanel::reload_table_list()
{
  Glib::ustring catalog;
  Glib::ustring schema;
  Gtk::TreeIter node= _schema_browser->get_selected();

  if (node)
  {
    catalog= _schema_browser->get_catalog(node);
    schema= _schema_browser->get_schema(node);

    display_schema(catalog, schema, true);
  }
}


void MACatalogsPanel::table_dbl_clicked(const Gtk::TreeModel::Path &path, Gtk::TreeViewColumn *column)
{
  edit_table();
}



void MACatalogsPanel::toggle_details()
{
  if (_details_shown)
  {
    _xml->get_widget("table_notebook")->hide();
    _xml->get_button("details_button")->set_label(_("Show Details"));
    _details_shown= false;
  }
  else
  {
    _xml->get_widget("table_notebook")->show();
    _xml->get_button("details_button")->set_label(_("Hide Details"));
    _details_shown= true;
  }
}


void MACatalogsPanel::close_create_table_window(GdkEventAny *ev)
{
  Gtk::Main::instance()->quit();
}
  
void MACatalogsPanel::save_create_table(const char *script)
{
  Gtk::FileSelection fsel(_("Save Script"));
  Gtk::TreeIter iter= _table_store->get_iter(*get_tree("table_tree")->get_selection()->get_selected_rows().begin());

  if (iter)
  {
    Gtk::TreeRow row= *iter;
    Glib::ustring table= row[_table_columns._name];

    fsel.set_filename(ufmt("create_%s.sql", table.c_str()));
  }

  if (fsel.run() == Gtk::RESPONSE_OK)
  {
    FILE *f= fopen(fsel.get_filename().c_str(), "w+");
    fwrite(script, strlen(script), 1, f);
    fclose(f);
  }
}

void MACatalogsPanel::show_create_table()
{
  char *script;

  Gtk::TreeSelection::ListHandle_Path::iterator list_iter= 
    get_tree("table_tree")->get_selection()->get_selected_rows().begin();

  if(list_iter == get_tree("table_tree")->get_selection()->get_selected_rows().end())
    return;

  // vkolesnikov: when i replace the arg of get_iter() below with *list_iter
  // it doesnt work - assertion (path != NULL) fails. strange...
  Gtk::TreeIter iter= _table_store->get_iter(*get_tree("table_tree")->get_selection()->get_selected_rows().begin());

  if (iter)
  {
    Gtk::TreeRow row= *iter;
    Glib::ustring catalog, schema;
    Glib::ustring table= row[_table_columns._name];
    Gtk::TreeIter it= _schema_browser->get_selected();

    catalog= _schema_browser->get_catalog(it);
    schema= _schema_browser->get_schema(it);

    script= myx_get_create_table_script(_inst->get_mysql(),
                                        catalog.c_str(), schema.c_str(),
                                        table.c_str());
    if (script)
    {
      Gtk::Window window(Gtk::WINDOW_TOPLEVEL);
      Gtk::VBox vbox(false, 5);
      Gtk::ScrolledWindow swin;
      Gtk::TextView text;
      Gtk::HButtonBox bbox(Gtk::BUTTONBOX_END, 8);
      Gtk::Button close(Gtk::Stock::CLOSE), save(Gtk::Stock::SAVE);

      window.set_default_size(500, 250);
      window.set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
      window.set_transient_for(*_app->window());
      
      window.add(vbox);
      window.set_title(ufmt(_("CREATE Script for Table '%s'"),
                              table.c_str()));
      vbox.set_border_width(12);
      vbox.pack_start(swin);
      swin.add(text);
      swin.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
      swin.set_shadow_type(Gtk::SHADOW_IN);
      text.get_buffer()->set_text(script);

      window.signal_delete_event().connect_notify(sigc::mem_fun(*this,&MACatalogsPanel::close_create_table_window));
      
      save.signal_clicked().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MACatalogsPanel::save_create_table),script));
      close.signal_clicked().connect(sigc::bind<GdkEventAny*>(sigc::mem_fun(*this,&MACatalogsPanel::close_create_table_window),0));
      
      bbox.pack_start(save);
      bbox.pack_start(close);
      vbox.pack_start(bbox, false, false);

      window.show_all();
      window.show();
      
      Gtk::Main::instance()->run();
      g_free(script);
    }
  }
}

void MACatalogsPanel::create_table()
{
  Glib::ustring catalog, schema;
  Gtk::TreeIter it= _schema_browser->get_selected();

  catalog= _schema_browser->get_catalog(it);
  schema= _schema_browser->get_schema(it);

  table_edit_mode(true);
  
  MGTableEditor *editor= new MGTableEditor(true);
  editor->signal_close().connect(sigc::bind<MGTableEditor*>(sigc::mem_fun(*this,&MACatalogsPanel::table_editor_closed), editor));
  editor->new_table(_inst->get_mysql(), catalog, schema, _data->get_catalogs());
  _open_editors.push_back(editor);
}


void MACatalogsPanel::edit_table()
{
  Gtk::TreeView *tree= get_tree("table_tree");
  if (tree->get_selection()->count_selected_rows() != 1)
    return;
  
  Gtk::TreeIter iter= _table_store->get_iter(*tree->get_selection()->get_selected_rows().begin());

  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    Glib::ustring catalog, schema;
    Glib::ustring table= row[_table_columns._name];
    Gtk::TreeIter it= _schema_browser->get_selected();
    
    catalog= _schema_browser->get_catalog(it);
    schema= _schema_browser->get_schema(it);

    
    // check if there's already an editor open for this
    for (unsigned int i= 0; i < _open_editors.size(); i++)
    {
      MGTableEditor *editor= _open_editors[i];
      
      if (editor->get_catalog() == catalog && editor->get_schema() == schema
          && editor->get_table() == table)
      {
        ((Gtk::Window*)editor->get_widget()->get_toplevel())->present();
        return;
      }
    }
    
    
    table_edit_mode(true);

    MGTableEditor *editor= new MGTableEditor(true);
    editor->signal_close().connect(sigc::bind<MGTableEditor*>(sigc::mem_fun(*this,&MACatalogsPanel::table_editor_closed), editor));

    editor->show_table(_inst->get_mysql(), catalog, schema, table,
                        _data->get_catalogs());
    
    _open_editors.push_back(editor);
  }
}

void MACatalogsPanel::edit_table_data()
{
  Gtk::TreeView *tree= get_tree("table_tree");
  if (tree->get_selection()->count_selected_rows() != 1)
    return;
  
  Gtk::TreeIter iter= _table_store->get_iter(*tree->get_selection()->get_selected_rows().begin());

  if (iter)
  {    
    Gtk::TreeRow row = *iter;
    Glib::ustring catalog, schema;
    Glib::ustring table = row[_table_columns._name];
    Gtk::TreeIter it = _schema_browser->get_selected();

    catalog = _schema_browser->get_catalog(it);
    schema = _schema_browser->get_schema(it);

    try {
      Glib::spawn_command_line_async(ufmt("mysql-query-browser-bin --mqb-dir=%s --query=\"SELECT * FROM \\`%s\\`.\\`%s\\`\"",
                                    getenv("MA_DIR"), schema.c_str(), table.c_str()));
    } catch (...) {
      Gtk::MessageDialog dlg(*_app->window(),
                             _("Error launching MySQL Query Browser. Make sure it is installed and available under this account."),
                             false,
                             Gtk::MESSAGE_ERROR,
                             Gtk::BUTTONS_OK,
                             true);
      dlg.run();
    }
  }
}

static void *execute_query(MYSQL *mysql, const char *query, MYX_LIB_ERROR *error)
{
  long long int affected_rows;
  myx_query_execute_direct(mysql, query, error, &affected_rows);
  return NULL;
}


void MACatalogsPanel::drop_table()
{
  Gtk::TreeView *tree= get_tree("table_tree");
  if (tree->get_selection()->count_selected_rows() == 0)
    return;
  
  std::list<Glib::ustring> tables= get_selected_tables();
  if (!tables.empty())
  {
    Glib::ustring catalog, schema;
    Gtk::TreeIter it= _schema_browser->get_selected();

    catalog= _schema_browser->get_catalog(it);
    schema= _schema_browser->get_schema(it);

    if (tables.size() > 1)
    {
      Glib::ustring tables_str;
      
      for (std::list<Glib::ustring>::const_iterator i= tables.begin(); i != tables.end(); ++i)
        tables_str+= "    "+*i+"\n";
      Gtk::MessageDialog dlg(*_app->window(),
                             ufmt(_("Please confirm deletion of the following tables:\n%s"
                                    "Drop table will delete all data in the table and discard the table definition from the schema."),
                                    tables_str.c_str()),
                             false,
                             Gtk::MESSAGE_WARNING, Gtk::BUTTONS_OK_CANCEL, true);
      if (dlg.run() != Gtk::RESPONSE_OK)
        return;
      dlg.hide();
    }
    else
    {
      Gtk::MessageDialog dlg(*_app->window(), 
                             ufmt(_("Please confirm deletion of table %s.\n"
                                    "Drop table will delete all data in the table and discard the table definition from the schema."),
                                  tables.begin()->c_str()),
                             false,
                             Gtk::MESSAGE_WARNING, Gtk::BUTTONS_OK_CANCEL, true);
      if (dlg.run() != Gtk::RESPONSE_OK)
        return;
      dlg.hide();
    }
    MYX_LIB_ERROR error;
    
    for (std::list<Glib::ustring>::const_iterator tbl= tables.begin(); tbl != tables.end(); ++tbl)
    {
      Glib::ustring query= "DROP TABLE "+*tbl;
      _inst->perform_data_fetch3((MInstanceInfo::DataFetcher3)execute_query, (void*)query.c_str(), &error, 
                                 ufmt("Dropping table %s...", tbl->c_str()));
      if (error != MYX_NO_ERROR)
      {
        if (error == MYX_SQL_ERROR)
          myg_show_mysql_error(*_app->window(), ufmt(_("Could not drop table '%s'."), tbl->c_str()), _inst->get_mysql());
        else
          myg_show_xlib_error(*_app->window(), ufmt(_("Could not drop table '%s'."), tbl->c_str()), error);
      }
    }
    reload_table_list();
  }
}


void MACatalogsPanel::table_editor_closed(MGTableEditor *editor)
{
  _open_editors.erase(std::find(_open_editors.begin(), _open_editors.end(), editor));
  if (_open_editors.empty())
    table_edit_mode(false);
  reload_table_list();
}


void MACatalogsPanel::table_edit_mode(bool flag)
{
  if (flag)
  {
//    _xml->get_widget("note")->hide();
//    _editor->get_widget()->show();
    _schema_browser->set_sensitive(false);
  }
  else
  {
//    _xml->get_widget("note")->show();
//    _editor->get_widget()->hide();

    _schema_browser->set_sensitive(true);
  }
}


void MACatalogsPanel::table_maintenance(MaintenanceCommand maint)
{
  Gtk::Window *win= static_cast<Gtk::Window*>(_maint_dlg_xml->get_widget("maintenance_window"));

  Gtk::TreeIter iter;
  Gtk::TreeRow row;

  win->set_transient_for(*_app->window());
  
  _maint_table_store->clear();
  
  std::list<Glib::ustring> tables= get_selected_tables();
  for (std::list<Glib::ustring>::const_iterator it= tables.begin();
       it != tables.end(); ++it)
  {
    iter= _maint_table_store->append();
    row= *iter;
    
    row[_maint_table_columns._table]= *it;
  }

  switch (maint)
  {
  case Select:
    _maint_dlg_xml->get_note("note")->set_current_page(0);
    break;
  case Optimize:
    _maint_dlg_xml->get_note("note")->set_current_page(1);
    _maint_dlg_xml->get_toggle("optimize_radio")->set_active(true);
    break;
  case Check:
    _maint_dlg_xml->get_note("note")->set_current_page(2);
    _maint_dlg_xml->get_toggle("check_radio")->set_active(true);
    break;
  case Repair:
    _maint_dlg_xml->get_note("note")->set_current_page(3);
    _maint_dlg_xml->get_toggle("repair_radio")->set_active(true);
    break;
  }

  _maint_dlg_xml->get_button("cancel_button")->show();
  _maint_dlg_xml->get_button("next_button")->show();
  _maint_dlg_xml->get_button("next_button")->set_sensitive(true);
  _maint_dlg_xml->get_button("finish_button")->hide();

  win->show();

  Gtk::Main::instance()->run();
  
  win->hide();
}


void MACatalogsPanel::maint_cancel()
{
  Gtk::Main::instance()->quit();
}


bool MACatalogsPanel::maint_pulse_progressbar()
{
  static_cast<Gtk::ProgressBar*>(_maint_dlg_xml->get_widget("progressbar"))->pulse();
  
  return true;
}


void MACatalogsPanel::maint_next_page()
{
  MYX_TABLE_COMMAND_STATUSES *status= NULL;

  switch (_maint_dlg_xml->get_note("note")->get_current_page())
  {
  case 0: // initial page  
    if (_maint_dlg_xml->get_toggle("optimize_radio")->get_active())
    {
      _maint_dlg_xml->get_label("progress_label")->set_markup(_("<big><b>Optimizing Tables</b></big>\nThis operation may take a few minutes, please wait..."));
      _maint_dlg_xml->get_note("note")->set_current_page(1);
    }
    else if (_maint_dlg_xml->get_toggle("check_radio")->get_active())
    {
      _maint_dlg_xml->get_label("progress_label")->set_markup(_("<big><b>Checking Tables</b></big>\nThis operation may take a few minutes, please wait..."));
      _maint_dlg_xml->get_note("note")->set_current_page(2);
    }
    else // repair
    {
      _maint_dlg_xml->get_label("progress_label")->set_markup(_("<big><b>Repairing Tables</b></big>\nThis operation may take a few minutes, please wait..."));
      _maint_dlg_xml->get_note("note")->set_current_page(3);
    }
    break;
    
  case 1: // optimize
    {
      Glib::ustring objects= strjoin(get_selected_tables());
      long type_mask= 0;
     
      if (_maint_dlg_xml->get_toggle("o_local_check")->get_active())
        type_mask|= MYX_REPAIR_NO_WRITE_TO_BINLOG;

      _maint_dlg_xml->get_button("next_button")->set_sensitive(false);
      _maint_dlg_xml->get_note("note")->set_current_page(4);
      SigC::Connection conn = Glib::signal_timeout().connect(sigc::mem_fun(*this, &MACatalogsPanel::maint_pulse_progressbar), 200);

      status= (MYX_TABLE_COMMAND_STATUSES*)
        _data->get_instance()->perform_data_fetch3((MInstanceInfo::DataFetcher3)myx_optimize_table,
                                                   (void*)objects.c_str(), (void*)type_mask,
                                                   _("Optimizing table(s)..."));
      
      conn.disconnect();
      
      if (!status)
        _data->show_last_error(_("Error executing OPTIMIZE TABLE."));
      
      _maint_dlg_xml->get_label("result_label")->set_markup(_("<big><b>Optimize Tables</b></big>\nThe selected tables were optimized.\n\nResults:"));
    }
    break;
    
  case 2: // check
    {
      Glib::ustring objects= strjoin(get_selected_tables());
      struct {
        char *name;
        long mask;
      } options[]= {
        {"c_quick_check", MYX_CHECK_QUICK},
        {"c_fast_check", MYX_CHECK_FAST},
        {"c_medium_check", MYX_CHECK_MEDIUM},
        {"c_extended_check", MYX_CHECK_EXTENDED},
        {"c_changed_check", MYX_CHECK_CHANGED},
        {NULL, 0}
      };
      long type_mask= 0;
      
      for (int i= 0; options[i].name; i++)
      {
        if (_maint_dlg_xml->get_toggle(options[i].name)->get_active())
          type_mask |= options[i].mask;
      }

      _maint_dlg_xml->get_button("next_button")->set_sensitive(false);
      _maint_dlg_xml->get_note("note")->set_current_page(4);      
      SigC::Connection conn = Glib::signal_timeout().connect(sigc::mem_fun(*this, &MACatalogsPanel::maint_pulse_progressbar), 200);

      status= (MYX_TABLE_COMMAND_STATUSES*)
        _data->get_instance()->perform_data_fetch3((MInstanceInfo::DataFetcher3)myx_check_table,
                                                   (void*)objects.c_str(), (void*)type_mask,
                                                   _("Checking table(s)..."));
      
      conn.disconnect();
      
      if (!status)
        _data->show_last_error(_("Error executing CHECK TABLE."));
      
      _maint_dlg_xml->get_label("result_label")->set_markup(_("<big><b>Check Tables</b></big>\nThe selected tables were checked.\n\nResults:"));
    }
    break;

  case 3: // repair
    {
      Glib::ustring objects= strjoin(get_selected_tables());
      struct {
        char *name;
        long mask;
      } options[]= {
        {"r_quick_check", MYX_CHECK_QUICK},
        {"r_extended_check", MYX_CHECK_EXTENDED},
        {"r_use_frm_check", MYX_REPAIR_USE_FRM},
        {NULL, 0}
      };
      long type_mask= 0;
      for (int i= 0; options[i].name; i++)
      {
        if (_maint_dlg_xml->get_toggle(options[i].name)->get_active())
          type_mask |= options[i].mask;
      }

      if (_maint_dlg_xml->get_toggle("r_local_check")->get_active())
        type_mask |= MYX_REPAIR_NO_WRITE_TO_BINLOG;

      _maint_dlg_xml->get_button("next_button")->set_sensitive(false);
      _maint_dlg_xml->get_note("note")->set_current_page(4);
      SigC::Connection conn = Glib::signal_timeout().connect(sigc::mem_fun(*this, &MACatalogsPanel::maint_pulse_progressbar), 200);

      status= (MYX_TABLE_COMMAND_STATUSES*)
        _data->get_instance()->perform_data_fetch3((MInstanceInfo::DataFetcher3)myx_repair_table,
                                                   (void*)objects.c_str(), (void*)type_mask,
                                                   _("Repairing table(s)..."));
      conn.disconnect();
      
      if (!status)
        _data->show_last_error(_("Error executing REPAIR TABLE."));
      
      _maint_dlg_xml->get_label("result_label")->set_markup(_("<big><b>Repair Tables</b></big>\nThe selected tables were repaired.\n\nResults:"));
    }
    break;
    
  case 5: //results
    Gtk::Main::instance()->quit();
    break;
  }

  if (status)
  {
    Glib::RefPtr<Gtk::TextBuffer> buffer= _maint_dlg_xml->get_text("result_text")->get_buffer();
    Glib::RefPtr<Gtk::TextTag> bold;
    
    bold= Gtk::TextTag::create();
    buffer->get_tag_table()->add(bold);

    bold->property_weight()= Pango::WEIGHT_BOLD;
    
    _maint_dlg_xml->get_note("note")->set_current_page(5);
    _maint_dlg_xml->get_button("cancel_button")->hide();
    _maint_dlg_xml->get_button("next_button")->hide();
    _maint_dlg_xml->get_button("finish_button")->show();

    buffer->set_text("");
    
    for (unsigned int i= 0; i < status->status_num; i++)
    {
      switch (status->status[i].message_type)
      {
      case MYX_MESSAGE_STATUS:
        buffer->insert_with_tag(buffer->end(), _("Table: "), bold);
        buffer->insert(buffer->end(), Glib::ustring(status->status[i].table)+"  ");
        buffer->insert_with_tag(buffer->end(), _("Status: "), bold);
        buffer->insert(buffer->end(), Glib::ustring(status->status[i].message)+"\n");
        break;
      case MYX_MESSAGE_ERROR:
        buffer->insert_with_tag(buffer->end(), _("Table: "), bold);
        buffer->insert(buffer->end(), Glib::ustring(status->status[i].table)+"  ");
        buffer->insert_with_tag(buffer->end(), _("ERROR: "), bold);
        buffer->insert(buffer->end(), Glib::ustring(status->status[i].message)+"\n");
        break;
      case MYX_MESSAGE_WARNING:
        buffer->insert_with_tag(buffer->end(), _("Table: "), bold);
        buffer->insert(buffer->end(), Glib::ustring(status->status[i].table)+"  ");
        buffer->insert_with_tag(buffer->end(), _("WARNING: "), bold);
        buffer->insert(buffer->end(), Glib::ustring(status->status[i].message)+"\n");
        break;
      case MYX_MESSAGE_INFO:
        buffer->insert_with_tag(buffer->end(), _("Table: "), bold);
        buffer->insert(buffer->end(), Glib::ustring(status->status[i].table)+"  ");
        buffer->insert_with_tag(buffer->end(), _("Info: "), bold);
        buffer->insert(buffer->end(), Glib::ustring(status->status[i].message)+"\n");
        break;
      }
    }
    myx_free_command_status(status);
  }
}


MAPanel *create_catalogs_panel(MAdministrator *app, MDataInterface *data)
{
  return new MACatalogsPanel(app, data);
}

char *MACatalogsPanel::input_string_dialog(const char *title,
                                          const char *prompt)
{
  Gtk::Dialog dialog;
  Gtk::Entry entry;
  Gtk::Button close(Gtk::Stock::CLOSE), save(Gtk::Stock::SAVE);
  Gtk::Label lab(prompt);

  dialog.set_default_size(400, 125);
  dialog.set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
  dialog.set_transient_for(*_app->window());

  Gtk::VBox* box= dynamic_cast<Gtk::VBox*>(dialog.get_child());
  
  dialog.set_title(title);
  box->set_border_width(12);
  box->pack_start(lab);
  box->pack_start(entry);

  dialog.add_button(Gtk::Stock::CANCEL, GTK_RESPONSE_CANCEL);
  dialog.add_button(Gtk::Stock::OK, GTK_RESPONSE_OK);

  dialog.show_all();
  dialog.show();
  if (dialog.run() == GTK_RESPONSE_OK)
  {
    return g_strdup(entry.get_text().c_str());
  }
  return NULL;
}

char *MACatalogsPanel::run_script_editor(const char *title, 
                                        const char *sql)
{
  Gtk::Dialog dialog;
  Gtk::ScrolledWindow swin;
  Gtk::TextView text;
  Gtk::HButtonBox bbox(Gtk::BUTTONBOX_END, 8);
  MGTextViewUndoManager undo(&text);
  //Gtk::Button close(Gtk::Stock::CLOSE), save(Gtk::Stock::SAVE);

  dialog.set_default_size(500, 250);
  dialog.set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
  dialog.set_transient_for(*_app->window());
  
  Gtk::VBox* box= dynamic_cast<Gtk::VBox*>(dialog.get_child());
  
  dialog.set_title(title);
  box->set_border_width(12);
  box->pack_start(swin);
  swin.add(text);
  swin.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  swin.set_shadow_type(Gtk::SHADOW_IN);
  text.get_buffer()->set_text(sql);

  undo.reset();
  
  dialog.add_button(Gtk::Stock::CANCEL, GTK_RESPONSE_CANCEL);
  dialog.add_button(Gtk::Stock::EXECUTE, GTK_RESPONSE_OK);

  dialog.show_all();
  dialog.show();

  if (dialog.run() == GTK_RESPONSE_OK)
  {
    Glib::RefPtr<Gtk::TextBuffer> buf= text.get_buffer();
    return g_strdup(text.get_buffer()->get_text(true).c_str());
  }
  return NULL;
}

void MACatalogsPanel::create_procedure_impl(MYX_SCHEMA_STORED_PROCEDURE_TYPE t)
{
  char *name= input_string_dialog("Routine Name", "Please Enter Routine Name");
  if (name)
  {
    Gtk::TreeIter node;
    Glib::ustring schema= _schema_browser->get_schema(_schema_browser->get_selected());

    char *sp_template;
    if (t == MSPT_PROCEDURE)
    {
      sp_template= g_strdup_printf("CREATE PROCEDURE `%s`.`%s` ()\nBEGIN\nEND", 
                            schema.c_str(), name);
    }
    else
    {
      sp_template= g_strdup_printf("CREATE FUNCTION `%s`.`%s` () RETURNS int\nBEGIN\n\n     RETURN 0;\nEND",
                            schema.c_str(), name);
    }
    while (sp_template)
    {
      char *sp_code= run_script_editor("Create a New Stored Routine", sp_template);
      g_free(sp_template);
      sp_template= NULL;
      if (sp_code)
      {
        MYX_LIB_ERROR error;
        execute_query(_data->get_instance()->get_mysql(), sp_code, &error);

        if (error != MYX_NO_ERROR)
        {
          if (error == MYX_SQL_ERROR)
            myg_show_mysql_error(*_app->window(), 
                                 ufmt(_("Could not create routine '%s.%s'."), 
                                      schema.c_str(), name),
                                 _inst->get_mysql());
          else
            myg_show_xlib_error(*_app->window(), 
                                ufmt(_("Could not create routine '%s.%s'."), 
                                     schema.c_str(), name),
                                error);
          sp_template= sp_code;
          sp_code= NULL;
        }
        else
        {
          display_schema("def", schema.c_str(), false);
          g_free(sp_code);
        }
      }
    }
    g_free(name);
  }
}

void MACatalogsPanel::create_procedure()
{
  create_procedure_impl(MSPT_PROCEDURE);
}

void MACatalogsPanel::create_function()
{
  create_procedure_impl(MSPT_FUNCTION);
}

void MACatalogsPanel::edit_procedure()
{
  Gtk::TreeView *tree= get_tree("procedure_tree");
  if (tree->get_selection()->count_selected_rows() != 1)
    return;
  
  Gtk::TreeIter iter= 
    _procedure_store->get_iter(*tree->get_selection()->get_selected_rows().begin());

  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    Glib::ustring catalog, schema;
    Glib::ustring rv= row[_procedure_columns._return_datatype];
    MYX_SCHEMA_STORED_PROCEDURE_TYPE t= 
      rv.length() > 0 ? MSPT_FUNCTION : MSPT_PROCEDURE;
    Glib::ustring sp= row[_procedure_columns._name];
    Gtk::TreeIter it= _schema_browser->get_selected();

    catalog= _schema_browser->get_catalog(it);
    schema= _schema_browser->get_schema(it);

    MYX_DBM_STORED_PROCEDURE_DATA *sp_data= myx_dbm_get_sp_data(
        _data->get_instance()->get_mysql(), 
        catalog.c_str(), schema.c_str(), sp.c_str(), t, 
        MYX_DEFAULT_QUOTE_CHAR, 0);

    char *sql2= run_script_editor("Edit Stored Routine", sp_data->definition);
    if (sql2)
    {
      MYX_LIB_ERROR error;
      char *sql;
      if (t == MSPT_FUNCTION)
      {
        sql= g_strdup_printf("DROP FUNCTION IF EXISTS `%s`.`%s`", 
          schema.c_str(), sp.c_str());
      }
      else
      {
        sql= g_strdup_printf("DROP PROCEDURE IF EXISTS `%s`.`%s`", 
          schema.c_str(), sp.c_str());
      }
      execute_query(_data->get_instance()->get_mysql(), sql, &error);
      g_free(sql);
      
      do
      {
        execute_query(_data->get_instance()->get_mysql(), sql2, &error);
        if (error != MYX_NO_ERROR)
        {
          if (error == MYX_SQL_ERROR)
          {
            myg_show_mysql_error(*_app->window(), _("Could not save changes to routine"), _inst->get_mysql());
          }
          else
          {
            myg_show_xlib_error(*_app->window(), _("Could not save changes to routine"), error);
          }
          char *sql3= run_script_editor("Edit Stored Routine", sql2);
          if (sql3)
          {
            execute_query(_data->get_instance()->get_mysql(), sql2, &error);
            g_free(sql2);
            sql2= sql3;
          }
          else  // operation canceled
          {
            // restore old definition
            execute_query(_data->get_instance()->get_mysql(), sp_data->definition, &error);
            break;
          }
        }
        else
        {
          break;
        }
      }
      while(1);

      g_free(sql2);        
      display_schema(catalog, schema, false);
    }
    myx_dbm_free_sp_data(sp_data);
  }
}

void MACatalogsPanel::drop_procedure()
{
  Gtk::TreeView *tree= get_tree("procedure_tree");
  if (tree->get_selection()->count_selected_rows() != 1)
    return;

  Gtk::TreeIter iter= 
    _procedure_store->get_iter(*tree->get_selection()->get_selected_rows().begin());

  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    Glib::ustring catalog, schema;
    Glib::ustring rv= row[_procedure_columns._return_datatype];
    MYX_SCHEMA_STORED_PROCEDURE_TYPE t= 
      rv.length() > 0 ? MSPT_FUNCTION : MSPT_PROCEDURE;
    Glib::ustring sp= row[_procedure_columns._name];
    Gtk::TreeIter it= _schema_browser->get_selected();

    catalog= _schema_browser->get_catalog(it);
    schema= _schema_browser->get_schema(it);

    Gtk::MessageDialog dlg(*_app->window(), 
                           ufmt(_("Drop stored routine '%s'?"),
                                schema.c_str()),
                           false,
                           Gtk::MESSAGE_QUESTION, Gtk::BUTTONS_YES_NO, true);
    if (dlg.run() != GTK_RESPONSE_YES)
      return;

    MYX_LIB_ERROR error;
    char *sql;
    if (t == MSPT_FUNCTION)
    {
      sql= g_strdup_printf("DROP FUNCTION IF EXISTS `%s`.`%s`", 
        schema.c_str(), sp.c_str());
    }
    else
    {
      sql= g_strdup_printf("DROP PROCEDURE IF EXISTS `%s`.`%s`", 
        schema.c_str(), sp.c_str());
    }
    execute_query(_data->get_instance()->get_mysql(), sql, &error);
    g_free(sql);

    if (error != MYX_NO_ERROR)
    {
      if (error == MYX_SQL_ERROR)
        myg_show_mysql_error(*_app->window(), _("Could not drop routine"), _inst->get_mysql());
      else
        myg_show_xlib_error(*_app->window(), _("Could not drop routine"), error);
    }
    display_schema(catalog.c_str(), schema.c_str(), false);
  }
}

void MACatalogsPanel::create_view()
{
  char *name= input_string_dialog("View Name", "Please Enter View Name");
  if (name)
  {
    Gtk::TreeIter node;
    Glib::ustring schema= _schema_browser->get_schema(_schema_browser->get_selected());

    char *view_template= g_strdup_printf("CREATE OR REPLACE VIEW `%s`.`%s` AS SELECT ", 
                                schema.c_str(), name);
    while (view_template)
    {
      char *view_code= run_script_editor("Create a New View", view_template);

      g_free(view_template);
      view_template= NULL;

      if (view_code)
      {
        MYX_LIB_ERROR error;
        execute_query(_data->get_instance()->get_mysql(), view_code, &error);

        if (error != MYX_NO_ERROR)
        {
          if (error == MYX_SQL_ERROR)
            myg_show_mysql_error(*_app->window(),
                                 ufmt(_("Could not create view '%s.%s'."), schema.c_str(), name),
                                 _inst->get_mysql());
          else
            myg_show_xlib_error(*_app->window(), 
                                ufmt(_("Could not create view '%s.%s'."), schema.c_str(), name),
                                error);

          view_template= view_code;
          view_code= NULL;
          // retry with the old code
        }
        else
        {
          display_schema("def", schema, false);
          g_free(view_code);
          // leave view_template = NULL, which will break the loop
        }
      }
    }
    g_free(name);
  }
}

void MACatalogsPanel::edit_view()
{
  Gtk::TreeView *tree= get_tree("view_tree");
  if (tree->get_selection()->count_selected_rows() != 1)
    return;
  
  Gtk::TreeIter iter= 
    _view_store->get_iter(*tree->get_selection()->get_selected_rows().begin());

  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    Glib::ustring catalog, schema;
    Glib::ustring view= row[_view_columns._name];
    Gtk::TreeIter it= _schema_browser->get_selected();

    catalog= _schema_browser->get_catalog(it);
    schema= _schema_browser->get_schema(it);

    MYX_DBM_VIEW_DATA *view_data= myx_dbm_get_view_data(
      _data->get_instance()->get_mysql(), catalog.c_str(), schema.c_str(), view.c_str(), 
                                                        MYX_DEFAULT_QUOTE_CHAR);

    char *sql2= run_script_editor("Edit View", view_data->definition);
    if (sql2)
    {
      MYX_LIB_ERROR error;
      char *sql= g_strdup_printf("DROP VIEW IF EXISTS `%s`.`%s`", 
          schema.c_str(), view.c_str());

      execute_query(_data->get_instance()->get_mysql(), sql, &error);
      g_free(sql);

      do
      {
        execute_query(_data->get_instance()->get_mysql(), sql2, &error);
    
        if (error != MYX_NO_ERROR)
        {
          if (error == MYX_SQL_ERROR)
          {
            myg_show_mysql_error(*_app->window(), _("Could not save changes to view"), _inst->get_mysql());
          }
          else
          {
            myg_show_xlib_error(*_app->window(), _("Could not save changes to view"), error);
          }
          char *sql3= run_script_editor("Edit View", sql2);
          if (sql3)
          {
            execute_query(_data->get_instance()->get_mysql(), sql3, &error);
            g_free(sql2);
            sql2= sql3;
          }
          else  // cancel operation
          {
            // restore old view definition
            execute_query(_data->get_instance()->get_mysql(), 
              view_data->definition, &error);
            break;
          }
        }
        else
        {
          break;
        }
      }
      while(1);

      g_free(sql2);
      display_schema(catalog, schema, false);
    }
    myx_dbm_free_view_data(view_data);
  }
}

void MACatalogsPanel::drop_view()
{
  Gtk::TreeView *tree= get_tree("view_tree");
  if (tree->get_selection()->count_selected_rows() != 1)
    return;
  
  Gtk::TreeIter iter= 
    _view_store->get_iter(*tree->get_selection()->get_selected_rows().begin());

  if (iter)
  {
    Gtk::TreeRow row= *iter;
    Glib::ustring catalog, schema;
    Glib::ustring view= row[_view_columns._name];
    Gtk::TreeIter it= _schema_browser->get_selected();

    catalog= _schema_browser->get_catalog(it);
    schema= _schema_browser->get_schema(it);


    Gtk::MessageDialog dlg(*_app->window(), 
                           ufmt("Drop view `%s`.`%s`?", schema.c_str(), view.c_str()), false,
                           Gtk::MESSAGE_QUESTION, Gtk::BUTTONS_YES_NO, true);
    if (dlg.run() != GTK_RESPONSE_YES)
      return;

    MYX_LIB_ERROR error;
    char *sql;
    sql= g_strdup_printf("DROP VIEW IF EXISTS `%s`.`%s`", 
      schema.c_str(), view.c_str());

    execute_query(_data->get_instance()->get_mysql(), sql, &error);
    g_free(sql);

    if (error != MYX_NO_ERROR)
    {
      if (error == MYX_SQL_ERROR)
        myg_show_mysql_error(*_app->window(), _("Could not drop view"), _inst->get_mysql());
      else
        myg_show_xlib_error(*_app->window(), _("Could not drop view"), error);
    }
    display_schema(catalog.c_str(), schema.c_str(), false);
  }
}
