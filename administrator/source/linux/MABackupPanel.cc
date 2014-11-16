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
#include "MABackupPanel.h"
#include "MDataInterface.h"
#include "MDispatcher.h"
#include "MAdministrator.h"

#include "MGFileBrowserList.h"
#include "MGTableBrowserList.h"

#include "myg_gtkutils.h"
#include "myg_utils.h"
#include "util.h"


#include <list>


#define PROFILE_SUFFIX ".mbp"


static bool backup_aborted;


static int backup_options[]={
  MYX_B_NO_CREATES,
  MYX_B_NO_EXTENDED_INSERT,
  MYX_B_ADD_DROP_TABLE,
  MYX_B_COMPLETE_INSERTS,
  MYX_B_COMMENT,
  MYX_B_DONT_WRITE_FULL_PATH,
  MYX_B_ANSI_QUOTES,
  MYX_B_DISABLE_KEYS,
  MYX_B_ADD_LOCKS//?
};

struct ProgressData {
  Glib::ustring table;
  int num_tables_total;
  int num_tables;
  int num_rows_total;
  int num_rows;
  bool ready;
};


struct BackupThreadArgument {
  MABackupPanel *me;
  std::string path;
  MYX_BACKUP_PROFILE *profile;
  ProgressData pdata;
};




MABackupPanel::MABackupPanel(MAdministrator *app, MDataInterface *data)
    : MAPanel(app, data), _dirty(false)
{
}


MABackupPanel::~MABackupPanel()
{
  delete _progress_xml;	
}



void MABackupPanel::setup_selection_list()
{
  Gtk::TreeView *tree= get_tree("selection_tree");
  Gtk::TreeView::Column *column;
  MGCellRendererTristate::ImageList image_list(3);
  
  Gdk::Color color;
  // load tristate button image list
  image_list[0]= Gdk::Pixmap::create_from_xpm(_app->window()->get_window(),
                                              color,
                                              get_app_file("option_unchecked.xpm"));
  image_list[1]= Gdk::Pixmap::create_from_xpm(_app->window()->get_window(),
                                              color,
                                              get_app_file("option_checked_light.xpm"));
  image_list[2]= Gdk::Pixmap::create_from_xpm(_app->window()->get_window(),
                                              color,
                                              get_app_file("option_checked.xpm"));
  
  // schedule buttons
  get_button("install_button")->signal_clicked().connect(sigc::mem_fun(*this,&MABackupPanel::install_schedule));
  get_button("uninstall_button")->signal_clicked().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MABackupPanel::uninstall_schedule), true));
  
  // setup list columns

  _selection_tree= tree;
  
  _selection_store= Gtk::TreeStore::create(_sel_columns);
  tree->set_model(_selection_store);

  MGCellRendererTristate *trend= new MGCellRendererTristate(image_list);
  column= new Gtk::TreeView::Column(_("Objects"), *Gtk::manage(trend));
  tree->append_column(*Gtk::manage(column));
  column->add_attribute(trend->property_state(), _sel_columns._state);
  trend->signal_toggled().connect(sigc::mem_fun(*this,&MABackupPanel::row_toggled));
  
  column= tree->get_column(0);
  column->pack_start(_sel_columns._icon, false);
  column->pack_start(_sel_columns._object);
  

  tree->append_column(_("Table Type"), _sel_columns._type);
  tree->append_column(_("Rows"), _sel_columns._rows);
  tree->append_column(_("Data Length"), _sel_columns._length);
  tree->append_column(_("Update Time"), _sel_columns._time);
  
  tree->get_column(1)->set_alignment(1.0);
  tree->get_column(2)->set_alignment(1.0);

  Glib::RefPtr<Gtk::TreeSelection> sel= tree->get_selection();

  sel->signal_changed().connect(sigc::mem_fun(*this,&MABackupPanel::selected_object));
  
}


bool MABackupPanel::init()
{
  std::string profile_dir= prefs.build_path_to(prefs.backup_profiles_directory);

  if (_xml)
    return true;

  if (!MAPanel::init_from_glade(get_glade_file(GLADE_BACKUP_FILE), "panel_frame"))
    return false;

  _progress_xml= new MGGladeXML(get_glade_file(GLADE_BACKUP_FILE), "progress_dialog", "mysql-administrator");
  if (!_progress_xml)
  {
    g_error(_("Could not load backup progress dialog"));
    return false;
  }

  _progress_label= (Gtk::Label*)_progress_xml->get_widget("progress_label");
  _table_progress= (Gtk::ProgressBar*)_progress_xml->get_widget("table_progress");
  _row_progress= (Gtk::ProgressBar*)_progress_xml->get_widget("row_progress");

  _file_browser= new MGFileBrowserList(_("Backup Projects"),
                                       profile_dir);
  _file_browser->set_extension(PROFILE_SUFFIX);
  _file_browser->signal_selected().connect(sigc::mem_fun(*this,&MABackupPanel::profile_selected));

  _file_browser->set_empty_message(_("Click 'New Project' to create a Backup Profile."));
  
  {
    Gtk::Menu *menu= Gtk::manage(new Gtk::Menu);
    Gtk::MenuItem *item;
    Gtk::Image *image;

    image= Gtk::manage(new Gtk::Image(Gtk::Stock::NEW, Gtk::ICON_SIZE_MENU));
    item= Gtk::manage(new Gtk::ImageMenuItem(*image, _("_New Profile"), true));
    item->signal_activate().connect(sigc::mem_fun(*this,&MABackupPanel::create_profile));
    image->show();
    item->show();
    menu->append(*item);

    image= Gtk::manage(new Gtk::Image(Gtk::Stock::COPY, Gtk::ICON_SIZE_MENU));
    item= Gtk::manage(new Gtk::ImageMenuItem(*image, _("_Clone Profile"), true));
    item->signal_activate().connect(sigc::mem_fun(*this,&MABackupPanel::clone_profile));
    image->show();
    item->show();
    menu->append(*item);

    image= Gtk::manage(new Gtk::Image(Gtk::Stock::DELETE, Gtk::ICON_SIZE_MENU));
    item= Gtk::manage(new Gtk::ImageMenuItem(*image, _("_Delete Profile"), true));
    item->signal_activate().connect(sigc::mem_fun(*this,&MABackupPanel::delete_profile));
    image->show();
    item->show();
    menu->append(*item);
    
    _file_browser->set_popup_menu(menu);
  }
  
  setup_selection_list();
  
  _schema_icon= PIXCACHE->load("16x16_Database.png");
  _sys_schema_icon= _schema_icon;

  _table_icon= PIXCACHE->load("16x16_Table.png");
  _view_icon= PIXCACHE->load("16x16_View.png");
  _proc_icon= PIXCACHE->load("16x16_StoredProc.png");
  _column_icon= PIXCACHE->load("16x16_Field.png");

  _schema_browser= new MGTableBrowserList("", MGTableBrowserList::Schema);
  
  _schema_browser->set_name("schema_list");
  
  ((Gtk::Container*)get_widget("schemata_frame"))->add(*Gtk::manage(_schema_browser));
  _schema_browser->signal_selected().connect(sigc::mem_fun(*this,&MABackupPanel::schema_selected));
 // _schema_browser->set_icons(_schema_icon, _sys_schema_icon, _table_icon, _column_icon);
  _schema_browser->show();

  _data->signal_catalogs_refreshed().connect(sigc::mem_fun(*this,&MABackupPanel::schemata_reloaded));

  get_button("target_button")->signal_clicked().connect(sigc::mem_fun(*this,&MABackupPanel::select_target_path));
  get_toggle("backup_all_check")->signal_toggled().connect(sigc::mem_fun(*this,&MABackupPanel::backup_all_check_toggled));

  // setup signals
  Gtk::Button *btn;
  
  btn= (Gtk::Button*)_progress_xml->get_widget("stop_button");
  btn->signal_clicked().connect(sigc::mem_fun(*this,&MABackupPanel::stop_backup));
  
  btn= get_button("add_button");
  btn->signal_clicked().connect(sigc::mem_fun(*this,&MABackupPanel::add_object));

  btn= get_button("remove_button");
  btn->signal_clicked().connect(sigc::mem_fun(*this,&MABackupPanel::remove_object));

  btn= get_button("new_button");
  btn->signal_clicked().connect(sigc::mem_fun(*this,&MABackupPanel::create_profile));

  btn= get_button("import_button");
  btn->signal_clicked().connect(sigc::mem_fun(*this,&MABackupPanel::import_profile));

  btn= get_button("save_button");
  btn->signal_clicked().connect(sigc::mem_fun(*this,&MABackupPanel::save_profile));

  btn= get_button("start_button");
  btn->signal_clicked().connect(sigc::mem_fun(*this,&MABackupPanel::perform_backup));

  get_entry("name_entry")->signal_changed().connect(sigc::mem_fun(*this,&MABackupPanel::set_dirty));
  get_entry("name_entry")->signal_changed().connect(sigc::mem_fun(*this,&MABackupPanel::name_changed));
  
  for (int i= 1; i <= 8; i++)
  {
    char buf[32];
    sprintf(buf, "option%i_check", i);
    ((Gtk::CheckButton*)get_widget(buf))->signal_toggled().connect(sigc::mem_fun(*this,&MABackupPanel::set_dirty));
  }

  ((Gtk::RadioButton*)get_widget("lockall_radio"))->signal_toggled().connect(sigc::mem_fun(*this,&MABackupPanel::set_dirty));
  ((Gtk::RadioButton*)get_widget("singletrans_radio"))->signal_toggled().connect(sigc::mem_fun(*this,&MABackupPanel::set_dirty));
  ((Gtk::RadioButton*)get_widget("normal_radio"))->signal_toggled().connect(sigc::mem_fun(*this,&MABackupPanel::set_dirty));
  ((Gtk::RadioButton*)get_widget("binlogpos_radio"))->signal_toggled().connect(sigc::mem_fun(*this,&MABackupPanel::set_dirty));

  get_toggle("backup_all_check")->signal_toggled().connect(sigc::mem_fun(*this,&MABackupPanel::set_dirty));
  
  ((Gtk::OptionMenu*)get_widget("schedule_menu"))->signal_changed().connect(sigc::mem_fun(*this,&MABackupPanel::schedule_type_changed));
  schedule_type_changed();
  
  update_sensitivity(false);

  return true;
}


void MABackupPanel::show()
{
  MAPanel::show();
  
  _schema_browser->set_catalogs(_data->get_catalogs());
  _file_browser->refresh();
}


bool MABackupPanel::before_show()
{
  _app->add_side_panel(_file_browser);

  return true;
}


bool MABackupPanel::before_hide()
{  
  _app->remove_side_panel(_file_browser);

  return true;
}

void MABackupPanel::schemata_reloaded(const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &schemata)
{
  _schema_browser->set_catalogs(schemata);
}

/**********************************************************************/

void MABackupPanel::name_changed()
{
}


void MABackupPanel::update_sensitivity(bool flag)
{
  get_widget("notebook")->set_sensitive(flag);
  get_widget("save_button")->set_sensitive(flag);
  
  update_buttonbar_sensitivity();
}

void MABackupPanel::update_backup_all_state()
{
  bool check_backup_all= true;

  if (get_widget("notebook")->is_sensitive())
  {
    // check whether there's some selected table
    Gtk::TreeModel::Children ch= _selection_store->children();
    for (Gtk::TreeIter p= ch.begin(); p != ch.end(); ++p)
    {
      Gtk::TreeModel::Row pr= *p;
      Gtk::TreeModel::Children cch= pr.children();
      for (Gtk::TreeIter i= cch.begin(); i != cch.end(); ++i)
      {
        Gtk::TreeModel::Row cr= *i;
        if (cr[_sel_columns._state] != MGCellRendererTristate::ON)
        {
          check_backup_all= false;
          break;
        }
      }
    }
  }

  get_toggle("backup_all_check")->set_active(check_backup_all);
}

void MABackupPanel::update_button_sensitivity()
{
  Glib::RefPtr<Gtk::TreeSelection> sel= _selection_tree->get_selection();
  Gtk::TreeIter iter;
  Gtk::TreeModel::Row row;

  if (sel->count_selected_rows())
  {
    get_widget("add_button")->set_sensitive(false);

    iter= sel->get_selected();
    
    bool flag= false;
    if (iter)
    {
      row= *iter;
      flag= row[_sel_columns._what] == SelectionColumns::CSchema;
    }
    // we can only remove whole schemas
    get_widget("remove_button")->set_sensitive(flag);
  }
  else
  {
    get_widget("remove_button")->set_sensitive(false);

    Glib::ustring catalog, schema;
    iter= _schema_browser->get_selected();
    if (iter)
    {
      catalog= _schema_browser->get_catalog(iter);
      schema= _schema_browser->get_catalog(iter);
    
      // check if the selected schema is already in the list
      Gtk::TreeModel::Children ch= _selection_store->children();
      bool found= false;
      
      for (iter= ch.begin(); iter != ch.end(); ++iter)
      {
        row= *iter;
        
        if (schema == row[_sel_columns._object])
        {
          found= true;
          break;
        }
      }
      // if schema not yet in list, allow it to be added
      get_widget("add_button")->set_sensitive(!found);
    }
  }
}

void MABackupPanel::backup_all_check_toggled()
{
  bool b= get_toggle("backup_all_check")->get_active();
  if(b)
  {
    Gtk::TreeModel::Children ch= _selection_store->children();
    for (Gtk::TreeIter p= ch.begin(); p != ch.end(); ++p)
    {
      Gtk::TreeModel::Row pr= *p;
      Gtk::TreeModel::Children cch= pr.children();
      for (Gtk::TreeIter i= cch.begin(); i != cch.end(); ++i)
      {
        Gtk::TreeModel::Row cr= *i;
        cr[_sel_columns._state] = MGCellRendererTristate::ON;
      }
    }
  }
}

void MABackupPanel::select_target_path()
{
  Gtk::FileSelection fsel(_("Select Target Directory"));

  fsel.hide_fileop_buttons();
  fsel.set_filename(get_entry("target_entry")->get_text()+"/");
  fsel.get_file_list()->set_sensitive(false);

  if (fsel.run() == Gtk::RESPONSE_OK)
  {
    get_entry("target_entry")->set_text(fsel.get_filename());
  }
}


void MABackupPanel::update_buttonbar_sensitivity()
{
  bool enable_start= false;

  if (get_widget("notebook")->is_sensitive())
  {
    // check whether there's some selected table
    Gtk::TreeModel::Children ch= _selection_store->children();
    for (Gtk::TreeIter p= ch.begin(); p != ch.end(); ++p)
    {
      Gtk::TreeModel::Row pr= *p;
      Gtk::TreeModel::Children cch= pr.children();
      if(cch.size() > 0)
      {
        for (Gtk::TreeIter i= cch.begin(); i != cch.end(); ++i)
        {
          Gtk::TreeModel::Row cr= *i;
          if (cr[_sel_columns._state] == MGCellRendererTristate::ON)
          {
            enable_start= true;
            break;
          }
        }
      }
      else
      {
        if (pr[_sel_columns._state] == MGCellRendererTristate::ON)
        {
          enable_start= true;
          break;
        }
      }
    }
  }

  get_widget("start_button")->set_sensitive(enable_start);
}


void MABackupPanel::add_schema_to_list(const Glib::ustring &catalog,
                                       const Glib::ustring &schema,
                                       bool checked)
{ 
  Gtk::TreeIter s_iter, t_iter;
  Gtk::TreeModel::Row s_row, t_row;
  unsigned long long total_rows= 0, total_length= 0;

  // add schema
  s_iter= _selection_store->append();
  s_row= *s_iter;

  s_row[_sel_columns._icon]= _schema_icon;
  s_row[_sel_columns._object]= schema;
  s_row[_sel_columns._state]= checked?MGCellRendererTristate::ON:MGCellRendererTristate::OFF;
  s_row[_sel_columns._what]= SelectionColumns::CSchema;

  // fetch table list for schema
  MYX_SCHEMA_ENTITY_STATUS *entities= _data->get_schema_entity_status(catalog,schema);

  // add tables of this schema
  for (unsigned int i= 0; entities && i < entities->schema_entities_num; ++i)
  {
    if(entities->schema_entities[i].entity_type == MYX_ENTITY_TABLE)
    {
      MYX_TABLE_STATUS *table= (MYX_TABLE_STATUS *)entities->schema_entities[i].entity;
  
      t_iter= _selection_store->append(s_row.children());
      t_row= *t_iter;
  
      t_row[_sel_columns._icon]= _table_icon;
      t_row[_sel_columns._object]= (char*)table->table_name;
      t_row[_sel_columns._type]= (table->table_type)? ((char*)table->table_type):"";
      t_row[_sel_columns._rows]= strtoll((char*)table->rows, NULL, 10);
      t_row[_sel_columns._length]= format_value(strtoll((char*)table->data_length, NULL, 10));
      t_row[_sel_columns._time]= (char*)table->update_time;
  
      t_row[_sel_columns._lengthn]= strtoll((char*)table->data_length, NULL, 10);
      t_row[_sel_columns._state]= checked?MGCellRendererTristate::ON:MGCellRendererTristate::OFF;
      t_row[_sel_columns._what]= SelectionColumns::CTable;
      
      total_rows+= strtoll((char*)table->rows, NULL, 10);
      total_length+= strtoll((char*)table->data_length, NULL, 10);
    }
    else if(entities->schema_entities[i].entity_type == MYX_ENTITY_VIEW)
    {
      MYX_VIEW_STATUS *view= (MYX_VIEW_STATUS *)entities->schema_entities[i].entity;
  
      t_iter= _selection_store->append(s_row.children());
      t_row= *t_iter;
  
      t_row[_sel_columns._icon]= _view_icon;
      t_row[_sel_columns._object]= (char*)view->view_name;
      t_row[_sel_columns._type]= "VIEW";
      t_row[_sel_columns._rows]= 0;
      t_row[_sel_columns._length]= "";
      t_row[_sel_columns._time]= "";
      t_row[_sel_columns._lengthn]= 0;
      t_row[_sel_columns._state]= checked?MGCellRendererTristate::ON:MGCellRendererTristate::OFF;
      t_row[_sel_columns._what]= SelectionColumns::CView;
    }
    else if((entities->schema_entities[i].entity_type == MYX_ENTITY_PROC) ||
      (entities->schema_entities[i].entity_type == MYX_ENTITY_FUNC))
    {
      MYX_SCHEMA_STORED_PROCEDURE *sp= 
      (MYX_SCHEMA_STORED_PROCEDURE *)entities->schema_entities[i].entity;
  
      t_iter= _selection_store->append(s_row.children());
      t_row= *t_iter;
  
      t_row[_sel_columns._icon]= _proc_icon;
      t_row[_sel_columns._object]= (char*)sp->name;
      t_row[_sel_columns._type]= 
        entities->schema_entities[i].entity_type == MYX_ENTITY_PROC 
        ? "PROCEDURE":"FUNCTION";
      t_row[_sel_columns._rows]= 0;
      t_row[_sel_columns._length]= "";
      t_row[_sel_columns._time]= sp->modified;
      t_row[_sel_columns._lengthn]= 0;
      t_row[_sel_columns._state]= checked?MGCellRendererTristate::ON:MGCellRendererTristate::OFF;
      t_row[_sel_columns._what]= SelectionColumns::CRoutine;
    }
  }

  s_row[_sel_columns._rows]= total_rows;
  s_row[_sel_columns._lengthn]= total_length;
  s_row[_sel_columns._length]= format_value(total_length);
  
  if (entities)
    myx_free_schema_entity_status(entities);
}


void MABackupPanel::add_object()
{
  Gtk::TreeIter node;
  Glib::ustring catalog;
  Glib::ustring schema;

  node= _schema_browser->get_selected();
  if (node)
  {
    catalog= _schema_browser->get_catalog(node);
    schema= _schema_browser->get_schema(node);

    add_schema_to_list(catalog, schema, true);
  }

  //update_buttonbar_sensitivity(); //set dirty does that
  set_dirty();
}


void MABackupPanel::remove_object()
{
  Gtk::TreeIter iter= _selection_tree->get_selection()->get_selected();
  
  _selection_store->erase(iter);
  
  //update_buttonbar_sensitivity(); //set dirty does that
  set_dirty();
}


void MABackupPanel::selected_object()
{
  _schema_browser->unselect();

  update_button_sensitivity();
}


void MABackupPanel::copy_profile(const std::string &source_name,
                                 const std::string &dest_name)
{
  std::string profile_dir= _file_browser->get_directory()+"/";
  std::string new_name;
  MYX_BACKUP_PROFILE *profile;
  MYX_ADMIN_LIB_ERROR err;
  int i;

  // copy the contents to the profile directory
  profile= myx_load_profile(Glib::path_get_basename(source_name).c_str(), 
                            std::string(Glib::path_get_dirname(source_name)+"/").c_str(),
                            &err);
  if (!profile)
  {
    show_adminlib_error(*_app->window(), 
                        ufmt(_("Could not read the profile file '%s'"),
                             source_name.c_str()),
                        err);
    return;
  }
  i= 1;

  new_name= dest_name;
  while (_file_browser->check_exists(new_name))
    new_name= dest_name+tostr(i++);

  g_free(profile->profile_name);
  profile->profile_name= g_strdup(new_name.c_str());

  new_name+= PROFILE_SUFFIX;
  
  if (myx_save_profile(new_name.c_str(),
                       profile_dir.c_str(),
                       profile) < 0)
  {
    myx_free_profile(profile);
    myg_show_error(*_app->window(), 
               _("Could not write the copied profile file"));
    return;
  }
  myx_free_profile(profile);
}


void MABackupPanel::import_profile()
{
  Gtk::FileSelection fsel;

  if (fsel.run() == Gtk::RESPONSE_OK)
  {
    std::string name= Glib::path_get_basename(fsel.get_filename());
    if (name.rfind('.')!=std::string::npos)
    {
      name= name.substr(0, name.rfind('.'));
    }

    copy_profile(fsel.get_filename(), name);

    _file_browser->refresh();
  }
}


void MABackupPanel::create_profile()
{
  std::string name= "profile";

  int i= 1;
  // figure a new name
  while (_file_browser->check_exists(name))
  {
    name= "profile_"+tostr(i++);
  }

  _file_browser->unselect();

  get_entry("name_entry")->set_text(name);

  // add to the list
  save_profile();

  Gtk::TreeIter node;
  _file_browser->refresh();
  if (_file_browser->find_node(name, _file_browser->_columns._name, node))
    _file_browser->set_selection(node);
}


void MABackupPanel::clone_profile()
{
  Gtk::TreeIter iter= _file_browser->get_selected();
  std::string profile_dir= _file_browser->get_directory()+"/";
  std::string name, new_name;

  _file_browser->get_row_object(iter, name);

  copy_profile(_file_browser->get_path_for_entry(name),
               name+"_copy");

  _file_browser->refresh();
}


void MABackupPanel::delete_profile()
{
  Gtk::TreeIter iter= _file_browser->get_selected();
  std::string name;

  _file_browser->get_row_object(iter, name);

  _file_browser->remove(name);
  _file_browser->refresh();
  
  //XXX removed scheduled backup if there is one
  g_message("remove schedules!");
}


void MABackupPanel::show_profile(MYX_BACKUP_PROFILE *profile)
{
  if (profile)
  {
    _profile_name= profile->profile_name;
  
    get_entry("name_entry")->set_text(profile->profile_name?:"");

  }
  else
  {
    _profile_name.clear();
    get_entry("name_entry")->set_text("");
  }
  for (int i= 1; i <= 8; i++)
  {
    char buf[32];
    Gtk::CheckButton *check;
    sprintf(buf, "option%i_check", i);
    check= (Gtk::CheckButton*)get_widget(buf);

    check->set_active(profile && (profile->options & backup_options[i-1])!=0);
  }
  
  get_toggle("backup_all_check")->set_active(profile && (profile->options & MYX_B_COMPLETE_SCHEMATAS));
  if (profile && profile->options & MYX_B_SINGLE_TRANSACTION)
    get_toggle("singletrans_radio")->set_active(true);
  else if (profile && profile->options & MYX_B_LOCK_ALL_TABLES)
    get_toggle("lockall_radio")->set_active(true);
  else if (profile && profile->options & MYX_B_POINT_IN_TIME_BACKUP)
    get_toggle("binlogpos_radio")->set_active(true);
  else
    get_toggle("normal_radio")->set_active(true);

  _selection_store->clear();

  
  if (profile)
  {
    std::map<Glib::ustring,unsigned int> seen;

    for (unsigned int i= 0; i < profile->backup_content->tables_num; i++)
    {
      MYX_BACKUP_TABLE *table= profile->backup_content->tables+i;
      Glib::ustring key(Glib::ustring(table->catalog)+"."+Glib::ustring(table->schema));
      
      if (seen.find(key) == seen.end())
      {
        seen[key]= 1;
        
        add_schema_to_list(table->catalog, table->schema, false);
      }
      else
      {
        // count how many tables are selected
        seen[key]++;
      }
    }
    
    Gtk::TreeModel::Children top_nodes= _selection_store->children();
    for (unsigned int i= 0; i < profile->backup_content->tables_num; i++)
    {
      MYX_BACKUP_TABLE *table= profile->backup_content->tables+i;
      
      Gtk::TreeIter piter, citer;
      Gtk::TreeModel::Row prow, crow;
      Glib::ustring catalog(table->catalog?:"def");
      Glib::ustring schema(table->schema);
      Glib::ustring key(catalog+"."+schema);

      // find schema node
      for (piter= top_nodes.begin(); piter != top_nodes.end(); ++piter)
      {
        prow= *piter;
        
        if (prow[_sel_columns._object] == schema)
        {
          if(!table->table)
          {
            prow[_sel_columns._state]= MGCellRendererTristate::ON;
          }
          else
          {
            Gtk::TreeModel::Children sub_nodes= prow.children();
            Glib::ustring tname(table->table);
            // find table node
            for (citer= sub_nodes.begin(); citer != sub_nodes.end(); ++citer)
            {
              crow= *citer;
              
              if (crow[_sel_columns._object] == tname)
              {
                crow[_sel_columns._state]= MGCellRendererTristate::ON;
                break;
              }
            }
            // if all nodes are checked, check parent too
            if (seen[key]==sub_nodes.size())
            {
              prow[_sel_columns._state]= MGCellRendererTristate::ON;
            }
            else if (seen[key] > 0) // at least one checked
            {
              prow[_sel_columns._state]= MGCellRendererTristate::MAYBE;
            }
            else
            {
              prow[_sel_columns._state]= MGCellRendererTristate::OFF;
            }
            break;
          }
        }
      }
    }
  }

  show_schedule();
  
  update_sensitivity(true);
}

MYX_BACKUP_CONTENT *MABackupPanel::gather_selected_tables()
{
  Gtk::TreeIter siter, titer;
  unsigned int i;
  MYX_BACKUP_CONTENT *content;
  GPtrArray *ta= g_ptr_array_new();
  Glib::ustring str;
  
  for (siter= _selection_store->children().begin();
       siter != _selection_store->children().end();
       ++siter)
  {
    Gtk::TreeModel::Row row= *siter;

    if(row.children().empty())
    {
      MYX_BACKUP_TABLE *bt= 
              (MYX_BACKUP_TABLE *)g_malloc(sizeof(MYX_BACKUP_TABLE));
      
      str= row[_sel_columns._object];
      bt->catalog= g_strdup("def");//XXX
      bt->schema= g_strdup(str.c_str());
      bt->table= NULL;
      bt->flags= 0;
      g_ptr_array_add(ta, bt);
    }
    else
    {
      for (titer= row.children().begin(); titer != row.children().end(); ++titer)
      {
        Gtk::TreeModel::Row rrow= *titer;

        if (rrow[_sel_columns._state] == MGCellRendererTristate::ON)
        {
          MYX_BACKUP_TABLE *bt= 
                  (MYX_BACKUP_TABLE *)g_malloc(sizeof(MYX_BACKUP_TABLE));
          bt->catalog= g_strdup("def");//XXX
          str= row[_sel_columns._object];
          bt->schema= g_strdup(str.c_str());
          str= rrow[_sel_columns._object];
          bt->table= g_strdup(str.c_str());

          str= rrow[_sel_columns._type];
          if(strcmp(str.c_str(), "VIEW") == 0) 
          {
            bt->flags= MYX_BTF_IS_VIEW;
          }
          else if(strcmp(str.c_str(), "PROCEDURE") == 0) 
          {
            bt->flags= MYX_BTF_IS_PROCEDURE;
          }
          else if(strcmp(str.c_str(), "FUNCTION") == 0)
          {
            bt->flags= MYX_BTF_IS_FUNCTION;
          }
          else
          {
            bt->flags= MYX_BTF_IS_TABLE;
          }
          g_ptr_array_add(ta, bt);
        }
      }
    }
  }

  content= (MYX_BACKUP_CONTENT*)g_malloc0(sizeof(MYX_BACKUP_CONTENT));
  content->tables_num= ta->len;
  content->tables= (MYX_BACKUP_TABLE *)
                   g_malloc(sizeof(MYX_BACKUP_TABLE)*ta->len);

  for(i= 0; i < ta->len; i++)
  {
    MYX_BACKUP_TABLE *bt= (MYX_BACKUP_TABLE *)g_ptr_array_index(ta, i);
    content->tables[i].catalog= bt->catalog;
    content->tables[i].schema= bt->schema;
    content->tables[i].table= bt->table;
    content->tables[i].flags= bt->flags;
  }

  return content;
}


MYX_BACKUP_PROFILE *MABackupPanel::gather_profile()
{
  MYX_BACKUP_PROFILE *profile;
  Glib::ustring str;
  
  profile= (MYX_BACKUP_PROFILE*)g_malloc0(sizeof(MYX_BACKUP_PROFILE));

  profile->profile_name= g_strdup(get_entry("name_entry")->get_text().c_str());

  int flags= 0;
  for (int i= 1; i <= 8; i++)
  {
    Gtk::CheckButton *check;
    char buf[32];
    sprintf(buf, "option%i_check", i);
    check= (Gtk::CheckButton*)get_widget(buf);
    if (check->get_active())
        flags |= backup_options[i-1];
  }
  if (get_toggle("lockall_radio")->get_active())
    flags |= MYX_B_LOCK_ALL_TABLES; 
  else if (get_toggle("singletrans_radio")->get_active())
    flags |= MYX_B_SINGLE_TRANSACTION;
  else if (get_toggle("binlogpos_radio")->get_active())
    flags |= MYX_B_POINT_IN_TIME_BACKUP;
  
  if (get_toggle("backup_all_check")->get_active())
    flags |= MYX_B_COMPLETE_SCHEMATAS;


  profile->options= flags;

  profile->backup_type= MYX_BT_SQL_SCRIPT;
  
  profile->backup_content= gather_selected_tables();

  return profile;
}


void MABackupPanel::save_profile()
{
  MYX_BACKUP_PROFILE *profile;

  // gather data
  profile= gather_profile();
  
  // check data
  if (!*profile->profile_name)
  {
    Gtk::MessageDialog dlg(*_app->window(),
                           _("Please supply a name for this profile"),
                           false,
                           Gtk::MESSAGE_ERROR,
                           Gtk::BUTTONS_OK,
                           true);
    dlg.run();
  }
  else if (strchr(profile->profile_name, '/'))
  {
    Gtk::MessageDialog dlg(*_app->window(),
                           _("Profile name cannot contain character '/'"),
                           false,
                           Gtk::MESSAGE_ERROR,
                           Gtk::BUTTONS_OK,
                           true);
    dlg.run();
  }
  else
  {
    std::string profile_dir= _file_browser->get_directory()+"/";
    Gtk::TreeIter iter= _file_browser->get_selected();
    std::string old_name;
    std::string new_name= profile->profile_name;

    // get old profile name
    if (iter)
    {
      _file_browser->get_row_object(iter, old_name);
    }

    // save
    if (myx_save_profile(std::string(new_name+PROFILE_SUFFIX).c_str(),
                         profile_dir.c_str(),
                         profile) < 0)
    {
      g_warning("could not save profile '%s'", profile->profile_name);
    }
    else
    {
      if (old_name != new_name)
      {
        // delete schedule with old name
        uninstall_schedule(false);

        // delete old profile
        _file_browser->remove(old_name);
      }

      _profile_name= profile->profile_name;
      
      // reinstall schedule with new name
      install_schedule();

      _file_browser->refresh();

      _dirty= false;
      
      if (old_name != new_name)
      {
        Gtk::TreeIter node;
        
        // reselect profile, which will refresh everything
        if (_file_browser->find_node(profile->profile_name, _file_browser->_columns._name, node))
          _file_browser->set_selection(node);
      }
    }
  }

  myx_free_profile(profile);
}


void MABackupPanel::row_toggled(const Glib::ustring &path)
{
  Gtk::TreeIter iter= _selection_store->get_iter(path);
  Gtk::TreeModel::Row row= *iter;
  
  // toggled the schema
  if (row[_sel_columns._what] == SelectionColumns::CSchema)
  {
    Gtk::TreeModel::Children ch= row->children();

    if (row[_sel_columns._state] != MGCellRendererTristate::ON)
    {
      unsigned long total_rows= 0, total_length= 0;
      // schema was checked: check all tables
      for (Gtk::TreeIter it= ch.begin(); it != ch.end(); ++it)
      {
        Gtk::TreeModel::Row r= *it;
        
        r[_sel_columns._state]= MGCellRendererTristate::ON;

        total_rows+= r[_sel_columns._rows];
        total_length+= r[_sel_columns._lengthn];
      }
      row[_sel_columns._state]= MGCellRendererTristate::ON;
      row[_sel_columns._rows]= total_rows;
      row[_sel_columns._lengthn]= total_length;
      row[_sel_columns._length]= format_value(total_length);
    }
    else
    {
      // schema was unchecked: unselect all tables
      for (Gtk::TreeIter it= ch.begin(); it != ch.end(); ++it)
      {
        Gtk::TreeModel::Row r= *it;
        
        r[_sel_columns._state]= MGCellRendererTristate::OFF;
      }
      row[_sel_columns._state]= MGCellRendererTristate::OFF;
      row[_sel_columns._rows]= 0;
      row[_sel_columns._lengthn]= 0;
      row[_sel_columns._length]= "0";
    }
  }
  else
  {
    Gtk::TreeIter par= row.parent();
    Gtk::TreeModel::Row rpar= *par;
    unsigned long tmp;

    if (row[_sel_columns._state] != MGCellRendererTristate::ON)
    {
      // table was checked
      row[_sel_columns._state]= MGCellRendererTristate::ON;

      // update totals
      tmp= rpar[_sel_columns._rows];
      tmp+= row[_sel_columns._rows];
      rpar[_sel_columns._rows]= tmp;

      tmp= rpar[_sel_columns._lengthn];
      tmp+= row[_sel_columns._lengthn];
      rpar[_sel_columns._lengthn]= tmp;
      rpar[_sel_columns._length]= format_value(tmp);
    }
    else
    {
      // table was unchecked
      row[_sel_columns._state]= MGCellRendererTristate::OFF;

      // update totals
      tmp= rpar[_sel_columns._rows];
      tmp-= row[_sel_columns._rows];
      rpar[_sel_columns._rows]= tmp;

      tmp= rpar[_sel_columns._lengthn];
      tmp-= row[_sel_columns._lengthn];
      rpar[_sel_columns._lengthn]= tmp;
      rpar[_sel_columns._length]= format_value(tmp);
    }
    
    int total= 0, total_checked= 0;
    Gtk::TreeIter it;
    // update parent row according to the state of the children
    for (it= rpar.children().begin(); 
         it != rpar.children().end() && (total == total_checked || total_checked == 0);
         ++it)
    {
      Gtk::TreeModel::Row r= *it;
      total++;
      if (r[_sel_columns._state] == MGCellRendererTristate::ON)
        total_checked++;
    }
    
    if (total == total_checked)
      rpar[_sel_columns._state]= MGCellRendererTristate::ON;
    else if (total_checked == 0)
      rpar[_sel_columns._state]= MGCellRendererTristate::OFF;
    else
      rpar[_sel_columns._state]= MGCellRendererTristate::MAYBE;
  }
  
  //update_buttonbar_sensitivity(); //set dirty does that
  update_backup_all_state();
  set_dirty();
}


void MABackupPanel::set_dirty()
{
  _dirty= true;
  
  update_buttonbar_sensitivity();
}

int MABackupPanel::backup_progress_cb(const char *table,
                                      int num_tables_total,
                                      int num_tables,
                                      int num_rows_total,
                                      int num_rows,
                                      void *user_data)
{
  ProgressData *data= (ProgressData*)user_data;

  if (!data->ready)
  {
    data->table= table;
    data->num_tables_total= num_tables_total;
    data->num_tables= num_tables;
    data->num_rows_total= num_rows_total;
    data->num_rows= num_rows;
    data->ready= true;
  }
  return backup_aborted ? -1 : 0;
}


bool MABackupPanel::update_backup_progress(void *data)
{
  ProgressData *p= (ProgressData*)data;

  if (p->ready)
  {
    _progress_label->set_text(ufmt(_("Backing UP Table %s"), p->table.c_str()));

    if (p->num_rows_total > 0)
    {
      _row_progress->set_fraction((double)p->num_rows/p->num_rows_total);
      _row_progress->set_text(ufmt(_("row %i of %i"), p->num_rows, p->num_rows_total));
    }
    else
    {
      _row_progress->set_fraction(0.0);
      _row_progress->set_text(ufmt(_("row %i of %i"), 0, 0));
    }

    if (p->num_tables_total > 0)
    {
      _table_progress->set_fraction((double)p->num_tables/p->num_tables_total);
      _table_progress->set_text(ufmt(_("table %i of %i"), p->num_tables, p->num_tables_total));
    }
    else
    {
      _table_progress->set_fraction(0.0);
      _table_progress->set_text(ufmt(_("table %i of %i"), 0, 0));
    }
    p->ready= false;
  }

  return true;
}


void *MABackupPanel::backup_thread(void *data)
{
  BackupThreadArgument *arg;
  MYX_BACKUP_ERROR err;
  MInstanceInfo *inst;

  mysql_thread_init();

  arg= (BackupThreadArgument*)data;
  inst= arg->me->_data->get_instance();

  inst->lock_mysql(true);

  err= myx_make_backup_with_profile(inst->get_mysql(), arg->profile, 
                                    arg->path.c_str(),
                                    1, backup_progress_cb, &arg->pdata);
  inst->lock_mysql(false);

  mysql_thread_end();
  
  return (void*)err;
}


void MABackupPanel::stop_backup()
{
  backup_aborted= true;
}


void MABackupPanel::perform_backup()
{
  BackupThreadArgument arg;
  MDispatcher disp;
  char suffix[128];
  time_t now= time(NULL);
  Gtk::Dialog *dlg= (Gtk::Dialog*)_progress_xml->get_widget("progress_dialog");
  MYX_BACKUP_ERROR err;

  _progress_label->set_text(_("Preparing..."));
  _table_progress->set_fraction(0);
  _row_progress->set_fraction(0);

  if (!prefs.disable_backup_timestamps)
    strftime(suffix, sizeof(suffix), "_%Y%m%d_%H%M.sql", localtime(&now));
  else
    suffix[0]= 0;

  arg.me= this;
  {
    Gtk::FileSelection fsel(_("Select Backup File Name"));
    fsel.set_filename(_profile_name+std::string(suffix));

    if (fsel.run() != Gtk::RESPONSE_OK)
      return;

    arg.path= fsel.get_filename();
  }
  _app->push_status(_("Performing backup..."));

  arg.profile= gather_profile();
  arg.pdata.ready= false;

  backup_aborted= false;

  if (_inst->check_connection())
  {
    SigC::Connection con= Glib::signal_timeout().connect(sigc::bind<void*>
                                                         (sigc::mem_fun(*this, &MABackupPanel::update_backup_progress),&arg.pdata),
                                                         50);
    dlg->show();

    err= (MYX_BACKUP_ERROR)(long)disp.dispatch(backup_thread, &arg);

    dlg->hide();

    con.disconnect();
  }
  else
    err= MYX_BACKUP_SERVER_ERROR;

  myx_free_profile(arg.profile);
  
  _app->pop_status();

  Glib::ustring msg= get_backup_error_text(_data->get_instance()->get_mysql(), err);

  if (msg.empty())
  {
    if (backup_aborted)
      _app->set_status(_("Backup cancelled."));
    else
      _app->set_status(_("Backup finished."));
    
    Gtk::MessageDialog dlg(*_app->window(),
                           ufmt(_("Backup written to file '%s'."),
                                arg.path.c_str()),
                           false,
                           Gtk::MESSAGE_INFO,
                           Gtk::BUTTONS_OK,
                           true);
    dlg.run();
  } 
  else
  {
    Gtk::MessageDialog dlg(*_app->window(), _("<b>Error during Backup</b>\n")+msg,
                           true,
                           Gtk::MESSAGE_ERROR,
                           Gtk::BUTTONS_OK,
                           true);
    dlg.run();
  }
}


void MABackupPanel::schedule_type_changed()
{
  int i= static_cast<Gtk::OptionMenu*>(get_widget("schedule_menu"))->get_history();

  ((Gtk::Notebook*)get_widget("schedule_note"))->set_current_page(i);
  get_widget("schedule_frame")->set_sensitive(i > 0);
}


bool MABackupPanel::gather_schedule(MCrontab::Entry &schedule)
{
  extern std::string get_argv0(); // from main.cc
  extern std::string get_startup_dir(); // from main.cc

  switch (static_cast<Gtk::OptionMenu*>(get_widget("schedule_menu"))->get_history())
  {
  case 0:
    return false;
  case 1:
    schedule.hour= get_spin("dailyh_spin")->get_value_as_int();
    schedule.minute= get_spin("dailym_spin")->get_value_as_int();
    break;
  case 2:
    schedule.hour= get_spin("weekh_spin")->get_value_as_int();
    schedule.minute= get_spin("weekm_spin")->get_value_as_int();
    schedule.weekday= 0;
    for (int i= 1; i <= 7; i++)
    {
      if (get_toggle("week"+tostr(i)+"_check")->get_active())
        schedule.weekday|= (1<<(i-1));
    }
    break;
  case 3:
    schedule.hour= get_spin("monthh_spin")->get_value_as_int();
    schedule.minute= get_spin("monthm_spin")->get_value_as_int();
    schedule.day= get_entry("monthday_entry")->get_text();
    break;
  }


  std::string target= get_entry("target_entry")->get_text();
  std::string prefix= get_entry("prefix_entry")->get_text();
  
  if (_profile_name.empty())
  {
    myg_show_error(*_app->window(), _("Please select a profile first."));
    return false;
  }

  std::string command= Glib::path_get_dirname(get_argv0());

  command= Glib::build_filename(command, "mabackup");
  if (!Glib::path_is_absolute(command))
  {    
    command = get_startup_dir() + "/" + command;
  }
  schedule.command= command;

  if (!target.empty())
    schedule.command+= " -d "+target;
  if (!prefix.empty())
    schedule.command+= " -x "+prefix;

  Gtk::Combo *combo= static_cast<Gtk::Combo*>(get_widget("connection_combo"));
  if (!combo->get_entry()->get_text().empty())
    schedule.command+= " -c "+combo->get_entry()->get_text();

  schedule.command+= " "+_profile_name;
  
  return true;
}


void MABackupPanel::install_schedule()
{
  MCrontab::Entry schedule;

  if (gather_schedule(schedule))
  {
    MCrontab crontab;
    if (crontab.load())
    {      
      crontab.remove_entry_with_comment("["+_profile_name+"]");

      crontab.add_entry(schedule,
                        ufmt("MySQL Administrator Backup [%s] (do not change this line)",
                             _profile_name.c_str()));

      if (!crontab.install())
      {
        myg_show_error(*_app->window(),
                       _("Could not install new crontab."));
      }

      show_schedule();
    }
    else
    {
      myg_show_error(*_app->window(),
                     _("Could not retrieve list of currently scheduled tasks by crontab"));
    }
  }
}


void MABackupPanel::uninstall_schedule(bool refresh_screen)
{
  MCrontab crontab;
  if (crontab.load())
  {
    std::string profile;
  
    _file_browser->get_row_object(_file_browser->get_selected(), profile);

    crontab.remove_entry_with_comment("["+profile+"]");
    
    if (!crontab.install())
    {
      myg_show_error(*_app->window(),
                     _("Could not install new crontab without the scheduled backup."));
    }

    if (refresh_screen)
      show_schedule();
  }
  else
  {
    myg_show_error(*_app->window(),
                   _("Could not retrieve list of currently scheduled tasks by crontab"));
  }
}


void MABackupPanel::show_schedule()
{
  MCrontab crontab;
  std::string profile;
  MCrontab::Entry schedule;
  
  _file_browser->get_row_object(_file_browser->get_selected(), profile);
  
  if (!crontab.load())
  {
    myg_show_error(*_app->window(),
                   _("Could not retrieve list of currently scheduled tasks by crontab"));
    return;
  }

  // dont change the following comment or it will not work with old scheduled backups!
  if (!crontab.find_entry_by_comment(ufmt("MySQL Administrator Backup [%s] (do not change this line)",
                                          profile.c_str()),
                                     schedule))
  {
    show_schedule(schedule);
  }
  else
  {
    show_schedule(schedule);
  }
  
  int count= 0;
  std::list<std::string> list= crontab.get_lines();
  // count other schedules for other profiles
  for (std::list<std::string>::const_iterator iter= list.begin();
       iter!= list.end(); ++iter)
  {
    MCrontab::Entry entry;
    
    if (iter->empty())
      continue;
    if (iter->find("MySQL Administrator Backup")!=std::string::npos)
    {
      if (iter->find("["+profile+"]")==std::string::npos)
        count++;
    }
  }
  
  if (count > 0)
    get_label("extra_label")->set_text(ufmt(_("Besides this, there are %i backup projects currently scheduled for the current user."),
                                            count));
  else
    get_label("extra_label")->set_text("");
}


void MABackupPanel::show_schedule(const MCrontab::Entry &schedule)
{
  // we can only interpret what we write, if the user changes
  // the schedule to something other than we would ourselves,
  // then we can't handle it...

  Glib::ustring conn_name;

  get_entry("target_entry")->set_text("");
  get_entry("prefix_entry")->set_text("");

  char **tokens= g_strsplit(schedule.command.c_str(), " ", -1);
  if (tokens)
  {
    for (int i=0; tokens[i]; i++)
    {
      if (strcmp(tokens[i], "-d")==0 && tokens[i+1])
      {
        get_entry("target_entry")->set_text(tokens[i+1]);
        i++;
      }
      else if (strcmp(tokens[i], "-x")==0 && tokens[i+1])
      {
        get_entry("prefix_entry")->set_text(tokens[i+1]);
        i++;
      }
      else if (strcmp(tokens[i], "-c")==0 && tokens[i+1])
      {
        conn_name= tokens[i+1];
        i++;
      }
    }
  }
  // fill list of connection profiles
  {
    Gtk::Combo *combo= static_cast<Gtk::Combo*>(get_widget("connection_combo"));
  
    MYX_LIB_ERROR merror;
    MYX_USER_CONNECTIONS *connl;

    // try to connect using passed named connection
    connl= myx_load_user_connections(prefs.build_path_to(prefs.connections_filename).c_str(),
                                     &merror);
    if (connl)
    {
      std::list<Glib::ustring> l;
      
      for (unsigned int i= 0; i < connl->user_connections_num; i++)
      {
        if (connl->user_connections[i].connection_name)
          l.push_back(connl->user_connections[i].connection_name);
      }
      combo->set_popdown_strings(l);
      
      myx_free_user_connections(connl);
      
    }

    if (!conn_name.empty())
      combo->get_entry()->set_text(conn_name);
    else
      combo->get_entry()->set_text(_inst->get_connection_data().connection_name);
  }

  get_button("install_button")->set_label(_("Update Schedule"));
  get_button("uninstall_button")->set_sensitive(true);
  get_label("state_label")->set_markup(_("Backups Are Currently <b>Scheduled</b>"));
  if (!schedule.day.empty())
  {
    get_spin("monthh_spin")->set_value(schedule.hour);
    get_spin("monthm_spin")->set_value(schedule.minute);
    get_entry("monthday_entry")->set_text(schedule.day);
    static_cast<Gtk::OptionMenu*>(get_widget("schedule_menu"))->set_history(3);
  }
  else if (schedule.weekday)
  {
    get_spin("weekh_spin")->set_value(schedule.hour);
    get_spin("weekm_spin")->set_value(schedule.minute);
    for (int i= 0; i < 7; i++)
    {
      if (schedule.weekday & (1<<i))
        get_toggle("week"+tostr(i+1)+"_check")->set_active(true);
      else
        get_toggle("week"+tostr(i+1)+"_check")->set_active(false);
    }
    static_cast<Gtk::OptionMenu*>(get_widget("schedule_menu"))->set_history(2);
  }
  else if (schedule.hour >= 0 && schedule.minute >= 0)
  {
    get_spin("dailyh_spin")->set_value(schedule.hour);
    get_spin("dailym_spin")->set_value(schedule.minute);
    static_cast<Gtk::OptionMenu*>(get_widget("schedule_menu"))->set_history(1);
  }
  else
  {
    get_label("state_label")->set_markup(_("Backups Are Currently <b>Not Scheduled</b>"));
    static_cast<Gtk::OptionMenu*>(get_widget("schedule_menu"))->set_history(0);
    get_button("uninstall_button")->set_sensitive(false);
    get_button("install_button")->set_label(_("Schedule Backup"));
  }
}



/**********************************************************************/

void MABackupPanel::schema_selected(MGBrowserList *sender,
                                    const Gtk::TreeIter &node)
{
  _selection_tree->get_selection()->unselect_all();

  update_button_sensitivity();
}


void MABackupPanel::profile_selected(MGBrowserList *sender,
                                     const Gtk::TreeIter &node)
{
  MYX_BACKUP_PROFILE *profile;
  MYX_ADMIN_LIB_ERROR err;
  std::string name;

  if (node)
  {
    std::string profile_dir= _file_browser->get_directory()+"/";

    _file_browser->get_row_object(node, name);
    name+= PROFILE_SUFFIX;
    
    profile= myx_load_profile(name.c_str(), profile_dir.c_str(), &err);
    if (profile)
    {
      show_profile(profile);

      myx_free_profile(profile);
    }
    else
    {
      show_adminlib_error(*_app->window(),
                          ufmt(_("Could not load backup profile '%s/%s'"),
                               profile_dir.c_str(), name.c_str()),
                          err);
    }
  }
  else
  {
    show_profile(0);
  }
}


Glib::ustring MABackupPanel::get_backup_error_text(MYSQL *mysql, MYX_BACKUP_ERROR err)
{ 
  Glib::ustring msg;
  
  switch (err)
  {
  case MYX_BACKUP_SERVER_ERROR:
    //msg= _("MySQL error during backup.");
    msg= myx_mysql_error(mysql);
    break;
  case MYX_BACKUP_CANT_OPEN_FILE:
    msg= _("Could not open file for writing backup.");
    break;
  case MYX_BACKUP_ILLEGAL_OPTION:
    msg= _("Illegal option.");
    break;
  case MYX_BACKUP_PCRE_ERROR:
    msg= _("Internal error in PCRE call.");
    break;
  case MYX_BACKUP_MALLOC_FAILED:
    msg= _("Memory allocation failed during backup.");
    break;
  case MYX_BACKUP_OUTPUTDEVICE_FULL:
    msg= _("Out of disk space during backup write.");
    break;
  case MYX_BACKUP_CANNOT_FLUSH_TABLES_WITH_READ_LOCK:
    msg= _("Cannot flush tables with read lock.");
    break;
  case MYX_BACKUP_CANNOT_START_TRANSACTION:
    msg= _("Cannot start transaction.");
    break;
  case MYX_BACKUP_CANNOT_SET_ANSI_QUOTES:
    msg= _("Cannot set ANSI quotes.");
    break;
  case MYX_BACKUP_CANT_READ_FROM_FILE:
    msg= _("Cannot read from file.");
    break;
  case MYX_BACKUP_XML_PARSE_ERROR:
    msg= _("Error parsing XML file.");
    break;
  case MYX_BACKUP_SQL_ERROR:
    msg= _("SQL error");
    break;
  case MYX_BACKUP_STOPPED:
    msg= _("Stopped by user.");
    break;
  case MYX_BACKUP_CHARSET_CONVERSION:
    msg= _("Character set conversion error.");
    break;
  case MYX_BACKUP_WRONG_CHARSET:
    msg= _("Wrong character set.");
    break;
  case MYX_BACKUP_UNKNOWN:
    msg= _("Unknown error.");
    break;
  case MYX_BACKUP_NO_ERROR:
    break;
  }
  return msg;
}


    
bool MABackupPanel::execute_backup(MYSQL *mysql,
                                   const std::string &profile,
                                   const std::string &path,
                                   const std::string &prefix)
{
  MYX_BACKUP_PROFILE *prof;
  MYX_ADMIN_LIB_ERROR err;
  std::string profile_dir= prefs.build_path_to(prefs.backup_profiles_directory)+"/";
  std::string backup_path;
  MYX_BACKUP_ERROR bkerr;
  
  if (!prefs.disable_backup_timestamps)
  {
    // determine name of the file to write
    char suffix[128];
    time_t now= time(NULL);
    strftime(suffix, sizeof(suffix), "_%F_%T.sql", localtime(&now));
    if (prefix.empty())
      backup_path= profile+std::string(suffix);
    else
      backup_path= prefix+std::string(suffix);
  }
  else
  {
    if (prefix.empty())
      backup_path= profile;
    else
      backup_path= prefix;
  }
  
  if (!path.empty())
  {
    if (path[0]!='/')
    {
      myg_log("[%i]: ERROR: Backup directory path cannot be relative: %s",
              getpid(), path.c_str());
      return false;
    }

    if (path[path.length()-1]=='/')
      backup_path= path+backup_path;
    else
      backup_path= path+"/"+backup_path;
  }

  std::string profile_file= profile+".mbp";

  // load profile
  prof= myx_load_profile(profile_file.c_str(), profile_dir.c_str(), &err);
  if (!prof)
  {
    myg_log("ERROR: could not load backup profile '%s/%s'",
            profile_dir.c_str(), profile_file.c_str());
    return false;
  }

  // execute backup
  myg_log("Executing backup profile '%s'...", profile.c_str());
  bkerr= myx_make_backup_with_profile(mysql, prof, backup_path.c_str(),
                                      0, NULL, NULL);
  if (bkerr != MYX_BACKUP_NO_ERROR)
  {
    char *tmp= NULL;
    //myg_log("ERROR: error while performing backup: %s%s",
    //        get_backup_error_text(mysql, bkerr).c_str(),
    //        bkerr != MYX_BACKUP_MYSQL_ERROR 
    //        ? "" : ufmt(" %s", tmp=myx_mysql_error(mysql)).c_str());
    myg_log("ERROR: error while performing backup: %s",
            get_backup_error_text(mysql, bkerr).c_str());
    g_free(tmp);
  }
  else
    myg_log("Wrote backup to file '%s'", backup_path.c_str());
  myx_free_profile(prof);

  return true;
}



/**********************************************************************/

MAPanel *create_backup_panel(MAdministrator *app, MDataInterface *data)
{
  return new MABackupPanel(app, data);
}

