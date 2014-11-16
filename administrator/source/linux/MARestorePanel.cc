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
#include "MABackupPanel.h"
#include "MARestorePanel.h"
#include "MDataInterface.h"
#include "MInstanceInfo.h"
#include "MGCharsetPickDialog.h"

#include "myg_utils.h"
#include "myg_gtkutils.h"
#include "MGFileBrowserList.h"


static bool op_aborted= false;


class ProgressData {
 public:
  ProgressData() { pthread_mutex_init(&mutex, NULL); };

  bigint bytes;
  bigint bytes_total;
  bool ready;
  time_t start_time;

  pthread_mutex_t mutex;
  std::list<Glib::ustring> warnings;
};


struct LoadThreadArgument {
  MARestorePanel *me;
  std::string path;
  std::string charset;
  ProgressData pdata;
  MYX_BACKUP_ERROR error;
};


struct RestoreThreadArgument {
  MARestorePanel *me;
  std::string path;
  std::string charset;
  Glib::ustring target_catalog;
  Glib::ustring target_schema;
  int options;
  MYX_BACKUP_CONTENT *content;
  ProgressData pdata;
};



MARestorePanel::MARestorePanel(MAdministrator *app, MDataInterface *data)
    : MAPanel(app, data), _browser(0), _cur_content(0)
{
}


MARestorePanel::~MARestorePanel()
{
  delete _browser;
  delete _progress_xml;
  
  if (_cur_content)
  {
    myx_free_backup_content(_cur_content);
  }
}


void MARestorePanel::setup_selection_list()
{
  Gtk::TreeView *tree= (Gtk::TreeView*)get_widget("selection_tree");
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
  
  // setup list columns

  _selection_tree= tree;
  
  _selection_store= Gtk::TreeStore::create(_sel_columns);
  tree->set_model(_selection_store);

  MGCellRendererTristate *trend= new MGCellRendererTristate(image_list);
  column= new Gtk::TreeView::Column(_("Objects"), *Gtk::manage(trend));
  tree->append_column(*Gtk::manage(column));
  column->add_attribute(trend->property_state(), _sel_columns._state);
  trend->signal_toggled().connect(sigc::mem_fun(*this,&MARestorePanel::row_toggled));

  column= tree->get_column(0);
  column->pack_start(_sel_columns._icon, false);
  column->pack_start(_sel_columns._object);
  
  tree->append_column("", _sel_columns._type);
//  tree->append_column(_("Table type"), _sel_columns._type);
//  tree->append_column(_("Rows"), _sel_columns._rows);

  Glib::RefPtr<Gtk::TreeSelection> sel= tree->get_selection();

//  sel->signal_changed().connect(sigc::mem_fun(*this,&MARestorePanel::selected_object));

}


bool MARestorePanel::init()
{
  if (_xml)
    return true;

  if (!MAPanel::init_from_glade(get_glade_file(GLADE_RESTORE_FILE), "panel_frame"))
    return false;

  _progress_xml= new MGGladeXML(get_glade_file(GLADE_RESTORE_FILE), "progress_dialog");

  ((Gtk::Button*)_progress_xml->get_widget("stop_button"))->signal_clicked().connect(sigc::mem_fun(*this,&MARestorePanel::abort_operation));
  ((Gtk::Button*)_progress_xml->get_widget("ok_button"))->signal_clicked().connect(sigc::mem_fun(*this,&MARestorePanel::close_progress));

  _progress= (Gtk::ProgressBar*)_progress_xml->get_widget("progressbar");
  _warning_buffer= _progress_xml->get_text("warning_text")->get_buffer();

  setup_selection_list();
  
  _schema_icon= PIXCACHE->load("16x16_Database.png");
  _sys_schema_icon= _schema_icon;

  _table_icon= PIXCACHE->load("16x16_Table.png");
  _column_icon= PIXCACHE->load("16x16_Field.png");
  _view_icon= PIXCACHE->load("16x16_View.png");
  _proc_icon= PIXCACHE->load("16x16_StoredProc.png");

  _browser= new MGFileBrowserList(_("Backup File to Restore"),
                                  prefs.last_backup_directory,
                                  true);
  _browser->set_extension(".sql");

  _browser->signal_selected().connect(sigc::mem_fun(*this,&MARestorePanel::file_selected));

  get_button("restore_button")->signal_clicked().connect(sigc::mem_fun(*this,&MARestorePanel::perform_restore));

  _panel->set_sensitive(false);
 
  _data->signal_catalogs_refreshed().connect(sigc::mem_fun(*this,&MARestorePanel::schemata_reloaded));
  
  _schema_store= Gtk::ListStore::create(_target_columns);
  _xml->get_combo("target_pop")->set_model(_schema_store);
  _xml->get_combo("target_pop")->pack_start(_target_columns.name);
  _xml->get_combo("target_pop")->signal_changed().connect(sigc::mem_fun(*this,&MARestorePanel::target_changed));
  
  _old_schema_index= 0;

  return true;
}


void MARestorePanel::schemata_reloaded(const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &schemata)
{
  Gtk::TreeRow row;
  MYX_CATALOGS *cats= schemata->ptr();

  _schema_store->clear();

  row= *_schema_store->append();
  row[_target_columns.name]= _("Original Schema");
  row[_target_columns.created]= false;

  row= *_schema_store->append();
  row[_target_columns.name]= _("New Schema...");
  row[_target_columns.created]= false;

  for (int i= 0; i < cats->catalogs_num; i++)
  {
    MYX_CATALOG *cat= cats->catalogs+i;

    for (int j= 0; j < cat->schemata_num; j++)
    {
      row= *_schema_store->append();
      row[_target_columns.name]= cat->schemata[j].schema_name;
      row[_target_columns.created]= false;
    }
  }

  _xml->get_combo("target_pop")->set_active(0);
}


void MARestorePanel::target_changed()
{
  if (_xml->get_combo("target_pop")->get_active_row_number()==1)
  {
    Glib::ustring name;
    Gtk::TreeRow row;
    int i;
    
    if (!myg_ask_string(_("Restore in New Schema"), _("Enter a name for the schema where the backup should be restored."),
                       name))
    {
      _xml->get_combo("target_pop")->set_active(_old_schema_index);
      return;
    }

    i= 0;
    for (Gtk::TreeIter iter= _schema_store->children().begin();
         iter != _schema_store->children().end(); ++iter, ++i)
    {
      row= *iter;
      if (row[_target_columns.name] == name)
      {
        _old_schema_index= i;
        _xml->get_combo("target_pop")->set_active(i);
        return;
      }
    }

    row= *_schema_store->append();
    row[_target_columns.name]= name;
    row[_target_columns.created]= true;
    _old_schema_index= i;
    _xml->get_combo("target_pop")->set_active(i);
  }
  _old_schema_index= _xml->get_combo("target_pop")->get_active_row_number();
}


void MARestorePanel::show_content_info(const std::string &file,
                                       const std::string &charset,
                                       MYX_BACKUP_CONTENT *content)
{
  int schema_count= 0;

  _selection_store->clear();

  for (unsigned int i= 0; i < content->tables_num; i++)
  {
    MYX_BACKUP_TABLE *table= content->tables+i;
    Gtk::TreeIter sit, tit;
    Gtk::TreeModel::Row row, trow;
    bool found= false;
    
    // check if this schema is already in the list
    for (sit= _selection_store->children().begin();
         sit != _selection_store->children().end(); ++sit)
    {
      row= *sit;
      if (row[_sel_columns._object] == (char*)table->schema)
      {
        found= true;
        break;
      }
    }
    if (!found)
    {
      schema_count++;
      sit= _selection_store->append();
      row= *sit;
      row[_sel_columns._icon]= strcmp(table->catalog,"sys")==0?_sys_schema_icon:_schema_icon;
      row[_sel_columns._state]= MGCellRendererTristate::ON;
      row[_sel_columns._object]= (char*)table->schema;
//      row[_sel_columns._type]= "";//XXX
//      row[_sel_columns._rows]= 0;
      row[_sel_columns._what]= SelectionColumns::CSchema;
    }
    
    tit= _selection_store->append(row.children());
    trow= *tit;

    if(table->flags & (MYX_BTF_IS_PROCEDURE|MYX_BTF_IS_FUNCTION))
    {
      trow[_sel_columns._icon]= _proc_icon;
    }
    else if(table->flags & MYX_BTF_IS_VIEW)
    {
      trow[_sel_columns._icon]= _view_icon;
    }
    else
    {
      trow[_sel_columns._icon]= _table_icon;
    }
    
    trow[_sel_columns._state]= MGCellRendererTristate::ON;
    trow[_sel_columns._object]= (char*)table->table;

//    trow[_sel_columns._type]= "";//XXX
//    trow[_sel_columns._rows]= 0;
    trow[_sel_columns._what]= SelectionColumns::CTable;
  }

  get_label("info_label")->set_text(ufmt("%s\n%s\n%i\n%i\n%sB",
                                         file.c_str(),
                                         charset.c_str(),
                                         schema_count,
                                         content->tables_num,
                                         format_value(get_file_size(file.c_str())).c_str()));



  _xml->get_combo("target_pop")->set_active(0);
  
  _data->refresh_catalogs();
}


void MARestorePanel::row_toggled(const Glib::ustring &path)
{
  Gtk::TreeIter iter= _selection_store->get_iter(path);
  Gtk::TreeModel::Row row= *iter;
  
  // toggled the schema
  if (row[_sel_columns._what] == SelectionColumns::CSchema)
  {
    Gtk::TreeModel::Children ch= row->children();

    if (row[_sel_columns._state] != MGCellRendererTristate::ON)
    {
//      unsigned long total_rows= 0, total_length= 0;
      // schema was checked: check all tables
      for (Gtk::TreeIter it= ch.begin(); it != ch.end(); ++it)
      {
        Gtk::TreeModel::Row r= *it;
        
        r[_sel_columns._state]= MGCellRendererTristate::ON;

///        total_rows+= r[_sel_columns._rows];
//        total_length+= r[_sel_columns._lengthn];
      }
      row[_sel_columns._state]= MGCellRendererTristate::ON;
//      row[_sel_columns._rows]= total_rows;
//      row[_sel_columns._lengthn]= total_length;
//      row[_sel_columns._length]= format_value(total_length);
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
//      row[_sel_columns._rows]= 0;
//      row[_sel_columns._lengthn]= 0;
//      row[_sel_columns._length]= "0";
    }
  }
  else
  {
    Gtk::TreeIter par= row.parent();
    Gtk::TreeModel::Row rpar= *par;
//    unsigned long tmp;

    if (row[_sel_columns._state] != MGCellRendererTristate::ON)
    {
      // table was checked
      row[_sel_columns._state]= MGCellRendererTristate::ON;

      // update totals
//      tmp= rpar[_sel_columns._rows];
//      tmp+= row[_sel_columns._rows];
//      rpar[_sel_columns._rows]= tmp;

//      tmp= rpar[_sel_columns._lengthn];
//      tmp+= row[_sel_columns._lengthn];
//      rpar[_sel_columns._lengthn]= tmp;
//      rpar[_sel_columns._length]= format_value(tmp);
    }
    else
    {
      // table was unchecked
      row[_sel_columns._state]= MGCellRendererTristate::OFF;

      // update totals
//      tmp= rpar[_sel_columns._rows];
//      tmp-= row[_sel_columns._rows];
//      rpar[_sel_columns._rows]= tmp;

//      tmp= rpar[_sel_columns._lengthn];
//      tmp-= row[_sel_columns._lengthn];
//      rpar[_sel_columns._lengthn]= tmp;
//      rpar[_sel_columns._length]= format_value(tmp);
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
}

void MARestorePanel::file_selected(MGBrowserList *sender, const Gtk::TreeIter &node)
{  
  if (node)
  {
    std::string item;
    std::string path;
    std::string charset;
    MYX_BACKUP_ERROR error;

    _browser->get_row_object(node, item);

    path= _browser->get_path_for_entry(item);

    if (_cur_content)
    {
      myx_free_backup_content(_cur_content);
      _cur_content= 0;
    }

    Glib::ustring tmp;
    {
      MGCharsetPickDialog dialog;

      charset= dialog.show(_("Pick the character set used in the selected file."));
      if (charset.empty())
      {
        _browser->unselect();
        return;
      }
      dialog.hide();
    }

    _app->push_status(_("Processing backup file..."));
    _cur_content= perform_load(path, charset, error);
    _cur_content_charset= charset;
    
    if (!_cur_content)
    {
      if (!op_aborted)
      {
        if (error != MYX_BACKUP_FILE_IS_NOT_MA_DUMP)
        {
          char *ptr= myx_get_backup_error_string(error);
          Gtk::MessageDialog dlg(*_app->window(),
                                 ufmt(_("<b>Error Reading Backup File</b>\n\nFile: %s\nError: %s"),
                                      path.c_str(),
                                      ptr),
                                 true,
                                 Gtk::MESSAGE_ERROR,
                                 Gtk::BUTTONS_OK,
                                 true);
          g_free(ptr);
          dlg.run();
        }
        else
        {
          Gtk::MessageDialog dlg(*_app->window(),
                                 ufmt(_("<b>Unrecognized Backup File</b>\n\nFile: %s\nThe selected file was not recognized as a MySQL Administrator generated backup. If the file was generated with mysqldump, you may restore it by feeding it to the MySQL command line client."),
                                      path.c_str()),
                                 true,
                                 Gtk::MESSAGE_ERROR,
                                 Gtk::BUTTONS_OK,
                                 true);
          dlg.run();
        }
      }
      _app->pop_status();
      _app->set_status(_("Backup file processing cancelled."));
      _panel->set_sensitive(false);
      _browser->unselect();
    }
    else
    {
      _app->pop_status();
      _app->set_status(_("Backup file processed."));
      show_content_info(path, charset, _cur_content);
      _panel->set_sensitive(true);
    }
  }
  else
  {
    _panel->set_sensitive(false);
  }
}


MYX_BACKUP_CONTENT *MARestorePanel::gather_selected_tables()
{
  Gtk::TreeIter siter, titer;
  int alloced= 8, count= 0;
  MYX_BACKUP_CONTENT *content;
  MYX_BACKUP_TABLE *tables= (MYX_BACKUP_TABLE*)g_malloc0(alloced*sizeof(MYX_BACKUP_TABLE));

  for (siter= _selection_store->children().begin();
       siter != _selection_store->children().end();
       ++siter)
  {
    Gtk::TreeModel::Row row= *siter;

    for (titer= row.children().begin(); titer != row.children().end(); ++titer)
    {
      Gtk::TreeModel::Row rrow= *titer;
      Glib::ustring str;

      if (rrow[_sel_columns._state] == MGCellRendererTristate::ON)
      {
        if (count == alloced)
        {
          alloced += 10;
          tables= (MYX_BACKUP_TABLE*)g_realloc(tables, alloced*sizeof(MYX_BACKUP_TABLE));
        }

        tables[count].catalog= g_strdup("def");//XXX
        str= row[_sel_columns._object];
        tables[count].schema= g_strdup(str.c_str());
        str= rrow[_sel_columns._object];
        tables[count].table= g_strdup(str.c_str());
        count++;
      }
    }
  }

  content= (MYX_BACKUP_CONTENT*)g_malloc0(sizeof(MYX_BACKUP_CONTENT));
  content->tables_num= count;
  content->tables= tables;

  return content;
}


bool MARestorePanel::update_status(void *data)
{
  ProgressData *ldata= (ProgressData*)data;

  if (ldata->ready)
  {
    double d = (double)ldata->bytes/(double)ldata->bytes_total;
    if(d < 0.01)	// workaround for a gtkmm (2.2.12) bug (?)
      // if 0.004 <= d < 0.007 it displays progress fraction == 100%
    {
      d = 0.01;
    }
    _progress->set_fraction(d);
    _progress->set_text(ufmt(_("%'lli of %'lli bytes"), ldata->bytes, ldata->bytes_total));
    ldata->ready= false;
    
    bool flag= false;
    pthread_mutex_lock(&ldata->mutex);
    while (!ldata->warnings.empty())
    {
      Glib::ustring msg= ldata->warnings.front();
      ldata->warnings.pop_front();

      _warning_buffer->insert(_warning_buffer->end(), msg+"\n");
      flag= true;
    }
    pthread_mutex_unlock(&ldata->mutex);

    Glib::ustring t;
    int elapsed= time(NULL) - ldata->start_time;

    t= ufmt("%i:%02i", elapsed/60, elapsed%60);
    _progress_xml->get_label("elaps_time")->set_text(t);

    if (ldata->bytes > 0)
    {
      int eta= (int)(((double)elapsed * (double)ldata->bytes_total) / (double)ldata->bytes - elapsed);

      t= ufmt("%i:%02i", eta/60, eta%60);
      
      if (eta <= 10)
        _progress_xml->get_label("rem_time")->set_text(_("less than 10s"));
      else
        _progress_xml->get_label("rem_time")->set_text(t);
    }
    else
      _progress_xml->get_label("rem_time")->set_text("--:--");

    if (flag)
      _progress_xml->get_widget("warning_expander")->set_sensitive(true);
  }

  return true;
}


void MARestorePanel::abort_operation()
{
  op_aborted= true;
}

void MARestorePanel::close_progress()
{
  ((Gtk::Dialog*)_progress_xml->get_widget("progress_dialog"))->hide();
}

void *MARestorePanel::load_thread(void *data)
{
  LoadThreadArgument *arg;
  MYX_BACKUP_CONTENT *content;
  
  mysql_thread_init();

  arg= (LoadThreadArgument*)data;

  content= myx_get_backup_content(arg->path.c_str(), arg->charset.c_str(),
                                  MYX_BT_SQL_SCRIPT,
                                  1024, // update every 1024 bytes
                                  progress_cb,
                                  restore_warning_cb, &arg->pdata, &arg->error,
                                  0, 1);
  mysql_thread_end();
  return content;
}


void *MARestorePanel::restore_thread(void *data)
{
  RestoreThreadArgument *arg= (RestoreThreadArgument*)data;
  MYX_BACKUP_ERROR err;
  MInstanceInfo *inst= arg->me->_data->get_instance();

  inst->lock_mysql(true);

  err= myx_restore_backup(inst->get_mysql(),
                          arg->path.c_str(), arg->charset.c_str(),
                          arg->content,
                          arg->target_catalog.empty()?NULL:arg->target_catalog.c_str(),
                          arg->target_schema.empty()?NULL:arg->target_schema.c_str(),
                          MYX_BT_SQL_SCRIPT,
                          arg->options,
                          1024,
                          progress_cb, &arg->pdata,
                          restore_warning_cb, arg->me);

  inst->lock_mysql(false);

  return (void*)err;
}


int MARestorePanel::progress_cb(bigint bytes_read, bigint bytes_total, void *user_data)
{
  ProgressData *data= (ProgressData*)user_data;

  if (data)
  {
    data->bytes= bytes_read;
    data->bytes_total= bytes_total;
    data->ready= true;
  }

  return op_aborted ? -1 : 0;
}


void MARestorePanel::restore_warning_cb(const char *msg, void *data)
{
  ProgressData *ldata= (ProgressData*)data;
  
  if (ldata)
  {
    pthread_mutex_lock(&ldata->mutex);
    ldata->warnings.push_back(msg);
    pthread_mutex_unlock(&ldata->mutex);
    ldata->ready= true;
  }
}


MYX_BACKUP_CONTENT *MARestorePanel::perform_load(const std::string &file,
                                                 const std::string &charset,
                                                 MYX_BACKUP_ERROR &error)
{
  LoadThreadArgument arg;
  MDispatcher disp;
  Gtk::Dialog *dlg= (Gtk::Dialog*)_progress_xml->get_widget("progress_dialog");
  MYX_BACKUP_CONTENT *content;

  _app->push_status(_("Reading backup file..."));
  ((Gtk::Label*)_progress_xml->get_widget("label"))->set_text(ufmt(_("Reading backup file '%s'..."),
                                 file.c_str()));
  ((Gtk::Label*)_progress_xml->get_widget("stop_button"))->show();
  ((Gtk::Label*)_progress_xml->get_widget("ok_button"))->hide();
  _progress->set_fraction(0);
  _warning_buffer->erase(_warning_buffer->begin(), _warning_buffer->end());
  _progress_xml->get_widget("warning_expander")->set_sensitive(false);
  _progress_xml->get_label("rem_time")->set_text("0:00");
  _progress_xml->get_label("elaps_time")->set_text("0:00");

  arg.me= this;
  arg.path= file;
  arg.charset= charset;
  arg.pdata.ready= false;
  arg.pdata.start_time= time(NULL);

  op_aborted= false;

  {
    sigc::connection con= Glib::signal_timeout().connect(sigc::bind<void*>
                                                         (sigc::mem_fun(*this, &MARestorePanel::update_status),&arg.pdata),
                                                         50);

    dlg->show();

    content= (MYX_BACKUP_CONTENT*)disp.dispatch(load_thread, &arg);

    dlg->hide();

    con.disconnect();
  }

  _app->pop_status();

  if (op_aborted)
    _app->set_status(_("Cancelled."));
  else
    _app->set_status(_("Backup data loaded."));

  get_toggle("option1_check")->set_active(false);
  get_toggle("option2_check")->set_active(true);
  
  error= arg.error;

  return content;
}


bool MARestorePanel::check_tables_to_be_dropped(MYX_BACKUP_CONTENT *content)
{
  MYX_BACKUP_CONTENT *drop_list;

  drop_list= myx_get_restore_drop_list(_data->get_instance()->get_mysql(), content);
  if (drop_list->tables_num==0)
  {
    myx_free_backup_content(drop_list);
    return true;
  }
  
  Glib::ustring msg, text;

  // check for tables that will be restored
  for (unsigned int i= 0; i < drop_list->tables_num; i++)
  {
    if (i > 0)
      msg+=",\n"+ufmt("\t- %s.%s", drop_list->tables[i].schema, drop_list->tables[i].table);
    else
      msg= ufmt("\t- %s.%s", drop_list->tables[i].schema, drop_list->tables[i].table);
  }
  
  myx_free_backup_content(drop_list);
  
  text= _("The following tables in the backup file already exist and will be dropped before restoring from backup.\nWould you like to drop these tables and proceed with restoration?");
  text+= "\n"+msg;
  Gtk::MessageDialog dlg(text, false, Gtk::MESSAGE_WARNING, Gtk::BUTTONS_YES_NO);

  if (dlg.run()==Gtk::RESPONSE_NO)
  {
    _app->set_status(_("Restore cancelled."));
    return false;
  }

  return true;
}


void MARestorePanel::perform_restore()
{
  RestoreThreadArgument arg;
  MDispatcher disp;
  MYX_BACKUP_CONTENT *content= gather_selected_tables();
  Glib::ustring msg;
  
  if (_xml->get_combo("target_pop")->get_active_row_number() > 1
      && !check_tables_to_be_dropped(content))
  {
    myx_free_backup_content(content);
    return;
  }

  // do the restore
  Gtk::Dialog *dlg= (Gtk::Dialog*)_progress_xml->get_widget("progress_dialog");

  _app->push_status(_("Restoring backup..."));
  ((Gtk::Label*)_progress_xml->get_widget("label"))->set_text(_("Restoring backup..."));
  ((Gtk::Label*)_progress_xml->get_widget("stop_button"))->show();
  ((Gtk::Label*)_progress_xml->get_widget("ok_button"))->hide();
  _progress->set_fraction(0);
  _warning_buffer->erase(_warning_buffer->begin(), _warning_buffer->end());
  _progress_xml->get_widget("warning_expander")->set_sensitive(false);
  _progress_xml->get_label("rem_time")->set_text("0:00");
  _progress_xml->get_label("elaps_time")->set_text("0:00");

  arg.me= this;
  _browser->get_row_object(_browser->get_selected(), arg.path);
  arg.path= _browser->get_path_for_entry(arg.path);
  arg.charset= _cur_content_charset;
  arg.content= content;
  arg.pdata.ready= false;
  arg.pdata.start_time= time(NULL);
  
  if (_xml->get_combo("target_pop")->get_active_row_number() <= 1)
  {
    arg.target_catalog= "";
    arg.target_schema= "";
  }
  else
  {
    Gtk::TreeRow row= *_xml->get_combo("target_pop")->get_active();

    arg.target_catalog= "";
    arg.target_schema= row[_target_columns.name];
  }

  arg.options= 0;
  if (get_toggle("option1_check")->get_active())
    arg.options |= MYX_RBS_FORCE;
  if (!get_toggle("option2_check")->get_active())
    arg.options |= MYX_RBS_DONT_CREATE_TARGETS;

  op_aborted= false;

  //{
    MYX_BACKUP_ERROR err;

    sigc::connection con= Glib::signal_timeout().connect(sigc::bind<void*>
                                                         (sigc::mem_fun(*this, &MARestorePanel::update_status),&arg.pdata),
                                                         50);

    dlg->show();

    err= (MYX_BACKUP_ERROR)(long)disp.dispatch(restore_thread, &arg);
    con.disconnect();
  //}

  myx_free_backup_content(arg.content);
  
  _app->pop_status();

  if(err)
  {
    Glib::ustring msg= MABackupPanel::get_backup_error_text(_data->get_instance()->get_mysql(), err);
    Gtk::MessageDialog errdlg(*_app->window(), _("Error during backup.\n")+msg,
                              false,
                              Gtk::MESSAGE_ERROR,
                              Gtk::BUTTONS_OK,
                              true);
    errdlg.run();
    _app->set_status(_("Restore failure."));
    dlg->hide();
  }
  else if (op_aborted)
  {
    _app->set_status(_("Cancelled."));
    dlg->hide();
  }
  else
  {
    _app->set_status(_("Backup restored."));
    ((Gtk::Label*)_progress_xml->get_widget("label"))->set_text(_("Backup restoration finished."));
    _progress->set_fraction(1);
    ((Gtk::Label*)_progress_xml->get_widget("stop_button"))->hide();
    ((Gtk::Label*)_progress_xml->get_widget("ok_button"))->show();
  }
}



bool MARestorePanel::before_show()
{
  _app->add_side_panel(_browser);
  
  _browser->refresh();

  return true;
}


bool MARestorePanel::before_hide()
{
  _app->remove_side_panel(_browser);
  
  return true;
}


bool MARestorePanel::before_quit()
{
  if (_browser)
    prefs.last_backup_directory= _browser->get_directory();
  
  return true;
}



MAPanel *create_restore_panel(MAdministrator *app, MDataInterface *data)
{
  return new MARestorePanel(app, data);
}
