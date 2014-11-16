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

#define _XOPEN_SOURCE /* glibc2 needs this */
#include <time.h>

#include "myadmin.h"
#include "MAdministrator.h"
#include "MAServerLogsPanel.h"
#include "MDataInterface.h"
#include <errno.h>
#include <math.h>
#include <unistd.h>
#include "myg_utils.h"
#include "myg_gtkutils.h"
#include "myx_admin_public_interface.h"


#define PAGE_BLOCK_SIZE (1024*5)


class MGFindDialog : public Gtk::Window {
    Gtk::VBox vbox;
    Gtk::VBox cbox;
    Gtk::HBox bbox;
    Gtk::Entry entry;
    Gtk::Button find_button;
    Gtk::Button cancel_button;
    Gtk::CheckButton case_check;
    Gtk::Label status;
    Glib::RefPtr<Gtk::SizeGroup> sgroup;
    
    //Gtk::TextView *text;
    Gtk::TreeView *tree;
    int column;
    
    void event_delete(GdkEventAny *ev);
    void pressed_find();
    void pressed_cancel();
    
  public:
    MGFindDialog(const Glib::ustring &title, Gtk::TreeView *tree, int column);
};


void MGFindDialog::event_delete(GdkEventAny *ev)
{
  pressed_cancel();
}


void MGFindDialog::pressed_find()
{  
  Glib::ustring text= entry.get_text();
  bool case_sens= case_check.get_active();
  Gtk::TreeIter iter, end;
  Gtk::TreeRow row;

  iter= tree->get_selection()->get_selected();
  if (!iter)
  {
    iter= tree->get_model()->children().begin();
    status.set_text("");
  }
  else
    ++iter;
  end= tree->get_model()->children().end();

  if (!case_sens)
    text= text.casefold();
  
  while (iter != end)
  {
    Glib::ustring line;
    
    row= *iter;
    row.get_value(column, line);

    if (!case_sens)
      line= line.casefold();

    if (line.find(text) != Glib::ustring::npos)
    {
      tree->get_selection()->select(iter);
      status.set_text("");
      break;
    }
    ++iter;
  }
  if (iter == end)
  {
    status.set_text("Text not found");
  }
}


void MGFindDialog::pressed_cancel()
{
  hide();
  delete this;
}



MGFindDialog::MGFindDialog(const Glib::ustring &title, Gtk::TreeView *aTree, int aColumn)
  : Gtk::Window(Gtk::WINDOW_TOPLEVEL), vbox(false, 12), cbox(false, 8), bbox(false, 12),
   find_button(Gtk::Stock::FIND), cancel_button(Gtk::Stock::CANCEL),
   case_check(_("Case-sensitive"))
{
  tree= aTree;
  column= aColumn;

  set_position(Gtk::WIN_POS_MOUSE);
  set_title(_("Find Text in Page"));
  add(vbox);
  vbox.set_border_width(12);
  
  sgroup= Gtk::SizeGroup::create(Gtk::SIZE_GROUP_HORIZONTAL);
  
  set_transient_for(*(Gtk::Window*)aTree->get_toplevel());
  set_default_size(300, 100);

  vbox.pack_start(entry, false, false);
  vbox.pack_start(cbox, true, true);
  vbox.pack_start(bbox, false, false);

  cbox.pack_start(case_check, false, false);
  cbox.pack_start(status, false, false);

  bbox.pack_end(find_button, false, true);
  bbox.pack_end(cancel_button, false, true);

  sgroup->add_widget(find_button);
  sgroup->add_widget(cancel_button);
  
  find_button.signal_clicked().connect(sigc::mem_fun(*this,&MGFindDialog::pressed_find));
  cancel_button.signal_clicked().connect(sigc::mem_fun(*this,&MGFindDialog::pressed_cancel));
  
  show_all();
}



MAServerLogsPanel::MAServerLogsPanel(MAdministrator *app, MDataInterface *data)
  : MAPanel(app, data)
{
}


void MAServerLogsPanel::search_log(LogType type)
{
  MGFindDialog *dlg= new MGFindDialog(_("Find Text in Page"), _list[type], 0);
}


void MAServerLogsPanel::setup_panel(LogType type)
{
  Gtk::TreeView *tree= _tree[type];
//  _menu[type]->set_sensitive(false);

  tree->append_column("", _columns._icon);
  tree->append_column(_("Time"), _columns._time);
  tree->append_column(_("Event"), _columns._text);

  tree->get_column(0)->set_fixed_width(30);
  tree->get_column(1)->set_resizable(true);
  tree->get_column(2)->set_resizable(true);

  _store[type]= Gtk::ListStore::create(_columns);
  tree->set_model(_store[type]);
}


bool MAServerLogsPanel::init()
{
  int i;
  static char *types[]={
    "error","binary","slow","general","innodb"
  };

  if (_xml)
    return true;

  if (!MAPanel::init_from_glade(get_glade_file(GLADE_SERVERLOGS_FILE), "panel_frame"))
    return false;

  for (i= 0; i < 5; i++)
  {
    char buffer[32];
    
    sprintf(buffer, "%s_label", types[i]);
    _label[i]= get_label(buffer);

    sprintf(buffer, "%s_frame", types[i]);
    _frame[i]= get_label(buffer);

    sprintf(buffer, "%s_tree", types[i]);
    _tree[i]= get_tree(buffer);

    _tree[i]->get_selection()->signal_changed().connect(sigc::bind<LogType>(sigc::mem_fun(*this,&MAServerLogsPanel::selected_event),(LogType)i));

//    sprintf(buffer, "%s_option", types[i]);
//    _menu[i]= (Gtk::OptionMenu*)get_widget(buffer);
    sprintf(buffer, "%s_list", types[i]);
    _list[i]= get_tree(buffer);
    _list[i]->append_column("", _log_columns._text);

    _list_store[i]= Gtk::ListStore::create(_log_columns);
    _list[i]->set_model(_list_store[i]);

    sprintf(buffer, "%s_scrollbar", types[i]);
    _sbar[i]= (Gtk::Scrollbar*)get_widget(buffer);

    _sbar[i]->signal_value_changed().connect(sigc::bind<LogType>(sigc::mem_fun(*this,&MAServerLogsPanel::scroll_log),(LogType)i));

    sprintf(buffer, "%s_select", types[i]);
    get_button(buffer)->signal_clicked().connect(sigc::bind<LogType>(sigc::mem_fun(*this,&MAServerLogsPanel::select_log),(LogType)i));

    sprintf(buffer, "%s_search", types[i]);
    get_button(buffer)->signal_clicked().connect(sigc::bind<LogType>(sigc::mem_fun(*this,&MAServerLogsPanel::search_log),(LogType)i));

    sprintf(buffer, "%s_save", types[i]);
    _saveb[i]= get_button(buffer);
    _saveb[i]->signal_clicked().connect(sigc::bind<LogType>(sigc::mem_fun(*this,&MAServerLogsPanel::save_log),(LogType)i));

    sprintf(buffer, "%s_select", types[i]);
    get_button(buffer)->set_sensitive(_inst->get_saved_info()!=0);
  }

  _start_icon= PIXCACHE->load("service_start.png");
  _stop_icon= PIXCACHE->load("service_stop.png");

  _warning_icon= PIXCACHE->load("16x16_warning.png");
  _error_icon= PIXCACHE->load("16x16_error.png");
  
  setup_panel(LError);
 // setup_panel(LBinary);
  setup_panel(LSlow);
  setup_panel(LGeneral);
 // setup_panel(LInnoDB);

  return true;
}


bool MAServerLogsPanel::before_show()
{
  process_logs(LError);
  process_logs(LSlow);
  process_logs(LGeneral);

  return true;
}


bool MAServerLogsPanel::before_hide()
{

  return true;
}


void MAServerLogsPanel::selected_event(LogType type)
{
  Gtk::TreeIter iter= _tree[type]->get_selection()->get_selected();
  
  if (iter)
  {
    Gtk::TreeModel::Row row= *iter;
    int line= row[_columns._line];

    Gtk::TreeIter iter= _list_store[type]->children().begin();
    while (--line > 0)
      ++iter;

    _list[type]->get_selection()->select(iter);
    _list[type]->scroll_to_row(_list_store[type]->get_path(iter));
  }
}


void MAServerLogsPanel::scroll_log(LogType type)
{
  double val= floor(_sbar[type]->get_value());
  _sbar[type]->set_value(val);

  process_logs(type);
}


void MAServerLogsPanel::select_log(LogType type)
{
  Gtk::FileSelection fsel(_("Select Log File"));
  MInstanceInfo::SavedInfo *info= _inst->get_saved_info();

  if (!info)
    return;
  
  switch (type)
  {
  case LError: fsel.set_filename(info->error_log_path); break;
  case LGeneral: fsel.set_filename(info->general_log_path); break;
  case LSlow: fsel.set_filename(info->slow_log_path); break;
  default: break;
  }

  if (fsel.run() == Gtk::RESPONSE_OK)
  {
    switch (type)
    {
    case LError: info->error_log_path= fsel.get_filename(); break;
    case LGeneral: info->general_log_path= fsel.get_filename(); break;
    case LSlow: info->slow_log_path= fsel.get_filename(); break;
    default: break;
    }
  
    if (process_logs(type))
      info->save();
  }
}


void MAServerLogsPanel::save_log(LogType type)
{
  Gtk::FileSelection fsel(_("Save Log"));

  if (fsel.run() != Gtk::RESPONSE_OK)
    return;
  
  std::string target= fsel.get_filename();
  std::string source;

  switch (type)
  {
  case LError: source= _inst->get_server_path(MInstanceInfo::PErrorLog); break;
  case LGeneral: source= _inst->get_server_path(MInstanceInfo::PLogs); break;
  case LSlow: source= _inst->get_server_path(MInstanceInfo::PSlowLog); break;
  default: break;
  }

  if (copy_file(source.c_str(), target.c_str()) < 0)
  {
    myg_show_sys_error(*_app->window(),
                       ufmt(_("Could not save log file to '%s'"), target.c_str()),
                       errno);
  }
  else
  {
    _app->set_status(ufmt(_("Log file saved to '%s'"), target.c_str()));
  }
}


void MAServerLogsPanel::set_page_sensitive(LogType type, bool flag)
{
  _label[type]->set_sensitive(flag);
  _tree[type]->set_sensitive(flag);
  _list[type]->set_sensitive(flag);
  _sbar[type]->set_sensitive(flag);
  _saveb[type]->set_sensitive(flag);
}


bool MAServerLogsPanel::process_logs(LogType type)
{
  std::string file;
  int block;
  MYX_LOGFILE *logf= 0;
  long total_size;
  
  static char *types[]={
    N_("Error Log"),N_("Binary Log"),N_("Slow Log"),N_("General Log"),N_("InnoDB Log")
  };

  switch (type)
  {
  case LError: 
    file= _inst->get_server_path(MInstanceInfo::PErrorLog);
    if (!file.empty())
      logf= myx_parse_error_log(file.c_str(), PAGE_BLOCK_SIZE, 
                              (int)_sbar[type]->get_value()+1, &block);
    break;
  case LGeneral: 
    file= _inst->get_server_path(MInstanceInfo::PLogs);
    if (!file.empty())
      logf= myx_parse_general_log(file.c_str(), PAGE_BLOCK_SIZE, 
                              (int)_sbar[type]->get_value()+1, &block);
    break;
  case LSlow: 
    file= _inst->get_server_path(MInstanceInfo::PSlowLog); 
    if (!file.empty())
      logf= myx_parse_slow_log(file.c_str(), PAGE_BLOCK_SIZE, 
                              (int)_sbar[type]->get_value()+1, &block);
    break;
  default: break;
  }
  
  _store[type]->clear();
  _list_store[type]->clear();

  if (Glib::file_test(file, Glib::FILE_TEST_IS_REGULAR))
  {
    total_size= get_file_size(file.c_str());
    
    _label[type]->set_markup(ufmt("<b>%s</b>\n%s: %sB", 
                                  _(types[type]), file.c_str(),
                                  format_value(total_size).c_str()));
  }
  else
  {
    _label[type]->set_markup(ufmt("<b>%s</b>\n%s: %s", 
                                  _(types[type]), file.c_str(),
                                  _("Invalid file")));
  }
  
  if (logf)
  {
    set_page_sensitive(type, true);

    if (total_size/PAGE_BLOCK_SIZE == 0)
      _sbar[type]->set_range(0, 1);
    else
      _sbar[type]->set_range(0, total_size/PAGE_BLOCK_SIZE);
    _sbar[type]->get_adjustment()->set_page_size(1);
    _sbar[type]->set_increments(1, 1);

    for (unsigned int i= 0; i < logf->events_num; i++)
    {
      MYX_LOGFILE_EVENT *event= logf->events + i;
      Gtk::TreeIter iter = _store[type]->append();
      Gtk::TreeModel::Row row= *iter;
      
      switch (event->event_type)
      {
      case MYX_EVENT_START:
        row[_columns._icon]= _start_icon;
        row[_columns._text]= _("Server startup");
        break;
      case MYX_EVENT_END:
        row[_columns._icon]= _stop_icon;
        row[_columns._text]= _("Server shutdown");
        break;
      case MYX_EVENT_ERROR:
        row[_columns._icon]= _error_icon;
        row[_columns._text]= _("Error");
        break;
      case MYX_EVENT_INNODB_START:
        row[_columns._icon]= _start_icon;
        row[_columns._text]= _("InnoDB startup");
        break;
      case MYX_EVENT_INNODB_SHUTDOWN:
        row[_columns._icon]= _stop_icon;
        row[_columns._text]= _("InnoDB shutdown");
        break;
      case MYX_EVENT_FORCED_CLOSE_THREAD:
        //row[_columns._icon]= _stop_icon;
        row[_columns._text]= _("Thread closed");
        break;
      case MYX_EVENT_ABORT:
        //row[_columns._icon]= _stop_icon;
        row[_columns._text]= _("Abort");
        break;
      case MYX_EVENT_SELECT:
        //row[_columns._icon]= _stop_icon;
        row[_columns._text]= _("Select");
        break;
      case MYX_EVENT_INIT:
        //row[_columns._icon]= _stop_icon;
        row[_columns._text]= _("Init DB");
        break;
      case MYX_EVENT_CONNECT:
        //row[_columns._icon]= _stop_icon;
        row[_columns._text]= _("Connect");
        break;
      case MYX_EVENT_QUIT:
        //row[_columns._icon]= _stop_icon;
        row[_columns._text]= _("Quit");
        break;
      case MYX_EVENT_QUERY:
        //row[_columns._icon]= _stop_icon;
        row[_columns._text]= _("Query");
        break;
      }
      
      row[_columns._line]= event->line_no;

      char buffer[64];
      struct tm t;
      t.tm_year= event->date->tm_year;
      t.tm_mon= event->date->tm_mon;
      t.tm_mday= event->date->tm_mday;
      t.tm_hour= event->date->tm_hour;
      t.tm_min= event->date->tm_min;
      t.tm_sec= event->date->tm_sec;
      sprintf(buffer, "%i/%i/%i %i:%i:%i", 
              t.tm_mon, t.tm_mday, t.tm_year,
              t.tm_hour, t.tm_min, t.tm_sec);
      strptime(buffer,"%D %T",&t);
      strftime(buffer, sizeof(buffer), "%a %X %x", &t);
      row[_columns._time]= buffer;
    }

    for (unsigned int i= 0; i < logf->lines_num; i++)
    {
      Gtk::TreeIter iter= _list_store[type]->append();
      Gtk::TreeRow row= *iter;
      
      row[_log_columns._text]= Glib::ustring((char*)logf->lines[i]);
    }

    myx_free_logfile(logf);

    return true;
  }
  else
  {
    Glib::ustring message;

    set_page_sensitive(type, false);
    
    if (!file.empty() && access(file.c_str(), F_OK)==0)
    {
      if (access(file.c_str(), R_OK)!=0)
        message= ufmt(_("Could not open log file '%s': no permission for reading"),
                      file.c_str());
      else
        message= ufmt(_("Could not open log file '%s'"),
                      file.c_str());
    }
    else
      message= _("Log file not found");
    
    Gtk::TreeIter iter= _list_store[type]->append();
    Gtk::TreeRow row= *iter;

    row[_log_columns._text]= message;
  }
  return false;
}




MAPanel *create_server_logs_panel(MAdministrator *app, MDataInterface *data)
{
  return new MAServerLogsPanel(app, data);
}
