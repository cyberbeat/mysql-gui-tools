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
#include "MAServiceControlPanel.h"
#include "MAdministrator.h"
#include "MDataInterface.h"
#include "MInstanceInfo.h"
#include "MYXInterface.h"
#include "myg_gtkutils.h"
#include "MGConnectDialog.h"


MAServiceControlPanel::MAServiceControlPanel(MAdministrator *app, MDataInterface *data)
    : MAPanel(app, data), _can_reconnect(false)
{
}


bool MAServiceControlPanel::init()
{
  if (_xml)
    return true;

  if (!MAPanel::init_from_glade(get_glade_file(GLADE_SERVICECONTROL_FILE), "panel_frame"))
    return false;

  
  ((Gtk::Button*)get_widget("control_button"))->signal_clicked().connect(sigc::mem_fun(*this,&MAServiceControlPanel::toggle_service));
  

  return true;
}


bool MAServiceControlPanel::before_show()
{
  set_server_state(_inst->get_server_state()==MInstanceInfo::SRunning);

  return true;
}


bool MAServiceControlPanel::before_hide()
{

  return true;
}


void MAServiceControlPanel::reconnect()
{
  MGConnectDialog dlg(&prefs);

  if (dlg.run_with_defaults(MY_NAME, &_last_user_conn, 0))
  {
    _inst->set_connection(dlg.get_connection(), dlg.get_user_connection());
    _can_reconnect= false;
  }
}


void MAServiceControlPanel::append_log_text(const Glib::ustring &msg)
{
  Gtk::TextView *text= (Gtk::TextView*)get_widget("log_text");
  Glib::RefPtr<Gtk::TextBuffer> text_buf= text->get_buffer();
  Gtk::TextIter iter;

  iter= text_buf->end();

  text_buf->insert(iter, msg);

  iter= text_buf->end();

  text->scroll_to_iter(iter, 1.0);
}

  
void MAServiceControlPanel::toggle_service()
{
  if (_inst->get_server_state()==MInstanceInfo::SRunning)
      start_stop_service(true);
  else
      start_stop_service(false);
  
  before_show();
}
  

void MAServiceControlPanel::start_stop_service(bool stop)
{
  Glib::RefPtr<Gtk::TextBuffer> text;

  if (stop)
  {
    _last_user_conn= _inst->get_connection_data();
    _can_reconnect= true;
  }

  _inst->mark_log_file();
  
  text= ((Gtk::TextView*)get_widget("log_text"))->get_buffer();
  _inst->start_stop_service(stop, text);

  if (_inst->get_server_state()!=MInstanceInfo::SRunning)
    _inst->disconnect();
  else
  {
    if (!stop && _can_reconnect)
      reconnect();
  }
    
  set_server_state(_inst->get_server_state()==MInstanceInfo::SRunning);
  
  text->insert(text->end(), "*** Begin Logs:\n");
  
  text->insert(text->end(), _inst->fetch_logs_from_mark());
  
  text->insert(text->end(), "*** End Logs\n");
}


void MAServiceControlPanel::set_server_state(bool running)
{
  Gtk::Image *image= (Gtk::Image*)get_widget("status_image");
  Gtk::Label *label= (Gtk::Label*)get_widget("status_label");
  Gtk::Image *btn_image= (Gtk::Image*)get_widget("control_image");
  Gtk::Label *btn_label= (Gtk::Label*)get_widget("control_label");
  Gtk::Label *ctl_help= (Gtk::Label*)get_widget("control_help");


  if (running)
  {
    label->set_markup(_("Server Status:\n    <b>Server is running</b>"));
    image->set(PIXCACHE->load("service_status_running.png"));
    
    btn_label->set_text(_("Stop the Server"));
    ctl_help->set_markup(_("Click this button to stop the MySQL Server.\n"
                           "<small>Be aware that all connected users will be disconnected!</small>"));
    
    btn_image->set(PIXCACHE->load("service_stop.png"));
  }
  else
  {
    label->set_markup(_("Server Status:\n    <b>Server is stopped</b>"));
    image->set(PIXCACHE->load("service_status_stopped.png"));
    
    btn_label->set_text(_("Start the Server"));
    ctl_help->set_markup(_("Click this button to start the MySQL Server."));
    
    btn_image->set(PIXCACHE->load("service_start.png"));
  }
}


MAPanel *create_service_control_panel(MAdministrator *app, MDataInterface *data)
{
  return new MAServiceControlPanel(app, data);
}
