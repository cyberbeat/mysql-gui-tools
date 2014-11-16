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
#include "MAServerInformationPanel.h"

#include <string.h>
#include <stdio.h>

#include "MAdministrator.h"
#include "MDataInterface.h"

#include "myg_utils.h"
#include "myg_gtkutils.h"

#include "myx_public_interface.h"
#include "myx_admin_public_interface.h"


MAServerInformationPanel::MAServerInformationPanel(MAdministrator *app, MDataInterface *data)
  : MAPanel(app, data)
{
}


bool MAServerInformationPanel::init()
{
  if (_xml)
    return true;

  if (!MAPanel::init_from_glade(get_glade_file(GLADE_SERVERINFORMATION_FILE), "panel_frame"))
    return false;
  
  // fill data
  MYX_MACHINE_INFO *server= _inst->get_server_info();
  MYX_MACHINE_INFO *local= _inst->get_client_info();
  Gtk::Label *label;

  char *ptr= strstr(server->version, " via");
  if (ptr)
    *ptr= 0;

  {
    MYX::UserConnection conn= _inst->get_connection_data();

    label= (Gtk::Label*)get_widget("left_label1");
    label->set_text(ufmt("%s\n%s\n%s",
                         _("User:"),
                         _("Host:"),
                         _inst->get_mysql()->unix_socket ? _("Socket:") : _("Port:")));

    label= (Gtk::Label*)get_widget("server_label");
    
    if (_inst->get_mysql()->unix_socket)
      label->set_text(ufmt("%s\n%s\n%s",
                           conn.username.c_str(),
                           conn.hostname.c_str(),
                           _inst->get_mysql()->unix_socket));
    else
      label->set_text(ufmt("%s\n%s\n%d",
                           conn.username.c_str(),
                           conn.hostname.c_str(),
                           conn.port));
  }

  label= (Gtk::Label*)get_widget("server_info_label");
  label->set_text(ufmt("%s\n%s\n%s",
                       server->version,
                       server->network_name,
                       server->IP));

  label= (Gtk::Label*)get_widget("client_label");
  label->set_text(ufmt("%s\n%s\n%s\n%s\n%s",
                       local->version,
                       local->network_name,
                       local->IP,
                       local->OS,
                       local->hardware));

  // set the left labels to be all the same width
  _size_group= Gtk::SizeGroup::create(Gtk::SIZE_GROUP_HORIZONTAL);
  _size_group->add_widget(*get_widget("left_label1"));
  _size_group->add_widget(*get_widget("left_label2"));
  _size_group->add_widget(*get_widget("left_label3"));
  
  myx_free_pc_info(server);
  myx_free_pc_info(local);

  return true;
}


void MAServerInformationPanel::show()
{
  MAPanel::show();  
  
  {
    Glib::RefPtr<Gdk::Pixbuf> logo= PIXCACHE->load("mysql_logo.png");
    if (logo)
    {
      ((Gtk::Image*)get_widget("image"))->set(logo);
    }
  }
}


bool MAServerInformationPanel::before_show()
{
  set_server_state(_data->get_instance()->get_server_state()==MInstanceInfo::SRunning);

  return true;
}


bool MAServerInformationPanel::before_hide()
{
  return true;
}


void MAServerInformationPanel::set_server_state(bool running)
{
  Gtk::Image *image= (Gtk::Image*)get_widget("status_image");
  Gtk::Label *label= (Gtk::Label*)get_widget("status_label");

  if (running)
  {
    label->set_markup(_("Server Status:\n    <b>Server is running</b>"));
    image->set(PIXCACHE->load("service_status_running.png"));
  }
  else
  {
    label->set_markup(_("Server Status:\n    <b>Server is stopped</b>"));
    image->set(PIXCACHE->load("service_status_stopped.png"));
  }
}



MAPanel *create_server_information_panel(MAdministrator *app, MDataInterface *data)
{
  return new MAServerInformationPanel(app, data);
}
