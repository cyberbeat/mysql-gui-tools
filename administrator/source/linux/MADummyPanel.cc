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


#include "myadmin.h"

#include "MADummyPanel.h"



MADummyPanel::MADummyPanel(MAdministrator *app, MDataInterface *data)
  : MAPanel(app, data)
{
  init();
}


bool MADummyPanel::init()
{
  if (_panel)
    return true;

  _panel= Gtk::manage(new Gtk::Frame());

  label= Gtk::manage(new Gtk::Label);

  ((Gtk::Container*)_panel)->add(*label);
  label->set_justify(Gtk::JUSTIFY_CENTER);
  label->show();

  return true;
}


void MADummyPanel::set_local_text()
{
  label->set_markup(_("<b>This section is currently only\n"
                      "available for connections to localhost.</b>"));
}

void MADummyPanel::set_connection_text()
{
  label->set_markup(_("<b>Please connect to a MySQL server.</b>"));
}
