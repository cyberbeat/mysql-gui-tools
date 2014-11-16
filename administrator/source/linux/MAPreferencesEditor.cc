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

#include "MAPreferencesEditor.h"
#include "myadmin.h"


MAPreferenceGroup::MAPreferenceGroup()
{
  _xml= new MGGladeXML(get_glade_file("admin_preferences.glade"), "admin_prefs");
}

MAPreferenceGroup::~MAPreferenceGroup()
{
  delete _xml;
}


Glib::ustring MAPreferenceGroup::get_title()
{
  return _("MySQL Administrator Options");
}


void MAPreferenceGroup::init()
{
  _xml->get_entry("init_script_entry")->
    signal_changed().connect(sigc::mem_fun(*this,&MAPreferenceGroup::set_dirty));

  _xml->get_toggle("admin_show_global_check")->
    signal_toggled().connect(sigc::mem_fun(*this,&MAPreferenceGroup::set_dirty));

  _xml->get_toggle("admin_show_table_check")->
    signal_toggled().connect(sigc::mem_fun(*this,&MAPreferenceGroup::set_dirty));
 
  _xml->get_toggle("timestamp_backups_check")->
    signal_toggled().connect(sigc::mem_fun(*this,&MAPreferenceGroup::set_dirty));
  
  _xml->get_spin("connlist_autorefresh")->
    signal_changed().connect(sigc::mem_fun(*this,&MAPreferenceGroup::set_dirty));
}


void MAPreferenceGroup::update()
{
  MAPreferences *prefs= static_cast<MAPreferences*>(_prefs);

  if (prefs->start_script_path != _xml->get_entry("init_script_entry")->get_text())
    _xml->get_entry("init_script_entry")->set_text(prefs->start_script_path);
  if (prefs->show_user_global_privileges != _xml->get_toggle("admin_show_global_check")->get_active())
    _xml->get_toggle("admin_show_global_check")->set_active(prefs->show_user_global_privileges);
  if (prefs->show_user_table_column_privileges != _xml->get_toggle("admin_show_table_check")->get_active())
    _xml->get_toggle("admin_show_table_check")->set_active(prefs->show_user_table_column_privileges);
  if ((!prefs->disable_backup_timestamps) != _xml->get_toggle("timestamp_backups_check")->get_active())
    _xml->get_toggle("timestamp_backups_check")->set_active(!prefs->disable_backup_timestamps);
  if (prefs->connection_list_auto_refresh != _xml->get_spin("connlist_autorefresh")->get_value_as_int())
    _xml->get_spin("connlist_autorefresh")->set_value(prefs->connection_list_auto_refresh);
}


void MAPreferenceGroup::commit()
{
  MAPreferences *prefs= static_cast<MAPreferences*>(_prefs);

  prefs->start_script_path= _xml->get_entry("init_script_entry")->get_text();
  prefs->show_user_global_privileges= _xml->get_toggle("admin_show_global_check")->get_active();
  prefs->show_user_table_column_privileges= _xml->get_toggle("admin_show_table_check")->get_active();
  prefs->disable_backup_timestamps= !_xml->get_toggle("timestamp_backups_check")->get_active();
  prefs->connection_list_auto_refresh= _xml->get_spin("connlist_autorefresh")->get_value_as_int();
}


Gtk::Widget *MAPreferenceGroup::widget()
{
  return _xml->get_widget("admin_prefs");
}
