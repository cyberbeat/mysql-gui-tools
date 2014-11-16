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

#include "MQPreferencesEditor.h"
#include "MQHistory.h"
#include "myqb.h"


MQPreferenceGroup::MQPreferenceGroup()
{
  _xml= new MGGladeXML(get_app_file("qb_preferences.glade"), "prefs");
}
    
Glib::ustring MQPreferenceGroup::get_title()
{
  return _("Query Browser Preferences");
}
    
void MQPreferenceGroup::init()
{
  _xml->get_button("reset_history")->signal_clicked().connect(sigc::mem_fun(*this,&MQPreferenceGroup::reset_history));
  
  _xml->get_spin("history_spin")->signal_changed().connect(sigc::mem_fun(*this,&MGPreferenceGroup::set_dirty));
  _xml->get_spin("blob_size_spin")->signal_changed().connect(sigc::mem_fun(*this,&MGPreferenceGroup::set_dirty));
  
  _xml->get_toggle("beep_check")->signal_clicked().connect(sigc::mem_fun(*this,&MGPreferenceGroup::set_dirty));
  _xml->get_toggle("auto_add_pk_check")->signal_clicked().connect(sigc::mem_fun(*this,&MGPreferenceGroup::set_dirty));
  
  _xml->get_spin("limit_spin")->signal_changed().connect(sigc::mem_fun(*this,&MGPreferenceGroup::set_dirty));
}


void MQPreferenceGroup::update()
{
  _xml->get_spin("history_spin")->set_value(((MQPreferences*)_prefs)->max_query_history);
  _xml->get_spin("blob_size_spin")->set_value(((MQPreferences*)_prefs)->max_blob_length);
  _xml->get_toggle("beep_check")->set_active(!((MQPreferences*)_prefs)->dont_beep);
  _xml->get_toggle("auto_add_pk_check")->set_active(((MQPreferences*)_prefs)->auto_add_pk_check);
  
  _xml->get_spin("limit_spin")->set_value(((MQPreferences*)_prefs)->default_limit_value);
}

void MQPreferenceGroup::commit()
{
  ((MQPreferences*)_prefs)->max_query_history= (int)_xml->get_spin("history_spin")->get_value();
  ((MQPreferences*)_prefs)->max_blob_length= (int)_xml->get_spin("blob_size_spin")->get_value();
  ((MQPreferences*)_prefs)->dont_beep= !_xml->get_toggle("beep_check")->get_active();
  ((MQPreferences*)_prefs)->auto_add_pk_check= _xml->get_toggle("auto_add_pk_check")->get_active();
  ((MQPreferences*)_prefs)->default_limit_value= (int)_xml->get_spin("limit_spin")->get_value();
}

Gtk::Widget *MQPreferenceGroup::widget()
{
  return _xml->get_widget("prefs");
}


void MQPreferenceGroup::reset_history()
{
  MQHistory::instance()->reset();
}
