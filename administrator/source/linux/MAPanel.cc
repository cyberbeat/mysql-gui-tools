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

#include "MDataInterface.h"

#include "MAPanel.h"
#include "MAdministrator.h"
#include "myg_utils.h"


MAPanel::MAPanel(MAdministrator *app, MDataInterface *data)
  : _xml(0), _app(app), _data(data), _panel(0)
{
  _parent_frame= app->parent_frame();
  //_transl= app->get_translation();
  _inst= _data->get_instance();
}


MAPanel::~MAPanel()
{
  delete _xml;
}


bool MAPanel::init_from_glade(const std::string & glade_file, const std::string &panel_name)
{
  _xml= new MGGladeXML(glade_file, panel_name);
  if (!_xml)
  {
    Gtk::MessageDialog *dlg;

    dlg= new Gtk::MessageDialog(ufmt(_("The user interface file (%s) could not be loaded.\n"
                                       "%s was probably not installed correctly."),
                                     glade_file.c_str(),
                                     PACKAGE),
                                false,
                                Gtk::MESSAGE_ERROR,
                                Gtk::BUTTONS_OK,
                                true);
    dlg->run();
    delete dlg;

    return false;
  }

  _panel= get_widget(panel_name);

  return true;
}


void MAPanel::show()
{  
  _panel->reference(); // increase refcount so its not destroyed when removed
  _parent_frame->add(*_panel);
  _panel->show();
}


void MAPanel::hide()
{
  _panel->hide();
  _parent_frame->remove(*_panel);
}


Gtk::Widget *MAPanel::get_widget(const Glib::ustring &name) const
{
  Gtk::Widget *w= _xml->get_widget(name);

  if (w==0)
  {
    g_error("request for bad widget '%s'", name.c_str());
    g_assert_not_reached();
  }
  
  return w;
}


Gtk::Entry *MAPanel::get_entry(const Glib::ustring &name) const
{
  return (Gtk::Entry*)get_widget(name);
}


Gtk::ToggleButton *MAPanel::get_toggle(const Glib::ustring &name) const
{
  return (Gtk::ToggleButton*)get_widget(name);
}


Gtk::TreeView *MAPanel::get_tree(const Glib::ustring &name) const
{
  return (Gtk::TreeView*)get_widget(name);
}


Gtk::Button *MAPanel::get_button(const Glib::ustring &name) const
{
  return (Gtk::Button*)get_widget(name);
}


Gtk::Label *MAPanel::get_label(const Glib::ustring &name) const
{
  return (Gtk::Label*)get_widget(name);
}

Gtk::SpinButton *MAPanel::get_spin(const Glib::ustring &name) const
{
  return (Gtk::SpinButton*)get_widget(name);
}
