/* Copyright (C) 2005 MySQL AB

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

#include "MGRTRoutineEmbeddedEditor.h"
#include <MySQLGRT/MGRT.h>
#include <gtkmm/separator.h>
/**
 * @file  MGRTRoutineEmbeddedEditor.cc
 * @brief 
 */



MGRTRoutineEmbeddedEditor::MGRTRoutineEmbeddedEditor(MGRT *grt, MGRTValue routine)
  : Gtk::Frame(), _grt(grt), _routine(routine), _vbox(false, 0), 
  _down_button(""), _right_button(""), _name_label("", 0.0, 0.5), _close_button("")
{
  Gtk::HBox *hbox= Gtk::manage(new Gtk::HBox(false, 8));

  add(_vbox);
  set_shadow_type(Gtk::SHADOW_OUT);

  _vbox.pack_start(*hbox, false, false);
  hbox->pack_start(_down_button, false, false);
  hbox->pack_start(_right_button, false, false);
  hbox->pack_start(_name_label, true, true);
  hbox->pack_start(_close_button, false, false);
  
  _down_button.set_relief(Gtk::RELIEF_NONE);
  _down_button.set_focus_on_click(false);
  _down_button.set_border_width(0);
  _right_button.set_relief(Gtk::RELIEF_NONE);
  _right_button.set_focus_on_click(false);
  _close_button.set_relief(Gtk::RELIEF_NONE);
  _close_button.set_focus_on_click(false);
  
  Gtk::Image *img;
  
  img= Gtk::manage(new Gtk::Image(Gtk::Stock::GO_FORWARD, Gtk::ICON_SIZE_MENU));
  //_right_button.set_image(*img);
  _right_button.signal_clicked().connect(sigc::mem_fun(*this,&MGRTRoutineEmbeddedEditor::open_code));
  img->show();

  img= Gtk::manage(new Gtk::Image(Gtk::Stock::GO_DOWN, Gtk::ICON_SIZE_MENU));
  //_down_button.set_image(*img);
  _down_button.signal_clicked().connect(sigc::mem_fun(*this,&MGRTRoutineEmbeddedEditor::close_code));
  img->show();

  img= Gtk::manage(new Gtk::Image(Gtk::Stock::CLOSE, Gtk::ICON_SIZE_MENU));
  //_close_button.set_image(*img);
  img->show();

  _vbox.pack_start(*Gtk::manage(new Gtk::HSeparator()));
  
  _vbox.pack_start(_text, true, true);
  
  show_all();
  
  open_code();

  show_object();
}


MGRTRoutineEmbeddedEditor::~MGRTRoutineEmbeddedEditor()
{
}


void MGRTRoutineEmbeddedEditor::open_code()
{
  _right_button.hide();
  _down_button.show();
  _text.show();
}


void MGRTRoutineEmbeddedEditor::close_code()
{
  _right_button.show();
  _down_button.hide();
  _text.hide();
}


void MGRTRoutineEmbeddedEditor::show_object()
{
  _name_label.set_text(_routine["name"].asString());
  _text.get_buffer()->set_text(_routine["routineCode"].asString());
}


void MGRTRoutineEmbeddedEditor::apply_changes()
{
  MGRTValue args(MGRTValue::createList());

  args.append(MGRTValue(_text.get_buffer()->get_text().c_str()));
  
  Glib::ustring type= _grt->call_string_function("DbUtils","getRoutineType", args);

  if (!type.empty())
  {
    _routine.set("routineType", type.c_str());
  }

  Glib::ustring name= _grt->call_string_function("DbUtils", "getRoutineName", args);

  if (!name.empty())
  {
    _routine.set("name", name.c_str());
  }

  _name_label.set_text(ufmt("%s %s", !type.empty()?type.c_str():"routine", !name.empty()?name.c_str():"?"));

  _routine.set("routineCode", _text.get_buffer()->get_text().c_str());
}


void MGRTRoutineEmbeddedEditor::discard_changes()
{
  show_object();
}


void MGRTRoutineEmbeddedEditor::close()
{
  _close_button.clicked();
}


