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

#include "MGCellRendererCombo.h"
#include <gtkmm/comboboxtext.h>
/**
 * @file  MGCellRendererCombo.cc
 * @brief 
 */



class MGCellRendererCombo::CellCombo : public Gtk::ComboBoxText, public Gtk::CellEditable
{
  public:
};



MGCellRendererCombo::MGCellRendererCombo()
  : Glib::ObjectBase(typeid(MGCellRendererCombo)),
    Gtk::CellRendererText()
{
  property_mode()= Gtk::CELL_RENDERER_MODE_ACTIVATABLE;
  _combo= 0;
}


Gtk::CellEditable* MGCellRendererCombo::start_editing_vfunc(GdkEvent* event, 
                                                            Gtk::Widget& widget, 
                                                            const Glib::ustring& path,
                                                            const Gdk::Rectangle& background_area, 
                                                            const Gdk::Rectangle& cell_area,
                                                            Gtk::CellRendererState flags)
{
  if (!_combo)
  {
    _combo= new CellCombo();

    _combo->set_model(_model);
    _combo->set_active(1);
    _combo->show();
    
    _combo->signal_hide().connect(sigc::mem_fun(*this,&MGCellRendererCombo::on_combo_hide));
    _combo->signal_changed().connect(sigc::mem_fun(*this,&MGCellRendererCombo::on_combo_changed));
  }
  _path= path;
  return _combo;
}


void MGCellRendererCombo::set_model(Glib::RefPtr<Gtk::TreeModel> model)
{
  _model= model;
}


void MGCellRendererCombo::on_combo_hide()
{
  delete _combo;
  _combo= 0;
}


void MGCellRendererCombo::on_combo_changed()
{
  edited(_path, _combo->get_active_text());
}


