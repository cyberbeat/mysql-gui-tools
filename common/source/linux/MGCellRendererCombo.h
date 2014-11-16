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

#ifndef _MGCELLRENDERERCOMBO_H_
#define _MGCELLRENDERERCOMBO_H_

#include <gtkmm/cellrenderertext.h>
#include <gtkmm/treemodel.h>
#include <gtkmm/menu.h>


class MGCellRendererCombo : public Gtk::CellRendererText 
{
    class CellCombo;
    
    Glib::RefPtr<Gtk::TreeModel> _model;
    Gtk::TreeModelColumnBase *_text_column;

    CellCombo *_combo;
    Glib::ustring _path;

    void on_combo_hide();
    void on_combo_changed();
    
    virtual Gtk::CellEditable* start_editing_vfunc(GdkEvent* event, 
                                                   Gtk::Widget& widget, 
                                                   const Glib::ustring& path,
                                                   const Gdk::Rectangle& background_area, 
                                                   const Gdk::Rectangle& cell_area,
                                                   Gtk::CellRendererState flags);

  public:
    MGCellRendererCombo();

    void set_model(Glib::RefPtr<Gtk::TreeModel> model);
};

#endif /* _MGCELLRENDERERCOMBO_H_ */
