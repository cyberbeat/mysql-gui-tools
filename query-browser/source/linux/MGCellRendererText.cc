/* Copyright (C) 2006 MySQL AB

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

#include "MGCellRendererText.h"
#include "MGCellRendererBlob.h" // for str2data

void MGCellRendererText::render_vfunc(const Glib::RefPtr<Gdk::Drawable>& window,
                                    Gtk::Widget& widget,
                                    const Gdk::Rectangle& background_area,
                                    const Gdk::Rectangle& cell_area,
                                    const Gdk::Rectangle& expose_area,
                                    Gtk::CellRendererState flags)
{
  Gtk::CellRendererText::render_vfunc(window, widget, background_area, cell_area, expose_area, flags);

  if (!_gc)
    _gc= Gdk::GC::create(window);
  
  
  Glib::ValueBase value;
  value.init(G_TYPE_STRING);
  get_property_value("text", value);

  if (!g_value_peek_pointer(value.gobj()))
  {
    window->draw_pixbuf(_gc, _null_icon, 0, 0,
                        cell_area.get_x(), cell_area.get_y()+(cell_area.get_height()-_null_icon->get_height())/2, -1, -1,
                        Gdk::RGB_DITHER_NONE, 0, 0);
  }
}


void MGCellRendererText::get_size_vfunc(Gtk::Widget& widget, const Gdk::Rectangle* cell_area, int* x_offset, int* y_offset, int* width, int* height) const
{
  Gtk::CellRendererText::get_size_vfunc(widget, cell_area, x_offset, y_offset, width, height);
}


MGCellRendererText::MGCellRendererText(Glib::RefPtr<Gdk::Pixbuf> null_icon)
  : _null_icon(null_icon)
{
}


