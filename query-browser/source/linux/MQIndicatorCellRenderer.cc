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


#include "MQIndicatorCellRenderer.h"

MQIndicatorCellRenderer::MQIndicatorCellRenderer()
:
  Glib::ObjectBase(typeid(MQIndicatorCellRenderer)),
  Gtk::CellRenderer(),
  _property_active(*this, "active", false)
{
  property_xpad()= 4;
  property_ypad()= 2;
}


Glib::PropertyProxy<bool> MQIndicatorCellRenderer::property_active()
{
  return _property_active.get_proxy();
}


void MQIndicatorCellRenderer::get_size_vfunc(Gtk::Widget&,
                                            const Gdk::Rectangle* cell_area,
                                            int* x_offset, int* y_offset,
                                            int* width, int* height)
{
#define INDICATOR_WIDTH 10

  const int calc_width= property_xpad() * 2 + INDICATOR_WIDTH;
  const int calc_height= property_ypad() * 2 + INDICATOR_WIDTH;

  if (width)
    *width = calc_width;

  if (height)
    *height = calc_height;

  if (cell_area)
  {
    if (x_offset)
    {
      *x_offset= int(property_xalign() * (cell_area->get_width() - calc_width));
      *x_offset= std::max(0, *x_offset);
    }

    if (y_offset)
    {
      *y_offset= int(property_yalign() * (cell_area->get_height() - calc_height));
      *y_offset= std::max(0, *y_offset);
    }
  }
}


void MQIndicatorCellRenderer::render_vfunc(const Glib::RefPtr<Gdk::Window>& window,
                                        Gtk::Widget& widget,
                                        const Gdk::Rectangle& back_area,
                                        const Gdk::Rectangle& cell_area,
                                        const Gdk::Rectangle&,
                                        Gtk::CellRendererState flags)
{
  int x_offset = 0, y_offset = 0, width = 0, height = 0;
  get_size(widget, cell_area, x_offset, y_offset, width, height);

  Glib::RefPtr<Gdk::GC> gc= Gdk::GC::create(window);
  {
    Gdk::Color color;
    
    if (_property_active)
      color= Gdk::Color("#b0b0bc");
    else
      color= Gdk::Color("#f0f0f0");
    window->get_colormap()->alloc_color(color);
    
    gc->set_foreground(color);

    window->draw_rectangle(gc,
                           true,
                           back_area.get_x(), back_area.get_y(),
                           back_area.get_width(), back_area.get_height());
  }

  if (flags & Gtk::CELL_RENDERER_SELECTED)
  {
    std::list<Gdk::Point> points;

    Gdk::Color color;
    
    if (_property_active)
      color= Gdk::Color("#000000");
    else
      color= Gdk::Color("#a0a0a0");
    window->get_colormap()->alloc_color(color);

    gc->set_foreground(color);
    
    int x;
    
    x= cell_area.get_x() + cell_area.get_width() - x_offset - 2;

    points.push_back(Gdk::Point(x, cell_area.get_y()+y_offset+height/2));
    points.push_back(Gdk::Point(x - INDICATOR_WIDTH*2/3, cell_area.get_y()+y_offset+height/2-INDICATOR_WIDTH/2));
    points.push_back(Gdk::Point(x - INDICATOR_WIDTH*2/3, cell_area.get_y()+y_offset+height/2+INDICATOR_WIDTH/2));

    window->draw_polygon(gc, true, points);
  }
}


bool MQIndicatorCellRenderer::activate_vfunc(GdkEvent*,
                                          Gtk::Widget&,
                                          const Glib::ustring& path,
                                          const Gdk::Rectangle&,
                                          const Gdk::Rectangle&,
                                          Gtk::CellRendererState)
{
  return false;
}

