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


#include "MGHMeterGraphPlotter.h"
#include <cstring>


MGHMeterGraphPlotter::MGHMeterGraphPlotter(GtkDrawingArea *darea)
  : MGGraphPlotter(darea, false), _current_value(0), _total_value(0), _unit(Count)
{
  
}


MGHMeterGraphPlotter::MGHMeterGraphPlotter()
  : MGGraphPlotter(), _current_value(0), _total_value(0)
{
  
}


Glib::ustring MGHMeterGraphPlotter::format_value(double value)
{
  char buffer[64];
  double tmp= value;
  int i= 0;
  
  switch (_unit)
  {
  case Count:
  case Byte:
    while (tmp > 1024)
    {
      tmp /= 1024.0;
      i++;
    }

    switch (i)
    {
    case 4:
      g_snprintf(buffer, sizeof(buffer)-4, "%.4f T", value/(1024.0*1024.0*1024.0*1024.0));
      break;
    case 3:
      g_snprintf(buffer, sizeof(buffer)-4, "%.2f G", value/(1024.0*1024.0*1024.0));
      break;
    case 2:
      g_snprintf(buffer, sizeof(buffer)-4, "%.2f M", value/(1024.0*1024.0));
      break;
    case 1:
      g_snprintf(buffer, sizeof(buffer)-4, "%.2f k", value/1024.0);
      break;
    case 0:
      g_snprintf(buffer, sizeof(buffer)-4, "%.0f ", value);
      break;
    }
    if (_unit == Byte)
      strcat(buffer, "B");
    break;

  case Percentage:
    g_snprintf(buffer, sizeof(buffer), "%i%%", (int)value);
    break;

  case Seconds:
    g_snprintf(buffer, sizeof(buffer), "%i s", (int)value);
    break;
  }

  return Glib::ustring(buffer);
}


void MGHMeterGraphPlotter::update()
{
  Glib::RefPtr<Gdk::GC> gc= get_style()->get_fg_gc(Gtk::STATE_NORMAL);
  int width, height;
  int xx;
  
  g_assert(_back_image && _fore_image);

  MGGraphPlotter::update();

  if (_width < 2 || _height < 2)
    return;

  width= _back_image->get_width();
  height= _back_image->get_height();

  int tmpw, tmph;
  if (_back_pixmap)
    _back_pixmap->get_size(tmpw, tmph);
  else
    tmpw= 0;
  if (tmpw != _width-2 || tmph != _height-2)
  {
    Glib::RefPtr<Gdk::Bitmap> mask;
    _back_image->scale_simple(_width-2, _height-2, Gdk::INTERP_BILINEAR)->render_pixmap_and_mask(_back_pixmap, mask, 100);
    _fore_image->scale_simple(_width-2, _height-2, Gdk::INTERP_BILINEAR)->render_pixmap_and_mask(_fore_pixmap, mask, 100);
  }

  xx= (int)(_width * _current_value / _total_value);

  _buffer->draw_drawable(gc, _back_pixmap, 0, 0, 1, 1);

  if (xx > 0)
  {
    _buffer->draw_drawable(gc, _fore_pixmap, 0, 0, 1, 1, xx);
  }

  if (!_current_format.empty())
  {
    Glib::ustring text;
    
    text= _current_format+"  "+format_value(_current_value);

    Glib::RefPtr<Pango::Layout> l= create_pango_layout(text);
    int tw, th;

    l->get_pixel_size(tw, th);

    _buffer->draw_layout(get_style()->get_white_gc(),
                         4, (_height - th - 1)/2, l);
  }

  if (!_total_format.empty())
  {
    Glib::ustring text;

    text= _total_format+"  "+format_value(_total_value);

    Glib::RefPtr<Pango::Layout> l= create_pango_layout(text);
    int tw, th;

    l->get_pixel_size(tw, th);

    _buffer->draw_layout(get_style()->get_white_gc(),
                         _width - tw - 4, (_height - th - 1)/2, l);
  }

  refresh();
}


void MGHMeterGraphPlotter::set_images(const Glib::RefPtr<Gdk::Pixbuf> &back,
                                     const Glib::RefPtr<Gdk::Pixbuf> &fore)
{
  _back_image= back;
  _fore_image= fore;

  if (is_realized() && is_visible())
    update();
}


void MGHMeterGraphPlotter::set_current_format(const Glib::ustring &fmt)
{
  _current_format= fmt;
}


void MGHMeterGraphPlotter::set_total_format(const Glib::ustring &fmt)
{
  _total_format= fmt;
}


void MGHMeterGraphPlotter::set_value_unit(Unit unit)
{
  _unit= unit;
}


void MGHMeterGraphPlotter::set_current_value(double value)
{
  _current_value= value;

  if (is_realized() && is_visible())
    update();
}


void MGHMeterGraphPlotter::set_total_value(double value)
{
  _total_value= value;

  if (is_realized() && is_visible())
    update();
}
