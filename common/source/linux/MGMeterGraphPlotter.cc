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


#include "MGMeterGraphPlotter.h"



MGMeterGraphPlotter::MGMeterGraphPlotter(GtkDrawingArea *darea)
  : MGGraphPlotter(darea, false), _value(0)
{
  
}


MGMeterGraphPlotter::MGMeterGraphPlotter()
  : MGGraphPlotter(false), _value(0)
{
  
}


void MGMeterGraphPlotter::update()
{
  Glib::RefPtr<Gdk::GC> gc= get_style()->get_fg_gc(Gtk::STATE_NORMAL);
  int width, height;
  int x, y;

  if (_segment_number <= 0)
    return;
  if (!_back_image || !_fore_image)
    return;

  MGGraphPlotter::update();

  _back_image->get_size(width, height);

  x= (_width - width)/2;

  if (!_text_format.empty())
  {
    char buffer[_text_format.bytes()+20];

    g_snprintf(buffer, sizeof(buffer), _text_format.c_str(), _value);

    Glib::RefPtr<Pango::Layout> l= create_pango_layout(buffer);
    int tw, th;

    l->get_pixel_size(tw, th);

    y= (_height - height - th - 2) / 2;

    _buffer->draw_rectangle(get_style()->get_bg_gc(Gtk::STATE_NORMAL), true,
                            1, y + height + (_height - height - y - th)/2, 
                            _width-2, th);

    _buffer->draw_layout(get_style()->get_text_gc(Gtk::STATE_NORMAL),
                         (_width-tw)/2, y + height + (_height - height - y - th)/2, l);
  }
  else
  {
    y= (_height - height) / 2;
  }

  int yy= (height*_value/100) + (height/_segment_number)/2;
  yy -= yy%(height/_segment_number);

  _buffer->draw_drawable(gc, _back_image, 0, 0, x, y);

  _buffer->draw_drawable(gc, _fore_image, 0, height - yy,
                         x, y + (height - yy),
                         width, yy);

  gc= get_style()->get_bg_gc(Gtk::STATE_NORMAL);

  for (int i= 0; i <= _segment_number; i++)
  {
    _buffer->draw_line(gc, x, y + i*height/_segment_number,
                       x + width, y + i*height/_segment_number);
  }

  refresh();
}


void MGMeterGraphPlotter::set_images(const Glib::RefPtr<Gdk::Pixbuf> &back,
                                     const Glib::RefPtr<Gdk::Pixbuf> &fore)
{
  Glib::RefPtr<Gdk::Bitmap> mask;

  _back_image.clear();
  _fore_image.clear();

  back->render_pixmap_and_mask(_back_image, mask, 100);
  fore->render_pixmap_and_mask(_fore_image, mask, 100);

  if (is_realized() && is_visible())
    update();
}


void MGMeterGraphPlotter::set_images(const Glib::RefPtr<Gdk::Pixmap> &back,
                                     const Glib::RefPtr<Gdk::Pixmap> &fore)
{
  _back_image= back;
  _fore_image= fore;

  if (is_realized() && is_visible())
    update();
}


void MGMeterGraphPlotter::set_text_format(const Glib::ustring &fmt)
{
  _text_format= fmt;
}


void MGMeterGraphPlotter::set_segment_number(int n)
{
  _segment_number= n;
}


void MGMeterGraphPlotter::set_value(int value)
{
  g_return_if_fail(value >= 0 && value <= 100);

  _value= value;

  if (is_realized() && is_visible())
    update();
}
