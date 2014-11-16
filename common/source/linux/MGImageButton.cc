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

#include "MGImageButton.h"
/**
 * @file  MGImageButton.cc
 * @brief 
 */


bool MGImageButton::on_expose_event(GdkEventExpose* event)
{
  int w, h;
  int x, y;
  
  if (event->count != 0)
    return true;

  w= get_width();
  h= get_height();
  x= get_allocation().get_x();
  y= get_allocation().get_y();

  if (_hoverIcon && !_pressed && _inside)
    _hoverIcon->render_to_drawable(get_window(),
                                    Gdk::GC::create(get_window()),
                                    0, 0,
                                    x + (w - _normalIcon->get_width()) / 2,
                                    y + (h - _normalIcon->get_height()) / 2,
                                    -1, -1,
                                    Gdk::RGB_DITHER_NONE, 0, 0);
  else if (!_pressed || !_inside)
    _normalIcon->render_to_drawable(get_window(),
                                    Gdk::GC::create(get_window()),
                                    0, 0,
                                    x + (w - _normalIcon->get_width()) / 2,
                                    y + (h - _normalIcon->get_height()) / 2,
                                    -1, -1,
                                    Gdk::RGB_DITHER_NONE, 0, 0);
  else
    _pushedIcon->render_to_drawable(get_window(),
                                    Gdk::GC::create(get_window()),
                                    0, 0,
                                    x + (w - _pushedIcon->get_width()) / 2,
                                    y + (h - _pushedIcon->get_height()) / 2,
                                    -1, -1,
                                    Gdk::RGB_DITHER_NONE, 0, 0);
  return true;
}


MGImageButton::MGImageButton()
  : Gtk::Button()
{
  _pressed= false;
  
  signal_pressed().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MGImageButton::on_pressed),true));
  signal_released().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MGImageButton::on_pressed),false));
  signal_enter().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MGImageButton::on_entered),true));
  signal_leave().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MGImageButton::on_entered),false));
}


void MGImageButton::on_pressed(bool flag)
{
  _pressed= flag;
  queue_draw();
}


void MGImageButton::on_entered(bool enter)
{
  _inside= enter;
  queue_draw();
}



void MGImageButton::set_image(Glib::RefPtr<Gdk::Pixbuf> image)
{
  _normalIcon= image;
  
  set_size_request(image->get_width(), image->get_height());
}


void MGImageButton::set_alt_image(Glib::RefPtr<Gdk::Pixbuf> image)
{
  _pushedIcon= image;
}
 

void MGImageButton::set_hover_image(Glib::RefPtr<Gdk::Pixbuf> image)
{
  _hoverIcon= image;
}
