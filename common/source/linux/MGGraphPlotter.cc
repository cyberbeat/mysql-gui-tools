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


#include "MGGraphPlotter.h"


MGGraphPlotter::MGGraphPlotter(GtkDrawingArea *darea, bool use_buffer)
  : Gtk::DrawingArea(darea), _initialized(false), _resized(false), _use_buffer(use_buffer),
    _refreshing(false)
{
  modify_bg(Gtk::STATE_NORMAL, get_style()->get_black());
}


MGGraphPlotter::MGGraphPlotter(bool use_buffer)
  : Gtk::DrawingArea(), _initialized(false), _resized(false), _use_buffer(use_buffer),
    _refreshing(false)
{
  modify_bg(Gtk::STATE_NORMAL, get_style()->get_black());
}


void MGGraphPlotter::refresh()
{
  if (!_refreshing)
  {
    _refreshing= true;

    if (!_buffer || !_use_buffer)
      update();

    if (_use_buffer)
      get_window()->draw_drawable(get_style()->get_fg_gc(Gtk::STATE_NORMAL),
                                  _buffer, 0, 0, 0, 0);
    
    _refreshing= false;
  }
}


void MGGraphPlotter::update()
{
  Glib::RefPtr<Gtk::Style> style= get_style();

  if (!_initialized || _resized)
  {
    _initialized= true;
    _resized= false;

    get_window()->get_size(_width, _height);

    if (_use_buffer)
    {
      _buffer= Gdk::Pixmap::create(get_window(), _width, _height, 
                                   get_window()->get_depth());

      _buffer->draw_rectangle(style->get_bg_gc(Gtk::STATE_NORMAL), true,
                              0, 0, _width, _height);
    }
  }
  
  if (!_use_buffer)
  {
    _buffer= get_window();
  }

  _buffer->draw_line(style->get_dark_gc(Gtk::STATE_ACTIVE), 0, 0, _width, 0);
  _buffer->draw_line(style->get_dark_gc(Gtk::STATE_ACTIVE), 0, 0, 0, _height);

  _buffer->draw_line(style->get_light_gc(Gtk::STATE_ACTIVE), 0, _height-1, _width, _height-1);
  _buffer->draw_line(style->get_light_gc(Gtk::STATE_ACTIVE), _width-1, 0, _width-1, _height);
}


bool MGGraphPlotter::on_expose_event(GdkEventExpose *event)
{
  if (event->count == 0)
    refresh();

  return true;
}


bool MGGraphPlotter::on_configure_event(GdkEventConfigure *event)
{
  if (!_initialized || event->width != _width || event->height != _height)
  {
    _resized= true;
    update();
  }

  return Gtk::DrawingArea::on_configure_event(event);
}
