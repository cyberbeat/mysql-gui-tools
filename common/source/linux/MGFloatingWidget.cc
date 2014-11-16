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


#include "MGFloatingWidget.h"

#include <gtkmm/main.h>


MGFloatingWidget::MGFloatingWidget()
  : Gtk::Window(Gtk::WINDOW_POPUP)
{
  add_events(Gdk::BUTTON_MOTION_MASK);
}


void MGFloatingWidget::get_background(int x, int y)
{
  int w, h;
  Glib::RefPtr<Gdk::Drawable> root= get_root_window();

  get_size_request(w, h);

  _background= Gdk::Pixbuf::create(root, get_colormap(),
                                   x, y, 0, 0, w, h);
}

bool MGFloatingWidget::on_enter_notify_event(GdkEventCrossing *event)
{
  _mouse_inside= true;

  update_mouse((int)event->x, (int)event->y);

  return superclass::on_enter_notify_event(event);
}


bool MGFloatingWidget::on_leave_notify_event(GdkEventCrossing *event)
{
  _mouse_inside= false;

  update_mouse(-1, -1);
  
  return superclass::on_leave_notify_event(event);
}


void MGFloatingWidget::update_mouse(int x, int y)
{
}


bool MGFloatingWidget::on_expose_event(GdkEventExpose *event)
{
  paint_contents();
  
  return true;
}
    

bool MGFloatingWidget::on_motion_notify_event(GdkEventMotion *event)
{
  if (_mouse_inside)
  {
    int x, y;
    Gdk::ModifierType state= Gdk::ModifierType(0);
    get_window()->get_pointer(x, y, state);
    
    update_mouse(x, y);
  }
  
  return superclass::on_motion_notify_event(event);
}


bool MGFloatingWidget::on_button_release_event(GdkEventButton *event)
{
  Gtk::Main::instance()->quit();

  return superclass::on_button_release_event(event);
}


bool MGFloatingWidget::show_at(int x, int y)
{
  int w, h;

  get_size_request(w, h);

  get_background(x, y);

  if (!get_window())
  {
    realize();
    add_events(Gdk::POINTER_MOTION_MASK|Gdk::POINTER_MOTION_HINT_MASK|
               Gdk::ENTER_NOTIFY_MASK|Gdk::LEAVE_NOTIFY_MASK|
               Gdk::EXPOSURE_MASK|
               Gdk::BUTTON_PRESS_MASK|Gdk::BUTTON_RELEASE_MASK);
  }
  get_window()->move(x, y);
  
  paint_contents();

  show();

  get_window()->pointer_grab(false,
                             Gdk::POINTER_MOTION_MASK|Gdk::POINTER_MOTION_HINT_MASK|
                             Gdk::BUTTON_MOTION_MASK|
                             Gdk::BUTTON1_MOTION_MASK|
                             Gdk::ENTER_NOTIFY_MASK|Gdk::LEAVE_NOTIFY_MASK|
                             Gdk::BUTTON_RELEASE_MASK,
                             /*Gdk::CURRENT_TIME*/0);

  _mouse_inside= true;
  Gtk::Main::instance()->run();

  hide();

  get_window()->pointer_ungrab(/*Gdk::CURRENT_TIME*/0);

  return _mouse_inside;
}
