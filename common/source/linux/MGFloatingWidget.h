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

#ifndef _MGFLOATINGWIDGET_H_
#define _MGFLOATINGWIDGET_H_

#include <gtkmm/window.h>


class MGFloatingWidget : public Gtk::Window {
    typedef Gtk::Window superclass;

  protected:
    Glib::RefPtr<Gdk::Pixbuf> _background;

    bool _mouse_inside;
    
    virtual void update_mouse(int x, int y);

    virtual void get_background(int x, int y);
    virtual void paint_contents()= 0;

    virtual bool on_expose_event(GdkEventExpose *event);
    virtual bool on_enter_notify_event(GdkEventCrossing *event);
    virtual bool on_leave_notify_event(GdkEventCrossing *event);
    virtual bool on_motion_notify_event(GdkEventMotion *event);
    virtual bool on_button_release_event(GdkEventButton *event);
    
  public:
    MGFloatingWidget();
    virtual bool show_at(int x, int y);
};


#endif /* _MGFLOATINGWIDGET_H_ */
