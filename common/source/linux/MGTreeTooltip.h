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

#ifndef _MGTREETOOLTIP_H_
#define _MGTREETOOLTIP_H_

#include <gtkmm/window.h>
#include <gtkmm/treeview.h>
#include <gtkmm/label.h>

class MGTreeTooltip : public Glib::Object {
  public:
    typedef sigc::signal1<bool,const Gtk::TreePath&> WillShowSignal;

  protected:
    Gtk::Window _window;
    Gtk::Label _label;

    Gtk::TreeView *_tree;

    bool _showing;
    
    gint _last_x, _last_y;
    gint _last_rx, _last_ry;
    Gtk::TreePath _prev_path;
    
    WillShowSignal _show_signal;
    
    sigc::connection _timeout;

    void expose_event(GdkEventExpose *event);
    void leave_event(GdkEventCrossing *event);
    void motion_event(GdkEventMotion *motion);
    bool timeout();
    
    void update();

  public:
    MGTreeTooltip(Gtk::TreeView *tree);
    WillShowSignal signal_will_show() { return _show_signal; };
    
    void set_text(const Glib::ustring &text);
};

#endif /* _MGTREETOOLTIP_H_ */
