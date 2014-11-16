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

#include "MGTreeTooltip.h"


MGTreeTooltip::MGTreeTooltip(Gtk::TreeView *tree)
  : _window(Gtk::WINDOW_POPUP), _tree(tree), _showing(false)
{
  _window.add(_label);
  _window.set_border_width(8);
  _window.set_resizable(false);
  _label.show();
  _label.set_alignment(0.0, 0.5);
  _label.signal_expose_event().connect_notify(sigc::mem_fun(*this,&MGTreeTooltip::expose_event));

  _window.set_double_buffered(false);
  _label.set_double_buffered(false);
  
  Gdk::Color color("#f8fcb8");
  tree->get_colormap()->alloc_color(color);
  _window.modify_bg(Gtk::STATE_NORMAL, color);

  Gdk::Color lcolor("black");
  tree->get_colormap()->alloc_color(lcolor);
  _label.modify_bg(Gtk::STATE_NORMAL, lcolor);

  tree->signal_motion_notify_event().connect_notify(sigc::mem_fun(*this,&MGTreeTooltip::motion_event));
  tree->signal_leave_notify_event().connect_notify(sigc::mem_fun(*this,&MGTreeTooltip::leave_event));
}


void MGTreeTooltip::expose_event(GdkEventExpose *event)
{
  int w, h;
  _window.get_window()->get_size(w,h);
  _window.get_window()->draw_rectangle(_window.get_style()->get_black_gc(),
                                       false, 0, 0, w-1, h-1);
}


void MGTreeTooltip::motion_event(GdkEventMotion *motion)
{
  _last_x= (gint)motion->x;
  _last_y= (gint)motion->y;

  _last_rx= (gint)motion->x_root;
  _last_ry= (gint)motion->y_root;

  if (_showing)
    update();
  else
  {
    if (!_timeout)
      _timeout= Glib::signal_timeout().connect(sigc::mem_fun(*this,&MGTreeTooltip::timeout), 1000);
  }
}


bool MGTreeTooltip::timeout()
{
  _showing= true;
  _prev_path.clear();
  update();
  return false;
}


void MGTreeTooltip::leave_event(GdkEventCrossing *event)
{
  if (_timeout)
    _timeout.disconnect();

  _window.hide();
  _showing= false;
}


void MGTreeTooltip::update()
{
  Gtk::TreePath path;
  Gtk::TreeViewColumn *column;
  int cx, cy;
  
  if (_tree->get_path_at_pos(_last_x, _last_y, path, column, cx, cy))
  {
    if (_prev_path.empty() || _prev_path != path)
    {
      if (_show_signal.emit(path))
      {
        _prev_path= path;
        _window.move(_last_rx+10, _last_ry-cy+40);
        _window.show();
        if (_window.get_window())
          _window.get_window()->clear();
        _window.queue_draw();
      }
      else
        _window.hide();
    }
  }
  else
    _window.hide();
}


void MGTreeTooltip::set_text(const Glib::ustring &text)
{
  if (_window.get_window())
    _window.get_window()->clear();
  _label.set_markup(text);
}
