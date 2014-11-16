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


#include "MGImageCheckButton.h"


MGImageCheckButton::MGImageCheckButton(const Glib::RefPtr<Gdk::Pixbuf> &off_img,
                                       const Glib::RefPtr<Gdk::Pixbuf> &on_img)
  : Gtk::CheckButton(), _on_img(on_img), _off_img(off_img)
{
  set_focus_on_click(false);
  
  set_size_request(on_img->get_width()+4, on_img->get_height()+4);
}


void MGImageCheckButton::draw_indicator_vfunc(GdkRectangle* area)
{
  Gtk::Allocation alloc= get_allocation();
  
  get_window()->draw_pixbuf(get_style()->get_fg_gc(Gtk::STATE_NORMAL), 
                            get_active()?_on_img:_off_img, 0, 0,
                            alloc.get_x() + (get_width()-_on_img->get_width())/2,
                            alloc.get_y() + (get_height()-_on_img->get_height())/2,
                            -1, -1, Gdk::RGB_DITHER_NORMAL,
                            0, 0);
}




MGImageRadioButton::MGImageRadioButton(Gtk::RadioButtonGroup &group,
                                       const Glib::RefPtr<Gdk::Pixbuf> &off_img,
                                       const Glib::RefPtr<Gdk::Pixbuf> &on_img)
  : Gtk::RadioButton(group), _on_img(on_img), _off_img(off_img)
{
  set_size_request(on_img->get_width()+4, on_img->get_height()+4);
}


void MGImageRadioButton::draw_indicator_vfunc(GdkRectangle* area)
{
  Gtk::Allocation alloc= get_allocation();
  
  get_window()->draw_pixbuf(get_style()->get_fg_gc(Gtk::STATE_NORMAL), 
                            get_active()?_on_img:_off_img, 0, 0,
                            alloc.get_x() + (get_width()-_on_img->get_width())/2,
                            alloc.get_y() + (get_height()-_on_img->get_height())/2,
                            -1, -1, Gdk::RGB_DITHER_NORMAL,
                            0, 0);
}

