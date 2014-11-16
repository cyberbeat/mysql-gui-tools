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

#include "MGImage.h"

MGImage::MGImage(GtkImage *image)
  : Gtk::Image(image)
{
  signal_state_changed().connect(sigc::mem_fun(*this,&MGImage::state_changed));
}


MGImage::MGImage()
  : Gtk::Image()
{
  signal_state_changed().connect(sigc::mem_fun(*this,&MGImage::state_changed));
}


void MGImage::set_image(Gtk::StateType state, const Glib::RefPtr<Gdk::Pixbuf> &icon)
{
  switch (state) 
  {
  case Gtk::STATE_NORMAL:
    _normal= icon;
    break;
  case Gtk::STATE_INSENSITIVE:
    _disabled= icon;
    break;
  case Gtk::STATE_ACTIVE:
    _active= icon;
    break;
  case Gtk::STATE_PRELIGHT:
    _prelight= icon;
    break;
  default:
    break;
  }
  
  state_changed(get_state());
}


void MGImage::state_changed(Gtk::StateType type)
{
  switch (type)
  {
  case Gtk::STATE_NORMAL:
    set(_normal);
    break;
  case Gtk::STATE_INSENSITIVE:
    set(_disabled);
    break;
  case Gtk::STATE_ACTIVE:
    set(_active);
    break;
  case Gtk::STATE_PRELIGHT:
    set(_prelight);
    break;
  default:
    break;
  }
}
