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

#ifndef _MGIMAGE_H_
#define _MGIMAGE_H_

#include <gtkmm/image.h>

class MGImage : public Gtk::Image {
    Glib::RefPtr<Gdk::Pixbuf> _normal;
    Glib::RefPtr<Gdk::Pixbuf> _prelight;
    Glib::RefPtr<Gdk::Pixbuf> _disabled;
    Glib::RefPtr<Gdk::Pixbuf> _active;

    void state_changed(Gtk::StateType state);
  public:
    MGImage(GtkImage *image);
    MGImage();
    void set_image(Gtk::StateType state, const Glib::RefPtr<Gdk::Pixbuf> &icon);
};


#endif /* _MGIMAGE_H_ */
