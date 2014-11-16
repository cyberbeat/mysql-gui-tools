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


#ifndef _MGIMAGEBUTTON_H_
#define _MGIMAGEBUTTON_H_

#include <gtkmm/button.h>

class MGImageButton : public Gtk::Button {
  protected:
    virtual bool on_expose_event(GdkEventExpose* event);
    
    Glib::RefPtr<Gdk::Pixbuf> _normalIcon;
    Glib::RefPtr<Gdk::Pixbuf> _pushedIcon;
    Glib::RefPtr<Gdk::Pixbuf> _hoverIcon;

    bool _pressed;
    bool _inside;
    
    void on_pressed(bool flag);
    void on_entered(bool flag);

  public:
    MGImageButton();
    void set_image(Glib::RefPtr<Gdk::Pixbuf> image);
    void set_alt_image(Glib::RefPtr<Gdk::Pixbuf> image);
    void set_hover_image(Glib::RefPtr<Gdk::Pixbuf> image);
};


#endif /* _MGIMAGEBUTTON_H_ */
