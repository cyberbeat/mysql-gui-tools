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

#ifndef _MGIMAGECHECKBUTTON_H_
#define _MGIMAGECHECKBUTTON_H_

#include <gtkmm/checkbutton.h>
#include <gtkmm/radiobutton.h>
#include <gdkmm/pixbuf.h>

class MGImageCheckButton : public Gtk::CheckButton {
  private:
    Glib::RefPtr<Gdk::Pixbuf> _on_img;
    Glib::RefPtr<Gdk::Pixbuf> _off_img;

    virtual void draw_indicator_vfunc(GdkRectangle* area);

  public:
    MGImageCheckButton(const Glib::RefPtr<Gdk::Pixbuf> &off_img,
                       const Glib::RefPtr<Gdk::Pixbuf> &on_img);
};



class MGImageRadioButton : public Gtk::RadioButton {
  private:
    Glib::RefPtr<Gdk::Pixbuf> _on_img;
    Glib::RefPtr<Gdk::Pixbuf> _off_img;
    
    virtual void draw_indicator_vfunc(GdkRectangle* area);

  public:
    MGImageRadioButton(Gtk::RadioButtonGroup &group,
                       const Glib::RefPtr<Gdk::Pixbuf> &off_img,
                       const Glib::RefPtr<Gdk::Pixbuf> &on_img);
};


#endif /* _MGIMAGECHECKBUTTON_H_ */
