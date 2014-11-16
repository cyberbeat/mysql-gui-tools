/* Copyright (C) 2006 MySQL AB

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

#ifndef _MGCELLRENDERERTEXT_H_
#define _MGCELLRENDERERTEXT_H_


#include <gtkmm/cellrenderertext.h>

class MGCellRendererText : public Gtk::CellRendererText {
private:
  Glib::RefPtr<Gdk::Pixbuf> _null_icon;
  Glib::RefPtr<Gdk::GC> _gc;

protected:
  virtual void render_vfunc(const Glib::RefPtr<Gdk::Drawable>& window,
                            Gtk::Widget& widget,
                            const Gdk::Rectangle& background_area,
                            const Gdk::Rectangle& cell_area,
                            const Gdk::Rectangle& expose_area,
                            Gtk::CellRendererState flags);

  virtual void get_size_vfunc(Gtk::Widget& widget, const Gdk::Rectangle* cell_area, int* x_offset, int* y_offset, int* width, int* height) const;
public:
  MGCellRendererText(Glib::RefPtr<Gdk::Pixbuf> null_icon);
};

#endif /* _MGCELLRENDERERTEXT_H_ */
