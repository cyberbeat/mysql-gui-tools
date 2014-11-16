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

#ifndef _MQINDICATORCELLRENDERER_H_
#define _MQINDICATORCELLRENDERER_H_


#include <gtkmm/cellrenderer.h>
#include <vector>

class MQIndicatorCellRenderer : public Gtk::CellRenderer {
  private:    
    Glib::Property<bool> _property_active;

  protected:
    virtual void get_size_vfunc(Gtk::Widget& widget,
                                const Gdk::Rectangle* cell_area,
                                int* x_offset, int* y_offset,
                                int* width, int* height);
    
    virtual void render_vfunc(const Glib::RefPtr<Gdk::Window>& window,
                              Gtk::Widget& widget,
                              const Gdk::Rectangle& background_area,
                              const Gdk::Rectangle& cell_area,
                              const Gdk::Rectangle& expose_area,
                              Gtk::CellRendererState flags);
    
    virtual bool activate_vfunc(GdkEvent* event,
                                Gtk::Widget& widget,
                                const Glib::ustring& path,
                                const Gdk::Rectangle& background_area,
                                const Gdk::Rectangle& cell_area,
                                Gtk::CellRendererState flags);

  public:
    MQIndicatorCellRenderer();
    
    Glib::PropertyProxy<bool> property_active();
};

#endif /* _MQINDICATORCELLRENDERER_H_ */
