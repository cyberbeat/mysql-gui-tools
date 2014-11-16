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

#ifndef _MGCELLRENDERER_H_
#define _MGCELLRENDERER_H_

#include <gtkmm.h>
#include <vector>

class MGCellRendererTristate : public Gtk::CellRenderer {
  public:
    enum State {
      OFF,
      MAYBE,
      ON
    };
    
    typedef std::vector<Glib::RefPtr<Gdk::Pixmap> > ImageList;
    typedef std::vector<Glib::RefPtr<Gdk::Pixbuf> > PixbufList;

  private:
    ImageList _image_list;
    PixbufList _pixbuf_list;
    
    Glib::Property<bool> _property_activatable;
    Glib::Property<State> _property_state;

    sigc::signal1<void, const Glib::ustring&> _signal_toggled;

  protected:
    virtual void get_size_vfunc(Gtk::Widget& widget,
                                const Gdk::Rectangle* cell_area,
                                int* x_offset, int* y_offset,
                                int* width, int* height) const;
    
    virtual void render_vfunc(const Glib::RefPtr<Gdk::Drawable>& drawable,
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
    MGCellRendererTristate(const ImageList &images);
    MGCellRendererTristate(const PixbufList &images);
    virtual ~MGCellRendererTristate();
    
    Glib::PropertyProxy<bool> property_activatable();
    Glib::PropertyProxy<State> property_state();
    
    sigc::signal1<void, const Glib::ustring&>& signal_toggled();
};


class MGCellRendererText : public Gtk::CellRendererText 
{
    Glib::RefPtr<Gtk::EntryCompletion> _completer;

    sigc::signal1<void, const Glib::ustring&> _signal_clicked;

    virtual bool activate_vfunc(GdkEvent* event, Gtk::Widget& widget,
                                const Glib::ustring& path,
                                const Gdk::Rectangle& background_area,
                                const Gdk::Rectangle& cell_area,
                                Gtk::CellRendererState flags);
    virtual Gtk::CellEditable* start_editing_vfunc(GdkEvent* event,
                                                   Gtk::Widget& widget,
                                                   const Glib::ustring& path,
                                                   const Gdk::Rectangle& background_area,
                                                   const Gdk::Rectangle& cell_area,
                                                   Gtk::CellRendererState flags);
  public:
    void set_completer(Glib::RefPtr<Gtk::EntryCompletion> completer);
   
    sigc::signal1<void, const Glib::ustring&>& signal_clicked() { return _signal_clicked; };
};


#endif /* _MGCELLRENDERER_H_ */
