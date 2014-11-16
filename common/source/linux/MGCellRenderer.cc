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



#include "MGCellRenderer.h"


MGCellRendererTristate::MGCellRendererTristate(const PixbufList &images)
:
  Glib::ObjectBase(typeid(MGCellRendererTristate)),
  Gtk::CellRenderer(),
  _pixbuf_list(images),
  _property_activatable(*this, "activatable", true),
  _property_state(*this, "state", OFF)
{
  property_mode()= Gtk::CELL_RENDERER_MODE_ACTIVATABLE;
  property_xpad()= 2;
  property_ypad()= 2;
}


MGCellRendererTristate::MGCellRendererTristate(const ImageList &images)
:
  Glib::ObjectBase(typeid(MGCellRendererTristate)),
  Gtk::CellRenderer(),
  _image_list(images),
  _property_activatable(*this, "activatable", true),
  _property_state(*this, "state", OFF)
{
  property_mode()= Gtk::CELL_RENDERER_MODE_ACTIVATABLE;
  property_xpad()= 2;
  property_ypad()= 2;
}


MGCellRendererTristate::~MGCellRendererTristate()
{
}


Glib::PropertyProxy<bool> MGCellRendererTristate::property_activatable()
{
  return _property_activatable.get_proxy();
}


Glib::PropertyProxy<MGCellRendererTristate::State> MGCellRendererTristate::property_state()
{
  return _property_state.get_proxy();
}


sigc::signal1<void, const Glib::ustring&>& MGCellRendererTristate::signal_toggled()
{
  return _signal_toggled;
}


void MGCellRendererTristate::get_size_vfunc(Gtk::Widget&,
                                            const Gdk::Rectangle* cell_area,
                                            int* x_offset, int* y_offset,
                                            int* width, int* height) const
{
  int calc_width= property_xpad() * 2;
  int calc_height= property_ypad() * 2;
  
  if (!_image_list.empty())
  {
    int w, h;
    _image_list[0]->get_size(w,h);
    calc_width+= 12;
    calc_height+= 12;
  }
  else
  {
    calc_width+= _pixbuf_list[0]->get_width();
    calc_height+= _pixbuf_list[0]->get_height();
  }

  if (width)
    *width = calc_width;

  if (height)
    *height = calc_height;

  if (cell_area)
  {
    if (x_offset)
    {
      *x_offset= int(property_xalign() * (cell_area->get_width() - calc_width));
      *x_offset= std::max(0, *x_offset);
    }

    if (y_offset)
    {
      *y_offset= int(property_yalign() * (cell_area->get_height() - calc_height));
      *y_offset= std::max(0, *y_offset);
    }
  }
}


void MGCellRendererTristate::render_vfunc(const Glib::RefPtr<Gdk::Drawable>& drawable,
                                        Gtk::Widget& widget,
                                        const Gdk::Rectangle&,
                                        const Gdk::Rectangle& cell_area,
                                        const Gdk::Rectangle&,
                                        Gtk::CellRendererState flags)
{
  const unsigned int cell_xpad = property_xpad();
  const unsigned int cell_ypad = property_ypad();

  int x_offset = 0, y_offset = 0, width = 0, height = 0;
  get_size(widget, cell_area, x_offset, y_offset, width, height);
  
  width  -= cell_xpad * 2;
  height -= cell_ypad * 2;

  if (width <= 0 || height <= 0)
    return;

  Gtk::StateType state = Gtk::STATE_INSENSITIVE;

  if (_property_activatable)
    state = Gtk::STATE_NORMAL;
  
  if ((flags & Gtk::CELL_RENDERER_SELECTED) != 0)
    state = (widget.has_focus()) ? Gtk::STATE_SELECTED : Gtk::STATE_ACTIVE;

  if (!_image_list.empty())
  {
    Glib::RefPtr<Gdk::Pixmap> image;
    
    switch (_property_state)
    {
    case OFF: image= _image_list[0]; break;
    case MAYBE: image= _image_list[1]; break;
    case ON: image= _image_list[2]; break;
    default: g_assert_not_reached(); break;
    }

    int iw, ih;
    image->get_size(iw, ih);
  
    drawable->draw_drawable(widget.get_style()->get_fg_gc(state),
                          image, 
                          0, 0,
                          cell_area.get_x() + x_offset + cell_xpad + (width - iw)/2,
                          cell_area.get_y() + y_offset + cell_ypad + (height - ih)/2);
  }
  else
  {
    Glib::RefPtr<Gdk::Pixbuf> image;
    
    switch (_property_state)
    {
    case OFF: image= _pixbuf_list[0]; break;
    case MAYBE: image= _pixbuf_list[1]; break;
    case ON: image= _pixbuf_list[2]; break;
    default: g_assert_not_reached(); break;
    }

    int iw, ih;
    iw= image->get_width();
    ih= image->get_height();

    image->render_to_drawable(drawable, widget.get_style()->get_fg_gc(state),
                              0, 0,
                              cell_area.get_x() + x_offset + cell_xpad + (width - iw)/2,
                              cell_area.get_y() + y_offset + cell_ypad + (height - ih)/2,
                              -1, -1, 
                              Gdk::RGB_DITHER_NORMAL, 0, 0);
  }
}


bool MGCellRendererTristate::activate_vfunc(GdkEvent*,
                                          Gtk::Widget&,
                                          const Glib::ustring& path,
                                          const Gdk::Rectangle&,
                                          const Gdk::Rectangle&,
                                          Gtk::CellRendererState)
{
  if (_property_activatable)
  {
    _signal_toggled(path);
    return true;
  }

  return false;
}



//-----------------------------------------------------------------------------


Gtk::CellEditable* MGCellRendererText::start_editing_vfunc(GdkEvent* event,
                                                           Gtk::Widget& widget,
                                                           const Glib::ustring& path,
                                                           const Gdk::Rectangle& background_area,
                                                           const Gdk::Rectangle& cell_area,
                                                           Gtk::CellRendererState flags)
{
  Gtk::CellEditable *cell= Gtk::CellRendererText::start_editing_vfunc(event,
                                                                      widget,
                                                                      path,
                                                                      background_area,
                                                                      cell_area,
                                                                      flags);

  if (_completer)
    dynamic_cast<Gtk::Entry*>(cell)->set_completion(_completer);
 
  return cell;
}


void MGCellRendererText::set_completer(Glib::RefPtr<Gtk::EntryCompletion> completer)
{
  _completer= completer;
}


bool MGCellRendererText::activate_vfunc(GdkEvent* event, Gtk::Widget& widget,
                                        const Glib::ustring& path,
                                        const Gdk::Rectangle& background_area,
                                        const Gdk::Rectangle& cell_area,
                                        Gtk::CellRendererState flags)
{
  _signal_clicked.emit(path);

  return !Gtk::CellRendererText::activate_vfunc(event, widget,
                                                path,
                                                background_area,
                                                cell_area,
                                                flags);
}
