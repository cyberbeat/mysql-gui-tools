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

#include "MGCellRendererBlob.h"
#include "myx_public_interface.h"
#include "myg_gtkutils.h"
#include "myg_utils.h"
#include "myqb.h"
#include "MGBlobEditor.h"

#include <gtkmm.h>

CellRendererBlob::CellRendererBlob(bool binary)
  : Glib::ObjectBase(typeid(CellRendererBlob)),
  Gtk::CellRenderer(),
  _property_field(*this, "field", ""),
  _property_editable(*this, "editable", false),
  _property_background(*this, "background", ""),
  _property_column(*this, "column", 0),
  _binary(binary)
{
  _max_text_length= 100;

  property_mode()= Gtk::CELL_RENDERER_MODE_ACTIVATABLE;
}


CellRendererBlob::~CellRendererBlob()
{
}


void CellRendererBlob::set_blob_icon(const Glib::RefPtr<Gdk::Pixbuf> &icon)
{
  _blob_icon= icon;
}


void CellRendererBlob::set_overlay_icons(const Glib::RefPtr<Gdk::Pixbuf> &clear_icon,
                                         const Glib::RefPtr<Gdk::Pixbuf> &edit_icon,
                                         const Glib::RefPtr<Gdk::Pixbuf> &load_icon,
                                         const Glib::RefPtr<Gdk::Pixbuf> &save_icon,
                                         const Glib::RefPtr<Gdk::Pixbuf> &view_icon,
                                         const Glib::RefPtr<Gdk::Pixbuf> &null_icon)
{
  _clear_icon= clear_icon;
  _edit_icon= edit_icon;
  _load_icon= load_icon;
  _save_icon= save_icon;
  _view_icon= view_icon;
  _null_icon= null_icon;
}


void CellRendererBlob::set_max_text_width(guint width)
{
  _max_text_length= width;
}


Glib::PropertyProxy<std::string> CellRendererBlob::property_field() const
{
  return _property_field.get_proxy();
}


Glib::PropertyProxy<bool> CellRendererBlob::property_editable()
{
  return _property_editable.get_proxy();
}


Glib::PropertyProxy<std::string> CellRendererBlob::property_background()
{
  return _property_background.get_proxy();
}


Glib::PropertyProxy<int> CellRendererBlob::property_column()
{
  return _property_column.get_proxy();
}


void CellRendererBlob::get_size_vfunc(Gtk::Widget& widget,
                                      const Gdk::Rectangle* cell_area,
                                      int* x_offset, int* y_offset,
                                      int* width,    int* height) const
{
  gpointer fdata;
  gsize fsize;
  
  str2data(((std::string)property_field()).c_str(), &fdata, &fsize);
  
  if (!_layout)
  {
    _layout= widget.create_pango_layout("ABC123j");
    _layout->get_pixel_size(_text_char_width, _text_height);
    _text_char_width/= 7;
  }
  
  if (_binary)
  {
    if (width)
      *width= _blob_icon->get_width()+4;
    if (height)
      *height= _blob_icon->get_height();
    if (x_offset)
      *x_offset= 0;
    if (y_offset)
      *y_offset= 0;
  }
  else
  {
    //XXX
    if (width)
    {
      if (_max_text_width >= _max_text_length * _text_char_width)
        *width= _max_text_length * _text_char_width;
      else
      {
        int h;
        Glib::ustring text;
    
        if (fdata)
        {
          if (fsize > _max_text_length)
            text= Glib::ustring((char*)fdata, 0, _max_text_length)+"...";
          else
            text= Glib::ustring((char*)fdata);

          text= strreplace(text, "\r\n", "\xc2\xab\xc2\xb6");
          text= strreplace(text, "\n", "\xc2\xb6");

          _layout->set_text(text);

          _layout->get_pixel_size(*width, h);
        
          if (*width > (int)_max_text_width)
            _max_text_width= *width;
        }
        else
          *width= _max_text_length * _text_char_width;
      }
    }
    
    if (height)
      *height= _text_height;
  }

  if (width)
    *width+= 5 * (_clear_icon->get_width()+4);
}


void CellRendererBlob::render_vfunc(const Glib::RefPtr<Gdk::Drawable>& window, Gtk::Widget& widget,
                                    const Gdk::Rectangle& background_area, const Gdk::Rectangle& cell_area,
                                    const Gdk::Rectangle& expose_area, Gtk::CellRendererState flags)
//void CellRendererBlob::render_vfunc(const Glib::RefPtr<Gdk::Window>& window, Gtk::Widget& widget,
//                                    const Gdk::Rectangle& background_area, const Gdk::Rectangle& cell_area,
//                                    const Gdk::Rectangle& expose_area, Gtk::CellRendererState flags)
{
  gpointer fdata;
  gsize fsize;
  Gdk::Color color;
  std::string bgcolor= property_background();

  if (!_gc)
    _gc= Gdk::GC::create(window);

  if (!bgcolor.empty())
  {
    color= Gdk::Color(bgcolor.c_str());
    widget.get_colormap()->alloc_color(color);
    _gc->set_foreground(color);

    window->draw_rectangle(_gc, true,
                           background_area.get_x(), background_area.get_y(),
                           background_area.get_width(), background_area.get_height());
  }

  str2data(((std::string)property_field()).c_str(), &fdata, &fsize);

  if (_binary)
  {
    if (fdata && fsize > 0)
      window->draw_pixbuf(_gc, _blob_icon, 0, 0, 
                          cell_area.get_x(), cell_area.get_y()+(cell_area.get_height()-_blob_icon->get_height())/2, -1, -1,
                          Gdk::RGB_DITHER_NONE, 0, 0);
  }
  else
  {    
    if (fdata)
    {
      Glib::ustring text;
      
      if (fsize > _max_text_length)
        text= Glib::ustring((char*)fdata, 0, _max_text_length)+"...";
      else
        text= Glib::ustring((char*)fdata);

      text= strreplace(text, "\r\n", "\xc2\xab\xc2\xb6");
      text= strreplace(text, "\n", "\xc2\xb6");

      _layout->set_text(text);

      Gtk::StateType state= Gtk::STATE_NORMAL;

      if ((flags & Gtk::CELL_RENDERER_SELECTED) != 0)
        state= (widget.has_focus()) ? Gtk::STATE_SELECTED : Gtk::STATE_ACTIVE;

      window->draw_layout(widget.get_style()->get_text_gc(state), cell_area.get_x(), cell_area.get_y() + (cell_area.get_height() - _text_height)/2, _layout);
    }
  }

  if (property_editable())
  {
    int x= cell_area.get_x()+cell_area.get_width();
    
    if (fdata)
    {
      x-= _clear_icon->get_width()+4;
      window->draw_pixbuf(_gc, _clear_icon, 0, 0, 
                          x, cell_area.get_y()+(cell_area.get_height()-_clear_icon->get_height())/2, -1, -1,
                          Gdk::RGB_DITHER_NONE, 0, 0);
    }

    x-= _load_icon->get_width()+4;
    window->draw_pixbuf(_gc, _load_icon, 0, 0,
                        x, cell_area.get_y()+(cell_area.get_height()-_load_icon->get_height())/2, -1, -1,
                        Gdk::RGB_DITHER_NONE, 0, 0);

    x-= _edit_icon->get_width()+4;
    window->draw_pixbuf(_gc, _edit_icon, 0, 0,
                        x, cell_area.get_y()+(cell_area.get_height()-_edit_icon->get_height())/2, -1, -1,
                        Gdk::RGB_DITHER_NONE, 0, 0);

    x-= _save_icon->get_width()+4;
    window->draw_pixbuf(_gc, _save_icon, 0, 0,
                        x, cell_area.get_y()+(cell_area.get_height()-_save_icon->get_height())/2, -1, -1,
                        Gdk::RGB_DITHER_NONE, 0, 0);
    
    x-= _view_icon->get_width()+4;
    window->draw_pixbuf(_gc, _view_icon, 0, 0,
                        x, cell_area.get_y()+(cell_area.get_height()-_view_icon->get_height())/2, -1, -1,
                        Gdk::RGB_DITHER_NONE, 0, 0);
  }
  else
  {
    int x= cell_area.get_x()+cell_area.get_width();

    x-= _save_icon->get_width()+4;
    window->draw_pixbuf(_gc, _save_icon, 0, 0,
                        x, cell_area.get_y()+(cell_area.get_height()-_save_icon->get_height())/2, -1, -1,
                        Gdk::RGB_DITHER_NONE, 0, 0);
    
    x-= _view_icon->get_width()+4;
    window->draw_pixbuf(_gc, _view_icon, 0, 0,
                        x, cell_area.get_y()+(cell_area.get_height()-_view_icon->get_height())/2, -1, -1,
                        Gdk::RGB_DITHER_NONE, 0, 0);
  }

  if (!fdata)
      window->draw_pixbuf(_gc, _null_icon, 0, 0, 
                          cell_area.get_x(), cell_area.get_y()+(cell_area.get_height()-_null_icon->get_height())/2, -1, -1,
                          Gdk::RGB_DITHER_NONE, 0, 0);
}


bool CellRendererBlob::activate_vfunc(GdkEvent* event,
                                      Gtk::Widget& widget,
                                      const Glib::ustring& path,
                                      const Gdk::Rectangle& background_area,
                                      const Gdk::Rectangle& cell_area,
                                      Gtk::CellRendererState flags)
{
  gpointer fdata; 
  gsize fsize; 
  int cx;
  
  str2data(((std::string)property_field()).c_str(), &fdata, &fsize);

  if (event && event->type == GDK_BUTTON_PRESS)
  {
    cx= (int)event->button.x;

    if (property_editable())
    {
      int x= cell_area.get_x()+cell_area.get_width();

      if (fdata)
      {
        x-= _clear_icon->get_width()+4;
        if (cx >= x && cx < x+_clear_icon->get_width())
        {
          clear_clicked(widget.get_parent(), path);
          return true;
        }
      }
        
      x-= _load_icon->get_width()+4;
      if (cx >= x && cx < x+_load_icon->get_width())
      {
        load_clicked(widget.get_parent(), path);
        return true;
      }

      x-= _edit_icon->get_width()+4;
      if (cx >= x && cx < x+_edit_icon->get_width())
      {
        edit_clicked(widget.get_parent(), path);
        return true;
      }

      
      x-= _save_icon->get_width()+4;
      if (cx >= x && cx < x+_save_icon->get_width())
      {
        save_clicked(widget.get_parent(), path);
        return true;
      }

      x-= _view_icon->get_width()+4;
      if (cx >= x && cx < x+_view_icon->get_width())
      {
        view_clicked(widget.get_parent(), path);
        return true;
      }
    }
    else
    {
      int x= cell_area.get_x()+cell_area.get_width();
      
      x-= _save_icon->get_width()+4;
      if (cx >= x && cx < x+_save_icon->get_width())
      {
        save_clicked(widget.get_parent(), path);
        return true;
      }

      x-= _view_icon->get_width()+4;
      if (cx >= x && cx < x+_view_icon->get_width())
      {
        view_clicked(widget.get_parent(), path);
        return true;
      }
    }
  }
  return false;
}


void CellRendererBlob::clear_clicked(Gtk::Widget *parent, const Glib::ustring &path)
{
  _delegate->blob_clear(this, parent, path, property_column());
}


void CellRendererBlob::save_clicked(Gtk::Widget *parent, const Glib::ustring &path)
{
  _delegate->blob_save(this, parent, path, property_column());
}


void CellRendererBlob::load_clicked(Gtk::Widget *parent, const Glib::ustring &path)
{
  _delegate->blob_load(this, parent, path, property_column());
}


void CellRendererBlob::view_clicked(Gtk::Widget *parent, const Glib::ustring &path)
{
  _delegate->blob_view(this, parent, path, property_column());
}


void CellRendererBlob::edit_clicked(Gtk::Widget *parent, const Glib::ustring &path)
{
  _delegate->blob_edit(this, parent, path, property_column());
}
