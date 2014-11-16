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

#include "MGAboutPanel.h"
#include "myg_utils.h"
#include "mygpriv.h"
#include <gtkmm/label.h>

#define LICENSE_STRING\
  "This software is released under the GNU General Public License (GPL),\n"\
    "which is probably the best known Open Source license. The formal\n"\
    "terms of the GPL license can be found at http://www.fsf.org/licenses/."

void MGAboutPanel::dismiss()
{
  hide();
  if (_delete_on_close)
    delete this;
}

bool MGAboutPanel::delete_event(GdkEventAny *ev)
{
  dismiss();
  return true;
}


void MGAboutPanel::toggle_credits()
{
  if (_showing_credits)
  {
    _image.set(_pabout, _mask);
    ((Gtk::Label*)_creditButton.get_child())->set_text(_("Credits"));
  }
  else
  {
    _image.set(_pcredits, _mask);
    ((Gtk::Label*)_creditButton.get_child())->set_text(_("About"));
  }
  _showing_credits=!_showing_credits;
  queue_draw();
}

    
MGAboutPanel::MGAboutPanel(const std::string &image_path,
                           const std::string &name,
                           const std::string &version,
                           const std::string &copyright_string,
                           const Glib::ustring &credits_string)
  : Gtk::Window(Gtk::WINDOW_TOPLEVEL),
    _okButton(_("OK")), _creditButton(_("Credits")),
   _showing_credits(false), _delete_on_close(false)
{
  Glib::RefPtr<Pango::Layout> layout;
  Glib::RefPtr<Gdk::Pixbuf> pixbuf;
  Glib::RefPtr<Gdk::GC> gc;
  Gdk::Color color("#505050");
  Pango::FontDescription font("sans");
  int s= 9;
  
  
  font.set_size(Pango::SCALE*s);
  realize();
  modify_font(font);

  if (create_pango_layout("Hi")->get_pixel_ink_extents().get_height() > 9)
    s-= 2;

  font.set_size(Pango::SCALE*s);
  realize();
  modify_font(font);


  
  pixbuf= Gdk::Pixbuf::create_from_file(image_path);
  pixbuf->render_pixmap_and_mask(_pcredits, _mask, 100);
  pixbuf->render_pixmap_and_mask(_pabout, _mask, 100);

  gc= Gdk::GC::create(_pcredits);
  gc->get_colormap()->alloc_color(color);
  gc->set_foreground(color);

  int w, h;
  w= pixbuf->get_width();
  h= pixbuf->get_height();

  layout= create_pango_layout(ufmt("version %s", version.c_str()));
  _pcredits->draw_layout(gc, 30, 235, layout);
  _pabout->draw_layout(gc, 30, 235, layout);

  layout= create_pango_layout(copyright_string);
  _pabout->draw_layout(gc, 30, 260, layout);

  layout= create_pango_layout(LICENSE_STRING);
  _pabout->draw_layout(gc, 30, 280, layout);

  layout= create_pango_layout(credits_string);
  _pcredits->draw_layout(gc, 30, 260, layout);
  
  _image.set(_pabout, _mask);

  set_title(ufmt(_("About %s"), name.c_str()));
  
  add(_fixed);

  _fixed.put(_frame, 0, 0);
  _frame.add(_image);

  _frame.set_size_request(w, h);

  set_position(Gtk::WIN_POS_CENTER);
  set_size_request(w, h+40);

  signal_delete_event().connect(sigc::mem_fun(*this,&MGAboutPanel::delete_event));
  _okButton.signal_clicked().connect(sigc::mem_fun(*this,&MGAboutPanel::dismiss));
  _creditButton.signal_clicked().connect(sigc::mem_fun(*this,&MGAboutPanel::toggle_credits));

  _fixed.put(_okButton, w - 100, h + 8);
  _okButton.set_size_request(80, -1);
  _fixed.put(_creditButton, 20, h + 8);
  _creditButton.set_size_request(80, -1);
  
  show_all();
}


void MGAboutPanel::set_delete_on_close()
{
  _delete_on_close= true;
}
  
