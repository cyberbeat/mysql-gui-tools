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

#ifndef _MGABOUTPANEL_H_
#define _MGABOUTPANEL_H_

#include <gtkmm/window.h>
#include <gtkmm/image.h>
#include <gtkmm/button.h>
#include <gtkmm/frame.h>
#include <gtkmm/fixed.h>

class MGAboutPanel : public Gtk::Window {
    Gtk::Fixed _fixed;
    Gtk::Frame _frame;
    Gtk::Image _image;
    Gtk::Button _okButton;
    Gtk::Button _creditButton;

    Glib::RefPtr<Gdk::Pixmap> _pcredits, _pabout;
    Glib::RefPtr<Gdk::Bitmap> _mask;
    
    bool _showing_credits;
    
    bool _delete_on_close;

    void toggle_credits();

    void dismiss();
    bool delete_event(GdkEventAny *ev);
    
  public:
    MGAboutPanel(const std::string &image_path,
                 const std::string &name,
                 const std::string &version,
                 const std::string &copyright_string,
                 const Glib::ustring &credits_string);
    
    void set_delete_on_close();
};


#endif /* _MGABOUTPANEL_H_ */
