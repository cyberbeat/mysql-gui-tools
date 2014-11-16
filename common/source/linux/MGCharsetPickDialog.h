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

#ifndef _MGCHARSETPICKDIALOG_H_
#define _MGCHARSETPICKDIALOG_H_

#include <gtkmm/dialog.h>
#include <gtkmm/label.h>
#include <gtkmm/optionmenu.h>
#include <gtkmm/box.h>
#include <gtkmm/button.h>

class MGCharsetPickDialog : public Gtk::Dialog {
    Gtk::Label _message;
    Gtk::HBox _hbox;
    Gtk::OptionMenu _option;
    Gtk::Button _detect;
    
    sigc::slot1<bool,Glib::ustring&> _detect_func;

    void autodetect();

  public:
    MGCharsetPickDialog();
    
    void set_detect_function(const sigc::slot1<bool,Glib::ustring&> &func);
    
    Glib::ustring show(const Glib::ustring &message);
};

#endif /* _MGCHARSETPICKDIALOG_H_ */
