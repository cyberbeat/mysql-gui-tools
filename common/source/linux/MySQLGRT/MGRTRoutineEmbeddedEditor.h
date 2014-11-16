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

#ifndef _MGRTROUTINEEMBEDDEDEDITOR_H_
#define _MGRTROUTINEEMBEDDEDEDITOR_H_

#include <MySQLGRT/MGRTRoutine.h>
#include <gtkmm/box.h>
#include <gtkmm/button.h>
#include <gtkmm/stock.h>
#include <gtkmm/entry.h>
#include <gtkmm/textview.h>
#include <gtkmm/frame.h>

class MGRT;

class MGRTRoutineEmbeddedEditor : public Gtk::Frame {
    friend class MGRTRoutineGroupEditor;
    
    MGRT *_grt;

    MGRTValue _routine;

    Gtk::VBox _vbox;
    Gtk::Button _down_button;
    Gtk::Button _right_button;
    Gtk::Label _name_label;
    Gtk::Button _close_button;
    Gtk::TextView _text;

    void open_code();
    void close_code();
    
    void show_object();

  public:
    MGRTRoutineEmbeddedEditor(MGRT *grt, MGRTValue routine);
    ~MGRTRoutineEmbeddedEditor();

    void apply_changes();
    void discard_changes();
    void close();
    
    Glib::SignalProxy0<void> signal_close() { return _close_button.signal_clicked(); }
};

#endif /* _MGRTROUTINEEMBEDDEDEDITOR_H_ */
