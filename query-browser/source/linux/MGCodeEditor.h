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

#ifndef _MGCODEEDITOR_H_
#define _MGCODEEDITOR_H_

#include <gtkmm/scrolledwindow.h>
#include <gtkmm/textview.h>
#include <gtksourceview/gtksourceview.h>

#include <map>

class MGCodeEditor : public Glib::ObjectBase {
    Gtk::ScrolledWindow _swin;
    Gtk::TextView *_wrapper; // this is a wrapper for the srcv

    GtkWidget *_srcv;
    GtkSourceBuffer *_buffer;

    GtkSourceMarker *_pc_marker;

    std::map<int,GtkSourceMarker*> _st_markers;
    
    Glib::RefPtr<Gdk::Pixbuf> _pc_pixbuf;
    Glib::RefPtr<Gdk::Pixbuf> _statement_pixbuf;
    Glib::RefPtr<Gdk::Pixbuf> _breakpoint_pixbuf;

    Glib::RefPtr<Gdk::GC> _gc, _clip_gc;

    typedef sigc::signal1<void,bool> ChangedSignal;
    ChangedSignal _changed_signal;

    virtual bool is_statement_start(int line);
    bool dirty;
    
  public:
    MGCodeEditor();
    ~MGCodeEditor();

    void set_language(const std::string &mime);

    void begin_no_undo();
    void end_no_undo();
    
    void invalidate_lines(int begin, int end=-1);

    int get_line_pointer();
    void set_line_pointer(int line);

    void set_show_line_numbers(bool flag);

    void set_gutters(const Glib::RefPtr<Gdk::Pixbuf> &pc,
                     const Glib::RefPtr<Gdk::Pixbuf> &breakpoint,
                     const Glib::RefPtr<Gdk::Pixbuf> &statement);

    void clear_statement_markers();
    void remove_statement_marker(int line);
    void add_statement_marker(int line);

    void toggle_breakpoint();
    void clear_breakpoints();
    bool has_breakpoint(int line);
    bool has_breakpoint(const Gtk::TextIter &iter);
    
    void set_dirty(bool dirty);
    bool get_dirty();

    ChangedSignal signal_changed() { return _changed_signal; };

    Gtk::Widget *get_widget();
    Gtk::TextView *get_wrapper() { return _wrapper; };
};


#endif /* _MGCODEEDITOR_H_ */
