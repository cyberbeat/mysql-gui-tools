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

#ifndef _MGRTWIZARDBASE_H_
#define _MGRTWIZARDBASE_H_

#include <gtkmm/window.h>
#include <MySQLGRT/MGRT.h>
#include <MySQLGRT/MGRTConnectDialog.h>


class MGRTWizardBase : public Gtk::Window {
  protected:
    Glib::RefPtr<Gdk::Pixbuf> _task_unchecked;
    Glib::RefPtr<Gdk::Pixbuf> _task_checked;
    Glib::RefPtr<Gdk::Pixbuf> _task_error;
    Glib::RefPtr<Gdk::Pixbuf> _task_disabled;

    MGRT *_grt;
    MGGladeXML *_xml;

    int _section;
    int _last_section;

    bool _cancelled;
    bool _advanced_shown;
    bool _back_was_enabled;
    bool _next_was_enabled;


    virtual void log_text(const Glib::ustring &text);
    virtual void log_msg(const Glib::ustring &msg, const Glib::ustring &type);

    virtual void begin_work();
    virtual void end_work();

    int status_query();
    
    virtual void go_back();
    virtual void go_next();
    virtual void go_finish();
    virtual void cancel();
    virtual void show_details();

    virtual void setup();
    
    virtual void update_advanced();
    virtual void update_section(bool back);
    virtual bool validate_section();
    
  public:
    MGRTWizardBase(GtkWindow *win);
    
    virtual void set_grt(MGRT *grt);

    virtual bool run();
};

#endif /* _MGRTWIZARDBASE_H_ */
