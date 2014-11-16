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

#ifndef _MGSHELLVIEW_H_
#define _MGSHELLVIEW_H_

#include <gtkmm/textview.h>

class MGShellInterface {
  public:
    virtual ~MGShellInterface() {};
    virtual Glib::ustring get_prompt()= 0;
    virtual void perform_command(const Glib::ustring &command)= 0;
};


class MGShellView : public Gtk::TextView {
    MGShellInterface *_shell;
    
    Glib::RefPtr<Gtk::TextMark> _prompt_start;
    Glib::RefPtr<Gtk::TextMark> _prompt_end;
    bool _prompt_visible;
    
    bool idle();

    bool key_press_event(GdkEventKey* event);
    virtual void on_insert_at_cursor(const Glib::ustring& str);
  public:
    MGShellView();
    
    void set_shell(MGShellInterface *shell);
    
    void out_text(const Glib::ustring &str);
};


#endif /* _MGRTSHELLVIEW_H_ */
