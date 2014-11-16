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

#ifndef _MGMYSQLCONSOLE_H_
#define _MGMYSQLCONSOLE_H_

#include <queue>

#include <gtkmm/scrolledwindow.h>
#include <gtkmm/textview.h>
#include <gtkmm/textbuffer.h>

#include "myx_public_interface.h"

class MGMySQLConsole : public Gtk::ScrolledWindow {
    pthread_mutex_t _mutex;
    std::queue<Glib::ustring> _output_queue;
    Glib::Dispatcher _output_ready;

    void process_output_queue();
    
  protected:
    Gtk::TextView _text;
    Glib::RefPtr<Gtk::TextBuffer> _buffer;

    bool _busy;
    bool _abort_command;

    gunichar _delimiter;

    MYX_TEXT_SHELL *_shell;
    std::vector<Glib::ustring> _history;
    unsigned int _history_index;

    static int handle_shell_output(const char *text, void *udata);
    static int handle_threaded_shell_output(const char *text, void *udata);

    bool key_press(GdkEventKey *ev);
    
    virtual Glib::ustring get_prompt_text(bool line_cont= false);
    
    virtual void put_prompt(bool line_cont= false);

    bool scroll_bottom();

    virtual bool execute(const Glib::ustring &command, int key_state);
    virtual void command_finished();

  public:
    MGMySQLConsole();
    virtual ~MGMySQLConsole();

    virtual void do_execute(const Glib::ustring &command);
    
    void append(const Glib::ustring &text);

    bool command_complete();
    Glib::ustring get_command();
    void set_command(const Glib::ustring &command);
    
    void set_delimiter(gunichar del);

    void set_connection(MYSQL *mysql);
    
    bool is_busy() { return _busy; };
    
    Gtk::TextView *get_text_view() { return &_text; };
};


#endif /* _MGMYSQLCONSOLE_H_ */
