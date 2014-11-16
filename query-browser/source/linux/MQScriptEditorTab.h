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

#ifndef _MQSCRIPTEDITORTAB_H_
#define _MQSCRIPTEDITORTAB_H_

#include "MQBaseTab.h"

#include "myx_qb_public_interface.h"

#include <gtkmm/paned.h>
#include <gtkmm/treeview.h>
#include <gtkmm/liststore.h>
#include <gtkmm/scrolledwindow.h>

#include <set>

#include "MGCodeEditor.h"



class MQSQLCodeEditor : public MGCodeEditor {    
  protected:
    std::set<int> _ignored_errors;
    
    MYX_SQL_TEXT *_sql_text;

    sigc::signal0<void> _cursor_moved_signal;
    int _last_line_count;

    //void move_cursor(Gtk::MovementStep step, int count, bool extend_selection);
    void move_cursor(const Gtk::TextBuffer::iterator&, const Glib::RefPtr<Gtk::TextBuffer::Mark>&);
    virtual bool is_statement_start(int line);
  public:
    MQSQLCodeEditor();
    virtual ~MQSQLCodeEditor();

    sigc::signal0<void> signal_cursor_moved() { return _cursor_moved_signal; };

    bool is_error_ignored(int err);
    void ignore_error(int err);
    void reset_ignored_errors();

    void reparse();

    void text_changed();
    Glib::ustring get_text();
    void set_text(const Glib::ustring &text, bool no_undo= false);

    void get_cursor(int &line, int &column);
    
    MYX_SQL_STATEMENT *get_statement(unsigned int i);
    unsigned int statement_index_by_line(unsigned int i);

    bool get_statement(unsigned int i, Glib::ustring &text, unsigned int &line);
};

class MQQueryDispatcher;

class MQScriptEditorTab : public MQBaseTab {
  public:
    enum State
    {
      SIdle,
      SRunning,
      SPaused,
      SWaiting,
      SBreakpoint,
      SError
    };
    
    typedef sigc::signal2<void,MQScriptEditorTab*,State> SignalStateChanged;
    typedef sigc::signal1<void,MQScriptEditorTab*> SignalScriptEditor;
    
  protected:
    class MessageColumns : public Gtk::TreeModel::ColumnRecord
    {
      public:
        MessageColumns()
        {
          add(icon);
          add(errornum);
          add(message);
          add(row);
          add(index);
        }
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<int> errornum;
        Gtk::TreeModelColumn<Glib::ustring> message;
        Gtk::TreeModelColumn<int> row;
        Gtk::TreeModelColumn<int> offset;
        Gtk::TreeModelColumn<int> index;
    } _msg_columns;

    Gtk::VPaned _paned;
    
    MQSQLCodeEditor *_editor;
    Gtk::ScrolledWindow _msg_swin;
    Gtk::TreeView _msg_tree;
    Glib::RefPtr<Gtk::ListStore> _msg_store;

    MQQueryDispatcher *_dispatcher;
    
    SignalStateChanged _state_changed_signal;
    SignalScriptEditor _script_finished_signal;
    
    std::string _file_name;
    int _pc;
    State _state;

    bool _stop_requested;
    bool _stepping;
    
    bool button_pressed(GdkEventButton *ev);
    
    virtual bool tab_will_close();
    
  public:
    MQScriptEditorTab(MQQueryDispatcher *disp);
    virtual ~MQScriptEditorTab();

    virtual MQTabType get_type() { return TScriptEditor; };

    MQSQLCodeEditor *get_editor() { return _editor; };
    
    int get_pc();
    void set_pc(int pc);
    
    std::string get_file_name();
    void set_file_name(const std::string &name);

    int load_file(const std::string &file);
    void set_text(const Glib::ustring &text);
    
    void reset();
    
    void add_message(int errnum, const Glib::ustring &message, int row, int st_index);

    void execute_selection();
    bool execute_statement(const Glib::ustring &s);
    void execute_script(bool stepping= false);
    void continue_script();
    void change_state(State state);
    void do_execute_script(bool cont, int until=-1);
    void step_over();
    void toggle_breakpoint();
    void clear_breakpoints();
    void run_until_return();
    void stop_script(bool pause_only= false);
    
    State get_state() { return _state; };
    bool is_busy();
    
    bool has_selection();
    
    SignalStateChanged signal_state_changed() { return _state_changed_signal; };
    SignalScriptEditor signal_script_finished() { return _script_finished_signal; };
};


#endif /* _MQSCRIPTEDITORTAB_H_ */
