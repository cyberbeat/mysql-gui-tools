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

#include "myqb.h"
#include <gtkmm/main.h>
#include <gtkmm/messagedialog.h>
#include "MQScriptEditorTab.h"
#include "MQQueryDispatcher.h"
#include "MGCodeEditor.h"
#include "MGCharsetPickDialog.h"

#include "myg_utils.h"
#include "myg_gtkutils.h"


#define HIGHLIGHT_DISABLE_LENGTH 500


MQSQLCodeEditor::MQSQLCodeEditor()
  : _last_line_count(0)
{  
  _sql_text= myx_init_mysql_text();
  
  get_wrapper()->get_buffer()->signal_changed().connect(sigc::mem_fun(*this,&MQSQLCodeEditor::text_changed));
  //get_wrapper()->signal_move_cursor().connect(sigc::mem_fun(*this,&MQSQLCodeEditor::move_cursor));
  get_wrapper()->get_buffer()->signal_mark_set().connect(sigc::mem_fun(*this,&MQSQLCodeEditor::move_cursor));

  Pango::FontDescription font("Courier New");
  font.set_size(11*Pango::SCALE);
  get_wrapper()->modify_font(font);
}


MQSQLCodeEditor::~MQSQLCodeEditor()
{
  myx_free_sql_text(_sql_text);
}


bool MQSQLCodeEditor::is_error_ignored(int err)
{
  if (_ignored_errors.find(err) == _ignored_errors.end())
    return false;
  return true;
}


void MQSQLCodeEditor::ignore_error(int err)
{
  _ignored_errors.insert(err);
}


void MQSQLCodeEditor::reset_ignored_errors()
{
  _ignored_errors.clear();
}


bool MQSQLCodeEditor::is_statement_start(int line)
{
  int i= 0;
  int j= _sql_text->stmts_num;
  MYX_SQL_STATEMENT *statement;
  int m;

  while (i <= j)
  {
    m= (i+j)/2;
    statement= _sql_text->stmts+m;
    if (statement->stmt_begin_line == line)
      return true;
    else if (line < statement->stmt_begin_line)
      j= m-1;
    else
      i= m+1;
  }
  /*
  while (i < (int)_sql_text->stmts_num)
  {
    MYX_SQL_STATEMENT *statement= _sql_text->stmts+i;
    
    if (line > statement->stmt_end_line)
      break;
    if (statement->stmt_begin_line == line)
      return true;
    i++;
  }
   */
  return false;
}


void MQSQLCodeEditor::reparse()
{
  Glib::ustring tmp= get_wrapper()->get_buffer()->get_text();
  myx_analyze_text(_sql_text, tmp.c_str());
}


void MQSQLCodeEditor::text_changed()
{
  MYX_SQL_STATEMENT *statement;
  Glib::RefPtr<Gtk::TextBuffer> buffer= get_wrapper()->get_buffer();
  int cur_line;
  int refresh_line= -1;
  int prev_line= 0;
  int count;
  
  _cursor_moved_signal.emit();
  
  cur_line= buffer->get_insert()->get_iter().get_line();

  if ((count= buffer->end().get_line()) != _last_line_count)
  {
    Glib::ustring tmp= buffer->get_text();
    _last_line_count= count;

    myx_analyze_text(_sql_text, tmp.c_str());

    clear_statement_markers();
    
    if (_sql_text && _sql_text->stmts_num > 0)
    {
      for (unsigned int i= _sql_text->stmts_num; i > 0; i--)
      {
        statement= _sql_text->stmts+i-1;
        add_statement_marker(statement->stmt_begin_line);
        
        if (statement->stmt_begin_line == cur_line)
        {
          refresh_line= statement->stmt_end_line;
          break;
        }
        prev_line= statement->stmt_begin_line;
      }
    }
  }
  
  int l0, l1;
    
  if (refresh_line > -1)
  {
    l0= std::min(std::min(refresh_line, cur_line), prev_line);
    l1= std::max(std::max(refresh_line, cur_line), prev_line);
  }
  else
  {
    l0= std::min(cur_line, prev_line);
    l1= std::max(cur_line, prev_line);
  }
  invalidate_lines(l0, l1);
  set_dirty(true);
}


Glib::ustring MQSQLCodeEditor::get_text()
{
  return get_wrapper()->get_buffer()->get_text();
}


void MQSQLCodeEditor::set_text(const Glib::ustring &text, bool no_undo)
{
  Glib::ustring::size_type b, e, next;

  next= 0;
  do 
  {
    b= next;
    e= text.find('\n', b);
    next= e+1;
  } while (e != Glib::ustring::npos && e-b<HIGHLIGHT_DISABLE_LENGTH);

  if (e != Glib::ustring::npos)
  {
    // check the length of lines in the text.
    set_language("");
  }

  if (no_undo)
    begin_no_undo();

  _last_line_count= -1;
  get_wrapper()->get_buffer()->set_text(text);
  
  if (no_undo)
    end_no_undo();
  
  set_dirty(!no_undo);
}


MYX_SQL_STATEMENT *MQSQLCodeEditor::get_statement(unsigned int i)
{
  if (_sql_text && i < _sql_text->stmts_num)
    return _sql_text->stmts+i;

  return NULL;
}


unsigned int MQSQLCodeEditor::statement_index_by_line(unsigned int line)
{
  unsigned int i;
  for (i=0; i < _sql_text->stmts_num; i++)
  {
    if (_sql_text->stmts[i].stmt_begin_line <= (int)line &&
        (int)line <= _sql_text->stmts[i].stmt_end_line)
      return i;
  }
  return (unsigned int)-1;
}


void MQSQLCodeEditor::get_cursor(int &line, int &column)
{
  Gtk::TextIter it= get_wrapper()->get_buffer()->get_iter_at_mark(get_wrapper()->get_buffer()->get_insert());

  line= it.get_line();
  column= it.get_visible_line_offset();
}


bool MQSQLCodeEditor::get_statement(unsigned int i, Glib::ustring &text,
                                    unsigned int &line)
{  
  if (_sql_text && i < _sql_text->stmts_num)
  {
    MYX_SQL_STATEMENT *st= _sql_text->stmts+i;

    text= get_wrapper()->get_buffer()->get_text().substr(st->stmt_begin_char,
                                                         st->stmt_end_char-st->stmt_begin_char);
    line= st->stmt_begin_line;
              
    return true;
  }
  return false;
}

//void MQSQLCodeEditor::move_cursor(Gtk::MovementStep step, int count, bool extend_selection)
void MQSQLCodeEditor::move_cursor(const Gtk::TextBuffer::iterator&, const Glib::RefPtr<Gtk::TextBuffer::Mark>&)
{
  _cursor_moved_signal.emit();
}


//----------------------------------------------------------------------------

MQScriptEditorTab::MQScriptEditorTab(MQQueryDispatcher *disp)
  : _dispatcher(disp)
{
  _state= SIdle;

  add(_paned);
  _paned.show();
  
  set_icon(PIXCACHE->load("tabsheet_icon_script.png"));

  // setup the msg list
  _msg_store= Gtk::ListStore::create(_msg_columns);
  _msg_tree.append_column("", _msg_columns.icon);
  _msg_tree.append_column(_("Message"), _msg_columns.message);
  _msg_tree.append_column(_("errno"), _msg_columns.errornum);
  _msg_tree.append_column(_("Row"), _msg_columns.row);
  _msg_tree.set_model(_msg_store);
//  _msg_tree.get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MQResultSetView::selected_error));
  for (unsigned int i= 0; i < 4; i++)
    _msg_tree.get_column(i)->set_resizable();
  
  _msg_swin.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);

  // setup the editor
  _editor= new MQSQLCodeEditor();    
  _editor->set_language("text/x-mysql");

  _editor->get_widget()->show();
  _editor->set_show_line_numbers(true);

  _editor->set_gutters(PIXCACHE->load("gutter_current_pos.png"),
                       PIXCACHE->load("gutter_breakpoint.png"),
                       PIXCACHE->load("gutter_query_start.png"));

  _paned.add1(*_editor->get_widget());
  _paned.add2(_msg_swin);
  _msg_swin.add(_msg_tree);
  _msg_tree.show();
  
  _msg_swin.hide();
}


MQScriptEditorTab::~MQScriptEditorTab()
{
  delete _editor;
}


std::string MQScriptEditorTab::get_file_name()
{
  return _file_name;
}


void MQScriptEditorTab::set_file_name(const std::string &name)
{
  _file_name= name;
  set_title(name.rfind('/')==std::string::npos ? name : name.substr(name.rfind('/')+1));
}


int MQScriptEditorTab::get_pc()
{
  return _pc;
}


void MQScriptEditorTab::set_pc(int pc)
{
  _pc= pc;
}


void MQScriptEditorTab::add_message(int errnum, const Glib::ustring &message,
                                    int row, int st_index)
{
  Gtk::TreeIter iter= _msg_store->append();
  Gtk::TreeRow trow= *iter;
  
  trow[_msg_columns.icon]= PIXCACHE->load("rs_error.png");
  trow[_msg_columns.errornum]= errnum;
  trow[_msg_columns.message]= message;
  trow[_msg_columns.row]= row;
  trow[_msg_columns.index]= st_index;
  
  _msg_swin.show();
  _paned.set_position(_paned.get_height()-80);
}


void MQScriptEditorTab::reset()
{
  set_pc(0);
  
  _msg_store->clear();
  _msg_swin.hide();
  
  _editor->reset_ignored_errors();
  _editor->set_line_pointer(-1);
  
  change_state(SIdle);
}



void MQScriptEditorTab::execute_selection()
{
  Gtk::TextIter sel_start, sel_end;
  
  _editor->get_wrapper()->get_buffer()->get_selection_bounds(sel_start, sel_end);

  if (sel_start.get_offset() != sel_end.get_offset())
  {
    unsigned int from_line= sel_start.get_line();
    unsigned int to_line= sel_end.get_line();
    unsigned int from_st= _editor->statement_index_by_line(from_line);
    
    if (from_st != (unsigned int)-1)
    {
      reset();
      set_pc(from_st);
      _stepping= false;
      do_execute_script(false, to_line);
    }
  }
}


bool MQScriptEditorTab::has_selection()
{
  Gtk::TextIter sel_start, sel_end;
  
  _editor->get_wrapper()->get_buffer()->get_selection_bounds(sel_start, sel_end);

  if (sel_start.get_offset() != sel_end.get_offset())
    return true;

  return false;
}


bool MQScriptEditorTab::execute_statement(const Glib::ustring &s)
{
  Glib::ustring stmt= s;
  
  if (g_strncasecmp(stmt.c_str(), "DELIMITER ", sizeof("DELIMITER ")-1)==0)
    return true;

//  if (Glib::str_has_suffix(stmt, "$$"))
//    stmt= stmt.substr(0, stmt.size()-2)+";";

  if (!_dispatcher->execute(stmt, false))
  {
    MYSQL *my= _dispatcher->get_mysql();
    bool ignore= false;
    Glib::ustring error_message;
    int error;
    
    change_state(SError);

    if (!_editor->is_error_ignored(myx_mysql_errno(my)))
    {
      error= mysql_errno(my);
      error_message= mysql_error(my) ? : "";
      
      Gtk::MessageDialog dlg(*static_cast<Gtk::Window*>(get_toplevel()),
                             ufmt(_("Error while executing query: %s:\n%s (errno: %i)\n"
                                    "Click 'Ignore' if you'd like to have this error ignored until the end of the script."),
                                  s.c_str(),
                                  error_message.c_str(), error),
                             false,
                             Gtk::MESSAGE_ERROR,
                             Gtk::BUTTONS_NONE, true);
      
      dlg.add_button(_("_Ignore"), 10);
      dlg.add_button(_("_Skip"), 9);
      dlg.add_button(_("_OK"), 1);

      int rc;
      if ((rc= dlg.run()) != 1)
      {
        if (rc == 10)
          _editor->ignore_error(error);
        ignore= true;
      }
    }
    else
      ignore= true;
    add_message(error, error_message,
                _editor->get_statement(get_pc())->stmt_begin_line+1,
                get_pc());
    return ignore ? true : false;
  }

  return true;
}


void MQScriptEditorTab::execute_script(bool stepping)
{
  reset();
  
  _stepping= stepping;
  do_execute_script(false);
}


void MQScriptEditorTab::continue_script()
{  
  _stepping= false;
  do_execute_script(true);
}


void MQScriptEditorTab::change_state(State state)
{
  _state= state;
  _state_changed_signal.emit(this,state);
}


void MQScriptEditorTab::do_execute_script(bool cont, int until)
{
  Glib::ustring statement;
  unsigned int line;
  bool error= false;
  bool bp= false;
  
  _editor->reparse();

  change_state(SRunning);

  _stop_requested= false;
  while (_editor->get_statement(get_pc(), statement, line) && !_stop_requested
         // starting, or has a breakpoint, or is stepping
         && (!cont || !(bp= _editor->has_breakpoint(line)) || _stepping)
         && (until < 0 || line <= (unsigned int)until))
  {
    cont= false;

    _editor->set_line_pointer(line);

    if (!execute_statement(statement))
    {
      error= true;
      break;
    }
 
    set_pc(get_pc()+1);

    // flush pending events
    while (Gtk::Main::instance()->events_pending())
      Gtk::Main::instance()->iteration(false);

    if (_stepping)
      break;
  }

  if (_stop_requested || bp || error || _stepping)
  {
    _editor->set_line_pointer(get_pc());
    if (error)
      change_state(SError);
    else
    {
      if (bp)
        change_state(SBreakpoint);
      else
        change_state(_stepping ? SWaiting : SPaused);
    }
  }
  else
  {
    _editor->set_line_pointer(-1);
    change_state(SIdle);
    _script_finished_signal.emit(this);
  }
  if(!error)
  {
    _editor->set_dirty(false);
  }
}


void MQScriptEditorTab::step_over()
{
  Glib::ustring statement;
  unsigned int line;

  if (_editor->get_statement(get_pc(), statement, line))
  {
    _editor->set_line_pointer(line);

    change_state(SRunning);
    
    if (execute_statement(statement))
    {
      set_pc(get_pc()+1);
      if (_editor->get_statement(get_pc(), statement, line))
        _editor->set_line_pointer(line);

      if (!_editor->get_statement(get_pc()))
      {
        _editor->set_line_pointer(-1);
        change_state(SIdle);
        _script_finished_signal.emit(this);
      }
      else
        change_state(SWaiting);
    }
    else
      change_state(SError);
  }
  else
  {
    _editor->set_line_pointer(-1);
    change_state(SIdle);
    _script_finished_signal.emit(this);
  }
}


void MQScriptEditorTab::toggle_breakpoint()
{
  if (_editor)
    _editor->toggle_breakpoint();
}


void MQScriptEditorTab::clear_breakpoints()
{
  if (_editor)
    _editor->clear_breakpoints();
}


void MQScriptEditorTab::run_until_return()
{
}


void MQScriptEditorTab::stop_script(bool pause_only)
{
  if (!pause_only && (_state == SPaused || _state == SError))
    reset();
  else
  {
    change_state(SPaused);
    _stop_requested= true;
  }
}


bool MQScriptEditorTab::is_busy()
{
  return false;
}


int MQScriptEditorTab::load_file(const std::string &file)
{
  if (!file.empty())
  {
    gchar *contents;
    gsize length;
    GError *error= 0;

    if (!g_file_get_contents(file.c_str(), &contents, &length, &error))
    {
      myg_show_error(ufmt(_("Could not open file '%s': %s"), 
                          file.c_str(), error->message));
      g_error_free(error);
      return -1;
    }
    else
    {
      const gchar *invalid;
      Glib::ustring text;
      Glib::ustring charset;
      gchar *converted;
      gsize bread, bwritten;

      if (!g_utf8_validate(contents, length, &invalid))
      {
        MGCharsetPickDialog dialog;
        
        charset= dialog.show(_("The script editor only supports text in the UTF-8 encoding.\n"
                               "You may select the encoding of the file and click OK to convert it."));

        if (charset.empty())
        {
          g_free(contents);
          return 0;
        }

        converted= g_convert(contents, length, "UTF-8", charset.c_str(),
                             &bread, &bwritten, &error);
        if (!converted)
        {
          if (error)
            myg_show_error(ufmt(_("Could not convert the file contents to UTF-8.\n%s"),
                                error->message));
          g_free(contents);
          return -1;
        }
        else
        {
          text= converted;
          g_free(converted);
        }
      }
      else
        text= contents;

      if (strstr(text.c_str(), "\r\n"))
      {
        char *unix_text;

        // DOS format file, convert it to UNIX 1st
        unix_text= str_g_subst(text.c_str(), "\r\n", "\n");
        text= unix_text;
        g_free(unix_text);
      }
      
      set_file_name(file);

      set_text(text);
      g_free(contents);
    }
  }
  else
  {
    set_file_name("new_script.sql");

    set_text("");
  }
  
  reset();
  
  return 1;
}


void MQScriptEditorTab::set_text(const Glib::ustring &text)
{
  _editor->set_text(text, true);
  reset();
}

bool MQScriptEditorTab::tab_will_close()
{
  if(_editor->get_dirty())
  {
    Gtk::MessageDialog dlg(
      *static_cast<Gtk::Window*>(get_toplevel()),
      Glib::ustring(_("The script is modified, are you sure you want to close the editor?")),
      false,
      Gtk::MESSAGE_QUESTION,
      Gtk::BUTTONS_OK_CANCEL);
    
    return (dlg.run() == GTK_RESPONSE_OK);
  }
  return true;
}
