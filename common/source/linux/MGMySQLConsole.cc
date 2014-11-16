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

#include "mygpriv.h"
#include "MGMySQLConsole.h"
#include <gdkmm.h>

int MGMySQLConsole::handle_shell_output(const char *text, void *udata)
{
  MGMySQLConsole *self= (MGMySQLConsole*)udata;

  self->append(text);
  
  return self->_abort_command;
}


int MGMySQLConsole::handle_threaded_shell_output(const char *text, void *udata)
{
  MGMySQLConsole *self= (MGMySQLConsole*)udata;

  pthread_mutex_lock(&self->_mutex);
  
  self->_output_queue.push(text);
  
  pthread_mutex_unlock(&self->_mutex);
  
  self->_output_ready.emit();
  
  return self->_abort_command;
}


void MGMySQLConsole::process_output_queue()
{
  Glib::ustring tmp;
  pthread_mutex_lock(&_mutex);
  while (!_output_queue.empty())
  {
    tmp+= _output_queue.front();
    _output_queue.pop();
  }
  pthread_mutex_unlock(&_mutex);
  if (!tmp.empty())
    append(tmp);
}


MGMySQLConsole::MGMySQLConsole()
  : _busy(false), _delimiter(g_utf8_get_char(";")), _shell(0), _history_index(0)
{
  _text.signal_key_press_event().connect(sigc::mem_fun(*this,&MGMySQLConsole::key_press), false);
  
  _buffer= _text.get_buffer();
  
  set_shadow_type(Gtk::SHADOW_IN);
  set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  
  /*
  _text.modify_base(Gtk::STATE_NORMAL, get_style()->get_black());
  _text.modify_text(Gtk::STATE_NORMAL, get_style()->get_white());
  _text.modify_fg(Gtk::STATE_NORMAL, get_style()->get_white());
  */
  
  pthread_mutex_init(&_mutex, NULL);
  
  Pango::FontDescription font;
  font.set_family("bitstream vera sans mono");
  font.set_size(Pango::SCALE*9);

  _text.modify_font(font);
  
  add(_text);
  _text.show();

  _output_ready.connect(sigc::mem_fun(*this,&MGMySQLConsole::process_output_queue));
}


MGMySQLConsole::~MGMySQLConsole()
{
  if (_shell)
    myx_finalize_text_shell(_shell);
}


void MGMySQLConsole::set_connection(MYSQL *mysql)
{
  if (_shell)
    myx_finalize_text_shell(_shell);

  _shell= myx_init_text_shell(mysql);

  myx_ts_set_output_callback(_shell, this, MGMySQLConsole::handle_threaded_shell_output);

  put_prompt();
}


Glib::ustring MGMySQLConsole::get_prompt_text(bool line_cont)
{
  if (line_cont)
    return " > ";
  else
    return ">> ";
}

    
void MGMySQLConsole::put_prompt(bool cont)
{
  if (!_buffer->end().starts_line())
    _buffer->insert(_buffer->end(), "\n");
  _buffer->insert(_buffer->end(), get_prompt_text(cont));

  Gtk::TextIter end= _buffer->end();
//  end.backward_char();
  _buffer->place_cursor(end);
  
  scroll_bottom();

  if (!cont)
  {
    Glib::RefPtr<Gtk::TextBuffer::Mark> mark= _buffer->get_mark("cmdstart");
    if (mark)
      _buffer->move_mark(mark, end);
    else
      _buffer->create_mark("cmdstart", end, true);
  }
}

bool MGMySQLConsole::execute(const Glib::ustring &command, int key_state)
{
//  _text.get_window(Gtk::TEXT_WINDOW_TEXT)->set_cursor(Gdk::Cursor(Gdk::WATCH));
  
  if (_shell)
  {
    do_execute(command);
  }
  else
  {
    append(_("No connection."));
  }
  
//  _text.get_window(Gtk::TEXT_WINDOW_TEXT)->set_cursor(Gdk::Cursor(Gdk::XTERM));
  
  return false;
}


void MGMySQLConsole::do_execute(const Glib::ustring &command)
{
  _abort_command= false;
  myx_ts_execute_command(_shell, command.c_str());
}


void MGMySQLConsole::command_finished()
{
  _busy= false;
  put_prompt();
  scroll_bottom();
}


bool MGMySQLConsole::scroll_bottom()
{
  get_vadjustment()->set_value(get_vadjustment()->get_upper());
  get_vadjustment()->value_changed();
  return false;
}


bool MGMySQLConsole::command_complete()
{
  Glib::ustring command= get_command();
  if (!command.empty())
  {
    Glib::ustring::iterator end= command.end();

    while (--end >= command.begin())
    {
      if (*end == _delimiter)
        return true;
    }
  }
  return false;
}


void MGMySQLConsole::append(const Glib::ustring &text)
{
  _buffer->insert(_buffer->end(), text);

  _buffer->place_cursor(_buffer->end());

  scroll_bottom();
}


void MGMySQLConsole::set_delimiter(gunichar del)
{
  _delimiter= del;
}


bool MGMySQLConsole::key_press(GdkEventKey *ev)
{
  Gtk::TextIter cursor_iter= _buffer->get_iter_at_mark(_buffer->get_insert());
  int cursor_line= cursor_iter.get_line();
  
  if (_busy)
    return true;
  
  if (ev->keyval == GDK_Return)
  {
    if ((ev->state & GDK_SHIFT_MASK) == GDK_SHIFT_MASK)
    {
      _buffer->insert(cursor_iter, "\n"+get_prompt_text(true));
      return true;
    }
    else
    {
      Glib::ustring cmd= get_command();
      bool cont= false;
      bool waiting= false;
      
      if (!cmd.empty())
      {
        _buffer->insert(_buffer->end(), "\n");
        
        if (command_complete())
        {
          _busy= true;
          waiting= execute(cmd, ev->state);
          
          _history.push_back(cmd);
        }
        else
          cont= true;
      }
      if (!waiting)
      {
        put_prompt(cont);
        _busy= false;
      }
      _history_index= 0;
      return true;
    }
  }
  else if (ev->keyval == GDK_BackSpace)
  {
    Gtk::TextIter cmdstart= _buffer->get_iter_at_mark(_buffer->get_mark("cmdstart"));
    Gtk::TextIter cursor= _buffer->get_iter_at_mark(_buffer->get_insert());
    
    if (cursor.get_offset() > cmdstart.get_offset())
    {
      if (cursor.get_line_index() > cmdstart.get_line_index())
        return false;
      else 
      {
        Gtk::TextIter prevl= cursor;
        prevl.backward_line();
        prevl.forward_to_line_end();
        _buffer->erase(prevl, cursor);
      }
    }
    return true;
  }
  else if (ev->keyval == GDK_C && (ev->state & GDK_CONTROL_MASK))
  {
    _abort_command= true;
    append(_("Control-C pressed. Aborting data retrieval..."));
  }
  else if (ev->keyval == GDK_U && (ev->state & GDK_CONTROL_MASK))
  {
    set_command("");
  }
  else if (ev->keyval == GDK_Home || ev->keyval == GDK_End 
           || ev->keyval == GDK_Left || ev->keyval == GDK_Right
           || ev->keyval == GDK_Up || ev->keyval == GDK_Down)
  {
    Gtk::TextIter cmdstart= _buffer->get_iter_at_mark(_buffer->get_mark("cmdstart"));
    Gtk::TextIter cursor= _buffer->get_iter_at_mark(_buffer->get_insert());
    bool cant_move= false;

    switch (ev->keyval)
    {
    case GDK_Home:
      // if we're in the last line
      cursor= cmdstart;
      break;
    case GDK_End:
      cursor= _buffer->end();
      break;
    case GDK_Left:
      if (cursor.get_line_index() > cmdstart.get_line_index())
        cursor.backward_char();
      else
      {
        if (cursor.get_line() > cmdstart.get_line())
        {
          cursor.backward_line();
          cursor.forward_to_line_end();
        }
        else
          cant_move= true;
      }
      break;
    case GDK_Right:
      if (!cursor.ends_line())
        cursor.forward_char();
      else
      {
        if (cursor.get_line() < _buffer->end().get_line())
        {
          cursor.forward_line();
          cursor.set_line_index(cmdstart.get_line_index());
        }
        else
          cant_move= true;
      }
      break;
    case GDK_Up:
      if (cursor.get_line() > cmdstart.get_line())
      {
        int index= cursor.get_line_index();
        cursor.backward_line();
        cursor.forward_to_line_end();
        if (cursor.get_line_index() > index)
          cursor.set_line_index(index);
      }
      else
        cant_move= true;
      break;
    case GDK_Down:
      if (cursor.get_line() < _buffer->end().get_line())
      {
        int index= cursor.get_line_index();
        cursor.forward_line();
        cursor.forward_to_line_end();
        if (cursor.get_line_index() > index)
          cursor.set_line_index(index);
      }
      else
        cant_move= true;
      break;
    }
    if (ev->state & GDK_SHIFT_MASK)
    {
      Gtk::TextIter sbound= _buffer->get_iter_at_mark(_buffer->get_selection_bound());

      if (sbound == _buffer->get_iter_at_mark(_buffer->get_insert()))
      {
        _buffer->place_cursor(cursor);
        _buffer->move_mark_by_name("selection_bound", sbound);
      }
      else
        _buffer->place_cursor(cursor);
      //        _buffer->move_mark_by_name("selection_bound");
    }
    else
      _buffer->place_cursor(cursor);
    return true;
  }
  else if (ev->state == GDK_CONTROL_MASK && (ev->keyval == GDK_Up || ev->keyval == GDK_Down))
  {
    Glib::ustring text;
    if (ev->keyval == GDK_Up)
      _history_index++;
    else 
      _history_index--;
    if (_history_index < 0)
    {
      text= "";
      _history_index= 0;
    }
    else
      text= _history[_history_index];
    _buffer->erase(_buffer->get_iter_at_mark(_buffer->get_mark("cmdstart")), _buffer->end());
    _buffer->insert(_buffer->get_iter_at_mark(_buffer->get_mark("cmdstart")), text);

    return true;
  }

  return false;
}


Glib::ustring MGMySQLConsole::get_command()
{
  Glib::ustring cmd;
  Gtk::TextIter ls= _buffer->get_iter_at_mark(_buffer->get_mark("cmdstart"));
  Gtk::TextIter le;
  int index= ls.get_line_index();

  for (;;)
  {
    le= ls;
    le.forward_to_line_end();
    if (!cmd.empty())
      cmd+="\n";
    cmd+= _buffer->get_text(ls, le);
    if (!ls.forward_line())
      break;
    ls.set_line_index(index);
  }
  
  return cmd;
}


void MGMySQLConsole::set_command(const Glib::ustring &command)
{
  Gtk::TextIter iter= _buffer->get_iter_at_mark(_buffer->get_mark("cmdstart"));
  bool cont= false;

  iter.set_line_offset(0);
  _buffer->erase(iter, _buffer->end());

  Glib::ustring::size_type p0, pf;
  Glib::ustring substr= command;

  p0= 0;
  pf= command.find('\n');
  while (pf != Glib::ustring::npos)
  {
    substr= substr.substr(p0, pf);

    put_prompt(cont); cont= true;
    _buffer->insert(_buffer->end(), substr+"\n");
    p0= pf+1;
    pf= command.substr(p0).find('\n');
  }
  put_prompt(cont);
  _buffer->insert(_buffer->end(), command.substr(p0));
}
