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

#include "MGShellView.h"
/**
 * @file  MGShellView.cc
 * @brief 
 */



MGShellView::MGShellView()
{
  _shell= 0;
  _prompt_visible= false;

  set_wrap_mode(Gtk::WRAP_WORD);
  
  signal_key_press_event().connect(sigc::mem_fun(*this,&MGShellView::key_press_event), false);
}



void MGShellView::on_insert_at_cursor(const Glib::ustring& str)
{
  g_message("INSERT");
}


bool MGShellView::key_press_event(GdkEventKey* event)
{
  if (event->keyval == GDK_Return)
  {
    Glib::ustring command= get_buffer()->get_slice(_prompt_end->get_iter(), get_buffer()->end());

    Gtk::TextView::on_key_press_event(event);

    _prompt_start.clear();
    _prompt_end.clear();
    _prompt_visible= false;
    
    idle();
    
    if (_shell && !command.empty())
    {
      _shell->perform_command(command);
    }
    
    return true;
  }
  else if (event->keyval == GDK_BackSpace)
  {
    if (get_buffer()->get_insert()->get_iter().get_offset() <= _prompt_end->get_iter().get_offset())
      return true;
  }
  else if (event->keyval == GDK_Home)
  {
    if (_prompt_end)
      get_buffer()->move_mark(get_buffer()->get_insert(), _prompt_end->get_iter());
    return true;
  }
  else
  {
    if (get_buffer()->get_insert()->get_iter().get_offset() <= _prompt_end->get_iter().get_offset()
        && event->string && *event->string && event->state == 0)
      get_buffer()->move_mark(get_buffer()->get_insert(), get_buffer()->end());
  }
  return Gtk::TextView::on_key_press_event(event);
}


bool MGShellView::idle()
{
  if (!_prompt_visible)
  {
    if (_shell)
    {
      Glib::RefPtr<Gtk::TextBuffer> buffer= get_buffer();
    
      if (buffer->end().get_line_offset() > 0)
        buffer->insert(buffer->end(), "\n");

      _prompt_start= buffer->create_mark(buffer->end());
      buffer->insert(buffer->end(), _shell->get_prompt());
      _prompt_end= buffer->create_mark(buffer->end());
    }
    _prompt_visible= true;
  }
  
  return false;
}


void MGShellView::out_text(const Glib::ustring &str)
{
  if (_prompt_visible)
  {
    get_buffer()->erase(_prompt_start->get_iter(), _prompt_end->get_iter());
    
    Glib::signal_idle().connect(sigc::mem_fun(*this,&MGShellView::idle));
    _prompt_visible= false;
  }

  get_buffer()->insert(get_buffer()->end(), str);
}


void MGShellView::set_shell(MGShellInterface *shell)
{
  _shell= shell;
  Glib::signal_idle().connect(sigc::mem_fun(*this,&MGShellView::idle));
}
