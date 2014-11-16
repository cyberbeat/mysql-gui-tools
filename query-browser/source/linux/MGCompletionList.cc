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

#include "MGCompletionList.h"
#include <gtkmm/frame.h>


#define IS_SEPARATOR(c) (c == ' ' || (ispunct(c) && c!='.' && c!='_'))

class MTextIterator : public Gtk::TextBuffer::iterator
{
  public:
    MTextIterator(const Gtk::TextBuffer::iterator &iter)
      : Gtk::TextBuffer::iterator(iter)
    {
    }

    bool backward_token_start()
    {
      int c;
      
      if (!backward_char())
        return false;

      for (;;)
      {
        c= get_char();
        if (IS_SEPARATOR(c))
        {
          forward_char();
          break;
        }
        if (!backward_char())
          break;
      }
      return true;
    }

    bool forward_token_end()
    {
      int c;

      if (!forward_char())
        return false;

      for (;;)
      {
        c= get_char();
        if (IS_SEPARATOR(c))
          break;
        if (!forward_char())
          break;
      }
      return true;
    }
};


MGCompletionList::MGCompletionList(Gtk::TextView *text_view)
  : Gtk::Window(Gtk::WINDOW_POPUP), _text_view(text_view), _activated(false)
{
  Gtk::Frame *frame= Gtk::manage(new Gtk::Frame());
  
  add(*frame);
  
  frame->add(_swin);

  _swin.add(_tree);
  _tree.append_column("", _columns.text);
  _tree.set_headers_visible(false);
  _swin.set_shadow_type(Gtk::SHADOW_IN);
  _swin.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  
  _store= Gtk::ListStore::create(_columns);
  
  _tree.set_model(_store);
  
  _buffer= _text_view->get_buffer();
  
  _tree.get_selection()->set_mode(Gtk::SELECTION_BROWSE);
  
  _text_view->signal_key_press_event().connect(sigc::mem_fun(*this,&MGCompletionList::query_text_key_press), false);
  _text_view->signal_key_release_event().connect(sigc::mem_fun(*this,&MGCompletionList::query_text_key_release), false);

  frame->show_all();
  
  reset_size();
}


void MGCompletionList::set_font(const Pango::FontDescription &font)
{
  _tree.modify_font(font);
}


void MGCompletionList::set_list(const std::list<Glib::ustring> &list,
                                const Glib::ustring &input)
{
  _store->clear();
  
  for (std::list<Glib::ustring>::const_iterator iter= list.begin();
       iter != list.end(); ++iter)
  {
    Gtk::TreeIter ri= _store->append();
    Gtk::TreeRow row= *ri;
    Gdk::Rectangle rect;

    row[_columns.text]= *iter;
  }
  _tree.get_selection()->select(_store->children().begin());
  reset_size();
}


void MGCompletionList::reset_size()
{
  if (!_tree.is_realized())
    return;
  
  int height= 0;
  int width= 0;

  for (Gtk::TreeIter ri= _store->children().begin(); 
       ri != _store->children().end();
       ++ri)
  {
    Gdk::Rectangle rect;

    _tree.get_cell_area(_store->get_path(ri),
                        *_tree.get_column(0),
                        rect);
    width= std::max(width, rect.get_width());
    height += rect.get_height();
  }

  _tree.columns_autosize();
  
  width= std::min(width, 150);
  height= std::min(height, 350);

  if (width > 0 && height > 0)
    resize(width+20, height);
}


void MGCompletionList::popup(int x, int y)
{
  move(x,y);
  show();
  reset_size();
}


std::list<Glib::ustring> MGCompletionList::get_suggestions(const Gtk::TextIter &iter)
{
  std::list<Glib::ustring> l;
  _request_suggestions(iter, l);
  return l;
}


void MGCompletionList::update(bool insert_if_unique)
{
  MTextIterator iter(_buffer->get_insert()->get_iter());
  Gdk::Rectangle rect;
  Gtk::TextBuffer::iterator tmpiter;

  tmpiter= iter;
  if (tmpiter.backward_char())
  {
    if (*tmpiter == ' ')
    {
      hide();
      return;
    }
  }
  else
  {
    hide();
    return;
  }

  iter.backward_token_start();
  MTextIterator end= iter;
  end.forward_token_end();

  Glib::ustring word= _buffer->get_slice(iter, end);

  std::list<Glib::ustring> compls= get_suggestions(iter);

  if (compls.size() == 1 && insert_if_unique)
  {
    insert_word(*compls.begin());
    hide();
  }
  else if (compls.empty())
    hide();
  else
  {
    int x, y;
    set_list(compls, word);
    _text_view->get_iter_location(iter, rect);
    _text_view->get_window(Gtk::TEXT_WINDOW_WIDGET)->get_origin(x, y);
    x+= rect.get_x(); y+= rect.get_y();

//    _buffer->move_mark(_buffer->get_selection_bound(), iter);
    
    popup(x, y+rect.get_height());
  }
}


void MGCompletionList::insert_word(const Glib::ustring &word)
{
  Gtk::TextBuffer::iterator iter= _buffer->get_insert()->get_iter();
  Gtk::TextBuffer::iterator end;
  iter.backward_word_start();
  end= iter;
  end.forward_word_end();
  _buffer->begin_user_action();
  iter= _buffer->erase(iter, end);
  _buffer->insert(iter, word);
  _buffer->end_user_action();
}


void MGCompletionList::insert_selection()
{
  Gtk::TreeIter iter= _tree.get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    Glib::ustring text= row[_columns.text];
    
    insert_word(text);
  }
}


bool MGCompletionList::query_text_key_press(GdkEventKey *ev)
{
  Gtk::TreeIter iter= _tree.get_selection()->get_selected();
  Gtk::TreePath path;
  if (iter)
    path= Gtk::TreePath(iter);

  switch (ev->keyval)
  {
  case GDK_Escape:
    if (is_mapped())
      hide();
    _activated= false;
    break;
  case GDK_Return:
    if (is_mapped())
    {
      insert_selection();
      hide();
      _activated= false;
      return true;
    }
    break;
  case GDK_Up:
    if (iter && is_mapped())
    {
      if (path.prev())
        _tree.set_cursor(path);
      return true;
    }
    break;
  case GDK_Down:
    if (iter && is_mapped())
    {
      path.next();
      _tree.set_cursor(path);
      return true;
    }
    break;
  case GDK_Home:
    if (iter && is_mapped())
    {
      path= Gtk::TreePath(_store->children().begin());
      _tree.set_cursor(path);
      return true;
    }
    break;
  case GDK_Page_Down:
    if (iter && is_mapped())
    {
      for (int i= 0; i < 5; i++)
        path.next();
      if (!_store->get_iter(path))
        goto woohoo;
      _tree.set_cursor(path);
      return true;
    }
    break;
  case GDK_Page_Up:
    if (iter && is_mapped())
    {
      for (int i= 0; i < 5; i++)
        path.prev();
      _tree.set_cursor(path);
      return true;
    }
    break;
  case GDK_End:
    woohoo:
    if (iter && is_mapped())
    {
      char buf[100];
      sprintf(buf, "%i", _store->children().size()-1);
      path= Gtk::TreePath(buf);
      _tree.set_cursor(path);
      return true;
    }
    break;
  case GDK_space:
    if (ev->state & GDK_CONTROL_MASK)
    {
      update(true);
      if (is_mapped())
        _activated= true;
    }
  }
  return false;
}


bool MGCompletionList::query_text_key_release(GdkEventKey *ev)
{
  if (_activated && (ev->keyval&0xff00)==0 && !(ev->state & GDK_CONTROL_MASK))
  {
    update();
    if (!is_mapped())
      _activated= false;
  }
  
  return false;
}
