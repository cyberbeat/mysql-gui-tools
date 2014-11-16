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

#include "MGSQLTextBuffer.h"


MGSQLTextBuffer::MGSQLTextBuffer(const Glib::RefPtr<Gtk::TextTagTable> &tagtable)
  : Gtk::TextBuffer(tagtable), _syn(0), _sqlhl(0)
{
  signal_end_user_action().connect(sigc::mem_fun(*this,&MGSQLTextBuffer::do_hiliting));
}


MGSQLTextBuffer::~MGSQLTextBuffer()
{
  if (_syn)
    myx_free_syn(_syn);
  
  if (_sqlhl)
    myx_free_sql_highlighting(_sqlhl);
}


void MGSQLTextBuffer::set_text(const Glib::ustring &text)
{
  Gtk::TextBuffer::set_text(text);

  do_hiliting();
}


bool MGSQLTextBuffer::init_highlighting(MYSQL *mysql)
{
  if (_syn)
    myx_free_syn(_syn);
  _syn= 0;
  if (_sqlhl)
    myx_free_sql_highlighting(_sqlhl);
  _sqlhl= 0;
  
  _syn= myx_refresh_dbinfo(mysql, _syn);
  if (!_syn)
    return false;
  
  _sqlhl= myx_init_sql_parsing(_syn);
  if (!_sqlhl)
  {
    myx_free_syn(_syn);
    _syn= 0;
    return false;
  }

  return true;
}


Glib::RefPtr<MGSQLTextBuffer> MGSQLTextBuffer::create(const Glib::RefPtr<Gtk::TextTagTable> &tagtable, MYSQL *mysql)
{
  MGSQLTextBuffer *buf= new MGSQLTextBuffer(tagtable);
  
  if (!buf->init_highlighting(mysql))
  {
    return Glib::RefPtr<MGSQLTextBuffer>(buf);
  }

  return Glib::RefPtr<MGSQLTextBuffer>(buf);
}


void MGSQLTextBuffer::do_hiliting()
{
  if (!_sqlhl)
    return;
  
  int res;
  int line= 0;
  Gtk::TextBuffer::iterator bol;
  Gtk::TextBuffer::iterator eol=get_iter_at_line(0);

  do {
    bol= eol;
    if (!eol.forward_line())
      eol= end();
    Glib::ustring text= bol.get_text(eol);

    res= myx_highlight(_sqlhl, text.c_str(), line);
    remove_all_tags(bol, eol);
    
    for (unsigned int i= 0; i < _sqlhl->words_num; i++)
    {
      Glib::ustring name;
      switch (_sqlhl->words[i].word_type)
      {
      case MYX_SYN_NORMAL: name= "normal"; break;
      case MYX_SYN_TABLE: name= "table"; break;
      case MYX_SYN_COLUMN: name= "column"; break;
      case MYX_SYN_COMMENT: name= "comment"; break;
      case MYX_SYN_STRING: name= "string"; break;
      case MYX_SYN_SYMBOL: name= "symbol"; break;
      case MYX_SYN_FUNCTION: name= "function"; break;
      default: g_warning("unknown word type in syntax highlight"); name.clear(); break;
      }

      Gtk::TextBuffer::iterator bow= get_iter_at_line_offset(line, _sqlhl->words[i].word_begin);
      Gtk::TextBuffer::iterator eow= get_iter_at_line_offset(line, 0);
      eow.forward_to_line_end();
      if (_sqlhl->words[i].word_end >= 0 && eow.get_line_offset() >= _sqlhl->words[i].word_end + 1)
        eow= get_iter_at_line_offset(line, _sqlhl->words[i].word_end + 1);
/*XXX
      if (_sqlhl->words[i].word_type == MYX_SYN_SYMBOL)
      {
        Glib::ustring word= bow.get_text(eow);
        Glib::ustring upcase= word.uppercase();

        // refresh word iterators
        bow= get_iter_at_line_offset(line, _sqlhl->words[i].word_begin);
        eow= get_iter_at_line_offset(line, _sqlhl->words[i].word_end+1);

        if (upcase != word)
        {
          erase(bow, eow);
          bow= get_iter_at_line_offset(line, _sqlhl->words[i].word_begin);
          insert(bow, upcase);
        }

        // refresh eol iterator
        eol= get_iter_at_line(line);
        if (!eol.forward_line())
          eol= end();
      }
 */
      if (!name.empty())
        apply_tag_by_name(name, bow, eow);
    }

    line++;
  } while (!eol.is_end());
}



std::list<Glib::ustring> MGSQLTextBuffer::get_suggestions(const Gtk::TextBuffer::iterator &bol)
{
  Gtk::TextBuffer::iterator eol= bol;
  eol.forward_word_end();

  std::list<Glib::ustring> list;

  Glib::ustring word= get_slice(bol, eol).uppercase();

  MYX_SYN_SUGGESTIONS* s= myx_lookup_line(_syn, word.c_str());

  // it seems the suggestions give more stuff than it should...
  for (unsigned int i=0; i < s->suggestions_num; i++)
  {
    // so we need to filter it a bit here
    Glib::ustring suggestion(s->suggestions[i].name);

    if (suggestion.size() >= word.size() && suggestion.uppercase().substr(0, word.size()) == word)
      list.push_back(suggestion);
  }
  myx_free_syn_suggestions(s);
  
  return list;
}
