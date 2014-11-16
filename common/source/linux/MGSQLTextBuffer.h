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

#ifndef _MGSQLTEXTBUFFER_H_
#define _MGSQLTEXTBUFFER_H_

#include <list>
#include <gtkmm/textbuffer.h>
#include "myx_public_interface.h"

class MGSQLTextBuffer : public Gtk::TextBuffer {
    MYX_SYN *_syn;
    MYX_SQL_HIGHLIGHTING *_sqlhl;

    Glib::RefPtr<Gtk::TextTagTable> _tagtable;
    
    MGSQLTextBuffer(const Glib::RefPtr<Gtk::TextTagTable> &tagtable);
  public:
    virtual ~MGSQLTextBuffer();
    bool init_highlighting(MYSQL *mysql);

    virtual void set_text(const Glib::ustring &text);
    
    void do_hiliting();

    std::list<Glib::ustring> get_suggestions(const Gtk::TextBuffer::iterator &bol);

    static Glib::RefPtr<MGSQLTextBuffer> create(const Glib::RefPtr<Gtk::TextTagTable> &tagtable, MYSQL *mysql);

    bool content_is_valid();
};


#endif /* _MGSQLTEXTBUFFER_H_ */

