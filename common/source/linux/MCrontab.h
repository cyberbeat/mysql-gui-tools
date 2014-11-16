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

#ifndef _MCRONTAB_H_
#define _MCRONTAB_H_

#include <glibmm.h>


class MCrontab {
  public:
    class Entry {
    public:
      Entry() : minute(-1), hour(-1),  month(-1), weekday(0) {};
      int minute;
      int hour;
      std::string day;
      int month;
      unsigned int weekday; // bitmask
      std::string command;
    };

  protected:
    std::list<std::string> _lines;

    std::string format_entry(const Entry &entry);
    
  public:
    bool find_entry_by_comment(const std::string &comment, Entry &entry);

    bool remove_entry_with_comment(const std::string &comment,
                                             const std::string &command="");
    
    void add_entry(Entry &entry, const std::string &comment);

    bool load();
    bool install();

    bool parse_line(const std::string &line, Entry &entry);
    std::list<std::string> get_lines() { return _lines; };
};


#endif /* _MCRONTAB_H_ */
