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

#ifndef _MQHISTORY_H_
#define _MQHISTORY_H_

#include "myx_qb_public_interface.h"
#include <glibmm.h>

class MQHistory : public Glib::ObjectBase {
  public:
    typedef sigc::signal0<void> HistoryChangedSignal;

  private:
    MYX_HISTORY *_history;

    HistoryChangedSignal _signal_changed;

  public:
    MQHistory();
    static MQHistory *instance();
    
    void save();
    void reset();

    std::string check_dupe_and_mark(const Glib::ustring &query);
    std::string add_entry(const Glib::ustring &query,
                           const Glib::ustring &catalog,
                           const Glib::ustring &schema);
    std::string get_id_for(MYX_HISTORY_ENTRY *entry);

    MYX_HISTORY_ENTRY *find_entry(const std::string &id);
    MYX_HISTORY_TREE *get_tree();
    
    void mark_entry(MYX_HISTORY_ENTRY *entry);
    void remove_entry(MYX_HISTORY_ENTRY *entry);

    HistoryChangedSignal signal_changed() const { return _signal_changed; };
};



#endif /* _MQHISTORY_H_ */
