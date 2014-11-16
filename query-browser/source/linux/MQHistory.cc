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

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE /* glibc2 needs this */
#endif
#include <time.h>

#include "myx_public_interface.h"
#include <glibmm.h>

#include "MQHistory.h"

MQHistory::MQHistory()
{
  MYX_LIB_ERROR err;
  
  _history= myx_history_load(prefs.build_path_to("query-browser/history.xml").c_str(), &err);
  if (!_history)
  {
    _history= g_new0(MYX_HISTORY, 1);
  }
}


static MQHistory *_instance= 0;

MQHistory *MQHistory::instance()
{
  if (!_instance)
    _instance= new MQHistory();

  return _instance;
}


void MQHistory::save()
{
  MYX_LIB_ERROR err;

  err= myx_history_store(prefs.build_path_to("query-browser/history.xml").c_str(),
			_history);
}


void MQHistory::reset()
{
  myx_history_free(_history);

  _history= g_new0(MYX_HISTORY, 1);
  save();
  _signal_changed.emit();
}



std::string MQHistory::add_entry(const Glib::ustring &query,
                                  const Glib::ustring &catalog,
                                  const Glib::ustring &schema)
{
  MYX_HISTORY_ENTRY *item= myx_history_add_entry(_history,
                                                 catalog.c_str(), schema.c_str(),
                                                 query.c_str(),
                                                 prefs.max_query_history);

  save();

  _signal_changed.emit();

  return get_id_for(item);
}


MYX_HISTORY_ENTRY *MQHistory::find_entry(const std::string &id)
{
  for (unsigned int i= 0; i < _history->entries_num; i++)
  {
    if (id == get_id_for(_history->entries+i))
    return _history->entries+i;
  }
  return NULL;
}


std::string MQHistory::check_dupe_and_mark(const Glib::ustring &query)
{
  for (unsigned int i= 0; i < _history->entries_num; i++)
  {
    if (query.compare(_history->entries[i].sql)==0)
    {
      mark_entry(_history->entries+i);
      save();
      _signal_changed.emit();
      return get_id_for(_history->entries+i);
    }
  }
  return "";
}


std::string MQHistory::get_id_for(MYX_HISTORY_ENTRY *entry)
{ 
  return std::string(entry->sql);
}


void MQHistory::mark_entry(MYX_HISTORY_ENTRY *entry)
{
  time_t timestamp;
  struct tm *cur_tm_date;
  char *g_str;

  timestamp= time(NULL);
  cur_tm_date= gmtime(&timestamp);
  //YYYY-MM-DDThh:mm:ssTZD  TZD= timezone or Z for UTC
  g_str= g_strdup_printf("%d-%02d-%02dT%02d:%02d:%02dZ", cur_tm_date->tm_year+1900,
                         cur_tm_date->tm_mon+1, cur_tm_date->tm_mday,
                         cur_tm_date->tm_hour, cur_tm_date->tm_min,
                         cur_tm_date->tm_sec);
  entry->date_last_access=g_str;
}


void MQHistory::remove_entry(MYX_HISTORY_ENTRY *entry)
{
  entry->marked_deleted= 1;
  save();
}


MYX_HISTORY_TREE *MQHistory::get_tree()
{
  return myx_history_get_tree(_history);
}
