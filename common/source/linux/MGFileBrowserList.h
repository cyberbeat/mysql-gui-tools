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



#ifndef _MGFILEBROWSERLIST_H_
#define _MGFILEBROWSERLIST_H_

#include "MGBrowserList.h"

class MGFileBrowserList : public MGBrowserList {
  public:
    class Columns : public Gtk::TreeModel::ColumnRecord {
      public:
        Columns() {
          add(_name);
        };
        Gtk::TreeModelColumn<Glib::ustring> _name;
    } _columns;

  private:
    std::string _path;
    std::string _extension;
        
    Gtk::Label *_label;
    
    bool _is_empty;
    Glib::ustring _empty_message;

    virtual void refresh_list(const Glib::ustring &filter);

    virtual bool list_selection_function(const Glib::RefPtr<Gtk::TreeModel> &model,
                                         const Gtk::TreeModel::Path &path,
                                         bool path_currently_selected);

    void change_path();

  public:
    MGFileBrowserList(const Glib::ustring &title,
                      const std::string &path=std::string(),
                      bool enable_change_path=false);
    
    virtual void set_empty_message(const Glib::ustring &message);
    
    void get_row_object(const Gtk::TreeIter &iter, std::string &item);

    void set_extension(const std::string &ext);
    
    std::string get_path_for_entry(const std::string &name);
    std::string get_directory() { return _path; };
    
    bool check_exists(const std::string &name);

    bool rename(const std::string &old_name, const std::string &new_name);
    bool remove(const std::string &name);
};


#endif /* _MGFILEBROWSERLIST_H_ */
