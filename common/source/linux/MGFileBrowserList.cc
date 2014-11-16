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
#include "MGFileBrowserList.h"
#include <glibmm.h>
#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h> /* rename() in FreeBSD */
#include <cstring>


MGFileBrowserList::MGFileBrowserList(const Glib::ustring &title,
                                     const std::string &path,
                                     bool enable_change_path)
  : MGBrowserList(false, title), _path(path), _is_empty(false)
{
  if (!g_file_test(path.c_str(), G_FILE_TEST_IS_DIR))
  {
    if (mkdir(path.c_str(), 0700) < 0)
    {
      g_warning(_("Could not create directory %s: %s"), path.c_str(),
                strerror(errno));
    }
  }
  
  if (enable_change_path)
  {
    _label= Gtk::manage(new Gtk::Label(path));
    pack_start(*_label, false, true);
    _label->set_alignment(0.0, 0.5);
    _label->set_line_wrap(true);
    _label->show();
    
    Gtk::Button *btn= Gtk::manage(new Gtk::Button(_("Change Path...")));
    pack_start(*btn, false, true);
    btn->show();
    btn->signal_clicked().connect(sigc::mem_fun(*this,&MGFileBrowserList::change_path));
  }

  _tree->append_column("", _columns._name);
  set_store(Gtk::TreeStore::create(_columns));
}


void MGFileBrowserList::set_empty_message(const Glib::ustring &message)
{
  _empty_message= message;
}


void MGFileBrowserList::refresh_list(const Glib::ustring &filter)
{
  try
  {
    Glib::Dir dir(_path);
    std::string file;
    Gtk::TreeIter old_sel;
    std::string old_file;
    
    old_sel= get_selected();
    if (old_sel)
      get_row_object(old_sel, old_file);
    old_sel= Gtk::TreeIter();
    
    _refreshing= true;
    
    clear();
    
    while (!(file= dir.read_name()).empty())
    {
      Glib::ustring s= Glib::filename_to_utf8(file);
      
      if (s.find(filter)!=Glib::ustring::npos)
      {
        if (str_has_suffix(s, _extension))
        {
          Gtk::TreeIter iter= _store->append();
          Gtk::TreeModel::Row row= *iter;
          
          s= s.substr(0, s.size()-_extension.size());
          row[_columns._name]= s;
          
          if (old_file == s)
            old_sel= iter;
        }
      }
    }
    
    if (old_sel)
      set_selection(old_sel);
    
    _refreshing= false;
    
    // if old selected item disappeared
    if (!old_sel && !old_file.empty())
      list_selection_changed();
    
    if (_store->children().empty())
    {
      Gtk::TreeIter iter= _store->append();
      Gtk::TreeRow row= *iter;
      
      row[_columns._name]= _empty_message;
      
      _is_empty= true;
    }
    else
      _is_empty= false;
  }
  catch (...)
  {
    clear();
  }
}


void MGFileBrowserList::get_row_object(const Gtk::TreeIter &iter,
                                       std::string &item)
{
  if (iter)
  {
    Gtk::TreeModel::Row row= *iter;
  
    Glib::ustring s= row[_columns._name];
  
    item= s;
  }
}


void MGFileBrowserList::set_extension(const std::string &ext)
{
  _extension= ext;
}


std::string MGFileBrowserList::get_path_for_entry(const std::string &name)
{
  return Glib::build_filename(_path, name)+_extension;
}


bool MGFileBrowserList::check_exists(const std::string &name)
{
  if (g_file_test(get_path_for_entry(name).c_str(), G_FILE_TEST_EXISTS))
      return true;
  return false;
}


bool MGFileBrowserList::rename(const std::string &old_name, 
                               const std::string &new_name)
{
  std::string opath, npath;
  
  opath= get_path_for_entry(old_name);
  npath= get_path_for_entry(new_name);
  
  if (::rename(opath.c_str(), npath.c_str()) < 0)
  {
    g_warning(_("Could not rename file '%s' to '%s': %s"),
            opath.c_str(), npath.c_str(), strerror(errno));
    return false;
  }
  return true;
}


bool MGFileBrowserList::remove(const std::string &name)
{
  std::string path= get_path_for_entry(name);

  if (::unlink(path.c_str()) < 0)
  {
    g_warning(_("Could not erase file '%s': %s"),
            path.c_str(), strerror(errno));
    return false;
  }
  return true;
}


void MGFileBrowserList::change_path()
{
  Gtk::FileSelection fsel(_("Select Backup File Directory"));

  fsel.hide_fileop_buttons();
  fsel.set_filename(_path+"/");
  fsel.get_file_list()->set_sensitive(false);

  if (fsel.run() == Gtk::RESPONSE_OK)
  {
    _path= fsel.get_filename();
    _label->set_label(_path);

    refresh();
  }
}


bool MGFileBrowserList::list_selection_function(const Glib::RefPtr<Gtk::TreeModel> &model,
                                                const Gtk::TreeModel::Path &path,
                                                bool path_currently_selected)
{
  if (_is_empty)
    return false;
  
  return MGBrowserList::list_selection_function(model, path, path_currently_selected);
}
