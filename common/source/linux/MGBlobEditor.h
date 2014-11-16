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

#ifndef _MGBLOBEDITOR_H_
#define _MGBLOBEDITOR_H_

#include <gtkmm/window.h>
#include <gtkmm/label.h>
#include <gtkmm/box.h>
#include <gtkmm/notebook.h>
#include <gtkmm/textview.h>
#include <gtkmm/image.h>
#include <gtkmm/treeview.h>
#include <gtkmm/liststore.h>
#include <gtkmm/buttonbox.h>

class MGBlobListStore;

class MGBlobEditor : public Gtk::Window
{
    class Columns : public Gtk::TreeModel::ColumnRecord
    {
      public:
        Columns()
        {
          add(offset);
          for (int i= 0; i < 16; i++)
          {
            data.push_back(Gtk::TreeModelColumn<std::string>());
            add(data[i]);
          }
          for (int i= 0; i < 4; i++)
          {
            text.push_back(Gtk::TreeModelColumn<std::string>());
            add(text[i]);
          }
        }
        Gtk::TreeModelColumn<std::string> offset;
        std::vector<Gtk::TreeModelColumn<std::string> > data;
        std::vector<Gtk::TreeModelColumn<std::string> > text;
    } _columns;
    
  public:
    enum ViewType {
      VText=(1<<0),
      VImage=(1<<1),
      VBinary=(1<<2)
    };
  protected:
    Gtk::Label _summary;
    Gtk::VBox _topbox;
    Gtk::Notebook _note;
    Gtk::TextView *_text;
    Gtk::HButtonBox _bbox;
    Gtk::Button *_button1, *_button2;

    Gtk::Image *_image;
    Gtk::Label *_image_label;
    
    Gtk::TreeView *_tree;
    Glib::RefPtr<MGBlobListStore> _tree_store;
    
    ViewType _vtypes;
    
    gpointer _data;
    gsize _size;
    
    sigc::signal0<void> _save_signal;
    
    bool _delete_on_close;
    
    bool delete_event(GdkEventAny *ev);
    void close();
    void save();

  public:
    MGBlobEditor(ViewType vtypes, bool edit= false);
    virtual ~MGBlobEditor();
    void set_delete_on_close(bool flag= true);
    void set_data(gpointer data, gsize size);
    void get_data(gpointer &data, gsize &size);
    
    void note_page_switched(GtkNotebookPage *, guint);
    sigc::signal0<void> signal_save() { return _save_signal; }
};


#endif /* _MGBLOBEDITOR_H_ */
