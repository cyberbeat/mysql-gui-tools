/* Copyright (C) 2003, 2004 MySQL AB

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


#ifndef _MAPANEL_H_
#define _MAPANEL_H_


//#include "MTranslation.h"

class MAdministrator;
class MDataInterface;
class MInstanceInfo;

#include <gtkmm.h>

#include "MGGladeXML.h"

class MAPanel : public Glib::ObjectBase  {
 protected:
    MGGladeXML *_xml;

    MAdministrator *_app;
    MDataInterface *_data;
    MInstanceInfo *_inst;
    //MTranslation *_transl;

    Gtk::Container *_parent_frame;
    Gtk::Widget *_panel;

    Gtk::Widget *get_widget(const Glib::ustring &name) const;
    Gtk::Entry *get_entry(const Glib::ustring &name) const;
    Gtk::ToggleButton *get_toggle(const Glib::ustring &name) const;
    Gtk::TreeView *get_tree(const Glib::ustring &name) const;
    Gtk::Button *get_button(const Glib::ustring &name) const;
    Gtk::Label *get_label(const Glib::ustring &name) const;
    Gtk::SpinButton *get_spin(const Glib::ustring &name) const;
 public:
    MAPanel(MAdministrator *app, MDataInterface *data);
    ~MAPanel();

    virtual bool init()= 0;
    virtual bool init_from_glade(const std::string &glade_file, const std::string &panel_name);

    virtual bool before_show() { return true; };
    virtual bool before_hide() { return true; };
    
    virtual bool before_quit() { return true; };

    virtual void show();
    virtual void hide();

    virtual bool has_unsaved_changes() const { return false; };

    virtual bool is_local_only()= 0;
    virtual bool needs_connection()= 0;
};


#endif /* _MAPANEL_H_ */
