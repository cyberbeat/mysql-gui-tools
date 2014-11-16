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


#ifndef _MGPREFERENCESEDITOR_H_
#define _MGPREFERENCESEDITOR_H_

#include <gtkmm.h>

#include <list>

#include "MGPreferences.h"
#include "MGGladeXML.h"

#include "MYXInterface.h"
#include "myg_utils.h"

using MYX::UserConnection;
using MYX::UserConnectionList;

class MGPreferencesEditor;

class MGPreferenceGroup : public Glib::ObjectBase {
  protected:
    friend class MGPreferencesEditor;

    MGPreferences *_prefs;
    MGPreferencesEditor *_owner;

  public:

    void set_dirty();

    virtual Glib::ustring get_title()= 0;

    virtual void init()= 0;
    virtual void update()= 0;
    virtual void commit()= 0;

    virtual Gtk::Widget *widget()= 0;
};


class MGPreferencesEditor : public Gtk::Window {
  protected:
    friend class MGPreferenceGroup;
    friend class MGConnectionGroup;
    friend class MGGeneralGroup;
    
    sigc::signal0<void> _changed_signal;
    sigc::signal0<void> _closed_signal;
    
    MGPreferences *_prefs;

    MGGladeXML *_xml;

    Glib::ustring _heading;
    bool _dirty;
    
    UserConnectionList *_connections;

    Gtk::Widget *get_widget(const std::string &name) { return _xml->get_widget(name); };

    MGPreferenceGroup *_connGroup;
    std::list<Gtk::ToggleButton*> _buttons;
    std::list<MGPreferenceGroup*> _groups;
    
    void update();

    void update_sensitivity();
    void set_dirty();
    void unset_dirty();

    bool close_window(GdkEventAny *ev);
    void discard_clicked();
    void apply_clicked();
    void close_clicked();

    void menu_changed(MGPreferenceGroup *group);

    bool expose_heading(GdkEventExpose *expose);

    ~MGPreferencesEditor();

  public:
    MGPreferencesEditor(GtkWindow *widget);
    static MGPreferencesEditor *instance();
    static void setup(MGPreferences *prefs);

    void init();

    virtual void show(bool connection_only= false);
    
    void addGroup(MGPreferenceGroup *group, const Glib::ustring &caption,
                  Glib::RefPtr<Gdk::Pixbuf> icon);
        
    sigc::signal0<void> signal_changed() { return _changed_signal; };
    sigc::signal0<void> signal_closed() { return _closed_signal; };
};


#endif /* _MGPREFERENCESEDITOR_H_ */
