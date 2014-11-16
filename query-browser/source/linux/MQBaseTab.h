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

#ifndef _MQBASETAB_H_
#define _MQBASETAB_H_

#include <gtkmm/frame.h>
#include <gtkmm/label.h>
#include <gtkmm/button.h>
#include <gtkmm/box.h>
#include <gtkmm/image.h>
#include "MGImageButton.h"

enum MQTabType {
  TResultSet,
  THelp,

  TScriptEditor
};


class MQBaseTab : public Gtk::Frame {
  public:
    enum TabActionType {
      ANewScript,
      ANewQuery,
      ACloseOther,
      AClose,
      ASplit,
      ASplitV,
      AUnsplit
    };

    typedef sigc::slot2<void,MQBaseTab*,/*TabActionType*/int> TabActionSlot;
    
  protected:
    Gtk::HBox _tab_box;
    Gtk::Image _icon;
    Gtk::Label _label; // label to be used as title in notebook
    MGImageButton _button; // close button
     
    Glib::RefPtr<Gdk::Pixbuf> _icon_image;
    
    TabActionSlot _tab_action;

  public:
    MQBaseTab();


    virtual bool tab_will_close() { return true; };
    virtual void tab_did_close() {};

    virtual MQTabType get_type()= 0;

    void do_tab_action(TabActionType type);
    
    void set_tab_action_handler(const TabActionSlot &slot);

    void set_busy(bool flag);
    void set_icon(const Glib::RefPtr<Gdk::Pixbuf> &icon);
    void set_title(const Glib::ustring &title);

    Gtk::Widget *get_tab_widget() { return &_tab_box; };
};


#endif /* _MQBASETAB_H_ */
