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
#include "myg_gtkutils.h"
#include "MQBaseTab.h"

MQBaseTab::MQBaseTab()
  : _tab_box(false, 2)
{
  set_shadow_type(Gtk::SHADOW_NONE);
  
  _button.set_image(PIXCACHE->load("tabsheet_icon_close2.png"));
  _button.set_hover_image(PIXCACHE->load("tabsheet_icon_close1.png"));
  _button.set_alt_image(PIXCACHE->load("tabsheet_icon_close3.png"));
  _button.show_all();
  _button.set_relief(Gtk::RELIEF_NONE);
  _button.set_size_request(14, 10);
  _button.signal_clicked().connect(sigc::bind<TabActionType>(sigc::mem_fun(*this,&MQBaseTab::do_tab_action),AClose));
  
  _tab_box.pack_start(_icon, false, false);
  _tab_box.pack_start(_label, true, true);
  _tab_box.pack_start(_button, false, false);
  _tab_box.show_all();
}


void MQBaseTab::do_tab_action(TabActionType type)
{
  _tab_action(this,type);
}


void MQBaseTab::set_tab_action_handler(const TabActionSlot &slot)
{
  _tab_action= slot;
}


void MQBaseTab::set_icon(const Glib::RefPtr<Gdk::Pixbuf> &icon)
{
  _icon_image= icon;
  _icon.set(icon);
}


void MQBaseTab::set_busy(bool flag)
{
  if (flag)
    _icon.set(PIXCACHE->load_anim("busy.gif"));
  else
    _icon.set(_icon_image);
}
 

void MQBaseTab::set_title(const Glib::ustring &title)
{
  _label.set_text(title);
}
