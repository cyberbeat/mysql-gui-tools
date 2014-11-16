/* Copyright (C) 2003 MySQL AB

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


#ifndef _MASERVERINFORMATIONPANEL_H_
#define _MASERVERINFORMATIONPANEL_H_

#include "MAPanel.h"

class MDataInterface;


class MAServerInformationPanel : public MAPanel {

    Glib::RefPtr<Gtk::SizeGroup> _size_group;
    
    void set_server_state(bool running);
    
  public:
    MAServerInformationPanel(MAdministrator *app, MDataInterface *data);

    virtual bool before_show();
    virtual bool before_hide();
    
    virtual void show();

    virtual bool init();

    virtual bool is_local_only() { return false; };
    virtual bool needs_connection() { return true; };
};

extern MAPanel *create_server_information_panel(MAdministrator *app, MDataInterface *data);

#endif /* _MASERVERINFORMATIONPANEL_H_ */
