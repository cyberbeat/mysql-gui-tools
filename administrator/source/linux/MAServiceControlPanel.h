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


#ifndef _MASERVICECONTROLPANEL_H_
#define _MASERVICECONTROLPANEL_H_

#include "MAPanel.h"

#include "MYXInterface.h"

class MDataInterface;

class MAServiceControlPanel : public MAPanel {
    MYX::UserConnection _last_user_conn;
    bool _can_reconnect;

    void set_server_state(bool running);
    
    void start_stop_service(bool stop);

    void append_log_text(const Glib::ustring &msg);

    void toggle_service();
    
    void reconnect();
  public:
    MAServiceControlPanel(MAdministrator *app, MDataInterface *data);

    virtual bool before_show();
    virtual bool before_hide();

    virtual bool init();

    virtual bool is_local_only() { return true; };
    virtual bool needs_connection() { return false; };
};

extern MAPanel *create_service_control_panel(MAdministrator *app, MDataInterface *data);

#endif /* _MASERVICECONTROLPANEL_H_ */
