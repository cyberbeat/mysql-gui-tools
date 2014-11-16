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


#ifndef _MASTARTUPPARAMETERSPANEL_H_
#define _MASTARTUPPARAMETERSPANEL_H_

#include "MAPanel.h"
#include "MDynamicInterface.h"

class MDataInterface;


class MAStartupParametersPanel : public MAPanel {

    MGGladeXML *_file_dlg;
    MGGladeXML *_showcase;

    MDynamicInterface *_dint;

    bool _my_cnf_missing_error_displayed;
    
    bool show_configuration(const std::string &file, const std::string &section);

    virtual void show();

    bool create_file(const std::string &path);
    void request_file_path();
    
    void select_file();
    void revert_changes();
    void apply_changes();
    
  public:
    MAStartupParametersPanel(MAdministrator *app, MDataInterface *data);
    ~MAStartupParametersPanel();

    virtual bool before_show();
    virtual bool before_hide();
    
    virtual bool init();

    virtual bool is_local_only() { return true; };
    virtual bool needs_connection() { return false; };
};

extern MAPanel *create_startup_parameters_panel(MAdministrator *app, MDataInterface *data);

#endif /* _MASTARTUPPARAMETERSPANEL_H_ */
