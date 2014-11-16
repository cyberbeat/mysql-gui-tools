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

#ifndef _MQPREFERENCES_H_
#define _MQPREFERENCES_H_

#include "MGPreferences.h"

class MQPreferences : public MGPreferences 
{
    virtual void process_options(MYX_APPLICATION_OPTIONS *options, bool shared);
    virtual MYX_APPLICATION_OPTIONS *prepare_options(bool shared);
  public:
    MQPreferences();

    bool dont_beep;
    bool auto_add_pk_check;
    
    int default_limit_value; // default value for LIMIT
    
    int view_type; // 'N'ormal, 'Q'uery area, 'R'resultset
    bool show_sidebar;
    int max_query_history;
    int max_blob_length;
};


#endif /* _MQPREFERENCES_H_ */
