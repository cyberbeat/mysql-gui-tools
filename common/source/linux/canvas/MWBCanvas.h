/* Copyright (C) 2005 MySQL AB

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

#ifndef _MWBCANVAS_H_
#define _MWBCANVAS_H_

#include "MGCanvas.h"
#include <MySQLGRT/MGRT.h>
#include "myx_grt_wb_public_interface.h"

class MWBCanvas : public MGCanvas {
    MGRT *_grt;

  public:
    MWBCanvas(MGRT *_grt);
    
    void set_marker(int marker);
    void go_to_marker(int marker);
    bool has_marker(int marker);
};


#endif /* _MWBCANVAS_H_ */
