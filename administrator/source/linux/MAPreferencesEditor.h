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

#ifndef _MAPREFERENCESEDITOR_H_
#define _MAPREFERENCESEDITOR_H_

#include "MGPreferencesEditor.h"

class MAPreferenceGroup : public MGPreferenceGroup {
  protected:
    MGGladeXML *_xml;
    
  public:
    MAPreferenceGroup();
    virtual ~MAPreferenceGroup();

    virtual Glib::ustring get_title();
    virtual void init();
    virtual void update();
    virtual void commit();

    virtual Gtk::Widget *widget();
};

#endif /* _MAPREFERENCESEDITOR_H_ */
