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


#ifndef _MTRANSLATION_H_
#define _MTRANSLATION_H_

#include <string>
#include <glibmm.h>

#include <myx_library.h>


class MTranslation {
    MYX_TRANS *_trans;

  public:
    MTranslation();
    ~MTranslation();
    bool open(const std::string &file, const std::string &app_file, 
              const std::string &lang);
    
    Glib::ustring get(const char *group, const char *keyword, bool general= false);
    Glib::ustring get(const std::string &group, const std::string &keyword, bool general= false);
    Glib::ustring get(const Glib::ustring &group, const Glib::ustring &keyword, bool general= false);
};

#endif /* _MTRANSLATION_H_ */
