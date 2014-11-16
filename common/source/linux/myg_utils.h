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

#ifndef _MYG_UTILS_H_
#define _MYG_UTILS_H_

#ifndef WITHOUT_MYX
#include "myx_public_interface.h"
#endif

#include <glibmm.h>

class MGGladeXML;


void myg_set_datadir(const std::string &path);

// wrap and free the string
inline Glib::ustring uswrapf(char *s) 
{ 
    Glib::ustring us= s; 
    g_free(s); 
    return us;
}

typedef struct {
  char *name;
  char *widget;
  int type; // 'B' bool, 'S' string
} MGAdvancedOption;

bool myg_fetch_advanced_options(MGGladeXML *xml, MGAdvancedOption *options,
                                std::list<std::string> &advanced_options);


bool myg_show_advanced_options(MGGladeXML *xml, MGAdvancedOption *options,
                               const std::list<std::string> &advanced_options);

void myg_convert_string_list(char **list, unsigned int list_num,
                             std::list<std::string> &slist);


void myg_convert_string_list(const std::list<std::string> &slist,
                             char **&list, unsigned int &list_num);

#ifndef WITHOUT_MYX
Glib::ustring myg_message_for_xlib_error(MYX_LIB_ERROR err);
#endif

Glib::ustring ufmt(const gchar *msg, ...);
std::string tostr(int num);

Glib::ustring strjoin(const std::list<Glib::ustring> &list);
Glib::ustring strjoin(const std::vector<Glib::ustring> &list);

Glib::ustring strreplace(const Glib::ustring &str,const Glib::ustring &from,const Glib::ustring &to);


#if __GNUC__ < 3
#define myg_log(msg, args...) g_printerr("%s: "msg"\n", g_get_prgname(), args)
#else
#define myg_log(msg, ...) g_printerr("%s: "msg"\n", g_get_prgname(), ## __VA_ARGS__)
#endif

#endif /* _MYG_UTILS_H_ */
