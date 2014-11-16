
/* Copyright (C) 2003,2004 MySQL AB

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

#ifndef _myx_const_string_h
# define _myx_const_string_h

#include <string.h>
#include "myx_util_functions.h"

///////////////////////////////////////////////////////////////////////////////
class const_string
{
 public:

  const_string()                             { m_data= 0;   m_length= 0;   }
  const_string(const char * str, size_t len) { m_data= str; m_length= len; }
  const_string(const const_string & s)       { 
                                               m_data= s.m_data;
                                               m_length= s.m_length; 
                                             }

  bool operator == (const const_string & s) const
         { return m_length==s.m_length && !strncmp(m_data,s.m_data,m_length); }
  bool operator != (const const_string & s) const
         { return !(*this==s); }

  bool equal_case_insensitively_to(const const_string & str)const
  {
    return length()==str.length() && !strncasecmp(data(),str.data(),length());
  }

  char operator [] (size_t i) const {return m_data[i];}

  size_t        length    () const {return m_length;}
  const char *  data  () const {return m_data;}

  const char *  begin () const {return m_data;}
  const char *  end   () const {return m_data+m_length;}

  const char & front () const { return m_data[0]; }
  const char & back  () const { return m_data[m_length-1]; }

  bool empty() const { return m_data==0 || m_length==0; }

 protected:
  const char * m_data;
  size_t m_length;
};
#define CONST_STR(s) const_string(s,sizeof(s)-1)
///////////////////////////////////////////////////////////////////////////////

#endif // _myx_const_string_h
