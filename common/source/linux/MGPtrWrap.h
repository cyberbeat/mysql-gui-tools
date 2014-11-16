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

#ifndef _MGPTRWRAP_H_
#define _MGPTRWRAP_H_

template <typename T>
class MGPtrWrap 
{
  public:
    typedef int (*MGDestroyFunction)(T);
  private:
    T _data;
    int _refcount;

    MGDestroyFunction _destroy_func;
  public:
    MGPtrWrap(T ptr) : _data(ptr), _refcount(1), _destroy_func(0) {};
    MGPtrWrap(T ptr, MGDestroyFunction destroy_func) : _data(ptr), _refcount(1), _destroy_func(destroy_func) {};
    ~MGPtrWrap()
    {
      if (_destroy_func && _data)
        (*_destroy_func)(_data);
    }

    T ptr() { return _data; };
    
    void unreference() { _refcount--; if (_refcount <= 0) delete this; };
    void reference() { _refcount++; };
};

#endif /* _MGPTRWRAP_H_ */
