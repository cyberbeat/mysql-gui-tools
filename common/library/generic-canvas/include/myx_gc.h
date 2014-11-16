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

/**
 * @file myx_gc.h 
 * @brief Base configuration header. Here most of the platform specific switches are kept.
 */

#ifndef __GC_H__
#define __GC_H__

#if defined(__WIN__) || defined(_WIN32) // _WIN32 also includes WIN64
  #ifndef _WINDOWS
    // Just to ease life. This way the symbol has not to be given during build.
    // __WIN__ and _WIN32 are implicitely defined on Windows, however _WINDOWS is not.
    #define _WINDOWS
  #endif

  #define  WIN32_LEAN_AND_MEAN
  #include <windows.h>
  #include <direct.h>
  #include <shlobj.h> // Font folder location
  #include <wingdi.h> // wglGetProcAddress
  #include "mmsystem.h" // OutputDebugStr

  // The export/import template macro is needed if any STL template is used across DLL boundaries.
  // The only exceptions are string and wstring, as they are already exported.
  // Make sure all particpating DLLs/apps are using the same shared library type (either debug or release)
  // and they must use a shared (dynamic) runtime instead of a statically linked one.
  #ifdef MAKEDLL
    #define GENERIC_CANVAS_API  __declspec(dllexport)
    #define EXPORT_IMPORT_TEMPLATE
  #else
    #define GENERIC_CANVAS_API  __declspec(dllimport)
    #define EXPORT_IMPORT_TEMPLATE extern
  #endif

  #pragma warning(disable: 4251) // Disable warning about DLL interface for template classes.

  #ifdef __BORLANDC__
    #define HIDESBASE __declspec(hidesbase)
  #endif

  #include <GL/gl.h>
  #include <GL/glu.h>
  #include "gc_glext.h"
#else // !WINDOWS
  #define GENERIC_CANVAS_API 
  #define __cdecl

  #include <stdlib.h>
  #define MAX_PATH PATH_MAX
  #define HIDESBASE

  #ifdef __APPLE__
    #define GL_GLEXT_LEGACY
    #include <OpenGL/gl.h>
    #include "gc_glext.h"
    #include <OpenGL/glu.h>
    #include <OpenGL/OpenGL.h>
  #else // !__APPLE__
    #include <X11/Xlib.h>

    #define HIDESBASE
    #define GL_GLEXT_LEGACY
    #include <GL/gl.h>
    #include "gc_glext.h"
    #include <GL/glu.h>
    #define GLX_GLXEXT_LEGACY
    #include <GL/glx.h>
    #include <GL/glxext.h>
  #endif
#endif // !_WINDOWS

#define _USE_MATH_DEFINES
#include <math.h>

#include <time.h>

// STL namespaces and templates.
#include <string>
#include <map>
#include <set>
#include <vector>
#include <algorithm>

using namespace std;

#include <errno.h>
#include <glib.h>

#endif // #ifndef __GC_H__
