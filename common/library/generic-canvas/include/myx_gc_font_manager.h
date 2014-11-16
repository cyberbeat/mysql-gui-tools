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
 * @file myx_gc_font_manager.h 
 * @brief Implements the global font manager class that handles all text output related tasks.
 */

#ifndef __GC_FONT_MANAGER_H__
#define __GC_FONT_MANAGER_H__

#include "myx_gc_utilities.h"

#ifdef USE_FONTCONFIG
#include <fontconfig/fontconfig.h>
#endif

// FTGL
#include <FTGLOutlineFont.h>
#include <FTGLPolygonFont.h>
#include <FTGLBitmapFont.h>
#include <FTGLTextureFont.h>
#include <FTGLPixmapFont.h>
#include <FTGLExtrdFont.h>

//----------------------------------------------------------------------------------------------------------------------

typedef map<string, FTFont*> Fonts;

typedef struct tagFontFileEntry
{
  int useCount;
  string entries[2][2];
} FontFileEntry;

typedef map<string, FontFileEntry*> FontFiles;

/**
 * CFontManager is a helper class for text output in the generic canvas. It maps a description string for a font with 
 * attributes to a display list. If there is no display for a given font then one is created.
 * The font manager is basically a singleton class. We only need one instance of it.
 */
class CFontManager
{
private:
  Fonts FFonts;
  bool FUseLocalDisplayLists;
  string FFontResourcesPath;

#ifndef USE_FONTCONFIG
  FontFiles FFiles;
#endif

protected:
  string getFontFile(string family, const string& style, int Weight);
public:
  CFontManager(void);
  virtual ~CFontManager(void);

  float ascender(const string& fontId);
  void boundingBox(const wstring& output, const string& fontId, TBoundingBox& Box);
  void clear(void);
  float descender(const string& fontId);
  string fontIdCreate(const string& family, const string& style, int weight, int fontSize);
  float lineHeight(const string& fontId);
  void setFontResourcesPath(const string& path);
  void textOut(const wstring& output, const string& fontId);
};

//----------------------------------------------------------------------------------------------------------------------

int convertFontWeight(const string Weight);

/** Returns the singleton font manager instance. */
CFontManager* fontManager(void);
/** Increase lock count for the manager. */
void lockFontManager(void);
/** Decrease lock count for the manager. */
void unlockFontManager(void);

#endif // #ifdef __GC_FONT_MANAGER_H__

