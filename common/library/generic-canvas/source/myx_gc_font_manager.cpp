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
 * @file myx_gc_font_manager.cpp 
 * @brief Implements the global font manager class that handles all text output related tasks.
 */

#ifdef __APPLE__
#include <CoreServices/CoreServices.h>
#include <ApplicationServices/ApplicationServices.h>
#include <Carbon/Carbon.h>
#endif

#include "myx_gc_font_manager.h"

//#define FONTCLASS FTGLPixmapFont
#define FONTCLASS FTGLBitmapFont
//#define FONTCLASS FTGLTextureFont

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given string into a font weight value.
 * Allowed values are: normal | bold | bolder | lighter | 100 | 200 | 300| 400 | 500 | 600 | 700 | 800 | 900 | inherit
 *
 * @param weight A string containing a font weight specification.
 * @return A font weight value corresponding to the given description.
 */
int convertFontWeight(const string weight)
{
  int result = 0;
#ifdef USE_FONTCONFIG
  if (weight == "normal")
    result = FC_WEIGHT_NORMAL;
  else if (weight == "bold")
    result = FC_WEIGHT_BOLD;
  else if (weight == "bolder")
    result = FC_WEIGHT_EXTRABOLD;
  else if (weight == "lighter")
    result = FC_WEIGHT_LIGHT;
  else if (weight == "inherit")
    result = FC_WEIGHT_NORMAL;
  else
    result = atoi(weight.c_str());
#else // !USE_FONTCONFIG
  if (weight == "normal")
    result = 400;
  else
    if (weight == "bold")
      result = 700;
    else
      if (weight == "bolder")
        result = 800;
      else
        if (weight == "lighter")
          result = 300;
        else
          if (weight == "inherit")
            result = 400;
          else
            result = atoi(weight.c_str());
#endif

  return result;
}

//----------------- CFontManager ---------------------------------------------------------------------------------------

static CFontManager* internalManager;
static int lockCount = 0;

// Returns the current font manager (there is always only one).
CFontManager* fontManager(void)
{
  return internalManager;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Increases the lock count of the font manager. If the manager does not yet exist it is created.
 *
 */
void lockFontManager(void)
{
  if (internalManager == NULL)
  {
    internalManager = new CFontManager();
#ifdef USE_FONTCONFIG
    if (!FcInit())
      g_warning("Could not initialize Fontconfig");
#endif
  }
  ++lockCount;
}

//----------------------------------------------------------------------------------------------------------------------

// Returns the current font manager (there is always only one).
void unlockFontManager(void)
{
  if (lockCount > 0)
  {
    --lockCount;
    if (lockCount == 0)
    {
      delete internalManager;
      internalManager = NULL;
    }
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Constructor of the class.
 */
CFontManager::CFontManager(void)
{
  FUseLocalDisplayLists = false; // Set only to true if the text is as a whole not rendered in a display list.    

#ifndef USE_FONTCONFIG
  // Fill our font file list with some predefined font files.
  FontFileEntry* entry;

  // "Arial" normal, bold, italic, bold italic.
  entry = new FontFileEntry;
  entry->entries[false][false] = "Arial.ttf"; entry->entries[false][true] = "Arialbd.ttf"; 
  entry->entries[true][false] = "Ariali.ttf";  entry->entries[true][true] = "Arialbi.ttf"; 
  entry->useCount = 5;
  FFiles["Arial"] = entry;
  FFiles["Arial Normal"] = entry;
  FFiles["Arial-Normal"] = entry;
  FFiles["Arial Standard"] = entry;
  FFiles["Arial-Standard"] = entry;

  // Courier New
  entry = new FontFileEntry;
  entry->entries[false][false] = "Cour.ttf"; entry->entries[false][true] = "Courbd.ttf"; 
  entry->entries[true][false] = "Couri.ttf";  entry->entries[true][true] = "Courbi.ttf"; 
  entry->useCount = 3;
  FFiles["Courier New"] = entry;
  FFiles["Courier New Standard"] = entry;
  FFiles["Courier"] = entry;

  // Garamond
  entry = new FontFileEntry;
  entry->entries[false][false] = "Gara.ttf"; entry->entries[false][true] = "Garabd.ttf"; 
  entry->entries[true][false] = "Garait.ttf";  entry->entries[true][true] = "Garabi.ttf";
  entry->useCount = 3;
  FFiles["Garamond"] = entry;
  FFiles["Garamond Standard"] = entry;
  FFiles["Garamond-Standard"] = entry;

  // Palatino Linotype
  entry = new FontFileEntry;
  entry->entries[false][false] = "Pala.ttf"; entry->entries[false][true] = "Palab.ttf"; 
  entry->entries[true][false] = "Palai.ttf";  entry->entries[true][true] = "Palabi.ttf";
  entry->useCount = 1;
  FFiles["Palation Linotype"] = entry;

  // Tahoma
  entry = new FontFileEntry;
  entry->entries[false][false] = "Tahoma.ttf"; entry->entries[false][true] = "Tahomabd.ttf"; 
  entry->entries[true][false] = "Tahomai.ttf";  entry->entries[true][true] = "Tahomabi.ttf";
  entry->useCount = 1;
  FFiles["Tahoma"] = entry;

  // Tahoma-bold, not an own font but Adobe Illustrator denotes it so.
  entry = new FontFileEntry;
  entry->entries[false][false] = "Tahomabd.ttf"; entry->entries[false][true] = "Tahomabd.ttf"; 
  entry->entries[true][false] = "Tahomabdi.ttf";  entry->entries[true][true] = "Tahomabdi.ttf";
  entry->useCount = 1;
  FFiles["Tahoma-bold"] = entry;

  // Times New Roman
  entry = new FontFileEntry;
  entry->entries[false][false] = "Times.ttf"; entry->entries[false][true] = "Timesbd.ttf"; 
  entry->entries[true][false] = "Timesi.ttf";  entry->entries[true][true] = "Timesbi.ttf";
  entry->useCount = 2;
  FFiles["Times New Roman"] = entry;
  FFiles["Times"] = entry;

  // Trebuchet MS
  entry = new FontFileEntry;
  entry->entries[false][false] = "Trebuc.ttf"; entry->entries[false][true] = "Trebucbd.ttf"; 
  entry->entries[true][false] = "Trebucit.ttf";  entry->entries[true][true] = "Trebucbi.ttf";
  entry->useCount = 2;
  FFiles["Trebuchet MS"] = entry;
  FFiles["Trebuchet"] = entry;

  // Verdana
  entry = new FontFileEntry;
  entry->entries[false][false] = "Verdana.ttf"; entry->entries[false][true] = "Verdanab.ttf"; 
  entry->entries[true][false] = "Verdanai.ttf";  entry->entries[true][true] = "Verdanaz.ttf";
  entry->useCount = 1;
  FFiles["Verdana"] = entry;
#endif // !USE_FONTCONFIG
}


//----------------------------------------------------------------------------------------------------------------------

/**
 * Destructor of the class. Does some clean up.
 */
CFontManager::~CFontManager(void)
{
  clear();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines platform dependantly the full path of a font file depending on some characteristics.
 *
 * @param family The font family (e.g. Arial, Tahoma, Verdana).
 * @param style The font style (normal, italic).
 * @param weight The "boldness" of the font in the range of [100..900]. Currently values <= 400 are
 *               considered als normal font, everything else as bold.
 *
 * @return The full path and file name of a font that represents the given characteristics.
 *          A kind of intelligent replacement is done here, though. If there is no file with the given characteristics 
 *          (or cannot be found) then the one from the same family, but normal styles, is used instead. If there is no 
 *          entry for the family or even the standard style for a family cannot be found then Arial Standard is returned as 
 *          default. If this files is also not present on the system then the FTGL lib will throw an exception.
 * @note The returned file name is ANSI encoded as FTGL expects it so.
 */
string CFontManager::getFontFile(string family, const string& style, int weight)
{
  string filename;

#ifdef USE_FONTCONFIG
  static std::map<std::string,std::string> fileCache;
  typedef struct {
    char *family;
    char *aliases[10];
  } FamilyAlias;
  static FamilyAlias fontFamilyAliases[]= {
    {"Tahoma", {"Bitstream Vera Sans", "Bitstream Vera", "Nimbus Sans L", "Arial", NULL}},
    {"Tahoma-Bold", {"hlv", "Bitstream Vera Sans", "Bitstream Vera", "Nimbus Sans L", "Arial", NULL}},
    {NULL, {NULL}}
  };

  FcResult result;
  int slant;
  int f, i;
  const char *tryFamily= family.c_str();
  
  if (style == "italic")
    slant= FC_SLANT_ITALIC;
  else if (style == "oblique")
    slant= FC_SLANT_OBLIQUE;
  else
    slant= FC_SLANT_ROMAN;

  for (f= 0; fontFamilyAliases[f].family!=NULL; f++)
    if (strcasecmp(fontFamilyAliases[f].family, family.c_str())==0)
  	break;
  if (fontFamilyAliases[f].family == NULL) f= -1;

  i= -1;
  do 
  {
    FcPattern *pattern= FcPatternCreate();
    FcPatternAddString(pattern, FC_FAMILY, (FcChar8*)tryFamily);
    FcPatternAddInteger(pattern, FC_SLANT, slant);
    FcPatternAddInteger(pattern, FC_WEIGHT, weight);

    if (!FcConfigSubstitute(FcConfigGetCurrent(), pattern, FcMatchPattern))
    {
      g_warning("Error calling FcConfigSubstitute() on font '%s %s'",
                family.c_str(), style.c_str());
      FcPatternDestroy(pattern);
    }
    else
    {
      FcPattern *fpat;

      FcDefaultSubstitute(pattern);
  
      fpat= FcFontMatch(FcConfigGetCurrent(), pattern, &result);
      if (fpat)
      {
	FcChar8 *tmp= NULL;
        if (FcPatternGetString(fpat, FC_FILE, 0, (FcChar8**)&tmp) == FcResultMatch)
        {
          filename= (char*)tmp;
        }
        FcPatternDestroy(fpat);
      }
      FcPatternDestroy(pattern);
    }
  } while (filename.empty() && f >= 0 && (tryFamily= fontFamilyAliases[f].aliases[++i])!=NULL);
#elif defined(__APPLE__)
  FSSpec pathSpec;
  CFStringRef name;
  ATSFontRef match= 0;
  bool wants_bold= weight > 400;
  bool wants_italic= style == "italic" || style == "oblique";
  string fullName;
  
  fullName= family + (wants_bold ? " bold" : "");
  if (wants_italic)
    fullName= fullName+" "+style;
  fullName= family;
  name= CFStringCreateWithCString(NULL, fullName.c_str(), kCFStringEncodingUTF8);
  match= ATSFontFindFromName(name, 0);
  CFRelease(name);
  
  if (match)
  {
    FSRef fsref;
    ATSFontGetFileSpecification(match, &pathSpec);
    if (FSpMakeFSRef(&pathSpec, &fsref) == 0)
    {  
      UInt8 thePath[1024];
      if (FSRefMakePath(&fsref, thePath, sizeof(thePath)) == 0)
      {
        filename= (char*)thePath;
      }
    }
  }  
//  if (filename.empty() && family != "Arial")
//    filename= getFontFile("Arial", "", 0);
  if (filename.empty())
  {
    if (FFontResourcesPath.empty())
      filename="/Library/Fonts/Vera.ttf";
    else
      filename=FFontResourcesPath+"/Vera.ttf";
  }
#else // !USE_FONTCONFIG
  FontFileEntry* entry;

  // Adobe Illustrator (and probably others too) surround the family name with quotes.
  if (family[0] == '"' || family[0] == '\'')
    family = family.substr(1, family.size() - 2);
  FontFiles::iterator iterator = FFiles.find(family);
  if (iterator == FFiles.end())
  {
    // Family is unknown, use Arial as default.
    entry = FFiles["Arial"];
  }
  else
  {
    entry = iterator->second;
  };

  bool italic = (style == "italic") || (style == "oblique");
  bool bold = weight > 400;

  LPITEMIDLIST pidl;
  wchar_t Path[MAX_PATH];
  if (SHGetSpecialFolderLocation(0, CSIDL_FONTS, &pidl) == NOERROR)
  {
    if (SHGetPathFromIDList(pidl, Path))
      filename = utf16ToANSI(Path);
    
    // We are required to free the returned pidl via the shell's IMalloc interface.
    LPMALLOC pMalloc;
    SHGetMalloc(&pMalloc);
    pMalloc->Free(pidl);
  }
  string fontFile = entry->entries[italic][bold];
  string testFile = filename + '/' + fontFile;

  FILE* file = fopen(testFile.c_str(), "rb");
  if (file != NULL)
  {
    fclose(file);
    filename = testFile;
  }
  else
  {
    // There is no file for the required style. Try the base entry.
    fontFile = entry->entries[false][false];
    testFile = filename + '/' + fontFile;
    file = fopen(testFile.c_str(), "rb");
    if (file != NULL)
    {
      fclose(file);
      filename = testFile;
    }
    else
    {
      // That's bad luck. No standard file either. Use the default font.
      entry = FFiles["Arial"];
      filename += '/' + entry->entries[false][false];
    };
  };
#endif // !USE_FONTCONFIG
  return filename;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the ascender of the given font, that is, the units above the base line of the font.
 *
 * @param fontId The identifier of a font.
 * @result The ascender of the given font or 0 if the font does not exist.
 */
float CFontManager::ascender(const string& fontId)
{
  Fonts::iterator iterator = FFonts.find(fontId);
  if (iterator != FFonts.end())
  {
    FTFont* font = iterator->second;
    return font->Ascender();
  }
  else
    return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines the bounding box that entirely encloses the given string.
 *
 * @param output The string, whose bounds are to be determined.
 * @param fontId The identifier of a font. Use fontIdeCreate to create an ID.
 * @param box [out] A structure that takes the retrieved coordinates. It's content is undefined if
 *                  the given font ID does not exist.
 */
void CFontManager::boundingBox(const wstring& output, const string& fontId, TBoundingBox& box)
{
  Fonts::iterator iterator = FFonts.find(fontId);
  if (iterator != FFonts.end())
  {
    FTFont* font = iterator->second;
    font->BBox(output.c_str(), box.upper.x, box.upper.y, box.upper.z, box.lower.x, box.lower.y, box.lower.z);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Clears the font file list.
 */
void CFontManager::clear(void)
{
  for (Fonts::iterator iterator = FFonts.begin(); iterator != FFonts.end(); ++iterator)
  {
    FTFont* Font = iterator->second;
    delete Font;
  };
  FFonts.clear();

#ifndef USE_FONTCONFIG
  for (FontFiles::iterator iterator = FFiles.begin(); iterator != FFiles.end(); iterator++)
  {
    FontFileEntry* entry = iterator->second;
    if (--entry->useCount == 0)
      delete entry;
  };
  FFiles.clear();
#endif
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the descender of the given font, that is, the units below the base line of the font.
 *
 * @param fontId The identifier of a font.
 * @result The descender of the given font or 0 if the font does not exist.
 */
float CFontManager::descender(const string& fontId)
{
  Fonts::iterator iterator = FFonts.find(fontId);
  if (iterator != FFonts.end())
  {
    FTFont* font = iterator->second;
    return font->Descender();
  }
  else
    return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a key based on the given values that uniquely identifies a font along with its properties (weight, size).
 * The key is then used to look up the internal font list to check if already a font object exists for it.
 * If not then the font class is created.
 *
 * @param family The font family (Arial, Courier etc.)
 * @param style The font style.
 * @param weight The boldness of the font (400 for normal).
 * @param FontSize The font size.
 * @result The font identifier that corresponds to the given properties.
 */
string CFontManager::fontIdCreate(const string& family, const string& style, int weight, int fontSize)
{
  char extra[30];

  // Construct a lookup string out of the font properties.
  #ifdef _WINDOWS
    _snprintf(extra, sizeof(extra), "%i:%i\0", weight, fontSize);
  #else
    snprintf(extra, sizeof(extra), "%i:%i", weight, fontSize);
  #endif // #ifdef _WINDOWS

  string fontId = family + style + extra;

  FTFont* font= 0;
  Fonts::iterator iterator = FFonts.find(fontId);
  if (iterator == FFonts.end())
  {
    string fontFile = getFontFile(family, style, weight);
    font = new FONTCLASS(fontFile.c_str());
    font->FaceSize(fontSize);
    font->UseDisplayList(FUseLocalDisplayLists);
    FFonts[fontId] = font;
  };

  return fontId;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the line height of the given font.
 *
 * @param fontId The identifier of an existing font.
 * @result The line height or 0 if the font ID does not exist.
 */
float CFontManager::lineHeight(const string& fontId)
{
  Fonts::iterator iterator = FFonts.find(fontId);
  if (iterator != FFonts.end())
  {
    FTFont* font = iterator->second;
    return font->LineHeight();
  }
  else
    return 0;
}

//----------------------------------------------------------------------------------------------------------------------

void CFontManager::setFontResourcesPath(const string& path)
{
  FFontResourcesPath= path;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates the necessary OpenGL commands to display the given text.
 *
 * @param output The text to display.
 * @param fontId The ID of a font previously created with fontIdCreate. This ID must exist or no commands are issued.
 */
void CFontManager::textOut(const wstring& output, const string& fontId)
{
  Fonts::iterator iterator = FFonts.find(fontId); 
  if (iterator != FFonts.end())
  {
    FTFont* font = iterator->second;
    font->Render(output.c_str());
  };
}

//----------------------------------------------------------------------------------------------------------------------

