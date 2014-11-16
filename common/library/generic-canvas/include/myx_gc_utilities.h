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
 * @file myx_gc_utilities.h 
 * @brief Some common utility functions.
 * 
 */

#ifndef __GC_GL_UTILITIES_H__
#define __GC_GL_UTILITIES_H__

#include <png.h>
#include <sstream>

#include "myx_gc_datatypes.h"

//----------------------------------------------------------------------------------------------------------------------

typedef enum tagColorType
{
  COLOR_TYPE_PALETTE =     PNG_COLOR_TYPE_PALETTE,
  COLOR_TYPE_GRAY =        PNG_COLOR_TYPE_GRAY,
  COLOR_TYPE_GRAY_ALPHA =  PNG_COLOR_TYPE_GRAY_ALPHA,
  COLOR_TYPE_RGB =         PNG_COLOR_TYPE_RGB,
  COLOR_TYPE_RGB_ALPHA =   PNG_COLOR_TYPE_RGB_ALPHA
} TColorType;

typedef struct tagImage
{
  int width;              // The width of the image in pixels.
  int height;             // The height of the image in pixels.
  unsigned char* data;   // The image data.
  TColorType colorType;   // The color format of the image (RGB, gray, palette etc.)
                          // Note: Palette images are not supported.
  unsigned int channels; // Bytes per pixel.
  GLenum format;          // OpenGL color format specifier. Set by the image user.
} TImage;


//----------------------------------------------------------------------------------------------------------------------

/** Converts the given string, which is supposed to be an UTF-8 encoded text into an UTF-16 string. */
GENERIC_CANVAS_API wstring utf8ToUtf16(const string& Source);

/** Converts the given UTF-16 string into an UTF-8 string. */
GENERIC_CANVAS_API string utf16ToUtf8(const wstring& Source);

/** Converts the given string into an ANSI string using the current system locale. */
GENERIC_CANVAS_API string utf16ToANSI(const wstring& Source);

/** Converts the given string, which is supposed to be an UTF-8 encoded text into an ANSI string using the current system locale. */
GENERIC_CANVAS_API string utf8ToANSI(const string& Source);

/** Saves the given image as a PNG image. */
GENERIC_CANVAS_API bool savePNG(const wstring& filename, TImage *image, bool flipped, const char* titel, const char* software);

/** Loads the given PNG image from disk. */
GENERIC_CANVAS_API TImage* loadPNG(const wstring& filename);

/** Releases the given image. */
GENERIC_CANVAS_API void freeImage(TImage* image);

/** Returns the current working folder. */
GENERIC_CANVAS_API string getCurrentDir(void);

/** Sets the current working folder. */
GENERIC_CANVAS_API void setCurrentDir(const string& folder);

/** Extracts the drive and path from the given file name. */
GENERIC_CANVAS_API string extractFilePath(const string& filename);

/** Platform neutral file open function. */
FILE* openFile(string filename, const char* openMode);
FILE* openFile(wstring filename, const wchar_t* openMode);

/** Read a text file into memory and perform $variable$ substitutions */
char *preprocessFile(const string &filename, map<wstring, wstring>& variables, size_t &size);

/** Sorts the given bounds so that left <= right and bottom <= top. */
TBoundingBox sortBounds(const TBoundingBox& bounds);

/** Determines whether both bounds overlap. */
bool boundsIntersect(const TBoundingBox& bounds1, const TBoundingBox& bounds2);

/** Determines whether both the second bounds are within the first bounds. */
bool boundsContainBounds(const TBoundingBox& bounds1, const TBoundingBox& bounds2);

/** Determines whether bounds are empty. */
bool boundsAreEmpty(const TBoundingBox& bounds);

/** Checks if a given point is within the given bounds. */
bool boundsContainPoint(const TBoundingBox& bounds, const float x, const float y);

/** Converts a container name to an identifier suitable for quick lookup. */
GENERIC_CANVAS_API TContainerID getContainerID(const string& container);

/** Returns an identifier for a given property name. */
GENERIC_CANVAS_API TPropertyID getPropertyID(const string& property);

/** Returns the index value for a given property. */
int getEntryIndex(string& Path);

/** Converts a (byte) color to a string. */
string colorToString(GLubyte* color);

/** Converts a (float) color to a string. */
string colorToString(GLfloat* color);

// Adds colors to the named color table.
void registerSystemColors(const CColorMap& colorMap);

// Find a color by name.
bool colorByName(string name, GLubyte* color);

/** Converts a string to color with byte members. */
int stringToColor(string ColorString, GLubyte* color);

/** Converts a string to color with float members. */
int stringToColor(string ColorString, GLfloat* color);

/** matrix code */
void matrixMultiply(TMatrix product, const TMatrix a, const TMatrix b);
void matrixRotate(TMatrix mat, GLfloat angle, GLfloat x, GLfloat y, GLfloat z);
void matrixScale(TMatrix mat, GLfloat x, GLfloat y, GLfloat z);
void matrixTranslate(TMatrix mat, GLfloat x, GLfloat y, GLfloat z);
TVertex matrixTransform(TMatrix m, const TVertex& v);
bool matrixInvert(const TMatrix& input, TMatrix& output);

/** Shadow creation */
void averageMeanBlur(unsigned char* values, int width, int height, int size = 3);

//----------------------------------------------------------------------------------------------------------------------

/**
 * The bounding box computer is a neat little helper class to construct a final bound box out of an arbitrary number of
 * other boxes as well as (lists of) points and vertices.
 */
class CBoundingBoxComputer
{
private:
  TBoundingBox FCurrentBox;
  bool FEmpty;
protected:
  void includeVertex(const TVertex& vertex);
public:
  CBoundingBoxComputer(void);
  CBoundingBoxComputer(const TBoundingBox& initialBox);

  TBoundingBox boundingBox(void);
  void include(TMatrix matrix, const TBoundingBox& newBox);
  void include(const TVertex& offset, const TBoundingBox& newBox);
  void include(TMatrix matrix, const TVertex& vertex);
  void include(TMatrix matrix, const float& x, const float& y);
  void reset(void) { FEmpty = true; };
};
 
//----------------------------------------------------------------------------------------------------------------------

/**
 * Simple tokenizer class that works similar as Java's StringTokenizer.
 */  
class StringTokenizer
{
  wstring FInput, FDelimiters;
  wstring::size_type FPosition;
  wchar_t FLastDelimiter;
  bool FSkipDelimiterBlock;
public:
  StringTokenizer(wstring text, wstring delimiters, bool skipDelimiterBlock): FInput(text), FDelimiters(delimiters),
    FSkipDelimiterBlock(skipDelimiterBlock)
  {
    FPosition = FInput.find_first_not_of(FDelimiters);
    FLastDelimiter = '\0';
  };

  StringTokenizer(string text, string delimiters, bool skipDelimiterBlock): FSkipDelimiterBlock(skipDelimiterBlock)
  {
    FInput = utf8ToUtf16(text);
    FDelimiters = utf8ToUtf16(delimiters);
    FPosition = FInput.find_first_not_of(FDelimiters);
    FLastDelimiter = '\0';
  };
  
  bool hasMoreTokens()
  {
    return FPosition != wstring::npos;
  };
  
  wchar_t lastDelimiter(void) { return FLastDelimiter; };

  wstring nextToken()
  {
    wstring::size_type endPosition = FInput.find_first_of(FDelimiters, FPosition);
    if (endPosition == wstring::npos)
      FLastDelimiter = '\0';
    else
      FLastDelimiter = FInput[endPosition];
    wstring token = FInput.substr(FPosition, endPosition - FPosition);
    FPosition = endPosition;
    if (FPosition != wstring::npos)
    {
      ++FPosition;
      if (FSkipDelimiterBlock)
        FPosition = FInput.find_first_not_of(FDelimiters, FPosition);
    };
    return token;
  };

  float nextTokenAsFloat()
  {
    float result;
    swscanf(nextToken().c_str(), L"%g", &result);

    return result;
  };

  int nextTokenAsInt()
  {
    int result;
    swscanf(nextToken().c_str(), L"%i", &result);

    return result;
  };
};

//----------------------------------------------------------------------------------------------------------------------

#endif // __GC_GL_UTILITIES_H__
