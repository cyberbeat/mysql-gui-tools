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
 * @file myx_gc_utilities.cpp
 * @brief Some common utility functions.
 * 
 */

#include "myx_gc_utilities.h"

#if !defined(__WIN__) && !defined(_WIN32) 
#include <unistd.h>
#endif

//----------------------------------------------------------------------------------------------------------------------

typedef struct tagColorEntry
{
  char* name;
  GLubyte color[4];
} TColorEntry;

static TColorEntry colors[] =
{
  {"aliceblue",            {240, 248, 255, 255}},
  {"antiquewhite",         {250, 235, 215, 255}},
  {"aqua",                 {  0, 255, 255, 255}},
  {"aquamarine",           {127, 255, 212, 255}},
  {"azure",                {240, 255, 255, 255}},
  {"beige",                {245, 245, 220, 255}},
  {"bisque",               {255, 228, 196, 255}},
  {"black",                {  0,   0,   0, 255}},
  {"blanchedalmond",       {255, 235, 205, 255}},
  {"blue",                 {  0,   0, 255, 255}},
  {"blueviolet",           {138,  43, 226, 255}},
  {"brown",                {165,  42,  42, 255}},
  {"burlywood",            {222, 184, 135, 255}},
  {"cadetblue",            { 95, 158, 160, 255}},
  {"chartreuse",           {127, 255,   0, 255}},
  {"chocolate",            {210, 105,  30, 255}},
  {"coral",                {255, 127,  80, 255}},
  {"cornflowerblue",       {100, 149, 237, 255}},
  {"cornsilk",             {255, 248, 220, 255}},
  {"crimson",              {220,  20,  60, 255}},
  {"cyan",                 {  0, 255, 255, 255}},
  {"darkblue",             {  0,   0, 139, 255}},
  {"darkcyan",             {  0, 139, 139, 255}},
  {"darkgoldenrod",        {184, 134,  11, 255}},
  {"darkgray",             {169, 169, 169, 255}},
  {"darkgreen",            {  0, 100,   0, 255}},
  {"darkgrey",             {169, 169, 169, 255}},
  {"darkkhaki",            {189, 183, 107, 255}},
  {"darkmagenta",          {139,   0, 139, 255}},
  {"darkolivegreen",       { 85, 107,  47, 255}},
  {"darkorange",           {255, 140,   0, 255}},
  {"darkorchid",           {153,  50, 204, 255}},
  {"darkred",              {139,   0,   0, 255}},
  {"darksalmon",           {233, 150, 122, 255}},
  {"darkseagreen",         {143, 188, 143, 255}},
  {"darkslateblue",        { 72,  61, 139, 255}},
  {"darkslategray",        { 47,  79,  79, 255}},
  {"darkslategrey",        { 47,  79,  79, 255}},
  {"darkturquoise",        {  0, 206, 209, 255}},
  {"darkviolet",           {148,   0, 211, 255}},
  {"deeppink",             {255,  20, 147, 255}},
  {"deepskyblue",          {  0, 191, 255, 255}},
  {"dimgray",              {105, 105, 105, 255}},
  {"dimgrey",              {105, 105, 105, 255}},
  {"dodgerblue",           { 30, 144, 255, 255}},
  {"firebrick",            {178,  34,  34, 255}},
  {"floralwhite",          {255, 250, 240, 255}},
  {"forestgreen",          { 34, 139,  34, 255}},
  {"fuchsia",              {255,   0, 255, 255}},
  {"gainsboro",            {220, 220, 220, 255}},
  {"ghostwhite",           {248, 248, 255, 255}},
  {"gold",                 {255, 215,   0, 255}},
  {"goldenrod",            {218, 165,  32, 255}},
  {"gray",                 {128, 128, 128, 255}},
  {"grey",                 {128, 128, 128, 255}},
  {"green",                {  0, 128,   0, 255}},
  {"greenyellow",          {173, 255,  47, 255}},
  {"honeydew",             {240, 255, 240, 255}},
  {"hotpink",              {255, 105, 180, 255}},
  {"indianred",            {205,  92,  92, 255}},
  {"indigo",               { 75,   0, 130, 255}},
  {"ivory",                {255, 255, 240, 255}},
  {"khaki",                {240, 230, 140, 255}},
  {"lavender",             {230, 230, 250, 255}},
  {"lavenderblush",        {255, 240, 245, 255}},
  {"lawngreen",            {124, 252,   0, 255}},
  {"lemonchiffon",         {255, 250, 205, 255}},
  {"lightblue",            {173, 216, 230, 255}},
  {"lightcoral",           {240, 128, 128, 255}},
  {"lightcyan",            {224, 255, 255, 255}},
  {"lightgoldenrodyellow", {250, 250, 210, 255}},
  {"lightgray",            {211, 211, 211, 255}},
  {"lightgreen",           {144, 238, 144, 255}},
  {"lightgrey",            {211, 211, 211, 255}},
  {"lightpink",            {255, 182, 193, 255}},
  {"lightsalmon",          {255, 160, 122, 255}},
  {"lightseagreen",        { 32, 178, 170, 255}},
  {"lightskyblue",         {135, 206, 250, 255}},
  {"lightslategray",       {119, 136, 153, 255}},
  {"lightslategrey",       {119, 136, 153, 255}},
  {"lightsteelblue",       {176, 196, 222, 255}},
  {"lightyellow",          {255, 255, 224, 255}},
  {"lime",                 {  0, 255,   0, 255}},
  {"limegreen",            { 50, 205,  50, 255}},
  {"linen",                {250, 240, 230, 255}},
  {"magenta",              {255,   0, 255, 255}},
  {"maroon",               {128,   0,   0, 255}},
  {"mediumaquamarine",     {102, 205, 170, 255}},
  {"mediumblue",           {  0,   0, 205, 255}},
  {"mediumorchid",         {186,  85, 211, 255}},
  {"mediumpurple",         {147, 112, 219, 255}},
  {"mediumseagreen",       { 60, 179, 113, 255}},
  {"mediumslateblue",      {123, 104, 238, 255}},
  {"mediumspringgreen",    {  0, 250, 154, 255}},
  {"mediumturquoise",      { 72, 209, 204, 255}},
  {"mediumvioletred",      {199,  21, 133, 255}},
  {"midnightblue",         { 25,  25, 112, 255}},
  {"mintcream",            {245, 255, 250, 255}},
  {"mistyrose",            {255, 228, 225, 255}},
  {"moccasin",             {255, 228, 181, 255}},
  {"navajowhite",          {255, 222, 173, 255}},
  {"navy",                 {  0,   0, 128, 255}},
  {"oldlace",              {253, 245, 230, 255}},
  {"olive",                {128, 128,   0, 255}},
  {"olivedrab",            {107, 142,  35, 255}},
  {"orange",               {255, 165,   0, 255}},
  {"orangered",            {255,  69  , 0, 255}},
  {"orchid",               {218, 112, 214, 255}},
  {"palegoldenrod",        {238, 232, 170, 255}},
  {"palegreen",            {152, 251, 152, 255}},
  {"paleturquoise",        {175, 238, 238, 255}},
  {"palevioletred",        {219, 112, 147, 255}},
  {"papayawhip",           {255, 239, 213, 255}},
  {"peachpuff",            {255, 218, 185, 255}},
  {"peru",                 {205, 133,  63, 255}},
  {"pink",                 {255, 192, 203, 255}},
  {"plum",                 {221, 160, 221, 255}},
  {"powderblue",           {176, 224, 230, 255}},
  {"purple",               {128,   0, 128, 255}},
  {"red",                  {255,   0,   0, 255}},
  {"rosybrown",            {188, 143, 143, 255}},
  {"royalblue",            { 65, 105, 225, 255}},
  {"saddlebrown",          {139,  69,  19, 255}},
  {"salmon",               {250, 128, 114, 255}},
  {"sandybrown",           {244, 164,  96, 255}},
  {"seagreen",             { 46, 139,  87, 255}},
  {"seashell",             {255, 245, 238, 255}},
  {"sienna",               {160,  82,  45, 255}},
  {"silver",               {192, 192, 192, 255}},
  {"skyblue",              {135, 206, 235, 255}},
  {"slateblue",            {106,  90, 205, 255}},
  {"slategray",            {112, 128, 144, 255}},
  {"slategrey",            {112, 128, 144, 255}},
  {"snow",                 {255, 250, 250, 255}},
  {"springgreen",          {  0, 255, 127, 255}},
  {"steelblue",            { 70, 130, 180, 255}},
  {"tan",                  {210, 180, 140, 255}},
  {"teal",                 {  0, 128, 128, 255}},
  {"thistle",              {216, 191, 216, 255}},
  {"tomato",               {255,  99,  71, 255}},
  {"turquoise",            { 64, 224, 208, 255}},
  {"violet",               {238, 130, 238, 255}},
  {"wheat",                {245, 222, 179, 255}},
  {"white",                {255, 255, 255, 255}},
  {"whitesmoke",           {245, 245, 245, 255}},
  {"yellow",               {255, 255,   0, 255}},
  {"yellowgreen",          {154, 205,  50, 255}}
};

#define COLOR_COUNT (sizeof(colors) / sizeof(colors[0]))

typedef struct tagSystemColorEntry
{
  char* name;
  int value;
  GLubyte color[4];
} TSystemColorEntry;

#ifdef _WINDOWS
static TSystemColorEntry systemColors[] =
{
  {"ActiveBorder",        COLOR_ACTIVEBORDER},        // Active window border. 
  {"ActiveCaption",       COLOR_ACTIVECAPTION},       // Active window caption. 
  {"AppWorkspace",        COLOR_APPWORKSPACE},        // Background color of multiple document interface. 
  {"Background",          COLOR_BACKGROUND},          // Desktop background. 
  {"ButtonFace",          COLOR_BTNFACE},             // Face color for three-dimensional display elements. 
  {"ButtonHighlight",     COLOR_BTNHIGHLIGHT},        // Dark shadow for three-dimensional display elements (for edges facing away from the light source). 
  {"ButtonShadow",        COLOR_BTNSHADOW},           // Shadow color for three-dimensional display elements. 
  {"ButtonText",          COLOR_BTNTEXT},             // Text on push buttons. 
  {"CaptionText",         COLOR_CAPTIONTEXT},         // Text in caption, size box, and scrollbar arrow box. 
  {"GrayText",            COLOR_GRAYTEXT},            // Grayed (disabled) text. This color is set to #000 if the current display driver does not support a solid gray color. 
  {"Highlight",           COLOR_HIGHLIGHT},           // Item(s) selected in a control. 
  {"HighlightText",       COLOR_HIGHLIGHTTEXT},       // Text of item(s) selected in a control. 
  {"Hotlight",            COLOR_HOTLIGHT},            // Hot tracked item.
  {"InactiveBorder",      COLOR_INACTIVEBORDER},      // Inactive window border. 
  {"InactiveCaption",     COLOR_INACTIVECAPTION},     // Inactive window caption. 
  {"InactiveCaptionText", COLOR_INACTIVECAPTIONTEXT}, // color of text in an inactive caption. 
  {"InfoBackground",      COLOR_INFOBK},              // Background color for tooltip controls. 
  {"InfoText",            COLOR_INFOTEXT},            // Text color for tooltip controls. 
  {"Menu",                COLOR_MENU},                // Menu background. 
  {"MenuText",            COLOR_MENUTEXT},            // Text in menus. 
  {"Scrollbar",           COLOR_SCROLLBAR},           // Scroll bar gray area. 
  {"ThreeDDarkShadow",    COLOR_3DDKSHADOW},          // Dark shadow for three-dimensional display elements. 
  {"ThreeDFace",          COLOR_3DFACE},              // Face color for three-dimensional display elements. 
  {"ThreeDHighlight",     COLOR_3DHIGHLIGHT},         // Highlight color for three-dimensional display elements. 
  {"ThreeDLightShadow",   COLOR_3DLIGHT},             // Light color for three-dimensional display elements (for edges facing the light source). 
  {"ThreeDShadow",        COLOR_3DSHADOW},            // Dark shadow for three-dimensional display elements. 
  {"Window",              COLOR_WINDOW},              // Window background. 
  {"WindowFrame",         COLOR_WINDOWFRAME},         // Window frame.
  {"WindowText",          COLOR_WINDOWTEXT}           // Text in windows.
};

#define SYS_COLOR_COUNT (sizeof(systemColors) / sizeof(systemColors[0]))
#endif // _WINDOWS

#define EPSILON 1E-6         // The distance two float values can have at most and are still considered as being the same.
#define DEG2RAD M_PI / 180   // Constant for angle conversion from radians to degrees.

extern const TMatrix identity = 
{
  1, 0, 0, 0,
  0, 1, 0, 0,
  0, 0, 1, 0,
  0, 0, 0, 1
};

// Default values for text.
extern const string defaultFontFamily("Arial");
extern const string defaultFontStyle("normal");
extern const string defaultFontWeight("400");      // Must be a string as we get it from an attribute that can contain strings.
extern const int defaultFontSize = 20;             // 20 px

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given string, which is supposed to be an UTF-8 encoded text into an UTF-16 string.
 *
 * @param source Contains the source string encoded in UTF-8.
 * @return The converted string in UTF-16 encoding.
 */
wstring utf8ToUtf16(const string& source)
{
  wstring result;

#ifdef _WINDOWS
  const char* raw = source.c_str();
  int size = MultiByteToWideChar(CP_UTF8, 0, raw, -1, NULL, 0);
  wchar_t* buffer = new wchar_t[size];
  MultiByteToWideChar(CP_UTF8, 0, raw, -1, buffer, size);
  result = buffer;
  delete[] buffer;
#else
  wchar_t *tmp= (wchar_t*)g_malloc(sizeof(wchar_t)*(source.length()+1));
  size_t size;
  size= mbstowcs(tmp, source.data(), source.length());
  result= wstring(tmp, size);
  g_free(tmp);
#endif

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given UTF-16 string into an UTF-8 string.
 *
 * @param source Contains the source string encoded in UTF-16.
 * @return The converted string in UTF-8 encoding.
 */
string utf16ToUtf8(const wstring& source)
{
  string result;

#ifdef _WINDOWS
  const wchar_t* raw = source.c_str();
  int size = WideCharToMultiByte(CP_UTF8, 0, raw, -1, NULL, 0, NULL, NULL);
  char* buffer = new char[size];
  WideCharToMultiByte(CP_UTF8, 0, raw, -1, buffer, size, NULL, NULL);
  result = buffer;
  delete[] buffer;
#else
  gchar *tmp= (gchar*)g_malloc(sizeof(char*)*(source.length()+1));
  size_t size;
  size= wcstombs(tmp, source.data(), source.length());
  result= string(tmp, size);
  g_free(tmp);
#endif

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief Converts the given string into an ANSI string.
 * @param source Contains the source string encoded in UTF-16.
 * @return The converted string in ANSI encoding.
 * @note The current user locale is used to convert the Unicode string to ANSI.
 */
string utf16ToANSI(const wstring& source)
{
  string result;

#ifdef _WINDOWS
  const wchar_t* raw = source.c_str();
  int size = WideCharToMultiByte(CP_ACP, 0, raw, -1, NULL, 0, NULL, NULL);
  char* buffer = new char[size];
  WideCharToMultiByte(CP_ACP, 0, raw, -1, buffer, size, NULL, NULL);
  result = buffer;
  delete[] buffer;
#else
  result= utf16ToUtf8(source);
#endif

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief Converts the given string, which is supposed to be an UTF-8 encoded text into an ANSI string.
 * @param source Contains the source string encoded in UTF-8.
 * @return The converted string in ANSI encoding.
 * @note The current user locale is used to convert the Unicode string to ANSI.
 */
string utf8ToANSI(const string& source)
{
#ifdef _WINDOWS
  return utf16ToANSI(utf8ToUtf16(source));
#else
  return source;
#endif
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * User defined file handling callback for handling file i/o in png lib.
 * We have to make it so because of the FILE allocation and png being in a DLL on Windows.
 */
void pngRead(png_structp png, png_bytep data, png_size_t length)
{
  fread(data, length, 1, (FILE*) png->io_ptr);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * User defined file handling callback for handling file i/o in png lib.
 * We have to make it so because of the FILE allocation and png being in a DLL on Windows.
 */
void pngWrite(png_structp png, png_bytep data, png_size_t length)
{
  fwrite(data, length, 1, (FILE*) png->io_ptr);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * User defined file handling callback for handling file i/o in png lib.
 * We have to make it so because of the FILE allocation and png being in a DLL on Windows.
 */
void pngFlush(png_structp png)
{
  fflush((FILE*) png->io_ptr);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief Loads a png file.
 *
 * @param filename An ANSI encoded file name (can contain path info) to the png file.
 * @return Returns a pointer to a TImage structure containing the image data.
 * @note The return memory must be freed using freeImage().
 */
TImage* loadPNG(const wstring& filename)
{
  TImage *image = NULL;
  FILE* file;
  png_structp png;
  png_infop info;
  png_uint_32 width, height;
  int depth, junk, color_type;
  int offset;

  file = openFile(filename, L"rb");
  try
  {
    if (file) 
    {
      png_byte magic[8];
      if (fread(magic, sizeof(magic), 1, file) != 1)
      {
        fclose(file);
        return NULL;
      };
      if (png_sig_cmp(magic, 0, sizeof(magic)))
      {
        fclose(file);
        return NULL;
      };

      png = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
      if (!png)
      {
        fclose(file);
        return NULL;
      };
      
      info = png_create_info_struct(png);
      if (!info) 
      {
        fclose(file);
        png_destroy_read_struct(&png, NULL, NULL);
        return NULL;
      };

      png_init_io(png, file);
      png_set_read_fn(png, file, pngRead);
      png_set_sig_bytes(png, sizeof(magic));

      png_read_info(png, info);

      png_get_IHDR(png, info, &width, &height, &depth, &color_type,
                  &junk, &junk, &junk);

      /* sanity check */
      if (width < 1 || height < 1) 
      {
        fclose(file);
        png_destroy_read_struct(&png, &info, NULL);
        return NULL;
      }

      image = (TImage*) malloc(sizeof(TImage));
      if (!image)
      {
        fclose(file);
        png_destroy_read_struct(&png, &info, NULL);
        return NULL;
      };

      image->data = (unsigned char*) malloc(width * height * info->channels);
      if (!image->data) 
      {
        fclose(file);
        png_destroy_read_struct(&png, &info, NULL);
        return NULL;
      };

      image->width = width;
      image->height = height;
      image->colorType = (TColorType) info->color_type;
      image->channels = info->channels;

      // normalize to 8bpp with alpha channel
      if (color_type == PNG_COLOR_TYPE_PALETTE && depth <= 8)
        png_set_expand(png);

      if (color_type == PNG_COLOR_TYPE_GRAY && depth <= 8)
        png_set_expand(png);

      if (png_get_valid(png, info, PNG_INFO_tRNS))
        png_set_expand(png);

      if (depth == 16)
        png_set_strip_16(png);

      if (color_type == PNG_COLOR_TYPE_GRAY ||
          color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
        png_set_gray_to_rgb(png);

      /* do the transforms */
      png_read_update_info(png, info);

      offset= 0;
      /* read data */
      for (unsigned int i = 0; i < height; i++)
      {
        png_read_row(png, &image->data[offset], NULL);
        offset += info->channels * width;
      }
      png_read_end(png, info);
      png_destroy_read_struct(&png, &info, NULL);

      fclose(file);
    };
  }
  catch (...)
  {
    if (image)
      freeImage(image);
    image = NULL;
  };

  return image;
}

//----------------------------------------------------------------------------------------------------------------------

static jmp_buf png_error_jmpbuf;

static void png_error_handler(png_structp png, png_const_charp msg)
{
  fprintf(stderr, "png write error: %s\n", msg);
  longjmp(png_error_jmpbuf, 1);
}

// -----------------------------------------------------------------------------

/**
* @brief Saves a png file.
 *
 * @param filename An ANSI encoded file name (can contain path info) to the png file.
 * @param image An image structure containing the actual pixel data.
 * @param flipped If true then the image will be stored bottom-up otherwise top-down.
 * @param title The titel for the document. Must be ANSI encoded for now.
 * @param software A string describing the producer of the document. Must be ANSI encoded for now.
 * @return Returns true if the image could be saved otherwise false.
 */
bool savePNG(const wstring& filename, TImage *image, bool flipped, const char* title, const char* software)
{
  FILE *file;
  png_structp  png;
  png_infop  info;
  png_time  modtime;
  png_text comments[3];
  
  file = openFile(filename, L"wb");
  if (!file)
    return false;
  
  png = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, png_error_handler, NULL);
  if (png)
  {
	  info = png_create_info_struct(png);
	  if (!info)
			png_destroy_write_struct(&png, NULL);
		else
		{
			if (setjmp(png_error_jmpbuf))
			{
			  png_destroy_write_struct(&png, &info);
			}
			else
			{
				png_set_write_fn(png, file, &pngWrite, &pngFlush);
				png_init_io(png, file);
				png_set_IHDR(png, info, image->width, image->height, 8, image->colorType, PNG_INTERLACE_NONE,
				PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
				png_convert_from_time_t(&modtime, time(NULL));
				png_set_tIME(png, info, &modtime);
				png_write_info(png, info);
				png_set_packing(png);

				// A few words to the user.
				comments[0].key = "Software";
				comments[0].text = (char*) software;
				comments[0].compression = PNG_TEXT_COMPRESSION_NONE;
				comments[1].key = "Title";
				comments[1].text = (char*) title;
				comments[1].compression = PNG_TEXT_COMPRESSION_NONE;
				comments[2].key = "Author";
				comments[2].text = "MySQL GUI tools";
				comments[2].compression = PNG_TEXT_COMPRESSION_NONE;
				png_set_text(png, info, comments, 3);

				int bytesPerPixel = 1;
				if (image->colorType == COLOR_TYPE_RGB)
				bytesPerPixel = 3;
				else
				if (image->colorType == COLOR_TYPE_RGB_ALPHA)
					bytesPerPixel = 4;

				unsigned char *data = NULL;
				int offset = 0;
				if (flipped)
				{
				data = image->data + (image->height - 1) * image->width * bytesPerPixel;
				offset = -image->width * bytesPerPixel;
				}
				else
				{
				data = image->data;
				offset = image->width * bytesPerPixel;
				};
				for (int i= 0; i < image->height; i++)
				{
				png_write_row(png, (png_byte*) data);
				data += offset;
				};
				png_write_end(png, info);

				if (png && info)
				png_destroy_write_struct(&png, &info);

				fclose(file);

				return true;
			};
		};
	};
  if (png && info)
	  png_destroy_write_struct(&png, &info);

  fclose(file);

  return false;
}

//----------------------------------------------------------------------------------------------------------------------

void freeImage(TImage* image)
{
  free(image->data);
  free(image);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief Returns the current working folder (ANSI encoded).
 * @return The current working folder.
 */
string getCurrentDir(void)
{
  char buffer[MAX_PATH];

  getcwd(buffer, sizeof(buffer));

  return string(buffer);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief Sets the current working folder (folder name must be ANSI encoded).
 * @param folder The new folder to be set.
 */
void setCurrentDir(const string& folder)
{
  chdir(folder.c_str());
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief ExtractFilePath extracts the drive and directory parts of the given filename. The resulting string is the 
          leftmost characters of FileName, up to and including the colon or backslash that separates the path information 
          from the name and extension. The resulting string is empty if FileName contains no drive and directory parts.
 * @param filename The file name (ANSI encoded) of which the path is to be extracted.
 * @return The extracted path part (ANSI encoded).
 */
string extractFilePath(const string& filename)
{
  gchar* raw = g_path_get_dirname(filename.c_str());

  string result(raw);
  g_free(raw);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Platform neutral file open routine.
 *
 * @param filename The name of file encoded in UTF-8.
 * @param openMode The mode how to open the file (the same as used for fopen calls).
 * @return A pointer to a FILE structure if the file could be opened, NULL otherwise.
 */
FILE* openFile(string filename, const char* openMode)
{
#ifdef _WINDOWS
  wstring name = utf8ToUtf16(filename);
  wstring mode = utf8ToUtf16(string(openMode));
  return _wfopen(name.c_str(), mode.c_str());
#else
  FILE *file;
  char * local_filename;

  if (!(local_filename = g_filename_from_utf8(filename.c_str(), -1, NULL, NULL, NULL)))
    return NULL;

  file = fopen(local_filename, openMode);

  g_free(local_filename);

  return file;
#endif // #ifdef _WINDOWS
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Platform neutral file open routine.
 *
 * @param filename The name of file.
 * @param openMode The mode how to open the file (the same as used for fopen calls).
 * @return A pointer to a FILE structure if the file could be opened, NULL otherwise.
 */
FILE* openFile(wstring filename, const wchar_t* openMode)
{
#ifdef _WINDOWS
  return _wfopen(filename.c_str(), openMode);
#else
  FILE *file;
  char * local_filename;

  if (!(local_filename = g_filename_from_utf8(utf16ToUtf8(filename).c_str(), -1, NULL, NULL, NULL)))
    return NULL;

  file = fopen(local_filename, utf16ToANSI(openMode).c_str());

  g_free(local_filename);

  return file;
#endif // #ifdef _WINDOWS
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Helper method to sort left/right and bottom/top coordinates so that for left/top are always smaller than right/bottom
 * (origin is considered in the left-upper corner, +y pointing down).
 */
TBoundingBox sortBounds(const TBoundingBox& bounds)
{
  TBoundingBox result;

  result.upper.x = (bounds.upper.x <= bounds.lower.x) ? bounds.upper.x : bounds.lower.x;
  result.lower.x = (bounds.upper.x <= bounds.lower.x) ? bounds.lower.x : bounds.upper.x;
  result.upper.y = (bounds.upper.y <= bounds.lower.y) ? bounds.upper.y : bounds.lower.y;
  result.lower.y = (bounds.upper.y <= bounds.lower.y) ? bounds.lower.y : bounds.upper.y;

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Determines whether both bounds overlap. 
 *
 * @param bounds1 One of the bounds to compare.
 * @param bounds2 The other bounds to compare.
 * @return True if both bounds overlap each other, otherwise false.
 * @note Bounds must be sorted.
 */
bool boundsIntersect(const TBoundingBox& bounds1, const TBoundingBox& bounds2)
{
  return (bounds1.lower.x >= bounds2.upper.x) && (bounds1.upper.x <= bounds2.lower.x) && 
    (bounds1.lower.y >= bounds2.upper.y) && (bounds1.upper.y <= bounds2.lower.y);
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Determines whether the second bounds are completely within the first bounds. 
 *
 * @param bounds1 The outer bounds to compare.
 * @param bounds2 The inner bounds to compare.
 * @return True if the second bounds are completely within the first bounds, otherwise false.
 * @note Bounds must be sorted.
 */
bool boundsContainBounds(const TBoundingBox& bounds1, const TBoundingBox& bounds2)
{
  return (bounds1.upper.x <= bounds2.upper.x) && (bounds2.lower.x <= bounds1.lower.x) && 
    (bounds1.upper.y <= bounds2.upper.y) && (bounds2.lower.y <= bounds1.lower.y);
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Determines whether the given bounds include the given point.
 *
 * @param bounds The bounds to check the point against.
 * @param x The horizontal coordinate to check.
 * @param y The vertical coordinate to check.
 * @return True if the point is within the bounds, otherwise false.
 * @note Bounds must be sorted.
 */
bool boundsContainPoint(const TBoundingBox& bounds, const float x, const float y)                  
{
  return (bounds.lower.x >= x) && (bounds.upper.x <= x) && (bounds.lower.y >= y) && (bounds.upper.y <= y);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Examines the given bounds and returns whether it is empty or not.
 *
 * @param bounds The bounds to examine.
 * @return True if the bounds are empty, false otherwise.
 */
bool boundsAreEmpty(const TBoundingBox& bounds)
{
  return (bounds.upper.x == bounds.lower.x) && (bounds.upper.y == bounds.lower.y);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Looks the given container name up and returns an identifier for it that can be used for quick lookup/handling.
 *
 * @param container The name of the container.
 * @return An identifier that specifies which container was given.
 */
TContainerID getContainerID(const string& container)
{
  TContainerID result = GC_CONTAINER_UNKNOWN;

  static map<string, TContainerID> containers;

  // Initialize container mapping if not yet done.
  if (containers.size() == 0)
  {
    containers[""] = GC_CONTAINER_UNKNOWN;
    containers["layers"] = GC_CONTAINER_LAYERS;
    containers["feedback"] = GC_CONTAINER_FEEDBACK;
    containers["views"] = GC_CONTAINER_VIEWS;
    containers["model"] = GC_CONTAINER_MODEL;
    containers["style"] = GC_CONTAINER_STYLE;
    containers["styles"] = GC_CONTAINER_STYLES;
    containers["layouts"] = GC_CONTAINER_LAYOUTS;
    containers["figure"] = GC_CONTAINER_FIGURE;
    containers["figures"] = GC_CONTAINER_FIGURES;
    containers["figureInstances"] = GC_CONTAINER_FIGURE_INSTANCES;
    containers["content"] = GC_CONTAINER_FIGURE_CONTENT;
    containers["scaling"] = GC_CONTAINER_SCALING;
    containers["translation"] = GC_CONTAINER_TRANSLATION;
    containers["rotation"] = GC_CONTAINER_ROTATION;
    containers["groups"] = GC_CONTAINER_GROUPS;
    containers["children"] = GC_CONTAINER_CHILDREN;
    containers["caption"] = GC_CONTAINER_CAPTION;
    containers["content"] = GC_CONTAINER_CONTENT;
    containers["color"] = GC_CONTAINER_COLOR;
    containers["hotcolor"] = GC_CONTAINER_HOT_COLOR;

    containers["connection"] = GC_CONTAINER_CONNECTION;
    containers["end1"] = GC_CONTAINER_END1;
    containers["end1Decoration"] = GC_CONTAINER_END1_DECORATION;
    containers["end1Label"] = GC_CONTAINER_END1_LABEL;
    containers["end2"] = GC_CONTAINER_END2;
    containers["end2Decoration"] = GC_CONTAINER_END2_DECORATION;
    containers["end2Label"] = GC_CONTAINER_END2_LABEL;
    containers["centerDecoration"] = GC_CONTAINER_CENTER_DECORATION;
    containers["centerLabel"] = GC_CONTAINER_CENTER_LABEL;
  };

  if (containers.find(container) != containers.end())
    result = containers[container];

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Looks up the property name and returns an identifier for it.
 *
 * @param property The property name to look up.
 * @return An identifier that specifies which property was given.
 */
TPropertyID getPropertyID(const string& property)
{
  static map<string, TPropertyID> properties;
  TPropertyID result = GC_PROPERTY_UNKNOWN;

  // Fill property mapping first if not yet done.
  if (properties.size() == 0)
  {
    // Simple properties.
    properties["width"] = GC_PROPERTY_WIDTH;
    properties["height"] = GC_PROPERTY_HEIGHT;
    properties["x"] = GC_PROPERTY_X;
    properties["y"] = GC_PROPERTY_Y;
    properties["z"] = GC_PROPERTY_Z;
    properties["name"] = GC_PROPERTY_NAME;
    properties["_id"] = GC_PROPERTY_ID;
    properties["description"] = GC_PROPERTY_DESCRIPTION;
    properties["zoom"] = GC_PROPERTY_ZOOM;
    properties["color"] = GC_PROPERTY_COLOR;
    properties["jitter"] = GC_PROPERTY_JITTER;
    properties["angle"] = GC_PROPERTY_ANGLE;
    properties["visible"] = GC_PROPERTY_VISIBLE;
    properties["enabled"] = GC_PROPERTY_ENABLED;
    properties["selected"] = GC_PROPERTY_SELECTED;
    properties["layout"] = GC_PROPERTY_LAYOUT;
    properties["resizable"] = GC_PROPERTY_RESIZABLE;
    properties["expanded"] = GC_PROPERTY_EXPANDED;
    properties["constraintMinWidth"] = GC_PROPERTY_MIN_WIDTH;
    properties["constraintMinHeight"] = GC_PROPERTY_MIN_HEIGHT;
    properties["constraintMaxWidth"] = GC_PROPERTY_MAX_WIDTH;
    properties["constraintMaxHeight"] = GC_PROPERTY_MAX_HEIGHT;
    properties["text"] = GC_PROPERTY_TEXT;
    properties["fontFamily"] = GC_PROPERTY_FONT_FAMILY;
    properties["fontSize"] = GC_PROPERTY_FONT_SIZE;
    properties["fontWeight"] = GC_PROPERTY_FONT_WEIGHT;
    properties["fontStyle"] = GC_PROPERTY_FONT_STYLE;
    properties["verticalAlignment"] = GC_PROPERTY_ALIGNMENT_VERTICAL;
    properties["horizontalAlignment"] = GC_PROPERTY_ALIGNMENT_HORIZONTAL;
    properties["bidiMode"] = GC_PROPERTY_BIDI_MODE;
    properties["wrap"] = GC_PROPERTY_WRAP_TEXT;
    properties["center"] = GC_PROPERTY_CENTER;
    properties["owner"] = GC_PROPERTY_OWNER;
    properties["label1OffsetX"] = GC_PROPERTY_LABEL1_OFFSET_X;
    properties["label1OffsetY"] = GC_PROPERTY_LABEL1_OFFSET_Y;
    properties["label2OffsetX"] = GC_PROPERTY_LABEL2_OFFSET_X;
    properties["label2OffsetY"] = GC_PROPERTY_LABEL2_OFFSET_Y;
    properties["labelCenterOffsetX"] = GC_PROPERTY_LABEL_CENTER_OFFSET_X;
    properties["labelCenterOffsetY"] = GC_PROPERTY_LABEL_CENTER_OFFSET_Y;
  };

  if (properties.find(property) != properties.end())
    result = properties[property];

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Treats the given path as property name preceeded with a slash. The first (or only) subpath must be an integer number
 * denoting an index in a list.
 *
 * @param path [in, out] The path to examine. It must start with a slash to be valid. On return it contains a new 
 *                       path without the index part (can be empty then).
 * @return The integer value extracted from the top path part.
 */
int getEntryIndex(string& path)
{
  int result = -1;

  if (path.size() > 0 && path[0] == '/')
  {
    // Is there another path separator after the initial one?
    string::size_type Position = path.find_first_of('/', 1);
    string Index;
    if (Position != path.npos)
    {
      Index = path.substr(1, Position - 1);
      path = path.substr(Position);
    }
    else
    {
      Index = path.substr(1);
      path = "";
    };
    result = atoi(Index.c_str());
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

static CColorMap predefinedColors;

/**
 * Converts a color to a string in the form #RRGGBB.
 *
 * @param color The color to convert.
 * @return The given color as HTML string.
 */
string colorToString(GLfloat* color)
{
  char buffer[10];

  #ifdef _WINDOWS
    // Microsoft has confirmed that there is no guaranteed NULL termination (see MSDN for details).
    // Hence we better add one.
    _snprintf(buffer, sizeof(buffer), "#%.2X%.2X%.2X\0", ROUND(color[0] * 255), ROUND(color[1] * 255), ROUND(color[2] * 255));
  #else
    snprintf(buffer, sizeof(buffer), "#%.2X%.2X%.2X", ROUND(color[0] * 255), ROUND(color[1] * 255), ROUND(color[2] * 255));
  #endif // #ifdef _WINDOWS

  return string(buffer);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts a color to a string in the form #RRGGBB.
 *
 * @param color The color to convert.
 * @return The given color as HTML string.
 */
string colorToString(GLubyte* color)
{
  char buffer[10];

  #ifdef _WINDOWS
    // Microsoft has confirmed that there is no guaranteed NULL termination (see MSDN for details).
    // Hence we better add one.
    _snprintf(buffer, sizeof(buffer), "#%.2X%.2X%.2X\0", color[0], color[1], color[2]);
  #else
    snprintf(buffer, sizeof(buffer), "#%.2X%.2X%.2X", color[0], color[1], color[2]);
  #endif // #ifdef _WINDOWS

  return string(buffer);
}

//----------------------------------------------------------------------------------------------------------------------

// Converts the two hex digits given by upper and lower to an unsigned byte.
unsigned char hexToByte(char upper, char lower)
{
  upper -= '0';
  if (upper > 9)
    upper -= 7;
  lower -= '0';
  if (lower > 9)
    lower -= 7;
  return upper * 16 + lower;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Searchs the predifined colors and tries to find one with the given name.
 *
 * @param name The name of the color to find.
 * @param color [out] The color data if it could be found.
 * @returns true if the color could be found, otherwise false;
 */
bool colorByName(string name, GLubyte* color)
{

  // Fill the color map if not already done.
  if (predefinedColors.size() == 0)
  {
    for (unsigned int i = 0; i < COLOR_COUNT; i++)
      predefinedColors[colors[i].name] = colors[i].color;

#ifdef _WINDOWS
    for (unsigned int i = 0; i < SYS_COLOR_COUNT; i++)
    {
      COLORREF reference = GetSysColor(systemColors[i].value);
      systemColors[i].color[0] = GetRValue(reference);
      systemColors[i].color[1] = GetGValue(reference);
      systemColors[i].color[2] = GetBValue(reference);
      systemColors[i].color[3] = 1;
      predefinedColors[systemColors[i].name] = systemColors[i].color;
    };
#endif // #ifdef _WINDOWS
  };

  CColorMapIterator iterator = predefinedColors.find(name);
  bool result = iterator != predefinedColors.end();
  if (result)
  {
    color[0] = iterator->second[0];
    color[1] = iterator->second[1];
    color[2] = iterator->second[2];
    color[3] = iterator->second[3];
  };

  return result;
}


//----------------------------------------------------------------------------------------------------------------------

/**
 * Registers predifined colors.
 *
 * @param colorMap colors to add to the predefined color list.
 */
void registerSystemColors(const CColorMap& colorMap)
{
  for (CColorMapIterator iterator = colorMap.begin(); iterator != colorMap.end(); ++iterator)
    predefinedColors[iterator->first]= iterator->second;
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Converts a string to a color with byte members.
 * The allowed syntax for colors is (as given by the SVG specification) either an HTML like
 * value (e.g. #FFFFFF, #FFF) or a function like form (e.g. rgb(100, 255, 255), rgb(10%, 100%, 0%)).
 *
 * @param colorString The string to parse.
 * @param [out] The color converted from the string. It must have room for at least 3 members.
 * @return 
 *   0 - If a color could be found and converted.
 *   1 - If a color could be found but a conversion error occured.
 *   2 - No color was given.
 *   3 - The special color "none" was found.
 */
int stringToColor(string colorString, GLubyte* color)
{
  int result = 2;
  
  if (colorString.size() > 0)
  {
    color[0] = 0;
    color[1] = 0; 
    color[2] = 0; 
    const char* Head = colorString.c_str();
    const char* Tail;

    // Start by skipping leading white spaces. We have only simple ASCII compatible strings here,
    // so we don't need to care for conversion from UTF-8.
    while ((*Head != '\0') && ((*Head == ' ') || (*Head == '\t')))
      Head++;

    if (*Head != '\0')
    {
      if (*Head == '#')
      {
        // HTML color.
        Head++;
        Tail = Head;
        while ((*Tail >= '0' && *Tail <= '9') || (*Tail >= 'a' && *Tail <= 'f') || (*Tail >= 'A' && *Tail <= 'F'))
          Tail++;
        switch (Tail - Head)
        {
          // Support only the two defined cases. Other values are simply ignored.
          case 3:
            {
              color[0] = hexToByte(*Head, *Head);
              Head++;
              color[1] = hexToByte(*Head, *Head);
              Head++;
              color[2] = hexToByte(*Head, *Head);
              result = 0;

              break;
            };
          case 6:
            {
              Tail = Head + 1;
              color[0] = hexToByte(*Head, *Tail);
              Head += 2; Tail += 2;
              color[1] = hexToByte(*Head, *Tail);
              Head += 2; Tail += 2;
              color[2] = hexToByte(*Head, *Tail);
              result = 0;

              break;
            };
        }
      }
      else
        if (strcmp(Head, "none") == 0)
        {
          // Do not fill at all.
          result = 3;
        }
        else
        {
          bool isRGB = strncmp(Head, "rgb(", 4) == 0;
          if (isRGB)
          {
            // Found a function call like specification. Split the entries and look if they are absolute
            // or percentage values.
            int Index = 0;
            int value;
            int ComponentCount = 3;
            Head += ComponentCount + 1;
            while (Index < ComponentCount)
            {
              value = 0;
              
              // Skip leading white spaces.
              while ((*Head != '\0') && ((*Head == ' ') || (*Head == '\t')))
                Head++;
              while ((*Head >= '0') && (*Head <= '9'))
                value = value * 10 + (*Head++ - '0');

              if (value < 0)
                value = 0;
              if (*Head == '%')
              {
                if (value > 100)
                  value = 100;
                value = (value * 255) / 100;
                Head++;
              }
              else
              {
                if (value > 255)
                  value = 255;
              };

              color[Index++] = value;

              // Skip trailing white spaces.
              while ((*Head != '\0') && ((*Head == ' ') || (*Head == '\t')))
                Head++;

              // If there is no comma or closing parenthesis then there is something wrong.
              if (*Head == ')') 
                break;
              if (*Head != ',')
                return 1;

              Head++;
            };
            if ((*Head == ')') && (Index == ComponentCount))
              result = 0;
            else
              result = 1;
          }
          else
          {
            // Last chance are color names. Try to find the text in the color constants.
            if (colorByName(Head, color))
              result = 0;
            else
              result = 1;
          };
        };
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Converts a string to color with float members.
 * The allowed syntax for colors is (as given by the SVG specification) either an HTML like
 * value (e.g. #FFFFFF, #FFF) or a function like form (e.g. rgb(100, 255, 255), rgb(10%, 100%, 0%)).
 *
 * @param colorString The string to parse.
 * @param [out] The color converted from the string. It must have room for at least 3 members.
 * @return 
 *   0 - If a color could be found and converted.
 *   1 - If a color could be found but a conversion error occured.
 *   2 - No color was given.
 *   3 - The special color "none" was found.
 */
int stringToColor(string colorString, GLfloat* color)
{
  GLubyte ByteColor[3];
  int result = stringToColor(colorString, ByteColor);
  if (result == 0)
  {
    color[0] = (float) ByteColor[0] / 255;
    color[1] = (float) ByteColor[1] / 255;
    color[2] = (float) ByteColor[2] / 255;
  };
  return result;
}

//----------------------------------------------------------------------------------------------------------------------

// The following matrix code was taken from Mesa3D (http://www.mesa3d.org/).

#define A(row,col)  a[(col<<2)+row]
#define B(row,col)  b[(col<<2)+row]
#define P(row,col)  product[(col<<2)+row]

/**
 * Perform a full 4x4 matrix multiplication.
 *
 * @param a matrix.
 * @param b matrix.
 * @param product will receive the product of a and b.
 *
 * @warning Is assumed that product != b. product == a is allowed.
 *
 * @note KW: 4*16 = 64 multiplications
 * 
 * @author This function was taken from Mesa3D (http://www.mesa3d.org/).
 */
void matrixMultiply(TMatrix product, const TMatrix a, const TMatrix b)
{
   GLint i;
   for (i = 0; i < 4; i++) {
      const GLfloat ai0=A(i,0),  ai1=A(i,1),  ai2=A(i,2),  ai3=A(i,3);
      P(i,0) = ai0 * B(0,0) + ai1 * B(1,0) + ai2 * B(2,0) + ai3 * B(3,0);
      P(i,1) = ai0 * B(0,1) + ai1 * B(1,1) + ai2 * B(2,1) + ai3 * B(3,1);
      P(i,2) = ai0 * B(0,2) + ai1 * B(1,2) + ai2 * B(2,2) + ai3 * B(3,2);
      P(i,3) = ai0 * B(0,3) + ai1 * B(1,3) + ai2 * B(2,3) + ai3 * B(3,3);
   }
}

#undef A
#undef B
#undef P

//----------------------------------------------------------------------------------------------------------------------

/**
 * Generate a 4x4 transformation matrix from glRotate parameters, and post-multiply the input matrix by it.
 *
 * @param mat The target matrix to multiply the rotation into.
 * @param angle The angle around which to rotate.
 * @param x The x coordinate of the axis to turn around.
 * @param y The y coordinate of the axis to turn around.
 * @param z The z coordinate of the axis to turn around.
 * @author This function was taken from Mesa3D (http://www.mesa3d.org/).
 */
void matrixRotate(TMatrix mat, GLfloat angle, GLfloat x, GLfloat y, GLfloat z)
{
   GLfloat xx, yy, zz, xy, yz, zx, xs, ys, zs, one_c, s, c;
   TMatrix m;
   GLboolean optimized;

   s = (GLfloat) sin( angle * DEG2RAD );
   c = (GLfloat) cos( angle * DEG2RAD );

   memcpy(m, identity, sizeof(TMatrix));
   optimized = GL_FALSE;

#define m(row,col)  m[col * 4 + row]

   if (x == 0.0F) {
      if (y == 0.0F) {
         if (z != 0.0F) {
            optimized = GL_TRUE;
            /* rotate only around z-axis */
            m(0,0) = c;
            m(1,1) = c;
            if (z < 0.0F) {
               m(0,1) = s;
               m(1,0) = -s;
            }
            else {
               m(0,1) = -s;
               m(1,0) = s;
            }
         }
      }
      else if (z == 0.0F) {
         optimized = GL_TRUE;
         /* rotate only around y-axis */
         m(0,0) = c;
         m(2,2) = c;
         if (y < 0.0F) {
            m(0,2) = -s;
            m(2,0) = s;
         }
         else {
            m(0,2) = s;
            m(2,0) = -s;
         }
      }
   }
   else if (y == 0.0F) {
      if (z == 0.0F) {
         optimized = GL_TRUE;
         /* rotate only around x-axis */
         m(1,1) = c;
         m(2,2) = c;
         if (x < 0.0F) {
            m(1,2) = s;
            m(2,1) = -s;
         }
         else {
            m(1,2) = -s;
            m(2,1) = s;
         }
      }
   }

   if (!optimized) {
      const GLfloat mag = sqrt(x * x + y * y + z * z);

      if (mag <= 1.0e-4) {
         /* no rotation, leave mat as-is */
         return;
      }

      x /= mag;
      y /= mag;
      z /= mag;


      /*
       *     Arbitrary axis rotation matrix.
       *
       *  This is composed of 5 matrices, Rz, Ry, T, Ry', Rz', multiplied
       *  like so:  Rz * Ry * T * Ry' * Rz'.  T is the final rotation
       *  (which is about the x-axis), and the two composite transforms
       *  Ry' * Rz' and Rz * Ry are (respectively) the rotations necessary
       *  from the arbitrary axis to the x-axis then back.  They are
       *  all elementary rotations.
       *
       *  Rz' is a rotation about the z-axis, to bring the axis vector
       *  into the x-z plane.  Then Ry' is applied, rotating about the
       *  y-axis to bring the axis vector parallel with the x-axis.  The
       *  rotation about the x-axis is then performed.  Ry and Rz are
       *  simply the respective inverse transforms to bring the arbitrary
       *  axis back to it's original orientation.  The first transforms
       *  Rz' and Ry' are considered inverses, since the data from the
       *  arbitrary axis gives you info on how to get to it, not how
       *  to get away from it, and an inverse must be applied.
       *
       *  The basic calculation used is to recognize that the arbitrary
       *  axis vector (x, y, z), since it is of unit length, actually
       *  represents the sines and cosines of the angles to rotate the
       *  x-axis to the same orientation, with theta being the angle about
       *  z and phi the angle about y (in the order described above)
       *  as follows:
       *
       *  cos ( theta ) = x / sqrt ( 1 - z^2 )
       *  sin ( theta ) = y / sqrt ( 1 - z^2 )
       *
       *  cos ( phi ) = sqrt ( 1 - z^2 )
       *  sin ( phi ) = z
       *
       *  Note that cos ( phi ) can further be inserted to the above
       *  formulas:
       *
       *  cos ( theta ) = x / cos ( phi )
       *  sin ( theta ) = y / sin ( phi )
       *
       *  ...etc.  Because of those relations and the standard trigonometric
       *  relations, it is pssible to reduce the transforms down to what
       *  is used below.  It may be that any primary axis chosen will give the
       *  same results (modulo a sign convention) using thie method.
       *
       *  Particularly nice is to notice that all divisions that might
       *  have caused trouble when parallel to certain planes or
       *  axis go away with care paid to reducing the expressions.
       *  After checking, it does perform correctly under all cases, since
       *  in all the cases of division where the denominator would have
       *  been zero, the numerator would have been zero as well, giving
       *  the expected result.
       */

      xx = x * x;
      yy = y * y;
      zz = z * z;
      xy = x * y;
      yz = y * z;
      zx = z * x;
      xs = x * s;
      ys = y * s;
      zs = z * s;
      one_c = 1.0F - c;

      /* We already hold the identity-matrix so we can skip some statements */
      m(0,0) = (one_c * xx) + c;
      m(0,1) = (one_c * xy) - zs;
      m(0,2) = (one_c * zx) + ys;
/*    m(0,3) = 0.0F; */

      m(1,0) = (one_c * xy) + zs;
      m(1,1) = (one_c * yy) + c;
      m(1,2) = (one_c * yz) - xs;
/*    m(1,3) = 0.0F; */

      m(2,0) = (one_c * zx) - ys;
      m(2,1) = (one_c * yz) + xs;
      m(2,2) = (one_c * zz) + c;
/*    m(2,3) = 0.0F; */

/*
      m(3,0) = 0.0F;
      m(3,1) = 0.0F;
      m(3,2) = 0.0F;
      m(3,3) = 1.0F;
*/
   }
#undef m

   matrixMultiply(mat, mat, m);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Multiply a matrix with a general scaling matrix.
 *
 * @param mat matrix.
 * @param x x axis scale factor.
 * @param y y axis scale factor.
 * @param z z axis scale factor.
 *
 * Multiplies in-place the elements of mat by the scale factors.
 *
 * @author This function was taken from Mesa3D (http://www.mesa3d.org/).
 */
void matrixScale(TMatrix mat, GLfloat x, GLfloat y, GLfloat z)
{
  mat[0] *= x; mat[4] *= y; mat[8]  *= z;
  mat[1] *= x; mat[5] *= y; mat[9]  *= z;
  mat[2] *= x; mat[6] *= y; mat[10] *= z;
  mat[3] *= x; mat[7] *= y; mat[11] *= z;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Multiply a matrix with a translation matrix.
 *
 * @param mat matrix.
 * @param x translation vector x coordinate.
 * @param y translation vector y coordinate.
 * @param z translation vector z coordinate.
 *
 * Adds the translation coordinates to the elements of mat in-place.
 *
 * @author This function was taken from Mesa3D (http://www.mesa3d.org/).
*/
void matrixTranslate(TMatrix mat, GLfloat x, GLfloat y, GLfloat z)
{
  mat[12] = mat[0] * x + mat[4] * y + mat[8]  * z + mat[12];
  mat[13] = mat[1] * x + mat[5] * y + mat[9]  * z + mat[13];
  mat[14] = mat[2] * x + mat[6] * y + mat[10] * z + mat[14];
  mat[15] = mat[3] * x + mat[7] * y + mat[11] * z + mat[15];
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Multiplies the given vertex by matrix m and returns the result.
 *
 * @param m The matrix containing the transformation parameters.
 * @param v The vertex to transform.
 * @return The transformed vertex.
 */
TVertex matrixTransform(TMatrix m, const TVertex& v)
{
  TVertex result;

  result.x = v.x * m[0] + v.y * m[4] + v.z * m[8] + v.w * m[12];
  result.y = v.x * m[1] + v.y * m[5] + v.z * m[9] + v.w * m[13];
  result.z = v.x * m[2] + v.y * m[6] + v.z * m[10] + v.w * m[14];
  result.w = v.x * m[3] + v.y * m[7] + v.z * m[11] + v.w * m[15];

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * References an element of 4x4 matrix.
 *
 * @param m matrix array.
 * @param c column of the desired element.
 * @param r row of the desired element.
 * @return value of the desired element.
 * @author This code was taken from Mesa3D (http://www.mesa3d.org/).
 *
 * Calculate the linear storage index of the element and references it. 
 */
#define MAT(m,r,c) (m)[(c)*4+(r)]

/**
 * Swaps the values of two floating pointer variables.
 *
 * Used by invertMatrix to swap the row pointers.
 */
#define SWAP_ROWS(a, b) { GLfloat *_tmp = a; (a) = (b); (b) = _tmp; }

/**
 * Compute inverse of 4x4 transformation matrix.
 * 
 * @param input The matrix to invert.
 * @param output [out] The inverted matrix.
 * @return true for success, false for failure (singular matrix).
 * @author This function was taken from Mesa3D (http://www.mesa3d.org/).
 *
 * Calculates the inverse matrix by performing the gaussian matrix reduction
 * with partial pivoting followed by back/substitution with the loops manually
 * unrolled.
 */
bool matrixInvert(const TMatrix& input, TMatrix& output)
{
   GLfloat wtmp[4][8];
   GLfloat m0, m1, m2, m3, s;
   GLfloat *r0, *r1, *r2, *r3;

   r0 = wtmp[0], r1 = wtmp[1], r2 = wtmp[2], r3 = wtmp[3];

   r0[0] = MAT(input,0,0), r0[1] = MAT(input,0,1),
   r0[2] = MAT(input,0,2), r0[3] = MAT(input,0,3),
   r0[4] = 1.0, r0[5] = r0[6] = r0[7] = 0.0,

   r1[0] = MAT(input,1,0), r1[1] = MAT(input,1,1),
   r1[2] = MAT(input,1,2), r1[3] = MAT(input,1,3),
   r1[5] = 1.0, r1[4] = r1[6] = r1[7] = 0.0,

   r2[0] = MAT(input,2,0), r2[1] = MAT(input,2,1),
   r2[2] = MAT(input,2,2), r2[3] = MAT(input,2,3),
   r2[6] = 1.0, r2[4] = r2[5] = r2[7] = 0.0,

   r3[0] = MAT(input,3,0), r3[1] = MAT(input,3,1),
   r3[2] = MAT(input,3,2), r3[3] = MAT(input,3,3),
   r3[7] = 1.0, r3[4] = r3[5] = r3[6] = 0.0;

   /* choose pivot - or die */
   if (fabs(r3[0])>fabs(r2[0])) SWAP_ROWS(r3, r2);
   if (fabs(r2[0])>fabs(r1[0])) SWAP_ROWS(r2, r1);
   if (fabs(r1[0])>fabs(r0[0])) SWAP_ROWS(r1, r0);
   if (0.0 == r0[0])  return false;

   /* eliminate first variable     */
   m1 = r1[0]/r0[0]; m2 = r2[0]/r0[0]; m3 = r3[0]/r0[0];
   s = r0[1]; r1[1] -= m1 * s; r2[1] -= m2 * s; r3[1] -= m3 * s;
   s = r0[2]; r1[2] -= m1 * s; r2[2] -= m2 * s; r3[2] -= m3 * s;
   s = r0[3]; r1[3] -= m1 * s; r2[3] -= m2 * s; r3[3] -= m3 * s;
   s = r0[4];
   if (s != 0.0) { r1[4] -= m1 * s; r2[4] -= m2 * s; r3[4] -= m3 * s; }
   s = r0[5];
   if (s != 0.0) { r1[5] -= m1 * s; r2[5] -= m2 * s; r3[5] -= m3 * s; }
   s = r0[6];
   if (s != 0.0) { r1[6] -= m1 * s; r2[6] -= m2 * s; r3[6] -= m3 * s; }
   s = r0[7];
   if (s != 0.0) { r1[7] -= m1 * s; r2[7] -= m2 * s; r3[7] -= m3 * s; }

   /* choose pivot - or die */
   if (fabs(r3[1])>fabs(r2[1])) SWAP_ROWS(r3, r2);
   if (fabs(r2[1])>fabs(r1[1])) SWAP_ROWS(r2, r1);
   if (0.0 == r1[1])  return false;

   /* eliminate second variable */
   m2 = r2[1]/r1[1]; m3 = r3[1]/r1[1];
   r2[2] -= m2 * r1[2]; r3[2] -= m3 * r1[2];
   r2[3] -= m2 * r1[3]; r3[3] -= m3 * r1[3];
   s = r1[4]; if (0.0 != s) { r2[4] -= m2 * s; r3[4] -= m3 * s; }
   s = r1[5]; if (0.0 != s) { r2[5] -= m2 * s; r3[5] -= m3 * s; }
   s = r1[6]; if (0.0 != s) { r2[6] -= m2 * s; r3[6] -= m3 * s; }
   s = r1[7]; if (0.0 != s) { r2[7] -= m2 * s; r3[7] -= m3 * s; }

   /* choose pivot - or die */
   if (fabs(r3[2])>fabs(r2[2])) SWAP_ROWS(r3, r2);
   if (0.0 == r2[2])  return GL_FALSE;

   /* eliminate third variable */
   m3 = r3[2]/r2[2];
   r3[3] -= m3 * r2[3], r3[4] -= m3 * r2[4],
   r3[5] -= m3 * r2[5], r3[6] -= m3 * r2[6],
   r3[7] -= m3 * r2[7];

   /* last check */
   if (0.0 == r3[3]) return GL_FALSE;

   s = 1.0F/r3[3];             /* now back substitute row 3 */
   r3[4] *= s; r3[5] *= s; r3[6] *= s; r3[7] *= s;

   m2 = r2[3];                 /* now back substitute row 2 */
   s  = 1.0F/r2[2];
   r2[4] = s * (r2[4] - r3[4] * m2), r2[5] = s * (r2[5] - r3[5] * m2),
   r2[6] = s * (r2[6] - r3[6] * m2), r2[7] = s * (r2[7] - r3[7] * m2);
   m1 = r1[3];
   r1[4] -= r3[4] * m1, r1[5] -= r3[5] * m1,
   r1[6] -= r3[6] * m1, r1[7] -= r3[7] * m1;
   m0 = r0[3];
   r0[4] -= r3[4] * m0, r0[5] -= r3[5] * m0,
   r0[6] -= r3[6] * m0, r0[7] -= r3[7] * m0;

   m1 = r1[2];                 /* now back substitute row 1 */
   s  = 1.0F/r1[1];
   r1[4] = s * (r1[4] - r2[4] * m1), r1[5] = s * (r1[5] - r2[5] * m1),
   r1[6] = s * (r1[6] - r2[6] * m1), r1[7] = s * (r1[7] - r2[7] * m1);
   m0 = r0[2];
   r0[4] -= r2[4] * m0, r0[5] -= r2[5] * m0,
   r0[6] -= r2[6] * m0, r0[7] -= r2[7] * m0;

   m0 = r0[1];                 /* now back substitute row 0 */
   s  = 1.0F/r0[0];
   r0[4] = s * (r0[4] - r1[4] * m0), r0[5] = s * (r0[5] - r1[5] * m0),
   r0[6] = s * (r0[6] - r1[6] * m0), r0[7] = s * (r0[7] - r1[7] * m0);

   MAT(output,0,0) = r0[4]; MAT(output,0,1) = r0[5],
   MAT(output,0,2) = r0[6]; MAT(output,0,3) = r0[7],
   MAT(output,1,0) = r1[4]; MAT(output,1,1) = r1[5],
   MAT(output,1,2) = r1[6]; MAT(output,1,3) = r1[7],
   MAT(output,2,0) = r2[4]; MAT(output,2,1) = r2[5],
   MAT(output,2,2) = r2[6]; MAT(output,2,3) = r2[7],
   MAT(output,3,0) = r3[4]; MAT(output,3,1) = r3[5],
   MAT(output,3,2) = r3[6]; MAT(output,3,3) = r3[7];

   return true;
}
#undef SWAP_ROWS
#undef MAT

//----------------- CBoundingBoxComputer -------------------------------------------------------------------------------

CBoundingBoxComputer::CBoundingBoxComputer(void)
{
  FEmpty = true;
}

//----------------------------------------------------------------------------------------------------------------------

CBoundingBoxComputer::CBoundingBoxComputer(const TBoundingBox& initialBox)
{
  FEmpty = false;
  FCurrentBox = initialBox;
}

//----------------------------------------------------------------------------------------------------------------------

void CBoundingBoxComputer::includeVertex(const TVertex& vertex)
{
  if (FEmpty)
  {
    FEmpty = false;
    FCurrentBox.upper = vertex;
    FCurrentBox.lower = vertex;
  }
  else
  {
    // Keep in mind origin is in the upper left corner, positive y pointing down.
    // The upper vertex is the one visually above the other, but with smaller y values.
    if (vertex.x < FCurrentBox.upper.x)
      FCurrentBox.upper.x = vertex.x;
    if (vertex.x > FCurrentBox.lower.x)
      FCurrentBox.lower.x = vertex.x;
    if (vertex.y < FCurrentBox.upper.y)
      FCurrentBox.upper.y = vertex.y;
    if (vertex.y > FCurrentBox.lower.y)
      FCurrentBox.lower.y = vertex.y;
    if (vertex.z < FCurrentBox.upper.z)
      FCurrentBox.upper.z = vertex.z;
    if (vertex.z > FCurrentBox.lower.z)
      FCurrentBox.lower.z = vertex.z;
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current bounding box.
 *
 * @return The current bounding box.
 */
TBoundingBox CBoundingBoxComputer::boundingBox()
{
  if (FEmpty)
  {
    TBoundingBox empty; // Implicitly sets all values to zero.
    return empty;
  }
  else
    return FCurrentBox;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Takes the new box and merges it with the current bounding box. The new data is not required to be ordered.
 *
 * @param matrix An optional matrix that is used to transform the given coordinates.
 * @param newBox A new bounding box to merge in.
 */
void CBoundingBoxComputer::include(TMatrix matrix, const TBoundingBox& newBox)
{
  float LLX;
  float URX;
  if (newBox.upper.x < newBox.lower.x)
  {
    LLX = newBox.upper.x;
    URX = newBox.lower.x;
  }
  else
  {
    LLX = newBox.lower.x;
    URX = newBox.upper.x;
  };

  float LLY;
  float URY;
  if (newBox.upper.y < newBox.lower.y)
  {
    LLY = newBox.upper.y;
    URY = newBox.lower.y;
  }
  else
  {
    LLY = newBox.lower.y;
    URY = newBox.upper.y;
  };

  float LLZ;
  float URZ;
  if (newBox.upper.z < newBox.lower.z)
  {
    LLZ = newBox.upper.z;
    URZ = newBox.lower.z;
  }
  else
  {
    LLZ = newBox.lower.z;
    URZ = newBox.upper.z;
  };

	TVertex lower(LLX, LLY, LLZ, 1);
	if (matrix != NULL)
		lower = matrixTransform(matrix, lower);
	includeVertex(lower);

  TVertex upper(URX, URY, URZ, 1);
  if (matrix != NULL)
    upper = matrixTransform(matrix, upper);
  includeVertex(upper);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Simplyfied version to include a box but translate it by the given offset (instead a full blown matrix transformation).
 *
 * @param offset The offset by which the new box coordinates should be moved.
 * @param newBox The box to include.
 */
void CBoundingBoxComputer::include(const TVertex& offset, const TBoundingBox& newBox)
{
  float LLX;
  float URX;
  if (newBox.upper.x < newBox.lower.x)
  {
    LLX = newBox.upper.x;
    URX = newBox.lower.x;
  }
  else
  {
    LLX = newBox.lower.x;
    URX = newBox.upper.x;
  };

  float LLY;
  float URY;
  if (newBox.upper.y < newBox.lower.y)
  {
    LLY = newBox.upper.y;
    URY = newBox.lower.y;
  }
  else
  {
    LLY = newBox.lower.y;
    URY = newBox.upper.y;
  };

  float LLZ;
  float URZ;
  if (newBox.upper.z < newBox.lower.z)
  {
    LLZ = newBox.upper.z;
    URZ = newBox.lower.z;
  }
  else
  {
    LLZ = newBox.lower.z;
    URZ = newBox.upper.z;
  };

  TVertex v;
  v.x = LLX + offset.x;
	v.y = LLY + offset.y;
	v.z = LLZ + offset.z;
  includeVertex(v);

  v.x = URX + offset.y;
	v.y = URY + offset.y;
	v.y = URZ + offset.z;
  includeVertex(v);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Takes the vertex and merges it with the current bounding box.
 *
 * @param matrix An optional matrix that is used to transform the given coordinates.
 * @param vertex The vertex to be included
 */
void CBoundingBoxComputer::include(TMatrix matrix, const TVertex& vertex)
{
  TVertex v;
  if (matrix != NULL)
    v = matrixTransform(matrix, vertex);
  else
    v = vertex;
  includeVertex(v);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Takes the x and y values and merges them with the current bounding box.
 *
 * @param matrix An optional matrix that is used to transform the given coordinates.
 * @param x The x coordinate of the point to include.
 * @param y The y coordinate of the point to include.
 */
void CBoundingBoxComputer::include(TMatrix matrix, const float& x, const float& y)
{
  TVertex v(x, y, 0, 1);
  if (matrix != NULL)
    v = matrixTransform(matrix, v);
  includeVertex(v);
}

//----------------------------------------------------------------------------------------------------------------------

struct TextSegment {
  size_t offset;
  size_t length;
  wstring variable;
};


/**
 * Preprocesses a file containing $variables$, replacing them with the values
 * provided in the variable dictionary and returning the resulting data.
 *
 * @param filename The file to preprocess.
 * @param variables A mapping of variables names to it's values.
 * @param size size of the data is returned here.
 * 
 * @returns The preprocessed data or NULL on error.
 */
char *preprocessFile(const string &filename, map<wstring, wstring>& variables, size_t &size)
{
  char *data= NULL;
  size_t tmpSize;
  vector<TextSegment> segments;

  size= 0;
  FILE *file= openFile(filename, "rb");
  if (file)
  {
    fseek(file, 0, SEEK_END);
    tmpSize= ftell(file);
  
    data= (char*)malloc(tmpSize+1);
    if (data)
    {
      rewind(file);
      tmpSize= fread(data, 1, tmpSize, file);
      if (tmpSize > 0)
      {
        TextSegment segment;
        char *ptr, *begin;

        data[tmpSize]= 0;
        
        // build a list of text segment ranges from the file,
        // with variables inserted
        ptr= data;
        while (ptr && *ptr)
        {          
          begin= strchr(ptr, '$');
          if (begin)
          {            
            segment.offset= ptr-data;
            segment.length= begin-ptr;
            segment.variable= L"";
            segments.push_back(segment);
            size+= segment.length;

            char *end= begin+1;
            while (*end && *end != '$' && isalnum(*end)) ++end;
            if (*end == '$')
            {
              wstring var= wstring(begin + 1, end);

              segment.offset= 0;
              segment.length= 0;
              if (variables.find(var) != variables.end())
              {
                segment.variable= variables[var];
                segments.push_back(segment);
                size+= segment.variable.size();
                ptr= end+1;
              }
              else
                ptr= begin+1;
            }
            else
            {
              ptr= begin+1;
            }
          }
          else
            break;
        }
        segment.offset= ptr-data;
        segment.length= strlen(ptr);
        segment.variable= L"";
        segments.push_back(segment);
        size+= segment.length;
        
        // create the resulting data
        data= (char*)realloc(data, size+1);
        ptr= data;
        
        rewind(file);
        
        for (vector<TextSegment>::const_iterator iter= segments.begin(); 
             iter != segments.end(); ++iter)
        {
          if (iter->length > 0)
          {
            fseek(file, (long) iter->offset, SEEK_SET);
            fread(ptr, 1, iter->length, file);
            ptr+= iter->length;
          }
          else
          {
            string value = utf16ToUtf8(iter->variable);
            memcpy(ptr, value.c_str(), value.size());
            ptr+= value.size();
          }
        }
      }
      else
      {
        free(data);
        data= NULL;
      }
    }
    fclose(file);
  }
  return data;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Implementation of a moving average filter to blur the input bitmap.
 *
 * @param values [in, out] The bitmap to blur.
 * @param width The width of the bitmap.
 * @param height The height of the bitmap.
 * @param size The number of neighbour values to include in the average computation. Size must be odd!
 */
void averageMeanBlur(unsigned char* values, int width, int height, int size)
{
#define PIXELADDRESS(column, row) values + ((row) * width + (column))

  int accumulator;
  unsigned char* buffer;

  // Start with horizontal spans.
  buffer = (unsigned char*) malloc(width);
  for (int y = 0; y < height; ++y)
  {
    memset(buffer, 0, width);
    unsigned char* run = PIXELADDRESS(0, y);

    // Compute first size number of averages. Note: size must be odd!
    accumulator = 0;
    for (int i = 0; i < size; ++i)
      accumulator += *run++;

    buffer[size / 2] = accumulator / size;
    int runIndex = size / 2 + 1;

    // Continue with the iterative part.
    unsigned char* runLow = PIXELADDRESS(0, y);
    unsigned char* runHigh = PIXELADDRESS(size, y);
    for (int i = 0; i < width - size; ++i)
    {
      accumulator += *runHigh++ - *runLow++;
      buffer[runIndex++] = accumulator / size;
    };
    memcpy(PIXELADDRESS(0, y), buffer, width);
  };

  free(buffer);

  // The vertical part.
  buffer = (unsigned char*) malloc(height);
  for (int x = 0; x < width; ++x)
  {
    memset(buffer, 0, height);
    unsigned char* run = PIXELADDRESS(x, 0);

    // Compute first size number of averages.
    accumulator = 0;
    for (int i = 0; i < size; ++i)
    {
      accumulator +=  *run;
      run += width;
    };
    buffer[size / 2] = accumulator / size;
    int runIndex = size / 2 + 1;

    // Continue with the iterative part.
    unsigned char* runLow = PIXELADDRESS(x, 0);
    unsigned char* runHigh = PIXELADDRESS(x, size);
    for (int i = 0; i < height - size; ++i)
    {
      accumulator += *runHigh - *runLow;
      buffer[runIndex++] = accumulator / size;
      runHigh += width;
      runLow += width;
    };
    run = PIXELADDRESS(x, 0);
    for (int i = 0; i < height; ++i)
    {
      *run = buffer[i];
      run += width;
    };
  };

  free(buffer);
};

//----------------------------------------------------------------------------------------------------------------------

