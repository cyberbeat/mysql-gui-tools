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
 * @file myx_gc_gl_helper.cpp
 * @brief Helper functions for creating OpenGL data and structures out of XML data.
 */

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>

#include "myx_gc_gl_helper.h"

#ifdef __APPLE__
 #import <mach-o/dyld.h>
#endif

extern const string defaultTextureWrapMode;     
extern const string defaultTextureMinFilter; 
extern const string defaultTextureMagFilter;      
extern const int defaultTextureDimensions;
extern const string defaultTextureMode;

//----------------------------------------------------------------------------------------------------------------------

// Helper method to retrieve a float attribute.
bool getFloatAttribute(xmlNodePtr Element, const char* name, float& Value)
{
  xmlChar* Attribute = xmlGetProp(Element, (xmlChar*) name);
  if (Attribute == NULL)
    return false;
  Value = (float) atof((const char*) Attribute);
  xmlFree(Attribute);
  return true;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Helper method to retrieve an integer attribute. If it cannot be found a default value will be used instead.
 */
float getFloatAttributeDef(xmlNodePtr Element, const char* name, float Default)
{
  float result;
  if (!getFloatAttribute(Element, name, result))
    result = Default;

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

// Helper method to retrieve an integer attribute.
bool getIntAttribute(xmlNodePtr Element, const char* name, int& Value)
{
  xmlChar* Attribute = xmlGetProp(Element, (xmlChar*) name);
  if (Attribute == NULL)
    return false;
  Value = atoi((const char*) Attribute);
  xmlFree(Attribute);
  return true;
}

//----------------------------------------------------------------------------------------------------------------------

// Helper method to retrieve an integer attribute. If it cannot be found a default value will be used instead.
int getIntAttributeDef(xmlNodePtr Element, const char* name, int Default)
{
  int result;
  if (!getIntAttribute(Element, name, result))
    result = Default;

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

// Helper method to retrieve a string attribute.
// If the attribute could be found then true is returned and Value is set to the value of the attribute.
// Otherwise false is returned and Value is not touched.
bool getStringAttribute(xmlNodePtr Element, const char* name, string& Value)
{
  xmlChar* Attribute = xmlGetProp(Element, (xmlChar*) name);
  if (Attribute == NULL)
    return false;
  else
  {
    Value.clear();
    Value.append((char*) Attribute);
    return true;
  }
}

//----------------------------------------------------------------------------------------------------------------------

// Helper method to retrieve a string attribute. If the attribute is empty or cannot be found then a default value is returned.
string getStringAttributeDef(xmlNodePtr Element, const char* name, const string Default)
{
  string result;
  xmlChar* Value = xmlGetProp(Element, (xmlChar*) name);
  if ((Value == NULL) || (*Value == '\0'))
    result += Default;
  else
    result += (char*) Value;

  if (Value != NULL)
    xmlFree(Value);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Reads attribute name from Element and tries to treat the string as a color.
 * The allowed syntax for colors is (as given by the SVG specification) either an HTML like
 * value (e.g. #FFFFFF, #FFF) or a function like form (e.g. rgb(100, 255, 255), rgb(10%, 100%, 0%)).
 *
 * @param Element The XML element to parse.
 * @param name The name of the color attribute.
 * @param Color [out] The converted color.
 * @return 
 *   0 - If a color could be found and converted.
 *   1 - If a color could be found but a conversion error occured.
 *   2 - No color was given.
 *   3 - The special color "none" was found.
 */

int convertColor(xmlNodePtr Element, const char* name, GLubyte* Color)
{
  int result = 2;
  
  xmlChar* Attribute = xmlGetProp(Element, (xmlChar*) name);
  if (Attribute != NULL)
  {
    result = stringToColor((char*) Attribute, Color);
    xmlFree(Attribute);
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 *  Rounds the given value up to the next power of two boundary.
 *
 *  @param Value The value to round up.
 *  @return Returns the rounded value.
 */
int roundUpToPowerOf2(int Value)
{
#define LOG2(value) (log(value) / log(2.0))

  double LogTwo = LOG2((double) Value);
  if (floor(LogTwo) < LogTwo)
    return (int) floor(pow(2.0, floor(LogTwo) + 1));
  else
    return Value;

#undef LOG2
}

//----------------------------------------------------------------------------------------------------------------------

// Parses the given XML node for texture information and creates a new entry in the texture manager.
void parseTextureEntry(xmlNodePtr XML, CGCModel* model)
{
  // Collect font information.
  string id;
  string filename;
  if (getStringAttribute(XML, "id", id))
  {
    string wrapModeH  = getStringAttributeDef(XML, "wrap-mode-horizontal", defaultTextureWrapMode);
    string wrapModeV  = getStringAttributeDef(XML, "wrap-mode-vertical", defaultTextureWrapMode);
    string minFilter = getStringAttributeDef(XML, "min-filter", defaultTextureMinFilter);
    string maxFilter = getStringAttributeDef(XML, "mag-filter", defaultTextureMagFilter);
    int dimensions = getIntAttributeDef(XML, "dimensions", defaultTextureDimensions);
    string mode = getStringAttributeDef(XML, "mode", defaultTextureMode);

    // See if there are any LOD sub elements and if there is a LOD 0 entry as parameter.
    TLODList lodData;
    if (getStringAttribute(XML, "location", filename))
    {
      // The location parameter in the texture tag always gives the top level of detail.
      lodData.push_back(utf8ToUtf16(filename));
    };
    xmlNodePtr run = XML->children;
    while (run != NULL)
    {
      if (xmlStrcmp(run->name, (const xmlChar *) "texture-lod") == 0)
      {
        TLODList::size_type Level = getIntAttributeDef(run, "lod", 0);
        if (lodData.size() <= Level)
          lodData.resize(Level + 1);
        lodData[Level] = utf8ToUtf16(getStringAttributeDef(run, "location", ""));
      };
      run = run->next;
    };

    model->createTexture(lodData, utf8ToUtf16(id), wrapModeH, wrapModeV, minFilter, maxFilter, dimensions, mode);
  };
}

//----------------------------------------------------------------------------------------------------------------------
#if !defined(__APPLE__)
PFNGLISRENDERBUFFEREXTPROC glIsRenderbufferEXT;
PFNGLBINDRENDERBUFFEREXTPROC glBindRenderbufferEXT;
PFNGLDELETERENDERBUFFERSEXTPROC glDeleteRenderbuffersEXT;
PFNGLGENRENDERBUFFERSEXTPROC glGenRenderbuffersEXT;
PFNGLRENDERBUFFERSTORAGEEXTPROC glRenderbufferStorageEXT;
PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC glGetRenderbufferParameterivEXT;
PFNGLISFRAMEBUFFEREXTPROC glIsFramebufferEXT;
PFNGLBINDFRAMEBUFFEREXTPROC glBindFramebufferEXT;
PFNGLDELETEFRAMEBUFFERSEXTPROC glDeleteFramebuffersEXT;
PFNGLGENFRAMEBUFFERSEXTPROC glGenFramebuffersEXT;
PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC glCheckFramebufferStatusEXT;
PFNGLFRAMEBUFFERTEXTURE1DEXTPROC glFramebufferTexture1DEXT;
PFNGLFRAMEBUFFERTEXTURE2DEXTPROC glFramebufferTexture2DEXT;
PFNGLFRAMEBUFFERTEXTURE3DEXTPROC glFramebufferTexture3DEXT;
PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC glFramebufferRenderbufferEXT;
PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC glGetFramebufferAttachmentParameterivEXT;
PFNGLGENERATEMIPMAPEXTPROC glGenerateMipmapEXT;
#endif
//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the address of an OpenGL function given by name.
 *
 * @param name The name of the function.
 * @return The address of the function with the given name or NULL, if not found.
 */
void* getProcAddress(const char* name)
{
#ifdef _WINDOWS
  return wglGetProcAddress(name);
#elif __APPLE__
  NSSymbol symbol;
  char *symbolName;

  // Prepend a '_' for the Unix C symbol mangling convention
  symbolName = (char*)malloc (strlen (name) + 2);
  strcpy(symbolName + 1, name);
  symbolName[0] = '_';
  symbol = NULL;
#if 1
  symbol= NSLookupSymbolInImage(NULL,
                                symbolName,
                                NSLOOKUPSYMBOLINIMAGE_OPTION_BIND|NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
#else
  if (NSIsSymbolNameDefined (symbolName))
      symbol = NSLookupAndBindSymbol (symbolName);
#endif
  free (symbolName);
  return symbol ? NSAddressOfSymbol (symbol) : NULL;
#else
  return (void*)glXGetProcAddressARB((const GLubyte*)name);
#endif
}

//----------------------------------------------------------------------------------------------------------------------

bool extensionsInitialized = false;

void initializeOpenGLExtensions(void)
{
  if (!extensionsInitialized)
  {
    extensionsInitialized = true;
#ifndef __APPLE__
    glIsRenderbufferEXT = (PFNGLISRENDERBUFFEREXTPROC) getProcAddress("glIsRenderbufferEXT");
    glBindRenderbufferEXT = (PFNGLBINDRENDERBUFFEREXTPROC) getProcAddress("glBindRenderbufferEXT");
    glDeleteRenderbuffersEXT = (PFNGLDELETERENDERBUFFERSEXTPROC) getProcAddress("glDeleteRenderbuffersEXT");
    glGenRenderbuffersEXT = (PFNGLGENRENDERBUFFERSEXTPROC)getProcAddress("glGenRenderbuffersEXT");
    glRenderbufferStorageEXT = (PFNGLRENDERBUFFERSTORAGEEXTPROC) getProcAddress("glRenderbufferStorageEXT");
    glGetRenderbufferParameterivEXT = (PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC) getProcAddress("glGetRenderbufferParameterivEXT");
    glIsFramebufferEXT = (PFNGLISFRAMEBUFFEREXTPROC) getProcAddress("glIsFramebufferEXT");
    glBindFramebufferEXT = (PFNGLBINDFRAMEBUFFEREXTPROC) getProcAddress("glBindFramebufferEXT");
    glDeleteFramebuffersEXT = (PFNGLDELETEFRAMEBUFFERSEXTPROC) getProcAddress("glDeleteFramebuffersEXT");
    glGenFramebuffersEXT = (PFNGLGENFRAMEBUFFERSEXTPROC)  getProcAddress("glGenFramebuffersEXT");
    glCheckFramebufferStatusEXT = (PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC) getProcAddress("glCheckFramebufferStatusEXT");
    glFramebufferTexture1DEXT = (PFNGLFRAMEBUFFERTEXTURE1DEXTPROC) getProcAddress("glFramebufferTexture1DEXT");
    glFramebufferTexture2DEXT = (PFNGLFRAMEBUFFERTEXTURE2DEXTPROC) getProcAddress("glFramebufferTexture2DEXT");
    glFramebufferTexture3DEXT = (PFNGLFRAMEBUFFERTEXTURE3DEXTPROC) getProcAddress("glFramebufferTexture3DEXT");
    glFramebufferRenderbufferEXT =(PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC)  getProcAddress("glFramebufferRenderbufferEXT");
    glGetFramebufferAttachmentParameterivEXT = (PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC) getProcAddress("glGetFramebufferAttachmentParameterivEXT");
    glGenerateMipmapEXT = (PFNGLGENERATEMIPMAPEXTPROC) getProcAddress("glGenerateMipmapEXT");
#endif
  };
}

//----------------------------------------------------------------------------------------------------------------------

