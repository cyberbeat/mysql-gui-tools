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
 * @file myx_gc_gl_helper.h 
 * @brief Helper functions for creating OpenGL data and structures out of XML data.
 * 
 */

#ifndef __GC_GL_HELPER_H__
#define __GC_GL_HELPER_H__

#include <libxml/xmlmemory.h>

#include "myx_gc_font_manager.h"
#include "myx_gc_texture.h"

extern PFNGLISRENDERBUFFEREXTPROC glIsRenderbufferEXT;
extern PFNGLBINDRENDERBUFFEREXTPROC glBindRenderbufferEXT;
extern PFNGLDELETERENDERBUFFERSEXTPROC glDeleteRenderbuffersEXT;
extern PFNGLGENRENDERBUFFERSEXTPROC glGenRenderbuffersEXT;
extern PFNGLRENDERBUFFERSTORAGEEXTPROC glRenderbufferStorageEXT;
extern PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC glGetRenderbufferParameterivEXT;
extern PFNGLISFRAMEBUFFEREXTPROC glIsFramebufferEXT;
extern PFNGLBINDFRAMEBUFFEREXTPROC glBindFramebufferEXT;
extern PFNGLDELETEFRAMEBUFFERSEXTPROC glDeleteFramebuffersEXT;
extern PFNGLGENFRAMEBUFFERSEXTPROC glGenFramebuffersEXT;
extern PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC glCheckFramebufferStatusEXT;
extern PFNGLFRAMEBUFFERTEXTURE1DEXTPROC glFramebufferTexture1DEXT;
extern PFNGLFRAMEBUFFERTEXTURE2DEXTPROC glFramebufferTexture2DEXT;
extern PFNGLFRAMEBUFFERTEXTURE3DEXTPROC glFramebufferTexture3DEXT;
extern PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC glFramebufferRenderbufferEXT;
extern PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC glGetFramebufferAttachmentParameterivEXT;
extern PFNGLGENERATEMIPMAPEXTPROC glGenerateMipmapEXT;

// Reads the attribute with the given name (if it exists) and converts it to a float value.
bool getFloatAttribute(xmlNodePtr Element, const char* name, float& Value);

// Like GetFloatAttribute but with a default value in case the attribute does not exist.
float getFloatAttributeDef(xmlNodePtr Element, const char* name, float Default);

// Reads the attribute with the given name (if it exists) and converts it to an integer value.
bool getIntAttribute(xmlNodePtr Element, const char* name, int& Value);

// Like GetIntAttribute but with a default value in case the attribute does not exist.
int getIntAttributeDef(xmlNodePtr Element, const char* name, int Default);

// Reads the attribute with the given name (if it exists) and returns it.
bool getStringAttribute(xmlNodePtr Element, const char* name, string& Value);

// Like GetStringAttribute but with a default value in case the attribute does not exist.
string getStringAttributeDef(xmlNodePtr Element, const char* name, const string Default);

// Reads the attribute with the given name and treats it as color value.
int convertColor(xmlNodePtr Element, const char* name, GLubyte* Color);

// Rounds the given value up to the next power-of-two value.
int roundUpToPowerOf2(int Value);

// Parses the given XML element for a texture definition.
void parseTextureEntry(xmlNodePtr XML, CGCModel* model);

void initializeOpenGLExtensions(void);

/** Helper class to initialize certain OpenGL things. */
class ExtensionInitializer
{
public:
  ExtensionInitializer(void);
};

#endif // __GC_GL_HELPER_H__

