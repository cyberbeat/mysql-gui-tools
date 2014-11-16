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
 * @file myx_gc_texture.h 
 * @brief Implementation of a texture class.
 * 
 */

#ifndef __GC_TEXTURE_H__
#define __GC_TEXTURE_H__

#include "myx_gc_canvas.h"

//----------------------------------------------------------------------------------------------------------------------

// CGCTexture encapsulates a png image used to texture a figure in Generic Canvas. It loads the image data
// and manages it as well as the OpenGL properties for it.
// Note: A texture can be shared amongst several figures including its properties (e.g. minification filter).
class CGCTexture
{
private:
  TLODList FLODList;         // The list of file names and their associated LOD.
  wstring FName;              // The identification of this texture.
  GLenum FWrapModeS;         // Specifies how textures are handled at the edges of a figure (horizontal direction).
  GLenum FWrapModeT;         // Specifies how textures are handled at the edges of a figure (vertical direction).
  GLenum FMinFilter;         // Specifies how stretching of texture data is performed when the actual screen image is smaller than the image data.
  GLenum FMagFilter;         // Specifies how stretching of texture data is performed when the actual screen image is larger than the image data.
  GLenum FTarget;            // One-dimensional or two-dimensional textures.
  GLuint FHandle;            // The OpenGL handle for this texture.
  GLint FMode;               // The mode how to apply the texture to the surface.
protected:
  void loadTexture(void);
  TImage* loadTextureImage(const wstring& name, unsigned char*& Buffer);
public:
  CGCTexture(const TLODList& lodData, const wstring& id, GLenum wrapModeS, GLenum wrapModeT, GLenum minFilter, 
    GLenum magFilter, int dimensions, GLenum textureMode);
  ~CGCTexture(void);

  void activateTexture(void);
  void deactivateTexture(void);
  void invalidateHandle(void);
  void validateHandle(void);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // #ifdef __GC_TEXTURE_H__
