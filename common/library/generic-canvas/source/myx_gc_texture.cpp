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
 * @file myx_gc_texture.cpp 
 * @brief Implementation of a texture class.
 * 
 */

#include "myx_gc_texture.h"
#include "myx_gc_gl_helper.h"

//----------------- CGCTexture -----------------------------------------------------------------------------------------

CGCTexture::CGCTexture(const TLODList& lodData, const wstring& id, GLenum wrapModeS, GLenum wrapModeT, 
                       GLenum minFilter, GLenum magFilter, int dimensions, GLenum textureMode)
{
  FName = id;
  FLODList = lodData;
  FWrapModeS = wrapModeS;
  FWrapModeT = wrapModeT;
  FMinFilter = minFilter;
  FMagFilter = magFilter;
  FTarget = dimensions == 1 ? GL_TEXTURE_1D : GL_TEXTURE_2D;
  FMode = textureMode;
  FHandle = 0;
}

//----------------------------------------------------------------------------------------------------------------------

CGCTexture::~CGCTexture(void)
{
  invalidateHandle();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Delay loads texture data. Called from activateTexture, that is, when the texture is used the first time.
 * All level-of-detail texture data is loaded here and uploaded to OpenGL depending on the number of images given and
 * what is set for the minification filter.
 */
void CGCTexture::loadTexture(void)
{
  TImage* image;
  unsigned char* buffer;

  if ((FMinFilter == GL_NEAREST) || (FMinFilter == GL_LINEAR) || (FLODList.size() < 2))
  {
    // No mip-mapping.
    if (FLODList.size() > 0)
    {
      image = loadTextureImage(FLODList[0], buffer);
      if (image != NULL)
      {
        if (FTarget == GL_TEXTURE_1D)
        {
          if ((FMinFilter == GL_NEAREST) || (FMinFilter == GL_LINEAR))
            glTexImage1D(GL_TEXTURE_1D, 0, image->format, image->width, 0, image->format, GL_UNSIGNED_BYTE, buffer);
          else
            gluBuild1DMipmaps(GL_TEXTURE_1D, image->format, image->width, image->format, GL_UNSIGNED_BYTE, buffer);
        }
        else
        {
          if ((FMinFilter == GL_NEAREST) || (FMinFilter == GL_LINEAR))
            glTexImage2D(GL_TEXTURE_2D, 0, image->format, image->width, image->height, 0, image->format, 
              GL_UNSIGNED_BYTE, buffer);
          else
            gluBuild2DMipmaps(GL_TEXTURE_2D, image->format, image->width, image->height, image->format, 
              GL_UNSIGNED_BYTE, buffer);
        };

        if (buffer != image->data)
          free(buffer);
        freeImage(image);
      }
      else
        g_message("Can't load '%s'", utf16ToANSI(FLODList[0]).c_str());
    };
  }
  else
  {
    TLODList::size_type i;

    // Prepare mip-mapping pyramid using the size of the LOD 0 texture. All files must have the same color format!
    image = loadTextureImage(FLODList[0], buffer);
    if (image != NULL)
    {
      GLenum format = image->format;
      gluBuild2DMipmaps(GL_TEXTURE_2D, format, image->width, image->height, format, GL_UNSIGNED_BYTE, buffer);
      freeImage(image);

      for (i = 0; i < FLODList.size(); i++)
      {
        image = loadTextureImage(FLODList[i], buffer);
        if (image != NULL && image->format == format)
        {
          if (FTarget == GL_TEXTURE_1D)
            glTexImage1D(GL_TEXTURE_1D, (int) i, format, image->width, 0, format, GL_UNSIGNED_BYTE, buffer);
          else
            glTexImage2D(GL_TEXTURE_2D, (int) i, format, image->width, image->height, 0, format, GL_UNSIGNED_BYTE, buffer);
          if (buffer != image->data)
            free(buffer);
          freeImage(image);
        }
        else
          g_message("Can't load '%s'", utf16ToANSI(FLODList[i]).c_str());
        };
    }
    else
      g_message("Can't load '%s'", utf16ToANSI(FLODList[0]).c_str());
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Loads the image data referenced by name and returns it. If the either size of the image is not a power of 2 then
 * the image is scaled up so that it becomes this size.
 *
 * @param name The name of the file to load (ANSI encoded).
 * @param buffer A variable that gets either the the address of the actual image data (TImage->data) or a new memory
 *               reference if the image must be scaled. The caller is responsible to free this buffer if it differs 
 *               from TImage->data.
 * @return An image structure containing the actual image data. The caller is responsible to free this stucture via 
 *         freeImage if it is non null. If the image could not be loaded then the result is NULL and the content of 
 *         buffer is not touched.
 */
TImage* CGCTexture::loadTextureImage(const wstring& name, unsigned char*& buffer)
{
  TImage* image = loadPNG(name);

  if (image != NULL)
  {
		int actualWidth = roundUpToPowerOf2(image->width);
    int actualHeight = roundUpToPowerOf2(image->height);

    switch (image->colorType)
    {
      case COLOR_TYPE_PALETTE:
        {
          image->format = GL_COLOR_INDEX;
          break;
        };
      case COLOR_TYPE_GRAY:
        {
          image->format = GL_LUMINANCE;
          break;
        };
      case COLOR_TYPE_GRAY_ALPHA:
        {
          image->format = GL_LUMINANCE_ALPHA;
          break;
        };
      case COLOR_TYPE_RGB:
        {
          image->format = GL_RGB;
          break;
        };
      case COLOR_TYPE_RGB_ALPHA:
        {
          image->format = GL_RGBA;
          break;
        };
    };

    if (actualWidth != image->width || actualHeight != image->height)
    {
      buffer = (unsigned char*) malloc(actualWidth * actualHeight * image->channels);
      gluScaleImage(image->format, image->width, image->height, GL_UNSIGNED_BYTE, image->data, actualWidth, actualHeight, 
        GL_UNSIGNED_BYTE, buffer);
      image->width = actualWidth;
      image->height = actualHeight;
    }
    else
      buffer = image->data;
  };

  return image;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Activates this texture in OpenGL so all following vertex definitions are textured using this texture.
 * If the texture has not been loaded yet it will be done now. Additionally, texture mode is enabled in OpenGL.
 */
void CGCTexture::activateTexture(void)
{                        
  validateHandle();

  glEnable(FTarget);
  glBindTexture(FTarget, FHandle); 
  
  glTexParameteri(FTarget, GL_TEXTURE_WRAP_S, FWrapModeS);
  if (FTarget == GL_TEXTURE_2D)
    glTexParameteri(FTarget, GL_TEXTURE_WRAP_T, FWrapModeT);
	glTexParameteri(FTarget, GL_TEXTURE_MIN_FILTER, FMinFilter);
	glTexParameteri(FTarget, GL_TEXTURE_MAG_FILTER, FMagFilter);

  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, FMode);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Deactivates this texture and the texture mode in OpenGL.
 */
void CGCTexture::deactivateTexture(void)
{
  glDisable(FTarget);
  glBindTexture(FTarget, 0);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Destroys the texture's handle. This is required e.g. when the associated rendering context changes.
 */
void CGCTexture::invalidateHandle(void)
{
  if (FHandle != 0)
  {
    glBindTexture(FTarget, 0);
    glDeleteTextures(1, &FHandle);
    FHandle = 0;
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Checks if the texture's handle (texture object) has been created already and creates it if not yet done.
 */
void CGCTexture::validateHandle(void)
{
  if (FHandle == 0)
  {
    glGenTextures(1, &FHandle);
    glBindTexture(FTarget, FHandle); 
    loadTexture();
    glBindTexture(FTarget, 0);
  };
}

//----------------------------------------------------------------------------------------------------------------------

