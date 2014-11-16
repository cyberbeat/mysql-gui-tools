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
 * @file myx_gc_model.cpp 
 * @brief Implementation of the model that manages the visual representation in the generic canvas.
 * 
 */

#include "myx_gc_model.h"
#include "myx_gc_texture.h"

// Default values for texturing.
extern const string defaultTextureWrapMode("clamp");     
extern const string defaultTextureMinFilter("nearest"); 
extern const string defaultTextureMagFilter("nearest");      
extern const int defaultTextureDimensions = 2;
extern const string defaultTextureMode("decal");

//----------------- CFigureListener ------------------------------------------------------------------------------------

void CGCModel::CFigureListener::onDestroy(CGCBase* sender)
{
  if (model != NULL)
  {
    if (strcmp(sender->className(), "CFigure") == 0)
    {
      CFigure* figure = static_cast<CFigure*>(sender);
      if (figure != NULL)
        model->removeFigure(figure);
    }
    else
      if (strcmp(sender->className(), "CConnection") == 0)
        model->removeConnection((CConnection*) sender);
  };
}
  
//----------------- CGCModel -------------------------------------------------------------------------------------------

CGCModel::CGCModel(CGenericCanvas* canvas): CGCBase(canvas)
{
  _className = "CGCModel";
  FListener.model = this;
}

//----------------------------------------------------------------------------------------------------------------------

CGCModel::~CGCModel(void)
{
  setDestroying();

  clearConnections();
  clearDecors();
  clearFigures();
  clearLayouts();
  clearStyles();
  clearTextures();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds the given figure to the internal list and registers a listener with it.
 */
void CGCModel::addFigure(CFigure* figure)
{
  figure->addListener(&FListener);
  FFigures.push_back(figure);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes all connections.
 */
void CGCModel::clearConnections(void)
{
  beginUpdate();
  for (CConnections::iterator iterator = FConnections.begin(); iterator != FConnections.end(); ++iterator)
    delete *iterator;
  FConnections.clear();
  endUpdate();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes all decors.
 */
void CGCModel::clearDecors(void)
{
  beginUpdate();
  for (CConnectionDecors::iterator iterator = FConnectionDecors.begin(); iterator != FConnectionDecors.end(); ++iterator)
    delete iterator->second;
  FConnectionDecors.clear();
  endUpdate();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Clears the model, that is, the figures defined in this model.
 */
void CGCModel::clearFigures(void)
{
  beginUpdate();
  for (CFigureList::iterator iterator = FFigures.begin(); iterator != FFigures.end(); ++iterator)
    delete *iterator;
  FFigures.clear();
  endUpdate();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Clears the figure template list (layouts).
 *
 * @note: Existing figures are not concerned by deleting the template list.
 */
void CGCModel::clearLayouts(void)
{
  beginUpdate();
  for (CLayoutList::iterator iterator = FLayouts.begin(); iterator != FLayouts.end(); ++iterator)
    delete iterator->second;
  FLayouts.clear();
  endUpdate();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Clears all defined styles.
 *
 * @note: Deleting all styles means to leave all figures without visual representation.
 */
void CGCModel::clearStyles(void)
{
  beginUpdate();
  for (CStyleList::iterator iterator = FStyles.begin(); iterator != FStyles.end(); ++iterator)
    delete iterator->second;
  FStyles.clear();
  endUpdate();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Clears all defined textures.
 *
 * @note: Deleting all textures means to leave all styles without their assigned textures.
 */
void CGCModel::clearTextures(void)
{
  beginUpdate();
  for (CTextures::iterator iterator = FTextures.begin(); iterator != FTextures.end(); ++iterator)
    delete iterator->second;
  FTextures.clear();
  endUpdate();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a connection object for a logical connection between both end points. This connection, like a figure,
 * cannot be displayed on its own. It needs a connection instance, which is placed on a connection layer in a view.
 * Connections aren't directed so there is no start point. They just have two end points.
 *
 * @param decor The decor describing certain abilities of the connection.
 * @param endPoint1 One endpoint of the connection.
 * @param endPoint1 Another endpoint of the connection
 */
CConnection* CGCModel::createConnection(wstring type, wstring layoutClass, CFigure* endPoint1, CFigure* endPoint2)
{
  // createLayout only creates a new layout entry if it does not yet exist.
  CConnectionDecor* decor = createDecor(type, layoutClass);
  CConnection* result = new CConnection(canvas(), decor, endPoint1, endPoint2);
  result->addListener(&FListener);
  FConnections.push_back(result);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates and returns the decor entry for the given type.
 * If the decor entry already exists then no new instance is created, but the existing one is returned.
 *
 * @param type The type to which the decor is associated.
 * @param layoutClass The layout class to be used (e.g. icon, detail). Also application defined.
 * @return The corresponding decor entry.
 */
CConnectionDecor* CGCModel::createDecor(wstring type, wstring layoutClass)
{
  CConnectionDecor* result = decor(type, layoutClass);

  if (result == NULL)
  {
    result = new CConnectionDecor(this, type, layoutClass);
    FConnectionDecors.insert(CConnectionDecorPair(type, result));
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new figure and puts in into the internal list.
 *
 * @param type The name of the application defined type for which to create the figure.
 * @param layoutClass The layout class to be used (e.g. icon, detail). Also application defined.
 */
CFigure* CGCModel::createFigure(wstring type, wstring layoutClass)
{
  // createLayout only creates a new layout entry if it does not yet exist.
  CFigureTemplate* figureTemplate = createLayout(type, layoutClass);
  CFigure* figure = new CFigure(this, figureTemplate);

  return figure;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates and returns the layout entry for the given type and class.
 * If the layout entry already exists then no new instance is created, but the existing one is returned.
 *
 * @param type The type to which the layout is associated.
 * @param layoutClass An additional criterion specifying a certain layout arrangement. This value is app. specific.
 * @return The corresponding layout entry.
 */
CFigureTemplate* CGCModel::createLayout(wstring type, wstring layoutClass)
{
  CFigureTemplate* result = layout(type, layoutClass);

  if (result == NULL)
  {
    result = new CFigureTemplate(this, type, layoutClass);
    FLayouts.insert(CLayoutPair(type, result));
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new texture entry and adds the entry to the texture list. No image data is loaded yet as this will happen 
 * when the texture is used the first time.
 *
 * @param lodData A list of level identifiers and names of files, which contain the image data for that level.
 * @param id The identifier (name) of the texture.
 * @param wrapH Horizontal wrap mode as string.
 * @param wrapV Vertical wrap mode as string.
 * @param MinificationFilter The filter mode for minification as string.
 * @param MagnificationFilter The filter mode for magnification as string.
 * @param dimensions The number of dimensions of the image data (must be 1 or 2).
 * @param mode The mode how the texture must be applied as string.
 * return The newly created texture object.
 */
CGCTexture* CGCModel::createTexture(const TLODList& lodData, const wstring& id, const string& wrapH, const string& wrapV, 
                                    const string& minificationFilterStr, const string& magnificationFilterStr,
                                    int dimensions, const string& mode)
{
  static map<string, GLenum> mapper;
  map<string, GLenum>::const_iterator iterator;

  // Fill our lookup table first if not yet done.
  if (mapper.size() == 0)
  {
    mapper["clamp"] = GL_CLAMP;
    mapper["clamp-to-border"] = GL_CLAMP_TO_BORDER;
    mapper["clamp-to-edge"] = GL_CLAMP_TO_EDGE;
    mapper["repeat"] = GL_REPEAT;
    mapper["nearest"] = GL_NEAREST;
    mapper["linear"] = GL_LINEAR;
    mapper["nearest-mipmap-nearest"] = GL_NEAREST_MIPMAP_NEAREST;
    mapper["linear-mipmap-nearest"] = GL_LINEAR_MIPMAP_NEAREST;
    mapper["nearest-mipmap-linear"] = GL_NEAREST_MIPMAP_LINEAR;
    mapper["linear-mipmap-linear"] = GL_LINEAR_MIPMAP_LINEAR;
    mapper["decal"] = GL_DECAL;
    mapper["modulate"] = GL_MODULATE;
    mapper["blend"] = GL_BLEND;
    mapper["replace"] = GL_REPLACE;
  };

  GLenum wrapModeS = GL_CLAMP;
  iterator = mapper.find(wrapH);
  if (iterator != mapper.end())
    wrapModeS = iterator->second;

  GLenum wrapModeT = GL_CLAMP;
  iterator = mapper.find(wrapV);
  if (iterator != mapper.end())
    wrapModeT = iterator->second;

  GLenum minFilter = GL_NEAREST;
  iterator = mapper.find(minificationFilterStr);
  if (iterator != mapper.end())
    minFilter = iterator->second;

  GLenum magFilter = GL_NEAREST;
  iterator = mapper.find(magnificationFilterStr);
  if (iterator != mapper.end())
	magFilter = iterator->second;

  GLenum textureMode = GL_DECAL;
  iterator = mapper.find(mode);
  if (iterator != mapper.end())
	textureMode = iterator->second;

  TLODList prefixedLodData;
  for (TLODList::const_iterator path= lodData.begin(); path != lodData.end(); ++path)
  {
	wstring finalPath;
	if (path->substr(0, 2) == L"./")
	  finalPath = FTexturePath + path->substr(2);
	else
	  // If an absolute path is given then do not apply the texture image path.
	  if ((*path)[0] == '/' || (path->size() > 2 && (*path)[1] == ':'))
		finalPath = *path;
	  else
		finalPath = FTexturePath + *path;

	prefixedLodData.push_back(finalPath);
  }
  CGCTexture* result = new CGCTexture(prefixedLodData, id, wrapModeS, wrapModeT, minFilter, magFilter, dimensions, textureMode);
  FTextures[id] = result;

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Same as the decor(wstring, wstring) but for UTF-8 encoded strings.
 */
CConnectionDecor* CGCModel::decor(const char* type, const char* layoutClass)
{
  if (layoutClass == NULL)
    return decor(utf8ToUtf16(type), L"");
  else
    return decor(utf8ToUtf16(type), utf8ToUtf16(layoutClass));
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the decor entry for the given type if it exists.
 *
 * @param type The type to which the decor is associated.
 * @param layoutClass An additional criterion specifying a certain decor arrangement. This value is app. specific.
 * @result The corresponding decor entry if it exists or NULL if not.
 */
CConnectionDecor* CGCModel::decor(wstring type, wstring layoutClass)
{
  CConnectionDecor* result = NULL;
  pair<CConnectionDecors::iterator, CConnectionDecors::iterator> range;

  range = FConnectionDecors.equal_range(type);
  CConnectionDecors::iterator start = range.first;
  CConnectionDecors::iterator stop = range.second;

  // If no layout class is given then use the first one that was defined for the given type.
  if (start != stop && layoutClass.size() == 0)
    result = start->second;
  else
    while (start != stop)
    {
      if (start->second->layoutClass() == layoutClass)
      {
        result = start->second;
        break;
	  };
	  ++start;
	};

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Same as the layout(wstring, wstring) but for UTF-8 encoded strings.
 */
CFigureTemplate* CGCModel::layout(const char* type, const char* layoutClass)
{
  if (layoutClass == NULL)
    return layout(utf8ToUtf16(type), L"");
  else
    return layout(utf8ToUtf16(type), utf8ToUtf16(layoutClass));
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the layout entry for the given type if it exists.
 *
 * @param type The type to which the style is associated.
 * @param layoutClass An additional criterion specifying a certain layout arrangement. This value is app. specific.
 * @return The corresponding layout entry if it exists or NULL if not.
 */
CFigureTemplate* CGCModel::layout(wstring type, wstring layoutClass)
{
  CFigureTemplate* result = NULL;
  pair<CLayoutList::iterator, CLayoutList::iterator> range;

  range = FLayouts.equal_range(type);
  CLayoutList::iterator start = range.first;
  CLayoutList::iterator stop = range.second;

  // If no layout class is given then use the first one that was defined for the given type.
  if (start!= stop && layoutClass.size() == 0)
    result = start->second;
  else
    while (start != stop)
    {
      if (start->second->layoutClass() == layoutClass)
      {
        result = start->second;
        break;
	  };
	  ++start;
	};

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by path. The path syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property to return.
 * @param index If the property is a list then this is the index into that list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CGCModel::propertyGet(const char* name, unsigned int index)
{
  TGCVariant result;

  switch (getContainerID(name))
  {
    case GC_CONTAINER_UNKNOWN:
      {
        switch (getPropertyID(name))
        {
          case GC_PROPERTY_NAME:
            {
              result = "canvas model";
              break;
			};
          case GC_PROPERTY_DESCRIPTION:
            {
              result = "The central model of the canvas containing all styles, layouts and figures.";
              break;
            };
          case GC_PROPERTY_OWNER:
            {
              result = canvas();
              break;
            };
        };

        break;
      };
    case GC_CONTAINER_FIGURES:
      {
        if (index < FFigures.size())
          result = FFigures[index];

        break;
      };
    case GC_CONTAINER_STYLES:
      {
        if (index < FStyles.size())
        {
          CStyleList::iterator iterator = FStyles.begin();
          while (index > 0)
          {
            ++iterator;
            --index;
          };
          result = iterator->second;
        };

        break;
      };
    case GC_CONTAINER_LAYOUTS:
      {
        if (index < FLayouts.size())
        {
          CLayoutList::iterator iterator = FLayouts.begin();
		  while (index > 0)
          {
            ++iterator;
            --index;
          };
          result = iterator->second;
        };

        break;
      };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the value of the given property, which must be a simple property.
 *
 * @param name The name of the property.
 * @param index If the property is a list then this is the index into that list.
 * @param Value The new value of the property. Automatic conversion is performed where possible.
 */
void CGCModel::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  // No properties to set at this point.
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given connection from the model. The connection itself is not destroyed.
 *
 * @param connection The connection to be removed from this model.
 */
void CGCModel::removeConnection(CConnection* connection)
{
  if (!destroying())
  {
	for (CConnections::iterator iterator = FConnections.begin(); iterator != FConnections.end(); ++iterator)
	  if (*iterator == connection)
	  {
		connection->removeListener(&FListener);
		FConnections.erase(iterator);
		break;
	  };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given figure from the internal figures list.
 *
 * @param figure The figure to be removed.
 */
void CGCModel::removeFigure(CFigure* figure)
{
  if (!updating())
  {
    for (CFigureList::iterator iterator = FFigures.begin(); iterator != FFigures.end(); ++iterator)
    {
      if (*iterator == figure)
      {
        (*iterator)->removeListener(&FListener);
        FFigures.erase(iterator);
        break;
      };
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

void CGCModel::removeStyle(wstring ID)
{
  CStyleList::iterator iterator = FStyles.find(ID);
  if (iterator != FStyles.end())
  {
    delete iterator->second;
    FStyles.erase(iterator);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called by the canvas to set a new path from which textures are loaded.
 *
 * @param path The new texture path (UTF-8 encoded).
 */
void CGCModel::setTexturePath(const string& path)
{
  if (path.size() == 0)
	FTexturePath.clear();
  else
  {
	// Make sure the path ends with a slash.
	FTexturePath = utf8ToUtf16(path);
	if (FTexturePath[FTexturePath.size() - 1] != '/')
	  FTexturePath += '/';
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the style entry for the given identifier. If there is no style with that name one is created.
 *
 * @param ID The identification of the style.
 * @return The corresponding style entry, can be NULL if ID is empty.
 */
CGCStyle* CGCModel::style(const wstring &ID)
{
  CGCStyle* result = NULL;

  if (ID.size() > 0)
  {
    CStyleList::iterator iterator = FStyles.find(ID);
    if (iterator == FStyles.end())
    {
      result = new CGCStyle(this, ID);
      FStyles[ID] = result;
    }
    else
      result = iterator->second;
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns 1 if a style with the give name exists.
 *
 * @param ID The identification of the style.
 * @return 1 if the style exists, 0 if it does not exist
 */
int CGCModel::styleExists(const wstring &ID)
{
  int result = 0;

  if (ID.size() > 0)
  {
    CStyleList::iterator iterator = FStyles.find(ID);
    if (iterator != FStyles.end())
      result = 1;
  }

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Looks through the textures and tries to find one with the given name.
 *
 * @param name The name of the texture to find.
 * @return The texture if there is one with the given name or NULL.
 */
CGCTexture* CGCModel::texture(const wstring& name)
{
  CTextures::iterator iterator = FTextures.find(name);
  if (iterator == FTextures.end())
    return NULL;
  else
    return iterator->second;
}

//----------------------------------------------------------------------------------------------------------------------

