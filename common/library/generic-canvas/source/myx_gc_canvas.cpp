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
 * @file myx_gc_canvas.cpp 
 * @brief Generic canvas main class and entry point.
 * 
 */


#include "myx_gc_canvas.h"
#include "myx_gc_gl_helper.h"
#include "gl2ps/gl2ps.h"

//----------------------------------------------------------------------------------------------------------------------

// Factory function for a canvas.
GENERIC_CANVAS_API CGenericCanvas* CreateGenericCanvas(GCContext Context, char* name)
{
  return new CGenericCanvas(Context, utf8ToUtf16(name));
}

//----------------- CCanvasListener ------------------------------------------------------------------------------------

void CGenericCanvas::CCanvasListener::onAction(CGCBase* sender, CGCBase* origin, TAction** action)
{
  canvas->action(origin, action);
}

//----------------------------------------------------------------------------------------------------------------------

void CGenericCanvas::CCanvasListener::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  canvas->change(origin, reason);
}

//----------------------------------------------------------------------------------------------------------------------

void CGenericCanvas::CCanvasListener::onError(CGCBase* sender, CGCBase* origin, const char* message)
{
  canvas->error(origin, message);
}

//----------------- CGenericCanvas -------------------------------------------------------------------------------------

CGenericCanvas::CGenericCanvas(GCContext Context, wstring name): CGCBase(NULL)
{ 
  FCanvas = this;
  _className = "CGenericCanvas";
  FName = name;
  FContext = Context;
  FCurrentView = NULL;
  FStates = 0;

#ifdef _WINDOWS
  InitializeCriticalSection(&FLock);
#else
  g_static_rec_mutex_init(&FLock);
#endif

  FListener.canvas = this;
  FModel = new CGCModel(this);
  FModel->addListener(&FListener);
  FAnimationManager = new CAnimationManager(this);

  determineExtensions();
  initializeOpenGLExtensions();

  // Set a new lock on the font manager (and create it by the way if not yet done).
  lockFontManager();
}

//----------------------------------------------------------------------------------------------------------------------

CGenericCanvas::~CGenericCanvas(void)
{
  beginUpdate();
  disableEvents();

  // Release all managed data. Start with the visual parts to avoid frequent change events between model and other elements.
  for (CViews::iterator iterator = FViews.begin(); iterator != FViews.end(); ++iterator)
    delete *iterator;
  for (CLayers::iterator iterator = FLayers.begin(); iterator != FLayers.end(); ++iterator)
    delete *iterator;

  delete FModel;
  delete FAnimationManager;

  // Release the lock we placed in the constructor.
  // If this is the last GC instance then also the font manager can be released.
  unlockFontManager();
  endUpdate();

#ifdef _WINDOWS
  DeleteCriticalSection(&FLock);
#else
  g_static_rec_mutex_free(&FLock);
#endif
}

//----------------------------------------------------------------------------------------------------------------------

void CGenericCanvas::clearBuffers(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines whether certain OpenGL extension are supported.
 */
void CGenericCanvas::determineExtensions(void)
{
  FExtensions.push_back(false); // GC_OE_MULTISAMPLING
  FExtensions.push_back(false); // GC_OE_CONVOLUTION
  FExtensions.push_back(false); // GC_OE_HISTOGRAM
  FExtensions.push_back(false); // GC_OE_TEXTURE3D
  FExtensions.push_back(false); // GC_OE_FRAME_BUFFER_OBJECTS

  const GLubyte* extString = glGetString(GL_EXTENSIONS);
  if (extString != NULL)
  {
    if (strstr((char*) extString, "GL_EXT_convolution") != NULL)
      FExtensions[GC_OE_CONVOLUTION] = true;
    if (strstr((char*) extString, "GL_EXT_histogram") != NULL)
      FExtensions[GC_OE_HISTOGRAM] = true;
    if (strstr((char*) extString, "GL_EXT_texture3D") != NULL)
      FExtensions[GC_OE_TEXTURE3D] = true;
    if (strstr((char*) extString, "GL_ARB_multisample") != NULL)
      FExtensions[GC_OE_MULTISAMPLING] = true;
    if (strstr((char*) extString, "GL_EXT_framebuffer_object") != NULL)
      FExtensions[GC_OE_FRAME_BUFFER_OBJECTS] = true;
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Aquires the canvas lock used to synchronize threads.
 */
void CGenericCanvas::lock(void)
{
#ifdef _WINDOWS
  EnterCriticalSection(&FLock);
#else
  g_static_rec_mutex_lock(&FLock);
#endif
}

//----------------------------------------------------------------------------------------------------------------------

void CGenericCanvas::unlock(void)
{
#ifdef _WINDOWS
  LeaveCriticalSection(&FLock);
#else
  g_static_rec_mutex_unlock(&FLock);
#endif
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief Adds the given layer at the end of the layer list. The new layer becomes the top layer in the view then.
 * @param layer The layer to add.
 */
void CGenericCanvas::addLayer(CLayer* layer)
{
  FLayers.push_back(layer);
  change(this, GC_CHANGE_CANVAS_ADD_LAYER);
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Reads the layout info stored in the given (XML) file and creates figure templates.
 * Existing templates remain in where they are but are replaced if a new definition with an existing name is found.
 *
 * @param filename The name of the file to load.
 * @return Returns GC_NO_ERROR if everything was ok, otherwise an error code.
 */
TGCError CGenericCanvas::addLayoutsFromFile(const char* filename)
{
  TGCError result = GC_NO_ERROR;
  xmlNodePtr root, current;
  CFigureParser parser(this);
  parser.addListener(&FListener);

  string currentDir = getCurrentDir();
  xmlDocPtr document = xmlParseFile(utf8ToANSI(string(filename)).c_str());

  if (document == NULL)
    return GC_XML_PARSE_ERROR;

  root = xmlDocGetRootElement(document);

  if (root == NULL)
  {
    xmlFreeDoc(document);
    error(this, "XML Error: Template file is empty.");
    return GC_XML_EMPTY_DOCUMENT;
  }
  
  if (!XML_IS(root, "gc-layouts"))
  {
    xmlFreeDoc(document);
    error(this, "XML Error: Template file invalid.");
    return GC_XML_INVALID_DOCUMENT;
  }

  // Switch to the directory of the given file. This is necessary to make relative file names working.
  string path = extractFilePath(filename);
  setCurrentDir(path);

  // Parse description elements.
  glMatrixMode(GL_MODELVIEW);
  current = root->children;
  while (current != NULL)
  {
    // Be flexible, ignore any unknown layout entries.
    if (XML_IS(current, "layout-definition"))
      parser.parseLayoutDefinition(current, FModel);
    else
      if (XML_IS(current, "connection-definition"))
        parser.parseDecorDefinition(current, FModel);

    current = current->next;
  }

  setCurrentDir(currentDir);
  xmlFreeDoc(document);

  parser.removeListener(&FListener);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Reads the style template info stored in the given (XML) file and creates style templates (OpenGL display lists).
 * Existing templates remain where they are but are replaced if a new definition with an existing name is found.
 *
 * @param filename The name of the file to load.
 * @param variables A string containing line break separated name=value entries (note: no white spaces around the equal
 *                  sign or they will become part of the value and name strings, respectively.
 * @return Returns GC_NO_ERROR if everything was ok, otherwise an error code.
 */
TGCError CGenericCanvas::addStylesFromFile(const char* filename, const char* variables)
{
  map<wstring, wstring> variablesMap;

  if (variables != NULL)
  {
    StringTokenizer tokenizer(variables, "\r\n", true);
    while (tokenizer.hasMoreTokens())
    {
      wstring token = tokenizer.nextToken();
      size_t position = token.find(L"=");
      if (position != token.npos)
      {
        wstring name = token.substr(0, position);
        wstring value = token.substr(position + 1);
        variablesMap[name] = value;
      };
    };
  };

  return addStylesFromFile(filename, variablesMap);
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Reads the style template info stored in the given (XML) file and creates style templates (OpenGL display lists).
 * Existing templates remain where they are but are replaced if a new definition with an existing name is found.
 *
 * @param filename The name of the file to load.
 * @param variables A set of name/value pairs to be replaced in the xml text.
 * @return Returns GC_NO_ERROR if everything was ok, otherwise an error code.
 */
TGCError CGenericCanvas::addStylesFromFile(const char* filename, map<wstring, wstring>& variables)
{
  TGCError result = GC_NO_ERROR;
  xmlNodePtr root, current;
  CSVGParser parser;
  xmlDocPtr document = NULL;
  string currentDir = getCurrentDir();
  
  if (!variables.empty())
  {
    char *data;
    size_t length;

    data= preprocessFile(filename, variables, length);
    if (data)
    {
      document = xmlParseMemory(data, (int) length);
      free(data);
    }
  }
  else
    document = xmlParseFile(utf8ToANSI(filename).c_str());

  if (document == NULL)
    return GC_XML_PARSE_ERROR;                       

  root = xmlDocGetRootElement(document);

  if (root == NULL)
  {
    xmlFreeDoc(document);
    error(this, "XML Error: Template file is empty.");
    return GC_XML_EMPTY_DOCUMENT;
  }
  
  if (!XML_IS(root, "gc-styles"))
  {
    xmlFreeDoc(document);
    error(this, "XML Error: Template file invalid.");
    return GC_XML_INVALID_DOCUMENT;
  }

  // Switch to the directory of the given file. This is necessary to make relative file names working.
  string path = extractFilePath(filename);
  setCurrentDir(path);

  // Parse description elements.
  glMatrixMode(GL_MODELVIEW);
  current = root->children;
  while (current != NULL)
  {
    // Be flexible, ignore any unknown layout entries.
    if (XML_IS(current, "style-definition"))
    {
      parser.parseDefinition(current, FModel);
    }
    else
      if (XML_IS(current, "texture"))
      {
        // Adds a new texture to the texture list.
        parseTextureEntry(current, FModel);
        checkError();
      };
    current = current->next;
  }

  setCurrentDir(currentDir);
  xmlFreeDoc(document);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

TGCError CGenericCanvas::addStylesFromFile(const char* filename, map<string, string>& variables)
{
  map<wstring, wstring> wide_variables;
  for (map<string, string>::const_iterator iterator = variables.begin(); iterator != variables.end(); ++iterator)
    wide_variables[utf8ToUtf16(iterator->first)] = utf8ToUtf16(iterator->second);

  return addStylesFromFile(filename, wide_variables);
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Takes the given definition and creates a generic canvas style from it. The format in the definition is the same
 * format as in the XML file that can be used to load styles from disk. Only a single <style-definition></style-definition>
 * or <texture></texture> entry must be in the definition. The caller is responsible to ensure the currently active folder 
 * is set so that eventually used relative paths in the definition work as expected.
 * If a style with the ID used in the definition exists already then it will be changed to the new definition.
 *
 * @param definition An UTF-8 encoded <style-definition></style-definition>.
 * @return Returns GC_NO_ERROR if everything was ok, otherwise an error code.
 */
TGCError CGenericCanvas::addStyleFromDefinition(const char* definition)
{
  TGCError result = GC_NO_ERROR;
  xmlNodePtr root;
  CSVGParser parser;

  string documentText = string("<?xml version=\"1.0\" encoding=\"UTF-8\"?>") + definition;
  xmlDocPtr document = xmlParseMemory(documentText.c_str(), (int) documentText.size());

  if (document == NULL)
    return GC_XML_PARSE_ERROR;                       

  root = xmlDocGetRootElement(document);

  if (root == NULL)
  {
    xmlFreeDoc(document);
    error(this, "XML Error: Definition is empty.");
    return GC_XML_EMPTY_DOCUMENT;
  }
  
  if (!XML_IS(root, "style-definition"))
  {
    xmlFreeDoc(document);
    error(this, "XML Error: Definition is not invalid.");
    return GC_XML_INVALID_DOCUMENT;
  }

  glMatrixMode(GL_MODELVIEW);
  if (XML_IS(root, "style-definition"))
  {
    parser.parseDefinition(root, FModel);
  }
  else
    if (XML_IS(root, "texture"))
    {
      // Adds a new texture to the texture list.
      parseTextureEntry(root, FModel);
      checkError();
    };

  xmlFreeDoc(document);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Checks if there is an OpenGL error registered and triggers the error method if so.
 */
void CGenericCanvas::checkError(void)
{
  GLenum OGLError = glGetError();
  if (OGLError != GL_NO_ERROR)
  {
    char buffer[1000];
    const GLubyte* message = gluErrorString(OGLError);

    #ifdef _WINDOWS
      // Microsoft has confirmed that there is no guaranteed NULL termination (see MSDN for details).
      // Hence we better add one.
      _snprintf(buffer, sizeof(buffer), "OpenGL error encountered: %s (0x%x).\0", message, (int) OGLError);
    #else
      snprintf(buffer, sizeof(buffer), "OpenGL error encountered: %s  (0x%x).", message, (int) OGLError);
    #endif // #ifdef _WINDOWS
    error(this, buffer);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes all GC content.
 */
void CGenericCanvas::clearContent(void)
{
  beginUpdate();
  FModel->clearFigures();
  endUpdate();

  change(this, GC_CHANGE_CANVAS_CLEAR_CONTENT);
  refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes all layout info.
 */
void CGenericCanvas::clearLayouts(void)
{
  beginUpdate();
  FModel->clearLayouts();
  endUpdate();

  change(this, GC_CHANGE_CANVAS_CLEAR_LAYOUTS);
  refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Causes the model to clear its style definitions.
 */
void CGenericCanvas::clearStyles(void)
{
  beginUpdate();
  FModel->clearStyles();
  endUpdate();

  change(this, GC_CHANGE_CANVAS_CLEAR_STYLES);
  refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Interface function for the model's createConnection function. Read there for details.
 */
CConnection* CGenericCanvas::createConnection(const char* type, const char* layoutClass, CFigure* endPoint1, 
                                              CFigure* endPoint2)
{
  if (layoutClass == NULL)
    return FModel->createConnection(utf8ToUtf16(type), L"", endPoint1, endPoint2);
  else
    return FModel->createConnection(utf8ToUtf16(type), utf8ToUtf16(layoutClass), endPoint1, endPoint2);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new figure in the model and returns it.
 *
 * @param type The type of figure that is requested.
 * @param layoutClass The specialization of the figure for a particular layout (can be NULL).
 * @result The new figure instance.
 */
CFigure* CGenericCanvas::createFigure(const char* type, const char* layoutClass)
{
  if (layoutClass == NULL)
    return FModel->createFigure(utf8ToUtf16(type), L"");
  else
    return FModel->createFigure(utf8ToUtf16(type), utf8ToUtf16(layoutClass));
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new layer with the given name and returns it to the caller. The new layer is added to this canvas.
 *
 * @param name The layer identification, encoded in UTF-8.
 * @param AddToCurrentView If true then the new layer will be added to the current view (if there is any).
 * @return The new layer.
 */
CLayer* CGenericCanvas::createLayer(const char* name, bool AddToCurrentView)
{
  CLayer* layer = new CLayer(name, this);
  addLayer(layer);
  if (FCurrentView != NULL && AddToCurrentView)
    FCurrentView->addLayer(layer);

  return layer;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new view in this canvas, adds it to the internal list and returns it to the caller. If there is 
 * currently no current view set then the new view becomes the current one.
 *
 * @param name The identification of the view (encoded in UTF-8). Should be unique.
 * @return The new view.
 */
CGCView* CGenericCanvas::createView(const char* name)
{
  CGCView* View = new CGCView(this, name);
  FViews.push_back(View);
  View->addListener(&FListener);

  change(View, GC_CHANGE_CANVAS_ADD_VIEW);

  if (FCurrentView == NULL)
    currentViewSet(View);

  return View;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the currently active view.
 *
 * @return The currently active view.
 */
CGCView* CGenericCanvas::currentViewGet(void)
{
  return FCurrentView;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the currently active view.
 *
 * @param View The new view to activate.
 */
void CGenericCanvas::currentViewSet(CGCView* View)
{
  if (FCurrentView != View)
  {
	FCurrentView = View;
	if (FCurrentView != NULL)
	  FStates |= GC_STATE_PENDING_ACTIVATION;
	else
	  FStates &= ~GC_STATE_PENDING_ACTIVATION;
	change(FCurrentView, GC_CHANGE_CANVAS_SWITCH_VIEW);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current focus state of the canvas.
 *
 * @result True if the canvas is currently focused else false.
 */
bool CGenericCanvas::focusedGet(void)
{
  return FFocused;
}


//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the current focus state of the canvas.
 *
 * @param isFocused The new focus state.
 */
void CGenericCanvas::focusedSet(bool isFocused)
{
  if (FFocused != isFocused)
  {
    FFocused = isFocused;
    refresh();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the first layer with the given name from the layers collection.
 *
 * @param name The name of the layer to return.
 * @return The first found layer with the given name or NULL if no layer could be found.
 */
CLayer* CGenericCanvas::layerByName(const char* name)
{
  CLayer* result = NULL;

  wstring S = utf8ToUtf16(name);
  for (CLayers::const_iterator iterator = FLayers.begin(); iterator != FLayers.end(); ++iterator)
  {
    CLayer* layer = *iterator;
    if (layer->name() == S)
    {
      result = layer;
      break;
    };
  };
  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by path. The path syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property.
 * @param index If the property is a list then this is the index into that list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CGenericCanvas::propertyGet(const char* name, unsigned int index)
{
  // TODO: check if all properties are properly handled.

  TGCVariant result;

  switch (getContainerID(name))
  {
    case GC_CONTAINER_UNKNOWN:
      {
        switch (getPropertyID(name))
        {
          case GC_PROPERTY_NAME:
            {
              result = utf16ToUtf8(FName);
              
              break;
            };
          case GC_PROPERTY_DESCRIPTION:
            {
              result = "A generic 2D canvas based on OpenGL.";
              
              break;
            };
          case GC_PROPERTY_OWNER:
            {
              // There is no owner for the canvas in the canvas library itself.
              // However we recognize the request for it by returning NULL
              // instead of "unknown property".
              result = 0;

              break;
            };
        };
        break;
      };
    case GC_CONTAINER_LAYERS:
      {
        if (index < FLayers.size())
		  result = FLayers[index];
        break;
      };
    case GC_CONTAINER_VIEWS:
      {
        if (index < FViews.size())
          result = FViews[index];
        break;
      };
    case GC_CONTAINER_MODEL:
      {
        result = FModel;
        break;
      };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Set the value of the given property, which must be a simple property.
 *
 * @param name The name of the property.
 * @param index If the property is a list then this is the index into that list.
 * @param value The new value of the property. Automatic conversion is performed where possible.
 */
void CGenericCanvas::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  switch (getPropertyID(name))
  {
	case GC_PROPERTY_NAME:
	  {
		FName = utf8ToUtf16(value);
		break;
	  };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Invalidates the viewer, so it does a repaint of the canvas.
 */
void CGenericCanvas::refresh(void)
{
  change(this, GC_CHANGE_CANVAS_REFRESH);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given layer from the internal layer list. The layer itself will not be destroyed, just removed.
 *
 * @param layer The layer to be removed.
 */
void CGenericCanvas::removeLayer(CLayer* layer)
{
  beginUpdate();

  for (CLayers::iterator iterator = FLayers.begin(); iterator != FLayers.end(); ++iterator)
    if (*iterator == layer)
    {
      delete layer;
      FLayers.erase(iterator);
      break;
    };
  endUpdate();

  change(this, GC_CHANGE_CANVAS_REMOVE_LAYER);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the style with the given name (UTF-8 encoded) from the canvas. Figure elements which used this style
 * will be left without visual appearence then.
 *
 * @param name The name of a style (encoded in UTF-8).
 */
void CGenericCanvas::removeStyle(const char* name)
{
  wstring styleName = utf8ToUtf16(name);
  FModel->removeStyle(styleName);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given view from the internal list.
 *
 * @param View The view to be removed.
 */
void CGenericCanvas::removeView(CGCView* View)
{
  beginUpdate();

  if (FCurrentView == View)
    FCurrentView = NULL;
  for (CViews::iterator iterator = FViews.begin(); iterator != FViews.end(); ++iterator)
    if (*iterator == View)
    {
      (*iterator)->removeListener(&FListener);
      FViews.erase(iterator);
      change(View, GC_CHANGE_CANVAS_REMOVE_VIEW);
      break;
    };

  endUpdate();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * This is the main paint routine. It must be called by the viewer holding the reference to this canvas (e.g. when a 
 * window must be redrawn).
 *
 * @param content The elements to render.
 */
void CGenericCanvas::render(TGCRenderContent content)
{
  // No display if the canvas is currently being updated.
  if (!updating())
  {
    beginUpdate();
    try
    {
      if ((FStates & GC_STATE_PENDING_ACTIVATION) != 0)
      {
        // A pending activation state always means there is a valid current view.
        FStates &= ~GC_STATE_PENDING_ACTIVATION;
        FCurrentView->activate();
      };

      if (FCurrentView != NULL)
      {
        FCurrentView->validate();
        clearBuffers();
        FCurrentView->render(content);
      };
      endUpdate();
    }
    catch(...)
    {
      endUpdate();
      throw;
    };

    checkError();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders the current scene to a file in the format given.
 *
 * @param filename The name of the target file. It must already contain the correct extension and must be encoded in UTF-8.
 * @param format The format of the file to render. Supported are PNG, PDF, PS (postscript) and EPS (encapsulated postscript).
 * @param title The titel for the document. Must be ANSI encoded for now.
 * @param software A string describing the producer of the document. Must be ANSI encoded for now.
 * @param content A set of flags indicating what additional info to render.
 * @param zoom The zoom at which render the file.
 * @param bounds Position and size of the area to store in the file (currently only for PNG).
 * @return True if successful otherwise false.
 */
bool CGenericCanvas::renderToFile(const char* filename, TGCFileFormat format, const char* title, const char* software,
                                  TGCRenderContent content, float zoom, TGCViewport& bounds)
{
  bool result = false;

  if (!updating())
  {
    beginUpdate();
    try
    {
      if ((FStates & GC_STATE_PENDING_ACTIVATION) != 0)
      {
        // A pending activation state always means there is a valid current view.
        FStates &= ~GC_STATE_PENDING_ACTIVATION;
        FCurrentView->activate();
      };

      switch (format)
      {
        case GC_FILE_FORMAT_PDF:
        case GC_FILE_FORMAT_PS:
        case GC_FILE_FORMAT_EPS:
          {
            if (FCurrentView != NULL)
            {
              const int fileTypeMapper[4] = {GL2PS_PDF, GL2PS_PS, GL2PS_EPS, GL2PS_TEX};

              FILE *file = openFile(filename, "wb");

              GLint bufferSize = 0;
              GLint state = GL2PS_OVERFLOW;
              
              char *oldlocale = setlocale(LC_NUMERIC, "C");
              while (state == GL2PS_OVERFLOW)
              {
                bufferSize += 1024 * 1024;
                gl2psBeginPage(title, software, NULL, fileTypeMapper[format], GL2PS_NO_SORT, GL2PS_DRAW_BACKGROUND | GL2PS_USE_CURRENT_VIEWPORT |
                  GL2PS_COMPRESS, GL_RGBA, 0, NULL, 0, 0, 0, bufferSize, file, filename);
                gl2psEnable(GL2PS_BLEND);
                gl2psBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

                clearBuffers();
                FCurrentView->render(content);

                state = gl2psEndPage();
              };
              setlocale(LC_NUMERIC, oldlocale);
              fclose(file);

              result = true;
            };
            break;
          };
        case GC_FILE_FORMAT_PNG:
          {
            if (FCurrentView != NULL)
            {
              TImage image;

              float workspaceWidth;
              float workspaceHeight;
              FCurrentView->getWorkspace(&workspaceWidth, &workspaceHeight);
              image.width = bounds.width;
              if (image.width < 0)
                image.width = (int) workspaceWidth;
              image.height = bounds.height;
              if (image.height < 0)
                image.height = (int) workspaceHeight;

              // If the frame buffer extension is not supported then there is no sense
              // to allocate a buffer larger than the current viewport,
              // because we cannot render more than this area in this case.
              if (!supportsExtension(GC_OE_FRAME_BUFFER_OBJECTS))
              {
                if (image.height > FCurrentView->viewportGet().height)
                  image.height = FCurrentView->viewportGet().height;
                if (image.width > FCurrentView->viewportGet().width)
                  image.width = FCurrentView->viewportGet().width;
              };

              image.colorType = COLOR_TYPE_RGB_ALPHA;
              image.data = (unsigned char*) malloc(image.width * image.height * 4);
              if (image.data != NULL)
              {
                FCurrentView->renderToMemory(GC_COLOR_FORMAT_RGBA, content, zoom, bounds, image.data);
                image.width = bounds.width;
                image.height = bounds.height;
                result = savePNG(utf8ToUtf16(filename), &image, true, title, software);
                free(image.data);
              };
            };
            break;
          };
      };
      endUpdate();
    }
    catch(...)
    {
      endUpdate();
      throw;
    };

    checkError();
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders the content of the current view into the given memory using the RGBA color layout.
 *
 * @param memory A pointer to already allocated memory. This is the target where data is rendered to and must be at least
 *               bounds.width * bounds.height * 4 bytes in size.
 * @param format Determines the order of the color components in the result.
 * @param content A set of flags indicating what additional info to render.
 * @param zoom The zoom level at which to render the scene.
 * @param bounds [in/out] The coordinates of the area that should be rendered (in view space). When traditional rendering
 *                        is used then it might be necessary to implicitly restrict this area to the current viewport.
 *                        In this case bounds are used to return actual area to the caller.
 * @param result True if rendering went fine, otherwise false.
 */
bool CGenericCanvas::renderToMemory(unsigned char* memory, TGCColorFormat format, TGCRenderContent content, float zoom, 
                                    TGCViewport& bounds)
{
  if (FCurrentView != NULL)
  {
    FCurrentView->renderToMemory(format, content, zoom, bounds, memory);
    return true;
  }
  else
    return false;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the path to be used by the texture manager for loading textures.
 *
 * @param path The UTF-8 encoded texture path.
 */
void CGenericCanvas::setTexturePath(const char* path)
{
  // The texture manager is a singleton.
  FModel->setTexturePath(path);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns a flag telling the calling whether a certain OpenGL extension is supported.
 *
 * @param extension The extension to check.
 * @return true or false, depending on the support for this extension.
 */
bool CGenericCanvas::supportsExtension(TOglExtension extension)
{
  return FExtensions[extension];
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the first view with the given name from the views collection.
 *
 * @param name The name of the view to return.
 * @return The first found view with the given name or NULL if no view could be found.
 */
CGCView* CGenericCanvas::viewByName(const char* name)
{
  CGCView* result = NULL;
  wstring viewName = utf8ToUtf16(name);
  for (CViews::const_iterator iterator = FViews.begin(); iterator != FViews.end(); ++iterator)
  {
    CGCView* view = *iterator;
    if (view->FName == viewName)
    {
      result = view;
      break;
    };
  };
  return result;
}

//----------------------------------------------------------------------------------------------------------------------
