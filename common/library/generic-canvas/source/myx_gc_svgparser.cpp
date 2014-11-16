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
 * @file myx_gc_svgparser.cpp
 * @brief Parser for svg elements, which are converted to OpenGL calls.
 * 
 */

#include "myx_gc_gl_helper.h"
#include "myx_gc_svgparser.h"

//----------------------------------------------------------------------------------------------------------------------

typedef enum
{
  GC_SVG_PRIMITIVE_UNKNOWN = -1,
  GC_SVG_PRIMITIVE_RECT,
  GC_SVG_PRIMITIVE_LINE,
  GC_SVG_PRIMITIVE_POLYLINE,
  GC_SVG_PRIMITIVE_POLYGON,
  GC_SVG_PRIMITIVE_CIRCLE,
  GC_SVG_PRIMITIVE_TEXT,
  GC_SVG_PRIMITIVE_TSPAN,
  GC_SVG_PRIMITIVE_GROUP,
  GC_SVG_PRIMITIVE_IMAGE
} TSvgPrimitive;

extern const string defaultFontFamily;
extern const string defaultFontStyle;
extern const string defaultFontWeight;
extern const int defaultFontSize;

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts a primitive name to a number.
 *
 * @param name The name to convert.
 * @result The primitive that corresponds to the given name.
 */
TSvgPrimitive findPrimitive(string name)
{
  static map<string, TSvgPrimitive> primitives;

  if (primitives.size() == 0)
  {
    // Initialize the map first if necessary.
    primitives["rect"] = GC_SVG_PRIMITIVE_RECT;
    primitives["line"] = GC_SVG_PRIMITIVE_LINE;
    primitives["polyline"] = GC_SVG_PRIMITIVE_POLYLINE;
    primitives["polygon"] = GC_SVG_PRIMITIVE_POLYGON;
    primitives["circle"] = GC_SVG_PRIMITIVE_CIRCLE;
    primitives["text"] = GC_SVG_PRIMITIVE_TEXT;
    primitives["tspan"] = GC_SVG_PRIMITIVE_TSPAN;
    primitives["g"] = GC_SVG_PRIMITIVE_GROUP;
    primitives["image"] = GC_SVG_PRIMITIVE_IMAGE;
  };

  map<string, TSvgPrimitive>::iterator iterator = primitives.find(name);
  if (iterator == primitives.end())
    return GC_SVG_PRIMITIVE_UNKNOWN;
  else
    return iterator->second;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Scans the given string for coordinate values and fills the Points vector with them.
 */
void parsePoints(xmlChar* Raw, CVertexVector* Points)
{
  Points->clear();

  StringTokenizer tokenizer((char*) Raw, " ,\t\n", true);

  while (tokenizer.hasMoreTokens())
  {
    // Invalid input results in a value of 0, so we don't need to take special
    // care for such cases.
    TVertex v;
    v.x = tokenizer.nextTokenAsFloat();
    if (tokenizer.hasMoreTokens())
    {
      v.y = tokenizer.nextTokenAsFloat();
      Points->push_back(v);
    };
  };
}

//----------------- CSVGParser -----------------------------------------------------------------------------------------

CSVGParser::CSVGParser(void)
{
}

//----------------------------------------------------------------------------------------------------------------------

CSVGParser::~CSVGParser(void)
{
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses the content of a circle definition.
 *
 * @see parseElement for a description of the parameters.
 */
void CSVGParser::parseCircle(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, 
                             float strokeWidth)
{
  GLUquadricObj *quadric = gluNewQuadric();

  float x = getFloatAttributeDef(svg, "cx", 0);
  float y = getFloatAttributeDef(svg, "cy", 0);
  float innerRadius = getFloatAttributeDef(svg, "inner-radius", 0);
  float outerRadius = getFloatAttributeDef(svg, "r", 1);
  float startAngle = getFloatAttributeDef(svg, "start-angle", 0);
  float sweepAngle = getFloatAttributeDef(svg, "sweep-angle", 360);
  GLint slices = getIntAttributeDef(svg, "slices", 10);
  GLint loops = getIntAttributeDef(svg, "rings", 3);

  // Update the accumulated bounding box.
  GLfloat matrix[16];
  glGetFloatv(GL_MODELVIEW_MATRIX, matrix);
  FBoundsComputer.include(matrix, x - outerRadius, y - outerRadius);
  FBoundsComputer.include(matrix, x + outerRadius, y + outerRadius);

  gluQuadricOrientation(quadric, GLU_INSIDE);

  if (fillColor != NULL)
    glColor4ubv(fillColor);

  glTranslatef(x, y, 0);
  if (doFill)
  {
    gluQuadricDrawStyle(quadric, GLU_FILL);
    gluQuadricNormals(quadric, GLU_SMOOTH);
    gluQuadricTexture(quadric, GL_TRUE);

    gluPartialDisk(quadric, innerRadius, outerRadius, slices, loops, startAngle, sweepAngle);
  };

  if (doStroke)
  {
    if (strokeColor != NULL)
      glColor4ubv(strokeColor);

    gluQuadricDrawStyle(quadric, GLU_SILHOUETTE);
    gluPartialDisk(quadric, innerRadius, outerRadius, slices, loops, startAngle, sweepAngle);
  };

  gluDeleteQuadric(quadric);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Recursively called function that parses the given svg xml element and converts it to OpenGL calls.
 *
 * @param svg Any of the supported primitives.
 * @param doFill The parent's fill flag. If this is true then also this element is filled.
 * @param fillColor The parent's fill color. Only used if the we have a local opacity.
 * @param doStroke The parent's outline flag. If this true then also this element is outlined.
 * @param strokeColor The parent's stroke color. Only used if the we have a local opacity.
 * @param strokeWidth The parent's stroke width. Used only if we draw an outline at all and no local width is given.
 * @param masterAlpha The accumulated alpha value, which is currently active. Used in conjunction with a local opacity.
 * @return Returns a new display list for the content of this element.
 */
GLuint CSVGParser::parseElement(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, 
                                float strokeWidth, float masterAlpha)
{
  TSvgPrimitive primitive = findPrimitive((char*) svg->name);
  CGCTexture* texture = NULL;
  GLuint resultList = 0;
                                    
  if (primitive != GC_SVG_PRIMITIVE_UNKNOWN)
  {
    bool display = true;

    xmlChar* attribute = xmlGetProp(svg, (xmlChar*) "display");
    if (attribute != NULL)
    {
      display = xmlStrcmp(attribute, (const xmlChar *) "none") != 0;
      xmlFree(attribute);
    };
    
    if (display)
    {
      attribute = xmlGetProp(svg, (xmlChar*) "texture");
      if (attribute != NULL)
      {
        texture = FModel->texture(utf8ToUtf16((char*) attribute));
        xmlFree(attribute);
      };

      // See if we have a local color and/or a local opacity.
      float opacity = masterAlpha;
      bool hasOpacity = getFloatAttribute(svg, "opacity", opacity);
      if (hasOpacity)
        opacity *= masterAlpha;

      GLubyte localFillColor[4];
      GLubyte* fillColorPtr = NULL;

      int conversion = convertColor(svg, "fill", localFillColor);
      // Fill this element if either filling is not completely disabled (fill ="none")
      // and either a local color is given or the parent element used filling already.
      bool localFill = (conversion != 3) && (doFill || fillColor != NULL || conversion == 0);
      if (localFill)
      {
        // If there is no valid local color then use that of the parent.
        if (conversion != 0)
        {
          // Combine the parent color with this opacity to create a new color.
          localFillColor[0] = fillColor[0];
          localFillColor[1] = fillColor[1];
          localFillColor[2] = fillColor[2];
        };
        localFillColor[3] = GLubyte(255 * opacity);
        fillColorPtr = localFillColor;
      };

      GLubyte localStrokeColor[4];
      GLubyte* strokeColorPtr = NULL;

      conversion = convertColor(svg, "stroke", localStrokeColor);
      bool localStroke = (conversion != 3) && (doStroke || strokeColor != NULL || conversion == 0);
      if (localStroke)
      {
        // If there is no valid local color then use that of the parent.
        if (conversion != 0)
        {
          // Combine the parent color with this opacity to create a new color.
          localStrokeColor[0] = strokeColor[0];
          localStrokeColor[1] = strokeColor[1];
          localStrokeColor[2] = strokeColor[2];
        };
        localStrokeColor[3] = GLubyte(255 * opacity);
        strokeColorPtr = localStrokeColor;
      };

      float localStrokeWidth = getFloatAttributeDef(svg, "stroke-width", strokeWidth);

      // Prepare child display lists before starting with the one for this element.
      GLuint localList = 0;
      switch (primitive)
      {
        case GC_SVG_PRIMITIVE_TEXT:
        case GC_SVG_PRIMITIVE_TSPAN:
          {
            localList = parseText(svg, localFill, fillColorPtr, localStroke, strokeColorPtr, localStrokeWidth, opacity);
            break;
          };
        case GC_SVG_PRIMITIVE_GROUP:
          {
            localList = parseGroup(svg, localFill, fillColorPtr, localStroke, strokeColorPtr, localStrokeWidth, opacity);
            break;
          };
        case GC_SVG_PRIMITIVE_IMAGE:
          {
            parseImage(svg, false);
            break;
          };
      };

      if (texture != NULL)
        texture->validateHandle();

      resultList = glGenLists(1);
      // We need the display lists executed while being compiled to have the modelview matrix updated correctly.
      // This is needed to compute the bounding box correctly.
      glNewList(resultList, GL_COMPILE_AND_EXECUTE);

      glPushMatrix();

      if (texture != NULL)
        texture->activateTexture();

      attribute = xmlGetProp(svg, (xmlChar*) "transform");
      if (attribute != NULL)
      {
        parseTransformation((char*) attribute);
        xmlFree(attribute);
      };

      switch (primitive) 
      {
        case GC_SVG_PRIMITIVE_RECT:
          {
            parseRectangle(svg, localFill, fillColorPtr, localStroke, strokeColorPtr, localStrokeWidth);
            break;
          };
        case GC_SVG_PRIMITIVE_LINE:
          {
            parseLine(svg, localStroke, strokeColorPtr, localStrokeWidth);
            break;
          };
        case GC_SVG_PRIMITIVE_POLYLINE:
          {
            parsePolyline(svg, localFill, fillColorPtr, localStroke, strokeColorPtr, localStrokeWidth);
            break;
          };
        case GC_SVG_PRIMITIVE_POLYGON:
          {
            parsePolygon(svg, localFill, fillColorPtr, localStroke, strokeColorPtr, localStrokeWidth);
            break;
          };
        case GC_SVG_PRIMITIVE_CIRCLE:
          {
            parseCircle(svg, localFill, fillColorPtr, localStroke, strokeColorPtr, localStrokeWidth);
            break;
          };
        case GC_SVG_PRIMITIVE_TEXT:
        case GC_SVG_PRIMITIVE_TSPAN:
        case GC_SVG_PRIMITIVE_GROUP:
          {
            if (localList != 0)
              glCallList(localList);
            break;
          };
        case GC_SVG_PRIMITIVE_IMAGE:
          {
            parseImage(svg, true);
            break;
          };
      };

      if (texture != NULL)
        texture->deactivateTexture();

      glPopMatrix();

      glEndList();
    };
  };

  return resultList;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Collects all data in an <svg:g> element.
 *
 * @return A new display list comprising all subelements.
 * @see parseElement for the description of the parameters.
 */
GLuint CSVGParser::parseGroup(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, 
                              float strokeWidth, float masterAlpha)
{
  vector<GLuint> lists;
  for (xmlNodePtr child = svg->children; child != NULL; child = child->next)
    if (child->type == XML_ELEMENT_NODE)
    {
      GLuint List = parseElement(child, doFill, fillColor, doStroke, strokeColor, strokeWidth, masterAlpha);
      lists.push_back(List);
    };

  GLuint resultList = glGenLists(1);
  glNewList(resultList, GL_COMPILE);
  for (vector<GLuint>::iterator iterator = lists.begin(); iterator != lists.end(); iterator++)
    glCallList(*iterator);
  glEndList();

  return resultList;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses the content of an image definition.
 *
 * @param svg The XML svg element containing the definition.
 * @param render If true then the appropriate OpenGL calls for rendering are issued, otherwise setup is performed.
 */
void CSVGParser::parseImage(xmlNodePtr svg, bool render)
{
  string filename = getStringAttributeDef(svg, "href", "");
  wstring id = utf8ToUtf16(getStringAttributeDef(svg, "id", filename));

  if (!render)
  {
    // Create the image only if not yet done.
    CGCTexture* texture = FModel->texture(id);
    if (texture == NULL)
    {
      TLODList lodData;
      lodData.push_back(utf8ToUtf16(filename));

      texture = FModel->createTexture(lodData, id, "clamp-to-edge", "clamp-to-edge", "linear-mipmap-linear", 
        "linear", 2, "modulate");
    };
    // Make sure the handle of the texture is created before a display list is created.
    texture->validateHandle();
  }
  else
  {
    float x = getFloatAttributeDef(svg, "x", 0);
    float y = getFloatAttributeDef(svg, "y", 0);
    float width = getFloatAttributeDef(svg, "width", 0);
    float height = getFloatAttributeDef(svg, "height", 0);

    // Update the accumulated bounding box.
    GLfloat matrix[16];
    glGetFloatv(GL_MODELVIEW_MATRIX, matrix);
    FBoundsComputer.include(matrix, x, y);
    FBoundsComputer.include(matrix, x + width, y + height);

    CGCTexture* texture = FModel->texture(id);
    if (texture)
      texture->activateTexture();

    glColor4f(1, 1, 1, 1);
    glTranslatef(x, y, 0);
    glBegin(GL_POLYGON);
      glTexCoord2d(0, 0);
      glVertex2f(0, 0);
 //     glNormal3f(0, 0, 1);
      glTexCoord2d(1, 0);
      glVertex2f(width, 0);
//      glNormal3f(0, 0, 1);
      glTexCoord2d(1, 1);
      glVertex2f(width, height);
//      glNormal3f(0, 0, 1);
      glTexCoord2d(0, 1);
      glVertex2f(0, height);
//      glNormal3f(0, 0, 1);
    glEnd();

    if (texture)
      texture->deactivateTexture();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses the content of a line definition.
 *
 * @param svg The XML svg element containing the definition.
 * @param doStroke Flag to indicate if the line is to be drawn or not.
 * @param strokeColor The color to be used for the line (if set).
 * @param strokeWidth The width of the line.
 */
void CSVGParser::parseLine(xmlNodePtr svg, bool doStroke, GLubyte* strokeColor, float strokeWidth)
{
  float X1 = getFloatAttributeDef(svg, "x1", 0);
  float Y1 = getFloatAttributeDef(svg, "y1", 0);
  float X2 = getFloatAttributeDef(svg, "x2", 0);
  float Y2 = getFloatAttributeDef(svg, "y2", 0);

  // Update the accumulated bounding box.
  GLfloat matrix[16];
  glGetFloatv(GL_MODELVIEW_MATRIX, matrix);
  FBoundsComputer.include(matrix, X1, Y1);
  FBoundsComputer.include(matrix, X2, Y2);

  if (doStroke)
  {
    if (strokeColor != NULL)
      glColor4ubv(strokeColor);
    glLineWidth(strokeWidth);

    glBegin(GL_LINES);
      glVertex2f(X1, Y1);
 //     glNormal3f(0, 0, 1);
      glVertex2f(X2, Y2);
//      glNormal3f(0, 0, 1);
    glEnd();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses the content of a polygon definition.
 *
 * @see parseElement for a description of the parameters.
 */
void CSVGParser::parsePolygon(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, 
                              float strokeWidth)
{
  xmlChar* Raw = xmlGetProp(svg, (xmlChar*) "points");
  if (Raw != NULL)
  {
    CVertexVector vertices;
    parsePoints(Raw, &vertices);
    xmlFree(Raw);

    renderVertices(doFill, fillColor, doStroke, strokeColor, vertices, NULL, strokeWidth, true);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses the content of a polyline definition.
 *
 * @see parseElement for a description of the parameters.
 */
void CSVGParser::parsePolyline(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, 
                               float strokeWidth)
{
  xmlChar* Raw = xmlGetProp(svg, (xmlChar*) "points");
  if (Raw != NULL)
  {
    CVertexVector vertices;
    parsePoints(Raw, &vertices);
    xmlFree(Raw);

    renderVertices(doFill, fillColor, doStroke, strokeColor, vertices, NULL, strokeWidth, false);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses the content of a rectangle definition.
 *
 * @see parseElement for a description of the parameters.
 */
void CSVGParser::parseRectangle(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, 
                                float strokeWidth)
{
  float x, y, width, height, RX, RY;

  if (!getFloatAttribute(svg, "x", x))
    x = 0;
  if (!getFloatAttribute(svg, "y", y))
    y = 0;
  if (!getFloatAttribute(svg, "width", width))
    width = 1;
  if (!getFloatAttribute(svg, "height", height))
    height = 1;

  // There are a few more things to check with the corner radii.
  bool RXfound = getFloatAttribute(svg, "rx", RX);
  bool RYfound = getFloatAttribute(svg, "ry", RY);
  if (!RXfound && !RYfound)
  {
    RX = 0;
    RY = 0;
  }
  else
  {
    if (!RXfound)
      RX = RY;
    else
      if (!RYfound)
        RY = RX;
    if (RX > width / 2)
      RX = width / 2;
    if (RY > width / 2)
      RY = width / 2;
  };

  // TODO: Consider rounded corners.
  CVertexVector vertices;
  TVertex v;
  v.x = x;
  v.y = y;
  vertices.push_back(v);
  v.x = x + width;
  vertices.push_back(v);
  v.y = y + height;
  vertices.push_back(v);
  v.x = x;
  vertices.push_back(v);

  CVertexVector TextureCoordinates;
  v.x = 0;
  v.y = 0;
  TextureCoordinates.push_back(v);
  v.x = width;
  TextureCoordinates.push_back(v);
  v.y = height;
  TextureCoordinates.push_back(v);
  v.x = 0;
  TextureCoordinates.push_back(v);

  renderVertices(doFill, fillColor, doStroke, strokeColor, vertices, &TextureCoordinates, strokeWidth, true);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Takes a text node and gets all attributes for direct or dynamic rendering. This function is called recursively
 * and can take either a <text> or a <tspan> node.
 *
 * @param svg The text or tspan node to parse.
 * @param fillColor The color for the text interior.
 * @param strokeColor The color of the outline.
 * @param The width of the outlines.
 * @param masterAlpha Only passed through because text nodes can have children.
 * @return A new display list comprising all subelements.
 */
GLuint CSVGParser::parseText(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, 
                             float strokeWidth, float masterAlpha)
{
  // Collect font information.
  string fontFamily = getStringAttributeDef(svg, "font-family", defaultFontFamily);
  string fontStyle = getStringAttributeDef(svg, "font-style", defaultFontStyle);
  string fontWeightString = getStringAttributeDef(svg, "font-weight", defaultFontWeight);
  int weight = convertFontWeight(fontWeightString);
  int fontSize = getIntAttributeDef(svg, "font-size", defaultFontSize);

  vector<GLuint> lists;
  xmlNodePtr child = svg->children;
  while (child != NULL)
  {
    if (XML_IS(child, "tspan"))
    {
      GLuint localList = parseElement(child, doFill, fillColor, doStroke, strokeColor, strokeWidth, masterAlpha);
      lists.push_back(localList);
    }
    else
    {
      string source((char*) child->content);
      wstring output = utf8ToUtf16(source);

      // Make the font manager create all display lists before we build ours.
      string fontId = fontManager()->fontIdCreate(fontFamily, fontStyle, weight, fontSize);
      TBoundingBox box;
      fontManager()->boundingBox(output, fontId, box);

      // Update the accumulated bounding box.
      GLfloat matrix[16];
      glGetFloatv(GL_MODELVIEW_MATRIX, matrix);
      FBoundsComputer.include(matrix, box);

      GLuint localList = glGenLists(1);
      glNewList(localList, GL_COMPILE_AND_EXECUTE);
      if (fillColor != NULL)
        glColor4ubv(fillColor);
      // FTGL uses the lower left corner as origin, we however have the upper left corner for this task.
      // Adjust the vertical coordinate accordingly.
      glRasterPos2f(0, box.lower.y);
      fontManager()->textOut(output, fontId);
      glEndList();
      lists.push_back(localList);
    };

    child = child->next;
  };

  // Create final display list.
  GLuint displayList = glGenLists(1);
  glNewList(displayList, GL_COMPILE);

  float x = getFloatAttributeDef(svg, "x", 0);
  float y = getFloatAttributeDef(svg, "y", 0);

  glTranslatef(x, y, 0);

  if (fillColor !=  NULL)
    glColor4ubv(fillColor);

  for (vector<GLuint>::iterator iterator = lists.begin(); iterator != lists.end(); iterator++)
    glCallList(*iterator);
  glEndList();

  return displayList;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parsers the given string and interprets the content as one or more transformation of the form:
 *   translate(x, y, z) scale(x, y, z) etc.
 *
 * @param transformation The textual representation of the transformation to parse and perform.
 */
void CSVGParser::parseTransformation(char* transformation)
{
  string delimiters(" ,()\t\n");
  StringTokenizer tokenizer(transformation, delimiters, true);

  // Each run in the outer loop is one transformation.
  while (tokenizer.hasMoreTokens())
  {
    // Skip leading white spaces.
    wstring token = tokenizer.nextToken();

    int type = 0;
    if (token == L"matrix")
      type = 1;
    else
      if (token == L"translate")
        type = 2;
      else
        if (token == L"scale")
          type = 3;
        else
          if (token == L"rotate")
            type = 4;
          else
            if (token == L"skewX")
              type = 5;
            else
              if (token == L"skewY")
                type = 6;
              else
                break;

    switch (type)
    {
      case 1: // matrix
        {
          // Argument list found. Read 6 float numbers.
          // | a  d  0  0 |
          // | b  e  0  0 |
          // | 0  0  1  0 |
          // | c  f  0  1 |
          GLfloat matrix[16] = {
            1, 0, 0, 0, 
            0, 1, 0, 0, 
            0, 0, 1, 0, 
            0, 0, 0, 1
          };

          for (int j = 0; j < 2; j++)
            for (int i = 0; i < 2; i++)
              matrix[j * 4 + i] = tokenizer.nextTokenAsFloat();
          matrix[12] = tokenizer.nextTokenAsFloat();
          matrix[13] = tokenizer.nextTokenAsFloat();
          glMultMatrixf(matrix);

          break;
        };
      case 2: // Translation
        {
          // One or two floats are possible here.
          float tx = tokenizer.nextTokenAsFloat();
          float ty = 0;
          if (tokenizer.hasMoreTokens())
            ty = tokenizer.nextTokenAsFloat();
          glTranslatef(tx, ty, 0);

          break;
        };
      case 3: // Scaling
        {
          // One or two floats are possible here.
          float sx = tokenizer.nextTokenAsFloat();
          float sy = sx;
          if (tokenizer.hasMoreTokens())
            sy = tokenizer.nextTokenAsFloat();
          glScalef(sx, sy, 0);

          break;
        };
      case 4: // Rotation
        {
          // An angle (in degrees) and an optional rotation point are possible here.
          float Angle = tokenizer.nextTokenAsFloat();
          float x = 0;
          float y = 0;
          if (tokenizer.hasMoreTokens())
          {
            x = tokenizer.nextTokenAsFloat();
            y = tokenizer.nextTokenAsFloat();
          };

          glTranslatef(x, y, 0);
          glRotatef(Angle, 0, 0, 1);
          glTranslatef(-x, -y, 0);

          break;
        };
      case 5: // x Skewing
      case 6: // y Skewing
        {
          // Only one value is expected, which gives the skewing angle in degrees.
          GLdouble matrix[16] = {
            1, 0, 0, 0, 
            0, 1, 0, 0, 
            0, 0, 1, 0, 
            0, 0, 0, 1
          };

          int index = (type == 5) ? 4 : 1;
          matrix[index] = tan(tokenizer.nextTokenAsFloat() * M_PI / 180);
          glMultMatrixd(matrix);
          break;
        };
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders the given set of vertices.
 *
 * @param doFill Filled output is only done if this flag is true. For strokes the existance of the color as such is used
 *               as indicator.
 * @param fillColor If not NULL then it gives the local color for this call, otherwise the current color stays untouched.
 * @param strokeColor If not nULL then the vertices are render again as lines but using that color.
 * @param vertices The vertex data to render.
 * @param TextureCoordinates If given (can be NULL) then there must be exactly the same number of texture coordinates
 *                           as there are vertices.
 * @param strokeWidth The width of the border if one is rendered.
 * @param closeShape Determines whether the border line (if strokeColor is given) is closed (that is, the last point is
 *                   connected to the first point).
 */
void CSVGParser::renderVertices(bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, 
                                const CVertexVector& vertices, CVertexVector* TextureCoordinates, float strokeWidth, 
                                bool closeShape)
{
  GLfloat matrix[16];
  glGetFloatv(GL_MODELVIEW_MATRIX, matrix);

  if (doFill)
  {
    if (fillColor != NULL)
      glColor4ubv(fillColor);

    glBegin(GL_POLYGON);
    for (int i = 0; i < (int) vertices.size(); i++)
    {
      if (TextureCoordinates != NULL)
        glTexCoord2d((*TextureCoordinates)[i].x, (*TextureCoordinates)[i].y);
      glVertex2f(vertices[i].x, vertices[i].y);
 //     glNormal3f(0, 0, 1);

      // Update the accumulated bounding box.
      FBoundsComputer.include(matrix, vertices[i]);
    };
    glEnd();
  };

  if (doStroke)
  {
    if (strokeColor != NULL)
      glColor4ubv(strokeColor);
    if (strokeWidth <= 1)
    {
      glBegin(closeShape ? GL_LINE_LOOP : GL_LINE_STRIP);
      for (int i = 0; i < (int) vertices.size(); i++)
      {
        glVertex2f(vertices[i].x, vertices[i].y);
 //       glNormal3f(0, 0, 1);
      };
      glEnd();
    }
    else
    {
      // render small rectangles for each line.
      float halfWidth = strokeWidth / 2;
      vector<TVertex>::size_type index = 0;
      vector<TVertex>::size_type count = vertices.size();
      if (!closeShape)
        count--;
      while (index < count)
      {
        vector<TVertex>::size_type NextIndex = index + 1;
        if (index == vertices.size() - 1)
          NextIndex = 0;
        // We need the angle of the current line relative to the horizontal axis.
        float dX = vertices[NextIndex].x - vertices[index].x;
        float dY = vertices[NextIndex].y - vertices[index].y;
        float Angle = atan2(dY, dX);

        // Compute the four corners for the rectangle. We can use symmetry to speed up computation.
        dX = halfWidth * sin(Angle);
        dY = halfWidth * cos(Angle);
        glBegin(GL_POLYGON);
          glVertex2f(vertices[index].x - dX, vertices[index].y + dY);
 //         glNormal3f(0, 0, 1);
          FBoundsComputer.include(matrix, vertices[index]);

          glVertex2f(vertices[NextIndex].x - dX, vertices[NextIndex].y + dY);
   //       glNormal3f(0, 0, 1);
          FBoundsComputer.include(matrix, vertices[index]);

          glVertex2f(vertices[NextIndex].x + dX, vertices[NextIndex].y - dY);
     //     glNormal3f(0, 0, 1);
          FBoundsComputer.include(matrix, vertices[index]);

          glVertex2f(vertices[index].x + dX, vertices[index].y - dY);
       //   glNormal3f(0, 0, 1);
          FBoundsComputer.include(matrix, vertices[index]);
        glEnd();

        index++;
      };
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses the given svg xml description and converts it to OpenGL calls. This method can be called within an active
 * OpenGL display list compilation (but not between glBegin/glEnd).
 *
 * @param svg The svg top level element (<svg:svg> </svg:svg>).
 * @param displayList The OpenGL display list to render into.
 */
void CSVGParser::convert(xmlNodePtr svg, GLuint displayList)
{
  vector<GLuint> lists;
  for (xmlNodePtr child = svg->children; child != NULL; child = child->next)
    if (child->type == XML_ELEMENT_NODE)
    {
      GLuint localList = parseElement(child, false, NULL, false, NULL, 1, 1);
      lists.push_back(localList);
    };

  // Not necessary for creating the list but used to create a clean starting point to transform bounding boxes.
  glLoadIdentity();

  glNewList(displayList, GL_COMPILE);

  glColor4f(0, 0, 0, 1);
  for (vector<GLuint>::iterator iterator = lists.begin(); iterator != lists.end(); iterator++)
    glCallList(*iterator);

  glEndList();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses the given style definition and creates a new style, which is then added to the given model.
 *
 * @param definition The definition to parse.
 * @model The model to which the new style is to be added.
 */
void CSVGParser::parseDefinition(xmlNodePtr definition, CGCModel* model)
{
  string id; 
  if (getStringAttribute(definition, "id", id))
  {
    // Keep a reference to the model so other parts can access it.
    FModel = model;

    // Handle sub entries of the layout definition.
    xmlNodePtr element = definition->children;

    // This call will create an entry if there is none yet.
    CGCStyle* style = model->style(utf8ToUtf16(id));
    if (style->FDisplayList == 0)
      style->FDisplayList = glGenLists(1);
    while (element != NULL)
    {
      if (element->type == XML_ELEMENT_NODE)
      {
        if (XML_IS(element, "svg"))
        {
          FBoundsComputer.reset();

          // Always add the point (0, 0, 0) to the bounding box.
          FBoundsComputer.include(NULL, TVertex(0, 0, 0));
          convert(element, style->FDisplayList);
          style->FBoundingBox = FBoundsComputer.boundingBox();
          FModel->canvas()->checkError();
        };

        // Ignore everything else.
        break;
      };
      element = element->next;
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

