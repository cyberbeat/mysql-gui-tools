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
 * @file myx_gc_svgparser.h
 * @brief Parser for svg elements, which are converted to OpenGL calls.
 * @note This parser does not handle a full svg description but only single svg elements.
 * 
 */

#ifndef __GC_GL_SVGPARSER_H__
#define __GC_GL_SVGPARSER_H__

#include "myx_gc_texture.h"
#include "myx_gc_model.h"
#include <libxml/xmlmemory.h>

//----------------------------------------------------------------------------------------------------------------------

class CGCModel;

/**
 * CSVGParser is the main svg parser class. It converts an <svg> element description into OpenGL calls.
 *
 * @note Not all possible subelements/attributes can be parsed by this class. If they are specified then they will be
 *       ignored. See Generic Canvas documentation for more details.
 */
class CSVGParser
{
private:
  CBoundingBoxComputer FBoundsComputer;
  CGCModel* FModel;
protected:
  void parseCircle(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, float strokeWidth);
  GLuint parseElement(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, float strokeWidth, 
    float MasterAlpha);
  GLuint parseGroup(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, float strokeWidth, 
    float MasterAlpha);
  void parseImage(xmlNodePtr svg, bool render);
  void parseLine(xmlNodePtr svg, bool doStroke, GLubyte* strokeColor, float strokeWidth);
  void parsePolygon(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, float strokeWidth);
  void parsePolyline(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, float strokeWidth);
  void parseRectangle(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, float strokeWidth);
  GLuint parseText(xmlNodePtr svg, bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, float strokeWidth, 
    float MasterAlpha);
  void parseTransformation(char* Transformation);
  void renderVertices(bool doFill, GLubyte* fillColor, bool doStroke, GLubyte* strokeColor, const CVertexVector& vertices, 
    CVertexVector* textureCoordinates, float strokeWidth, bool closeShape);
public:
  CSVGParser(void);
  virtual ~CSVGParser(void);

  void convert(xmlNodePtr svg, GLuint DisplayList);
  void parseDefinition(xmlNodePtr definition, CGCModel* model);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // #ifndef __GC_GL_SVGPARSER_H__
