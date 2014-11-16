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
 * @brief Parser for figure elements, which are converted from XML to our internal model.
 * 
 */

#ifndef __GC_GL_FIGURE_PARSER_H__
#define __GC_GL_FIGURE_PARSER_H__

#include <libxml/xmlmemory.h>

#include "myx_gc_canvas.h"

//----------------------------------------------------------------------------------------------------------------------

class CCaptionElementTemplate;

/**
 * CFigureParser converts a figure descriptions given in XML to elements in our internal model.
 */
class CFigureParser: public CGCBase
{
protected:
  void checkLookupTables(void);
  void parseActions(xmlNodePtr node, CBaseTemplate* template_);
  CCaptionElementTemplate* parseCaption(xmlNodePtr node);
  CFigureElementTemplate* parseElement(xmlNodePtr node, CGCModel* model);
public:
  CFigureParser(CGenericCanvas* canvas);
  virtual ~CFigureParser(void);

  void parseDecorDefinition(xmlNodePtr definition, CGCModel* model);
  void parseLayoutDefinition(xmlNodePtr definition, CGCModel* model);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value) {};
};

//----------------------------------------------------------------------------------------------------------------------

#endif // #ifndef __GC_GL_FIGURE_PARSER_H__
