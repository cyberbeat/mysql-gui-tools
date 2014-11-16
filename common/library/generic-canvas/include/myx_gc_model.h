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
 * @file myx_gc_model.h 
 * @brief Implementation of the model that manages the visual representation in the generic canvas.
 * 
 */

#ifndef __GC_MODEL_H__
#define __GC_MODEL_H__

#include "myx_gc_style.h"
#include "myx_gc_figure.h"
#include "myx_gc_connection.h"

//----------------------------------------------------------------------------------------------------------------------

class CGenericCanvas;
class CConnection;
class CGCTexture;

class GENERIC_CANVAS_API CGCModel: public CGCBase
{
  friend class CFigure;
  friend class CFigureParser;
  friend class CSVGParser;
private:
  class GENERIC_CANVAS_API CFigureListener: public CGCBaseListener
  {
    friend class CGCModel;
  protected:
    CGCModel* model;
  public:
    virtual void __cdecl onDestroy(CGCBase* sender);
  };

  CLayoutList FLayouts;           // List of templates to create figures from.
  CConnectionDecors FConnectionDecors; // List of templates for connections.
  CFigureList FFigures;           // The list of figures.
  CStyleList FStyles;             // A list of elements for visual representation of figure elements.
  CConnections FConnections;
  CFigureListener FListener;
  CTextures FTextures;
  wstring FTexturePath;           // The path to be used for loading texture images.
protected:
  void addFigure(CFigure* figure);
public:
  CGCModel(CGenericCanvas* canvas);
  virtual ~CGCModel(void);

  void clearConnections(void);
  void clearDecors(void);
  void clearFigures(void);
  void clearLayouts(void);
  void clearStyles(void);
  void clearTextures(void);
  CConnection* createConnection(wstring type, wstring layoutClass, CFigure* endPoint1, CFigure* endPoint2);
  CConnectionDecor* createDecor(wstring type, wstring layoutClass);
  CFigure* createFigure(wstring type, wstring layoutClass);
  CFigureTemplate* createLayout(wstring type, wstring layoutClass);
  CGCTexture* createTexture(const TLODList& lodData, const wstring& id, const string& wrapH, const string& wrapV,
    const string& minificationFilterStr, const string& magnificationFilterStr, int dimensions, const string& mode);
  CConnectionDecor* decor(const char* type, const char* layoutClass);
  CConnectionDecor* decor(wstring type, wstring layoutClass);
  CFigureTemplate* layout(const char* type, const char* layoutClass);
  CFigureTemplate* layout(wstring type, wstring layoutClass);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
  void removeConnection(CConnection* connection);
  void removeFigure(CFigure* figure);
  void removeStyle(wstring ID);
  void setTexturePath(const string& path);
  CGCStyle* style(const wstring &ID);
  int styleExists(const wstring &ID);
  CGCTexture* texture(const wstring& name);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // #ifdef __GC_MODEL_H__

