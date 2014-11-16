/* Copyright (C) 2004 MySQL AB

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANy WARRANTy; without even the implied warranty of
   MERCHANTABILITy or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   you should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */

/**
 * @file myx_gc_figure.cpp 
 * @brief Implementation of the model element class.
 * 
 */

#include "myx_gc_figure.h"
#include "myx_gc_canvas.h"
#include "gc_glext.h"
#include "myx_gc_gl_helper.h"

extern const TMatrix identity;

extern const string DefaultLayout = "column"; // The layout to be used for figure elements without any given layout.
extern const string DefaultResize = "all";    // By default all figure elements can  be resized.

//----------------------------------------------------------------------------------------------------------------------

static inline bool isWordBoundary(wchar_t ch)
{
  return g_unichar_isspace(ch) || g_unichar_ispunct(ch);
}


//----------------------------------------------------------------------------------------------------------------------

static inline wstring::size_type findLineBoundary(const wstring &line)
{
  wstring::const_iterator iter;
  wstring::size_type i = 0;
  for (iter = line.begin(); iter != line.end(); ++iter, ++i)
  {
    GUnicodeBreakType breakType = g_unichar_break_type(*iter);
    switch (breakType)
    {
      case G_UNICODE_BREAK_CARRIAGE_RETURN:
      case G_UNICODE_BREAK_LINE_FEED:
        return i;
    };
  }
  return line.size();
}


//----------------- CStyleListener -------------------------------------------------------------------------------------

void CFigureElementTemplate::CStyleListener::onDestroy(CGCBase* sender)
{
  if (template_ != NULL)
    template_->freeNotification(sender);
}

//----------------- CFigureElementTemplate ------------------------------------------------------------------------------

CFigureElementTemplate::CFigureElementTemplate(wstring id, wstring key)
{
  FId = id;
  FKey = key;
  FLayout = GC_LAYOUT_ROW;
  FResizeMode = GC_RESIZE_NONE; 
  FParent = NULL;
  FFigure = NULL;
  FCaption = NULL;
  FOccurence = GC_OCC_ONCE;
  FListener.template_ = this;
}

//----------------------------------------------------------------------------------------------------------------------

CFigureElementTemplate::~CFigureElementTemplate(void)
{
  if (FStyle != NULL && !FStyle->destroying())
    FStyle->removeListener(&FListener);
  for (CElementTemplateList::iterator iterator = FChildren.begin(); iterator != FChildren.end(); ++iterator)
    delete *iterator;
  delete FCaption;
}

//----------------------------------------------------------------------------------------------------------------------

void CFigureElementTemplate::addChild(CFigureElementTemplate* Child)
{
  Child->FParent = this;
  Child->FFigure = NULL;
  FChildren.push_back(Child);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called when a style is about to be destroyed.
 *
 * @param object The object, which is about to be freed.
 */
void CFigureElementTemplate::freeNotification(CGCBase* object)
{
  if (FStyle == object)
    FStyle = NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the list element of this figure element template. There is only a list element if this template is a list
 * that is, has only one child that can appear more than once. This child element is then the list element.
 *
 * @return The only child element of this template if it is a list template, otherwise NULL.
 */
CFigureElementTemplate* CFigureElementTemplate::getListElement(void)
{
  CFigureElementTemplate* result = NULL;

  if (FChildren.size() == 1)
  {
    CFigureElementTemplate* child = FChildren[0];
    if (child->occurence() != GC_OCC_ONCE)
      result = FChildren[0];
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Helper method used by the figure parser to initialize some members.
 *
 * @param layout The the layout to be used in the element.
 * @param resizeMode Resizable flag.
 * @param constraints The resize constraints.
 */
void CFigureElementTemplate::initialize(TFigureElementLayout layout, TFigureElementResize resizeMode, CGCStyle* style, 
                                        const TConstraints& constraints, TOccurence occurence)
{
  FLayout = layout;
  FResizeMode = resizeMode;
  FStyle = style;
  if (FStyle != NULL)
    FStyle->addListener(&FListener);
  FConstraints = constraints;
  FOccurence = occurence;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines if this template is a list. A list is defined as having only one child, which is allowed
 * to appear more than once.
 *
 * @return True if this template is a list, otherwise false.
 */
bool CFigureElementTemplate::isList(void)
{
  if (FChildren.size() == 1)
  {
    CFigureElementTemplate* child = FChildren[0];
    return child->occurence() != GC_OCC_ONCE;
  };

  return false;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the (optional) caption for this element. If there is already one then it is freed.
 *
 * @param Caption The caption to be used from now on.
 */
void CFigureElementTemplate::setCaption(CCaptionElementTemplate* Caption)
{
  if (FCaption != NULL)
    delete FCaption;
  FCaption = Caption;
  if (FCaption != NULL)
    FCaption->FParent = this;
}

//----------------- CCaptionElementTemplate ------------------------------------------------------------------------------------

CCaptionElementTemplate::CCaptionElementTemplate(wstring key)
{
  FKey = key;
  FFontSize = 20;
  FWeight = 400;
  FHorizontalAlignment = GC_ALIGN_LEFT_TOP;
  FVerticalAlignment = GC_ALIGN_LEFT_TOP;
  FHasColor = false;
  FColor[0] = 0;
  FColor[1] = 0;
  FColor[2] = 0;
  FColor[3] = 255;
}

//----------------------------------------------------------------------------------------------------------------------

void CCaptionElementTemplate::initialize(wstring Text, float x, float y, string FontFamily, int FontSize, int Weight, 
                                         string FontStyle, TAlignment HorizontalAlignment, TAlignment VerticalAlignment, 
                                         GLubyte* Color, const TConstraints& constraints, bool WrapText)
{
  FText = Text;
  FOffsetX = x;
  FOffsetY = y;
  FFontFamily = FontFamily;
  FFontSize = FontSize;
  FWeight = Weight;
  FFontStyle = FontStyle;
  FHorizontalAlignment = HorizontalAlignment;
  FVerticalAlignment = VerticalAlignment;
  FHasColor = Color != NULL;
  FWrapText= WrapText;
  if (FHasColor)
  {
    for (int I = 0; I < 4; ++I)
      FColor[I] = (float) Color[I] / 255;
  };
  FConstraints = constraints;
}

//----------------- CFigureTemplate ------------------------------------------------------------------------------------

CFigureTemplate::CFigureTemplate(CGCModel* model, wstring type, wstring layoutClass): CGCBase(model->canvas())
{
  FModel = model;
  FType = type;
  FClass = layoutClass;
  FContent = NULL;
  _className = "CFigureTemplate";
}

//----------------------------------------------------------------------------------------------------------------------

CFigureTemplate::~CFigureTemplate(void)
{
  delete FContent;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by path. The path syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property to return.
 * @param index The index of the sub property to return if it is located in a list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CFigureTemplate::propertyGet(const char* name, unsigned int index)
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
              result = utf16ToUtf8(FType + L"@" + FClass);
              break;
            };
          case GC_PROPERTY_OWNER:
            {
              result = FModel;
              break;
            };
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
 * @param index The index of the sub property to return if it is located in a list.
 * @param value The new value of the property. Automatic conversion is performed where possible.
 */
void CFigureTemplate::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  // There are currently no properties that could be changed. The name is a unique identifier and must not be changed.
}

//----------------- CCaptionElement ------------------------------------------------------------------------------------

CCaptionElement::CCaptionElement(CGenericCanvas* canvas): CGraphicElement(canvas)
{
  _className = "CCaptionElement";
  FFontFamily = "Arial";
  FFontSize = 20;
  FWeight = 400;
  FFontStyle = "normal";
  FHorizontalAlignment = GC_ALIGN_LEFT_TOP;
  FVerticalAlignment = GC_ALIGN_LEFT_TOP;
  FParent = NULL;
  FHasColor = false;
  FWrapText = false;
  FBidiMode = GC_BIDI_LEFT_TO_RIGHT;

  if (canvas->currentViewGet() != NULL)
    FActualFontSize = ROUND(canvas->currentViewGet()->zoomGet() * FFontSize);
}

//----------------------------------------------------------------------------------------------------------------------

CCaptionElement::~CCaptionElement(void)
{
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines the end position of a part in the given text, which fits into a certain area.
 * The break algorithm takes care not to split words.
 *
 * @param line The line to break into parts that fit into the given space.
 * @param start The start position in the text for processing.
 * @param stop [out] The end position of the substring that fits.
 * @param availableWidth The space that is available for the text part.
 * @result Returns the bound box of the substring.
 */
TBoundingBox CCaptionElement::breakLine(const wstring& line, wstring::size_type start, wstring::size_type& stop, 
                                        float availableWidth)
{
  TBoundingBox localBounds;
  
  fontManager()->boundingBox(line.substr(start), FFontId, localBounds);

  if (localBounds.width() > availableWidth)
  {
    // Do a binary search for the optimal string length which fits into the given width.
    // Note: currently not RTL context is considered here. If needed it must be implemented here.
    wstring::size_type left = start;
    wstring::size_type right = line.size() - 1;
    while (left < right)
    {
      wstring::size_type center = (left + right + 1) >> 1;
      fontManager()->boundingBox(line.substr(start, center - start + 1), FFontId, localBounds);
      if (localBounds.width() <= availableWidth)
        left = center;
      else
        right = center - 1;
    };

    // Here we have the last character that still fits.
    // Check that we do not split words.
    if (left < line.size() - 1)
    {
      wstring::size_type saveLeft = left;
      while (left > 0 && !isWordBoundary(line[left]))
        --left;
	    if (left == 0)
        left = saveLeft;
      else
        // Recompute bounds as we changed the actual string again.
        fontManager()->boundingBox(line.substr(start, left - start + 1), FFontId, localBounds);
    
      stop = left + 1;
    }
    else
      stop = line.length();
  }
  else
    stop = line.length();

  return localBounds;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes the actual layout of one text line. If the given text fits entirely into the given space then it is aligned
 * as given by the FHorizontalAlignment and FVerticalAlignment properties. Otherwise the text is shortend and
 * an ellipsis (three dots) are appended to the display text (influenced by the bidi mode). If there isn't even enough
 * room for the ellipsis then no text will be displayed at all.
 *
 * @param layout [in/out] On enter contains the text to be layouted. On exit the actual display text is stored in there
 *                        as well as its actual width and position values.
 * @param availableWidth Available horizontal space to place the text in.
 * @param availableHeight Available vertical space to place the text in.
 * @param ellipsisWidth Precomputed width of the ellipsis ('...') using the current font data.
 * @parma freeSpace If true then we do not have outer bounds for alignment and/or shorten the caption. This means the full
 *                  caption is played and availableWidth and availableHeight are ignored.
 */
void CCaptionElement::computeLayout(CTextLineLayout& layout, float availableWidth, float availableHeight, float ellipsisWidth, 
                                    bool freeSpace)
{
  if (!freeSpace && (availableWidth <= ellipsisWidth || layout.text.empty()))
  {
    layout.text.clear();
    layout.xOffset = 0;
    layout.yOffset = 0;
  }
  else
  {
    if (!freeSpace && (layout.width > availableWidth))
    {
      // There is not enough space to display the full text.
      layout.xOffset = 0;
      float lastWidth = 0;
      TBoundingBox localBounds;
      
      // Do a binary search for the optimal string length which fits into the given width.
      int L = 0;
      int H = (int) layout.text.size() - 1;
      if (FBidiMode == GC_BIDI_RIGHT_TO_LEFT)
      {
        while (L < H)
        {
          int N = (L + H) >> 1;
          fontManager()->boundingBox(layout.text.substr(N), FFontId, localBounds);
          lastWidth = localBounds.lower.x - localBounds.upper.x + ellipsisWidth;
          if (lastWidth <= availableWidth)
            H = N;
          else
            L = N + 1;
        };
        layout.text = L"..." + layout.text.substr(L + 1);
      }
      else
      {
        while (L < H)
        {
          int N = (L + H + 1) >> 1;
          fontManager()->boundingBox(layout.text.substr(0, N), FFontId, localBounds);
          lastWidth = localBounds.lower.x - localBounds.upper.x + ellipsisWidth;
          if (lastWidth <= availableWidth)
            L = N;
          else
            H = N - 1;
        };
        layout.text = layout.text.substr(0, L - 1) + L"...";
      };
      layout.width = lastWidth;
    }
    else
    {
      // The full text line can be displayed. Compute actual horizontal alignment.
      TAlignment alignment = FHorizontalAlignment;

      // In right-to-left mode alignment is reverted.
      if (FBidiMode == GC_BIDI_RIGHT_TO_LEFT)
        if (alignment == GC_ALIGN_LEFT_TOP)
          alignment = GC_ALIGN_RIGHT_BOTTOM;
        else
          if (alignment == GC_ALIGN_RIGHT_BOTTOM)
            alignment = GC_ALIGN_LEFT_TOP;
    
      // If there is no space setting that we can use to align the text then
      // always align left, regardless of the actual alignment setting.
      if (freeSpace)
        alignment = GC_ALIGN_LEFT_TOP;

      switch (alignment)
      {
        case GC_ALIGN_LEFT_TOP: // Left aligned.
        {
          layout.xOffset = 0;
          break;
        };
        case GC_ALIGN_CENTER: // Centered.
        {
          layout.xOffset = (availableWidth - layout.width) / 2;
          break;
        };
        case GC_ALIGN_RIGHT_BOTTOM: // Right aligned.
        {
          layout.xOffset = availableWidth - layout.width;
          break;
        };
      };
    };
  };
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes the entire text layout of this caption. Depending on the text an enabled text wrapping this
 * might result in several output lines.
 *
 * @param availableWidth The available space that the caption can use. This is also used to center or right align the caption
 *                       or to shorten it, if it only partially fits.
 * @param availableHeight The available vertical space the caption can use. Only fullly visible lines are displayed.
 * @param totalHeight [out] Since we need this for the bounding box computation and it is here computed anyway, we return
 *                          this overall height to the caller.
 * @parma freeSpace If true then we do not have outer bounds for alignment and/or shorten the caption. This means the full
 *                  caption is played and availableWidth and availableHeight are ignored.
 * @result The computed text lines.
 */
CCaptionElement::CTextLayout CCaptionElement::createTextLayout(float availableWidth, float availableHeight, 
                                                               float& totalHeight, bool freeSpace)
{
  CTextLayout lines;
  CTextLineLayout layout;
  TBoundingBox bounds;
  TAlignment alignment = FHorizontalAlignment;
  
  // In right-to-left mode alignment is reverted.
  if (FBidiMode == GC_BIDI_RIGHT_TO_LEFT)
    if (alignment == GC_ALIGN_LEFT_TOP)
	    alignment = GC_ALIGN_RIGHT_BOTTOM;
    else
      if (alignment == GC_ALIGN_RIGHT_BOTTOM)
        alignment = GC_ALIGN_LEFT_TOP;

  fontManager()->boundingBox(L"...", FFontId, bounds);
  float ellipsisWidth = bounds.lower.x - bounds.upper.x;
  totalHeight = 0;

  if (FWrapText)
  {
    bool pendingLinebreak = false;
    StringTokenizer tokenizer(FText, L"\n\r\x2029", false);
    while (tokenizer.hasMoreTokens())
    {
      // Go for each line.
      wstring line = tokenizer.nextToken();

      // If we found a newline line break in the previous run then check for a line feed now 
      // (Windows style line breaks) and skip it.
      if ((line.length() == 0) && pendingLinebreak && (tokenizer.lastDelimiter() == '\n'))
        pendingLinebreak = false;
      else
      {
        pendingLinebreak = (tokenizer.lastDelimiter() == '\r');

        if (line.length() == 0)
        {
          // Only a line break was found. Create an empty line for display.
          layout.text = L"";
          layout.width = 0;
          layout.xOffset = 0;
          layout.yOffset = totalHeight;
          lines.push_back(layout);
          totalHeight += FLineHeight;
        }
        else
        {
          // Split the line in case it does not fit into the available space.
          wstring::size_type start = 0, stop;
          while ((start < line.length()) && (freeSpace || (totalHeight + FLineHeight < availableHeight)))
          {
            TBoundingBox lineBox = breakLine(line, start, stop, availableWidth);
            layout.text = line.substr(start, stop - start);
            start = stop;
            layout.width = lineBox.width();
            computeLayout(layout, availableWidth, availableHeight, ellipsisWidth, freeSpace);
            layout.yOffset = totalHeight;
            lines.push_back(layout);
            totalHeight += FLineHeight;
          };
        };
      };
    };
  }
  else
  {
    layout.text = FText;
    fontManager()->boundingBox(FText, FFontId, bounds);
    layout.width = bounds.width();
    layout.xOffset = 0;
    layout.yOffset = 0;
    computeLayout(layout, availableWidth, availableHeight, ellipsisWidth, freeSpace);
    lines.push_back(layout);
    totalHeight = FLineHeight;
  };

  // Rearrange the lines according to alignment.
  float offset = 0;
  switch (FVerticalAlignment)
  {
    case GC_ALIGN_LEFT_TOP: // Top aligned.
    {
      break;
    };
    case GC_ALIGN_CENTER: // Centered.
    {
      offset = (availableHeight - totalHeight) / 2;
      break;
    };
    case GC_ALIGN_RIGHT_BOTTOM: // Bottom aligned.
    {
      offset = availableHeight - totalHeight;
      break;
    };
  };

  // Apply alignment.
  if (offset != 0)
    for (wstring::size_type i = 0; i < lines.size(); ++i)
      lines[i].yOffset += offset;

  return lines;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Marks the element as changed so its bounding box it is validated next time it is used.
 *
 * @param reason An optional change reason.
 */
void CCaptionElement::makeDirty(TGCChangeReason reason)
{
  if (!dirty())
  {
    CGraphicElement::makeDirty(reason);

    if (FParent != NULL)
      FParent->makeDirty();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by name. The name syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property to return.
 * @param index If the property is a list then this is the index into that list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CCaptionElement::propertyGet(const char* name, unsigned int index)
{
  TGCVariant result;

  switch (getContainerID(name))
  {
    case GC_CONTAINER_UNKNOWN:
      {
        switch (getPropertyID(name))
        {
          case GC_PROPERTY_OWNER:
            {
              result = FParent;
              break;
            };
          case GC_PROPERTY_DESCRIPTION:
            {
              result = "The optional caption of an element";
              break;
            };
          case GC_PROPERTY_NAME:
          case GC_PROPERTY_TEXT:
            {
              result = utf16ToUtf8(FOriginalText);
              break;
            };
          case GC_PROPERTY_FONT_FAMILY:
            {
              result = FFontFamily;
              break;
            };
          case GC_PROPERTY_FONT_SIZE:
            {
              result = FFontSize;
              break;
            };
          case GC_PROPERTY_FONT_WEIGHT:
            {
              result = FWeight;
			  break;
            };
          case GC_PROPERTY_FONT_STYLE:
            {
              result = FFontStyle;
              break;
            };
          case GC_PROPERTY_COLOR:
            {
              result = colorToString(FColor);
              break;
            };
          case GC_PROPERTY_ALIGNMENT_VERTICAL:
            {
              static char* VAlignmentToString[3] = {"top", "center", "bottom"};
              result = VAlignmentToString[FVerticalAlignment];
              break;
            };
          case GC_PROPERTY_ALIGNMENT_HORIZONTAL:
            {
              static char* HAlignmentToString[3] = {"left", "center", "right"};
              result = HAlignmentToString[FHorizontalAlignment];
              break;
            };
          case GC_PROPERTY_BIDI_MODE:
            {
              static char* BidiModeToString[2] = {"left-to-right", "right-to-left"};
              result = BidiModeToString[FBidiMode];
              break;
            };
          case GC_PROPERTY_WRAP_TEXT:
            {
              result = FWrapText ? "true" : "false";
              break;
            };
          case GC_PROPERTY_MIN_WIDTH:
            {
              result = FConstraints.minWidth;
              break;
            };
          case GC_PROPERTY_MIN_HEIGHT:
            {
              result = FConstraints.minHeight;
              break;
            };
          case GC_PROPERTY_MAX_WIDTH:
            {
              result = FConstraints.maxWidth;
              break;
            };
          case GC_PROPERTY_MAX_HEIGHT:
            {
              result = FConstraints.maxHeight;
              break;
            };
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
void CCaptionElement::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  switch (getContainerID(name))
  {
	case GC_CONTAINER_UNKNOWN:
	  {
		switch (getPropertyID(name))
        {
          case GC_PROPERTY_TEXT:
            {
              FOriginalText = utf8ToUtf16(value);
              if (FOriginalText.size() > 100)
                FText = FOriginalText.substr(1, 100) + L"...";
              else
                FText = FOriginalText;
              makeDirty(GC_CHANGE_CAPTION_PROPERTY);

              break;
            };
          case GC_PROPERTY_FONT_FAMILY:
            {
              FFontFamily = (string)value;
              makeDirty(GC_CHANGE_CAPTION_PROPERTY);

              break;
            };
          case GC_PROPERTY_FONT_SIZE:
            {
              FFontSize = value;
              makeDirty(GC_CHANGE_CAPTION_PROPERTY);

              break;
            };
          case GC_PROPERTY_FONT_WEIGHT:
            {
              FWeight = value;
              makeDirty(GC_CHANGE_CAPTION_PROPERTY);

              break;
            };
          case GC_PROPERTY_FONT_STYLE:
            {
              FFontStyle = (string)value;
              makeDirty(GC_CHANGE_CAPTION_PROPERTY);

              break;
            };
          case GC_PROPERTY_COLOR:
            {
              GLfloat newColor[4];
              if (stringToColor(value, newColor) == 0)
              {
                memcpy(FColor, newColor, sizeof(newColor));
                FColor[3] = 1;
              };
              makeDirty(GC_CHANGE_CAPTION_PROPERTY);

              break;
            };
          case GC_PROPERTY_ALIGNMENT_VERTICAL:
            {
              string s = value;
              if (s == "top")
                FVerticalAlignment = GC_ALIGN_LEFT_TOP;
              else 
                if (s == "center")
                  FVerticalAlignment = GC_ALIGN_CENTER;
                else
                  if (s == "bottom")
                    FVerticalAlignment = GC_ALIGN_RIGHT_BOTTOM;
              makeDirty(GC_CHANGE_CAPTION_PROPERTY);

              break;
            };
          case GC_PROPERTY_ALIGNMENT_HORIZONTAL:
            {
              string s = value;
              if (s == "left")
                FHorizontalAlignment = GC_ALIGN_LEFT_TOP;
              else 
                if (s == "center")
                  FHorizontalAlignment = GC_ALIGN_CENTER;
                else
                  if (s == "right")
                    FHorizontalAlignment = GC_ALIGN_RIGHT_BOTTOM;
              makeDirty(GC_CHANGE_CAPTION_PROPERTY);

              break;
			};
          case GC_PROPERTY_BIDI_MODE:
            {
              string s = value;
              if (s == "left-to-right")
                FBidiMode = GC_BIDI_LEFT_TO_RIGHT;
              else 
                if (s == "right-to-left")
                  FBidiMode = GC_BIDI_RIGHT_TO_LEFT;
              makeDirty(GC_CHANGE_CAPTION_PROPERTY);

              break;
            };
          case GC_PROPERTY_WRAP_TEXT:
            {
              string s = value;
              FWrapText = s == "true";
              makeDirty(GC_CHANGE_CAPTION_PROPERTY);
            };
          case GC_PROPERTY_MIN_WIDTH:
            {
              FConstraints.minWidth = value;
              makeDirty(GC_CHANGE_ELEMENT_PROPERTY);

              break;
            };
          case GC_PROPERTY_MIN_HEIGHT:
            {
              FConstraints.minHeight = value;
              makeDirty(GC_CHANGE_ELEMENT_PROPERTY);

              break;
            };
          case GC_PROPERTY_MAX_WIDTH:
            {
              FConstraints.maxWidth = value;
              makeDirty(GC_CHANGE_ELEMENT_PROPERTY);

              break;
            };
          case GC_PROPERTY_MAX_HEIGHT:
            {
              FConstraints.maxHeight = value;
              makeDirty(GC_CHANGE_ELEMENT_PROPERTY);

              break;
            };
        };

        break;
      };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders the caption element.
 */
void CCaptionElement::render(void)
{
  if (FActualFontSize > 6 && !canvas()->currentViewGet()->inAnimation())
  {
    glPushAttrib(GL_ENABLE_BIT);
    if (canvas()->supportsExtension(GC_OE_MULTISAMPLING))
    {
      if (FActualFontSize > 20)
        glEnable(GL_MULTISAMPLE_ARB); 
      else
        glDisable(GL_MULTISAMPLE_ARB);
    };
    
    glColor4fv(FColor);

    unsigned int i;
    unsigned int count = (unsigned int) FTextLayout.size();

    for (i = 0; i < count; i++)
    {
      CTextLineLayout* layout = &FTextLayout[i];
      glRasterPos2f(FOffsetX + layout->xOffset, FOffsetY + layout->yOffset + FAscender);
      fontManager()->textOut(layout->text, FRenderFontId);
    };
    #if 0 // Render bounding box (for debugging)
      TBoundingBox temp = bounds();
      glPushAttrib(GL_CURRENT_BIT | GL_POLYGON_BIT);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glColor3f(0, 1, 0);
      glRectf(temp.upper.x, temp.upper.y, temp.lower.x, temp.lower.y);
      glPopAttrib();
    #endif 

    glPopAttrib();
  }
  else
  {
    glColor4f(FColor[0], FColor[1], FColor[2], FColor[3] * 0.25f);

    unsigned int i;
    unsigned int count = (unsigned int) FTextLayout.size();

    for (i = 0; i < count; i++)
    {
      CTextLineLayout layout = FTextLayout[i];
      glRectf(FOffsetX + layout.xOffset + 2, FOffsetY + layout.yOffset + 2, 
        FOffsetX + layout.xOffset + layout.width - 2, FOffsetY + layout.yOffset + FLineHeight - 2);
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets a new text string.
 *
 * @param newText The new text to set.
 */
void CCaptionElement::text(wstring newText)
{
  FOriginalText = newText;
  if (FOriginalText.size() > 100)
    FText = FOriginalText.substr(1, 100) + L"...";
  else
    FText = FOriginalText;
  makeDirty(GC_CHANGE_CAPTION_PROPERTY);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Validates the bounding box.
 */
void CCaptionElement::validate(void)
{
  if (dirty())
  {
    CGraphicElement::validate();

    // -- Horizontal alignment
    // If the text is larger than the available space then shorten it.
    TBoundingBox mainBounds = FParent->bounds();
    
    // Apply constraints to the bounding box.
    if (FConstraints.maxHeight > -1)
    {
      if (mainBounds.lower.y - mainBounds.upper.y > FConstraints.maxHeight)
        mainBounds.lower.y = mainBounds.upper.y + FConstraints.maxHeight;
    };
    if (FConstraints.maxWidth > -1)
    {
      if (mainBounds.lower.x - mainBounds.upper.x > FConstraints.maxWidth)
        mainBounds.lower.x = mainBounds.upper.x + FConstraints.maxWidth;
    };
    if (FConstraints.minHeight > -1)
    {
      if (mainBounds.lower.y - mainBounds.upper.y < FConstraints.minHeight)
        mainBounds.lower.y = mainBounds.upper.y + FConstraints.minHeight;
    };
    if (FConstraints.minWidth > -1)
    {
      if (mainBounds.lower.x - mainBounds.upper.x < FConstraints.minWidth)
        mainBounds.lower.x = mainBounds.upper.x + FConstraints.minWidth;
    };
    // Implicitly give it a 3 pixel border on the right hand side, to avoid a touch of text and parent border.
    float availableWidth = mainBounds.width() - FOffsetX - 3;
    float availableHeight = mainBounds.height() - FOffsetY;
    bool freeSpace = (mainBounds.width() == 0) && (mainBounds.height() == 0);

    // Get a font identifier from the font manager for our font. This will implicitely create
    // a font in the font manager if not yet done.
    FFontId = fontManager()->fontIdCreate(FFontFamily, FFontStyle, FWeight, FFontSize);
    FRenderFontId = fontManager()->fontIdCreate(FFontFamily, FFontStyle, FWeight, FActualFontSize);
    FLineHeight = fontManager()->lineHeight(FFontId);
    FAscender = fontManager()->ascender(FFontId);
 
    float totalHeight = 0;
    FTextLayout = createTextLayout(availableWidth, availableHeight, totalHeight, freeSpace);
  
    boundsNew();
    TVertex vertex;
    for (wstring::size_type i = 0; i < FTextLayout.size(); ++i)
    {
      boundsAdd(TVertex(FOffsetX + FTextLayout[i].xOffset, FOffsetY + FTextLayout[i].yOffset, 0));
      boundsAdd(TVertex(FOffsetX + FTextLayout[i].xOffset + FTextLayout[i].width, 
        FOffsetY + FTextLayout[i].yOffset + FLineHeight, 0));
    };
    boundsFinished();

    canvas()->checkError();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called when the current zoom (scale) factor has changed. Recompute font size.
 *
 * @param zoomFactor The new zoom factor.
 */
void CCaptionElement::zoomChanged(float zoomFactor)
{
  int NewFontSize = ROUND(zoomFactor * FFontSize);
  if (FActualFontSize != NewFontSize)
  {
    FActualFontSize = NewFontSize;
    makeDirty();
  };
}

//----------------- CElementListener -----------------------------------------------------------------------------------

void CFigureElement::CElementListener::onAction(CGCBase* sender, CGCBase* origin, TAction** action)
{
  element->action(origin, action);
}

//----------------------------------------------------------------------------------------------------------------------

void CFigureElement::CElementListener::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  element->change(origin, reason);
}

//----------------------------------------------------------------------------------------------------------------------

void CFigureElement::CElementListener::onDestroy(CGCBase* sender)
{
  if (sender == element->style())
    element->style(NULL);
  else
  {
    // A figure or caption element is about to be freed.
    if (strcmp(sender->className(), "CFigureElement") == 0)
      element->figure()->removeMapping((CFigureElement*) sender);
    element->removeListener(this);
  };
}

//----------------------------------------------------------------------------------------------------------------------

void CFigureElement::CElementListener::onError(CGCBase* sender, CGCBase* origin, const char* message)
{
  if (element != NULL)
    element->error(origin, message);
}

//----------------- CFigureElement -------------------------------------------------------------------------------------

CFigureElement::CFigureElement(CFigureElementTemplate* aTemplate, CGenericCanvas* canvas):CGraphicElement(canvas)
{
  _className = "CFigureElement";

  FTemplate = aTemplate;

  FTranslation[0] = 0;
  FTranslation[1] = 0;
  FTranslation[2] = 0;
  FTranslation[3] = 0;

  FLayout = GC_LAYOUT_COLUMN;
  FResizeMode = GC_RESIZE_NONE;
  FStyle = NULL;
  FParent = NULL;
  FFigure = NULL;
  FCaption = NULL;
  FLayouter = NULL;
  FExpanded = true;
  FListener.element = this;
  FForcedWidth = -1;
  FForcedHeight = -1;
  FNeedStyleScale = false;
  FStyleScaleX = 1;
  FStyleScaleY = 1;
}

//----------------------------------------------------------------------------------------------------------------------

CFigureElement::~CFigureElement(void)
{
  if (FStyle != NULL)
  {
	CGCStyle* OldStyle = FStyle;
	// Avoid recursion.
	FStyle = NULL;
	if (OldStyle != NULL && !OldStyle->destroying())
	  OldStyle->removeListener(&FListener);
  };

  for (CElementList::iterator iterator = FChildren.begin(); iterator != FChildren.end(); ++iterator)
    delete *iterator; // No need to remove the listener at this point.

  delete FCaption;
  delete FLayouter;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * (Re) computes the overall bounding box for this element. This includes the bounds of the style as well as all children.
 * It is assumed that the caller has already validated the owning figure (and so all contained elements).
 */
void CFigureElement::computeBoundingBox(void)
{
  boundsNew();

  if (FChildren.size() > 0)
  {
    TBoundingBox localBox;
    FLayouter->reset();
    while (FLayouter->hasNext())
    {
      FLayouter->nextBoundingBox(&localBox);
      boundsAdd(localBox);
    };
  }
  else
  {
    if (FStyle != NULL)
      boundsAdd(FStyle->boundingBox());
    else
      if (FCaption != NULL)
        boundsAdd(FCaption->bounds());
  };

  boundsFinished(FConstraints);

  // Override size settings if there is a forced size.
  if (FChildren.size() == 0 && (FForcedWidth >= 0 || FForcedHeight >= 0))
  {
    float implicitWidth = width();
    float implicitHeight = height();

    boundsNew();
    boundsAdd(TVertex(0, 0, 0));
    if (FForcedWidth >= 0)
      boundsAdd(TVertex(FForcedWidth, implicitHeight, 0));
    if (FForcedHeight >= 0)
      boundsAdd(TVertex(implicitWidth, FForcedHeight, 0));
    boundsFinished(FConstraints);
  };

  // Once the bounding box is computed we can determine whether the style must be scaled to fit its size.
  if (FStyle != NULL)
  {
    TBoundingBox styleBounds = FStyle->boundingBox();
    float styleWidth = styleBounds.lower.x - styleBounds.upper.x;
    float styleHeight = styleBounds.lower.y - styleBounds.upper.y;
    FStyleScaleX = width() / styleWidth;
    FStyleScaleY = height() / styleHeight;

    FNeedStyleScale = (FStyleScaleX != 1) || (FStyleScaleY != 1);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes the offset of the given element (which must be a child of this figure element) relative to its owning figure.
 *
 * @param child The child element for which the offset is requested.
 * @result The computed offset.
 */
TVertex CFigureElement::computeOffset(CFigureElement* child)
{
  TVertex result;
  if (FParent != NULL)
    result = FParent->computeOffset(this);
  
  TBoundingBox localBox = FLayouter->boundingBoxForChild(child);
  result.x += localBox.upper.x;
  result.y += localBox.upper.y;
  result.z += localBox.upper.z;

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Core action execution function.
 *
 * @param action The action to be executed.
 * @return The actual action that was executed (can be changed by the application in a callback, but is usually what was passed in).
 */
TAction* CFigureElement::doAction(TAction* action)
{                                                               
  TAction* result = action;

  // Notify listeners about the action and see if they confirm it.
  // Listeners can set the action to something else (including GC_ACTION_NONE) to control what happens.
  this->action(this, &result);
  if (result != NULL)
  {
    switch (result->type)
    {
      case GC_ACTION_TOGGLE:
        {
          // Invert expand state:
          // Synopsis: toggle(id-of-element-to-toggle, expanded-style-id, collapsed-style-id)
          if (action->parameters.size() >= 3)
          {
            CFigureElement* element = elementFromId(action->parameters[0]);
            if (element != NULL)
            {
              element->expanded(!element->expanded());
              CGCStyle* newStyle = figure()->model()->style(element->expanded() ? action->parameters[1] : action->parameters[2]);
              style(newStyle);
            }
            else
              error(this, ("CFigureElement: invalid element path %s", utf16ToUtf8(action->parameters[0])).c_str());
          }
          else
            error(this, "CFigureElement: invalid # of parameters to toggle action");
          break;
        };
      case GC_ACTION_EXPAND:
        {
          // Expand an element:
          // Synopsis: expand(id-of-element-to-expand, expanded-style-id)
          if (action->parameters.size() >= 2)
          {
            CFigureElement* element = elementFromId(action->parameters[0]);
            if (element != NULL && !element->expanded())
            {
              element->expanded(true);
              CGCStyle* newStyle = figure()->model()->style(action->parameters[1]);
              style(newStyle);
            };
          };
          break;
        };
      case GC_ACTION_COLLAPSE:
        {
          // Collapse an element:
          // Synopsis: collapse(id-of-element-to-collapse, collapsed-style-id)
          if (action->parameters.size() >= 2)
          {
            CFigureElement* element = elementFromId(action->parameters[0]);
            if (element != NULL && element->expanded())
            {
              element->expanded(false);
              CGCStyle* newStyle = figure()->model()->style(action->parameters[1]);
              style(newStyle);
            };
          };
          break;
        };
      case GC_ACTION_CHANGE_STYLE:
        {
          // Change the style of an element:
          // Synopsis: changeStyle(id-of-element-to-change, new-style-id)
          if (action->parameters.size() >= 1)
          {
            CFigureElement* element = elementFromId(action->parameters[0]);
            if (element != NULL)
            {
              CGCStyle* newStyle = figure()->model()->style(action->parameters[1]);
              style(newStyle);
            };
          };
          break;
        };
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Helper method to explicitely trigger creation of a child figure element.
 * The point here is that this must only be called for figures that represent lists, that is,
 * figures that only have one child template element, which can appear any number of times.
 */
CFigureElement* CFigureElement::addSubElement(void)
{
  CFigureElement* result = NULL;
  if (FTemplate != NULL)
  {
    CFigureElementTemplate* child = FTemplate->getListElement();
    // There is only a list element if the template is actually a list.
    if (child != NULL)
    {
      result = CFigureElement::createFromTemplate(canvas(), FFigure, child);
      if (result)
      {
        result->FParent = this;
        result->addListener(&FListener);
        FFigure->addMapping(child->key(), result);
        FChildren.push_back(result);

        change(this, GC_CHANGE_ELEMENT_ADD_CHILD);
        makeDirty();
      };
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines whether this element can be resized in the given direction.
 *
 * @param vertical If true then the element is checked for vertical size changes, otherwise for horizontal changes.
 * @return true if the element can be resized in the given direction, otherwise false.
 */
bool CFigureElement::canResize(bool vertical)
{
  if (vertical)
    return (FResizeMode == GC_RESIZE_VERTICAL_ONLY) || (FResizeMode == GC_RESIZE_ALL);
  else
    return (FResizeMode == GC_RESIZE_HORIZONTAL_ONLY) || (FResizeMode == GC_RESIZE_ALL);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a figure element from a layoutTemplate and returns it.
 * 
 * @param owner The controller for the new figure element. It is responsible to free the returned instance.
 * @param layoutTemplate The layoutTemplate to be used when creating the figure element.
 * @param Model The model to which the new element belongs.
 *
 * @return The new figure element instance.
 */
CFigureElement* CFigureElement::createFromTemplate(CGenericCanvas* canvas, CFigure* owner, 
                                                   CFigureElementTemplate* layoutTemplate)
{
  CFigureElement* newElement = NULL;
  float currentZoom = 1;
  
  if (canvas->currentViewGet() != NULL)
    currentZoom = canvas->currentViewGet()->zoomGet();

  if (layoutTemplate != NULL)
  {
    newElement = new CFigureElement(layoutTemplate, canvas);
    newElement->FLayout = layoutTemplate->FLayout;
    newElement->FLayouter = LayoutMapper::layouterForElement(newElement);
    newElement->FResizeMode = layoutTemplate->FResizeMode;
    newElement->style(layoutTemplate->FStyle);
    newElement->FConstraints = layoutTemplate->FConstraints;

    CCaptionElementTemplate* CaptionTemplate = layoutTemplate->FCaption;
    if (CaptionTemplate != NULL)
    {
      newElement->FCaption = new CCaptionElement(canvas);
      newElement->FCaption->addListener(&newElement->FListener);
      newElement->FCaption->FOriginalText = CaptionTemplate->FText;
      if (newElement->FCaption->FOriginalText.size() > 100)
        newElement->FCaption->FText = newElement->FCaption->FOriginalText.substr(1, 100) + L"...";
      else
        newElement->FCaption->FText = newElement->FCaption->FOriginalText;
      newElement->FCaption->FFontFamily = CaptionTemplate->FFontFamily;
      newElement->FCaption->FFontSize = CaptionTemplate->FFontSize;
      newElement->FCaption->FActualFontSize = ROUND(currentZoom * CaptionTemplate->FFontSize);
      newElement->FCaption->FWeight = CaptionTemplate->FWeight;
      newElement->FCaption->FFontStyle = CaptionTemplate->FFontStyle;
      newElement->FCaption->FHorizontalAlignment = CaptionTemplate->FHorizontalAlignment;
      newElement->FCaption->FVerticalAlignment = CaptionTemplate->FVerticalAlignment;
      newElement->FCaption->FParent = newElement;
      newElement->FCaption->FHasColor = CaptionTemplate->FHasColor;
      for (int j = 0; j < 4; ++j)
        newElement->FCaption->FColor[j] = CaptionTemplate->FColor[j];
      newElement->FCaption->FConstraints = CaptionTemplate->FConstraints;
      newElement->FCaption->FOffsetX = CaptionTemplate->FOffsetX;
      newElement->FCaption->FOffsetY = CaptionTemplate->FOffsetY;
      newElement->FCaption->FWrapText = CaptionTemplate->FWrapText;
    }
    else
      newElement->FCaption = NULL;

    // Child elements.
    for (CElementTemplateList::iterator iterator = layoutTemplate->FChildren.begin(); iterator != layoutTemplate->FChildren.end(); 
      ++iterator)
    {
      CFigureElementTemplate* template_ = *iterator;
      if (template_->occurence() != GC_OCC_ZERO_OR_MORE)
      {
        CFigureElement* childElement = CFigureElement::createFromTemplate(canvas, owner, template_);
        if (childElement != NULL)
        {
          childElement->FParent = newElement;
          childElement->addListener(&newElement->FListener);
          newElement->FChildren.push_back(childElement);
          owner->addMapping(template_->key(), childElement);
        };
      };
    };

    newElement->FFigure = owner;
  };

  return newElement;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines the child figure element with the given xml id and returns it. The identifier might contain a path
 * which is then analyzed and followed (similar to XPath).
 *
 * @param id The id to search for.
 * @return The figure element that corresponds to the given id or NULL if there is none.
 */
CFigureElement* CFigureElement::elementFromId(wstring id)
{
  CFigureElement* result = NULL;

  if (!id.empty())
  {
    bool doChildScan = true;

    if (id[0] == '/')
    {
      // If the given id contains an absolute path then start searching at figure level.
      if (FFigure != NULL)
        result = FFigure->elementFromId(id);
    }
    else
    {
      // Is a path included?
      wstring path;
      wstring rest;
      wstring::size_type position = id.find_first_of('/');
      if (position != wstring::npos)
      {
        path = id.substr(0, position);
        rest = id.substr(position + 1);

        // Handle special cases first.
        if (path == L".")
        {
          path.clear();
          doChildScan = false;

          // An id of "./" addresses this element.
          if (rest.empty())
            result = this;
          else
            // If there is more to parse then call this method recursively
            // to handle other potential special cases.
            result = elementFromId(rest);
        }
        else
          if (path == L"..")
          {
            doChildScan = false;
            if (FParent != NULL)
              result = FParent->elementFromId(rest);
          };
      }
      else
        path = id;

      if (doChildScan)
      {
        for (CElementList::iterator iterator = FChildren.begin(); iterator != FChildren.end(); ++iterator)
        {
          if ((*iterator)->FTemplate->FId == path)
          {
            // If no more sub parts need to be parsed then return the found element
            // otherwise use this to scan deeper.
            if (rest.empty())
              result = *iterator;
            else
              result = (*iterator)->elementFromId(rest);
            break;
          };
        };
      };
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the figure element that contains the given point.
 *
 * @param coords The point to check for in figure space.
 * @return NULL or a figure element occupying the given point.
 */
CFigureElement* CFigureElement::elementFromPoint(TVertex coords)
{
  CFigureElement* result = NULL;

  if (containsPoint(coords.x, coords.y))
  {
    if (FExpanded)
    {
      FLayouter->reset();
      while (FLayouter->hasNext())
      {
        result = FLayouter->elementFromPoint(coords);
        if (result != NULL) 
          break;
      };
    };

    if (result == NULL)
      result = this;
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given coordinates from local space to figure space (of the figure that owns this element).
 * 
 * @param coords The coordinates to convert.
 * @result The converted coordinates in figure space.
 * @note This function is quite time consuming as it requires a recursive computation.
 */
TVertex CFigureElement::elementToFigure(TVertex coords)
{
  TVertex result = coords;

  if (FParent != NULL)
  {
    TVertex offset = FParent->computeOffset(this);
    result.x += offset.x;
    result.y += offset.y;
    result.z += offset.z;
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the actions of the figure element that is located at the given location. The given coordinates are
 * already in element space, so no further conversion is needed.
 * Only actions with the correct trigger conditions are executed.
 *
 * @param button The mouse button which triggered the call.
 * @param event The event which triggered the action.
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The mouse coordinates in element space.
 * @return The last executed action or NULL if none.
 */
TAction* CFigureElement::executeAssociatedActions(TMouseButton button, TMouseEvent event, TModifiers modifiers, const TVertex& coords)
{
  TAction* result = NULL;

  if (containsPoint(coords.x, coords.y))
  {
    if (FExpanded)
    {
      FLayouter->reset();
      while (FLayouter->hasNext())
      {
        result = FLayouter->executeAssociatedActions(button, event, modifiers, coords);
        if (result != NULL && result->type != GC_ACTION_NONE) 
          break;
      };
    };

    if (result == NULL && FTemplate != NULL)
    {
      // None of the child element has had an action that could be used.
      // So execute those assigned to this element, if there are any.
      // Only actions which fit the given trigger conditions are considered.
      if (FTemplate->actions().size() > 0)
      {
        for (CActions::iterator iterator = FTemplate->actions().begin(); iterator != FTemplate->actions().end(); ++iterator)
        {
          TAction* action = (*iterator);

          // If the action has no triggeres assigned then execute it unconditionally.
          if (action->triggers.size() == 0)
            result = doAction(action);
          else
            for (CTriggers::iterator triggerIterator = action->triggers.begin(); triggerIterator != action->triggers.end(); ++triggerIterator)
            {
              if ((triggerIterator->button == GC_MOUSE_BUTTON_IGNORE || triggerIterator->button == button) &&
                (triggerIterator->modifiers == GC_MODIFIER_IGNORE || triggerIterator->modifiers == modifiers) &&
                (triggerIterator->event == GC_MOUSE_IGNORE || triggerIterator->event == event))
              {
                result = doAction(action);

                // Once we found a valid trigger we can continue with the next action.
                break;
              };
            };
        };
      };
    };
  };


  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Executes a single action by looking it up in the associated action list. They first action entry that corresponds to
 * the given action type is executed (if any) using the defined parameters.
 * Note: Eventually assigned triggeres are not checked. If an action of the given type is found then it will be executed.
 *
 * @param action The type of the action to be executed.
 * @return The actual action that was executed or that must be handled on a high level.
 */
TAction* CFigureElement::executeAction(TActionType action)
{
  TAction* result = NULL;

  if (FTemplate != NULL && FTemplate->actions().size() > 0)
  {
    for (CActions::iterator iterator = FTemplate->actions().begin(); iterator != FTemplate->actions().end(); ++iterator)
      if ((*iterator)->type == action)
      {
        result = doAction(*iterator);
        break;
      };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the expanded state of the element.
 *
 * @param isExpanded The new expand state.
 */
void CFigureElement::expanded(bool isExpanded)
{
  if (FExpanded != isExpanded)
  {
    FExpanded = isExpanded;

    // Do not pass the change reason to the makeDirty call to avoid a flood of change events
    // for the entire parent chain. Instead trigger it once here.
    change(this, GC_CHANGE_ELEMENT_PROPERTY);
    makeDirty();
    if (FExpanded && FCaption != NULL)
      FCaption->makeDirty();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given coordinates from figure space (of the figure that owns this element) to local space.
 * 
 * @param coords The coordinates to convert.
 * @result The converted coordinates in element space.
 * @note This function is quite time consuming as it requires a recursive computation.
 */
TVertex CFigureElement::figureToElement(TVertex coords)
{
  TVertex result;

  if (FParent != NULL)
  {
    TVertex offset = FParent->computeOffset(this);
    result.x -= offset.x;
    result.y -= offset.y;
    result.z -= offset.z;
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

const
  TFeedbackInfo ActionToFiMapper[] =
  {
    GC_FI_NONE,              // GC_ACTION_NONE
    GC_FI_TOGGLE,            // GC_ACTION_TOGGLE
    GC_FI_EXPAND,            // GC_ACTION_EXPAND
    GC_FI_COLLAPSE,          // GC_ACTION_COLLAPSE
    GC_FI_CHANGE_STYLE,      // GC_ACTION_CHANGE_STYLE
    GC_FI_DRAG_FIGURE,       // GC_ACTION_DRAG_FIGURE
    GC_FI_DRAG_ALL,          // GC_ACTION_DRAG_ALL
    GC_FI_CONNECTION         // GC_ACTION_CONNECTION
  };

/**
 * Used to determine more specifically what action they could execute at the specified location.
 * This method is mainly used to determine visual feedback for user in a frequent manner (e.g. cursor image), so
 * actual implementations should be fast.
 *
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The mouse coordinates in element space.
 * @return Returns a TFeedbackInfo result specifying what action could take place at the given location.
 */
TFeedbackInfo CFigureElement::getFeedbackInfo(TModifiers modifiers, const TVertex& coords)
{
  TFeedbackInfo result = GC_FI_NONE;

  if (containsPoint(coords.x, coords.y))
  {
    if (FExpanded)
    {
      FLayouter->reset();
      while (FLayouter->hasNext())
      {
        result = FLayouter->getFeedbackInfo(modifiers, coords);
        if (result != GC_FI_NONE) 
          break;
      };
    };

    if ((result == GC_FI_NONE) && FTemplate != NULL && FTemplate->actions().size() > 0)
    {
      // None of the child element has had an action that could be used.
      // So take the first one assigned to this element, if there is any and current input fits the assigned triggers.
      bool foundAction = false;
      TAction* action = NULL;
      for (CActions::iterator iterator = FTemplate->actions().begin(); iterator != FTemplate->actions().end(); ++iterator)
      {
        action = (*iterator);

        // If the action has no triggeres assigned then take that unconditionally.
        if (action->triggers.size() == 0)
          foundAction = true;
        else
          for (CTriggers::iterator triggerIterator = action->triggers.begin(); triggerIterator != action->triggers.end(); ++triggerIterator)
          {
            if (triggerIterator->modifiers == GC_MODIFIER_IGNORE || triggerIterator->modifiers == modifiers)
            {
              foundAction = true;
              break;
            };
          };
        if (foundAction)
          break;
      };

      if (foundAction)
      {
        if (action->type < GC_ACTION_RESIZE)
          result = ActionToFiMapper[action->type];
        else
          if (action->type == GC_ACTION_RESIZE)
            result = action->feedbackInfo;
      };
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called when either a child element or the style changes. The event is propagated to the parent element.
 *
 * @param reason An optional reason.
 */
void CFigureElement::makeDirty(TGCChangeReason reason)
{
  if (!updating() && !dirty())
  {
    CGraphicElement::makeDirty(reason);

    if (FParent != NULL)
      FParent->makeDirty(reason);
    else
      if (FFigure != NULL)
      {
        if (reason == GC_CHANGE_NONE)
          reason = GC_CHANGE_ELEMENT_PROPERTY;
        FFigure->makeDirty(reason);
      };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by name. The name syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property.
 * @param index If the property is a list then this is the index into that list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CFigureElement::propertyGet(const char* name, unsigned int index)
{
  TGCVariant result;

  switch (getContainerID(name))
  {
    case GC_CONTAINER_UNKNOWN:
      {
		switch (getPropertyID(name))
        {
          case GC_PROPERTY_OWNER:
            {
              if (FParent != NULL)
                result = FParent;
              else
                result = FFigure;
              break;
            };
          case GC_PROPERTY_NAME:
            {
              if (FStyle == NULL)
                result = "style element";
              else
              {
                result = FStyle->propertyGet("name", 0);
                result += " element";
              };

              break;
            };
          case GC_PROPERTY_DESCRIPTION:
            {
              result = "The visual atom of which figures consist";
              
              break;
            };
          case GC_PROPERTY_LAYOUT:
            {
              switch (FLayout)
              {
                case GC_LAYOUT_ROW: 
                  {
                    result = "row";
                    break;
                  };
                case GC_LAYOUT_COLUMN:
                  {
					result = "column";
                    break;
                  };
              };
              break;
            };
          case GC_PROPERTY_RESIZABLE:
            {
              result = FResizeMode;
              break;
            };
          case GC_PROPERTY_EXPANDED:
            {
              result = FExpanded;
              break;
            };
          case GC_PROPERTY_MIN_WIDTH:
            {
              result = FConstraints.minWidth;
              break;
            };
          case GC_PROPERTY_MIN_HEIGHT:
            {
              result = FConstraints.minHeight;
              break;
            };
          case GC_PROPERTY_MAX_WIDTH:
            {
              result = FConstraints.maxWidth;
              break;
            };
          case GC_PROPERTY_MAX_HEIGHT:
            {
              result = FConstraints.maxHeight;
              break;
            };
        };
        break;
      };
	case GC_CONTAINER_STYLE:
      {
        result = FStyle;
        break;
      };
    case GC_CONTAINER_TRANSLATION:
      {
        if (index < 3)
          result = FTranslation[index];
        break;
      };
    case GC_CONTAINER_CHILDREN:
      {
        if (index < FChildren.size())
          result = FChildren[index];
        break;
      };
    case GC_CONTAINER_CAPTION:
      {
        result = FCaption;
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
 * @param value The new value of the property. Automatic conversion is performed where possible.
 */
void CFigureElement::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  switch (getContainerID(name))
  {
    case GC_CONTAINER_UNKNOWN:
      {
		switch (getPropertyID(name))
        {
          case GC_PROPERTY_LAYOUT:
            {
              string s = value;
              if (s == "row")
                FLayout = GC_LAYOUT_ROW;
              else 
                if (s == "column")
                  FLayout = GC_LAYOUT_COLUMN;
              makeDirty(GC_CHANGE_ELEMENT_PROPERTY);
              break;
            };
          case GC_PROPERTY_RESIZABLE:
            {
              string s = value;
              if (s == "horizontal")
                FResizeMode = GC_RESIZE_HORIZONTAL_ONLY;
              else 
                if (s == "vertical")
                  FResizeMode = GC_RESIZE_VERTICAL_ONLY;
                else
                  if (s == "both")
                    FResizeMode = GC_RESIZE_ALL;
                  else
                    FResizeMode = GC_RESIZE_NONE;
              change(this, GC_CHANGE_ELEMENT_PROPERTY);
              break;
            };
          case GC_PROPERTY_EXPANDED:
            {
              expanded(value);
              break;
            };
          case GC_PROPERTY_MIN_WIDTH:
            {
              FConstraints.minWidth = value;
              makeDirty(GC_CHANGE_ELEMENT_PROPERTY);
              break;
			};
          case GC_PROPERTY_MIN_HEIGHT:
            {
              FConstraints.minHeight = value;
              makeDirty(GC_CHANGE_ELEMENT_PROPERTY);
              break;
            };
          case GC_PROPERTY_MAX_WIDTH:
            {
              FConstraints.maxWidth = value;
              makeDirty(GC_CHANGE_ELEMENT_PROPERTY);
              break;
            };
          case GC_PROPERTY_MAX_HEIGHT:
            {
              FConstraints.maxHeight = value;
              makeDirty(GC_CHANGE_ELEMENT_PROPERTY);
              break;
            };
        };

        break;
      };
      case GC_CONTAINER_STYLE:
        {
          // Be flexible, accept a new style as string or as reference.
          CGCStyle* newStyle = NULL;
          if (value.isString())
            newStyle = FFigure->model()->style(utf8ToUtf16(value));
          else
            if (value.isReference() && value->classIs("CGCStyle"))
            {
              CGCBase* temp = value;
              newStyle = (CGCStyle*) temp;
            };

          if (newStyle != NULL)
            style(newStyle);

          break;
        };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * This method renders the element and triggers rendering of its (potential) caption as well as its child elements.
 */
void CFigureElement::render()
{
  if (FStyle != NULL)
  {
    if (FNeedStyleScale)
    {
      glPushMatrix();
      glScalef(FStyleScaleX, FStyleScaleY, 1);
    };
    glCallList(FStyle->displayList());
    if (FNeedStyleScale)
      glPopMatrix();
  };

  if (FExpanded)
  {
    FLayouter->reset();
    while (FLayouter->hasNext())
      FLayouter->renderNext();
  };

  if (FCaption != NULL)
    FCaption->render();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called when by user input a figure must be resized. The current view is handling user input and forwards the
 * appropriate call to the figure instance.
 *
 * @param dX Horizontal amount to resize.
 * @param dY vertical amount to resize.
 * @param info Result of the feedback hit test that tells us how to resize.
 * @return true if the element was actually resized, otherwise false.
 */
bool CFigureElement::resize(float dX, float dY, TFeedbackInfo info)
{
  bool result = false;
  bool resizeHorizontal = false;
  bool resizeVertical = false;
  
  switch (info)
  {
    case GC_FI_RESIZE_EAST:
    case GC_FI_RESIZE_WEST:
    case GC_FI_RESIZE_NORTH_EAST:
    case GC_FI_RESIZE_SOUTH_EAST:
    case GC_FI_RESIZE_SOUTH_WEST:
    case GC_FI_RESIZE_NORTH_WEST:
      {
        resizeHorizontal = canResize(false);
        break;
      };
  };
  switch (info)
  {
    case GC_FI_RESIZE_NORTH:
    case GC_FI_RESIZE_SOUTH:
    case GC_FI_RESIZE_NORTH_EAST:
    case GC_FI_RESIZE_SOUTH_EAST:
    case GC_FI_RESIZE_SOUTH_WEST:
    case GC_FI_RESIZE_NORTH_WEST:
      {
        resizeVertical = canResize(true);
        break;
      };
  };

  if (!resizeHorizontal)
    dX = 0;
  if (!resizeVertical)
    dY = 0;

  if (resizeHorizontal)
  {
    float minimalWidth = 0;
    if (FConstraints.minWidth > -1)
      minimalWidth = FConstraints.minWidth;

    // Start resizing from the current size if this is the first resize action.
    if (FForcedWidth < 0)
      FForcedWidth = width();
    FForcedWidth += dX;

    // Enforce constraints, avoid resizing child elements if a limit is hit.
    if (FForcedWidth < minimalWidth)
    {
      dX -= FForcedWidth - minimalWidth;
      FForcedWidth = minimalWidth;
    };
    if (FConstraints.maxWidth > -1 && FForcedWidth > FConstraints.maxWidth)
    {
      dX -= FForcedWidth - FConstraints.maxWidth;
      FForcedWidth = FConstraints.maxWidth;
    };
  };
  if (resizeVertical)
  {
    float minimalHeight = 0;
    if (FConstraints.minHeight > -1)
      minimalHeight = FConstraints.minHeight;

    if (FForcedHeight < 0)
      FForcedHeight = height();
    FForcedHeight += dY;

    // Enforce constraints, avoid resizing child elements if a limit is hit.
    if (FForcedHeight < minimalHeight)
    {
      dY -= FForcedHeight - minimalHeight;
      FForcedHeight = minimalHeight;
    };
    if (FConstraints.maxHeight > -1 && FForcedHeight > FConstraints.maxHeight)
    {
      dY -= FForcedHeight - FConstraints.maxHeight;
      FForcedHeight = FConstraints.maxHeight;
    };
  };

  if (dX != 0 || dY != 0)
  {
    if (FChildren.size() == 0)
    {
      // When there are no child elements then the size change is always used for resizing.
      makeDirty(GC_CHANGE_FIGURE_RESIZE);
      result = true;
    }
    else
      result = FLayouter->distributeSizeChange(dX, dY, info);

    if (result && FCaption != NULL)
      FCaption->makeDirty();
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Convenience method to set the text of the caption of this element (if there is a caption at all).
 *
 * @param text The new text to display. Must be UTF-8 encoded.
 */
void CFigureElement::setCaption(const char* text)
{
  if (FCaption != NULL)
  {
    wstring newText = utf8ToUtf16(text);
    FCaption->text(newText);
    makeDirty(GC_CHANGE_ELEMENT_PROPERTY);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets a new style to be used for this element.
 *
 * @param newStyle The new style to be used.
 */
void CFigureElement::style(CGCStyle* newStyle)
{
  if (FStyle != newStyle)
  {
    if (FStyle != NULL && !FStyle->destroying())
      FStyle->removeListener(&FListener);
    FStyle = newStyle;
    if (FStyle != NULL)
      FStyle->addListener(&FListener);

    makeDirty(GC_CHANGE_ELEMENT_PROPERTY);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the currently used style.
 *
 * @return The current style for this element.
 */
CGCStyle* CFigureElement::style(void)
{
  return FStyle;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called before the owner figure creates its display list, so it can be used to create anything necessary that
 * must not be done while a display list is being compiled.
 */
void CFigureElement::validate(void)
{
  if (dirty())
  {
    CGraphicElement::validate();

    for (CElementList::iterator iterator = FChildren.begin(); iterator != FChildren.end(); ++iterator)
      (*iterator)->validate();
    computeBoundingBox();

    if (FCaption != NULL)
      FCaption->validate();

    canvas()->checkError();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called when the current scale factor was changed. Recompute caption if necessary.
 *
 * @param newZoom The current zoom (scale) factor.
 */
void CFigureElement::zoomChanged(float newZoom)
{
  if (FCaption != NULL)
    FCaption->zoomChanged(newZoom);

  for (CElementList::iterator iterator = FChildren.begin(); iterator != FChildren.end(); ++iterator)
    (*iterator)->zoomChanged(newZoom);
}

//----------------- CFigureELementListener -----------------------------------------------------------------------------

void CFigure::CFigureElementListener::onAction(CGCBase* sender, CGCBase* origin, TAction** actionType)
{
  figure->action(origin, actionType);
}

//----------------------------------------------------------------------------------------------------------------------

void CFigure::CFigureElementListener::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  figure->change(origin, reason);
}

//----------------------------------------------------------------------------------------------------------------------

void CFigure::CFigureElementListener::onDestroy(CGCBase* sender)
{
  CFigureElement* element = (CFigureElement*) sender;
  if (element != NULL)
    figure->freeNotification(element);
}

//----------------------------------------------------------------------------------------------------------------------

void CFigure::CFigureElementListener::onError(CGCBase* sender, CGCBase* origin, const char* message)
{
  figure->error(origin, message);
}

//----------------- CFigure --------------------------------------------------------------------------------------------

CFigure::CFigure(CGCModel* owner, CFigureTemplate* layoutTemplate): CGraphicElement(owner->canvas())
{
  _className = "CFigure";
  FModel = owner;
  if (owner != NULL)
    owner->addFigure(this);
  FContent = NULL;
  FLastZoom = -1;

  // Initialize with useful values.
  memcpy(FMatrix, identity, sizeof(TMatrix));
  memcpy(FInverse, identity, sizeof(TMatrix));

  FRotation[0] = 0;
  FRotation[1] = 0;
  FRotation[2] = 0;
  FRotation[3] = 1;

  FScaling[0] = 1;
  FScaling[1] = 1;
  FScaling[2] = 1;

  FTranslation[0] = 0;
  FTranslation[1] = 0;
  FTranslation[2] = 0;

  FDisplayList = 0;

  FTemplate = NULL;
  buildFromTemplate(layoutTemplate);

  FListener.figure = this;
}

//----------------------------------------------------------------------------------------------------------------------

CFigure::~CFigure(void)
{
  delete FContent;
  if (FDisplayList != 0)
    glDeleteLists(FDisplayList, 1);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Applies the current translation, rotation and scalie factors.
 */
void CFigure::applyTransformations(void)
{
  glTranslatef(FTranslation[0], FTranslation[1], FTranslation[2]);
  // Index 0 contains the angle, while the other three coordinates form the axis to rotate around.
  glRotatef(FRotation[0], FRotation[1], FRotation[2], FRotation[3]);
  glScalef(FScaling[0], FScaling[1], FScaling[2]);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses the given layoutTemplate and creates its child structure.
 */
void CFigure::buildFromTemplate(CFigureTemplate* layoutTemplate)
{
  if (layoutTemplate->content() == NULL)
  {
    wstring name = layoutTemplate->type() + L"@" + layoutTemplate->layoutClass();
    string message = "CFigure::buildFromTemplate: No content for layoutTemplate \"" + utf16ToUtf8(name) + "\" found.";
    error(this, message.c_str());
  }
  else
  {
    if (FContent != NULL)
    {
      delete FContent; // No need to remove the listener here.
      FContent = NULL;
    };

    FTemplate = layoutTemplate;
    FContent = CFigureElement::createFromTemplate(canvas(), this, layoutTemplate->content());
    if (FContent != NULL)
      FContent->addListener(&FListener);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Recomputes the local transformation matrix.
 */
void CFigure::updateMatrix(void)
{
  memcpy(FMatrix, identity, sizeof(TMatrix));
  matrixTranslate(FMatrix, FTranslation[0], FTranslation[1], FTranslation[2]);
  matrixRotate(FMatrix, FRotation[0], FRotation[1], FRotation[2], FRotation[3]);
  matrixScale(FMatrix, FScaling[0], FScaling[1], FScaling[2]);

  matrixInvert(FMatrix, FInverse);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Tells the figure's elements that the current zoom has changed (needed for text adjustments).
 *
 * @param newZoom The new zoom value.
 */
void CFigure::zoomChanged(float newZoom)
{
  if (FContent != NULL)
    FContent->zoomChanged(newZoom);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds a new mapping between the given key and element to the figure, for later lookup.
 *
 * @param path The key used for the mapping.
 * @param element The element to map to the given key.
 */
void CFigure::addMapping(wstring path, CFigureElement* element)
{
  if (path.size() > 0)
    FElementMap[path] = element;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines the child figure element with the given xml id and returns it. The identifier might contain a path
 * which is used similar to XPath to retrieve the sub element.
 *
 * @param id The id to search for. Must always begin with a slash (/).
 * @return The figure element that corresponds to the given id or NULL if there is none.
 */
CFigureElement* CFigure::elementFromId(const wstring& id)
{
  if (FContent != NULL && !id.empty() && id[0] == '/')
    return FContent->elementFromId(id.substr(1));
  else
    return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines the child figure element with the given key and returns it.
 *
 * @param key The key to search for (UTF-8 encoded).
 * @return The figure element that corresponds to the given key or NULL if there is none.
 */
CFigureElement* CFigure::elementFromKey(const char* key)
{
  return elementFromKey(utf8ToUtf16(key));
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines the child figure element with the given key and returns it.
 * See also description of CFigureElement::elementFromKey.
 *
 * @param key The key to search for.
 * @return The figure element that corresponds to the given key or NULL if there is none.
 */
CFigureElement* CFigure::elementFromKey(wstring key)
{
  CFigureElement* result = NULL;
  CFigureElementMap::iterator iterator = FElementMap.find(key);
  if (iterator != FElementMap.end())
    result = iterator->second;

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the figure element that contains the given point.
 *
 * @param coords The point to check for in figure space.
 * @return NULL or a figure element occupying the given point.
 */
CFigureElement* CFigure::elementFromPoint(TVertex coords)
{
  if (FContent != NULL)
    return FContent->elementFromPoint(coords);
  else
    return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given coordinates from local space to view space.
 *
 * @param original The coordinates to convert to view space.
 * @param instance The instance relative to which the coordinates are to be converted.
 * @param result [out] Converted coordinates.
 */
void CFigure::figureToView(TVertex original, CFigureInstance* instance, TVertex& result)
{
  validate();

  result = matrixTransform(FMatrix, original);

  if (instance != NULL)
    instance->instanceToView(result, result);
}

//----------------------------------------------------------------------------------------------------------------------

void CFigure::freeNotification(CFigureElement* object)
{
  if (object == FContent)
  {
    FContent = NULL;
    makeDirty();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by name. The name syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property.
 * @param index If the property is a list then this is the index into that list.
 * @return The value of the property, if found.
 */
TGCVariant CFigure::propertyGet(const char* name, unsigned int index)
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
              if (FTemplate == NULL)
                result = "figure";
              else
                result = utf16ToUtf8(FTemplate->type() + L"@" + FTemplate->layoutClass() + L" figure");
              break;
            };
          case GC_PROPERTY_DESCRIPTION:
            {
              result = "The visual base element of the canvas";
              break;
            };
          case GC_PROPERTY_OWNER:
            {
              result = FModel;
              break;
            };
        };
        break;
      };
    case GC_CONTAINER_SCALING:
      {
        if (index < 3)
        {
          result = FScaling[index];
        };
        break;
      };
    case GC_CONTAINER_TRANSLATION:
      {
        if (index < 3)
        {
          result = FTranslation[index];
        };
        break;
      };
	case GC_CONTAINER_ROTATION:
      {
        if (index < 4)
        {
          result = FRotation[index];
        };

        break;
      };
    case GC_CONTAINER_CONTENT:
      {
        result = FContent;

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
void CFigure::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  switch (getContainerID(name))
  {
	case GC_CONTAINER_UNKNOWN:
	  {
		switch (getPropertyID(name))
		{
		  case GC_PROPERTY_DESCRIPTION:
			{
			  // This branch only as a placeholder.
              break;
            };
        };

        break;
      };
    case GC_CONTAINER_SCALING:
      {
        if (index < 3)
        {
          FScaling[index] = value;
          change(this, GC_CHANGE_FIGURE_PROPERTY);
          makeDirty();
        };

        break;
      };
    case GC_CONTAINER_TRANSLATION:
      {
        if (index < 3)
        {
          FTranslation[index] = value;
          change(this, GC_CHANGE_FIGURE_PROPERTY);
          makeDirty();
        };

        break;
      };
    case GC_CONTAINER_ROTATION:
      {
        if (index < 4)
        {
          FRotation[index] = value;
          change(this, GC_CHANGE_FIGURE_PROPERTY);
		  makeDirty();
        };

        break;
      };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes a previously added mapping (@see addMapping).
 *
 * @param element The element to map to the given key.
 */
void CFigure::removeMapping(CFigureElement* element)
{
  wstring key = element->template_()->key();
  CFigureElementMap::iterator iterator = FElementMap.find(key);
  if (iterator != FElementMap.end())
    FElementMap.erase(iterator);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Executes the figure's display list.
 */
void CFigure::render(void)
{
  if (FContent != NULL)
  {
    glPushMatrix();
    applyTransformations();

    glCallList(FDisplayList);

    glPopMatrix();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Turns the figure around the given axis by the angle Angle (in radians). This version of Rotate uses a single float 
 * values in the parameter list.
 *
 * @param Angle The angle in radians to turn the figure.
 * @param Rx The x part of the rotation axis.
 * @param Ry The y part of the rotation axis.
 * @param Rz The z part of the rotation axis.
 * @note Currently there is no accumulative version of Rotate available (requires a quaternion lib, which we don't have yet).
 */
void CFigure::rotate(float Angle, float Rx, float Ry, float Rz)
{
  FRotation[0] = Angle;
  FRotation[1] = Rx;
  FRotation[2] = Ry;
  FRotation[3] = Rz;
  change(this, GC_CHANGE_FIGURE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Turns the figure around the given axis by the angle Angle (in radians). This version of Rotate uses a vector for the
 * rotation axis in the parameter list.
 *
 * @param Angle The angle in radians to turn the figure.
 * @param Axis The axis around which the figure is to be rotated.
 * @note Currently there is no accumulative version of Rotate available (requires a quaternion lib, which we don't have yet).
 */
void CFigure::rotateV(float Angle, const float Axis[3])
{
  FRotation[0] = Angle;
  FRotation[1] = Axis[0];
  FRotation[2] = Axis[1];
  FRotation[3] = Axis[2];
  change(this, GC_CHANGE_FIGURE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Scales the figure by the amount given in Factor. If Accumulative is true then the new scale factors are multiplied
 * with the existing values. This version of Scale uses single float values as parameters.
 *
 * @param Sx The scale factor in x direction.
 * @param Sy The scale factor in y direction.
 * @param Sz The scale factor in z direction.
 * @param Accumulative If true then the given values are added to any existing values otherwiese they are used as given.
 */
void CFigure::scale(float Sx, float Sy, float Sz, bool Accumulative)
{
  if (Accumulative)
  {
    FScaling[0] += Sx;
    FScaling[1] += Sy;
    FScaling[2] += Sz;
  }
  else
  {
    FScaling[0] = Sx;
    FScaling[1] = Sy;
    FScaling[2] = Sz;
  };
  change(this, GC_CHANGE_FIGURE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Scales the figure by the amount given in Factor. If Accumulative is true then the new scale factors are multiplied
 * with the existing values. This version of Scale uses an array of values in the parameter list.
 *
 * @param Factor An array containing the three scale values for x, y and z.
 * @param Accumulative If true then the given values are added to any existing values otherwiese they are used as given.
 */
void CFigure::scaleV(const float Factor[3], bool Accumulative)
{
  if (Accumulative)
  {
    FScaling[0] += Factor[0];
    FScaling[1] += Factor[1];
    FScaling[2] += Factor[2];
  }
  else
  {
    FScaling[0] = Factor[0];
    FScaling[1] = Factor[1];
    FScaling[2] = Factor[2];
  };
  change(this, GC_CHANGE_FIGURE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets a new size of the figure, that is, of the unscaled size of its content figure element.
 *
 * @param width The new width in layer space.
 * @param height The new height in layer space.
 * @return True if the figure really was resized (can be denied due to constraints).
 */
bool CFigure::setSize(float width, float height)
{
  if (FContent != NULL)
  {
    float dX = width - FContent->width();
    float dY = height - FContent->height();
    return FContent->resize(dX, dY, GC_FI_RESIZE_SOUTH_EAST);
  }
  else
    return false;
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Moves the figure by the amount given in Factor. If Accumulative is true then the new translation factors are 
 * multiplied with the existing values. This version of Translate uses an array for the values in the parameter list.
 *
 * @param Tx Scale factor for the x axis.
 * @param Ty Scale factor for the y axis.
 * @param Tz Scale factor for the z axis.
 * @param Accumulative If true then the given values are added to any existing values otherwiese they are used as given.
 */
void CFigure::translate(float Tx, float Ty, float Tz, bool Accumulative)
{
  if (Accumulative)
  {
    FTranslation[0] += Tx;
    FTranslation[1] += Ty;
    FTranslation[2] += Tz;
  }
  else
  {
    FTranslation[0] = Tx;
    FTranslation[1] = Ty;
    FTranslation[2] = Tz;
  };
  change(this, GC_CHANGE_FIGURE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Moves the figure by the amount given in Factor. If Accumulative is true then the new translation factors are 
 * multiplied with the existing values. This version of Translate uses an array for the values in the parameter list.
 *
 * @param Factor An array of translation values, for each axis one.
 * @param Accumulative If true then the given values are added to any existing values otherwiese they are used as given.
 */
void CFigure::translateV(const float Factor[3], bool Accumulative)
{
  if (Accumulative)
  {
    FTranslation[0] += Factor[0];
    FTranslation[1] += Factor[1];
    FTranslation[2] += Factor[2];
  }
  else
  {
    FTranslation[0] = Factor[0];
    FTranslation[1] = Factor[1];
    FTranslation[2] = Factor[2];
  };
  change(this, GC_CHANGE_FIGURE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates the display list of this figure (and all child figures) if necessary.
 */
void CFigure::validate(void)
{
  if (dirty())
  {
    CGraphicElement::validate();

    FMinimalScale = FScaling[0] < FScaling[1] ? FScaling[0] : FScaling[1];

    updateMatrix();

    // Let the content prepare itself if necessary.
    if (FContent != NULL)
    {
      FContent->validate();

      // Update bounding box. Take the bounds of the figure this instance standes for and 
      // transform that with the local rotation, translation and scaling factors.
      boundsNew();
      boundsAdd(FContent->bounds(), FMatrix);
      boundsFinished();

      if (FDisplayList == 0)
        FDisplayList = glGenLists(1);

      glNewList(FDisplayList, GL_COMPILE);
      FContent->render();
      glEndList();
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given coordinates from view space to figure space.
 *
 * @param original The coordinates to convert to local space.
 * @param instance The instance relative to which the coordinates are to be converted.
 * @param Converted coordinates.
 */
void CFigure::viewToFigure(TVertex original, CFigureInstance* instance, TVertex& result)
{
  validate();

  if (instance != NULL)
  {
    instance->viewToInstance(original, result);
    result = matrixTransform(FInverse, result);
  }
  else
    result = matrixTransform(FInverse, original);
}

//----------------- CFigureListener ------------------------------------------------------------------------------------

void CFigureInstance::CFigureListener::onAction(CGCBase* sender, CGCBase* origin, TAction** actionType)
{
  instance->action(origin, actionType);
}

//----------------------------------------------------------------------------------------------------------------------

void CFigureInstance::CFigureListener::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  if (instance != NULL)
  {
    switch (reason)
    {
      case GC_CHANGE_FIGURE_PROPERTY:
      case GC_CHANGE_ELEMENT_PROPERTY:
        {
          instance->makeDirty();
          break;
        };      
      case GC_CHANGE_FIGURE_EXCHANGE:
        {
          instance->replaceFigure((CFigure*) origin);
          break;
        };
    };
    instance->change(origin, reason); 
  };
}

//----------------------------------------------------------------------------------------------------------------------

void CFigureInstance::CFigureListener::onDestroy(CGCBase* sender)
{ 
  instance->onDestroy(sender); 
}

//----------------------------------------------------------------------------------------------------------------------

void CFigureInstance::CFigureListener::onError(CGCBase* sender, CGCBase* origin, const char* message)
{
  instance->error(origin, message);
}

//----------------- CFigureInstance ------------------------------------------------------------------------------------

int created = 0;
int destroyed = 0;

CFigureInstance::CFigureInstance(CLayer* owner, CFigure* figure):CGraphicElement(figure->canvas())
{
  _className = "CFigureInstance";
  FLayer = NULL;
  FFigure = figure;
  FShadow = 0;
  FShadowTexture = 0;
  FLastWidth = -1;
  FLastHeight = -1;
  FSelected = false;

  memcpy(FMatrix, identity, sizeof(TMatrix));
  memcpy(FInverse, identity, sizeof(TMatrix));

  FRotation[0] = 0;
  FRotation[1] = 0;
  FRotation[2] = 0;
  FRotation[3] = 1;

  FScaling[0] = 1;
  FScaling[1] = 1;
  FScaling[2] = 1;

  FTranslation[0] = 0;
  FTranslation[1] = 0;
  FTranslation[2] = 0;

  FListener.instance = this;
  if (figure != NULL)
    figure->addListener(&FListener);

  ++created;
}

//----------------------------------------------------------------------------------------------------------------------

CFigureInstance::~CFigureInstance(void)
{
  if (FShadowTexture != 0)
    glDeleteTextures(1, &FShadowTexture);
  if (FShadow != 0)
    glDeleteLists(FShadow, 1);

  if ((FFigure != NULL) && (!FFigure->destroying()))
    FFigure->removeListener(&FListener);

  ++destroyed;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Applies the current translation, rotation and scalie factors.
 */
void CFigureInstance::applyTransformations(void)
{
  glTranslatef(FTranslation[0], FTranslation[1], FTranslation[2]);
  // Index 0 contains the angle, while the other three coordinates form the axis to rotate around.
  glRotatef(FRotation[0], FRotation[1], FRotation[2], FRotation[3]);
  glScalef(FScaling[0], FScaling[1], FScaling[2]);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a drop shadow of the current figure content. Must only be called if there is an attached figure.
 *
 * @note Shadows can only be used if frame buffer objects are supported by the graphics board.
 */
void CFigureInstance::makeShadow(void)
{
  static int shadowOffset = 10;
  static int shadowSize = 6;

  if (FFigure != NULL && canvas()->supportsExtension(GC_OE_FRAME_BUFFER_OBJECTS))
  {
    bool canUseFBOs = true;
    
    // Make sure we do not allocate more buffer space than supported.
    GLint maxBufferSize = 0x7FFFFFFF;
    glGetIntegerv(GL_MAX_RENDERBUFFER_SIZE_EXT, &maxBufferSize);

    // Compute local coordinates used for single figure rendering.
    GLsizei objectWidth = (GLsizei) width() + 2 * shadowSize;
    GLsizei objectHeight = (GLsizei) height() + 2 * shadowSize;

    // Make sure the current view ratio is kept the same for the local render area.
    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);

    float ratio = (float) viewport[2] / viewport[3];
    GLsizei renderWidth; 
    GLsizei renderHeight;

    if (objectWidth > objectHeight)
    {
      renderWidth = objectWidth;
      renderHeight = ROUND(objectWidth / ratio);
    }
    else
    {
      renderWidth = ROUND(objectHeight * ratio);
      renderHeight = objectHeight;
    };

    int textureWidth = roundUpToPowerOf2(objectWidth);
    int textureHeight = roundUpToPowerOf2(objectHeight);

    if (textureWidth <= maxBufferSize && textureHeight <= maxBufferSize)
    {
      // Get current draw buffer for later restoration.
      GLint currentDrawBuffer = 0;
      glGetIntegerv(GL_DRAW_BUFFER, &currentDrawBuffer);

      GLuint frameBuffer = 0;
      GLuint renderBuffer = 0;

      glGenFramebuffersEXT(1, &frameBuffer);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, frameBuffer);

      // Render depth values to a buffer.
      glGenRenderbuffersEXT(1, &renderBuffer);
      glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, renderBuffer);
      glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT32, textureWidth, textureHeight); 
      glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, renderBuffer);

      glDrawBuffer(GL_NONE);
      glReadBuffer(GL_NONE);

      // Check validity of the frame buffer.
      GLenum status = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
      if (status != GL_FRAMEBUFFER_COMPLETE_EXT)
        canUseFBOs = false;
      
      unsigned char* buffer = NULL;
      int bufferSize = textureWidth * textureHeight;
      if (canUseFBOs)
      {
        glViewport(0, 0, renderWidth, renderHeight);

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glOrtho(0, renderWidth, renderHeight, 0, 10, -10);

        GLfloat matrix[16];
        glGetFloatv(GL_PROJECTION_MATRIX, matrix);

        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        canvas()->clearBuffers();

        glPushAttrib(GL_ENABLE_BIT);
        glEnable(GL_DEPTH_TEST);

        // Set a small depth offset for rendering otherwise we don't get any depth values back.
        //glTranslatef((GLfloat) shadowSize, (GLfloat) shadowSize, 5);
        glTranslatef(0, 0, 5);
        //FFigure->render(1);

        glBegin(GL_POLYGON);
          glVertex2i(0, 0);
          glVertex2i(textureWidth, 0);
          glVertex2f(textureWidth / 2.0f, (float) textureHeight);
        glEnd();
        
        glPopAttrib();

        buffer = (unsigned char*) malloc(bufferSize);
        glReadPixels(0, 0, textureWidth, textureHeight, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, buffer);

        // Convert depth map so that unused values become 0 (transparent).
        unsigned char* run = buffer;
        for (int i = 0; i < bufferSize; ++i)
          *run = 255 - *run;
          ++run;
      };
      
      // Release frame buffer binding to enable normal rendering again.
      glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
      glDeleteRenderbuffersEXT(1, &renderBuffer);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
      glDeleteFramebuffersEXT(1, &frameBuffer);

      glDrawBuffer(currentDrawBuffer);

      if (buffer != NULL)
      {
        // Setup the target texture object.
        if (FShadowTexture != 0)
          glDeleteTextures(1, &FShadowTexture);
	      glGenTextures(1, &FShadowTexture);
        glBindTexture(GL_TEXTURE_2D, FShadowTexture);

        // Convert the silhouette to an alpha channel of an otherwise black image. Blur it to make a soft shadow.
        //averageMeanBlur(buffer, textureWidth, textureHeight, shadowSize / 2);
        unsigned char* shadowImage = (unsigned char*) malloc(4 * textureWidth * textureHeight);
        memset(shadowImage, 0, 4 * textureWidth * textureHeight);
        unsigned char* run = buffer;
        //for (int y = textureHeight - 1; y >= 0; --y)
        for (int y = 0; y < textureHeight; ++y)
          for (int x = 0; x < textureWidth; ++x)
            shadowImage[y * 4 * textureWidth + 4 * x + 3] = *run++;

        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, textureWidth, textureHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, shadowImage);
        free(shadowImage);
        free(buffer);

        if (FShadow == 0)
          FShadow = glGenLists(1);
        glNewList(FShadow, GL_COMPILE);

          glEnable(GL_TEXTURE_2D);
          glBindTexture(GL_TEXTURE_2D, FShadowTexture);

          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
          
          glColor4f(0, 0, 0, 0.5);
          glBegin(GL_QUADS);
            /*
            glTexCoord2f(0, 0);
            glVertex2i(0, objectHeight);

            glTexCoord2f((float) objectWidth / textureWidth, 0);
            glVertex2i(objectWidth, objectHeight);

            //glTexCoord2f((float) objectWidth / textureWidth, (float) objectHeight / textureHeight);
            glTexCoord2f((float) objectWidth / textureWidth, 1);
            glVertex2i(objectWidth, 0);

            //glTexCoord2f(0, (float) objectHeight / textureHeight);
            glTexCoord2f(0, 1);
            glVertex2i(0, 0);
            */

            glTexCoord2f(0, 0);
            glVertex2i(0, 0);

            glTexCoord2f(1, 0);
            glVertex2i(textureWidth, 0);

            glTexCoord2f(1, 1);
            glVertex2i(textureWidth, textureHeight);

            glTexCoord2f(0, 1);
            glVertex2i(0, textureHeight);

          glEnd();

          glDisable(GL_TEXTURE_2D);

          // Debug only.
          glColor4f(1, 0, 0, 1);
          glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
          glRecti(0, 0, objectWidth, objectHeight);
          glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
        glEndList();

        canvas()->checkError();
        glBindTexture(GL_TEXTURE_2D, 0);
      };
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called when by user input a figure must be resized. The current view is handling user input and forwards the
 * appropriate call to the figure instance.
 *
 * @param dX Horizontal amount to resize.
 * @param dY vertical amount to resize.
 * @param info Result of the feedback hit test that tells us how to resize.
 */
void CFigureInstance::resize(float dX, float dY, TFeedbackInfo info)
{
  if (FFigure != NULL && FFigure->FContent != NULL)
  {
    bool resizeUp = false;
    float oldHeight = FFigure->height();
    switch (info)
    {
	  case GC_FI_RESIZE_NORTH:
      case GC_FI_RESIZE_NORTH_EAST:
      case GC_FI_RESIZE_NORTH_WEST:
        {
          resizeUp = true;
          dY = -dY;
          break;
        };
    };

    bool resizeLeft = false;
    float oldWidth = FFigure->width();
    switch (info)
    {
      case GC_FI_RESIZE_SOUTH_WEST:
      case GC_FI_RESIZE_WEST:
      case GC_FI_RESIZE_NORTH_WEST:
        {
          resizeLeft = true;
          dX = -dX;
          break;
        };
    };

    if (FFigure->FContent->resize(dX, dY, info))
    {
      // Move the figure instance if resize direction is either up or left.
      // The translation amount must be determined from the old and new sizes
      // as the mouse movement alone cannot tell us about size constraints.
      if (resizeUp)
        FTranslation[1] -= FFigure->height() - oldHeight;

      if (resizeLeft)
        FTranslation[0] -= FFigure->width() - oldWidth;

      makeDirty(GC_CHANGE_FIGURE_RESIZE);
      canvas()->refresh();
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Recomputes the local transformation matrix.
 */
void CFigureInstance::updateMatrix(void)
{
  memcpy(FMatrix, identity, sizeof(TMatrix));
  matrixTranslate(FMatrix, FTranslation[0], FTranslation[1], FTranslation[2]);
  matrixRotate(FMatrix, FRotation[0], FRotation[1], FRotation[2], FRotation[3]);
  matrixScale(FMatrix, FScaling[0], FScaling[1], FScaling[2]);

  matrixInvert(FMatrix, FInverse);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the figure element that contains the given point.
 *
 * @param coords The point to check for in view space.
 * @return NULL or a figure element occupying the given point.
 */
CFigureElement* CFigureInstance::elementFromPoint(TVertex coords)
{
  if (FFigure != NULL)
  {
    TVertex v;
    FFigure->viewToFigure(coords, this, v);
    return FFigure->elementFromPoint(v);
  }
  else
    return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the actions of the figure element that is located at the given location. The given coordinates must
 * be converted to figure space first, though. They are given in view coordinates.
 * On call of this function the current view's transformations are already applied.
 * OpenGL modelview and projection matrix are saved and restored.
 * Only actions with the correct trigger conditions are executed.
 *
 * @param button The mouse button which triggered the call.
 * @param event The event which triggered the action.
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The mouse coordinates in element space.
 * @return The last executed action or NULL if none.
 */
TAction* CFigureInstance::executeAssociatedActions(TMouseButton button, TMouseEvent event, TModifiers modifiers, const TVertex& coords)
{
  TAction* result = NULL;

  if (FFigure != NULL && FFigure->FContent != NULL)
  {
    TVertex v;
    FFigure->viewToFigure(coords, this, v);
    result = FFigure->FContent->executeAssociatedActions(button, event, modifiers, v);
    if (result != NULL && result->type != GC_ACTION_NONE)
      canvas()->refresh();
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Used to determine more specifically what action they could execute at the specified location.
 * This method is mainly used to determine visual feedback for user in a frequent manner (e.g. cursor image), so
 * actual implementations should be fast.
 *
 * @param coords The coordinates in space of the view to which this instance belongs.
 * @return Returns a TFeedbackInfo result specifying what action could take place at the given location.
 */
TFeedbackInfo CFigureInstance::getFeedbackInfo(TModifiers modifiers, const TVertex& coords)
{
  if (FFigure != NULL && FFigure->FContent != NULL)
  {
    TVertex v;
    FFigure->viewToFigure(coords, this, v);
    return FFigure->FContent->getFeedbackInfo(modifiers, v);
  }
  else
    return GC_FI_NONE;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given coordinates from local space to view space.
 *
 * @param coords The coordinates to convert to view space.
 * @param result [out] Converted coordinates.
 */
void CFigureInstance::instanceToView(TVertex original, TVertex& result)
{
  validate();

  result = matrixTransform(FMatrix, original);

  if (FLayer != NULL)
    FLayer->layerToView(result, result);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the untransformed bounds of the instance, that is, the bounds of the used figure. If there is no figure then
 * empty bounds are returned.
 */
TBoundingBox CFigureInstance::localBounds(void)
{
  if (FFigure != NULL)
    return FFigure->bounds();
  else
  {
    TBoundingBox dummy;

    return dummy; 
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Marks the display list for this figure instance as invalid, hence it will be recreated next time validate 
 * is called.
 *
 * @param reason An optional change reason.
 */
void CFigureInstance::makeDirty(TGCChangeReason reason)
{
  if (!dirty())
  {
    CGraphicElement::makeDirty(reason);

    if (FLayer != NULL && !FLayer->updating())
      FLayer->makeDirty();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called by a class (usually CFigure) with which we registered as notification sink and which is
 * about to be destroyed.
 */
void CFigureInstance::onDestroy(CGCBase* figure)
{
  if (figure == FFigure)
    delete this;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by name. The name syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property.
 * @param index If the property is a list then this is the index into that list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CFigureInstance::propertyGet(const char* name, unsigned int index)
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
              // figure instance do not have names.
              if (FFigure == NULL)
                result = "figure instance";
              else
              {
                result = FFigure->propertyGet("name", 0);
                result += " instance";
              };
              break;
            };
          case GC_PROPERTY_DESCRIPTION:
            {
              result = "An instance of a figure on a layer";
              break;
            };
          case GC_PROPERTY_OWNER:
            {
              result = FLayer;
			  break;
            };
          case GC_PROPERTY_SELECTED:
            {
              result = FSelected;
              break;
            };
        };
        break;
      };
    case GC_CONTAINER_SCALING:
      {
        if (index < 3)
        {
          result = FScaling[index];
        };
        break;
	  };
    case GC_CONTAINER_TRANSLATION:
      {
        if (index < 3)
          result = FTranslation[index];
        break;
      };
    case GC_CONTAINER_ROTATION:
      {
        if (index < 4)
          result = FRotation[index];
        break;
      };
    case GC_CONTAINER_FIGURE:
      {
        result = FFigure;

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
void CFigureInstance::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  switch (getContainerID(name))
  {
	case GC_CONTAINER_UNKNOWN:
	  {
		switch (getPropertyID(name))
        {
          case GC_PROPERTY_SELECTED:
            {
              FSelected = value;
              change(this, GC_CHANGE_FINSTANCE_PROPERTY);
              break;
            };
        };

        break;
      };
    case GC_CONTAINER_SCALING:
      {
        if (index < 3)
        {
          FScaling[index] = value;
          change(this, GC_CHANGE_FINSTANCE_PROPERTY);
          makeDirty();
        };
        break;
      };
    case GC_CONTAINER_TRANSLATION:
      {
        if (index < 3)
        {
          FTranslation[index] = value;
          change(this, GC_CHANGE_FINSTANCE_PROPERTY);
		  makeDirty();
        };
        break;
      };
    case GC_CONTAINER_ROTATION:
      {
        if (index < 4)
        {
          FRotation[index] = value;
          change(this, GC_CHANGE_FINSTANCE_PROPERTY);
          makeDirty();
		};
        break;
      };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Executes the figure's instance display list.
 */
void CFigureInstance::render(void)
{
  if (FFigure != NULL)
  {
    glPushMatrix();

    applyTransformations();

    /*
    if (FShadow != 0)
      glCallList(FShadow);
    */
    FFigure->render();

    glPopMatrix();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Replaces the figure this instance points to.
 *
 * @param figure The new figure this instance should use from now on. It must not be NULL.
 */
void CFigureInstance::replaceFigure(CFigure* figure)
{
  FFigure->removeListener(&FListener);
  FFigure = figure;
  FFigure->addListener(&FListener);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Rotates the figure around the given axis by the angle Angle (in radians). This version of Rotate uses a vector for the
 * rotation axis in the parameter list.
 *
 * @param Angle The rotation angle in radians.
 * @param Rx The x part of the axis around which to rotate the figure instance.
 * @param Ry The y part of the axis around which to rotate the figure instance.
 * @param Rz The z part of the axis around which to rotate the figure instance.
 * @note: Currently there is no accumulative version of Rotate available (requires a quaternion lib, which we don't have yet).
 */
void CFigureInstance::rotate(float Angle, float Rx, float Ry, float Rz)
{
  FRotation[0] = Angle;
  FRotation[1] = Rx;
  FRotation[2] = Ry;
  FRotation[3] = Rz;
  change(this, GC_CHANGE_FINSTANCE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/** 
 * Rotates the figure around the given axis by the angle Angle (in radians). This version of Rotate uses a vector for the
 * rotation axis in the parameter list.
 *
 * @param Angle The rotation angle in radians.
 * @param Axis The axis around which to rotate the figure instance.
 * @note: Currently there is no accumulative version of Rotate available (requires a quaternion lib, which we don't have yet).
 */
void CFigureInstance::rotateV(float Angle, const float Axis[3])
{
  FRotation[0] = Angle;
  FRotation[1] = Axis[0];
  FRotation[2] = Axis[1];
  FRotation[3] = Axis[2];
  change(this, GC_CHANGE_FINSTANCE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Scales the figure by the amount given in Factor. If Accumulative is true then the new scale factors are multiplied
 * with the existing values. This version of Scale uses an array of values in the parameter list.
 *
 * @param Sx The scale value for the x-axis
 * @param Sy The scale value for the y-axis
 * @param Sz The scale value for the z-axis
 * @param Accumulative If true then the new scale values are added to any previously assigned values.
 */
void CFigureInstance::scale(float Sx, float Sy, float Sz, bool Accumulative)
{
  if (Accumulative)
  {
    FScaling[0] += Sx;
    FScaling[1] += Sy;
    FScaling[2] += Sz;
  }
  else
  {
    FScaling[0] = Sx;
    FScaling[1] = Sy;
    FScaling[2] = Sz;
  };
  change(this, GC_CHANGE_FINSTANCE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Scales the figure by the amount given in Factor. If Accumulative is true then the new scale factors are multiplied
 * with the existing values. This version of Scale uses an array of values in the parameter list.
 *
 * @param Factor Contains the scaling factors for all three axes. Index 0 contains the value for the x-axis, index 1
 *               that for the y-axis and index 2 for z.
 * @param Accumulative If true then the new scale values are added to any previously assigned values.
 */
void CFigureInstance::scaleV(const float Factor[3], bool Accumulative)
{
  if (Accumulative)
  {
    FScaling[0] += Factor[0];
    FScaling[1] += Factor[1];
    FScaling[2] += Factor[2];
  }
  else
  {
    FScaling[0] = Factor[0];
    FScaling[1] = Factor[1];
    FScaling[2] = Factor[2];
  };
  change(this, GC_CHANGE_FINSTANCE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Tells the caller whether this instance is currently selected.
 * 
 * @return True if this figure instance is currently selected, otherwise false.
 */
bool CFigureInstance::selected(void)
{
  return FSelected;
}

//----------------------------------------------------------------------------------------------------------------------

void CFigureInstance::setSize(float width, float height)
{
  float dX = width - this->width();
  float dY = height - this->height();
  resize(dX, dY, GC_FI_RESIZE_SOUTH_EAST);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Moves the figure by the amount given in Factor. If Accumulative is true then the new translation factors are multiplied
 * with the existing values.
 *
 * @param Tx The scale factor to apply on the x-axis.
 * @param Ty The scale factor to apply on the y-axis.
 * @param Tz The scale factor to apply on the z-axis.
 * @param Accumulative If true scaling factors are added to the values already set previously.
 */
void CFigureInstance::translate(float Tx, float Ty, float Tz, bool Accumulative)
{
  if (Accumulative)
  {
    FTranslation[0] += Tx;
    FTranslation[1] += Ty;
    FTranslation[2] += Tz;
  }
  else
  {
    FTranslation[0] = Tx;
    FTranslation[1] = Ty;
    FTranslation[2] = Tz;
  };
  change(this, GC_CHANGE_FINSTANCE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Moves the figure by the amount given in Factor. If Accumulative is true then the new translation factors are multiplied
 * with the existing values.
 *
 * @param Factor The scale factor to apply. Index 0 contains the factor for the x-axis etc.
 * @param Accumulative If true scaling factors are added to the values already set previously.
 */
void CFigureInstance::translateV(const float Factor[3], bool Accumulative)
{
  if (Accumulative)
  {
    FTranslation[0] += Factor[0];
    FTranslation[1] += Factor[1];
    FTranslation[2] += Factor[2];
  }
  else
  {
    FTranslation[0] = Factor[0];
    FTranslation[1] = Factor[1];
    FTranslation[2] = Factor[2];
  };
  change(this, GC_CHANGE_FINSTANCE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Validates the associated figure if it is dirty.
 */
void CFigureInstance::validate(void)
{
  if (dirty())
  {
    CGraphicElement::validate();

    updateMatrix();
    FMinimalScale = FScaling[0] < FScaling[1] ? FScaling[0] : FScaling[1];

    if (FFigure != NULL)
    {
      FFigure->validate();

      // Update bounding box. Take the bounds of the figure this instance standes for and 
      // transform that with  the local rotation, translation and scaling factors.
      boundsNew();
      boundsAdd(FFigure->bounds(), FMatrix);
      boundsFinished();

      if (fabs(FLastWidth - width()) >= 1 || fabs(FLastHeight - height()) >= 1)
      {
        //makeShadow();
        FLastWidth = width();
        FLastHeight = height();
      };
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given coordinates from view space to figure instance space.
 *
 * @param coords The coordinates to convert to local space.
 * @param result [out] Converted coordinates.
 */
void CFigureInstance::viewToInstance(TVertex original, TVertex& result)
{
  validate();

  if (FLayer != NULL)
  {
    FLayer->viewToLayer(original, result);
    result = matrixTransform(FInverse, result);
  }
  else
    result = matrixTransform(FInverse, original);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Tells the associated figure that the current zoom has changed (needed for text adjustments).
 *
 * @param newZoom The new zoom value.
 */
void CFigureInstance::zoomChanged(float newZoom)
{
  if (FFigure != NULL)
    FFigure->zoomChanged(newZoom);
}

//----------------------------------------------------------------------------------------------------------------------

