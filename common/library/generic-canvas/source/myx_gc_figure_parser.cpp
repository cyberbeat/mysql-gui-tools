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
 * @file myx_gc_figure_parser.cpp
 * @brief Parser for figure elements, which are converted from XML to our internal model.
 * 
 */

#include "myx_gc_figure_parser.h"
#include "myx_gc_gl_helper.h"

//----------------------------------------------------------------------------------------------------------------------

extern const string defaultFontFamily;
extern const string defaultFontStyle;
extern const string defaultFontWeight;
extern const int defaultFontSize;

extern const string DefaultLayout;
extern const string DefaultResize;

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines whether the given node is defined for the platform we are currently executing on.
 *
 * @param node The node which must be checked.
 * @result true if the node is defined for the current platform otherwise false.
 */
bool isForThisPlatform(xmlNodePtr node)
{
  string platform = getStringAttributeDef(node, "platform", "");
 
  // If there is no platform attribute or it is empty then the element is allowed on all platforms.
  if (platform.empty())
    return true;
  else
  {
    StringTokenizer tokenizer(platform, ",", true);
    while (tokenizer.hasMoreTokens())
    {
      wstring token = tokenizer.nextToken();
#ifdef _WINDOWS
      if (token == L"windows")
        return true;
#elif __APPLE__
      if (token == L"macosx")
        return true;
#else
      if (token == L"linux")
        return true;
#endif
    };
    return false;
  };
}

//----------------- CFigureParser --------------------------------------------------------------------------------------

CFigureParser::CFigureParser(CGenericCanvas* canvas): CGCBase(canvas)
{
}

//----------------------------------------------------------------------------------------------------------------------

CFigureParser::~CFigureParser(void)
{
}

//----------------------------------------------------------------------------------------------------------------------

static map<string, TFigureElementResize> resizeLookup;
static map<wstring, TFeedbackInfo> resizeDirectionLookup;
static map<string, TFigureElementLayout> layoutLookup;
static map<string, TAlignment> alignmentLookup;

static map<wstring, TActionType> actionLookup;
static map<string, TMouseButton> buttonLookup;
static map<string, TMouseEvent> buttonEventLookup;
static map<wstring, TModifiers> modifierLookup;

/**
 * Checks if the static lookup tables are set up already. If not then it is done.
 */
void CFigureParser::checkLookupTables(void)
{
  static bool lookupsInitialized = false;

  if (!lookupsInitialized)
  {
    lookupsInitialized = true;

    layoutLookup["row"] = GC_LAYOUT_ROW;
    layoutLookup["column"] = GC_LAYOUT_COLUMN;

    alignmentLookup["left"] = GC_ALIGN_LEFT_TOP;
    alignmentLookup["center"] = GC_ALIGN_CENTER;
    alignmentLookup["centered"] = GC_ALIGN_CENTER;
    alignmentLookup["middle"] = GC_ALIGN_CENTER;
    alignmentLookup["right"] = GC_ALIGN_RIGHT_BOTTOM;
    alignmentLookup["top"] = GC_ALIGN_LEFT_TOP; 
    alignmentLookup["bottom"] = GC_ALIGN_RIGHT_BOTTOM; 

    actionLookup[L"none"] = GC_ACTION_NONE;
    actionLookup[L"toggle"] = GC_ACTION_TOGGLE;
    actionLookup[L"collapse"] = GC_ACTION_COLLAPSE;
    actionLookup[L"expand"] = GC_ACTION_EXPAND;
    actionLookup[L"resizeFigure"] = GC_ACTION_RESIZE;
    actionLookup[L"changeStyle"] = GC_ACTION_CHANGE_STYLE;
    actionLookup[L"dragFigure"] = GC_ACTION_DRAG;
    actionLookup[L"do"] = GC_ACTION_APPLICATION;

    resizeLookup["none"] = GC_RESIZE_NONE;
    resizeLookup["horizontal"] = GC_RESIZE_HORIZONTAL_ONLY;
    resizeLookup["vertical"] = GC_RESIZE_VERTICAL_ONLY;
    resizeLookup["all"] = GC_RESIZE_ALL;

    resizeDirectionLookup[L"north"] = GC_FI_RESIZE_NORTH;
    resizeDirectionLookup[L"north-east"] = GC_FI_RESIZE_NORTH_EAST;
    resizeDirectionLookup[L"east"] = GC_FI_RESIZE_EAST;
    resizeDirectionLookup[L"south-east"] = GC_FI_RESIZE_SOUTH_EAST;
    resizeDirectionLookup[L"south"] = GC_FI_RESIZE_SOUTH;
    resizeDirectionLookup[L"south-west"] = GC_FI_RESIZE_SOUTH_WEST;
    resizeDirectionLookup[L"west"] = GC_FI_RESIZE_WEST;
    resizeDirectionLookup[L"north-west"] = GC_FI_RESIZE_NORTH_WEST;

    buttonLookup["ignore"] = GC_MOUSE_BUTTON_IGNORE;
    buttonLookup["none"] = GC_MOUSE_BUTTON_NONE;
    buttonLookup["left"] = GC_MOUSE_BUTTON_LEFT;
    buttonLookup["middle"] = GC_MOUSE_BUTTON_MIDDLE;
    buttonLookup["right"] = GC_MOUSE_BUTTON_RIGHT;
    buttonLookup["xbutton1"] = GC_MOUSE_BUTTON_X1;
    buttonLookup["xbutton2"] = GC_MOUSE_BUTTON_X2;

    buttonEventLookup["ignore"] = GC_MOUSE_IGNORE;
    buttonEventLookup["down"] = GC_MOUSE_DOWN;
    buttonEventLookup["up"] = GC_MOUSE_UP;
    buttonEventLookup["double-click"] = GC_MOUSE_DBL_CLICK;

    modifierLookup[L"ignore"] = GC_MODIFIER_IGNORE;
    modifierLookup[L"none"] = GC_MODIFIER_NONE;
    modifierLookup[L"shift"] = GC_MODIFIER_SHIFT;
    modifierLookup[L"control"] = GC_MODIFIER_CONTROL;
    modifierLookup[L"alt"] = GC_MODIFIER_ALT;
    modifierLookup[L"command"] = GC_MODIFIER_COMMAND;
    modifierLookup[L"option"] = GC_MODIFIER_OPTION;
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses an "action" entry in the layout definition.
 *
 * @param node The XML node to parse.
 * @param template_ The target template that gets the actions.
 */
void CFigureParser::parseActions(xmlNodePtr node, CBaseTemplate* template_)
{
#define PARSE_ERROR(message) { \
  char buffer[100]; \
  sprintf(buffer, message, node->line); \
  error(this, buffer); }

  // First parse the function specification then the triggers.
  wstring function = utf8ToUtf16(getStringAttributeDef(node, "function", ""));
  if (function.empty())
    PARSE_ERROR("Figure action parser error (line %d): empty action specification.")
  else
  {
    // Split action definition into "action" and "parameters".
    StringTokenizer partTokenizer(function, L"()", true);
    while (partTokenizer.hasMoreTokens())
    {
      TAction action;
      action.feedbackInfo = GC_FI_NONE;
      action.type = actionLookup[partTokenizer.nextToken()];

      if (action.type != GC_ACTION_NONE)
      {
        if ((partTokenizer.lastDelimiter() != '(') && !partTokenizer.hasMoreTokens())
        {
          PARSE_ERROR("Figure action parser error (line %d): parentheses expected.");
          break;
        };

        // Collect parameters in the second part, separated by comma, if there are any.
        if (partTokenizer.hasMoreTokens())
        {
          StringTokenizer parameterTokenizer(partTokenizer.nextToken(), L",", true);
          while (parameterTokenizer.hasMoreTokens())
          {
            wstring parameter = parameterTokenizer.nextToken();

            // Trim the parameter. 
            string::size_type head = 0;
            string::size_type tail = parameter.size();
            while (head < tail && (parameter[head] == ' ' || parameter[head] == '\t'))
              ++head;
            while (tail > head && (parameter[tail] == ' ' || parameter[tail] == '\t'))
              --tail;
            
            // Remove quotes if there are any.
            if (parameter[0] == '"' || parameter[0] == '\'')
              parameter = parameter.substr(1, parameter.length() - 2);

            action.parameters.push_back(parameter.substr(head, tail));

          };
          
          // Special case resize action. Convert the direction parameter to a feedback info tag.
          if (action.type == GC_ACTION_RESIZE)
            action.feedbackInfo = resizeDirectionLookup[action.parameters[0]];
        };

        // Now the triggers if any.
        xmlNodePtr run = node->children;
        while (run != NULL)
        {
          if (XML_IS(run, "trigger") && isForThisPlatform(run))
          {
            TTrigger trigger;
            trigger.button = buttonLookup[getStringAttributeDef(run, "controller", "ignore")];
            trigger.event = buttonEventLookup[getStringAttributeDef(run, "event", "ignore")];
            trigger.modifiers = GC_MODIFIER_IGNORE;
            string modifiers = getStringAttributeDef(run, "modifiers", "ignore");
            StringTokenizer modifiersTokenizer(modifiers, ",", true);
            while (modifiersTokenizer.hasMoreTokens())
            {
              TModifiers modifier = modifierLookup[modifiersTokenizer.nextToken()];
              if (modifier == GC_MODIFIER_IGNORE || modifier ==  GC_MODIFIER_NONE)
              {
                // Ignore all other values. Set only this single one and stop looking for more.
                trigger.modifiers = modifier;
                break;
              }
              else
                trigger.modifiers |= modifier;
            };
            action.triggers.push_back(trigger);
          };
          run = run->next;
        };
        template_->addAction(action);
      };
    };
  };

#undef PARSE_ERROR
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Takes the given XML node and interprets it as a caption definition.
 *
 * @param node The XML node to parse.
 * @return A new caption element.
 */
CCaptionElementTemplate* CFigureParser::parseCaption(xmlNodePtr node)
{
  wstring key = utf8ToUtf16(getStringAttributeDef(node, "key", ""));
  wstring text = utf8ToUtf16(getStringAttributeDef(node, "default", ""));
  float x = getFloatAttributeDef(node, "x", 0);
  float y = getFloatAttributeDef(node, "y", 0);
  string fontFamily = getStringAttributeDef(node, "font-family", defaultFontFamily);
  int fontSize = getIntAttributeDef(node, "font-size", defaultFontSize);
  string fontStyle = getStringAttributeDef(node, "font-style", defaultFontStyle);
  string fontWeightString = getStringAttributeDef(node, "font-weight", defaultFontWeight);
  int weight = convertFontWeight(fontWeightString);
  string attribute = getStringAttributeDef(node, "horizontal-alignment", "left");
  TAlignment horizontalAlignment = alignmentLookup[attribute];
  attribute = getStringAttributeDef(node, "vertical-alignment", "top");
  TAlignment verticalAlignment = alignmentLookup[attribute];
  GLubyte color[4];
  color[3] = 255;
  GLubyte* colorPtr = color;
  if (convertColor(node, "fill", color) != 0)
    colorPtr = NULL;
  string wrap= getStringAttributeDef(node, "wrap", "false");

  TConstraints constraints;
  constraints.maxHeight = getFloatAttributeDef(node, "max-height", -1);
  constraints.maxWidth = getFloatAttributeDef(node, "max-width", -1);
  constraints.minHeight = getFloatAttributeDef(node, "min-height", -1);
  constraints.minWidth = getFloatAttributeDef(node, "min-width", -1);

  CCaptionElementTemplate* result = new CCaptionElementTemplate(key);
  result->initialize(text, x, y, fontFamily, fontSize, weight, fontStyle, horizontalAlignment, verticalAlignment, 
    colorPtr, constraints, wrap == "true");

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses a single element and returns a new figure element instance. Can be called recursively.
 *
 * @param node The XML node to parse.
 * @return The new figure element instance created out of the element description.
 */
CFigureElementTemplate* CFigureParser::parseElement(xmlNodePtr node, CGCModel* model)
{
  wstring id = utf8ToUtf16(getStringAttributeDef(node, "id", ""));
  wstring key = utf8ToUtf16(getStringAttributeDef(node, "key", ""));
  CFigureElementTemplate* result = new CFigureElementTemplate(id, key);

  string attribute = getStringAttributeDef(node, "layout", DefaultLayout);
  TFigureElementLayout layout = layoutLookup[attribute];

  attribute = getStringAttributeDef(node, "resize", DefaultResize);
  TFigureElementResize resizeMode = resizeLookup[attribute];

  wstring styleName;
  if (getStringAttribute(node, "template", attribute))
    styleName = utf8ToUtf16(attribute);

  TConstraints constraints;
  constraints.maxHeight = getFloatAttributeDef(node, "max-height", -1);
  constraints.maxWidth = getFloatAttributeDef(node, "max-width", -1);
  constraints.minHeight = getFloatAttributeDef(node, "min-height", -1);
  constraints.minWidth = getFloatAttributeDef(node, "min-width", -1);
  TOccurence occurence = GC_OCC_ONCE;
  if (getStringAttribute(node, "occurrence", attribute))
  {
    if (attribute == "+")
      occurence = GC_OCC_ONE_OR_MORE;
    if (attribute == "*")
      occurence = GC_OCC_ZERO_OR_MORE;
  };
  result->initialize(layout, resizeMode, model->style(styleName), constraints, occurence);

  // Now go through the child nodes. There can either be child elements or a caption node.
  xmlNodePtr run = node->children;
  while (run != NULL)
  {
    if (isForThisPlatform(run))
    {
      if (XML_IS(run, "caption"))
      {
        CCaptionElementTemplate* childElement = parseCaption(run);
        result->setCaption(childElement);
      }
      else
        if (XML_IS(run, "element"))
        {
          CFigureElementTemplate* childElement = parseElement(run, model);
          result->addChild(childElement);
        }
        else
          if (XML_IS(run, "action"))
            parseActions(run, result);
    };
    run = run->next;
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses a single conneciton decor definition and creates a decor instance from it.
 *
 * @param definition The definition to parse.
 * @param The model class that gets the new template.
 */
void CFigureParser::parseDecorDefinition(xmlNodePtr definition, CGCModel* model)
{
  checkLookupTables();

  wstring type = utf8ToUtf16(getStringAttributeDef(definition, "type", ""));
  wstring layoutClass = utf8ToUtf16(getStringAttributeDef(definition, "layout-class", ""));
  if (type.size() > 0)
  {
    CConnectionDecor* decor = model->createDecor(type, layoutClass);
    
    xmlNodePtr run = definition->children;
    while (run != NULL)
    {
      if (XML_IS(run, "action") && isForThisPlatform(run))
        parseActions(run, decor);
      run = run->next;
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Parses a single layout definition and creates a figure template from it.
 *
 * @param definition The definition to parse.
 * @param The model class that gets the new template.
 */
void CFigureParser::parseLayoutDefinition(xmlNodePtr definition, CGCModel* model)
{
  checkLookupTables();

  wstring type = utf8ToUtf16(getStringAttributeDef(definition, "type", ""));
  wstring layoutClass = utf8ToUtf16(getStringAttributeDef(definition, "layout-class", ""));
  if (type.size() > 0)
  {
    CFigureTemplate* figureTemplate = model->createLayout(type, layoutClass);
    
    // Find first <element> node. This is our root.
    xmlNodePtr run = definition->children;
    while (run != NULL)
    {
      if (XML_IS(run, "element") && isForThisPlatform(run))
        break;
      run = run->next;
    };

    if (run != NULL)
      figureTemplate->FContent = parseElement(run, model);
  };
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
TGCVariant CFigureParser::propertyGet(const char* name, unsigned int index)
{
  TGCVariant variant;

  return variant;
}

//----------------------------------------------------------------------------------------------------------------------


