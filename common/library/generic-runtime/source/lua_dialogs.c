/* Copyright (c) 2005 MySQL AB
  
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
  
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
  
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
 */

#include <newt.h>
#include <lua.h>
#include <lauxlib.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>


// hack for slang using private libc symbol (was causing the
// resulting RPM to require for ld-linux.so(GLIBC_PRIVATE) 
int __libc_enable_secure=0;

typedef struct {
  char **items;
  int count;
} STRINGLIST;


typedef struct {
  lua_State *lua;
  void *cb;
  void *data;
} callbackStruct;

#define I2P(x) ((void *)(long)(x))

static callbackStruct suspend;
static callbackStruct helpCallback;

static int buttonWidget(lua_State *l);
static int compactbuttonWidget(lua_State *l);
static int centeredWindow(lua_State *l);
static int checkboxWidget(lua_State *l);
static int choiceWindow(lua_State *l);
static int entryWidget(lua_State *l);
static int drawRootText(lua_State *l);
static int doResume(lua_State *l);
static int doSuspend(lua_State *l);
static int doSuspend(lua_State *l);
static int formCreate(lua_State *l);
static int finishScreen(lua_State *l);
static int initScreen(lua_State *l);
static int labelWidget(lua_State *l);
static int listboxWidget(lua_State *l);
static int messageWindow(lua_State *l);
static int openWindow(lua_State *l);
static int popHelpLine(lua_State *l);
static int popWindow(lua_State *l);
static int pushHelpLine(lua_State *l);
static int radioButtonWidget(lua_State *l);
static int refreshScreen(lua_State *l);
static int scaleWidget(lua_State *l);
static int scaleSet(lua_State *l);
static int screenSize(lua_State *l);
static int setSuspendCallback(lua_State *l);
static int setHelpCallback(lua_State *l);
static int reflowText(lua_State *l);
static int textWidget(lua_State *l);
static int ternaryWindow(lua_State *l);
static int checkboxTreeWidget(lua_State *l);


static luaL_reg dialogFunctions[]= {
  { "Button",  buttonWidget },
  { "CompactButton",  compactbuttonWidget },
  { "Checkbox",  checkboxWidget },
  { "Choice", choiceWindow },
  { "centeredWindow", centeredWindow },
  { "drawRootText", drawRootText },
  { "Entry",  entryWidget },
  { "finish", finishScreen },
  { "Form",  formCreate },
  { "helpcallback", setHelpCallback },
  { "init", initScreen },
  { "Label",  labelWidget },
  { "Listbox",  listboxWidget },
  { "message", messageWindow },
  { "openWindow", openWindow },
  { "popHelpLine", popHelpLine },
  { "popWindow", popWindow },
  { "pushHelpLine", pushHelpLine },
  { "RadioButton",  radioButtonWidget },
  { "reflow",  reflowText },
  { "refresh", refreshScreen },
  { "resume", doResume },
  { "Scale", scaleWidget },
  { "size", screenSize },
  { "suspend", doSuspend },
  { "suspendCallback", setSuspendCallback },
  { "ternary", ternaryWindow },
  { "Textbox",  textWidget },
  { "CheckboxTree",  checkboxTreeWidget },
  { NULL, NULL }
};



typedef struct {
  int type;
  newtComponent form;
} snackForm;

static int formAdd(lua_State *l);
static int formDraw(lua_State *l);
static int formRun(lua_State *l);
static int formHotKey(lua_State *l);
static int formSetCurrent(lua_State *l);
static int formSetTimer(lua_State *l);
static int formWatchFD(lua_State *l);

static luaL_reg formFunctions[] = {
    { "add",  formAdd },
    { "draw",  formDraw },
    { "run",  formRun },
    { "addHotkey",  formHotKey },
    { "setCurrent",  formSetCurrent },
    { "setTimer",  formSetTimer },
    { "watchFd",  formWatchFD },
    { NULL }
};



typedef struct {
  int type;
  newtComponent comp;
  int subtype;
  char achar;
  void * apointer;
  int anint;
  callbackStruct scs;
} snackWidget;

static int widgetIndex(lua_State *l);
static int widgetDestroy(lua_State *l);
static int widgetAddCallback(lua_State *l);
static int widgetEntrySetValue(lua_State *l);
static int widgetLabelText(lua_State *l);
static int widgetListboxSetW(lua_State *l);
static int widgetListboxAdd(lua_State *l);
static int widgetListboxIns(lua_State *l);
static int widgetListboxDel(lua_State *l);
static int widgetListboxGet(lua_State *l);
static int widgetListboxSet(lua_State *l);
static int widgetListboxClear(lua_State *l);
static int widgetTextboxText(lua_State *l);
static int widgetCheckboxTreeAddItem(lua_State *l);
static int widgetCheckboxTreeGetSel(lua_State *l);
static int widgetCheckboxTreeGetCur(lua_State *l);
static int widgetCheckboxTreeSetEntry(lua_State *l);
static int widgetCheckboxTreeSetWidth(lua_State *l);
static int widgetCheckboxTreeSetCurrent(lua_State *l);
static int widgetCheckboxTreeSetEntryValue(lua_State *l);
static int widgetCheckboxTreeGetEntryValue(lua_State *l);
static int widgetEntrySetFlags(lua_State *l);
static int widgetCheckboxSetFlags(lua_State *l);
static int widgetCheckboxSetValue(lua_State *l);

static int widgetMatchId(lua_State *l);

static luaL_reg widgetFunctions[] = {
    { "matchId", widgetMatchId },
    { "setCallback",  widgetAddCallback },
    { "labelText",  widgetLabelText },
    { "textboxText",  widgetTextboxText },
    { "entrySetValue",  widgetEntrySetValue },

    { "scaleSet",  scaleSet },

    { "entrySetFlags",  widgetEntrySetFlags },
    { "checkboxSetFlags",  widgetCheckboxSetFlags },
    { "checkboxSetValue",  widgetCheckboxSetValue },
    { NULL }
};

static luaL_reg listboxFunctions[]= {
  { "addItem",  widgetListboxAdd },
  { "insertItem",  widgetListboxIns },
  { "getCurrent",  widgetListboxGet },
  { "setCurrent",  widgetListboxSet },
  { "setWidth",  widgetListboxSetW },
  { "deleteItem",  widgetListboxDel },
  { "clear",  widgetListboxClear },

  { NULL }
};

static luaL_reg checktreeFunctions[]= {
  { "addItem",  widgetCheckboxTreeAddItem }, 
  { "getCurrent",  widgetCheckboxTreeGetCur },
  { "getEntryValue",  widgetCheckboxTreeGetEntryValue },
  { "setEntry",  widgetCheckboxTreeSetEntry },
  { "setWidth",  widgetCheckboxTreeSetWidth },
  { "setCurrent",  widgetCheckboxTreeSetCurrent },      
  { "setEntryValue",  widgetCheckboxTreeSetEntryValue },
  { "getSelection",  widgetCheckboxTreeGetSel },
  { NULL }
};


static void popArgs(lua_State *l, char self_type, const char *format, ...)
{
  va_list args;
  int i;
  int total_argn= 0, opt_argn= 0;
  int top= lua_gettop(l);
  int argc= top;
  int has_self= (self_type != 0);

  va_start(args, format);
  
  if (has_self)
    argc--;

  for (i= 0; format[i]!=0; i++)
  {
    total_argn++;
    if (format[i] == '|' || opt_argn)
      opt_argn++;
  }
  
  if (has_self)
    total_argn++;

  if ((lua_gettop(l) < (total_argn - opt_argn)) || lua_gettop(l) > total_argn)
  {
    char msg[200];
    
    if (has_self)
      total_argn--;

    if (opt_argn == 0)
      sprintf(msg, "Invalid number of arguments to function, expected %i, got %i\n",
              total_argn, lua_gettop(l)-has_self);
    else
      sprintf(msg, "Invalid number of arguments to function, expected %i to %i, got %i\n",
              total_argn-opt_argn, total_argn, lua_gettop(l)-has_self);
    
    luaL_error(l, msg);
  }
  
  if (has_self)
  {
    // move up value of self to the top of stack
    lua_pushvalue(l, 1);
    lua_remove(l, 1);
    
    switch (self_type)
    {
    case 'w':
      {
        snackWidget **value= va_arg(args, snackWidget**);
        if ((*value= luaL_checkudata(l, -1, "SNACK_WIDGET")))
          ;
        else
          luaL_typerror(l, -1, "widget object");
        break;
      }
      break;
    case 'f':
      {
        snackForm **value= va_arg(args, snackForm**);
        if ((*value= luaL_checkudata(l, -1, "SNACK_FORM")))
          ;
        else
          luaL_typerror(l, -1, "form object");
        break;
      }
      break;
    default:
      assert(0);
    }
  }
  

  for (i= 0; format[i]!=0 && i < argc; i++)
  {
    switch (format[i])
    {
    case 'i':
      {
        int *value= va_arg(args, int*);
        if (lua_isnumber(l, i+1))
          *value= luaL_checkint(l, i+1);
        else
          luaL_typerror(l, i+1, "int");
        break;
      }
    case 's':
    case 'S':
      {
        const char **value= va_arg(args, const char**);
        if (lua_isnil(l, i+1))
        {
          *value= NULL;
          if (format[i] == 's')
            luaL_typerror(l, i+1, "string");
        }
        else if (lua_isstring(l, i+1))
          *value= luaL_checkstring(l, i+1);
        else
          luaL_typerror(l, i+1, "string");
        break;
      }
    case 'p':
      {
        int **list= va_arg(args, int**);

        if (lua_istable(l, i+1))
        {
          int tbl= i+1;
          int j;
          int count;
          
          count= luaL_getn(l, tbl);
          (*list)= malloc(sizeof(int)*(count+1));
          
          for (j= 1; j <= count; j++)
          {
            lua_rawgeti(l, tbl, j);
            (*list)[j-1]= lua_tonumber(l, -1);
            lua_pop(l, 1);
          }
          (*list)[j-1]= NEWT_ARG_LAST;
        }
        else
          luaL_typerror(l, i+1, "int array");
        break;
      }
    case '|': // optional part
      
      break;

    case 'o': // ?
    case 'O':
      {
        void **value= va_arg(args, void**);
        if (lua_isnil(l, i+1))
        {
          *value= NULL;
          if (format[i] == 'o')
            luaL_typerror(l, i+1, "object");
        }
        else if ((*value= luaL_checkudata(l, i+1, "SNACK_WIDGET")) ||
            (*value= luaL_checkudata(l, i+1, "SNACK_GRID")) ||
            (*value= luaL_checkudata(l, i+1, "SNACK_FORM")))
          ;
        else
          luaL_typerror(l, i+1, "object");
      }
      break;
    case 'w':
    case 'W': // widget
      {
        snackWidget **value= va_arg(args, snackWidget**);
        if (lua_isnil(l, i+1))
        {
          *value= NULL;
          if (format[i] == 'w')
            luaL_typerror(l, i+1, "widget object");
        }
        else if ((*value= luaL_checkudata(l, i+1, "SNACK_WIDGET")))
          ;
        else
          luaL_typerror(l, i+1, "widget object");
      }
      break;
    case 'f':
    case 'F': // form
      {
        snackForm **value= va_arg(args, snackForm**);
        if (lua_isnil(l, i+1))
        {
          *value= NULL;
          if (format[i] == 'f')
            luaL_typerror(l, i+1, "form object");
        }
        else if ((*value= luaL_checkudata(l, i+1, "SNACK_FORM")))
          ;
        else
          luaL_typerror(l, i+1, "form object");
      }
      break;
    }
  }

  va_end(args);

  lua_pop(l, top);
}




static snackWidget *snackWidgetNew(lua_State *l, int subtype)
{
  snackWidget *widget= lua_newuserdata(l, sizeof(snackWidget));
  int meta;
  
  memset(widget, 0, sizeof(snackWidget));

  widget->type= 'w';
  widget->subtype= subtype;
  
  luaL_newmetatable(l, "SNACK_WIDGET");
  meta= lua_gettop(l);
  
  lua_pushstring(l, "__index");
  lua_pushcfunction(l, widgetIndex);
  lua_rawset(l, meta);

  lua_pushstring(l, "__gc");
  lua_pushcfunction(l, widgetDestroy);
  lua_rawset(l, meta);

  lua_setmetatable(l, -2);

  return widget;
}

static int initScreen(lua_State *l) 
{
  suspend.cb = NULL;
  suspend.data = NULL;

  newtInit();
  newtCls();

  return 0;
}

static int finishScreen(lua_State *l) 
{
  newtFinished();
  return 0;
}

static int refreshScreen(lua_State *l) 
{
  newtRefresh();
  return 0;
}

static int scaleWidget(lua_State *l) 
{
  // scaleWidget(x, y, width, fullAmount) -> widget

  snackWidget *widget;
  int width, fullAmount;
  int x, y;

  popArgs(l, 0, "iiii", &x, &y, &width, &fullAmount);
  
  widget= snackWidgetNew(l, 'S');
  widget->comp= newtScale(x, y, width, fullAmount);

  return 1;
}

static int scaleSet(lua_State *l)
{
  // <scale>.scaleSet(value)
  
  int amount;
  snackWidget *s;
  
  popArgs(l, 'w', "i", &s, &amount);

  newtScaleSet(s->comp, amount);

  return 0;
}


static int screenSize(lua_State *l) 
{
  // size() -> (width, height)

  int width, height;

  popArgs(l, 0, "");
  
  newtGetScreenSize(&width, &height);

  lua_newtable(l);
  lua_pushnumber(l, height);
  lua_rawseti(l, -2, 1); 
  lua_pushnumber(l, width);
  lua_rawseti(l, -2, 2);
  
  return 1;
}

/*
static void helpCallbackMarshall(newtComponent co, void * data) 
{
  lua_pushcfunction(helpcallback.lua, helpcallback.cb);
  lua_push
  
    Pyobject * args, * result;

    args = Py_BuildValue("(O)", data);
    result = PyEval_CallObject(helpCallback.cb, args);
    Py_DECREF (args);
    Py_XDECREF(result);

    return;
}*/
/*
static void suspendCallbackMarshall(void * data) {
    struct callbackStruct * scs = data;
    PyObject * args, * result;

    if (scs->data) {
	args = Py_BuildValue("(O)", scs->data);
	result = PyEval_CallObject(scs->cb, args);
	Py_DECREF (args);
    } else
	result = PyEval_CallObject(scs->cb, NULL);
    
    if (!result) {
	PyErr_Print();
	PyErr_Clear();
    }

    Py_XDECREF(result);

    return;
}

static void callbackMarshall(newtComponent co, void * data) {
    struct callbackStruct * scs = data;
    PyObject * args, * result;

    if (scs->data) {
	args = Py_BuildValue("(O)", scs->data);
	result = PyEval_CallObject(scs->cb, args);
	Py_DECREF (args);
    } else
	result = PyEval_CallObject(scs->cb, NULL);

    if (!result) {
	PyErr_Print();
	PyErr_Clear();
    }
    
    Py_XDECREF(result);

    return;
}
*/
static int setSuspendCallback(lua_State *l) 
{
  /*
    if (!PyArg_ParseTuple(args, "O|O", &suspend.cb, &suspend.data))
	return NULL;

    Py_INCREF (suspend.cb);
    Py_XINCREF (suspend.data);    
    
    newtSetSuspendCallback(suspendCallbackMarshall, &suspend);

    Py_INCREF(Py_None);
    return Py_None;
   * */
  return 0;
}

static int setHelpCallback(lua_State *l) 
{
/*  
    if (!PyArg_ParseTuple(args, "O", &helpCallback.cb))
	return NULL;


    Py_INCREF (helpCallback.cb);

    newtSetHelpCallback(helpCallbackMarshall);
*/
  return 0;
}

static int drawRootText(lua_State *l) 
{
  // drawRootText(left, top, text)
  
  int left, top;
  const char *text;

  popArgs(l, 0, "iis", &left, &top, &text);

  newtDrawRootText(left, top, text);

  return 0;
}

static int doSuspend(lua_State *l) 
{
  popArgs(l, 0, "");
  
  newtSuspend();

  return 0;
}

static int doResume(lua_State *l) 
{
  popArgs(l, 0, "");
  
  newtResume();
  
  return 0;
}

static int popHelpLine(lua_State *l) 
{
  popArgs(l, 0, "");
  
  newtPopHelpLine();

  return 0;
}

static int pushHelpLine(lua_State *l) 
{
  const char *line= NULL;
  
  popArgs(l, 0, "S", &line);
  
  if (!line)
  {
    lua_pop(l, 1);
    newtPushHelpLine(NULL);
  }
  else
  {
    newtPushHelpLine(line);
  }
  return 0;
}

static int reflowText(lua_State *l)
{
  // reflowText(text, width, minus=5, plus=5) -> (text, realWidth, realHeight)
  
  const char *text;
  char *newText;
  int width, minus = 5, plus = 5;
  int realWidth, realHeight;

  popArgs(l, 0, "si|ii", &text, &width, &minus, &plus);

  newText= newtReflowText((char*)text, width, minus, plus, &realWidth, &realHeight);

  lua_newtable(l);
  lua_pushnumber(l, realHeight);
  lua_rawset(l, -2);
  lua_pushnumber(l, realWidth);
  lua_rawset(l, -2);
  lua_pushstring(l, newText);
  lua_rawset(l, -2);
  free(newText);

  return 1;
}

static int centeredWindow(lua_State *l) 
{
  // centeredWindow(width, height, title)
  
  int width, height;
  const char *title;
  
  popArgs(l, 0, "iis", &width, &height, &title);

  newtCenteredWindow(width, height, title);

  return 0;
}


static int openWindow(lua_State *l) 
{
  // openWindow(left, top, width, height, title)
  int left, top, width, height;
  const char *title;

  popArgs(l, 0, "iiiis", &left, &top, &width, &height, &title);

  newtOpenWindow(left, top, width, height, title);

  return 0;
}

static int popWindow(lua_State *l)
{
  popArgs(l, 0, "");
  newtPopWindow();

  return 0;
}

static int messageWindow(lua_State *l) 
{
  // messageWindow(title, text, okButton="Ok")
  
  const char * title, * text;
  const char * okbutton = "Ok";

  popArgs(l, 0, "ss|s", &title, &text, &okbutton);

  newtWinMessage((char*)title, (char*)okbutton, (char*)text);

  return 0;
}


static int choiceWindow(lua_State *l)
{
  // choiceWindow(title, text, okButton="Ok", cancelbutton="Cancel")
  
  const char * title, * text;
  const char * okbutton = "Ok";
  const char * cancelbutton = "Cancel";
  int rc;

  popArgs(l, 0, "ss|ss", &title, &text, &okbutton, &cancelbutton);

  rc = newtWinChoice((char*)title, (char*)okbutton, (char*)cancelbutton, (char*)text);

  lua_pushnumber(l, rc);
  
  return 1;
}

static int ternaryWindow(lua_State *l) 
{
  // ternaryWindow(title, text, button1, button2, button3)
  
  const char * title, * text, * button1, * button2, * button3;
  int rc;

  popArgs(l, 0, "sssss", &title, &text, &button1, &button2, &button3);

  rc = newtWinTernary((char*)title, (char*)button1, (char*)button2, (char*)button3, (char*)text);
  
  lua_pushnumber(l, rc);
  
  return 1;
}

static int buttonWidget(lua_State *l) 
{
  // buttonWidget(label) -> widget
  
  snackWidget * widget;
  const char * label;
  int x= -1, y= -1;

  popArgs(l, 0, "iis", &x, &y, &label);

  widget= snackWidgetNew(l, 'B');
  widget->comp = newtButton(x, y, label);

  return 1;
}

static int compactbuttonWidget(lua_State *l) 
{
  // compactbuttonWidget(label) -> widget
  
  snackWidget * widget;
  const char * label;
  int x= -1, y= -1;

  popArgs(l, 0, "iis", &x, &y, &label);

  widget= snackWidgetNew(l, 'b');
  widget->comp = newtCompactButton(x, y, label);
  
  return 1;
}

static int labelWidget(lua_State *l) 
{
  // labelWidget(label) -> widget
  
  const char * label;
  snackWidget * widget;
  int x= -1, y= -1;

  popArgs(l, 0, "iis", &x, &y, &label);

  widget = snackWidgetNew(l, 'L');
  widget->comp = newtLabel(x, y, label);

  return 1;
}

static int widgetLabelText(lua_State *l)
{
  // <label>.labelText(text)
  
  snackWidget *s;
  const char * label;

  popArgs(l, 'w', "s", &s, &label);

  newtLabelSetText(s->comp, label);

  return 0;
}

static int widgetTextboxText(lua_State *l)
{
  // <textbox>.textboxText(text)
  
  snackWidget *s;
  const char * text;
  
  popArgs(l, 'w', "s", &s, &text);

  newtTextboxSetText(s->comp, text);

  return 0;
}

static int listboxWidget(lua_State *l) 
{
  // listboxWidget(height, doscroll=0, returnExit=0, showCursor=0) -> widget
  
  snackWidget * widget;
  int height;
  int doScroll = 0, returnExit = 0, showCursor = 0 ;
  int x= -1, y= -1;
  
  popArgs(l, 0, "iii|iii", &x, &y, &height, &doScroll, &returnExit, &showCursor);
  
  widget= snackWidgetNew(l, 'X');
  widget->comp = newtListbox(x, y, height,
                             (doScroll ? NEWT_FLAG_SCROLL : 0) |
                             (returnExit ? NEWT_FLAG_RETURNEXIT : 0) |
                             (showCursor ? NEWT_FLAG_SHOWCURSOR : 0)
                             );
  widget->anint = 1;
  
  return 1;
}

static int textWidget(lua_State *l) 
{
  // textWidget(widget, height, text, scrollBar= 0, wrap=0)
  
  const char * text;
  int width, height;
  int scrollBar = 0;
  int wrap = 0;
  snackWidget * widget;
  int x= -1, y= -1;
  
  popArgs(l, 0, "iiiis|ii", &x, &y, &width, &height, &text, &scrollBar, &wrap);

  widget = snackWidgetNew(l, 'T');
  widget->comp = newtTextbox(x, y, width, height,
                             (scrollBar ? NEWT_FLAG_SCROLL : 0) |
                             (wrap ? NEWT_FLAG_WRAP : 0));
  newtTextboxSetText(widget->comp, (char*)text);

  return 1;
}

static int radioButtonWidget(lua_State *l) 
{
  // radioButtonWidget(text, groupButton, state) -> widget
  
  snackWidget * widget, * group;
  const char * text;
  int x= -1, y= -1;
  int isOn;

  popArgs(l, 0, "iisWi", &x, &y, &text, &group, &isOn);
  
  widget = snackWidgetNew(l, 'R');

  if (group)
    widget->comp = newtRadiobutton(x, y, (char*)text, isOn, NULL);
  else
    widget->comp = newtRadiobutton(x, y, (char*)text, isOn, group->comp);

  return 1;
}

static int checkboxWidget(lua_State *l) 
{
  // checkboxWidget(text, state) -> widget

  snackWidget * widget;
  const char * text;
  int isOn;
  int x= -1, y= -1;

  popArgs(l, 0, "iisi", &x, &y, &text, &widget);

  widget = snackWidgetNew(l, 'C');
  widget->comp = newtCheckbox(x, y, (char*)text, isOn ? '*' : ' ', NULL, 
                              &widget->achar);

  return 1;
}

static int widgetCheckboxSetFlags(lua_State *l)
{
  // <cbox>.checkboxSetFlags(flag, sense)
  
  snackWidget *s;
  int flag, sense;
  
  popArgs(l, 'w', "ii", &s, &flag, &sense);
  
  newtCheckboxSetFlags(s->comp, flag, sense);

  return 0;
}

static int widgetCheckboxSetValue(lua_State *l)
{
  // <cbox>.checkboxSetValue(value)
  
  snackWidget *s;
  int value;
  
  popArgs(l, 'w', "i", &s, &value);
  
  newtCheckboxSetValue(s->comp, value ? '*' : ' ');

  return 0;
}

static int entryWidget(lua_State *l)
{
  // entryWidget(x,y, width, default, hidden, password, scrolled, returnExit) -> widget
  
  snackWidget * widget;
  int width;
  const char * initial;
  int isHidden=0, isScrolled=0, returnExit=0, isPassword=0;
  int x= -1, y= -1;

  popArgs(l, 0, "iiis|iiii", &x, &y, &width, &initial, &isHidden, &isScrolled, &isPassword, &returnExit);
  
  widget = snackWidgetNew(l, 'E');
  widget->comp = newtEntry(x, y, initial, width,
                           (const char **) &widget->apointer, 
                           (isHidden ? NEWT_FLAG_HIDDEN : 0) |
                           (isPassword ? NEWT_FLAG_PASSWORD : 0) |
                           (returnExit ? NEWT_FLAG_RETURNEXIT : 0) |
                           (isScrolled ? NEWT_FLAG_SCROLL : 0));
  
  return 1;
}


static int formIndex(lua_State *l)
{
  const char *member;
  snackForm *form;
  int i;
  
  popArgs(l, 'f', "s", &form, &member);
  
  for (i= 0; formFunctions[i].name; i++)
    if (strcmp(formFunctions[i].name, member)==0)
    {
      lua_pushcfunction(l, formFunctions[i].func);
      return 1;
    }
  luaL_error(l, "invalid method for form: %s", member);
  return 0;
}


static int formCreate(lua_State *l)
{
  // form(help="") -> form
  
  snackForm * form;
  const char *help= NULL;
  int meta;
  
  popArgs(l, 0, "|s", &help);
  
  form= lua_newuserdata(l, sizeof(snackForm));

  memset(form, 0, sizeof(snackForm));
  form->type= 'f';
  
  luaL_newmetatable(l, "SNACK_FORM");
  meta= lua_gettop(l);

  lua_pushstring(l, "__index");
  lua_pushcfunction(l, formIndex);
  lua_rawset(l, meta);
  
  lua_setmetatable(l, -2);

  form->form = newtForm(NULL, (char*)help, 0);
  
  return 1;
}


static int formDraw(lua_State *l)
{
  // <form>.draw()

  snackForm *s;
  
  popArgs(l, 'f', "", &s);
  
  newtDrawForm(s->form);
  
  return 0;
}

static int formAdd(lua_State *l)
{
  // <form>.add(widget, ...)

  snackForm *s;
  int size, i;

  lua_pushvalue(l, 1);
  s= luaL_checkudata(l, -1, "SNACK_FORM");
  lua_pop(l, 1);
  if (!s)
    luaL_error(l, "Invalid object self in form.add method");

  size= lua_gettop(l);
  if (size <= 1)
    luaL_error(l, "Insufficient arguments");
  
  for (i = 1; i < size; i++)
  {
    snackWidget *w= (snackWidget *)luaL_checkudata(l, i+1, "SNACK_WIDGET");
    snackForm *f= (snackForm *)luaL_checkudata(l, i+1, "SNACK_FORM");
    
    if (w)
    {
      newtFormAddComponent(s->form, w->comp);
      continue;
    }
    if (f)
    {
      newtFormAddComponent(s->form, f->form);
      continue;
    }

    luaL_typerror(l, i+1, "list of widgets");
  }
  lua_pop(l, size);

  lua_pop(l, 1); // pop self
  
  return 0;
}

static int formRun(lua_State *l)
{
  // <form>.run() -> (type, value)
  snackForm *s;
  struct newtExitStruct result;
  long value;
  const char *text;

  popArgs(l, 'f', "", &s);

  newtFormRun(s->form, &result);

  if (result.reason == NEWT_EXIT_HOTKEY)
  {
    value= result.u.key;
    text= "hotkey";
  }
  else if (result.reason == NEWT_EXIT_TIMER)
  {
    value= 0;
    text= "timer";
  }
  else if (result.reason == NEWT_EXIT_FDREADY)
  {
    value= result.u.watch;
    text= "fdready";
  }
  else
  {
    char id[40];
    
    text= "widget";
    
    sprintf(id, "%p", result.u.co);
    
    lua_newtable(l);
    lua_pushstring(l, text);
    lua_rawseti(l, -2, 1);
    lua_pushstring(l, id);
    lua_rawseti(l, -2, 2);
    
    return 1;
  }

  lua_newtable(l);
  lua_pushstring(l, text);
  lua_rawseti(l, -2, 1);
  lua_pushnumber(l, value);
  lua_rawseti(l, -2, 2);

  return 1;
}

static int formHotKey(lua_State *l)
{
  // <form>.hotKey(key)
  snackForm *s;
  int key;

  popArgs(l, 'f', "i", &s, &key);
  
  newtFormAddHotKey(s->form, key);
  
  return 0;
}

static int formSetTimer(lua_State *l)
{
  // <form>.setTimer(millisecs)
  snackForm *s;
  int millisecs;
  
  popArgs(l, 'f', "i", &s, &millisecs);
  
  newtFormSetTimer(s->form, millisecs);
  
  return 0;
}

static int formWatchFD(lua_State *l)
{
  // <form>.watchFd(fd, flags)
  snackForm *form;
  int fd, fdflags;
  
  popArgs(l, 'f', "ii", &form, &fd, &fdflags);
  
  newtFormWatchFd(form->form, fd, fdflags);

  return 0;
}

static int formSetCurrent(lua_State *l)
{
  // <form>.setCurrent(widget)
  
  snackForm *form;
  snackWidget *w;

  popArgs(l, 'f', "w", &form, &w);

  newtFormSetCurrent(form->form, w->comp);

  return 0;
}

static int widgetIndex(lua_State *l)
{
  // <widget>.__index()
  
  const char *name;
  snackWidget *w;
  
  popArgs(l, 'w', "s", &w, &name);
  
  if (!strcmp(name, "key"))
    lua_pushnumber(l, (long)w->comp);
  else if (!strcmp(name, "entryValue"))
    lua_pushstring(l, w->apointer);
  else if (!strcmp(name, "checkboxValue"))
    lua_pushnumber(l, w->achar == ' ' ? 0 : 1);
  else if (!strcmp(name, "radioValue"))
    lua_pushnumber(l, (long)newtRadioGetCurrent(w->comp));
  else
  {
    int i;
    for (i= 0; widgetFunctions[i].name; i++)
    {
      if (strcmp(widgetFunctions[i].name, name) == 0)
      {
        lua_pushcfunction(l, widgetFunctions[i].func);
        return 1;
      }
    }
    switch (w->subtype)
    {
    case 'X':
      for (i= 0; listboxFunctions[i].name; i++)
      {
        if (strcmp(listboxFunctions[i].name, name) == 0)
        {
          lua_pushcfunction(l, listboxFunctions[i].func);
          return 1;
        }
      }
      break;
    case 'K':
      for (i= 0; checktreeFunctions[i].name; i++)
      {
        if (strcmp(checktreeFunctions[i].name, name) == 0)
        {
          lua_pushcfunction(l, checktreeFunctions[i].func);
          return 1;
        }
      }
      break;
    }
    luaL_error(l, "invalid member %s", name);
  }
  return 1;
}

static int widgetDestroy(lua_State *l)
{
//  snackWidget * s = luaL_checkudata(l, 2, "SNACK_WIDGET");
  
  puts("DESTROY WID");
  
  //Py_XDECREF (s->scs.cb);
  //Py_XDECREF (s->scs.data);
  
  return 0;
}


static int widgetMatchId(lua_State *l)
{
  snackWidget *w;
  const char *id;
  char myid[100];
  
  popArgs(l, 'w', "s", &w, &id);
  
  sprintf(myid, "%p", w->comp);
  
  if (strcmp(myid, id)==0)
    lua_pushnumber(l, 1);
  else
    lua_pushnumber(l, 0);
  return 1;
}

static int widgetAddCallback(lua_State *l)
{
/*  
  
  s->scs.cb = NULL;
  s->scs.data = NULL;
  
  if (!PyArg_ParseTuple(args, "O|O", &s->scs.cb, &s->scs.data))
    return NULL;
  
  Py_INCREF (s->scs.cb);
  Py_XINCREF (s->scs.data);
  
  newtComponentAddCallback(s->co, callbackMarshall, &s->scs);
  */
  return 0;
}

static int widgetEntrySetValue(lua_State *l)
{
  // <w>.entrySetValue(str)
  const char * val;
  snackWidget *s;
  
  popArgs(l, 'w', "s", &s, &val);
  
  newtEntrySet(s->comp, (char*)val, 1);
  
  return 0;
}

static int widgetEntrySetFlags(lua_State *l)
{
  // <w>.entrySetFlags(flags, sense)
  snackWidget *s;
  int flag, sense;
  
  popArgs(l, 'w', "ii", &s, &flag, &sense);
  
  newtEntrySetFlags(s->comp, flag, sense);

  return 0;
}


static int widgetListboxAdd(lua_State *l)
{
  // <w>.addItem(str) -> index
  snackWidget *s;
  const char * text;
  
  popArgs(l, 'w', "s", &s, &text);
  
  newtListboxAddEntry(s->comp, (char*)text, I2P(s->anint));
  
  lua_pushnumber(l, s->anint++);
  
  return 1;
}

static int widgetListboxIns(lua_State *l)
{
  // <w>.listboxInsertItem(str, index) -> index
  const char * text;
  int key;
  snackWidget *s;
  
  popArgs(l, 'w', "si", &s, &text, &key);
  
  newtListboxInsertEntry(s->comp, (char*)text, I2P(s->anint), I2P(key));
  
  lua_pushnumber(l, s->anint++);
  
  return 1;
}

static int widgetListboxDel(lua_State *l)
{
  // <w>.listboxDeleteItem(index)
  int key;
  snackWidget *s;
  
  popArgs(l, 'w', "i", &s, &key);
  
  newtListboxDeleteEntry(s->comp, I2P(key));
  
  return 0;
}

static int widgetListboxGet(lua_State *l)
{
  snackWidget *s;
  
  popArgs(l, 'w', "", &s);
  
  lua_pushnumber(l, (long)newtListboxGetCurrent(s->comp));
  
  return 1;
}

static int widgetListboxSet(lua_State *l)
{
  snackWidget *s;
  int index;

  popArgs(l, 'w', "i", &s, &index);
  
  newtListboxSetCurrentByKey(s->comp, I2P(index));
  
  return 0;
}

static int widgetListboxSetW(lua_State *l)
{
  snackWidget *s;
  int width;
  
  popArgs(l, 'w', "i", &s, &width);
  
  newtListboxSetWidth(s->comp, width);
  
  return 0;
}

static int widgetListboxClear(lua_State *l)
{
  snackWidget *s;
  
  popArgs(l, 'w', "", &s);
  
  newtListboxClear(s->comp);
  
  return 0;
}


static int checkboxTreeWidget(lua_State *l)
{
  int height;
  int scrollBar = 0;
  int hide_checkbox = 0;
  int unselectable = 0;
  int x, y;
  int flags;
  snackWidget * widget;
  
  // (x, y, height, [scrollbar, hide, unselectable])
  popArgs(l, 0, "iii|iii", &x, &y, &height, &scrollBar, &hide_checkbox, &unselectable);

  flags = (scrollBar ? NEWT_FLAG_SCROLL : 0) |
    (hide_checkbox ? NEWT_CHECKBOXTREE_HIDE_BOX : 0) |    
    (unselectable ? NEWT_CHECKBOXTREE_UNSELECTABLE : 0);

  // stupid bug in newt will only take x,y into account if there's a scrollbar
  flags|= NEWT_FLAG_SCROLL;
  widget = snackWidgetNew (l, 'K');
  widget->comp = newtCheckboxTree(x, y, height, flags);
  
  widget->anint = 1;

  return 1;
}

static int widgetCheckboxTreeAddItem(lua_State *l)
{
  snackWidget *s;
  char * text;
  int selected = 0;
  int * path= NULL;

  popArgs(l, 'w', "spi", &s, &text, &path, &selected);
  
  newtCheckboxTreeAddArray(s->comp, text, I2P(s->anint),
                           selected ? NEWT_FLAG_SELECTED : 0, path);
  free(path);
  lua_pushnumber(l, s->anint++);
  return 1;
}

static int widgetCheckboxTreeGetCur(lua_State *l)
{
  snackWidget *s;

  popArgs(l, 'w', "", &s);

  lua_pushnumber(l, (int)(long)newtCheckboxTreeGetCurrent(s->comp));
  return 1;
}

static int widgetCheckboxTreeSetEntry(lua_State *l)
{
  snackWidget *s;
  int data;
  const char *text;

  popArgs(l, 'w', "si", &s, &text, &data);;
  
  newtCheckboxTreeSetEntry(s->comp, I2P(data), text);
  
  return 0;
}

static int widgetCheckboxTreeSetWidth(lua_State *l)
{
  snackWidget *s;
  int width;
  
  popArgs(l, 'w', "i", &s, &width);
  
  newtCheckboxTreeSetWidth(s->comp, width);
  
  return 0;
}

static int widgetCheckboxTreeSetCurrent(lua_State *l)
{
  snackWidget *s;
  int data;
  
  popArgs(l, 'w', "i", &s, &data);
  
  newtCheckboxTreeSetCurrent(s->comp, I2P(data));

  return 0;
}

static int widgetCheckboxTreeSetEntryValue(lua_State *l)
{
  snackWidget *s;
  int data;
  int isOn = 1;
  
  popArgs(l, 'w', "i|i", &s, &data, &isOn);

  s= luaL_checkudata(l, 1, "SNACK_WIDGET");
  
  newtCheckboxTreeSetEntryValue(s->comp, I2P(data),
                                isOn ? NEWT_CHECKBOXTREE_SELECTED :
                                NEWT_CHECKBOXTREE_UNSELECTED);
  return 0;
}

static int widgetCheckboxTreeGetEntryValue(lua_State *l)
{
  snackWidget *s;
  int data;
  int isOn = 0;
  int isBranch = 0;
  char selection;

  popArgs(l, 'w', "i", &s, &data);
  
  selection = newtCheckboxTreeGetEntryValue(s->comp, I2P(data));
  
  if (selection == -1) luaL_error(l, "invalid tree item");
  
  switch (selection)
  {
  case NEWT_CHECKBOXTREE_EXPANDED:
    isOn = 1;
  case NEWT_CHECKBOXTREE_COLLAPSED:
    isBranch = 1;
    break;
  case NEWT_CHECKBOXTREE_UNSELECTED:
    break;
  default:
    isOn = 1;
    break;
  }

  lua_newtable(l);
  lua_pushnumber(l, isBranch);
  lua_rawseti(l, -2, 1);
  lua_pushnumber(l, isOn);
  lua_rawseti(l, -2, 2);
  return 1;
}

static int widgetCheckboxTreeGetSel(lua_State *l)
{
  snackWidget *s;
  void ** selection;
  int numselected;
  int i;

  popArgs(l, 'w', "", &s);
  
  selection = (void **) newtCheckboxTreeGetSelection(s->comp, &numselected);
  
  lua_newtable(l);
  if (!selection)
    return 1;
  
  for (i = 0; i < numselected; i++)
  {
    lua_pushnumber(l, (int)(long)selection[i]);
    lua_rawseti(l, -2, i+1);
  }

  free(selection);
  
  return 1;
}


/*
static int l_wstrlen(lua_State *l)
{
  const char *str;
  int len = -1;
  
  if (lua_gettop(l) == 2)
    len= luaL_checkint(l, 1);
  str= luaL_checkstring(l, 1);
  
  lua_pushnumber(l, wstrlen(str, len));
  
  return 1;
}
 */


void myx_lua_init_textui(lua_State *l)
{
  static struct {
    char *name;
    long value;
  } int_consts[]= {    
    {"FD_READ", NEWT_FD_READ},
    {"FD_WRITE", NEWT_FD_WRITE},
    {"FD_EXCEPT", NEWT_FD_EXCEPT},
    
    {"KEY_F1", NEWT_KEY_F1},
    {"KEY_F2", NEWT_KEY_F2},
    {"KEY_F3", NEWT_KEY_F3},
    {"KEY_F4", NEWT_KEY_F4},
    {"KEY_F5", NEWT_KEY_F5},
    {"KEY_F6", NEWT_KEY_F6},
    {"KEY_F7", NEWT_KEY_F7},
    {"KEY_F8", NEWT_KEY_F8},
    {"KEY_F9", NEWT_KEY_F9},
    {"KEY_F10", NEWT_KEY_F10},
    {"KEY_F11", NEWT_KEY_F11},
    {"KEY_F12", NEWT_KEY_F12},
    
    {"FLAG_DISABLED", NEWT_FLAG_DISABLED},
    {"FLAGS_SET", NEWT_FLAGS_SET},
    {"FLAGS_RESET", NEWT_FLAGS_RESET},
    {"FLAGS_TOGGLE", NEWT_FLAGS_TOGGLE},
    
    {"ARG_APPEND", NEWT_ARG_APPEND},
    {NULL, 0}
  };
  int i;
  int dlg;

  luaL_openlib(l, "dlg", dialogFunctions, 0);
 // lua_pushstring(l, "dlg");
 // lua_gettable(l, LUA_GLOBALSINDEX);
 // dlg= lua_tonumber(l, -1);
 // lua_pop(l, 1);
  dlg= lua_gettop(l);
  
  for (i= 0; int_consts[i].name; i++)
  {
    lua_pushstring(l, int_consts[i].name);
    lua_pushnumber(l, int_consts[i].value);
    lua_settable(l, dlg);
  }

  lua_pushstring(l, "FORM_EXIT_HOTKEY");
  lua_pushstring(l, "hotkey");
  lua_settable(l, dlg);

  lua_pushstring(l, "FORM_EXIT_WIDGET");
  lua_pushstring(l, "widget");
  lua_settable(l, dlg);

  lua_pushstring(l, "FORM_EXIT_TIMER");
  lua_pushstring(l, "timer");
  lua_settable(l, dlg);

  lua_pushstring(l,  "FORM_EXIT_FDREADY");
  lua_pushstring(l, "fdready");
  lua_settable(l, dlg);

  lua_pop(l, 1);

}
