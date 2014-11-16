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

#include "myx_grt_base.h"

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  #include <shlobj.h>
  #include <shellapi.h>
#endif

// --------------------------------------------------------------------------
// module registration function

MYX_GRT_MODULE* myx_register_builtin_grt_module_base(MYX_GRT *grt)
{
  return myx_grt_module_register_builtin(grt, &grt_module_base, grt);
}

// --------------------------------------------------------------------------

static int (*copy_to_clipboard_callback)(const char *text, void *data)= 0;
static void *copy_to_clipboard_data= 0;
void myx_grt_module_base_set_copy_to_clipboard_callback(int (*callback)(const char *text, void *data), void *data)
{
  copy_to_clipboard_callback= callback;
  copy_to_clipboard_data= data;
}


/**
 ****************************************************************************
 * @brief Returns a GUID
 *
 *   Creates a globally unique indentifier
 *
 * @param param nil
 * @param data pointer to the Grt
 * 
 * @return  A GUID
 *****************************************************************************/
MYX_GRT_VALUE * get_guid(MYX_GRT_VALUE *param, void *data)
{
  char *guid= myx_grt_get_guid();
  MYX_GRT_VALUE *value= myx_grt_value_from_string(guid);

  g_free(guid);
  return make_return_value(value);
}

/**
 ****************************************************************************
 * @brief Returns the application's data directory
 *
 *   This returns c:\documents and settings\[user]\application data\mysql on
 * on Windows and ~/.mysql on Linux
 *
 * @param param nil
 * @param data pointer to the Grt
 * 
 * @return the application's data directory
 *****************************************************************************/
MYX_GRT_VALUE * get_app_data_dir(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *value;
  char *data_dir;
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  LPITEMIDLIST pidl;
  char path[MAX_PATH];

  SHGetFolderLocation(NULL, CSIDL_APPDATA, NULL, 0, &pidl);

  SHGetPathFromIDList(pidl, path);

  CoTaskMemFree(pidl);

  data_dir= g_strdup_printf("%s\\MySQL\\", path);

#elif defined(__APPLE__)
  data_dir= g_strdup_printf("%s/Library/Application Support/MySQL/", g_get_home_dir());
#else
  data_dir= g_strdup_printf("%s/.mysqlgui/", g_get_home_dir());
#endif

  value= myx_grt_value_from_string(data_dir);

  g_free(data_dir);

  return make_return_value(value);
}

/**
 ****************************************************************************
 * @brief Returns the type name of the OS
 *
 *   Returns WINDOWS on Windows, OSX on a mac and UNIX on all other platforms
 *
 * @param param nil
 * @param data pointer to the Grt
 * 
 * @return type name of the OS
 *****************************************************************************/
MYX_GRT_VALUE * get_os_type_name(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *value;

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  value= myx_grt_value_from_string("WINDOWS");
#elif defined(__APPLE__)
  value= myx_grt_value_from_string("OSX");
#else
  value= myx_grt_value_from_string("UNIX");
#endif

  return make_return_value(value);
}

/**
 ****************************************************************************
 * @brief Checks if the given string matches with the pattern
 *
 *   Checks if the given string matches with the pattern
 *
 * @param param nil
 * @param data pointer to the Grt
 * 
 * @return  0 or 1
 *****************************************************************************/
MYX_GRT_VALUE * pattern_match(MYX_GRT_VALUE *param, void *data)
{
  int match, case_sensitive= 0;

  if ( (myx_grt_list_item_count(param) < 2) || 
    (myx_grt_value_get_type(myx_grt_list_item_get(param, 0)) != MYX_STRING_VALUE) || 
    (myx_grt_value_get_type(myx_grt_list_item_get(param, 1)) != MYX_STRING_VALUE) )
    return make_return_value_error("patternMatch takes two parameters, a string and a pattern string.", "");

  if ( (myx_grt_list_item_count(param) == 3) && 
    (myx_grt_value_get_type(myx_grt_list_item_get(param, 2)) == MYX_INT_VALUE) )
    case_sensitive= myx_grt_value_as_int(myx_grt_list_item_get(param, 2));

  match= myx_match_pattern(myx_grt_list_item_get_as_string(param, 0), 
    myx_grt_list_item_get_as_string(param, 1), case_sensitive, 1);

  return make_return_value(myx_grt_value_from_int(match));
}

/**
 ****************************************************************************
 * @brief Returns a GUID
 *
 *   Creates a globally unique indentifier
 *
 * @param param nil
 * @param data pointer to the Grt
 * 
 * @return  A GUID
 *****************************************************************************/
MYX_GRT_VALUE * value_from_text(MYX_GRT_VALUE *param, void *data)
{
  int num= 1;
  const char *txt;
  char *val;
  MYX_GRT_VALUE *res= NULL;

  if ( (myx_grt_list_item_count(param) < 2) || 
    (myx_grt_value_get_type(myx_grt_list_item_get(param, 0)) != MYX_STRING_VALUE) || 
    (myx_grt_value_get_type(myx_grt_list_item_get(param, 1)) != MYX_STRING_VALUE) )
    return make_return_value_error("patternMatch takes two parameters, a string and a pattern string.", "");

  if ( (myx_grt_list_item_count(param) == 3) && 
    (myx_grt_value_get_type(myx_grt_list_item_get(param, 2)) == MYX_INT_VALUE) )
    num= myx_grt_value_as_int(myx_grt_list_item_get(param, 2));

  txt= myx_grt_list_item_get_as_string(param, 0);

  val= get_value_from_text_ex(txt, (int) strlen(txt), myx_grt_list_item_get_as_string(param, 1), num);

  if (val)
    res= myx_grt_value_from_string(val);

  g_free(val);

  return make_return_value(res);
}



MYX_GRT_VALUE * check_dir(MYX_GRT_VALUE *param, void *data)
{
  char *path;
  int exists= 0;
  if (myx_grt_list_item_count(param) != 1 || myx_grt_value_get_type(myx_grt_list_item_get(param, 0)) != MYX_STRING_VALUE)
    return make_return_value_error("checkDir takes one string parameter", "");

  path= g_strdup(myx_grt_list_item_get_as_string(param, 0));
  if ((path[strlen(path) - 1] == '\\') || (path[strlen(path) - 1] == '/'))
    path[strlen(path) - 1]= 0;

  if (g_file_test(path, G_FILE_TEST_IS_DIR))
    exists= 1;

  g_free(path);

  return make_return_value(myx_grt_value_from_int(exists));
}


static int create_dir_rec(const char *path)
{
  char *dirname;
  int error_no;
  int mkdir_ret= myx_mkdir(path, 0660, &error_no);

  if (mkdir_ret < 0 && error_no != EEXIST)
  {
    if (error_no == ENOENT)
    {
      int r;
      dirname= g_path_get_dirname(path);
      if (!dirname)
        return -1;
      r= create_dir_rec(dirname);
      g_free(dirname);
      if (r == 0)
      {
        if (myx_mkdir(path, 0660, &error_no) == 0)
          return 0;
      }
      return -1;
    }
    else
      return -1;
  }
  return 0;
}

MYX_GRT_VALUE * create_dir(MYX_GRT_VALUE *param, void *data)
{ 
  const char *path;
  if (myx_grt_list_item_count(param) != 1 || myx_grt_value_get_type(myx_grt_list_item_get(param, 0)) != MYX_STRING_VALUE)
    return make_return_value_error("createDir takes one string parameter", "");

  path= myx_grt_list_item_get_as_string(param, 0);
  
  if (create_dir_rec(path) != 0)
    return make_return_value_error("error creating directory", strerror(errno));

  return make_return_value(myx_grt_value_from_int(1));
}

MYX_GRT_VALUE * grt_object_editor(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *editor_list= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), "/app/editors");
  MYX_GRT_VALUE *object;
  MYX_GRT_VALUE *element;
  MYX_GRT_VALUE *action_value;
  int action;

  if (myx_grt_list_item_count(param) != 3)
    return make_return_value_error("editObj takes 3 parameters (object, element, action).", "");

  object= myx_grt_list_item_get(param, 0);
  element= myx_grt_list_item_get(param, 1);
  action_value= myx_grt_list_item_get(param, 2);
  if (myx_grt_value_get_type(action_value) != MYX_INT_VALUE)
    return make_return_value_error("last argument of editObj() is the action number", "");
  action= myx_grt_value_as_int(action_value);

  if (editor_list)
  {
    MYX_GRT_VALUE *obj_or_struct_name= object;
    const char * object_struct_name;
    unsigned int i;

    if (myx_grt_value_get_type(obj_or_struct_name) == MYX_STRING_VALUE)
      object_struct_name= myx_grt_value_as_string(obj_or_struct_name);
    else
      object_struct_name= myx_grt_dict_struct_get_name(obj_or_struct_name);

    // take a look at all available editors
    for (i= 0; i < myx_grt_list_item_count(editor_list); i++)
    {
      MYX_GRT_VALUE *editor= myx_grt_list_item_get(editor_list, i);
      const char *editor_struct_name= myx_grt_dict_item_get_as_string(editor, "objectStructName");
      //int editor_rating= myx_grt_dict_item_get_as_int(editor, "rating");
      int editor_action= myx_grt_dict_item_get_as_int(editor, "actionId");

      if (action == 0 || editor_action == 0 || editor_action == action)
      {
        if (myx_grt_struct_is_or_inherits_from(grt, object_struct_name, editor_struct_name))
        {
          return make_return_value(editor);
        }
      }
    }

  }

  return make_return_value(myx_grt_value_from_int(0));
}

MYX_GRT_VALUE * edit_obj(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *editor_list= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), "/app/editors");
  MYX_GRT_VALUE *object;
  MYX_GRT_VALUE *element;
  MYX_GRT_VALUE *action_value;
  int action;

  if (myx_grt_list_item_count(param) != 3)
    return make_return_value_error("editObj takes 3 parameters (object, element, action).", "");

  object= myx_grt_list_item_get(param, 0);
  element= myx_grt_list_item_get(param, 1);
  action_value= myx_grt_list_item_get(param, 2);
  if (myx_grt_value_get_type(action_value) != MYX_INT_VALUE)
    return make_return_value_error("last argument of editObj() is the action number", "");
  action= myx_grt_value_as_int(action_value);

  if (editor_list)
  {
    const char * object_struct_name= myx_grt_dict_struct_get_name(object);
    unsigned int i;
    int rating_best= -1, rating_best_exact= -1;
    MYX_GRT_VALUE *editor_best= NULL;
    MYX_GRT_VALUE *editor_best_exact= NULL;

    // take a look at all available editors
    for (i= 0; i < myx_grt_list_item_count(editor_list); i++)
    {
      MYX_GRT_VALUE *editor= myx_grt_list_item_get(editor_list, i);
      const char *editor_struct_name= myx_grt_dict_item_get_as_string(editor, "objectStructName");
      int editor_rating= myx_grt_dict_item_get_as_int(editor, "rating");
      int editor_action= myx_grt_dict_item_get_as_int(editor, "actionId");
      if (editor_action == 0 || action == 0 || editor_action == action)
      {
        if ((strcmp2(object_struct_name, editor_struct_name) == 0) && (editor_rating > rating_best_exact))
        {
          editor_best_exact= editor;
          rating_best_exact= editor_rating;
        }
        if (myx_grt_struct_is_or_inherits_from(grt, object_struct_name, editor_struct_name) && 
            (editor_rating > rating_best))
        {
          editor_best= editor;
          rating_best= editor_rating;
        }
      }
    }

    // if there is an editor for exactly this struct, prefere it over all editors that
    // edit only parents structs
    if (editor_best_exact)
      editor_best= editor_best_exact;
    
    if (editor_best)
    {
      MYX_GRT_ERROR error;

      return make_return_value(myx_grt_function_get_and_call(grt, 
        myx_grt_dict_item_get_as_string(editor_best, "moduleName"),
        myx_grt_dict_item_get_as_string(editor_best, "moduleFunctionName"),
        0, param, &error));
    }
    else
      return make_return_value(0);
  }
  else
    return make_return_value_error("No editors available.", "");
}

unsigned int ucs2_length(const char *s, unsigned int len) {
	unsigned int ulen = 0;
  unsigned int i;
  unsigned char ch;

	for (i= 0; i < len; i++) {
		ch = (unsigned char)(s[i]);

		if ((ch < 0x80) || (ch > (0x80 + 0x40)))
			ulen++;
	}
	return ulen;
}

/**
 ****************************************************************************
 * @brief Copies the given string to the clipboard
 *
 *   Converts the given GRT value to a string and copies it to the clipboard
 *
 * @param param the parameter list
 * @param data pointer to the Grt
 * 
 * @return  A GUID
 *****************************************************************************/
MYX_GRT_VALUE * copy_to_clipboard(MYX_GRT_VALUE *param, void *data)
{
  char *str;
  int l;
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  LPTSTR  lptstrCopy;
  HGLOBAL hand; 
  HGLOBAL uhand = NULL;
  int ucs2_l;
  wchar_t *uptr;
#endif

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes one value as parameter", "");

  str= myx_grt_value_formated_as_string(myx_grt_list_item_get(param, 0));
  l= (int) strlen(str);
  
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  // get UCS2 length
  ucs2_l= ucs2_length(str, l);

  // allocate mem and convert utf8 string to UCS2
  uhand = GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, 2 * (ucs2_l + 1));
  if (uhand) {
    uptr = (wchar_t *)(GlobalLock(uhand));

    MultiByteToWideChar(CP_UTF8, 0, str,
      l + 1, uptr, ucs2_l);
  }

  if (!OpenClipboard(NULL)) 
    return make_return_value_error("Cannot open clipboard.", "");
  EmptyClipboard();

  //copy the string using the CF_TEXT format. 
  hand = GlobalAlloc(GMEM_MOVEABLE, (l + 1) * sizeof(TCHAR)); 
  if (hand == NULL) 
  { 
      CloseClipboard(); 
      return make_return_value_error("Failed to allocate memory for the string", "");; 
  }

  // Lock the handle and copy the text to the buffer
  lptstrCopy = GlobalLock(hand); 
  memcpy(lptstrCopy, str, l * sizeof(TCHAR)); 
  lptstrCopy[l] = (TCHAR) 0;    // null character 
  GlobalUnlock(hand);

  // Place the ANSI handle on the clipboard
  SetClipboardData(CF_TEXT, hand);

  // Place the UNICODE handle on the clipboard
  GlobalUnlock(uhand);
  SetClipboardData(CF_UNICODETEXT, uhand);

  // Close the clipboard
  CloseClipboard();
#else
  if (copy_to_clipboard_callback)
  {
    if ((*copy_to_clipboard_callback)(str, copy_to_clipboard_data) < 0)
      return make_return_value_error("Error copying data to clipboard", "");
  }
  else
    return make_return_value_error("No callback set for clipboard copy", "");
#endif

  g_free(str);

  return make_return_value(myx_grt_value_from_int(1));
}
