/* Copyright (C) 2003,2004 MySQL AB

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

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include <assert.h>
#include <errno.h>

#include <glib.h>

#include "myx_admin_library.h"
#include "myx_xml_util_functions.h"

#define MAX_LINE_LENGTH 4096        /*the same as in mysys/default.c*/

/* an example of a name-only option would be "enable-innodb"; 
*  enabled means it should be in the
*  conf-file, disabled means it should not be written to the config-file 
*/
#define NAME_ONLY_OPTION_ENABLED "checked"
#define NAME_ONLY_OPTION_DISABLED "unchecked"


/*
* Private type definitions
*/

typedef enum {
  ILT_ERROR=-1,
  ILT_NOCONTENT,
  ILT_SECTION_IDENTIFIER,
  ILT_NAME_ONLY,
  ILT_NAME_VALUE_PAIR,
  ILT_NAME_VALUE_PAIR_NOT_READ
}
INI_LINE_TYPE;

typedef struct
{
  int found_group;
  int read_values;
  char *current_section_name;
  char *name;
  char *value;
} PROCESS_LINE_STATUS;

typedef struct
{
  char *line;
  INI_LINE_TYPE type;
  char *name;         /* NULL if type != ILT_NAME_ONLY && 
                                 type != ILT_NAME_VALUE_PAIR */
  char *value;        /* NULL if type != ILT_NAME_VALUE_PAIR */
  char *section_name; /* NULL if type != ILT_SECTION_IDENTIFIER */

  MYX_GUI_WIDGET *widget;
} INI_FILE_LINE;

typedef struct
{
  char *name;
  char *value;
  INI_LINE_TYPE type;
  int loose_option;

  int known_option;
  MYX_GUI_WIDGET *widget;
} NORMALIZED_OPTION;

/*
* Forward declarations
*/
static INI_LINE_TYPE process_ini_line(PROCESS_LINE_STATUS *plt, const char *line, const char **groups,
  MYX_ADMIN_LIB_ERROR *error_code);
static MYX_GUI_WIDGET *find_widget(xmlChar *mysql_identifier, MYX_GUI_DESCRIPTION *gui_desc);
static void init_line_status(PROCESS_LINE_STATUS * pls);
static void free_pls2( PROCESS_LINE_STATUS *pls);
static int find_group(const char* group_name, const char **group_list);
static void write_description(FILE *file, const MYX_GUI_WIDGET *widget);
static INI_FILE_LINE* new_ini_file_line(void);
static void free_ini_file_line(INI_FILE_LINE *fl);
static char *remove_end_comment(char *ptr);
static int written_to_file(const char *widget_id, const GPtrArray *already_processed_widgets);
static INI_LINE_TYPE read_in_name_value_pair(PROCESS_LINE_STATUS *pls, char* ptr);
static void write_gui_desc_values_to_ini_file(MYX_GUI_DESCRIPTION *desc, GPtrArray *already_processed_widgets,
  FILE *ini_file);
static int get_boolean_option_value(int result, const char* option_name, const char *value);
static char *has_loose_prefix(const char* option_name);
static char *xstr_append(char *base_str, const char *addon);
static NORMALIZED_OPTION* normalize_option(const char *name, const char *value, INI_LINE_TYPE result,
  MYX_GUI_DESCRIPTION *desc);
static void free_normalized_option(NORMALIZED_OPTION *option);

//----------------------------------------------------------------------------------------------------------------------

/*
* Public functions
*/

/*
* Returns all sections of a given mysql option file.
* A section is a line of the form [sectionName]
*/
MYX_STRINGLIST* myx_get_all_cnf_sections(const char *filename,
                                         MYX_ADMIN_LIB_ERROR *error_code)
{
  FILE *ini_file;
  PROCESS_LINE_STATUS pls;
  INI_LINE_TYPE result;
  char buffer[MAX_LINE_LENGTH];
  MYX_STRINGLIST *stringlist;
  char **tmp;

  ini_file= myx_fopen(filename, "r");
  if (ini_file == NULL)
  {
    *error_code= MYX_ADMIN_ERROR_CANT_OPEN_FILE;
    return NULL;
  }

  init_line_status(&pls);
  stringlist= g_malloc0(sizeof(MYX_STRINGLIST));

  // Read each line of the mysql-config-file.
  while (fgets(buffer, sizeof(buffer), ini_file) )
  {
    // Fill the gui_description with the value of this line or do nothing in case of a comment.
    result= process_ini_line(&pls, buffer,NULL, error_code);
    if (result == ILT_ERROR)
    {
      goto err;
    }
    else if (result == ILT_SECTION_IDENTIFIER)
    {
      stringlist->strings_num++;
      tmp= (char**) g_realloc(stringlist->strings, sizeof(MYX_STRINGLIST) * stringlist->strings_num);
      if (tmp == NULL)
      {
        *error_code= MYX_ADMIN_INI_PARSE_ERROR;
        goto err;
      }
      stringlist->strings= tmp;
      stringlist->strings[stringlist->strings_num - 1]= g_strdup(pls.current_section_name);
    }
  }

  if (ferror(ini_file))
  {
    *error_code= MYX_ADMIN_INI_PARSE_ERROR;
    goto err;
  }

  fclose(ini_file);
  free_pls2(&pls);
  return stringlist;
err:
  fclose(ini_file);
  free_pls2(&pls);
  myx_free_stringlist(stringlist);
  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the value for the option that is found
 * by name in the given section.
 * The return value has to be freed by the caller.
 */
char * myx_get_cnf_value(const char *filename, const char *section,
                         const char *name, MYX_ADMIN_LIB_ERROR *error_code)
{
  FILE *ini_file;
  const char *groups[]= {NULL, NULL};
  PROCESS_LINE_STATUS pls;
  INI_LINE_TYPE result;
  char buffer[MAX_LINE_LENGTH];
  char *value= NULL;

  // Borland's compiler does not allow to initialize this value already in the variable declaration.
  // Only constant values are allowed there.
  groups[0] = section;

  *error_code= MYX_ADMIN_NO_ERROR;
  
  ini_file= myx_fopen(filename, "r");
  if (ini_file == NULL)
  {
    *error_code= MYX_ADMIN_ERROR_CANT_OPEN_FILE;
    return NULL;
  }
  init_line_status(&pls);

  /*read each line of the mysql-config-file*/
  while (fgets(buffer, sizeof(buffer), ini_file) )
  {
    /* fill the gui_description with the value of this line or do
                                               nothing in case of a comment  */
    result= process_ini_line(&pls, buffer,groups, error_code);
    if (result == ILT_ERROR)
    {
      goto end; // the error_code was set in the process_ini_line..
    }
    else if (pls.name && !strcmp(pls.name,name))
    {
      if (result == ILT_NAME_VALUE_PAIR)
      {
        value= g_strdup(pls.value);
        break;
      }
      else if (result == ILT_NAME_ONLY)
      {
        value= g_strdup(NAME_ONLY_OPTION_ENABLED);
        break;
      }
    }
  }

  if (ferror(ini_file))
  {
    if(value)
    {
      g_free(value);
      value= 0;
    }
    *error_code= MYX_ADMIN_INI_PARSE_ERROR;
  }

end:
  fclose(ini_file);
  free_pls2(&pls);
  return value;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_GUI_DESCRIPTION * myx_init_gui_description_with_default_values(MYX_GUI_DESCRIPTION *desc)
{
  MYX_GUI_WIDGET *widget;
  unsigned int i,j,k;

  for (i= 0; i < desc->pages_num; i++)
  {
    for (j= 0; j < desc->pages[i].groups_num; j++)
    {
      for (k= 0; k < desc->pages[i].groups[j].widgets_num; k++)
      {
        widget= desc->pages[i].groups[j].widgets+k;

        if (widget->value)
        {
          xmlFree(widget->value);
          widget->value= NULL;
        }

        if (widget->widget_type == MYX_CHECKBOX)
          widget->active= 1;

        if (widget->default_value)
          widget->value= (char*)xmlStrdup((xmlChar*)widget->default_value);
      }
    }
  }
  return desc;
}

//----------------------------------------------------------------------------------------------------------------------

int group_exists_in_cnf_file(const char *ini_filepath, const char *group)
{
  unsigned int i;
  MYX_STRINGLIST *available_groups;
  MYX_ADMIN_LIB_ERROR error_code;

  error_code= MYX_ADMIN_NO_ERROR;
  available_groups=  myx_get_all_cnf_sections(ini_filepath, &error_code);
  if (error_code != MYX_ADMIN_NO_ERROR)
    return 0;

  for (i= 0; i < available_groups->strings_num; i++)
  {
    if (g_ascii_strncasecmp(available_groups->strings[i], group, 32) == 0)
    {
      return 1;
    }
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Parse the given mysql-config-file and fill the MYX_GUI_DESCRIPTION-struct
 *
 * SYNOPSIS
 *   desc                the gui-description where option-values
 *                       are to be saved
 *   ini_filpath         the full pathname of the configuration file
 *   groups              a null-terminated list of groupnames that should be read in
 *   error_code          this variable will be set to 0 or to an appropriate error-code in case of an error
 * DESCRIPTION
 *
 * RETURN VALUE
 *   desc or NULL in case of an error
 * NOTES
 */
MYX_GUI_DESCRIPTION* myx_process_mysql_cnf_file(MYX_GUI_DESCRIPTION *desc,
                                                const char *ini_filepath,
                                                const char *group,
                                                MYX_ADMIN_LIB_ERROR *error_code)
{
  FILE *ini_file;
  char buffer[MAX_LINE_LENGTH];
  PROCESS_LINE_STATUS pls;
  int result;
  const char *groups[]= {NULL, NULL};
  *error_code= MYX_ADMIN_NO_ERROR;

  // check if requested group exists. if not, read the default mysqld group
  if (group_exists_in_cnf_file(ini_filepath, group))
    groups[0]= group;
  else
    groups[0]= "mysqld";


  ini_file= myx_fopen(ini_filepath, "r");
  if (ini_file == NULL)
  {
    *error_code= MYX_ADMIN_ERROR_CANT_OPEN_FILE;
    return NULL;
  }

  init_line_status(&pls);

  /* read each line of the mysql-config-file */
  while (fgets(buffer, sizeof(buffer), ini_file) )
  {
    /* fill the gui_description with the value of this line or 
    * do nothing in case of a comment
    */
    result= process_ini_line(&pls, buffer,groups, error_code);

    if (result == ILT_ERROR)
    {
      desc= NULL;  // the error_code was set in the process_ini_line..
      goto end;
    }
    else if (result == ILT_NAME_ONLY || result == ILT_NAME_VALUE_PAIR)
    {
      NORMALIZED_OPTION *normalized_option;

      /* normalize the input */
      normalized_option= normalize_option(pls.name, pls.value,result, desc);
      if (!normalized_option)
        continue;

      if (normalized_option->known_option)
      {
        MYX_GUI_WIDGET *widget= normalized_option->widget;
        assert(widget);

        if (widget->value && !widget->multiple)
          xmlFree(widget->value);

        widget->active= normalized_option->known_option;
        widget->loose_option= normalized_option->loose_option;

        widget->value= !normalized_option->widget->multiple 
                          ? (char*)xmlStrdup((xmlChar*)normalized_option->value)
                          : xstr_append(xstr_append(widget->value, ","),
                                        normalized_option->value);
      }

      free_normalized_option(normalized_option);
    } /* end if result == ILT_NAME or NAME_VALUE_PAIR */
  }

  if (ferror(ini_file))
  {
    *error_code= MYX_ADMIN_INI_PARSE_ERROR;
    desc= NULL;
  }

end:
  fclose(ini_file);
  free_pls2(&pls);
  return desc;
}

//----------------------------------------------------------------------------------------------------------------------

void safe_fprintf_TEXTEDIT(FILE * file, MYX_GUI_WIDGET * widget)
{
  if (strcmp2(widget->textedit->edit_type, "file") &&
      strcmp2(widget->textedit->edit_type, "directory"))
  { // Handle filepaths by replacing \ with /
    fprintf(file,"%s=%s\n", widget->id, widget->value);
  }
  else
  {
    char *tmp= str_g_replace(g_strdup(widget->value), "\\", "/");
    fprintf(file, "%s=%s\n", widget->id, tmp);
    g_free(tmp);
  }
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * A function to write the values from the MYX_GUI_DESCRIPTION struct
 * back to a cnf file, preserving and adding comments.
 *
 * SYNOPSIS
 *   ini_filepath means the name of the ini-file including the full
 *                path information. (eg /etc/my.cnf)
 * DESCRIPTION
 *  This function updates one specific group/section of an ini-file.
 *  The rest of the file is just copied.
 * RETURN VALUE
 *  0 on success, error_code otherwise
 *
 * NOTES
 * Only options that are different from the default-value are written
 * to the config-file
 * If a new option is added, its description is added as a comment
 */
MYX_ADMIN_LIB_ERROR myx_update_mysql_cnf_file(MYX_GUI_DESCRIPTION *desc,
                                              const char *ini_filepath,
                                              const char *group)
{
  FILE *new_file;
  char *new_ini_filepath;
  char *bak_filepath;
  MYX_ADMIN_LIB_ERROR error;
  const char *actual_group;
  
  new_ini_filepath= g_strdup_printf("%s.new", ini_filepath);
  bak_filepath= g_strdup_printf("%s.old", ini_filepath);

  new_file= myx_fopen(new_ini_filepath, "w");
  if (new_file == NULL)
  {
    fprintf(stderr,
            "Error in function myx_update_mysql_cnf_file: "
            "Cannot open %s for writing.\n%s\n",
            new_ini_filepath,strerror(errno));
    g_free(new_ini_filepath);
	  g_free(bak_filepath);

    return MYX_ADMIN_ERROR_CANT_OPEN_FILE;
  }

  // check if the given group exists. if not, write to the default group mysqld
  // instead. This prevents the creation of new groups, but otherwise causes
  // problems with typical Windows setups where one my.cnf file per instance is
  // used and only the mysqld section in each of the files
  // Exclude also the [mysql] group/section.
  if (group_exists_in_cnf_file(ini_filepath, group) && (strcasecmp(group, "mysql") != 0))
    actual_group= group;
  else
    actual_group= "mysqld";

  error= myx_update_mysql_cnf_filef(desc, ini_filepath, new_file, actual_group);

  if (ferror(new_file))
  {
    fprintf(stderr, "Error in function myx_update_mysql_cnf_file: can't write to %s.\n%s\n",
      new_ini_filepath,strerror(ferror(new_file)));
  };

  fclose(new_file);

  if (error == MYX_ADMIN_NO_ERROR)
  {
    // backup and commit 

    if (file_exists(bak_filepath))
      remove(bak_filepath);

    if (file_exists(ini_filepath) && rename(ini_filepath, bak_filepath))
    {
      fprintf(stderr,
          "Error in function myx_update_mysql_cnf_file: "
          "can't rename file \"%s\" to \"%s\".\n%s\n",
          ini_filepath,bak_filepath,strerror(errno));
      error= MYX_ADMIN_ERROR_CANT_OPEN_FILE;
      goto error;
    }

    if (rename(new_ini_filepath, ini_filepath))
    {
      fprintf(stderr,
          "Error in function myx_update_mysql_cnf_file: "
          "can't rename file \"%s\" to \"%s\".\n%s\n",
          new_ini_filepath, ini_filepath, strerror(errno));
      error= MYX_ADMIN_ERROR_CANT_OPEN_FILE;
      goto error;
    }

    if (file_exists(bak_filepath) && remove(bak_filepath))
    {
      fprintf(stderr,
          "Error in function myx_update_mysql_cnf_file: "
          "can't remove file \"%s\".\n%s\n",
          bak_filepath,strerror(errno));
      error= MYX_ADMIN_ERROR_CANT_OPEN_FILE;
      goto error;
    }
  }
  
error:
  g_free(new_ini_filepath);
  
  return error;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_ADMIN_LIB_ERROR myx_update_mysql_cnf_filef(MYX_GUI_DESCRIPTION *desc, const char *ini_filepath, FILE *new_file,
  const char *group)
{
  FILE *ini_file;
  char buffer[MAX_LINE_LENGTH];
  GPtrArray *line_array, *processed_widgets;
  unsigned int i;
  const char *groups[]= {NULL, NULL};
  int our_section_has_started= 0;
  MYX_ADMIN_LIB_ERROR error_code= MYX_ADMIN_NO_ERROR;
  int section_needs_to_be_created= 1;

  line_array= g_ptr_array_new();
  processed_widgets= g_ptr_array_new();
  groups[0] = group; 

  ini_file= myx_fopen(ini_filepath, "r");
  if (ini_file != NULL)
  {
    PROCESS_LINE_STATUS pls;
    init_line_status(&pls);

    /* go through the config-file entries and read them into line_array */
    while (fgets(buffer, sizeof(buffer), ini_file) )
    {
      INI_LINE_TYPE result;
      INI_FILE_LINE *fl;
      /* analyze this line */
      result= process_ini_line(&pls, buffer, groups, &error_code);
      if (result == ILT_ERROR)
      {
        free_pls2(&pls);
        g_ptr_array_free(line_array,0);
        g_ptr_array_free(processed_widgets,0);
        fclose(ini_file);
        return error_code;
      }

      /* put it into our list */
      fl= new_ini_file_line();
      fl->line= buffer[strlen(buffer)-1] == '\n'
                  ? g_strdup(buffer)
                  : g_strconcat(buffer, "\n", NULL);
      fl->type= result;

      if (result != ILT_NAME_ONLY && result != ILT_NAME_VALUE_PAIR)
      {
        fl->section_name= g_strdup(pls.current_section_name);
      }
      else
      {
        NORMALIZED_OPTION *normalized_option= normalize_option(pls.name, pls.value, result, desc);
        if (normalized_option != NULL)
        {
          fl->name= g_strdup(normalized_option->name);
          fl->value= g_strdup(normalized_option->value);
          fl->widget= !normalized_option->known_option
                        ? NULL : normalized_option->widget;
          fl->type= normalized_option->type;
          free_normalized_option(normalized_option);
        }
      }
      g_ptr_array_add (line_array, (gpointer) fl);
    }
    if (ferror(ini_file))
    {
      free_pls2(&pls);
      g_ptr_array_free(line_array,0);
      g_ptr_array_free(processed_widgets,0);
      fclose(ini_file);
      return MYX_ADMIN_INI_PARSE_ERROR;
    }
    fclose(ini_file);
  }

  /* we go through the list and decide for each line if it is still valid */
  for (i= 0; i< line_array->len; i++)
  {
    INI_FILE_LINE *fl;

    fl= (INI_FILE_LINE*)g_ptr_array_index(line_array, i);

    if ( fl->type == ILT_SECTION_IDENTIFIER && 
         !strcasecmp(fl->section_name,group) )
    {
      our_section_has_started= 1;
      section_needs_to_be_created= 0;
      fprintf(new_file, "%s",fl->line);
    }
    else if (fl->type == ILT_NAME_VALUE_PAIR || fl->type == ILT_NAME_ONLY)
    {
      MYX_GUI_WIDGET *widget= fl->widget;

      if (!widget)
      {
        /* well if we don't know it we'll comment it out */
        // bad, bad thing! just write the original value
        //fprintf(new_file, "#(Unknown option:)%s", fl->line);
        fprintf(new_file, "%s", fl->line);
        continue;
      }

      if (widget->multiple)
      { /* yes not nice maybe but we are always going to
        * comment them out, and write them together in
        * the end */
        fprintf(new_file, "#%s", fl->line);
        continue;
      }

      g_ptr_array_add(processed_widgets, widget->id);

      if (fl->type == ILT_NAME_ONLY)
      {
        if (widget->widget_type == MYX_TEXTEDIT)
        {
          if (widget->value && widget->active)
          {
            safe_fprintf_TEXTEDIT(new_file,widget);
          }
          else if (widget->active)
          {
            /* yes comments get lost by this, but it could 
              be that the user is using a deprecated alias */
            fprintf(new_file,"%s\n", widget->id);
          }
          else
          {
            fprintf(new_file,"#%s", fl->line);
          }
        }
        else assert(0);
      }
      else
      {
        if (!widget->active)
        {
          fprintf(new_file, "#%s", fl->line);
          continue;
        }
        if (widget->widget_type == MYX_CHECKBOX)
        {
          assert(widget->value);

          if (!widget->checkbox->is_boolean)
          {
            fprintf(new_file,
                    strcasecmp(widget->value, widget->default_value)
                    ? "%s" : "#%s", fl->line);
          }
          else
          {
            char *w_val= !widget->checkbox->invert
                          ? (char*)widget->value
                          : !strcasecmp(widget->value,NAME_ONLY_OPTION_ENABLED)
                            ? NAME_ONLY_OPTION_DISABLED
                            : NAME_ONLY_OPTION_ENABLED;
            if (!strcasecmp(w_val, widget->default_value) )
            {
              fprintf(new_file, "#%s", fl->line);
            }
            else
            {
              if ( !strcasecmp(w_val, NAME_ONLY_OPTION_ENABLED))
              {
                fprintf(new_file, "%s\n", widget->id);
              }
              else if (!strcasecmp(w_val, NAME_ONLY_OPTION_DISABLED))
              {
                fprintf(new_file, "skip-%s\n", widget->id);
              }
              else assert(0);
            }
          }
        }
        else
        {
          /* it is no checkbox */
          if ( !widget->value || strcasecmp(widget->value, NAME_ONLY_OPTION_ENABLED) == 0)
          {
            fprintf(new_file,"%s\n", widget->id);
          }
          else if (strcasecmp(widget->value, fl->value))
          {
            /*print out the new value; comments in the same 
                                                   line will get lost by this*/
            if (widget->widget_type == MYX_TEXTEDIT)
            { 
              safe_fprintf_TEXTEDIT(new_file,widget); 
            }
            else
            {
              fprintf(new_file,"%s=%s\n", widget->id, widget->value);
            }
          }
          else
            fprintf(new_file, "%s",fl->line);
        }
      }
    }
    else if (our_section_has_started &&
             fl->type == ILT_SECTION_IDENTIFIER) /* the next section begins */
    {
      our_section_has_started = 0;

      /*before we leave our section we have to write all missning values*/
      write_gui_desc_values_to_ini_file(desc, processed_widgets, new_file);

      /*let the new section finally start*/
      fprintf(new_file, "\n\n%s", fl->line);
    }
    else
      fprintf(new_file, "%s",fl->line);

    free_ini_file_line(fl);
  }

  if (section_needs_to_be_created)
  {
    assert(processed_widgets->len == 0);
    fprintf(new_file, "[%s]\n", group);
    write_gui_desc_values_to_ini_file(desc, processed_widgets, new_file);
  }
  else if (our_section_has_started) /* our section is the last in the file */
  {
    write_gui_desc_values_to_ini_file(desc, processed_widgets, new_file);
  }

  g_ptr_array_free(line_array,0);
  g_ptr_array_free(processed_widgets,0);

  if (ferror(new_file))
    return MYX_ADMIN_ERROR_CANT_OPEN_FILE;

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Private functions
 */

static void free_normalized_option(NORMALIZED_OPTION *option)
{
  if (option)
  {
    g_free(option->name);
    g_free(option->value);
    g_free(option);
  }
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Since an option may have several different appearances we
 * must convert them into a standard form .
 *
 *
 * Return Value: A pointer to a NORMALIZED_OPTION-struct on succes.
 *  NULL in case of an error (eg wrong argument to a boolean option)
 */
static NORMALIZED_OPTION* normalize_option(const char *name, 
                                           const char *value, 
                                           INI_LINE_TYPE line_type, 
                                           MYX_GUI_DESCRIPTION *desc)
{
  MYX_GUI_WIDGET *widget;
  int boolean_option_value;
  NORMALIZED_OPTION *noption;

  g_return_val_if_fail(name, NULL);
  g_return_val_if_fail(line_type == ILT_NAME_ONLY ||
                       line_type == ILT_NAME_VALUE_PAIR, NULL);
  g_return_val_if_fail(desc, NULL);

  noption= g_malloc0(sizeof(NORMALIZED_OPTION ));

  noption->name= has_loose_prefix(name);
  if (noption->name != NULL)
  {
    noption->loose_option= 1;
  }
  else
  {
    noption->name= g_strdup(name);
    noption->loose_option= 0;
  }

  widget= find_widget((xmlChar*)noption->name, desc);
  if (widget !=  NULL)
  {
    if (widget->widget_type == MYX_CHECKBOX)
    {
      boolean_option_value= get_boolean_option_value(line_type,
                                                     noption->name,
                                                     value);
      if (boolean_option_value == -1)
        return NULL;

      if (widget->checkbox->invert)
        boolean_option_value= !boolean_option_value;

      noption->value= g_strdup(boolean_option_value
                               ? NAME_ONLY_OPTION_ENABLED
                               : NAME_ONLY_OPTION_DISABLED);
    }
    else if (widget->widget_type == MYX_TEXTEDIT)
    {
      if (value)
        noption->value= g_strdup(value);
    }
    else
    {
      assert(value && line_type == ILT_NAME_VALUE_PAIR);
      assert(!widget->multiple);
      noption->value= g_strdup(value);
    }
  }
  else
  {
    const char *tmpname= noption->name;

    /* widget was not found --> see if there is a
    * known prefix that caused that
    */
    if (!strncmp(tmpname, "skip-", sizeof("skip-")-1))
    {
      tmpname+= sizeof("skip-")-1;
      boolean_option_value= 0;
    }
    else if (!strncmp(tmpname, "disable-", sizeof("disable-")-1))
    {
      tmpname+= sizeof("disable-")-1;
      boolean_option_value= 0;
    }
    else if (!strncmp(tmpname, "enable-", sizeof("enable-")-1))
    {
      tmpname+= sizeof("enable-")-1;
      boolean_option_value= 1;
    }
    else
    {
      fprintf(stderr,
              "Warning: The option %s is not known. Please file a bug report "
              "if this is a valid option.\n", tmpname);
      return NULL;
    }

    widget= find_widget((xmlChar*)tmpname, desc);
    if (widget != NULL)
    {
      assert(widget->widget_type == MYX_CHECKBOX);
      assert(widget->checkbox->is_boolean);

      if (widget->checkbox->invert)
        boolean_option_value= !boolean_option_value;

      noption->value= g_strdup(boolean_option_value
                               ? NAME_ONLY_OPTION_ENABLED
                               : NAME_ONLY_OPTION_DISABLED);
    }
    else
    {
      fprintf(stderr, "Warning: The option %s is not known. "
              "Please file a bug report if this is a valid option.\n",
              name);
      return NULL;
    }
    
  }

  noption->known_option= 1;
  noption->widget= widget;
  noption->type= (noption->value)? ILT_NAME_VALUE_PAIR : ILT_NAME_ONLY;

  return noption;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Like myx_str_append but works with memory
 * allocated by the xml-library.
 *
 * @base_str becomes invalid after calling this
 * function!
 *
 * Return value: The joined string
 */
static char *xstr_append(char *base_str, const char *addon)
{
  unsigned int addon_len= (unsigned int)strlen(addon);
  char *tmp;

  tmp= xmlRealloc(base_str, (unsigned int)strlen(base_str)+addon_len+1);

  base_str= strncat(tmp, addon,addon_len);

  return base_str;
}

//----------------------------------------------------------------------------------------------------------------------

static char* has_loose_prefix(const char* option_name)
{
  g_return_val_if_fail(option_name, NULL);

  if (strncmp(option_name, "loose-",6)) return NULL;

  return ((char *)option_name + 6);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Return Value: 1 if the option is enabled, 0 if it is disabled.
 * -1 in case of an error.
 */
static int get_boolean_option_value(int result,
                                    const char * option_name,
                                    const char * value)
{
  if (result == ILT_NAME_ONLY)
  {
    return 1;
  }
  else if (result == ILT_NAME_VALUE_PAIR)
  {
    int i;
    char *eptr;

    assert(value);

    i= strtol(value, &eptr, 10);
    if (eptr == value || *eptr || (i != 0 && i != 1))
    {
      fprintf(stderr,
              "Error when reading the option %s. It has a "
              "wrong value assigned(%s)."
              "Only 0 or 1 are valid values.\n", option_name, value);
      return -1;
    }

    return i;
  }
  else
    assert(0);

  return -1;  //Keep some compilers happy
}

//----------------------------------------------------------------------------------------------------------------------

/**
 *
 * Writes all options to the ini_file that have
 * not already been written
 *
 */
static void write_gui_desc_values_to_ini_file(MYX_GUI_DESCRIPTION *desc, GPtrArray *processed_widgets, FILE *ini_file)
{
  unsigned int i,j,k;
  MYX_GUI_WIDGET *widget;

  for (i= 0; i < desc->pages_num; i++)
  {
    for (j= 0; j < desc->pages[i].groups_num; j++)
    {
      for (k= 0; k < desc->pages[i].groups[j].widgets_num; k++)
      {
        widget= desc->pages[i].groups[j].widgets+k;

        /* if we have already written out this widget */
        if (written_to_file(widget->id, processed_widgets) ||
            !widget->active)
        {
          continue;
        }

        if (widget->multiple)
        {
          /* write multiple entries.. */
          char *p;
          for (p= strtok (widget->value, ","); p; p= strtok(NULL, ","))
            fprintf(ini_file, "%s=%s\n", widget->id,p);
          continue;
        }
       
        if (widget->widget_type == MYX_CHECKBOX)
        {
          assert(widget->value);
          if (widget->checkbox->is_boolean)
          {
            char *w_val= !widget->checkbox->invert
                         ? (char*)widget->value
                         : !strcasecmp(widget->value, NAME_ONLY_OPTION_ENABLED)
                           ? NAME_ONLY_OPTION_DISABLED
                           : NAME_ONLY_OPTION_ENABLED;

            if (strcasecmp(w_val, widget->default_value) )
            {
              if ( !strcasecmp(w_val, NAME_ONLY_OPTION_ENABLED))
              {
                write_description(ini_file, widget);
                fprintf(ini_file, "%s\n", widget->id);
              }
              else if (!strcasecmp(w_val, NAME_ONLY_OPTION_DISABLED))
              {
                write_description(ini_file, widget);
                fprintf(ini_file, "skip-%s\n", widget->id);
              }
              else assert(0);
            }
          }
          else if (strcasecmp(widget->value, widget->default_value) )
          {
            write_description(ini_file, widget);
            fprintf(ini_file, "%s\n", widget->id);
          }
        }
        else
        {
          write_description(ini_file, widget);

          if (widget->value && (*widget->value))
          {            
            if (widget->widget_type == MYX_TEXTEDIT)
            {
              safe_fprintf_TEXTEDIT(ini_file,widget);
            }
            else
            {
              fprintf(ini_file, "%s=%s\n", widget->id, widget->value);
            }
          }
          else
            fprintf(ini_file, "%s\n", widget->id);
        }
      }
    }
  }
}

//----------------------------------------------------------------------------------------------------------------------

static int written_to_file(const char *widget_id, const GPtrArray *processed_widgets)
{
  unsigned int i;

  for (i=0; i < processed_widgets->len; i++)
  {
    if (!strcmp(widget_id, (char *) g_ptr_array_index(processed_widgets, i)) )
      return 1;
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

static INI_FILE_LINE* new_ini_file_line()
{
  return calloc(1, sizeof(INI_FILE_LINE));
}

//----------------------------------------------------------------------------------------------------------------------

static void free_ini_file_line(INI_FILE_LINE *fl)
{
  if (fl)
  {
    g_free(fl->line);
    g_free(fl->name);
    g_free(fl->value);

    free(fl);
  }
}

//----------------------------------------------------------------------------------------------------------------------

static void write_description(FILE *file, const MYX_GUI_WIDGET *widget)
{
  // Maybe we should split long descriptions to several comment-lines.
  fprintf(file, "#%s\n", widget->description);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * a mysql-identifier is a mysql option
 */
static MYX_GUI_WIDGET *find_widget(xmlChar *mysql_identifier, MYX_GUI_DESCRIPTION *gui_desc)
{
  unsigned int i,j,k,l;

  for (i=0; i < gui_desc->pages_num; i++)
  {
    for (j=0; j < gui_desc->pages[i].groups_num; j++)
    {
      for (k=0; k < gui_desc->pages[i].groups[j].widgets_num; k++)
      {
        MYX_GUI_WIDGET * widget= gui_desc->pages[i].groups[j].widgets+k;

        /* option is an alias for option_size */
        if (! xmlStrncmp((xmlChar*)widget->id,mysql_identifier,(int)strlen((char*)mysql_identifier))
            && !xmlStrcmp((xmlChar*)widget->id+strlen((char*)mysql_identifier),(xmlChar*)"-size"))
          return widget;
        if (! xmlStrcmp((xmlChar*)widget->id,mysql_identifier))
          return widget; /* we have found it */
        for (l=0; l< widget->alt_names_num; l++)
        {
          if (! xmlStrcmp((xmlChar*)widget->alt_names[l], mysql_identifier) )
            return widget;
        }
      }
    }
  }

  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

static void init_line_status(PROCESS_LINE_STATUS * pls)
{
  pls->found_group= 0;
  pls->read_values= 0;
  pls->current_section_name= 0;
  pls->name= 0;
  pls->value= 0;
}

//----------------------------------------------------------------------------------------------------------------------

static void free_pls2( PROCESS_LINE_STATUS *pls)
{
  if (pls->current_section_name)
    free(pls->current_section_name);
  if (pls->name)
    free(pls->name);
  if (pls->value)
    free(pls->value);
}

//----------------------------------------------------------------------------------------------------------------------

static int find_group(const char* group_name, const char **group_list)
{
  const char **p;
  if (! group_list) return 0;
  for (p= group_list; *p; p++)
  {
    if (! strcmp(*p, group_name) )
      return 1; /*Group found*/
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/* taken from mysys/default.c */
static char *remove_end_comment(char *ptr)
{
  char quote= 0;        /* we are inside quote marks */
  char escape= 0;       /* symbol is protected by escape chagacter */

  for (; *ptr; ptr++)
  {
    if ((*ptr == '\'' || *ptr == '\"') && !escape)
    {
      if (!quote)
        quote= *ptr;
      else if (quote == *ptr)
        quote= 0;
    }
    /* We are not inside a string */
    if (!quote && *ptr == '#')
    {
      *ptr= 0;
      return ptr;
    }
    escape= (quote && *ptr == '\\' && !escape);
  }
  return ptr;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Processes one line of an ini-file.
 * Processing means determining the type of a line.
 * The type of a line can be   ILT_ERROR, ILT_NOCONTENT,
 * ILT_SECTION_IDENTIFIER, ILT_NAME_ONLY, ILT_NAME_VALUE_PAIR,
 * ILT_NAME_VALUE_PAIR_NOT_READ
 * SYNOPSIS
 *   line : the line that should be processed
 * DESCRIPTION
 *
 * RETURN VALUE
 *
 * NOTES
 */
static INI_LINE_TYPE process_ini_line(PROCESS_LINE_STATUS *pls,
                                      const char *line,
                                      const char **groups,
                                      MYX_ADMIN_LIB_ERROR *error_code)
{
  char *ptr, *end;

  /* clear old information */
  if (pls->name)
    free(pls->name);
  if (pls->value)
    free(pls->value);
  pls->name= pls->value= NULL;

  for (ptr= (char*)line; isspace(*ptr) ; ptr++ ) ;

  /* Ignore comment and empty lines */
  if (*ptr == '#' || *ptr == ';' || !*ptr)
    return ILT_NOCONTENT;

  /*check if it's a section identifier*/
  if (*ptr == '[')                            /* Group name */
  {
    pls->found_group=1;
    end= (char *) strchr(++ptr,']');
    if (end == NULL)
    {
      fprintf(stderr, "error: Wrong group definition in config file: at line %s\n", line);
      *error_code = MYX_ADMIN_INI_PARSE_ERROR;
      return ILT_ERROR;
    }
    for ( ; isspace(end[-1]) ; end--) ;/* Remove end space */

    if (pls->current_section_name)
      free(pls->current_section_name);

    pls->current_section_name= malloc(end-ptr+1);
    strncpy(pls->current_section_name,ptr,end-ptr);
    pls->current_section_name[end-ptr]= '\0';

    pls->read_values= find_group(pls->current_section_name, groups);

    return ILT_SECTION_IDENTIFIER;
  }
  if (! pls->found_group)
  {
    fprintf(stderr,
            "error: Found option without preceding group in config file: "
            "at line: %s\n", line);
    *error_code = MYX_ADMIN_INI_PARSE_ERROR;
    return 1;
  }

  /* it is a name-value pair */
  if (! pls->read_values)
    return ILT_NAME_VALUE_PAIR_NOT_READ;

  return read_in_name_value_pair(pls, ptr);
}

//----------------------------------------------------------------------------------------------------------------------

static INI_LINE_TYPE read_in_name_value_pair(PROCESS_LINE_STATUS *pls, char* ptr)
{
  char *equal_sign, *end;
  size_t name_len, value_len;;

  for (; isspace(*ptr) ; ptr++ ) ;

  equal_sign= strchr(ptr, '=');
  if (equal_sign != NULL)
  {
    char *vp, *value_end, *value_start, *rp;

    for (end= equal_sign - 1; isspace(*end); end--)
      ;

    name_len= end-ptr+1;
    pls->name= malloc(name_len+1);
    strncpy(pls->name, ptr, name_len);
    pls->name[name_len]= '\0';

    if (!strcmp(pls->name, "set-variable"))
    {
      free(pls->name);
      return read_in_name_value_pair(pls, equal_sign+1);
    }

    /*now get the value*/
    value_end= remove_end_comment(equal_sign+1);
    for (value_end= value_end-1; isspace(*value_end); value_end--) ;
    for (value_start= equal_sign+1; isspace(*value_start); value_start++) ;

    /* remove quotes around argument */
    if ((*value_start == '\"' || *value_start == '\'') && 
         *value_start == *value_end)
    {
      value_start++;
      value_end--;
    }

    if (value_end < value_start)
    {
      /* value conists only of whitespace */
      pls->value= strdup(NAME_ONLY_OPTION_ENABLED);
      return ILT_NAME_ONLY;
    }
    value_len= value_end-value_start+1;

    rp= pls->value= malloc(value_len+1);
    for (vp= value_start; vp != value_end+1; vp++, rp++)
    {
      if (*vp != '\\' || vp == value_end)
      {
        *rp= *vp;
      }
      else
      {
        switch (*++vp)
        {
        case 'n':  *rp= '\n'; break;
        case 't':  *rp= '\t'; break;
        case 'r':  *rp= '\r'; break;
        case 'b':  *rp= '\b'; break;
        case 's':  *rp= ' ';  break; /* space */
        case '\\': *rp= '\\'; break;
        default:                     /* Unknown; Keep '\' */
          *rp++= '\\';
          *rp= *vp;
          break;
        }
      }
    }
    *rp= '\0';

    return ILT_NAME_VALUE_PAIR;
  }
  else
  {
    remove_end_comment(ptr);
    for (end= ptr+strlen(ptr)-1; isspace(*end); end--) ;

    name_len= end-ptr+1;
    pls->name= malloc(name_len + 1);
    strncpy(pls->name, ptr, name_len);
    pls->name[name_len]= '\0';

    pls->value= NULL;
    return ILT_NAME_ONLY;
  }
}

//----------------------------------------------------------------------------------------------------------------------


