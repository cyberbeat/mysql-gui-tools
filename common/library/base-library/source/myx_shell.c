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

#include "myx_public_interface.h"
#include "myx_library.h"

/*
 * defines
 */

//Remember to add 2 to the substring number since there are two () in this string
#define REGEX_IGNORE_LEADING_COMMENT "(\\s*(/\\*.*\\*/))*\\s*"

#define SHOUT(text) (*shell->output_callback)(text, shell->output_user_data)
#define SHELLOUT_BREAK(text, do_break) do_break= ((*shell->output_callback)(text, shell->output_user_data) != 0) ? TRUE : do_break
#define MAX_COLUMN_LENGTH 80

/*
 * implementation
 */

MYX_TEXT_SHELL *myx_init_text_shell(MYSQL *mysql)
{
  MYX_TEXT_SHELL *shell= g_new0(MYX_TEXT_SHELL, 1);
  shell->mysql= mysql;
  
  return shell;
}

void myx_finalize_text_shell(MYX_TEXT_SHELL *shell)
{
  g_free(shell);
}


void myx_ts_set_output_callback(MYX_TEXT_SHELL *shell, void *user_data,
    int (*process_output_func)(const char *text, void *user_data))
{
  shell->output_callback= process_output_func;
  shell->output_user_data= user_data;
}


static void fill_char(char *buffer, int ch, int count)
{
  int i;
  for (i= 0; i < count; i++)
    buffer[i]=ch;
  buffer[count]= 0;
}


static void show_result(MYX_TEXT_SHELL *shell, MYSQL_RES *res)
{
  unsigned int off;
  MYSQL_ROW cur;
  MYSQL_FIELD *field;
  unsigned int line_width= 4;
  char *line_sep;
  char *line;
  int do_break= FALSE;
  char *tmp_utf8= g_malloc(MAX_COLUMN_LENGTH*2+1);

  // calc line width
  do
  {
#if MYSQL_VERSION_ID < 40100
    unsigned int length= (field->name != 0 ? strnlen(field->name, field->max_length) : 0);
#else
    unsigned int length= field->name_length;
#endif

    field= mysql_fetch_field(res);
    if (field == NULL)
      break;

    length= max(length,field->max_length);
    length*= 2; //consider UTF8 2-byte chars
    if (length < 4 && !IS_NOT_NULL(field->flags))
      length=4;                                 // Room for "NULL"
    field->max_length=length+1;

    line_width+= length+3;
  }
  while (1);

  // build line separator
  line_sep= g_malloc(sizeof(char)*(line_width+1));
  strcpy(line_sep, "+");
  mysql_field_seek(res, 0);
  do
  {
    field= mysql_fetch_field(res);
    if (field == NULL)
      break;

    fill_char(line_sep+strlen(line_sep), '-', min((int)field->max_length+1, MAX_COLUMN_LENGTH+1));
    strcat(line_sep,"+");
  }
  while (1);

  strcat(line_sep,"\n");

  SHELLOUT_BREAK(line_sep, do_break);
  
  line= g_malloc(sizeof(char)*(line_width+1));

  // output column names
  mysql_field_seek(res,0);
  SHELLOUT_BREAK("|", do_break);

  do
  {
    char *field_name;

    field= mysql_fetch_field(res);
    if (field == NULL)
      break;

    field_name= myx_convert_dbstr_utf8(shell->mysql, field->name, -1);

    sprintf(line, " %-*s|",min((int)field->max_length,MAX_COLUMN_LENGTH),
            field_name);

    g_free(field_name);

    SHELLOUT_BREAK(line, do_break);
  }
  while (1);

  SHELLOUT_BREAK("\n", do_break);
  SHELLOUT_BREAK(line_sep, do_break);
  
  // output rows
  while (!do_break)
  {
    char *line_end= line;
    strcpy(line, "|");
    line_end++;

    cur= mysql_fetch_row(res);
    if (cur == NULL)
      break;

    mysql_field_seek(res, 0);
    for (off= 0; off < mysql_num_fields(res); off++)
    {
      unsigned int length, u8length, clength;
      char *field_value;
      
      if(cur[off])
        field_value= myx_convert_dbstr_utf8(shell->mysql, cur[off], -1);
      else
        field_value= g_strdup("NULL");

      field_value= str_g_replace(field_value, "\r\n", "\xc2\xab\xc2\xb6");
      field_value= str_g_replace(field_value, "\n", "\xc2\xb6");

      field= mysql_fetch_field(res);
      length= field->max_length;
      
      // compensage difference between bytecount and utf8 char count
      clength= (unsigned int)strlen(field_value);
      u8length= g_utf8_strlen(field_value, clength);
      length+= clength-u8length;

      if (u8length > MAX_COLUMN_LENGTH)
      {
        //Clear buffer
        memset(tmp_utf8, 0, MAX_COLUMN_LENGTH*2+1);

        tmp_utf8= g_utf8_strncpy(tmp_utf8, field_value, MAX_COLUMN_LENGTH-1);

        strcpy(line_end, " ");
        line_end++;
        g_utf8_strncpy(line_end, field_value, MAX_COLUMN_LENGTH-1);
        line_end+= strlen(tmp_utf8);
        strcpy(line_end, ">|");
        line_end+= 2;
      }
      else
      {
        line_end+= sprintf(line_end, IS_NUM(field->type) ? "%*s |" : " %-*s|",
                min(length, MAX_COLUMN_LENGTH+clength-u8length), field_value);
        //line_end+= strlen(line_end);
      }

      g_free(field_value);
    }
    strcpy(line_end, "\n");
    SHELLOUT_BREAK(line, do_break);
  }

  if(!do_break)
    SHOUT(line_sep);
  else
  {
    SHOUT("Ctrl+C pressed, cleaning up buffer ...\n");

    // Finish fetching rows
    do
    {
      cur= mysql_fetch_row(res);
    }
    while (cur != NULL);
      

    SHOUT("Buffer cleaned up.\n");
  }
  g_free(line_sep);
  g_free(tmp_utf8);
}




static int execute_check_internal_command(MYX_TEXT_SHELL *shell, const char *command)
{
  
  
  return -2;
}



int myx_ts_execute_command(MYX_TEXT_SHELL *shell, const char *command)
{
  int rc;
  char buffer[256];
  MYX_TIMER_VALUE timer;
  double qtime, ftime;

  if ((rc= execute_check_internal_command(shell, command)) != -2)
  {
    return rc;
  }

  timer_start(&timer);
  rc= myx_mysql_real_query(shell->mysql, command, (unsigned int)strlen(command));
  qtime= timer_stop(&timer);
  if (rc != 0)
  {
    char *tmp;

    sprintf(buffer, "ERROR %i: ", myx_mysql_errno(shell->mysql));
    SHOUT(buffer);
    SHOUT(tmp=myx_mysql_error(shell->mysql));
    g_free(tmp);
    SHOUT("\n");

    return rc;
  }
  else
  {
    do {
      MYSQL_RES *res;

      timer_start(&timer);
      res= mysql_store_result(shell->mysql);
      ftime= timer_stop(&timer);

      if (res)
      {
        if (!mysql_num_rows(res))
          SHOUT("Empty set.");
        else
          show_result(shell, res);
      
        mysql_free_result(res);
      }
      else if (mysql_affected_rows(shell->mysql) == ~(ulonglong)0)
        SHOUT("Query OK.\n");
      else
      {
        sprintf(buffer, "Query OK, %ld %s affected.\n",
                (long)mysql_affected_rows(shell->mysql),
                (long)mysql_affected_rows(shell->mysql) == 1 ? "row" : "rows");
        SHOUT(buffer);
      }
      
#if MYSQL_VERSION_ID >= 40100
      if (mysql_warning_count(shell->mysql)>0)
      {
        sprintf(buffer, "%i warning(s)\n", mysql_warning_count(shell->mysql));
        SHOUT(buffer);
      }
#endif
      SHOUT("\n");
      
      sprintf(buffer, "Query executed in %.4fs, retrieved in %.4fs.\n",
              qtime, ftime);
      SHOUT(buffer);
    }
#if MYSQL_VERSION_ID >= 40100
    while (mysql_next_result(shell->mysql) == 0);
#else
    while (0);
#endif
  }
  return rc;
}

void myx_ts_display_help(MYX_TEXT_SHELL *shell, const char *command)
{
  SHOUT(
    "List of all GUI Shell commands:\n"
    "Note that all text commands must be first on line and end with ';'\n"
    "help    (\\h)    Display this help.\n"
    "?       (\\?)    Synonym for `help'.\n"
    "\n");
}

//Extracts the database from a USE statment
//returns NULL when it is not a USE statment
char * myx_parse_sqlmemo_command_use(const char *command)
{
  //regex:  (\s*(/\*.*\*/))*\s*use\s+(`.*`|[^\s^;]*).*
  //test:   /* use */ use `te st`; /*use*/
  char *schema_name = get_value_from_text_ex_opt(command, (int)strlen(command), 
      REGEX_IGNORE_LEADING_COMMENT "use\\s+(`.*`|[^\\s^;]*).*", 3,
      PCRE_ANCHORED); // Match only at the first position !

  //remove enclosing `` if present
  if ((schema_name) && (schema_name[0]=='`'))
  {
    char *tmp= g_strdup(&schema_name[1]);
    tmp[strlen(tmp)-1]= 0;

    g_free(schema_name);
    schema_name= tmp;
  }

  return schema_name;
}

//Extracts the filename from a LOAD statment
//returns NULL when it is not a LOAD statment
char * myx_parse_sqlmemo_command_load(const char *command)
{
  //regex:  (\s*(/\*.*\*/))*\s*load\s+(".*"|[\w\.\\:]*).*
  //test:   /* load */ load "c:\program files\test.sql"; /* load */
  //        /* load */ load c:\programme\test.sql; /* load */
  return get_value_from_text_ex_opt(command, (int)strlen(command), 
    REGEX_IGNORE_LEADING_COMMENT "load\\s+(\".*\"|[\\w\\.\\\\:]*).*", 3,
    PCRE_ANCHORED); // Match only at the first position !
}

//Extracts the delimiter from a DELIMITER statment
//returns NULL when it is not a DELIMITER statment
char * myx_parse_sqlmemo_command_delimiter(const char *command)
{
  //regex:  (\s*(/\*.*\*/))*\s*delimiter\s+([^\s]*)
  //test:   /* delimiter */ delimiter ;
  //        /* delimiter */ delimiter //
  return get_value_from_text_ex_opt(command, (int)strlen(command),
    REGEX_IGNORE_LEADING_COMMENT "delimiter\\s+([^\\s]*)", 3,
    PCRE_ANCHORED);
}

//returns 1 when the given command is a EXIT statment
MYX_PUBLIC_FUNC int myx_parse_sqlmemo_command_exit(const char *command)
{
  //regex:  (exit|quit).*

  char *tmp;

  tmp= get_value_from_text_ex_opt(command, (int)strlen(command), 
    "(exit|quit).*", 1, PCRE_ANCHORED);

  if(tmp)
  {
    g_free(tmp);
    return 1;
  }
  else
    return 0;
}

//Extracts the command from a HELP statment
//returns NULL when it is not a HELP statment
MYX_PUBLIC_FUNC char * myx_parse_sqlmemo_command_help(const char *command)
{
  //regex:  (\s*(/\*.*\*/))*\s*(help|\?|\\|\\\?)\s*(`.*`|[^\s^;]*|;).*
  //test:   /* help */ help;
  //        /* help */ help select;
  return get_value_from_text_ex_opt(command, (int)strlen(command), 
    REGEX_IGNORE_LEADING_COMMENT 
    "(help|\\?|\\\\h|\\\\\\?)\\s*(`.*`|[^\\s^;]*|;).*", 4,
    PCRE_ANCHORED);
}


int myx_parse_sqlmemo_command_transaction_start(const char *command)
{
  char *tmp;

  tmp= get_value_from_text_ex(command, (int)strlen(command),
                              "^\\s*(start\\s+transaction).*", 1);
  if(tmp)
  {
    g_free(tmp);
    return 1;
  }
  else
    return 0;
}


int myx_parse_sqlmemo_command_transaction_commit(const char *command)
{
  char *tmp;

  tmp= get_value_from_text_ex(command, (int)strlen(command),
                              "^\\s*(commit).*", 1);
  if(tmp)
  {
    g_free(tmp);
    return 1;
  }
  else
    return 0;
}


int myx_parse_sqlmemo_command_transaction_rollback(const char *command)
{
  char *tmp;

  tmp= get_value_from_text_ex(command, (int)strlen(command),
                              "^\\s*(rollback).*", 1);
  if(tmp)
  {
    g_free(tmp);
    return 1;
  }
  else
    return 0;
}
