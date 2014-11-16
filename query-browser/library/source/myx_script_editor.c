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


#define INITIAL_STATEMENT_NUM 1024
#define STATEMENT_INCREMENT 1024
#define INITIAL_PACKET_LENGTH 1024*1024 /* initial size of 1M */

#include <glib.h>
#include <myx_qb_library.h>
#include <myx_simple_sql_parsing.h>

MYX_SQL_TEXT* myx_init_mysql_text()
{
  MYX_SQL_TEXT *s;

  s= g_malloc0(sizeof(MYX_SQL_TEXT));

  s->stmts_size= INITIAL_STATEMENT_NUM;
  s->stmts= g_malloc0(sizeof(MYX_SQL_STATEMENT) * s->stmts_size);

  s->private1= g_malloc0(INITIAL_PACKET_LENGTH);
  s->private2= INITIAL_PACKET_LENGTH;
  return s;
}

void myx_analyze_text(MYX_SQL_TEXT* s, const char *text)
{
  MYX_SQL_PARSE_ENVIRONMENT we;
  MYX_LIB_ERROR error_code = MYX_NO_ERROR;
  MYX_STRING_WRAPPER wrapper;

  myx_init_sql_parse_environment_fill_zero(&we);
  s->stmts_num= 0;
  myx_init_string_wrapper(text,&wrapper);

  while ( myx_get_next_sql_statement(&we, &(s->private1),&(s->private2),
                                     1, &error_code,
                                     myx_read_char_from_string,
                                     NULL,
                                     0,
                                     0,
                                     (void *)&wrapper) )
  {
    s->stmts_num++;

    if (s->stmts_num > s->stmts_size)
    {
      s->stmts_size += STATEMENT_INCREMENT;
      s->stmts= g_realloc(s->stmts, sizeof(MYX_SQL_STATEMENT) * s->stmts_size);
    }
    {
      MYX_SQL_STATEMENT * last_statement= s->stmts + s->stmts_num - 1;
      last_statement->stmt_begin_char= we.stmt_begin_char;
      last_statement->stmt_end_char=   we.stmt_end_char;
      last_statement->stmt_begin_line= we.stmt_begin_line;
      last_statement->stmt_end_line=   we.stmt_end_line;
    }
  }

  myx_done_sql_parse_environment(&we);
}

void myx_free_sql_text( MYX_SQL_TEXT * text)
{
  if (text)
  {
    g_free(text->stmts);
    g_free(text->private1);
    g_free(text);
  }
}
