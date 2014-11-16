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

#include <errno.h>
#include "mdi_text_import.h"


#define REGEX_MATCH_BUFFER_SIZE 10*1024


static inline int next_char(MDI_TXT_IMPORTER *ti)
{
  if (ti->buffer_pos >= ti->buffer_end)
  {
    ti->buffer_end= fread(ti->buffer, 1, ti->buffer_size, ti->file);
    ti->buffer_pos= 0;
    
    if (ti->buffer_end <= 0)
      return EOF;
  }

  return ti->buffer[ti->buffer_pos++];
}


static int fill_buffer(MDI_TXT_IMPORTER *ti, unsigned int count)
{
  if (ti->buffer_pos > 0)
  {
    memmove(ti->buffer, ti->buffer+ti->buffer_pos, ti->buffer_end-ti->buffer_pos);
    ti->buffer_end-= ti->buffer_pos;
    ti->buffer_pos= 0;
  }
  if (count > ti->buffer_end)
  {
    int rc;
    if (count > ti->buffer_size)
    {
      ti->buffer_size= count;

      ti->buffer= g_realloc(ti->buffer, count);
    }
    rc= fread(ti->buffer+ti->buffer_pos, 1, ti->buffer_size-ti->buffer_end, ti->file);
    if (rc <= 0)
      return rc;
    ti->buffer_end+= rc;
    return rc;
  }
  return 0;
}


static int load_until(MDI_TXT_IMPORTER *ti, const char *pattern, int discard)
{
  int c;
  int i= 0;
  int match= 0;

  if (discard)
  {
    while (pattern[match]!=0 && (c= next_char(ti)) != EOF)
    {
      if (pattern[match] == c)
        match++;
      else
        match= 0;
    }
  }
  else
  {
    while (pattern[match]!=0 && (c= next_char(ti)) != EOF)
    {
      if (i >= ti->current_row_size-1)
      {
        ti->current_row_size+= 1024;
        ti->current_row= g_realloc(ti->current_row, ti->current_row_size);
      }

      ti->current_row[i++]= c;
      if (pattern[match] == c)
        match++;
      else
        match= 0;
    }
    if (c != EOF)
      ti->current_row[i-strlen(pattern)]= 0;
    else
      ti->current_row[i]= 0;
  }

  return pattern[match] == 0 ? i : -1;
}


static int load_regex(MDI_TXT_IMPORTER *ti, pcre *re)
{
  int rc= fill_buffer(ti, REGEX_MATCH_BUFFER_SIZE);
  int ovector[9];
  int mc;

  if (rc < 0)
    return -1;
  
  mc= pcre_exec(re, NULL, 
                ti->buffer+ti->buffer_pos, ti->buffer_end-ti->buffer_pos,
                0, PCRE_NO_UTF8_CHECK,
                ovector, 9);
  if (mc < 0)
    return -1;

  if (mc > 0)
  {
    const char *ss;
    unsigned int len;

    if (pcre_get_substring(ti->buffer+ti->buffer_pos, ovector, mc, 1, &ss) < 0)
      return -1;

    len= strlen(ss);
    if (len > ti->current_row_size)
    {
      ti->current_row_size= len+1;
      ti->current_row= g_realloc(ti->current_row, ti->current_row_size);
    }
    memcpy(ti->current_row, ss, len);
    ti->current_row[len]= 0;
    
    pcre_free_substring(ss);
    
    ti->buffer_pos= ovector[3];
    
    return len;
  }
  return -1;
}


static int get_next_line(MDI_TXT_IMPORTER *ti)
{
  int rc= -1;
  
  switch (ti->row_type)
  {
  case MDI_TXI_DELIMITER:
    rc= load_until(ti, ti->linebreak, 0);
    break;
  case MDI_TXI_BEGIN_END:
    if ((rc= load_until(ti, ti->row_param1, 1)) >= 0)
      rc= load_until(ti, ti->row_param2, 0);
    break;
  case MDI_TXI_REGEX:
    rc= load_regex(ti, ti->row_re);
    break;
  }

  return rc;
}



static int break_column_at_delimiter(MDI_TXT_IMPORTER *ti, char *delim)
{
  char *ptr;
  unsigned int rowlen= strlen(ti->current_row);
  
  if (ti->current_col_end >= rowlen-1 || ti->current_col_next < 0)
    return -1;

  ti->current_col_begin= ti->current_col_next;
  
  ptr= strstr(ti->current_row+ti->current_col_begin, delim);
  if (!ptr)
  {
    ti->current_col_end= rowlen;
    ti->current_col_next= -1;
  }
  else
  {
    ti->current_col_end= ptr-ti->current_row;
    ti->current_col_next= ti->current_col_end+strlen(delim);
  }

  return ti->current_col_end-ti->current_col_begin;
}


static int break_column_enclosed_by(MDI_TXT_IMPORTER *ti, char *begin, char *end)
{
  char *ptr;
  unsigned int rowlen= strlen(ti->current_row);
  
  if (ti->current_col_end >= rowlen-1 || ti->current_col_next < 0)
    return -1;

  ptr= strstr(ti->current_row+ti->current_col_next, begin);
  if (!ptr)
  {
    ti->current_col_next= -1;
    return -1;
  }
  else
  {
    ti->current_col_begin= (ptr-ti->current_row)+strlen(begin);
    
    ptr= strstr(ti->current_row+ti->current_col_begin, end);
    if (!ptr)
    {
      ti->current_col_next= -1;
      return -1;
    }
    
    ti->current_col_end= (ptr-ti->current_row);
    ti->current_col_next= ti->current_col_end+strlen(end);
  }

  return ti->current_col_end-ti->current_col_begin;
}


static int break_column_with_regex(MDI_TXT_IMPORTER *ti, pcre *re)
{
  int ovector[9];
  int mc;

  ti->current_col_begin= ti->current_col_next;
  
  mc= pcre_exec(re, NULL, 
                ti->current_row, strlen(ti->current_row),
                ti->current_col_begin, PCRE_NO_UTF8_CHECK,
                ovector, 9);
  if (mc < 0)
    return -1;

  if (mc > 0)
  {
    ti->current_col_begin= ovector[2];
    ti->current_col_end= ovector[3];
    
    ti->current_col_next= ti->current_col_end;

    return ti->current_col_end-ti->current_col_begin;
  }
  return -1;
}


static int break_column(MDI_TXT_IMPORTER *ti)
{
  switch (ti->col_type)
  {
  case MDI_TXI_DELIMITER:
    return break_column_at_delimiter(ti, ti->col_param1);
    
  case MDI_TXI_BEGIN_END:
    break;
    
  case MDI_TXI_REGEX:
    return break_column_with_regex(ti, ti->col_re);
  }
}



MDI_TXT_IMPORTER *mdi_txt_importer_open_file(const char *file,
                                             int start_line,
                                             const char *linebreak)
{
  FILE *f= fopen(file, "r");
  MDI_TXT_IMPORTER *ti;

  if (!f) {
    g_warning("Can't open file '%s': %s", file, strerror(errno));
    return NULL;
  }

  ti= g_new0(MDI_TXT_IMPORTER, 1);

  ti->file= f;
  
  ti->linebreak= g_strdup(linebreak);

  ti->buffer_size= 100*1024;
  ti->buffer= g_malloc(ti->buffer_size);

  ti->current_row_size= 1024;
  ti->current_row= g_malloc(ti->current_row_size);

  ti->columns_size= 8;
  ti->columns= g_new0(char*, ti->columns_size);
  
  // skip the initial lines
  while (start_line > 0)
  {    
    if (load_until(ti, ti->linebreak, 1) < 0)
      break;
    start_line--;
  }

  if (start_line > 0)
  {
    mdi_free_txt_importer(ti);
    ti= NULL;
  }

  return ti;
}


void mdi_free_txt_importer(MDI_TXT_IMPORTER *ti)
{
  
}


void mdi_txt_importer_set_group_delimiter(MDI_TXT_IMPORTER *ti,
                                          MDI_TXI_DELIMITER_TYPE dtype,
                                          const char *param1,
                                          const char *param2)
{
  ti->group_type= dtype;
  ti->group_param1= g_strdup(param1);
  ti->group_param2= g_strdup(param2);
}


int mdi_txt_importer_set_row_delimiter(MDI_TXT_IMPORTER *ti,
                                        MDI_TXI_DELIMITER_TYPE dtype,
                                        const char *param1,
                                        const char *param2)
{
  const char *err;
  int erroffs;
  
  ti->row_type= dtype;
  ti->row_param1= g_strdup(param1);
  ti->row_param2= g_strdup(param2);

  if (dtype == MDI_TXI_REGEX)
  {
    ti->row_re= pcre_compile(param1, PCRE_UTF8|PCRE_MULTILINE|PCRE_CASELESS, &err, &erroffs, NULL);
    if (!ti->row_re)
    {
      g_warning("Error compiling regular expression '%s': %s", param1, err);
      return -1;
    }
  }
  return 0;
}


int mdi_txt_importer_set_column_delimiter(MDI_TXT_IMPORTER *ti,
                                           MDI_TXI_DELIMITER_TYPE dtype,
                                           const char *param1,
                                           const char *param2)
{
  const char *err;
  int erroffs;

  ti->col_type= dtype;
  ti->col_param1= g_strdup(param1);
  ti->col_param2= g_strdup(param2);

  if (dtype == MDI_TXI_REGEX)
  {
    ti->col_re= pcre_compile(param1, PCRE_UTF8|PCRE_MULTILINE|PCRE_CASELESS, &err, &erroffs, NULL);
    if (!ti->col_re)
    {
      g_warning("Error compiling regular expression '%s': %s", param1, err);
      return -1;
    }
  }
  return 0;
}



char **mdi_txt_importer_get_next_row(MDI_TXT_IMPORTER *ti, int *columns_num)
{
  int count;
  
  if (get_next_line(ti) < 0)
    return NULL;

  ti->current_col_next= 0;
  ti->current_col_begin= 0;
  ti->current_col_end= 0;

//  puts(ti->current_row);
  
  count= 0;
  while (break_column(ti) > 0)
  {
    if (count > ti->columns_size)
    {
      ti->columns_size+= 8;
      ti->columns= g_realloc(ti->columns, sizeof(char*)*ti->columns_size);
    }
    
    ti->columns[count++]= g_strndup(ti->current_row+ti->current_col_begin,
                                    ti->current_col_end-ti->current_col_begin);

    ti->current_col_begin= ti->current_col_end;
  }
  
  *columns_num= count;
  
  return ti->columns;
}


void mdi_txt_importer_free_columns(char **columns, int columns_num)
{
  int i;
  
  for (i= 0; i < columns_num; i++)
    g_free(columns[i]);
}
