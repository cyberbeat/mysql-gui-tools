/* Copyright (C) 2003 MySQL AB

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

#include "myx_admin_library.h"
#include "myx_util_functions.h"
#include "myx_library.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <sys/stat.h>
#include <glib.h>
#include <pcre.h>
#include "myx_xml_util_functions.h"

#define LOG_FILE_LINE_LEN 4096

//#define REGEX_MYSQL_START "(\\d+)\\s+(\\d+:\\d{2}:\\d{2})\\s+.*mysqld started.*"
#define REGEX_MYSQL_START "(\\d+)\\s+(\\d+:\\d{2}:\\d{2})\\s+.*ready for connections.*"
#define REGEX_MYSQL_END "(\\d+)\\s+(\\d+:\\d{2}:\\d{2})\\s+.*mysqld ended.*"
#define REGEX_MYSQL_ERROR "(\\d+)\\s+(\\d+:\\d{2}:\\d{2})\\s+.*(?:Cant start server|error|Errcode).*"
#define REGEX_INNODB_START "(\\d+)\\s+(\\d+:\\d{2}:\\d{2})\\s+InnoDB: Started"
#define REGEX_INNODB_SHUTDOWN "(\\d+)\\s+(\\d+:\\d{2}:\\d{2})\\s+InnoDB: Shutdown completed"
#define REGEX_MYSQL_FORCE_CLOSE_THREAD "(\\d+)\\s+(\\d+:\\d{2}:\\d{2})\\s+MySql: Forcing close"
#define REGEX_ABORT "(\\d+)\\s+(\\d+:\\d{2}:\\d{2})\\s+Aborting"

#define REGEX_SELECT "select"

#define REGEX_CONNECT "(\\d+)\\s+(\\d{1,2}:\\d{2}:\\d{2})\\s+\\S+\\s+Connect"
#define REGEX_INIT "(\\d+)\\s+(\\d{1,2}:\\d{2}:\\d{2})\\s+\\S+\\s+Init DB"
#define REGEX_QUIT "(\\d+)\\s+(\\d{1,2}:\\d{2}:\\d{2})\\s+\\S+\\s+Quit"
#define REGEX_QUERY "(\\d+)\\s+(\\d{1,2}:\\d{2}:\\d{2})\\s+\\S+\\s+Query"

#define O_VECTOR_COUNT 9

static void to_tm_date(const char *date, const char *time, MYX_DATETIME *tm_date)
{
  tm_date->tm_year= (date[1]-'0') + 10 * (date[0]-'0');
  tm_date->tm_mon=  (date[3]-'0') + 10 * (date[2]-'0');
  tm_date->tm_mday= (date[5]-'0') + 10 * (date[4]-'0');
  
  if(strlen(time)==8)
  {
    tm_date->tm_hour= (time[1]-'0') + 10 * (time[0]-'0');
    tm_date->tm_min=  (time[4]-'0') + 10 * (time[3]-'0');
    tm_date->tm_sec=  (time[7]-'0') + 10 * (time[6]-'0');
  }
  else
  {
    tm_date->tm_hour= (time[0]-'0');
    tm_date->tm_min=  (time[3]-'0') + 10 * (time[2]-'0');
    tm_date->tm_sec=  (time[6]-'0') + 10 * (time[5]-'0');
  }
}

int myx_free_logfile(MYX_LOGFILE *logfile)
{
  if (logfile)
  {
    unsigned int i;
    
    for (i=0; i < logfile->lines_num; i++)
    {
      g_free(logfile->lines[i]);
    }
    g_free(logfile->lines);
    
    for (i=0; i < logfile->events_num; i++)
    {
      g_free(logfile->events[i].date);
    }
    g_free(logfile->events);

    g_free(logfile);
  }
  
  return 0;
}

#define STEP_SIZE 100
static int find_line_beginning(FILE *fp, bigint *offset, bigint file_size)
{
  bigint uoff;
  char buffer[STEP_SIZE+1];
  char *newline_p= NULL;
  int step_size= STEP_SIZE;
  size_t bytes_read;
  size_t i;
  int err;
  
  buffer[STEP_SIZE]= '\0';
  
  //assert( (offset*(-1)) <= file_size);
  
  uoff= *offset - STEP_SIZE;
  
  while (!newline_p)
  {
    char *p;
    
    if (uoff*(-1) > file_size)
    {
      step_size= (int)(STEP_SIZE - (-1*uoff - file_size));
      uoff= -file_size;
      newline_p= buffer-1; /* if no newline is found, take the file-beginning as newline */
    };

    err= fseek(fp, (long)uoff, SEEK_END);
    if (err != 0)
    {
      return -1;
    }
    bytes_read= (int)fread(buffer, 1, step_size, fp);
   
    for (p= buffer,i= 0; i < bytes_read && *p; p++,i++)
    {
      if (*p == '\n')
        newline_p= p;
      
    }

    if (newline_p)
    {
      bigint ind= newline_p - buffer;
      *offset= uoff+ind+1;
    }
    uoff-= STEP_SIZE;
  }
  
  return 0;
}


static size_t get_fsize(FILE *file)
{
  struct stat st;
  
  if (fstat(fileno(file), &st) < 0)
	return 0;
  else
	return st.st_size;
}



MYX_LOGFILE* myx_parse_general_log(const char *filename, int block_size, int block_num, int *last_block)
{
  FILE *fp;
  MYX_LOGFILE *logfile;
  assert(filename != NULL);

  fp= myx_fopen(filename, "r");
  if (fp == NULL)
    return NULL;
  
  logfile= myx_parse_general_logf(fp, block_size, block_num, last_block);

  fclose(fp);
  
  return logfile;
}


MYX_LOGFILE* myx_parse_general_logf(FILE *fp, int block_size, int block_num, int *last_block)
{
  char line[LOG_FILE_LINE_LEN];
  bigint offset;
  int line_no;
  pcre *re_connect, *re_init, *re_quit, *re_query;
  int rc;
  int o_vector[O_VECTOR_COUNT];
  MYX_LOGFILE *logfile;
  const char *error_str;
  int erroffset;
  bigint bytes_read;
  bigint file_size;
  
  assert(block_size > 0);
  assert(block_num != 0);
  assert(last_block != NULL);
  
  rewind(fp);
  
  { /* seek to correct point (we need to begin after a newline) */
    int desired_seek_point= block_size * block_num;
    file_size= get_fsize(fp);
    offset=  (desired_seek_point > file_size) ? file_size: desired_seek_point;

    offset *= -1;
  
  
    if (find_line_beginning(fp,&offset, file_size))
    {
      fclose (fp);
      return NULL;
    }
    
    *last_block= (offset == -file_size) ? 1 : 0;
  }
  
  if (fseek(fp, (long)offset, SEEK_END) )
  {
    return NULL;
  }

  /* setup the regular expressions */
  re_connect= pcre_compile( REGEX_CONNECT, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);
  re_init= pcre_compile(REGEX_INIT, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);
  re_quit= pcre_compile(REGEX_QUIT, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);
  re_query= pcre_compile(REGEX_QUERY, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);

  
  if (!re_connect || !re_init || !re_quit || !re_query)
  {
     return NULL;
  }
  
  logfile= g_malloc0(sizeof(MYX_LOGFILE));

  logfile->file_size= file_size;
  logfile->block_num= (int)(file_size / block_size);
  
  bytes_read= 0;
  line_no=1; /* we start counting at 1! */
  while ( (bytes_read < block_size) && (fgets(line, sizeof(line), fp)) )
  {
    const char *date, *time;
    MYX_LOGFILE_EVENT_TYPE ev_type= 0;

    /* analyze this line */
    
    if ( (rc= pcre_exec(re_connect, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) ) > 0) 
    {
      assert(rc == 3); //3 matches
      
      pcre_get_substring(line, o_vector, rc, 1, &date);
      pcre_get_substring(line, o_vector, rc, 2, &time);
      ev_type= MYX_EVENT_CONNECT;
    }
    else if ( (rc= pcre_exec(re_init, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) ) > 0) 
    {
      assert(rc == 3); //3 matches
      
      pcre_get_substring(line, o_vector, rc, 1, &date);
      pcre_get_substring(line, o_vector, rc, 2, &time);
      ev_type= MYX_EVENT_INIT;
    }
    else if ( (rc= pcre_exec(re_quit, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) ) > 0) 
    {
      assert(rc == 3); //3 matches
      
      pcre_get_substring(line, o_vector, rc, 1, &date);
      pcre_get_substring(line, o_vector, rc, 2, &time);
      ev_type= MYX_EVENT_QUIT;
    }
    else if ( (rc= pcre_exec(re_query, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) ) > 0) 
    {
      assert(rc == 3); //3 matches
      
      pcre_get_substring(line, o_vector, rc, 1, &date);
      pcre_get_substring(line, o_vector, rc, 2, &time);
      ev_type= MYX_EVENT_QUERY;
    }

    if (ev_type > 0) 
    {
      MYX_LOGFILE_EVENT *event;
      
      logfile->events_num++;
      logfile->events= g_realloc(logfile->events, logfile->events_num * sizeof(MYX_LOGFILE_EVENT));
      event= logfile->events + logfile->events_num-1;
      
      event->date= g_malloc0(sizeof(MYX_DATETIME));
      to_tm_date(date, time, event->date);
      event->line_no= line_no;
      event->event_type= ev_type;
      
      pcre_free((void *)date);
      pcre_free((void *)time);
    }
    
    bytes_read+= strlen(line); 
    line[strlen(line)-1]= '\0';
    
    logfile->lines_num++;
    logfile->lines= g_realloc(logfile->lines, sizeof(char *) * logfile->lines_num);
    logfile->lines[logfile->lines_num-1]= g_strdup(line);
    line_no++;
  }
  
  if (ferror(fp))
  {
	myx_free_logfile(logfile);
    return NULL;
  }
  
  return logfile;  
}



MYX_LOGFILE* myx_parse_slow_log(const char *filename, int block_size, int block_num, int *last_block)
{
  FILE *fp;
  MYX_LOGFILE *logfile;
  
  assert(filename != NULL);

  fp= myx_fopen(filename, "r");
  if (fp == NULL)
    return NULL;
  
  logfile= myx_parse_slow_logf(fp, block_size, block_num, last_block);
  
  fclose(fp);
  
  return logfile;
}


MYX_LOGFILE* myx_parse_slow_logf(FILE *fp, int block_size, int block_num, int *last_block)
{
  char line[LOG_FILE_LINE_LEN];
  bigint offset;
  int line_no;
  pcre *re_select;
  int rc;
  int o_vector[O_VECTOR_COUNT];
  MYX_LOGFILE *logfile;
  const char *error_str;
  int erroffset;
  bigint bytes_read;
  bigint file_size;
  
  assert(block_size > 0);
  assert(block_num != 0);
  assert(last_block != NULL);
  
  rewind(fp);
  
  { /* seek to correct point (we need to begin after a newline) */
    int desired_seek_point= block_size * block_num;
    file_size= get_fsize(fp);
    offset=  (desired_seek_point > file_size) ? file_size: desired_seek_point;

    offset *= -1;
  
    if (find_line_beginning(fp,&offset, file_size))
    {
      return NULL;
    }
    
    *last_block= (offset == -file_size) ? 1 : 0;
  }
  
  if (fseek(fp, (long)offset, SEEK_END) )
  {
    return NULL;
  }

  /* setup the regular expressions */
  re_select= pcre_compile(REGEX_SELECT, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);


  if (!re_select)
  {
    return NULL;
  }
  
  logfile= g_malloc0(sizeof(MYX_LOGFILE));

  logfile->file_size= file_size;
  logfile->block_num= (int)(file_size/block_size);
  
  bytes_read= 0;
  line_no=1; /* we start counting at 1! */
  while ( (bytes_read < block_size) && (fgets(line, sizeof(line), fp)) )
  {
    MYX_LOGFILE_EVENT_TYPE ev_type= 0;

    /* analyze this line */
    
    if (pcre_exec(re_select, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) > 0) 
    {

      ev_type= MYX_EVENT_SELECT;
    }
    
    if (ev_type > 0) 
    {
      MYX_LOGFILE_EVENT *event;
      
      logfile->events_num++;
      logfile->events= g_realloc(logfile->events, logfile->events_num * sizeof(MYX_LOGFILE_EVENT));
      event= logfile->events + logfile->events_num-1;
      
      event->date= NULL;
      event->line_no= line_no;
      event->event_type= ev_type;      
    }
    
    bytes_read+= strlen(line); 
    line[strlen(line)-1]= '\0';
    
    logfile->lines_num++;
    logfile->lines= g_realloc(logfile->lines, sizeof(char *) * logfile->lines_num);
    logfile->lines[logfile->lines_num-1]= g_strdup(line);
    line_no++;
  }
  
  if (ferror(fp))
  {
	  myx_free_logfile(logfile);
    return NULL;
  }
  
  return logfile;  
}




/*
 *----------------------------------------------------------------------
 * Reads in and parses a block of the given file. Which block is 
 * determined by block_num. Block_num 1 will read in and parse the last block
 * of the file.
 *
 * SYNOPSIS
 *   block_size: measured in bytes
 *   block_num: determines which block starting from the file-end will be read in;
 *              eg block_num 1 reads in the last block_size bytes from the file
 *                 block_num 3 reads in the bytes that are between block_size * 3
 *                 and block_size *2
 *  last_block: this variables will be set to indicate if this is the last-block
 *              or if there is still at least one block before the current one
 *          
 * DESCRIPTION
 *  
 * RETURN VALUE
 *
 * NOTES
 *   We start counting with 1 for Block_num. 1 is the last block of the file.
 *    Dont supply 0! 
 *  The block may be a bit greater than block_size since we have to
 *  start and end the block after/before a newline
 *----------------------------------------------------------------------
 */
MYX_LOGFILE* myx_parse_error_log(const char *filename, int block_size, int block_num, int *last_block)
{
  FILE *fp;
  MYX_LOGFILE *logfile;
  
  assert(filename != NULL);

  fp= myx_fopen(filename, "r");
  if (fp == NULL)
    return NULL;
  
  logfile= myx_parse_error_logf(fp, block_size, block_num, last_block);
  
  fclose(fp);
  
  return logfile;
}


MYX_LOGFILE* myx_parse_error_logf(FILE *fp, int block_size, int block_num, int *last_block)
{
  char line[LOG_FILE_LINE_LEN];
  bigint offset;
  int line_no;
  pcre *re_mysql_start, *re_mysql_end, *re_mysql_error, *re_innodb_start, *re_innodb_shutdown, 
    *re_mysql_force_close_thread, *re_abort;
  int rc;
  int o_vector[O_VECTOR_COUNT];
  MYX_LOGFILE *logfile;
  const char *error_str;
  int erroffset;
  bigint bytes_read;
  bigint file_size;
  
  assert(block_size > 0);
  assert(block_num != 0);
  assert(last_block != NULL);

  rewind(fp);
  
  { /* seek to correct point (we need to begin after a newline) */
    int desired_seek_point= block_size * block_num;
    file_size= get_fsize(fp);
    offset=  (desired_seek_point > file_size) ? file_size: desired_seek_point;

    offset *= -1;
  
    if (find_line_beginning(fp,&offset, file_size))
    {
	  return NULL;
    }
    
    *last_block= (offset == -file_size) ? 1 : 0;
  }
  
  if (fseek(fp, (long)offset, SEEK_END) )
  {
    return NULL;
  }

  /* setup the regular expressions */
  re_mysql_start= pcre_compile(REGEX_MYSQL_START, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);
  re_mysql_end= pcre_compile(REGEX_MYSQL_END, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);
  re_mysql_error= pcre_compile(REGEX_MYSQL_ERROR, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);

  re_innodb_start= pcre_compile(REGEX_INNODB_START, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);
  re_innodb_shutdown= pcre_compile(REGEX_INNODB_SHUTDOWN, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);
  re_mysql_force_close_thread= pcre_compile(REGEX_MYSQL_FORCE_CLOSE_THREAD, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);
  re_abort= pcre_compile(REGEX_ABORT, PCRE_ANCHORED|PCRE_CASELESS, &error_str, &erroffset, NULL);

  
  if (!re_mysql_start || !re_mysql_end || !re_mysql_error)
  {
    return NULL;
  }
  
  logfile= g_malloc0(sizeof(MYX_LOGFILE));

  logfile->file_size= file_size;
  logfile->block_num= (int)(file_size/block_size);
  
  bytes_read= 0;
  line_no=1; /* we start counting at 1! */
  while ( (bytes_read < block_size) && (fgets(line, sizeof(line), fp)) )
  {
    const char *date, *time;
    MYX_LOGFILE_EVENT_TYPE ev_type= 0;

    /* analyze this line */
    
    if ( (rc= pcre_exec(re_mysql_start, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) ) > 0) 
    {
      assert(rc == 3); //3 matches
      
      pcre_get_substring(line, o_vector, rc, 1, &date);
      pcre_get_substring(line, o_vector, rc, 2, &time);
      ev_type= MYX_EVENT_START;
    }
    else if ( (rc= pcre_exec(re_mysql_end, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) ) > 0) 
    {
      assert(rc == 3); //3 matches
      
      pcre_get_substring(line, o_vector, rc, 1, &date);
      pcre_get_substring(line, o_vector, rc, 2, &time);
      ev_type= MYX_EVENT_END;
    }
    else if ( (rc= pcre_exec(re_mysql_error, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) ) > 0) 
    {
      assert(rc == 3); //3 matches
      
      pcre_get_substring(line, o_vector, rc, 1, &date);
      pcre_get_substring(line, o_vector, rc, 2, &time);
      ev_type= MYX_EVENT_ERROR;
    }
    else if ( (rc= pcre_exec(re_innodb_start, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) ) > 0) 
    {
      assert(rc == 3); //3 matches
      
      pcre_get_substring(line, o_vector, rc, 1, &date);
      pcre_get_substring(line, o_vector, rc, 2, &time);
      ev_type= MYX_EVENT_INNODB_START;
    }
    else if ( (rc= pcre_exec(re_innodb_shutdown, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) ) > 0) 
    {
      assert(rc == 3); //3 matches
      
      pcre_get_substring(line, o_vector, rc, 1, &date);
      pcre_get_substring(line, o_vector, rc, 2, &time);
      ev_type= MYX_EVENT_INNODB_SHUTDOWN;
    }
    else if ( (rc= pcre_exec(re_mysql_force_close_thread, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) ) > 0) 
    {
      assert(rc == 3); //3 matches
      
      pcre_get_substring(line, o_vector, rc, 1, &date);
      pcre_get_substring(line, o_vector, rc, 2, &time);
      ev_type= MYX_EVENT_FORCED_CLOSE_THREAD;
    }
    else if ( (rc= pcre_exec(re_abort, NULL, line, (int)strlen(line),0,0,o_vector, O_VECTOR_COUNT) ) > 0) 
    {
      assert(rc == 3); //3 matches
      
      pcre_get_substring(line, o_vector, rc, 1, &date);
      pcre_get_substring(line, o_vector, rc, 2, &time);
      ev_type= MYX_EVENT_ABORT;
    }

    
    if (ev_type > 0) 
    {
      MYX_LOGFILE_EVENT *event;
      
      logfile->events_num++;
      logfile->events= g_realloc(logfile->events, logfile->events_num * sizeof(MYX_LOGFILE_EVENT));
      event= logfile->events + logfile->events_num-1;
      
      event->date= g_malloc0(sizeof(MYX_DATETIME));
      to_tm_date(date, time, event->date);
      event->line_no= line_no;
      event->event_type= ev_type;
      
      pcre_free((void *)date);
      pcre_free((void *)time);
    }
    
    bytes_read+= strlen(line); 
    line[strlen(line)-1]= '\0';
    
    logfile->lines_num++;
    logfile->lines= g_realloc(logfile->lines, sizeof(char *) * logfile->lines_num);
    logfile->lines[logfile->lines_num-1]= g_strdup(line);
    line_no++;
  }
  
  if (ferror(fp))
  {
    myx_free_logfile(logfile);
    return NULL;
  }  

  return logfile;
}

