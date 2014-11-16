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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <glib.h>
#include "myx_qb_library.h"


#define SCRIPT_MAX_LINE_LEN 1024*1024

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



/* copied from myx_log_files.c.
 * TODO: should be factored out
 */
#define STEP_SIZE 100
static int find_line_beginning(FILE *fp, bigint *offset, bigint file_size)
{
  bigint uoff;
  char buffer[STEP_SIZE+1];
  char *newline_p= NULL;
  int step_size= STEP_SIZE;
  int i;
  
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
    }
    if (fseek(fp, (long)uoff, SEEK_END) )
    {
      return -1;
    }
    fread(buffer, 1, step_size, fp);
   
    for (p= buffer,i= 0; *p && i < step_size; p++,i++)
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


MYX_SCRIPT_FILE_BLOCK* myx_parse_script_file(const char *filename, int block_size, int block_num, int *last_block)
{
  char line[SCRIPT_MAX_LINE_LEN];
  FILE *fp;
  bigint offset;
  int line_no;
  bigint bytes_read;
  bigint file_size= -1;
  MYX_SCRIPT_FILE_BLOCK *script;

  g_return_val_if_fail(filename, NULL); 
  g_return_val_if_fail(block_size > 0, NULL);
  g_return_val_if_fail(block_num != 0, NULL);
  g_return_val_if_fail(last_block != NULL, NULL);
  
  if ((fp= fopen(filename, "r")) == NULL)
  {
    return NULL;
  }
  
  { /* seek to correct point (we need to begin after a newline) */
    int desired_seek_point= block_size * block_num;
    file_size= get_file_size(filename);
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
    fclose(fp);
    return NULL;
  }


  script= g_malloc0(sizeof(MYX_SCRIPT_FILE_BLOCK));

  script->file_size= file_size;
  script->block_num= (int)(file_size/block_size);
  
  bytes_read= 0;
  line_no=1; /* we start counting at 1! */
  while ( (bytes_read < block_size) && (fgets(line, sizeof(line), fp)) )
  {
    bytes_read+= strlen(line); 
    line[strlen(line)-1]= '\0';
    
    script->lines_num++;
    script->lines= g_realloc(script->lines, sizeof(char *) * script->lines_num);
    script->lines[script->lines_num-1]= g_strdup(line);
    line_no++;
  }
  
  if (ferror(fp))
  {
    fclose(fp);
    return NULL;
  }
  
  fclose(fp);
  return script;
}

void myx_free_script_file_block(MYX_SCRIPT_FILE_BLOCK *script)
{
  if (script)
  {
    unsigned int i;
    
    for (i=0; i < script->lines_num; i++)
    {
      g_free(script->lines[i]);
    }

    g_free(script->lines);
    g_free(script);
  }
}
