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

#define BUFFER_RESIZE_INCREMENT (1024 * 256) // increase in 256k increments
#define CALLBACK_INTERVAL (1024ULL * 128)    // Call back the application every 100KB.

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "myx_library.h"
#include <glib.h>
#include "myx_simple_sql_parsing.h"

#define MASK_CHAR '°'  // Used to mask out strings that are not relevant to the parse process.

/* 
 * Forward declarations
 */

static int make_state_transition(int state, bigint c, MYX_SQL_PARSE_ENVIRONMENT *we, int* delimiter_index);
                                    
/* (note: this diagram is missing several transitions)

   Begin
     |
     V
 .................
 :               :<-(N)---+
 :               :        |
 :   +--(-)->[4]----(-)->[5]
 :   |        |  :
 :   |        |  :<----------------------------------------+
 :   | <------+  :                                         |
 :->[0]          :<-(!)---+      +----------+ +-(*)-+      |
 :   | <------+  :        |      |          | |     |      |
 :   |        |  :        |      V          | |     |      |
 :   |        |  :        |  +->[9]-(*)-+   | |     V      |
 :   +--(/)->[7]----(*)->[8]-+          +->[==12=====]-(/)-+
 :               :           +------(*)-+
 :               :
 :               :<-(N)---+
 :               :        |
 :               :--(#)->[6]
 :               :
 :               :        +---------+
 :               :        |         |
 :               :        V         |
 :               :<-(")->[2]-(\)->[11]
 :               :
 :               :        +---------+
 :               :        |         |
 :               :        V         |
 :               :<-(')->[3]-(\)->[10]
 :               :
 :               :<-(`)---+
 :               :        |
 :               :--(`)->[14]
 :               :
 :               :
 :               :--(;)->[1]--------->[13]
 .................        ^            |
         |       ^        |            |
         +-(X)->[15]-(Y)--+            V
                                      End


 
State 15 is for alternative delimiters, such as \\, which require two of
them to be valid. When an alternative delimiter is enabled, the ; state
is interpreted as "any other character".

Note 2006-07-24: the alternative delimiter handling is a mess and I had to tweak the state table plus add
                 an explicit lookahead for the delimiter keyword.
*/

//N = newline, E= any other character, X= alternative delimiter, Y= last char of alternative delimiter
static int state_machine[16][13] =
{
  //     ;   '   "   #   -   /   *   N   \   E   !   `   X
/*0*/  { 1,  3,  2,  6,  4,  7,  0,  0,  0,  0,  0,  14, 15 },
/*1*/  { 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13 },
/*2*/  { 2,  2,  0,  2,  2,  2,  2,  2,  11, 2,  2,  2,  2  },
/*3*/  { 3,  0,  3,  3,  3,  3,  3,  3,  10, 3,  3,  3,  3  },
/*4*/  { 1,  3,  2,  6,  5,  7,  0,  0,  0,  0,  0,  14, 0  },
/*5*/  { 5,  5,  5,  5,  5,  5,  5,  0,  5,  5,  5,  5,  5  },
/*6*/  { 6,  6,  6,  6,  6,  6,  6,  0,  6,  6,  6,  6,  6  },
/*7*/  { 1,  3,  2,  6,  4,  7,  8,  0,  0,  0,  0,  14, 0  },
/*8*/  { 9,  9,  9,  9,  9,  9,  12, 9,  9,  9,  0,  9,  9  },
/*9*/  { 9,  9,  9,  9,  9,  9,  12, 9,  9,  9,  9,  9,  9  },
/*10*/ { 3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3  },
/*11*/ { 2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2  },
/*12*/ { 9,  9,  9,  9,  9,  0,  12, 9,  9,  9,  9,  9,  9  },
/*13*/ { 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13 },
/*14*/ { 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,  0, 14 },
/*15*/ { 15, 15, 15, 15, 15, 15, 15,  1, 15, 15, 15, 15, 15 }
};


/*
 * Public functions
 */

/**
 * This function reads the next sql-statement into
 * buffer.
 * If resize_buffer==1 then buffer is automatically
 * resized if the sql-statement is bigger than the buffer
 *
 * Buffer has to be a memory area returned by g_malloc if
 * it shall be resized!
 *
 * Return Value: the length of the next sql-statement or 0
 *
 * read_char is a function that returns the next character
 *   or -1 in case there is no more character.
 *   The first argument to read_char is arg
 *   The second argument will be set to the number of bytes read
 *   The third argument to read_char is a pointer to error-var
 *   If there is an error in read_char error should be set and 
 *   -1 be returned.
 **/
int myx_get_next_sql_statement(MYX_SQL_PARSE_ENVIRONMENT *we,
                               char **buffer, int *buffer_len,
                               int resize_buffer, MYX_LIB_ERROR *error,
                               bigint (*read_char)(void *user_data,
                                                   int *bytes_read,
                                                   MYX_LIB_ERROR *error),
                               int (*progress_report)(bigint bytes_read,
                                                       bigint bytes_total,
                                                       void *user_data),
                               bigint file_size,
                               void *user_data,
                               void *arg)
{
  int h= 0;
  int bytes_read= 0;
  int delimiter_index;
  int delimiter_length;
  bigint c;
  int state= we->state;
  int dos_line_pending= 0;

  delimiter_index= 0;
  delimiter_length= sizeof("delimiter") - 1; // exclude null char
  *error= MYX_NO_ERROR;

  //fast-forward
  while ( state!=1 && (c= read_char(arg, &bytes_read, error)) != -1 )
  {
    we->chars_read++;
    we->bytes_read+= bytes_read;
    state= make_state_transition(state, c, we, &delimiter_index);
  }

  // readchar returned an error
  if (*error) return 0;

  /* read in (the beginning of) the next sql-statement */
  if (state == 1)
    state= 0;

  h= 0;
  delimiter_index= 0;
  //while ( state!=1 && (resize_buffer || h< (*buffer_len)-6) &&
  //       (c= read_char(arg, &bytes_read, error)) != -1 )
  while (state!=1 && (resize_buffer || h< (*buffer_len)-6))
  {
    c= read_char(arg, &bytes_read, error);

    if(c != -1)
    {
      we->chars_read++;
      we->bytes_read+= bytes_read;
    }

    if((progress_report != NULL) && ((we->bytes_read % CALLBACK_INTERVAL) == 0) && (c != -1))
    {
      if(progress_report(we->bytes_read, file_size, user_data))
      {
        return -1;
      }
    }
    if (resize_buffer && !(h < (*buffer_len) - 7))
    {
      gchar *tmp;

      /* increase buffer size */
      tmp= g_try_realloc(*buffer, *buffer_len + BUFFER_RESIZE_INCREMENT);
      if (!tmp)
      {
        *error= -1;
        return 0;
      }
      *buffer= tmp;
      *buffer_len+= BUFFER_RESIZE_INCREMENT;
    }

    /* Record the character if we are in a "no comment"-section */
    if (state != 6 && state != 5 && state != 9 && state != 12 && c != -1)
    {
      //write
      if ( !(h==0 && g_unichar_isspace((gunichar)c)) && !(h==0 && c == 0xFEFF))
      {  /* ignore beginning white-space or a beginning BOF
          * Windows uses a byte order mark (BOM) for example to mark a
          *  file as UTF8
          */
        if (h == 0)
        {
          we->stmt_begin_char= we->chars_read-1;//we->bytes_read - 1;
          we->stmt_begin_line= we->line_num;
        }
        h+= g_unichar_to_utf8( (gunichar) c, (*buffer)+h );
      }
    }

    // Lookahead for delimiter keyword.
    if ((state == 0) && (h >= delimiter_length) && (g_ascii_strncasecmp(*buffer, "delimiter", delimiter_length) == 0))
      state= 15; // Put in alternative delimiter state.
    else
    {
      if (c == '\r' || c == '\n' || c == -1)
      {
        if (c == '\n')
        {
          if (dos_line_pending)
          {
            // Skip linefeeds directly after a carriage return.
            dos_line_pending= 0;
            continue;
          };
        }
        else
          dos_line_pending= 1;

        if(c != -1)
          we->line_num++;

        // Handle delimiter command.
        if (state == 15)
        {
          // Extract the new delimiter.
          char *delim = *buffer + delimiter_length;

          // str_trim does trimming in place.
          delim = str_trim(g_strndup(delim, (gsize) h - delimiter_length));
          if (delim)
          {
            g_free(we->alt_delimiter);
            if (strcmp(delim, ";") == 0)
            {
              g_free(delim);
              we->alt_delimiter= NULL;
            }
            else
              we->alt_delimiter= delim;
            delimiter_index= 0;
            state= 1; // force final state
            break;
          }
        }
      }
      else
        dos_line_pending= 0;

      // special hack for DELIMITER x\EOF
      if(c == -1)
        return 0;

      /* If this character triggers a comment, delete the comment-beginning */
      if (state == 4 && c == '-')
      {
        h-= 2; 
        if (h == 0)
        {
          we->stmt_begin_char= we->chars_read-1;//we->bytes_read - 1;
          we->stmt_begin_line= we->line_num;
        }   
      }
      else if (state == 0 && c == '#')
      {
        h-= 1;
        if (h == 0)
        {
          we->stmt_begin_char= we->chars_read-1;
          we->stmt_begin_line= we->line_num;
        }
      }
      else if (state == 8 && c != '!')
      {
        h-= 3;
        if (h == 0)
        {
          we->stmt_begin_char= we->chars_read-1;
          we->stmt_begin_line= we->line_num;
        };
      };

      state= make_state_transition(state, c, we, &delimiter_index);
    };
  };

  // readchar returned an error
  if (*error) return 0;

  if (we->alt_delimiter)
    we->stmt_end_char= we->chars_read
      - strlen(we->alt_delimiter);
  else
    we->stmt_end_char= we->chars_read-1;
  we->stmt_end_line= we->line_num;

  (*buffer)[h]= '\0';

  we->state= state;

  return h;
}


/**
 * Reads in the next sql-statement from file
 *
 * @note covered by unit tests
 */
int myx_get_next_sql_statement_file(MYX_SQL_PARSE_ENVIRONMENT *we,
                                    MYX_INTL_FILE *stat,
                                    char **buffer, int *buffer_len,
                                    int resize_buffer, 
                                    int (*progress_report)(bigint bytes_read,
                                                           bigint bytes_total,
                                                           void *user_data),
                                    bigint file_size,
                                    void *user_data,
                                    MYX_LIB_ERROR *error)
{
  bigint (* read_char)(void *, int *, MYX_LIB_ERROR *)=
        (bigint (*)(void *, int *, MYX_LIB_ERROR *)) myx_read_char_from_intl_file;
  return myx_get_next_sql_statement(we, buffer, buffer_len,
                                    resize_buffer, error,
                                    read_char,
                                    progress_report,
                                    file_size,
                                    user_data,
                                    (void *)stat);
}


void myx_init_string_wrapper(const char *string, MYX_STRING_WRAPPER * w)
{
  if (g_utf8_validate(string, -1 , NULL))
  {
    memset(w,0,sizeof(*w));
    w->string= (char *)string;
    w->pos= 0;
  }
}

void myx_done_string_wrapper(MYX_STRING_WRAPPER *w)
{
  g_free(w->string);
}

bigint myx_read_char_from_string(void *w, int *bytes_read, MYX_LIB_ERROR *error)
{
  MYX_STRING_WRAPPER *wrapper= (MYX_STRING_WRAPPER *) w;
  bigint c;
  bigint old_pos;

  c= g_utf8_get_char(wrapper->string + wrapper->pos);

  old_pos= wrapper->pos;

  wrapper->pos=
            g_utf8_next_char(wrapper->string + wrapper->pos) - wrapper->string;
  //the cast is ok because bytes_read can never be greater than 6
  *bytes_read= (int) (wrapper->pos - old_pos);
 
  return c == 0 ? EOF : c;
}

/**
 * @note covered by unit tests
 */
void myx_init_sql_parse_environment(MYX_SQL_PARSE_ENVIRONMENT * we)
{
  we->state= 1;
  we->bytes_read= 0;
  we->chars_read= 0;
}

void myx_init_sql_parse_environment_fill_zero(MYX_SQL_PARSE_ENVIRONMENT * we)
{
  memset(we,0,sizeof(*we));
  myx_init_sql_parse_environment(we);
}

void myx_done_sql_parse_environment(MYX_SQL_PARSE_ENVIRONMENT * we)
{
}

//----------------------------------------------------------------------------------------------------------------------

int process_alt_delimiter(int state, bigint next_symbol, MYX_SQL_PARSE_ENVIRONMENT* we, int* delimiter_index,
  int* res)
{
  if (we->alt_delimiter)
  {
    if (next_symbol == we->alt_delimiter[*delimiter_index])
    {
      if (we->alt_delimiter[*delimiter_index+1] == '\0')
      {
        // End of delimiter reached. This ends the whole statement.
        *delimiter_index= 0;
        *res= state_machine[state][0];
      }
      else
      {
        // Found a multi-char delimiter. Continue scanning.
        (*delimiter_index)++;
      }
      return 1;
    }
    else
    {
      // Not a complete delimiter was found so reset delimiter index and start over looking.
      *delimiter_index= 0;

      // Prevent an eventual semicolon from changing the current state to 1.
      if (next_symbol == ';')
      {
        *res= state_machine[state][9];
        return 1;
      }
    }
  }
  
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * This function works with Unicode characters too
 * because all ASCII characters have the same code-position
 * in Unicode as in ASCII
 * It won't work on non-Ascii platforms but we don't care.
 */
static int make_state_transition(int state, bigint next_symbol, MYX_SQL_PARSE_ENVIRONMENT* we, int* delimiter_index)
{
  int res= state;
  if (process_alt_delimiter(state,next_symbol, we, delimiter_index, &res))
    return res;
  
  switch(next_symbol)
  {
    case ';'  : return state_machine[state][0];
    case '\'' : return state_machine[state][1];
    case '"'  : return state_machine[state][2];
    case '#'  : return state_machine[state][3];
    case '-'  : return state_machine[state][4];
    case '/'  : return state_machine[state][5];
    case '*'  : return state_machine[state][6];
    case '\r' : // Only one of both comes in, depending on the platform.
    case '\n' : // \n is skipped if preceeded by a \r.
      return state_machine[state][7];
    case '\\' : return state_machine[state][8];
    case '!'  : return state_machine[state][10];
    case '`'  : return state_machine[state][11];
    default   : return state_machine[state][9];
  };
}

//----------------------------------------------------------------------------------------------------------------------

void fill(char * begin, char * end, char c)
{
  char * p;
  for (p= begin; p!=end; p++)
    *p= c;
}

void kill_comments(char * sql_to_mask)
{
  MYX_SQL_PARSE_ENVIRONMENT we;
  int state= 0, new_state;
  char * pos, * next_pos;
  myx_init_sql_parse_environment_fill_zero(&we);
  for (pos= sql_to_mask; *pos; pos= next_pos)
  {
    next_pos= g_utf8_next_char(pos);
    new_state= make_state_transition(state,(next_pos-pos) > 1 ? MASK_CHAR : *pos, &we, 0);
    switch (new_state)
    {
    case 5: case 12: // -- xxxxxx
      fill(pos,next_pos,' ');
      if (state==4)
        *(pos-1)= ' ';
      break;
    case 9:         // /* xxxxx */
      fill(pos,next_pos,' ');
      if (state==8)
      {
        *(pos-1)= ' ';
        *(pos-2)= ' ';
      }
      break;
    case 6:         // # xxxxx
      fill(pos,next_pos,' ');
      break;
    default:        // last slash in /* */
      if (state==12) 
        fill(pos,next_pos,' ');
    }
    state= new_state;
  }
  myx_done_sql_parse_environment(&we);
}

void mask_quotas(char * sql_to_mask)
{
  MYX_SQL_PARSE_ENVIRONMENT we;
  int state= 0;
  char * pos, * next_pos;
  myx_init_sql_parse_environment_fill_zero(&we);
  for (pos= sql_to_mask; *pos; pos= next_pos)
  {
    next_pos= g_utf8_next_char(pos);
    if ((next_pos-pos)>1)
    {
      fill(pos,next_pos, MASK_CHAR);
      state= make_state_transition(state,*pos,&we,0);
    }
    else
    {
      int new_state= make_state_transition(state,*pos,&we,0);
      if (new_state==5  ||                              // --xxxxxx
          new_state==9  ||                              // /*xxxxx*/
          new_state==2  || new_state==11 || state==2 || // "xxxxxxx"
          new_state==3  || new_state==10 || state==3 || // 'xxxxxxxx'
          new_state==14 || state==14                    // `xxxxxxxx`
          )
      {
        *pos= MASK_CHAR;
      }
      state= new_state;
    }
  }
  myx_done_sql_parse_environment(&we);
}

void mask_quotas_and_brackets(char * sql_to_mask)
{
  mask_quotas(sql_to_mask);
  mask_out_string(sql_to_mask, '(', ')', MASK_CHAR);
}

static char *check_delimiterX(const char *buffer, unsigned int buflen)
{
  unsigned int i;
  
  if (buflen < sizeof("DELIMITER X"))
    return NULL;
  
  for (i= 0; i < buflen; i++)
  {
    if (buffer[i]=='\n')
      break;
  }
  if (i == buflen) 
    return NULL;

  if (g_strncasecmp(buffer, "DELIMITER ", sizeof("DELIMITER ")-1)==0)
  {
    unsigned int j;
    for (j= sizeof("DELIMITER ")-1; j < i; j++)
      if (buffer[j]!=' ')
        break;
    if (j < i)
      return str_trim(g_strndup(buffer+j, i-j));
  }

  return NULL;
}
