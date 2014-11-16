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

#ifndef _MDI_TEXT_IMPORT_H_
#define _MDI_TEXT_IMPORT_H_

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define MYX_PUBLIC_FUNC __declspec(dllexport)
#else
#define MYX_PUBLIC_FUNC
#endif


#include <glib.h>
#include <pcre.h>
#include <stdio.h>

typedef enum {
  MDI_TXI_DELIMITER,
  MDI_TXI_REGEX,
  MDI_TXI_BEGIN_END
} MDI_TXI_DELIMITER_TYPE;


typedef struct {
  FILE *file;
  char *buffer;
  unsigned int buffer_pos;
  unsigned int buffer_end;
  unsigned int buffer_size;

  char *linebreak;
  
  char *current_row;
  unsigned int current_row_size;

  unsigned int current_col_begin;
  unsigned int current_col_end;
  unsigned int current_col_next;

  MDI_TXI_DELIMITER_TYPE group_type;
  char *group_param1;
  char *group_param2;

  MDI_TXI_DELIMITER_TYPE row_type;
  char *row_param1;
  char *row_param2;
  pcre *row_re;

  MDI_TXI_DELIMITER_TYPE col_type;
  char *col_param1;
  char *col_param2;
  pcre *col_re;

  int columns_size;
  char **columns;
} MDI_TXT_IMPORTER;



MYX_PUBLIC_FUNC MDI_TXT_IMPORTER *mdi_txt_importer_open_file(const char *file,
                                                             int start_line,
                                                             const char *linebreak);

MYX_PUBLIC_FUNC int mdi_txt_importer_set_row_delimiter(MDI_TXT_IMPORTER *ti,
                                                       MDI_TXI_DELIMITER_TYPE dtype,
                                                       const char *param1,
                                                       const char *param2);

MYX_PUBLIC_FUNC int mdi_txt_importer_set_column_delimiter(MDI_TXT_IMPORTER *ti,
                                                          MDI_TXI_DELIMITER_TYPE dtype,
                                                          const char *param1,
                                                          const char *param2);

MYX_PUBLIC_FUNC char **mdi_txt_importer_get_next_row(MDI_TXT_IMPORTER *ti, int *columns_num);

MYX_PUBLIC_FUNC void mdi_txt_importer_free_columns(char **columns, int columns_num);
MYX_PUBLIC_FUNC void mdi_free_txt_importer(MDI_TXT_IMPORTER *ti);

#endif /* _MDI_TEXT_IMPORT_H_ */
