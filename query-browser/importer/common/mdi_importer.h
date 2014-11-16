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

#ifndef _MDI_IMPORTER_H_
#define _MDI_IMPORTER_H_

#ifndef MYX_PUBLIC_FUNC
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define MYX_PUBLIC_FUNC __declspec(dllexport)
#else
#define MYX_PUBLIC_FUNC
#endif
#endif


typedef struct {
  char *column_name;
  char *column_type;
  
  unsigned int index; // index of the column in the file that it corresponds to. -1 means it's not mapped
} MDI_COLUMN;


typedef struct {
  unsigned int columns_num;
  MDI_COLUMN *columns;
} MDI_TABLE;


typedef struct {
  int out_fd;
  
  MDI_TABLE *table;
} MDI_IMPORTER;


void mdi_importer_output(MDI_IMPORTER *mi, char **columns, int columns_num);

#endif /* _MDI_IMPORTER_H_ */
