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


#include "mdi_importer.h"


void mdi_importer_output(MDI_IMPORTER *mi, char **columns, int columns_num)
{
  int i;
  
  for (i= 0; i < mi->table->columns_num; i++)
  {
    int idx= mi->table->columns[i].index;
    if (idx >= 0 && idx < columns_num)
    {
      write(mi->out_fd, columns[idx], strlen(columns[idx])+1);
    }
    else
    {
      write(mi->out_fd, "", 1);
    }
  }
  write(mi->out_fd, "", 1);
}
