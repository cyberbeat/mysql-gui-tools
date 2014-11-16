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

#include <myx_qb_library.h>
//#include <functions.h>
//#include <constants.h>

int myx_generate_pdf(MYX_RESULTSET *resultset, char *pdf_filename)
{
/*  panda_pdf *pdf;
  panda_page *curr_page;
  char *tempPtr;
  unsigned int i, j;
  MYX_RS_FIELD *field;

  // Initialise the library
  panda_init ();

  // Open our demo PDF
  if ((pdf= panda_open(pdf_filename, "w")) == NULL)
    return 1;

  curr_page= panda_newpage (pdf, panda_pagesize_a4);

  panda_setfont(pdf, tempPtr= panda_createfont(pdf, "Helvetica", 1, "WinAnsiEncoding"));

  panda_setlinestart(curr_page, 0, 2);
  panda_addlinesegment(curr_page, 600, 2);
  panda_closeline(curr_page);
  panda_strokeline(curr_page);
  panda_endline(curr_page);

  panda_setlinestart(curr_page, 0, 5);
  panda_setlinewidth (curr_page, 2);
  panda_addlinesegment(curr_page, 610, 5);
  panda_closeline(curr_page);
  panda_strokeline(curr_page);
  panda_endline(curr_page);

  panda_setlinestart(curr_page, 0, 10);
  panda_setlinewidth_d (curr_page, 1);
  panda_addlinesegment(curr_page, 600, 10);
  panda_closeline(curr_page);
  panda_strokeline(curr_page);
  panda_endline(curr_page);

  panda_setlinestart(curr_page, 0, 15);
  panda_setlinewidth_d (curr_page, 0.5);
  panda_addlinesegment(curr_page, 595, 15);
  panda_closeline(curr_page);
  panda_strokeline(curr_page);
  panda_endline(curr_page);

  panda_setlinestart(curr_page, 0, 20);
  panda_setlinewidth_d (curr_page, 0.1);
  panda_addlinesegment(curr_page, 590, 20);
  panda_closeline(curr_page);
  panda_strokeline(curr_page);
  panda_endline(curr_page);

  panda_setfontsize(pdf, 7);

  panda_textbox(pdf, curr_page, 0, 10, 16, 300,
		 "TopTest");

  panda_textrect(pdf, curr_page, 600, 10, 700, 300,
		 "MySQL Query Browser - Resultset printing ...");

  panda_textrect(pdf, curr_page, 800, 10, 16, 300,
		 "BottomTest");

  for(i= 0; i<resultset->rows_num; i++)
  {
    field= resultset->rows[i].fields;

    for(j= 0; j<resultset->columns_num_to_display; j++)
    {
      panda_textrect(pdf, curr_page, 10+j*60, 20+i*9, 10+(j+1)*60-10, 20+i*9+7,
        field->value);

      field++;
    }
  }

  panda_xfree(tempPtr);


  panda_close(pdf);*/

  return 0;
}