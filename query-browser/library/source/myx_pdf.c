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


#include <panda/functions.h>
#include <panda/constants.h>
#include <time.h>
#include "myx_public_interface.h"


#define LINE_SPACING 4


static char *replace_strings(const char *tmpl, int page, int total_pages,
                             MYX_RESULTSET *rset)
{
  char buffer[64];
  char *str;
  time_t t;
  str= g_strdup(tmpl);
  
  sprintf(buffer, "%i", page+1);
  str= str_g_replace(str, "$PAGE$", buffer);
  sprintf(buffer, "%i", total_pages);
  str= str_g_replace(str, "$TOTAL_PAGES$", buffer);
  time(&t);
  str= str_g_replace(str, "$DATE$", ctime(&t));
  
  return str;
}


static void write_header(panda_pdf *pdf, panda_page *page, 
                         MYX_RESULTSET *rset,
                         MYX_RS_PRINT_PARAMS *params,
                         int page_num, int total_pages)
{
  if (params->header)
  {
    char *s;
    s= replace_strings(params->header, page_num, total_pages, rset);

    panda_setfontsize(pdf, params->header_font_size);
    
    panda_textbox(pdf, page,
                  params->margin_top, params->margin_left,
                  params->margin_top+params->header_height,
                  params->paper_width-params->margin_right,
                  s);

    g_free(s);
  }
}


static void write_footer(panda_pdf *pdf, panda_page *page, 
                         MYX_RESULTSET *rset,
                         MYX_RS_PRINT_PARAMS *params,
                         int page_num, int total_pages)
{
  if (params->footer)
  {
    char *s;
    s= replace_strings(params->footer, page_num, total_pages, rset);

    panda_setfontsize(pdf, params->font_size);
    
    panda_textbox(pdf, page,
                  params->paper_height-params->footer_height-params->margin_bottom, params->margin_left,
                  params->paper_height-params->margin_bottom,
                  params->paper_width-params->margin_right,
                  s);

    g_free(s);
  }
}


static int write_table_header(panda_pdf *pdf, panda_page *page,
                               MYX_RESULTSET *rset,
                               MYX_RS_PRINT_PARAMS *params)
{
  int i;
  double x;
  int y1= params->margin_top+params->header_height;

  panda_setfontsize(pdf, params->font_size);
  
  panda_setlinecolor(page, 200, 200, 200);
  panda_setfillcolor(page, 200, 200, 200);
  panda_rectangle(page, y1, params->margin_left,
                  y1+params->font_size+LINE_SPACING, params->paper_width-params->margin_right);
  panda_fillline(page);

  panda_setlinecolor(page, 0, 0, 0);
  panda_setfillcolor(page, 0, 0, 0);
  
  x= 0;
  for (i= 0; i < rset->columns_num; i++)
  {
    panda_textbox(pdf, page,
                  y1+1, params->margin_left+x,
                  y1+params->font_size, params->column_widths[i]+1000,
                  rset->columns[i].name);
    x+= params->column_widths[i];
  }

  return 0;
}


static int write_table_body(panda_pdf *pdf, panda_page *page,
                            MYX_RESULTSET *rset,
                            MYX_RS_PRINT_PARAMS *params, 
                            int first_row)
{
  int max_lines;
  int i, r;
  int y;

  max_lines=(params->paper_height-(params->margin_top+params->margin_bottom+params->header_height+params->footer_height))/(params->font_size+LINE_SPACING);
  
  max_lines-= 1; // acct for the table header
  
  y= params->margin_top + params->header_height + params->font_size+LINE_SPACING;

  for (r= 0; r < MIN(max_lines, rset->rows_num); r++)
  {
    double x= params->margin_left;

    if (r&1)
    {
      panda_setlinecolor(page, 245, 245, 245);
      panda_setfillcolor(page, 245, 245, 245);
      panda_rectangle(page,
                      y, params->margin_left,
                      y+params->font_size+LINE_SPACING, params->paper_width-params->margin_right);
      panda_fillline(page);
      panda_setlinecolor(page, 0, 0, 0);
      panda_setfillcolor(page, 0, 0, 0);
    }
    for (i= 0; i < rset->columns_num; i++)
    {
      panda_textbox(pdf, page, 
                    y+1, x,
                    y+params->font_size+LINE_SPACING, params->column_widths[i]+100000,
                    rset->rows[r].fields[i].value);
      x+= params->column_widths[i];
    }
    y+= params->font_size+LINE_SPACING;
  }

  return 0;
}


int myx_write_resultset_pdf(MYX_RESULTSET *rset, MYX_RS_PRINT_PARAMS *params)
{
  panda_pdf *pdf;
  panda_page *page;
  panda_init();
  char *font, *bfont;
  char *psize;
  int max_lines_per_page;
  int total_pages;
  int i;
  
  pdf= panda_open("test.pdf", "w");
  
  psize= g_strdup_printf("[0 0 %.2f %.2f]", params->paper_width, params->paper_height);
  
  /* margins */
  /*
  panda_setlinecolor(page, 0, 0, 0);
  panda_setlinestart(page, params->margin_left, params->margin_top);
  panda_addlinesegment(page, params->paper_width-params->margin_right, params->margin_top);
  panda_addlinesegment(page, params->paper_width-params->margin_right, params->paper_height-params->margin_bottom);
  panda_addlinesegment(page, params->margin_left, params->paper_height-params->margin_bottom);
  panda_closeline(page);
  panda_strokeline(page);
   */

  bfont= panda_createfont(pdf, "Helvetica-Bold", 1, "MacRomanEncoding");
  font= panda_createfont(pdf, "Helvetica", 1, "MacRomanEncoding");

  max_lines_per_page= (params->paper_height-(params->margin_top+params->margin_bottom+params->header_height+params->footer_height))/(params->font_size+LINE_SPACING);
  total_pages= rset->rows_num / max_lines_per_page;
  if (rset->rows_num % max_lines_per_page < max_lines_per_page/2)
    total_pages++;
  g_message("writing %i pages", total_pages);

  for (i= 0; i < total_pages; i++)
  {
    page= panda_newpage(pdf, psize);
    write_header(pdf, page, rset, params, i, total_pages);
    
    panda_setfont(pdf, bfont);
    write_table_header(pdf, page, rset, params);

    panda_setfont(pdf, font);
    write_table_body(pdf, page, rset, params, i);
    
    write_footer(pdf, page, rset, params, i, total_pages);
  }

  g_free(psize);

  panda_close(pdf);
  free(font);
  
  return 0;
}


#ifdef TEST
int cb(unsigned long current_row_count, unsigned long previous_row_count, MYX_RESULTSET *result_set, void *user_data)
{
  return 0;
}
                            

int main()
{
  MYSQL *mysql= myx_mysql_init();
  MYX_USER_CONNECTION con;
  MYX_RESULTSET *rset;
  MYX_LIB_ERROR error_code;
  MYX_RS_PRINT_PARAMS params;
  memset(&con, 0, sizeof(MYX_USER_CONNECTION));
  con.username="root";
  con.password="a";
  con.hostname="localhost";

  memset(&params, 0, sizeof(params));
  params.header="MySQL Query Browser\nDate: $DATE$";
  params.header_font_size= 9;
  params.footer= "Page: $PAGE$ of $TOTAL_PAGES$";
  params.font_size= 8;
  params.margin_top= 15;
  params.margin_bottom= 20;
  params.margin_left= 30;
  params.margin_right= 20;
  params.header_height= 30;
  params.footer_height= 20;
  params.column_widths= g_malloc(3*sizeof(double));
  params.column_widths[0]= 50;
  params.column_widths[1]= 200;
  params.column_widths[2]= 200;
  
  params.paper_width= 594;
  params.paper_height= 841;
  
  myx_connect_to_instance(&con, mysql);
  myx_use_schema(mysql, "veggies");

  rset= myx_query_execute(mysql, "select * from nuke_sections", NULL, &error_code,
                          NULL, cb);
  if (rset)
    myx_write_resultset_pdf(rset, &params);
  else
    g_message("NO RESULT!");
  
  return 0;
}
#endif
