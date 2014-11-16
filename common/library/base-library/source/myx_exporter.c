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
#include <glib.h>
#include "myx_public_interface.h"
#include "myx_shared_util_functions.h"


static MYX_TABLE_EXPORTER_INFO *make_exporter_info(MYX_TABLE_EXPORTER *exporter)
{
  MYX_TABLE_EXPORTER_INFO *info= g_new0(MYX_TABLE_EXPORTER_INFO, 1);
  unsigned int i;

  info->te= exporter;

  for (i= 0; info->te->options[i].name; i++);
  info->option_values= g_new0(char*, i);
  for (i= 0; info->te->options[i].name; i++)
    info->option_values[i]= g_strdup(info->te->options[i].value);
  
  return info;
}


static void add_tabledef(MYX_TABLE_EXPORTER_INFO *info,
                         unsigned int columns_num, MYX_TE_COLUMN *columns)
{
  unsigned int i;

  info->table_levels++;
  info->columns_num= g_realloc(info->columns_num, sizeof(unsigned int)*info->table_levels);
  info->columns= g_realloc(info->columns, sizeof(MYX_TE_COLUMN)*info->table_levels);

  info->columns_num[info->table_levels-1]= columns_num;
  info->columns[info->table_levels-1]= g_new(MYX_TE_COLUMN, columns_num);

  for (i= 0; i < columns_num; i++)
  {
    info->columns[info->table_levels-1][i]= columns[i];
    info->columns[info->table_levels-1][i].name= g_strdup(columns[i].name);
  }
}


static void free_exporter_info(MYX_TABLE_EXPORTER_INFO *info)
{
  if (info->file_stream)
    fclose(info->file_stream);

  g_free(info);
}



// ------------------------------------------------------------
// CSV exporter
// ------------------------------------------------------------

//typedef struct {
//} MYX_CSV_EXPORTER_INFO;


static MYX_TABLE_EXPORTER_INFO *csv_exporter_init();
static void csv_exporter_free(MYX_TABLE_EXPORTER_INFO *info);
static int csv_exporter_setup(MYX_TABLE_EXPORTER_INFO *self, const char *filename);
static void csv_exporter_begin(MYX_TABLE_EXPORTER_INFO *self, const char *text);
static void csv_exporter_table_setup(MYX_TABLE_EXPORTER_INFO *self, unsigned int columns_num, MYX_TE_COLUMN *columns);
static void csv_exporter_table_header(MYX_TABLE_EXPORTER_INFO *self);
static void csv_exporter_columns(MYX_TABLE_EXPORTER_INFO *self, const char *const*text);
static void csv_exporter_table_footer(MYX_TABLE_EXPORTER_INFO *self);
static void csv_exporter_end(MYX_TABLE_EXPORTER_INFO *self);


static MYX_TE_OPTION csv_exporter_options[]= 
{
  {"Separator", ",", "Separator for column values.", MYX_TEOP_STRING},
  {NULL, NULL, NULL, 0}
};

MYX_TABLE_EXPORTER CSV_TABLE_EXPORTER= 
{
  "CSV",
  "Generates a CSV (Comma Separated Values) File",
  "Comma Separated Values File",
  "csv",
  csv_exporter_init,
  csv_exporter_free,
  csv_exporter_setup,
  csv_exporter_begin,
  csv_exporter_table_setup,
  csv_exporter_table_header,
  csv_exporter_columns,
  csv_exporter_table_footer,
  csv_exporter_end,

  csv_exporter_options
};


static MYX_TABLE_EXPORTER_INFO *csv_exporter_init()
{
  MYX_TABLE_EXPORTER_INFO *info;
  
  info= make_exporter_info(&CSV_TABLE_EXPORTER);
//  info->priv= g_new0(MYX_CSV_EXPORTER_INFO, 1);

  return info;
}

static void csv_exporter_free(MYX_TABLE_EXPORTER_INFO *self)
{
//  g_free(self->priv);
  
  free_exporter_info(self);
}


static int csv_exporter_setup(MYX_TABLE_EXPORTER_INFO *self, const char *filename)
{
  self->file_stream= myx_fopen(filename, "w+");
  if (!self->file_stream)
    return -1;
  return 0;
}


static void csv_exporter_begin(MYX_TABLE_EXPORTER_INFO *self, const char *text)
{
}


static void csv_exporter_table_setup(MYX_TABLE_EXPORTER_INFO *self, unsigned int columns_num, MYX_TE_COLUMN *columns)
{
  add_tabledef(self, columns_num, columns);
}


static void csv_exporter_table_header(MYX_TABLE_EXPORTER_INFO *self)
{
  unsigned int i;
  
  self->current_level++;
  if (self->current_level > 1)
  {
    g_warning("Multi-level export not supported for csv");
    return;
  }
  
  //fprintf(self->file_stream, "# ");
  for (i= 0; i < self->columns_num[0]; i++)
  {
    char *s;
    if (self->columns[0][i].name)
      s= str_g_replace(g_strdup(self->columns[0][i].name), "\"", "\"\"");
    else
      s= NULL;

    if (i > 0)
      fprintf(self->file_stream, "%s\"%s\"", self->option_values[0], s?s:"");
    else
      fprintf(self->file_stream, "\"%s\"", s?s:"");

    g_free(s);
  }

  fprintf(self->file_stream, "\n");
}


static void csv_exporter_columns(MYX_TABLE_EXPORTER_INFO *self, const char *const*text)
{
  unsigned int i;
  for (i= 0; i < self->columns_num[0]; i++)
  {
    if(self->columns[0][i].ctype == MYX_TE_COL_STRING || self->columns[0][i].ctype == MYX_TE_COL_DATE)
    {
      char *s;
      
      if (text[i])
        s= str_g_replace(g_strdup(text[i]), "\"", "\"\"");
      else
        s= NULL;

      if (i > 0)
        fprintf(self->file_stream, "%s\"%s\"", self->option_values[0], s?s:"");
      else
        fprintf(self->file_stream, "\"%s\"", s?s:"");

      g_free(s);
    }
    else
    {
      const char *s;
      
      if (text[i])
        s= text[i];
      else
        s= "";
      
      if (i > 0)
        fprintf(self->file_stream, "%s%s", self->option_values[0], s);
      else
        fprintf(self->file_stream, "%s", s);
    }
  }
  fprintf(self->file_stream, "\n");
}

static void csv_exporter_table_footer(MYX_TABLE_EXPORTER_INFO *self)
{
  self->current_level--;
}

static void csv_exporter_end(MYX_TABLE_EXPORTER_INFO *self)
{
}



// ------------------------------------------------------------
// HTML exporter
// ------------------------------------------------------------

typedef struct {
  int row_num;
} MYX_HTML_EXPORTER_INFO;

static MYX_TABLE_EXPORTER_INFO *html_exporter_init();
static void html_exporter_free(MYX_TABLE_EXPORTER_INFO *info);
static int html_exporter_setup(MYX_TABLE_EXPORTER_INFO *self, const char *filename);
static void html_exporter_begin(MYX_TABLE_EXPORTER_INFO *self, const char *text);
static void html_exporter_table_setup(MYX_TABLE_EXPORTER_INFO *self, unsigned int columns_num, MYX_TE_COLUMN *columns);
static void html_exporter_table_header(MYX_TABLE_EXPORTER_INFO *self);
static void html_exporter_columns(MYX_TABLE_EXPORTER_INFO *self, const char *const*text);
static void html_exporter_table_footer(MYX_TABLE_EXPORTER_INFO *self);
static void html_exporter_end(MYX_TABLE_EXPORTER_INFO *self);


static MYX_TE_OPTION html_exporter_options[]= 
{
  {"Even Row Color", "", "Color for even rows.", MYX_TEOP_COLOR},
  {"Odd Row Color", "", "Color for odd rows.", MYX_TEOP_COLOR},
  {"HTML Table Attributes", "border=1 cellspacing=1 cellpadding=0", "Attributes that will be used in the generated HTML table.", MYX_TEOP_STRING},
  {NULL, NULL, NULL, 0}
};

MYX_TABLE_EXPORTER HTML_TABLE_EXPORTER= 
{
  "HTML",
  "Generates a HTML File",
  "HTML File",
  "html",
  html_exporter_init,
  html_exporter_free,
  html_exporter_setup,
  html_exporter_begin,
  html_exporter_table_setup,
  html_exporter_table_header,
  html_exporter_columns,
  html_exporter_table_footer,
  html_exporter_end,

  html_exporter_options
};


static MYX_TABLE_EXPORTER_INFO *html_exporter_init()
{
  MYX_TABLE_EXPORTER_INFO *info;
  
  info= make_exporter_info(&HTML_TABLE_EXPORTER);
  info->priv= g_new0(MYX_HTML_EXPORTER_INFO, 1);

  return info;
}

static void html_exporter_free(MYX_TABLE_EXPORTER_INFO *info)
{
  g_free(info->priv);
  free_exporter_info(info);
}


static int html_exporter_setup(MYX_TABLE_EXPORTER_INFO *self, const char *filename)
{
  self->file_stream= myx_fopen(filename, "w+");
  if (!self->file_stream)
    return -1;
  return 0;
}


static void html_exporter_begin(MYX_TABLE_EXPORTER_INFO *self, const char *text)
{
  fprintf(self->file_stream, "<html>\n");
  fprintf(self->file_stream, "<head>\n");
  fprintf(self->file_stream, "<title>%s</title>\n", text);
  fprintf(self->file_stream, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n");
  fprintf(self->file_stream, "</head>\n");
  fprintf(self->file_stream, "<body><h1>%s</h1>\n", text);
}


static void html_exporter_table_setup(MYX_TABLE_EXPORTER_INFO *self, unsigned int columns_num, MYX_TE_COLUMN *columns)
{
  add_tabledef(self, columns_num, columns);
}


static void html_exporter_table_header(MYX_TABLE_EXPORTER_INFO *self)
{
  unsigned int i;
  const char *table_extra= "";

  self->current_level++;
  
  if (self->current_level > 1)
  {
    fprintf(self->file_stream, "<tr><td colspan=%i>\n",
            self->columns_num[self->current_level-2]);
    table_extra= "width=100% ";
  }
  
  fprintf(self->file_stream, "<table %s%s><tr>\n", table_extra, self->option_values[2]);
  for (i= 0; i < self->columns_num[self->current_level-1]; i++)
  {
    fprintf(self->file_stream, "<th>%s</th>", self->columns[self->current_level-1][i].name);
  }
  fprintf(self->file_stream, "</tr>\n");
  
  ((MYX_HTML_EXPORTER_INFO*)self->priv)->row_num= 0;
}


static void html_exporter_columns(MYX_TABLE_EXPORTER_INFO *self, const char *const*text)
{
  unsigned int i;
  const char *color;
  
  if (((MYX_HTML_EXPORTER_INFO*)self->priv)->row_num%2)
    color= self->option_values[1];
  else
    color= self->option_values[0];

  if (color && *color)
    fprintf(self->file_stream, "<tr bgcolor=\"%s\">\n", color);
  else
    fprintf(self->file_stream, "<tr>\n");
  for (i= 0; i < self->columns_num[self->current_level-1]; i++)
  {
    char *tmp;
    
    if (text[i])
      tmp= escape_html_entities(text[i]);
    else
      tmp= "";
    fprintf(self->file_stream, "<td>%s</td>", tmp);
    if (text[i])
      g_free(tmp);
  }
  fprintf(self->file_stream, "</tr>\n");
}


static void html_exporter_table_footer(MYX_TABLE_EXPORTER_INFO *self)
{
  fprintf(self->file_stream, "</table>\n");
  
  if (self->current_level > 1)
    fprintf(self->file_stream, "</td></tr>\n");
  
  self->current_level--;
}


static void html_exporter_end(MYX_TABLE_EXPORTER_INFO *self)
{
  fprintf(self->file_stream, "</body></html>\n");
}


// ------------------------------------------------------------
// XML exporter
// ------------------------------------------------------------

typedef struct {
  char *dtd_filename;
  int dtd_written;
} MYX_XML_EXPORTER_INFO;


static MYX_TABLE_EXPORTER_INFO *xml_exporter_init();
static void xml_exporter_free(MYX_TABLE_EXPORTER_INFO *info);
static int xml_exporter_setup(MYX_TABLE_EXPORTER_INFO *self, const char *filename);
static void xml_exporter_begin(MYX_TABLE_EXPORTER_INFO *self, const char *text);
static void xml_exporter_table_setup(MYX_TABLE_EXPORTER_INFO *self, unsigned int columns_num, MYX_TE_COLUMN *columns);
static void xml_exporter_table_header(MYX_TABLE_EXPORTER_INFO *self);
static void xml_exporter_columns(MYX_TABLE_EXPORTER_INFO *self, const char *const*text);
static void xml_exporter_table_footer(MYX_TABLE_EXPORTER_INFO *self);
static void xml_exporter_end(MYX_TABLE_EXPORTER_INFO *self);

static MYX_TE_OPTION xml_exporter_options[]= 
{
  {"Generate DTD", "1", "Write the DTD for the generated XML file.", MYX_TEOP_BOOL},
  {"Record Tag", "row", "Name of tag for record delimiter.", MYX_TEOP_STRING},
  {"Root Attributes", "", "Attribute name/value pairs for the root tag.", MYX_TEOP_STRINGLIST},
  {NULL, NULL, NULL, 0}
};


MYX_TABLE_EXPORTER XML_TABLE_EXPORTER= 
{
  "XML",
  "Generates a XML File",
  "XML File",
  "xml",
  xml_exporter_init,
  xml_exporter_free,
  xml_exporter_setup,
  xml_exporter_begin,
  xml_exporter_table_setup,
  xml_exporter_table_header,
  xml_exporter_columns,
  xml_exporter_table_footer,
  xml_exporter_end,
  
  xml_exporter_options
};


static MYX_TABLE_EXPORTER_INFO *xml_exporter_init()
{
  MYX_TABLE_EXPORTER_INFO *info;
  
  info= make_exporter_info(&XML_TABLE_EXPORTER);

  info->priv= g_new0(MYX_XML_EXPORTER_INFO, 1);

  return info;
}


static void xml_exporter_free(MYX_TABLE_EXPORTER_INFO *self)
{
  MYX_XML_EXPORTER_INFO* xi= (MYX_XML_EXPORTER_INFO*)self->priv;
  if(xi->dtd_filename)
    g_free(xi->dtd_filename);
  
  g_free(self->priv);
  free_exporter_info(self);
}


static int xml_exporter_setup(MYX_TABLE_EXPORTER_INFO *self, const char *filename)
{
  MYX_XML_EXPORTER_INFO* xi= (MYX_XML_EXPORTER_INFO*)self->priv;
  char *ptr;

  self->file_stream= myx_fopen(filename, "w+");
  if(!self->file_stream)
  {
    return -1;
  }


  ptr= strrchr(filename,'.');
  if (ptr)
    xi->dtd_filename= g_strdup_printf("%.*s.dtd", ptr-filename, filename);
  else
    xi->dtd_filename= g_strdup_printf("%s.dtd", filename);

  return 0;
}


static void xml_exporter_begin(MYX_TABLE_EXPORTER_INFO *self, const char *text)
{
  const char *fn;

  fn= strrchr(((MYX_XML_EXPORTER_INFO*)self->priv)->dtd_filename, MYX_PATH_SEPARATOR);
  if (!fn)
    fn= ((MYX_XML_EXPORTER_INFO*)self->priv)->dtd_filename;
  else
    fn++;

  fprintf(self->file_stream, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  fprintf(self->file_stream, "<!DOCTYPE ROOT SYSTEM \"%s\">\n", fn);
  fprintf(self->file_stream, "<ROOT xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n");
}


static void xml_exporter_table_setup(MYX_TABLE_EXPORTER_INFO *self, unsigned int columns_num, MYX_TE_COLUMN *columns)
{
  add_tabledef(self, columns_num, columns);
}


static void xml_exporter_table_header(MYX_TABLE_EXPORTER_INFO *self)
{
  MYX_XML_EXPORTER_INFO* xi= (MYX_XML_EXPORTER_INFO*)self->priv;

  self->current_level++;
  
  // output dtd
  if (!xi->dtd_written && atoi(self->option_values[0]))
  {
    FILE *file= myx_fopen(xi->dtd_filename, "w+");

    xi->dtd_written= 1;
    
	// this is a "simple DTD" solution of #11830
	fprintf(file, "<!ELEMENT field (#PCDATA) >\n<!ATTLIST field name CDATA #REQUIRED >\n<!ELEMENT row (field)+ >\n<!ELEMENT ROOT (row)+ >");

	/*	
    fprintf(file, "<!ELEMENT ROOT (%s)+>\n", self->option_values[1]);

    fprintf(file, "\t<!ELEMENT %s (", self->option_values[1]);
    for (i= 0; i < self->columns_num[0]; i++)
    {
      if (i > 0)
        fprintf(file, ",%s", self->columns[0][i].name);
      else
        fprintf(file, "%s", self->columns[0][i].name);
    }
    fprintf(file, ")>\n");
    for (i= 0; i < self->columns_num[0]; i++)
    {
      char *s= escape_xml_entities(self->columns[0][i].name);

      fprintf(file, "\t\t<!ELEMENT %s (#PCDATA)>\n", s);

      g_free(s);
    }
	*/
    fclose(file);
	
  }
  
  if (self->current_level > 1)
  {
    fprintf(self->file_stream, "%*s<items>\n", self->current_level*4, "");
  }
}


static void xml_exporter_columns(MYX_TABLE_EXPORTER_INFO *self, const char *const*text)
{
  unsigned int i;
  MYX_TE_COLUMN *columns= self->columns[self->current_level-1];

  fprintf(self->file_stream, "%*s<%s>\n", self->current_level*4, "", self->option_values[1]);
  for (i= 0; i < self->columns_num[self->current_level-1]; i++)
  {
    char *n= escape_xml_entities(columns[i].name);
    if(text[i])
    {
      char *s= escape_xml_entities(text[i]);

      fprintf(self->file_stream, "%*s<field name=\"%s\">%s</field>\n", self->current_level*4+2, "",
              n, s?s:"");

      g_free(s);
    }
    else
      fprintf(self->file_stream, "%*s<field name=\"%s\" xsi:nil=\"true\" />\n", self->current_level*4+2, "",
              n);
    g_free(n);
  }

  if (self->table_levels > 1 && self->current_level == 1)
    ;
  else
    fprintf(self->file_stream, "%*s</%s>\n", self->current_level*4, "",
            self->option_values[1]);
}


static void xml_exporter_table_footer(MYX_TABLE_EXPORTER_INFO *self)
{
  if (self->current_level > 1)
  {
    fprintf(self->file_stream, "%*s</items>\n", self->current_level*4, "");
  }
  if (self->table_levels > 1 && self->current_level > 1)
    fprintf(self->file_stream, "%*s</%s>\n", (self->current_level-1)*4, "",
            self->option_values[1]);

  self->current_level--;
}


static void xml_exporter_end(MYX_TABLE_EXPORTER_INFO *self)
{
  fprintf(self->file_stream, "</ROOT>\n");
}


// ------------------------------------------------------------
// Excel exporter
// ------------------------------------------------------------


typedef struct {
  int dummy;
} MYX_EXCEL_EXPORTER_INFO;


static MYX_TABLE_EXPORTER_INFO *excel_exporter_init();
static void excel_exporter_free(MYX_TABLE_EXPORTER_INFO *info);
static int excel_exporter_setup(MYX_TABLE_EXPORTER_INFO *self, const char *filename);
static void excel_exporter_begin(MYX_TABLE_EXPORTER_INFO *self, const char *text);
static void excel_exporter_table_setup(MYX_TABLE_EXPORTER_INFO *self, unsigned int columns_num, MYX_TE_COLUMN *columns);
static void excel_exporter_table_header(MYX_TABLE_EXPORTER_INFO *self);
static void excel_exporter_columns(MYX_TABLE_EXPORTER_INFO *self, const char *const*text);
static void excel_exporter_table_footer(MYX_TABLE_EXPORTER_INFO *self);
static void excel_exporter_end(MYX_TABLE_EXPORTER_INFO *self);

static MYX_TE_OPTION excel_exporter_options[]=
{
  {NULL, NULL, NULL, 0}
};

MYX_TABLE_EXPORTER EXCEL_TABLE_EXPORTER= 
{
  "Excel",
  "Generates an Excel XML File (Excel XP and higher)",
  "Excel (XP or higher) XML File",
  "xls",
  excel_exporter_init,
  excel_exporter_free,
  excel_exporter_setup,
  excel_exporter_begin,
  excel_exporter_table_setup,
  excel_exporter_table_header,
  excel_exporter_columns,
  excel_exporter_table_footer,
  excel_exporter_end,

  excel_exporter_options
};


static MYX_TABLE_EXPORTER_INFO *excel_exporter_init()
{
  MYX_TABLE_EXPORTER_INFO *info;
  
  info= make_exporter_info(&EXCEL_TABLE_EXPORTER);

  info->priv= g_new0(MYX_EXCEL_EXPORTER_INFO, 1);

  return info;
}


static void excel_exporter_free(MYX_TABLE_EXPORTER_INFO *self)
{
  g_free(self->priv);
  
  free_exporter_info(self);
}


static int excel_exporter_setup(MYX_TABLE_EXPORTER_INFO *self, const char *filename)
{
  self->file_stream= myx_fopen(filename, "w+");
  if (!self->file_stream)
    return -1;

  return 0;
}


static void excel_exporter_begin(MYX_TABLE_EXPORTER_INFO *self, const char *text)
{
  fprintf(self->file_stream, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  fprintf(self->file_stream, "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"\n"
          " xmlns:x=\"urn:schemas-microsoft-com:office:excel\"\n"
          " xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\"\n"
          " xmlns:html=\"http://www.w3.org/TR/REC-html40\">\n");
}


static void excel_exporter_table_setup(MYX_TABLE_EXPORTER_INFO *self, unsigned int columns_num, MYX_TE_COLUMN *columns)
{
  add_tabledef(self, columns_num, columns);
  fprintf(self->file_stream, " <Worksheet ss:Name=\"Table1\">\n");
}


static void excel_exporter_table_header(MYX_TABLE_EXPORTER_INFO *self)
{
  unsigned int i;

  fprintf(self->file_stream, "  <Table>\n");

  //Set width for text columns
  for(i= 0; i<self->columns_num[0]; i++)
    if(self->columns[0][i].ctype == MYX_TE_COL_STRING || self->columns[0][i].ctype == MYX_TE_COL_DATE)
    {
      fprintf(self->file_stream,"   <Column ss:Index=\"%d\" ss:AutoFitWidth=\"0\" ss:Width=\"110\"/>", i+1);
    }

  fprintf(self->file_stream, "   <Row>\n");

  //Add column headers
  for(i= 0; i<self->columns_num[0]; i++)
  {
    char *s= escape_xml_entities(self->columns[0][i].name);

    fprintf(self->file_stream,"    <Cell><Data ss:Type=\"String\">%s</Data></Cell>\n", 
      s);

    g_free(s);
  }

  fprintf(self->file_stream, "   </Row>\n");
}


static void excel_exporter_columns(MYX_TABLE_EXPORTER_INFO *self, const char *const*text)
{
  unsigned int i;
  static char *excel_types[]= {
    "String",//MYX_TE_COL_STRING
    "Number",//MYX_TE_COL_INTEGER
    "Number",//MYX_TE_COL_FLOAT
    "Number"//MYX_TE_COL_BOOL,
    "String",//MYX_TE_COL_DATE
  };
  
  fprintf(self->file_stream, "   <Row>\n");
  for (i= 0; i < self->columns_num[0]; i++)
  {
    if(text[i])
    {
      char *s= escape_xml_entities(text[i]);
      if (s == NULL)
        s = "";

      fprintf(self->file_stream, "    <Cell><Data ss:Type=\"%s\">%s</Data></Cell>\n", 
        excel_types[self->columns[0][i].ctype], s);

      g_free(s);
    }
    else
      fprintf(self->file_stream, "    <Cell><Data ss:Type=\"String\"/></Cell>\n");

  }
  fprintf(self->file_stream, "   </Row>\n");
}


static void excel_exporter_table_footer(MYX_TABLE_EXPORTER_INFO *self)
{
  fprintf(self->file_stream, "  </Table>\n");
}


static void excel_exporter_end(MYX_TABLE_EXPORTER_INFO *self)
{
  fprintf(self->file_stream, " </Worksheet>\n");
  fprintf(self->file_stream, "</Workbook>\n");
}


// ------------------------------------------------------------
// MacOS PropertyList exporter
// ------------------------------------------------------------


static MYX_TABLE_EXPORTER_INFO *plist_exporter_init();
static void plist_exporter_free(MYX_TABLE_EXPORTER_INFO *info);
static int plist_exporter_setup(MYX_TABLE_EXPORTER_INFO *self, const char *filename);
static void plist_exporter_begin(MYX_TABLE_EXPORTER_INFO *self, const char *text);
static void plist_exporter_table_setup(MYX_TABLE_EXPORTER_INFO *self, unsigned int columns_num, MYX_TE_COLUMN *columns);
static void plist_exporter_table_header(MYX_TABLE_EXPORTER_INFO *self);
static void plist_exporter_columns(MYX_TABLE_EXPORTER_INFO *self, const char *const*text);
static void plist_exporter_table_footer(MYX_TABLE_EXPORTER_INFO *self);
static void plist_exporter_end(MYX_TABLE_EXPORTER_INFO *self);

static MYX_TE_OPTION plist_exporter_options[]= 
{
  {"Old style proplist format", "0", "Use the old, NEXTSTEP style property list file format.", MYX_TEOP_BOOL},
  {NULL, NULL, NULL, 0}
};


MYX_TABLE_EXPORTER PLIST_TABLE_EXPORTER= 
{
  "PLIST",
  "Generates a Property List File",
  "PropertyList File",
  "plist",
  plist_exporter_init,
  plist_exporter_free,
  plist_exporter_setup,
  plist_exporter_begin,
  plist_exporter_table_setup,
  plist_exporter_table_header,
  plist_exporter_columns,
  plist_exporter_table_footer,
  plist_exporter_end,
  
  plist_exporter_options
};


static MYX_TABLE_EXPORTER_INFO *plist_exporter_init()
{
  MYX_TABLE_EXPORTER_INFO *info;
  
  info= make_exporter_info(&PLIST_TABLE_EXPORTER);
    
  return info;
}


static void plist_exporter_free(MYX_TABLE_EXPORTER_INFO *self)
{
  free_exporter_info(self);
}


static int plist_exporter_setup(MYX_TABLE_EXPORTER_INFO *self, const char *filename)
{
  self->file_stream= myx_fopen(filename, "w+");
  if (!self->file_stream)
    return -1;
  
  return 0;
}


static void plist_exporter_begin(MYX_TABLE_EXPORTER_INFO *self, const char *text)
{
  fprintf(self->file_stream, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  fprintf(self->file_stream, "<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n");
  fprintf(self->file_stream, "<plist version=\"1.0\">\n");
}


static void plist_exporter_table_setup(MYX_TABLE_EXPORTER_INFO *self, unsigned int columns_num, MYX_TE_COLUMN *columns)
{
  add_tabledef(self, columns_num, columns);
}


static void plist_exporter_table_header(MYX_TABLE_EXPORTER_INFO *self)
{
  fprintf(self->file_stream, "<array>\n");
}


static void plist_exporter_columns(MYX_TABLE_EXPORTER_INFO *self, const char *const*text)
{
  unsigned int i;
  MYX_TE_COLUMN *columns= self->columns[0];
  
  fprintf(self->file_stream, "    <dict>\n");
  for (i= 0; i < self->columns_num[0]; i++)
  {
    if(text[i])
    {
      char *s= escape_xml_entities(text[i]);
      fprintf(self->file_stream, "        <key>%s</key>\n", columns[i].name);      
      switch (columns[i].ctype)
      {
        case MYX_TE_COL_STRING:   
          fprintf(self->file_stream, "        <string>%s</string>\n", s); 
          break;
        case MYX_TE_COL_INTEGER:  
          fprintf(self->file_stream, "        <integer>%s</integer>\n", s);
          break;
        case MYX_TE_COL_FLOAT:
          fprintf(self->file_stream, "        <real>%s</real>\n", s);
          break;
        case MYX_TE_COL_BOOL:
          fprintf(self->file_stream, "        <%s/>\n", 
                  ((strcasecmp(s,"true")==0 || strcasecmp(s, "1")==0)) ? "true" : "false");
          break;
        case MYX_TE_COL_DATE:
          fprintf(self->file_stream, "        <date>%s</date>\n", s);
          break;
      }      
      g_free(s);
    }
  }
  
  fprintf(self->file_stream, "    </dict>\n");
}


static void plist_exporter_table_footer(MYX_TABLE_EXPORTER_INFO *self)
{
  fprintf(self->file_stream, "</array>\n");
}


static void plist_exporter_end(MYX_TABLE_EXPORTER_INFO *self)
{
  fprintf(self->file_stream, "</plist>\n");
}

// ============================================================

static MYX_TABLE_EXPORTER *exporters[]= {
  &CSV_TABLE_EXPORTER,
  &HTML_TABLE_EXPORTER,
  &XML_TABLE_EXPORTER,
  &EXCEL_TABLE_EXPORTER,
  &PLIST_TABLE_EXPORTER
};


MYX_TABLE_EXPORTER *myx_get_table_exporter(const char *format)
{
  unsigned int i;
  for (i= 0; i < sizeof(exporters)/sizeof(MYX_TABLE_EXPORTER*); i++)
    if (strcmp(format, exporters[i]->name)==0)
      return exporters[i];
  return NULL;
}


int myx_set_table_exporter_option(MYX_TABLE_EXPORTER_INFO *info,
                                  const char *name,
                                  const char *value)
{
  int i;
  for (i= 0; info->te->options[i].name; i++)
  {
    if (strcmp(name, info->te->options[i].name)==0)
    {
      g_free(info->option_values[i]);
      info->option_values[i]= g_strdup(value);
      return 0;
    }
  }
  return -1;
}


MYX_TABLE_EXPORTER_INFO *myx_get_table_exporter_info(const char *format)
{
  const MYX_TABLE_EXPORTER *exporter= myx_get_table_exporter(format);
  
  if (!exporter)
    return NULL;
  
  return (*exporter->init)();
}


int myx_free_table_exporter_info(MYX_TABLE_EXPORTER_INFO *info)
{  
  (*info->te->free)(info);
  return 0;
}


MYX_STRINGLIST *myx_get_table_export_formats()
{
  MYX_STRINGLIST *sl= g_new0(MYX_STRINGLIST, 1);
  unsigned int i;

  sl->strings_num= sizeof(exporters)/sizeof(MYX_TABLE_EXPORTER*);
  sl->strings= g_new0(char*,sl->strings_num);
  for (i= 0; i < sl->strings_num; i++)
    sl->strings[i]= g_strdup(exporters[i]->name);

  return sl;
}


