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

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/valid.h>

#include <assert.h>
#include "myx_xml_util_functions.h"



/*
* Forward declarations
*/
static int compare_version(const char *ver1, const char *ver2);
static int check_version(const xmlNodePtr node, const char* desired_version);
static int check_platform(const xmlNodePtr node, enum myx_platform platform);
static void read_in_page(const xmlNodePtr node, MYX_GUI_PAGE *page,const char *version,enum myx_platform platform);
static void read_in_group(const xmlNodePtr node, MYX_GUI_GROUP *group,const char *version,enum myx_platform platform);
static void read_in_widget(const xmlNodePtr node, MYX_GUI_WIDGET *widget,const char *version,enum myx_platform platform);
static void read_in_checkbox(const xmlNodePtr node, MYX_GUI_WIDGET *widget,const char *version,enum myx_platform platform);
static void read_in_spinedit(const xmlNodePtr node, MYX_GUI_WIDGET *widget,const char *version,enum myx_platform platform);
static void read_in_textedit(const xmlNodePtr node, MYX_GUI_WIDGET *widget,const char *version,enum myx_platform platform);
static void read_in_dropdownbox(const xmlNodePtr node,MYX_GUI_WIDGET *widget,const char *version,enum myx_platform platform);

static void free_gui_page_content(MYX_GUI_PAGE *page);
static void free_group_content(MYX_GUI_GROUP *group);
static void free_widget_content(MYX_GUI_WIDGET *widget);
static void free_checkbox(MYX_GUI_CHECKBOX *checkbox);
static void free_spinedit( MYX_GUI_SPINEDIT *spinedit);
static void free_textedit(MYX_GUI_TEXTEDIT *textedit);
static void free_dropdownbox(MYX_GUI_DROPDOWNBOX *dropdownbox);

static void sort_pages_in_description(MYX_GUI_DESCRIPTION *gui_description);
static void sort_groups_in_page(MYX_GUI_PAGE *page);
static void sort_widgets_in_group(MYX_GUI_GROUP *group);

/*
* Public functions
*/

/*
int main()
{
enum myx_lib_error error_code;
MYX_GUI_DESCRIPTION *g = myx_get_gui_description("../xmlfiles/startup_parameters_description.xml", 4.0, MYX_LINUX , &error_code);


myx_free_gui_description(g);
return 0;
} */


/**
* loads the startup parameters layout and default values from an XML file. 
* We will validate the xml-file against the dtd.
* Platform and version will be used to read in only the correct elements.
**/
MYX_GUI_DESCRIPTION *myx_get_gui_description(const char *filename,
                                             const char *version, 
                                             MYX_PLATFORM platform, 
                                             MYX_ADMIN_LIB_ERROR *error_code, 
                                             const char *ini_filepath, 
                                             const char *ini_file_group)
{
  xmlDocPtr doc;
  xmlParserCtxtPtr ctxt;
  xmlNodePtr root,cur;
  MYX_GUI_DESCRIPTION *gui_description;
  int i;
  char *local_filename;

  *error_code = MYX_NO_ERROR;

  if (! file_exists(filename) )
  {
    *error_code = MYX_ERROR_CANT_OPEN_FILE;
    return NULL;
  }

  /* create a parser context */
  ctxt = xmlNewParserCtxt();
  if (ctxt == NULL) 
  {
    *error_code = MYX_ADMIN_XML_PARSE_ERROR;
    xmlFreeParserCtxt(ctxt);
    return NULL;
  }
  /* parse the file, activating the DTD validation option */
  local_filename= g_filename_from_utf8(filename,-1,NULL,NULL,NULL);
  if (local_filename == NULL)
    return NULL;
  doc = xmlCtxtReadFile(ctxt, local_filename, NULL, XML_PARSE_DTDVALID);
  g_free(local_filename);
  if (doc == NULL) 
  {
    *error_code = MYX_ADMIN_XML_PARSE_ERROR;
    xmlFreeParserCtxt(ctxt);
    return NULL;
  } 
  else 
  {
    /* check if validation suceeded */
    if (ctxt->valid == 0)
    {
      *error_code = MYX_XML_NO_VALID_DOCUMENT;
      xmlFreeParserCtxt(ctxt);
      xmlFreeDoc(doc);
      return NULL;
    }
  }

  xmlFreeParserCtxt(ctxt);

  root = xmlDocGetRootElement(doc);

  if (root == NULL) {
    *error_code = MYX_XML_EMPTY_DOCUMENT;
    xmlFreeDoc(doc);
    return NULL;
  }

  gui_description = calloc(1,sizeof(MYX_GUI_DESCRIPTION) );

  gui_description->pages_num = get_child_count(root,"page");

  if (gui_description->pages_num  > 0)
    gui_description->pages = calloc(gui_description->pages_num,sizeof(MYX_GUI_PAGE));

  /*read all pages*/
  i=0;
  cur = root->children;
  while (cur != NULL) 
  {
    /* read a single page*/
    if (!xmlStrcmp(cur->name, "page"))
    {
      read_in_page(cur, gui_description->pages + i, version, platform);
      i++;
    }

    cur = cur->next;
  }


  xmlFreeDoc(doc);

  sort_pages_in_description(gui_description);

  /* also fill this struct with values from the cnf-file
  * right away, since under Windows structs returned
  * by this library may only be changed here.
  */
  myx_init_gui_description_with_default_values(gui_description);

  //Delphi passes a '' instead of NULL
  if(ini_filepath && *ini_filepath)
  {
    myx_process_mysql_cnf_file(gui_description, ini_filepath, ini_file_group, error_code);
  }

  return gui_description;    
}

MYX_PUBLIC_FUNC int myx_free_gui_description(MYX_GUI_DESCRIPTION *gui_desc)
{
  unsigned int i;

  if (gui_desc)
  {
    for (i=0; i < gui_desc->pages_num; i++)
    {
      free_gui_page_content(gui_desc->pages+i);
    }
    free(gui_desc->pages);
    free(gui_desc);
  }

  return 0;
}



/*
* Private functions
*/ 

/* sort functions */
static int compare_pages(const void *a, const void *b)
{
  MYX_GUI_PAGE *t1 = ( MYX_GUI_PAGE *) a;
  MYX_GUI_PAGE *t2 = ( MYX_GUI_PAGE *) b;

  if (t1->position < t2->position)
  {
    return -1;
  }
  else if (t1->position > t2->position)
  {
    return 1;
  }
  else return 0;
}

static void sort_pages_in_description(MYX_GUI_DESCRIPTION *gui_description)
{
  unsigned int i;

  if (gui_description->pages)
    qsort(gui_description->pages, gui_description->pages_num, sizeof(MYX_GUI_PAGE), compare_pages);

  for (i=0; i< gui_description->pages_num; i++)
  {
    sort_groups_in_page(gui_description->pages+i);
  }
}


static int compare_groups(const void *a, const void *b)
{
  MYX_GUI_GROUP *t1 = ( MYX_GUI_GROUP *) a;
  MYX_GUI_GROUP *t2 = ( MYX_GUI_GROUP *) b;

  if (t1->position < t2->position)
  {
    return -1;
  }
  else if (t1->position > t2->position)
  {
    return 1;
  }
  else return 0;
}

static void sort_groups_in_page(MYX_GUI_PAGE *page)
{
  unsigned int i;

  if (page->groups)
    qsort(page->groups, page->groups_num, sizeof( MYX_GUI_GROUP), compare_groups);

  for (i=0; i< page->groups_num; i++)
  {
    sort_widgets_in_group(page->groups+i);
  }
}

static int compare_widgets(const void *a, const void *b)
{
  MYX_GUI_WIDGET *t1 = ( MYX_GUI_WIDGET *) a;
  MYX_GUI_WIDGET *t2 = ( MYX_GUI_WIDGET *) b;

  if (t1->position < t2->position)
  {
    return -1;
  }
  else if (t1->position > t2->position)
  {
    return 1;
  }
  else return 0;
}

static void sort_widgets_in_group(MYX_GUI_GROUP *group)
{
  if (group->widgets)
    qsort(group->widgets, group->widgets_num, sizeof(MYX_GUI_WIDGET), compare_widgets);
}




/* free helper functions */
static void free_gui_page_content(MYX_GUI_PAGE *page)
{
  unsigned int i;

  xmlFree(page->caption);
  xmlFree(page->description);
  xmlFree(page->id);

  for (i=0; i < page->groups_num; i++)
  {
    free_group_content(page->groups +i);
  }
  free(page->groups);
}

static void free_group_content(MYX_GUI_GROUP *group)
{
  unsigned int i;

  xmlFree(group->caption);
  xmlFree(group->id);

  for (i=0; i < group->widgets_num; i++)
  {
    free_widget_content(group->widgets+i);
  }
  free(group->widgets);
}

static void free_widget_content(MYX_GUI_WIDGET *widget)
{ 
  unsigned int i;

  for (i=0; i< widget->alt_names_num; i++)
  {
    xmlFree(widget->alt_names[i]);
  }
  if (widget->alt_names)
    free(widget->alt_names);

  xmlFree(widget->id);
  xmlFree(widget->caption);
  xmlFree(widget->description);
  xmlFree(widget->default_value);
  if (widget->value)
    xmlFree(widget->value);

  if (widget->checkbox)
    free_checkbox(widget->checkbox);
  if (widget->spinedit)
    free_spinedit(widget->spinedit);
  if (widget->textedit)
    free_textedit(widget->textedit);
  if (widget->dropdownbox)
    free_dropdownbox(widget->dropdownbox);
}

static void free_checkbox(MYX_GUI_CHECKBOX *checkbox)
{
}
static void free_spinedit( MYX_GUI_SPINEDIT *spinedit)
{
  if (spinedit)
  {
    xmlFree(spinedit->unitcontrolbox); 
    free(spinedit);
  }
}

static void free_textedit(MYX_GUI_TEXTEDIT *textedit)
{
  if (textedit)
  {
    xmlFree(textedit->edit_type); 
    free(textedit);
  }
}

static void free_dropdownbox(MYX_GUI_DROPDOWNBOX *dropdownbox)
{
  unsigned int i;

  if (dropdownbox)
  {
    for (i=0; i < dropdownbox->items_num; i++)
    {
      xmlFree(dropdownbox->items[i]);
    }
    free(dropdownbox->items);
    free(dropdownbox);
  }
}


/*
static void free_textgrid_button_content(MYX_GUI_BUTTON *b)
{
}*/

/*Notice: all the read_in_* functions are responsible for reading in a specified xml-element into our structures
The parameter node always points to the tree-element corresponding to this xml-element
*/

/*
About validation: If validation is turned off, and we really get a bad file we should not crash, but other
non-fatal bad things like memory leaks etc might occur.
*/

/*
Reads in the subtree of a <page> element
*/
static void read_in_page(const xmlNodePtr node, MYX_GUI_PAGE *page,const char *version,enum myx_platform platform)
{
  xmlNodePtr cur;
  int i;
  xmlDocPtr doc = node->doc;

  page->id = xmlGetProp(node, "id");
  page->position = atoi_and_free(xmlGetProp(node, "position"));

  /* count the groups */
  page->groups_num = 0;
  cur = node->children;
  while (cur)
  {        
    if ( (!xmlStrcmp(cur->name, "group")) & (check_version(cur, version)) )
      page->groups_num++;

    cur = cur->next;
  }

  if (page->groups_num > 0)
    page->groups = calloc(page->groups_num, sizeof(MYX_GUI_GROUP));

  i=0;
  cur = node->children;
  while (cur)
  {
    if ( !xmlStrcmp(cur->name, "caption") )
    {
      page->caption = xmlNodeListGetString(doc, cur->children, 1);
    }
    else if ( !xmlStrcmp(cur->name, "description") )
    {
      page->description = xmlNodeListGetString(doc, cur->children, 1);
    }
    else if ( !xmlStrcmp(cur->name, "group") )
    {
      if (check_version(cur, version) )
      {
        read_in_group(cur, page->groups +i, version, platform);
        i++;
      }
    }


    cur = cur->next;
  }    
}


static void read_in_group(const xmlNodePtr node, MYX_GUI_GROUP *group,const char *version,enum myx_platform platform)
{
  xmlNodePtr cur;
  int i;
  xmlDocPtr doc = node->doc;


  group->id = xmlGetProp(node, "id");
  group->position = atoi_and_free(xmlGetProp(node, "position"));

  /*how many widgets do we have?*/
  group->widgets_num = 0; 
  cur = node->children;
  while (cur)
  {
    if ( !xmlStrcmp(cur->name, "checkbox") || !xmlStrcmp(cur->name, "spinedit")  || !xmlStrcmp(cur->name, "textedit")  || !xmlStrcmp(cur->name, "dropdownbox") 
      || !xmlStrcmp(cur->name, "memo")  || !xmlStrcmp(cur->name, "textgrid") || !xmlStrcmp(cur->name, "fileselectiondialog") )
    {
      if ( check_version(cur,version) && check_platform(cur, platform) )
      {
        group->widgets_num++;
      }
    } 
    cur = cur->next;
  }
  if (group->widgets_num > 0)
    group->widgets = calloc(group->widgets_num, sizeof(MYX_GUI_WIDGET));

  i=0;
  cur = node->children;
  while (cur)
  {
    if ( !xmlStrcmp(cur->name, "caption") )
    {
      group->caption = xmlNodeListGetString(doc, cur->children, 1);
    } 
    else if ( !xmlStrcmp(cur->name, "checkbox") )
    {
      if ( check_version(cur,version) && check_platform(cur, platform) )
      {
        read_in_checkbox(cur, group->widgets+i, version, platform);
        i++;
      }
    }
    else if ( !xmlStrcmp(cur->name, "spinedit") )
    {
      if ( check_version(cur,version) && check_platform(cur, platform) )
      {
        read_in_spinedit(cur, group->widgets+i, version, platform);
        i++;
      }
    }
    else if ( !xmlStrcmp(cur->name, "textedit") )
    {
      if ( check_version(cur,version) && check_platform(cur, platform) )
      {
        read_in_textedit(cur, group->widgets+i, version, platform);
        i++;
      }
    }
    else if ( !xmlStrcmp(cur->name, "dropdownbox") )
    {
      if ( check_version(cur,version) && check_platform(cur, platform) )
      {
        read_in_dropdownbox(cur, group->widgets+i, version, platform);
        i++;
      }
    }
    cur = cur->next;
  } 
}


static void read_in_widget(const xmlNodePtr node, MYX_GUI_WIDGET *widget, const char *version,enum myx_platform platform)
{
  xmlNodePtr cur;
  xmlChar *platform_tmp;
  char *cur_version = NULL;
  int version_found = 0;
  int platform_found =0;
  xmlChar *desired_platform, *tmp;
  xmlDocPtr doc = node->doc;

  if (platform == MYX_WINDOWS)
    desired_platform = "windows";
  else if (platform == MYX_LINUX)
    desired_platform = "linux";
  else if (platform == MYX_MACOS) 
    desired_platform = "macos";

  widget->id= xmlGetProp(node, "id");
  widget->position= atoi_and_free(xmlGetProp(node, "position"));

  tmp= xmlGetProp(node, "multiple");
  if (tmp && !xmlStrcmp(tmp, "yes"))
    widget->multiple= 1;
  else
    widget->multiple= 0;
  xmlFree(tmp);

  cur=node->children;
  while (cur)
  {
    if ( !xmlStrcmp(cur->name, "caption") )
    {
      widget->caption = xmlNodeListGetString(doc, cur->children, 1);
    }         
    else if ( !xmlStrcmp(cur->name, "default_value") )
    {
      xmlChar *version_in_xml;

      platform_tmp = xmlGetProp(cur, "platform");
      if ( (! xmlStrcmp(platform_tmp, desired_platform)) || (!xmlStrcmp(platform_tmp, "all") && platform_found==0) )
      {
        version_in_xml = xmlGetProp(cur,"version");
        if (version_in_xml)
        {
          if (compare_version(version, version_in_xml)==0)
          {
            widget->default_value= xmlNodeListGetString(doc, cur->children, 1);
            widget->value= xmlNodeListGetString(doc, cur->children, 1);
            version_found = 1;
          }
          else if (compare_version(version_in_xml, version)<0 && (!cur_version || compare_version(version_in_xml, cur_version)>0) && (!version_found) )
          {
            widget->default_value= xmlNodeListGetString(doc, cur->children, 1);
            widget->value= xmlNodeListGetString(doc, cur->children, 1);
            cur_version = g_strdup(version);
          }
          xmlFree(version_in_xml);
        }
      }

      xmlFree(platform_tmp);

    }        
    else if ( !xmlStrcmp(cur->name, "description") )
    {
      widget->description = xmlNodeListGetString(doc, cur->children, 1);
    } 
    else if ( !xmlStrcmp(cur->name, "alt_name") )
    {
      widget->alt_names_num++;
      widget->alt_names = realloc(widget->alt_names, sizeof(char*) * widget->alt_names_num);
      widget->alt_names[widget->alt_names_num-1]= xmlNodeListGetString(doc, cur->children, 1);
    } 

    cur = cur->next;
  }
}

static void read_in_checkbox(const xmlNodePtr node, MYX_GUI_WIDGET *widget,const char *version,enum myx_platform platform)
{
  xmlChar *tmp;

  widget->widget_type = MYX_CHECKBOX;

  read_in_widget(node, widget, version, platform);

  widget->checkbox= calloc(1, sizeof(MYX_GUI_CHECKBOX) );

  tmp= xmlGetProp(node, "is_boolean");
  if (tmp && !xmlStrcmp(tmp, "yes"))
    widget->checkbox->is_boolean= 1;
  else
    widget->checkbox->is_boolean= 0;
  xmlFree(tmp);

  tmp= xmlGetProp(node, "invert");
  if (tmp && !xmlStrcmp(tmp, "yes"))
    widget->checkbox->invert= 1;
  else
    widget->checkbox->invert= 0;
  xmlFree(tmp);


}


static void read_in_spinedit(const xmlNodePtr node, MYX_GUI_WIDGET *widget,const char *version,enum myx_platform platform)
{
  xmlNodePtr cur;

  widget->widget_type = MYX_SPINEDIT;
  read_in_widget(node, widget, version, platform);

  widget->spinedit = calloc(1, sizeof(MYX_GUI_SPINEDIT) );

  cur=node->children;
  while (cur)
  {
    if ( !xmlStrcmp(cur->name, "unitcontrolbox") )
    {
      widget->spinedit->unitcontrolbox = xmlGetProp(cur, "type");
    }

    cur = cur->next;
  }
}

static void read_in_textedit(const xmlNodePtr node,  MYX_GUI_WIDGET *widget,const char *version,enum myx_platform platform)
{
  widget->widget_type = MYX_TEXTEDIT;
  read_in_widget(node, widget, version, platform);

  widget->textedit = calloc(1, sizeof(MYX_GUI_TEXTEDIT) );

  widget->textedit->edit_type = xmlGetProp(node, "type");
}


static void read_in_dropdownbox(const xmlNodePtr node,  MYX_GUI_WIDGET *widget,const char *version,enum myx_platform platform)
{
  xmlNodePtr cur, cur2;
  xmlDocPtr doc = node->doc;
  int j;

  widget->widget_type = MYX_DROPDOWNBOX;
  read_in_widget(node, widget, version, platform);

  widget->dropdownbox = calloc(1, sizeof(MYX_GUI_DROPDOWNBOX) );

  cur=node->children;
  while (cur)
  {
    if ( !xmlStrcmp(cur->name, "items") )
    {
      widget->dropdownbox->items_num = get_child_count(cur,"item");
      if (widget->dropdownbox->items_num > 0)
        widget->dropdownbox->items = calloc(widget->dropdownbox->items_num, sizeof(unsigned char*));

      j=0;
      cur2= cur->children;
      while (cur2)
      {
        if (! xmlStrcmp(cur2->name, "item") )
        {
          widget->dropdownbox->items[j++] = xmlNodeListGetString(doc,cur2->children,1);
        }
        cur2= cur2->next;
      }
    }         


    cur = cur->next;
  }
}


static int compare_version(const char *ver1, const char *ver2)
{
  int maj1=0, min1=0, pl1=0;
  int maj2=0, min2=0, pl2=0;

  sscanf(ver1, "%i.%i.%i", &maj1, &min1, &pl1);
  sscanf(ver2, "%i.%i.%i", &maj2, &min2, &pl2);

  if (maj1 < maj2)
    return -1;
  else if (maj1 == maj2)
  {
    if (min1 < min2)
      return -1;
    else if (min1 == min2)
    {
      if (pl1 < pl2)
        return -1;
      else if (pl1 == pl2)
        return 0;
      else
        return 1;
    }
    else
      return 1;
  }
  else
    return 1;
}


static int check_version(const xmlNodePtr node, const char *version)
{
  xmlChar *version_start = xmlGetProp(node, "version_start");
  xmlChar *version_end = xmlGetProp(node, "version_end");

  if (!version_start && !version_end)
    return 1;
  else if (!version_start)
  {
    if (compare_version(version, version_end) <= 0)
      return 1;
  }
  else if (!version_end)
  {
    if (compare_version(version, version_start) >= 0)
      return 1;
  }
  else if (compare_version(version, version_end) <= 0 &&
    compare_version(version, version_start) >= 0)
    return 1;

  return 0;
}


/* returns true if the widget described by node is valid for our desired
* platform and thus should be read in
*/
static int check_platform(const xmlNodePtr node, enum myx_platform platform)
{
  xmlChar *desired_platform, *widget_platform;
  int result;

  if (platform == MYX_WINDOWS)
    desired_platform = "windows";
  else if (platform == MYX_LINUX)
    desired_platform = "linux";
  else if (platform == MYX_MACOS) 
    desired_platform = "macos";

  widget_platform= xmlGetProp(node, "platform");
  if (widget_platform && xmlStrcmp(widget_platform, desired_platform) && 
    xmlStrcmp(widget_platform, "all") )
    result= 0;
  else
    result= 1;

  xmlFree(widget_platform);
  return result;
}
