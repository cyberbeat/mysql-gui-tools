/*
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
#include "myx_xml_util_functions.h"
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

//#include <assert.h>
#include <stdlib.h>


static void read_in_health_page(const xmlNodePtr node,MYX_HEALTH_PAGE *health_page);
static void read_in_health_group(const xmlNodePtr node, MYX_HEALTH_GROUP *group);
static void read_in_health_graph(const xmlNodePtr node, MYX_HEALTH_GRAPH *health_graph);
static void sort_pages_and_groups_and_graphs(MYX_HEALTH_PAGES *health_pages);

static int compare_pages(const void *a, const void *b);
static int compare_groups(const void *a, const void *b);
static int compare_graphs(const void *a, const void *b);

static void free_page_content(MYX_HEALTH_PAGE *page);
static void free_group_content(MYX_HEALTH_GROUP *group);
static void free_graph_content(MYX_HEALTH_GRAPH *graph);


/*
* Public functions
*/

int myx_free_health_pages(MYX_HEALTH_PAGES *pages)
{
  if (pages)
  {
    unsigned int i;

    for (i=0; i < pages->pages_num; i++)
    {
      free_page_content(pages->pages+i);
    }
    free(pages->pages);
    free(pages);
  }

  return 0;
}

int myx_save_health_pages(MYX_HEALTH_PAGES *pages, const char *filename)
{
  xmlDocPtr doc;
  xmlNodePtr root_node;
  unsigned int i,j,k;

  g_return_val_if_fail(pages, -1);
  g_return_val_if_fail(filename, -1);

  doc = xmlNewDoc("1.0");

  root_node = doc->children = xmlNewDocRawNode(doc,NULL, "health_pages", NULL);  

  for (i=0; i < pages->pages_num; i++)
  {
    MYX_HEALTH_PAGE *health_page;
    xmlNodePtr health_page_node, health_groups_node;

    health_page= pages->pages+i;
    health_page_node= xmlNewTextChild(root_node, NULL, "health_page", NULL);

    xmlNewTextChild(health_page_node, NULL, "caption", health_page->caption); 
    xmlNewTextChild(health_page_node, NULL, "caption_trans_id", health_page->caption_trans_id); 
    xmlNewTextChild(health_page_node, NULL, "description", health_page->description); 
    xmlNewTextChild(health_page_node, NULL, "description_trans_id", health_page->description_trans_id); 
    NewTextChild_int_content(health_page_node, NULL, "pos", health_page->pos); 

    /* save all the groups of a page */
    health_groups_node= xmlNewTextChild(health_page_node, NULL, "health_groups", NULL);

    for (j=0; j < health_page->groups_num; j++)
    {
      MYX_HEALTH_GROUP *health_group;
      xmlNodePtr health_group_node, health_graphs_node;

      health_group= health_page->groups+j;
      health_group_node= xmlNewTextChild(health_groups_node, NULL, "health_group", NULL);

      xmlNewTextChild(health_group_node, NULL, "caption", health_group->caption);
      xmlNewTextChild(health_group_node, NULL, "caption_trans_id", health_group->caption_trans_id);
      NewTextChild_int_content(health_group_node, NULL, "pos", health_group->pos);

      /* save all the graphs of a page */
      health_graphs_node= xmlNewTextChild(health_group_node, NULL, "health_graphs", NULL);

      for (k=0; k < health_group->graphs_num; k++)
      {
        MYX_HEALTH_GRAPH *health_graph;
        xmlNodePtr health_graph_node;

        health_graph= health_group->graphs+k;
        health_graph_node= xmlNewTextChild(health_graphs_node, NULL, "health_graph", NULL);

        xmlNewTextChild(health_graph_node, NULL, "graph_caption", health_graph->graph_caption);
        xmlNewTextChild(health_graph_node, NULL, "value_formula", health_graph->value_formula);
        xmlNewTextChild(health_graph_node, NULL, "max_formula", health_graph->max_formula);
        xmlNewTextChild(health_graph_node, NULL, "value_caption", health_graph->value_caption);
        xmlNewTextChild(health_graph_node, NULL, "value_caption_trans_id", health_graph->value_caption_trans_id);
        xmlNewTextChild(health_graph_node, NULL, "max_caption", health_graph->max_caption);
        xmlNewTextChild(health_graph_node, NULL, "max_caption_trans_id", health_graph->max_caption_trans_id);
        NewTextChild_int_content(health_graph_node, NULL, "display_graph_caption", health_graph->display_graph_caption);
        NewTextChild_int_content(health_graph_node, NULL, "graphtype", health_graph->graphtype);
        NewTextChild_int_content(health_graph_node, NULL, "value_unit", health_graph->value_unit);
        NewTextChild_int_content(health_graph_node, NULL, "autoextend_max", health_graph->autoextend_max);
        NewTextChild_int_content(health_graph_node, NULL, "refreshtime", health_graph->refreshtime);
        NewTextChild_int_content(health_graph_node, NULL, "pos", health_graph->pos);
        NewTextChild_double_content(health_graph_node, NULL, "min", health_graph->min);
        NewTextChild_double_content(health_graph_node, NULL, "max", health_graph->max);
      }
    }
  }

  if (myx_xmlSaveFile(filename, doc) == -1)
  {
    xmlFreeDoc(doc);
    return -1;
  }

  xmlFreeDoc(doc);
  return 0;
}

MYX_HEALTH_PAGES* myx_read_in_health_pages(const char* filename, MYX_ADMIN_LIB_ERROR *error_code)
{
  xmlDocPtr doc;
  xmlNodePtr root;
  MYX_HEALTH_PAGES *health_pages;

  char *local_filename;
  *error_code = MYX_ADMIN_NO_ERROR;

  local_filename= g_filename_from_utf8(filename, -1, NULL, NULL, NULL);
  if (local_filename == NULL)
  {
    *error_code= MYX_CHARSET_CONVERSION_ERROR;
    return NULL;
  };  
  if (! file_exists(local_filename) )
  {
    *error_code = MYX_ADMIN_ERROR_CANT_OPEN_FILE;
    return NULL;
  }

  doc= myx_xmlParseFile(filename);
  if (doc == NULL)
  {
    *error_code = MYX_ADMIN_XML_PARSE_ERROR;
    return NULL;
  }

  root = xmlDocGetRootElement(doc);

  if (root == NULL) 
  {
    *error_code = MYX_ADMIN_XML_EMPTY_DOCUMENT;
    xmlFreeDoc(doc);
    return NULL;
  }

  health_pages= calloc(1, sizeof(MYX_HEALTH_PAGES));

  health_pages->pages_num= get_child_count(root, "health_page");

  if (health_pages->pages_num  > 0)
  {
    int i;
    xmlNodePtr cur;

    health_pages->pages= calloc(health_pages->pages_num, sizeof(MYX_HEALTH_PAGE));

    i=0;
    cur = root->children;
    while (cur != NULL) 
    {
      if (!xmlStrcmp(cur->name, "health_page"))
      {
        read_in_health_page(cur, health_pages->pages+i);
        i++;
      }

      cur = cur->next;
    }
  }

  sort_pages_and_groups_and_graphs(health_pages);

  xmlFreeDoc(doc);

  return health_pages;    
}





/*
* Private functions
*/



static void read_in_health_page(const xmlNodePtr node,MYX_HEALTH_PAGE *health_page)
{
  xmlNodePtr cur;
  xmlDocPtr doc= node->doc;

  cur = node->children;
  while (cur != NULL) 
  {
    if (!xmlStrcmp(cur->name, "caption"))
    {
      health_page->caption = xmlNodeListGetString(doc, cur->children, 1);    
    }
    else if (!xmlStrcmp(cur->name, "caption_trans_id"))
    {
      health_page->caption_trans_id = xmlNodeListGetString(doc, cur->children, 1);    
    }
    else if (!xmlStrcmp(cur->name, "description"))
    {
      health_page->description = xmlNodeListGetString(doc, cur->children, 1);    
    }
    else if (!xmlStrcmp(cur->name, "description_trans_id"))
    {
      health_page->description_trans_id = xmlNodeListGetString(doc, cur->children, 1);    
    }
    else if (!xmlStrcmp(cur->name, "pos"))
    {
      health_page->pos = atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));    
    }
    else if (!xmlStrcmp(cur->name, "health_groups"))
    {
      health_page->groups_num= get_child_count(cur, "health_group");
      if (health_page->groups_num > 0)
      {
        int i=0;
        xmlNodePtr cur2;

        health_page->groups= calloc(health_page->groups_num, sizeof(MYX_HEALTH_GROUP));

        cur2 = cur->children;
        while (cur2 != NULL) 
        {
          if (!xmlStrcmp(cur2->name, "health_group"))
          {
            read_in_health_group(cur2, health_page->groups+i);
            i++;
          }

          cur2 = cur2->next;
        }
      }
    }
    cur= cur->next;
  }

}

static void read_in_health_group(const xmlNodePtr node, MYX_HEALTH_GROUP *group)
{
  xmlNodePtr cur;
  xmlDocPtr doc= node->doc;

  cur = node->children;
  while (cur != NULL) 
  {
    if (!xmlStrcmp(cur->name, "caption"))
    {
      group->caption= xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, "caption_trans_id"))
    {
      group->caption_trans_id= xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, "pos"))
    {
      group->pos= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, "health_graphs"))
    {
      group->graphs_num= get_child_count(cur, "health_graph");
      if (group->graphs_num > 0)
      {
        int i=0;
        xmlNodePtr cur2;

        group->graphs= calloc(group->graphs_num, sizeof(MYX_HEALTH_GRAPH));

        cur2 = cur->children;
        while (cur2 != NULL) 
        {
          if (!xmlStrcmp(cur2->name, "health_graph"))
          {
            read_in_health_graph(cur2, group->graphs+i);
            i++;
          }

          cur2 = cur2->next;
        }
      }
    }

    cur = cur->next;
  }
}

static void read_in_health_graph(const xmlNodePtr node, MYX_HEALTH_GRAPH *health_graph)
{
  xmlNodePtr cur;
  xmlDocPtr doc= node->doc;

  cur = node->children;
  while (cur != NULL) 
  {
    if (!xmlStrcmp(cur->name, "graph_caption"))
    {
      health_graph->graph_caption= xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, "display_graph_caption"))
    {
      health_graph->display_graph_caption= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, "graphtype"))
    {
      health_graph->graphtype= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, "value_unit"))
    {
      health_graph->value_unit= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, "min"))
    {
      health_graph->min= atof_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, "max"))
    {
      health_graph->max= atof_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, "autoextend_max"))
    {
      health_graph->autoextend_max= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, "value_formula"))
    {
      health_graph->value_formula= xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, "max_formula"))
    {
      health_graph->max_formula= xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, "value_caption"))
    {
      health_graph->value_caption= xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, "value_caption_trans_id"))
    {
      health_graph->value_caption_trans_id= xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, "max_caption"))
    {
      health_graph->max_caption= xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, "max_caption_trans_id"))
    {
      health_graph->max_caption_trans_id= xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, "refreshtime"))
    {
      health_graph->refreshtime= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, "pos"))
    {
      health_graph->pos= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }

    cur= cur->next;
  }
}






/* Functions for sorting */
static int compare_pages(const void *a, const void *b)
{
  MYX_HEALTH_PAGE *g1 = (MYX_HEALTH_PAGE *) a;
  MYX_HEALTH_PAGE *g2 = (MYX_HEALTH_PAGE *) b;

  if (g1->pos < g2->pos)
  {
    return -1;
  }
  else if (g1->pos > g2->pos)
  {
    return 1;
  }
  else
  {
    return 0;
  }
}

static int compare_groups(const void *a, const void *b)
{
  MYX_HEALTH_GROUP *g1 = (MYX_HEALTH_GROUP *) a;
  MYX_HEALTH_GROUP *g2 = (MYX_HEALTH_GROUP *) b;

  if (g1->pos < g2->pos)
  {
    return -1;
  }
  else if (g1->pos > g2->pos)
  {
    return 1;
  }
  else
  {
    return 0;
  }
}


static int compare_graphs(const void *a, const void *b)
{
  MYX_HEALTH_GRAPH *g1 = (MYX_HEALTH_GRAPH *) a;
  MYX_HEALTH_GRAPH *g2 = (MYX_HEALTH_GRAPH *) b;

  if (g1->pos < g2->pos)
  {
    return -1;
  }
  else if (g1->pos > g2->pos)
  {
    return 1;
  }
  else
  {
    return 0;
  }
}

static void sort_pages_and_groups_and_graphs(MYX_HEALTH_PAGES *health_pages)
{
  unsigned int i;

  /* sort all pages */
  qsort(health_pages->pages, health_pages->pages_num, sizeof(MYX_HEALTH_PAGE), compare_pages);

  for (i=0; i < health_pages->pages_num; i++)
  {
    MYX_HEALTH_PAGE *page;
    unsigned int j;

    page= health_pages->pages+i;

    /* sort all groups in this page */
    qsort(page->groups, page->groups_num, sizeof(MYX_HEALTH_GROUP), compare_groups);

    for (j=0; j < page->groups_num; j++)
    {
      MYX_HEALTH_GROUP *group;
      group= page->groups + j;

      /* sort all graphs in this group */
      qsort(group->graphs, group->graphs_num, sizeof(MYX_HEALTH_GRAPH), compare_graphs);
    }
  }
}


/* free functions */

static void free_page_content(MYX_HEALTH_PAGE *page)
{
  unsigned int i;

  xmlFree(page->caption);
  xmlFree(page->caption_trans_id);
  xmlFree(page->description);
  xmlFree(page->description_trans_id);

  for (i=0; i < page->groups_num; i++)
  {
    free_group_content(page->groups + i);
  }
  free(page->groups);
}

static void free_group_content(MYX_HEALTH_GROUP *group)
{
  unsigned int i;

  xmlFree(group->caption);
  xmlFree(group->caption_trans_id);

  for(i=0; i< group->graphs_num; i++)
  {
    free_graph_content(group->graphs + i);
  }
  free(group->graphs);
}

static void free_graph_content(MYX_HEALTH_GRAPH *graph)
{
  xmlFree(graph->graph_caption);
  xmlFree(graph->value_formula);
  xmlFree(graph->max_formula);
  xmlFree(graph->value_caption);
  xmlFree(graph->value_caption_trans_id);
  xmlFree(graph->max_caption);
  xmlFree(graph->max_caption_trans_id);
}
