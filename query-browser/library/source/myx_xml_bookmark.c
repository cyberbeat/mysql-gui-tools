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


#include <myx_qb_library.h>

#include <glib.h>

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>

#include <assert.h>
#include <myx_xml_util_functions.h>
#include <string.h>

/*
 * Macros
 */

// max valid xml option file size
#define MAX_VALID_OPTION_FILE_SIZE  (512*1024)

/* Forward declarations */
static void read_in_bookmark_group(xmlNodePtr node, MYX_BOOKMARK_GROUP *bookmark_group);
static void read_in_bookmark(xmlNodePtr node, MYX_BOOKMARK *bookmark);
static void free_bookmark_group_content(MYX_BOOKMARK_GROUP *group);
static void free_bookmark_content(MYX_BOOKMARK *bm);
static void store_bookmark(MYX_BOOKMARK *bookmark, xmlNodePtr parent);
static void store_bookmark_group(MYX_BOOKMARK_GROUP *bookmark_group, xmlNodePtr parent);
static void sort_bookmark_groups_and_bookmarks(MYX_BOOKMARKS *bookmarks);
static int compare_groups(const void *a, const void *b);
static int compare_bookmarks(const void *a, const void *b);
static void sort_bookmark_group(MYX_BOOKMARK_GROUP *groups, unsigned int groups_num);

/*
* Public functions 
*/

MYX_BOOKMARKS * myx_bookmarks_load(const char *filename, MYX_LIB_ERROR *error_code)
{
  xmlDocPtr doc;
  xmlNodePtr root;
  MYX_BOOKMARKS *bookmarks;
  char* local_filename;
  *error_code = MYX_NO_ERROR;

  if ((local_filename= g_filename_from_utf8(filename, -1, NULL, NULL, NULL)) == NULL)
  {
    *error_code= MYX_CHARSET_CONVERSION_ERROR;
    return NULL;
  };

  if (!file_exists(local_filename))
  {
    *error_code = MYX_ERROR_CANT_OPEN_FILE;
    return NULL;
  }

  doc = myx_xmlParseFile(filename);

  if (doc == NULL)
  {
    *error_code = MYX_XML_PARSE_ERROR;
    return NULL;
  }

  root = xmlDocGetRootElement(doc);

  if (root == NULL) {
    *error_code = MYX_XML_EMPTY_DOCUMENT;
    xmlFreeDoc(doc);
    return NULL;
  }

  bookmarks= g_malloc0(sizeof(MYX_BOOKMARKS));

  bookmarks->bookmark_groups_num= get_child_count(root, (xmlChar*)"bookmark_group");

  if (bookmarks->bookmark_groups_num > 0)
  {
    int i;
    xmlNodePtr cur;

    bookmarks->bookmark_groups= g_malloc0(bookmarks->bookmark_groups_num * sizeof(MYX_BOOKMARK_GROUP));

    i=0;
    cur = root->children;
    while (cur != NULL) 
    {
      if (!xmlStrcmp(cur->name, (xmlChar*)"bookmark_group"))
      {
        read_in_bookmark_group(cur, bookmarks->bookmark_groups+i);
        i++;
      }

      cur = cur->next;
    }
  }

  sort_bookmark_groups_and_bookmarks(bookmarks);

  xmlFreeDoc(doc);

  return bookmarks;
}


/**
 * Stores a bookmarks collection into a file.
 *
 * @param filename The name of the file to store the bookmarks in (UTF-8 encoded).
 * @param bookmarks The bookmarks to store.
 * @return 0 if everything went fine otherwise -1.
 */
MYX_LIB_ERROR myx_bookmarks_store(const char *filename, MYX_BOOKMARKS *bookmarks)
{
  xmlDocPtr doc;
  xmlNodePtr root_node;
  unsigned int i;

  g_return_val_if_fail(bookmarks, -1);
  g_return_val_if_fail(filename, -1);

  doc = xmlNewDoc((xmlChar*)"1.0");

  root_node = doc->children = xmlNewDocRawNode(doc, NULL, (xmlChar*)"bookmarks", NULL);  

  for (i=0; i < bookmarks->bookmark_groups_num; i++)
  {
    store_bookmark_group(bookmarks->bookmark_groups+i, root_node);
  }

  if (myx_xmlSaveFile(filename, doc) == -1)
  {
    xmlFreeDoc(doc);
    return -1;
  }

  xmlFreeDoc(doc);
  return 0;
}


void myx_bookmarks_free(MYX_BOOKMARKS *bookmarks)
{
  if (bookmarks)
  {
    unsigned int i;

    for(i=0; i < bookmarks->bookmark_groups_num; i++)
    {
      free_bookmark_group_content(bookmarks->bookmark_groups+i);
    }

    g_free(bookmarks->bookmark_groups);
    g_free(bookmarks);
  }
}


/*
* Private functions
*/

static void store_bookmark_group(MYX_BOOKMARK_GROUP *bookmark_group, xmlNodePtr parent)
{
  unsigned int j;
  xmlNodePtr bookmark_group_node;

  bookmark_group_node= xmlNewTextChild(parent, NULL, (xmlChar*)"bookmark_group", NULL);

  xmlNewTextChild(bookmark_group_node, NULL, (xmlChar*)"caption", (xmlChar*)bookmark_group->caption);
  NewTextChild_int_content(bookmark_group_node, NULL, (xmlChar*)"pos", bookmark_group->pos); 

  for (j=0; j < bookmark_group->bookmarks_num; j++)
  {
    store_bookmark(bookmark_group->bookmarks + j, bookmark_group_node);
  }

  for (j=0; j < bookmark_group->bookmark_groups_num; j++)
  {
    store_bookmark_group(bookmark_group->bookmark_groups + j, bookmark_group_node);
  }
}

static void store_bookmark(MYX_BOOKMARK *bookmark, xmlNodePtr parent)
{
  xmlNodePtr bookmark_node;

  bookmark_node= xmlNewTextChild(parent, NULL, (xmlChar*)"bookmark", NULL);

  xmlNewTextChild(bookmark_node, NULL, (xmlChar*)"caption", (xmlChar*)bookmark->caption);
  NewTextChild_int_content(bookmark_node, NULL, (xmlChar*)"pos", bookmark->pos);

  xmlNewTextChild(bookmark_node, NULL, (xmlChar*)"catalog", (xmlChar*)bookmark->catalog);
  xmlNewTextChild(bookmark_node, NULL, (xmlChar*)"schema", (xmlChar*)bookmark->schema);
  xmlNewTextChild(bookmark_node, NULL, (xmlChar*)"sql", (xmlChar*)bookmark->sql);

  NewTextChild_int_content(bookmark_node, NULL, (xmlChar*)"query_type", bookmark->query_type);
  NewTextChild_int_content(bookmark_node, NULL, (xmlChar*)"access_count", bookmark->access_count);

  xmlNewTextChild(bookmark_node, NULL, (xmlChar*)"date_created", (xmlChar*)bookmark->date_created);
  xmlNewTextChild(bookmark_node, NULL, (xmlChar*)"date_modified", (xmlChar*)bookmark->date_modified);
  xmlNewTextChild(bookmark_node, NULL, (xmlChar*)"date_last_access", (xmlChar*)bookmark->date_last_access);
}

static void free_bookmark_group_content(MYX_BOOKMARK_GROUP *group)
{
  unsigned int i;

  xmlFree(group->caption);

  for (i=0; i < group->bookmarks_num; i++)
  {
    free_bookmark_content(group->bookmarks+i);
  }
  g_free(group->bookmarks);

  for(i=0; i < group->bookmark_groups_num; i++)
  {
    free_bookmark_group_content(group->bookmark_groups+i);
  }
  g_free(group->bookmark_groups);
}

static void free_bookmark_content(MYX_BOOKMARK *bm)
{
  xmlFree(bm->caption);
  xmlFree(bm->catalog);
  xmlFree(bm->schema);
  xmlFree(bm->sql);
  xmlFree(bm->date_created);
  xmlFree(bm->date_modified);
  xmlFree(bm->date_last_access);
}

static void read_in_bookmark_group(xmlNodePtr node, MYX_BOOKMARK_GROUP *bookmark_group)
{
  int i,j;
  xmlNodePtr cur;
  xmlDocPtr doc= node->doc;

  bookmark_group->bookmark_groups_num= get_child_count(node, (xmlChar*)"bookmark_group");
  bookmark_group->bookmark_groups= g_malloc0(sizeof(MYX_BOOKMARK_GROUP) * bookmark_group->bookmark_groups_num);

  bookmark_group->bookmarks_num= get_child_count(node, (xmlChar*)"bookmark");
  bookmark_group->bookmarks= g_malloc0(sizeof(MYX_BOOKMARK) * bookmark_group->bookmarks_num);

  i=0;
  j=0;
  cur = node->children;
  while (cur != NULL) 
  {
    if (!xmlStrcmp(cur->name, (xmlChar*)"bookmark_group"))
    {
      read_in_bookmark_group(cur, bookmark_group->bookmark_groups+i);
      i++;
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"caption"))
    {
      bookmark_group->caption= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"pos"))
    {
      bookmark_group->pos= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"bookmark"))
    {
      read_in_bookmark(cur, bookmark_group->bookmarks+j);
      j++;
    }

    cur = cur->next;
  }
}

static void read_in_bookmark(xmlNodePtr node, MYX_BOOKMARK *bookmark)
{
  xmlNodePtr cur;
  xmlDocPtr doc= node->doc;

  cur = node->children;
  while (cur != NULL) 
  {
    if (!xmlStrcmp(cur->name, (xmlChar*)"caption"))
    {
      bookmark->caption= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"pos"))
    {
      bookmark->pos= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"catalog"))
    {
      bookmark->catalog= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"schema"))
    {
      bookmark->schema= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"sql"))
    {
      bookmark->sql= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"query_type"))
    {
      bookmark->query_type= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"access_count"))
    {
      bookmark->access_count= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"date_created"))
    {
      bookmark->date_created= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"date_modified"))
    {
      bookmark->date_modified= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }
    else if (!xmlStrcmp(cur->name, (xmlChar*)"date_last_access"))
    {
      bookmark->date_last_access= (char*)xmlNodeListGetString(doc, cur->children, 1);
    }

    cur= cur->next;
  }

}


/* for sorting .. */

static void sort_bookmark_groups_and_bookmarks(MYX_BOOKMARKS *bookmarks)
{
  sort_bookmark_group(bookmarks->bookmark_groups, bookmarks->bookmark_groups_num);
}

static int compare_groups(const void *a, const void *b)
{
  MYX_BOOKMARK_GROUP *g1= (MYX_BOOKMARK_GROUP *)a;
  MYX_BOOKMARK_GROUP *g2= (MYX_BOOKMARK_GROUP *)b;

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

static int compare_bookmarks(const void *a, const void *b)
{
  MYX_BOOKMARK *g1= (MYX_BOOKMARK *)a;
  MYX_BOOKMARK *g2= (MYX_BOOKMARK *)b;

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

static void sort_bookmark_group(MYX_BOOKMARK_GROUP *groups, unsigned int groups_num)
{
  unsigned int i;

  /* sort this list */
  qsort(groups, groups_num, sizeof(MYX_BOOKMARK_GROUP), compare_groups);

  /* sort the bookmarks */
  for (i=0; i < groups_num; i++)
  {
    qsort(groups[i].bookmarks, groups[i].bookmarks_num, sizeof(MYX_BOOKMARK), compare_bookmarks);
  }

  /* sort the sub-groups */
  for (i=0; i < groups_num; i++)
  {
    sort_bookmark_group(groups[i].bookmark_groups, groups[i].bookmark_groups_num);
  }
}
