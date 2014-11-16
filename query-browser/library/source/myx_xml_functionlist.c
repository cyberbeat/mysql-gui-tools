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


#include <myx_qb_library.h>

#include <glib.h>

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>

#include <assert.h>
#include <myx_xml_util_functions.h>
#include <string.h>

/* Forward declarations */
static void read_in_function(xmlNodePtr node, MYX_SQL_FUNCTION *function);
static void read_in_functiongroup(xmlNodePtr node, MYX_SQL_FUNCTIONGROUP *group);
static void free_function_content(MYX_SQL_FUNCTION *f);
static void free_sql_functiongroup_content(MYX_SQL_FUNCTIONGROUP *g);

//----------------------------------------------------------------------------------------------------------------------

MYX_SQL_FUNCTIONINDEX* myx_load_sql_function_list(const char *filename, MYX_LIB_ERROR *error_code)
{
  xmlDocPtr doc;
  xmlNodePtr root;
  MYX_SQL_FUNCTIONINDEX *functionindex;

  *error_code = MYX_NO_ERROR;

  if (! file_exists(filename) )  
  {
    *error_code = MYX_ERROR_CANT_OPEN_FILE;
    return NULL;
  }

  /* create a parser context */
  doc = myx_xmlParseFile(filename);

  if (doc == NULL ) {
    *error_code = MYX_XML_PARSE_ERROR;
    return NULL;
  }

  root = xmlDocGetRootElement(doc);

  if (root == NULL) {
    *error_code = MYX_XML_EMPTY_DOCUMENT;
    xmlFreeDoc(doc);
    return NULL;
  }

  functionindex= g_malloc0(sizeof(MYX_SQL_FUNCTIONINDEX));

  functionindex->groups_num= get_child_count(root, (xmlChar*)"functiongroup");

  if (functionindex->groups_num > 0)
  {
    int i;
    xmlNodePtr cur;

    functionindex->groups= g_malloc0(functionindex->groups_num * sizeof(MYX_SQL_FUNCTIONGROUP));

    i=0;
    cur = root->children;
    while (cur != NULL)
    {
      if (!xmlStrcmp(cur->name, (xmlChar*)"functiongroup"))
      {
        read_in_functiongroup(cur, functionindex->groups+i);
        i++;
      }

      cur = cur->next;
    }
  }


  xmlFreeDoc(doc);

  return functionindex;
}

//----------------------------------------------------------------------------------------------------------------------

int myx_free_sql_function_list(MYX_SQL_FUNCTIONINDEX *f)
{
  if (f)
  {
    unsigned int i;

    for(i= 0; i < f->groups_num; i++)
    {
      free_sql_functiongroup_content(f->groups + i);
    }
    g_free(f->groups);
    g_free(f);
  }

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

static void free_sql_functiongroup_content(MYX_SQL_FUNCTIONGROUP *g)
{
  unsigned int i;

  for (i= 0; i < g->subgroups_num; i++)
  {
    free_sql_functiongroup_content(g->subgroups + i);
  }
  g_free(g->subgroups);

  for (i= 0; i < g->functions_num; i++)
  {
    free_function_content(g->functions + i);
  }
  g_free(g->functions);
}

//----------------------------------------------------------------------------------------------------------------------

static void free_function_content(MYX_SQL_FUNCTION *f)
{
  xmlFree(f->caption);
  xmlFree(f->id);
}

//----------------------------------------------------------------------------------------------------------------------

static void read_in_functiongroup(xmlNodePtr node, MYX_SQL_FUNCTIONGROUP *group)
{
  unsigned int i;
  xmlNodePtr current;

  group->caption= (char*)xmlGetProp(node, (xmlChar*)"caption");

  // Sub function groups.
  group->subgroups_num= get_child_count(node, (xmlChar*)"functionsubgroup");
  if (group->subgroups_num > 0)
  {
    group->subgroups= g_malloc0(sizeof(MYX_SQL_FUNCTIONGROUP) * group->subgroups_num);

    i= 0;
    current = node->children;
    while (current != NULL)
    {
      if (!xmlStrcmp(current->name, (xmlChar*)"functionsubgroup"))
      {
        read_in_functiongroup(current, group->subgroups + i);
        i++;
      };

      current = current->next;
    };
  };

  group->functions_num= get_child_count(node, (xmlChar*)"function");
  if (group->functions_num > 0)
  {
    group->functions= g_malloc0(sizeof(MYX_SQL_FUNCTION) * group->functions_num);

    i= 0;
    current = node->children;
    while (current != NULL)
    {
      if (!xmlStrcmp(current->name, (xmlChar*)"function"))
      {
        read_in_function(current, group->functions + i);
        i++;
      }

      current = current->next;
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

static void read_in_function(xmlNodePtr node, MYX_SQL_FUNCTION *function)
{
  function->caption= (char*)xmlGetProp(node, (xmlChar*)"caption");
  function->id= (char*)xmlGetProp(node, (xmlChar*)"id");
}

//----------------------------------------------------------------------------------------------------------------------


