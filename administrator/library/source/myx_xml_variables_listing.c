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
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

//#include <assert.h>
#include <stdlib.h>
#include <myx_xml_util_functions.h>


/*
* Forward declarations
*/
static MYX_VARIABLES_LISTING* read_in_variables_listing(const xmlNodePtr node);
static void read_in_group(const xmlNodePtr node, MYX_VARIABLES_GROUP *group);
static void read_in_subgroup(const xmlNodePtr node, 
                             MYX_VARIABLES_SUBGROUP *subgroup);
static void read_in_variable_element(const xmlNodePtr node, 
                                     MYX_VARIABLE_ELEMENT *element);

static void free_group_content(MYX_VARIABLES_GROUP *group);
static void free_subgroup_content(MYX_VARIABLES_SUBGROUP *subgroup);
static void free_variable_element_content(MYX_VARIABLE_ELEMENT *variable);

static void sort_groups_and_subgroups(MYX_VARIABLES_LISTING *var_listing);

/*
* Public functions
*/

MYX_VARIABLES_LISTING* myx_get_variables_listing(const char *filename, 
                                                 MYX_ADMIN_LIB_ERROR *err_code)
{
  xmlDocPtr doc;
  xmlNodePtr root;
  char* local_filename;
  MYX_VARIABLES_LISTING *variables_listing= NULL;

  *err_code= MYX_ADMIN_NO_ERROR;

  local_filename= g_filename_from_utf8(filename, -1, NULL, NULL, NULL);
  if (local_filename == NULL)
  {
    *err_code= MYX_CHARSET_CONVERSION_ERROR;
    return NULL;
  };

  if (!file_exists(local_filename))
  {
    *err_code= MYX_ADMIN_ERROR_CANT_OPEN_FILE;
  }
  else
  {
    doc= myx_xmlParseFile(filename);
    if (doc == NULL)
    {
      *err_code= MYX_ADMIN_XML_PARSE_ERROR;
    }
    else
    {
      root= xmlDocGetRootElement(doc);
      if (doc == NULL)
      {
        *err_code= MYX_ADMIN_XML_EMPTY_DOCUMENT;
      }
      else
      {
        variables_listing= read_in_variables_listing(root);
        sort_groups_and_subgroups(variables_listing);
      }
      xmlFreeDoc(doc);
    };
  };

  return variables_listing;    
}

int myx_free_variables_listing(MYX_VARIABLES_LISTING* variables_listing)
{
  unsigned int i;

  if (variables_listing)
  {
    for (i= 0; i < variables_listing->groups_num; i++)
    {
      free_group_content(variables_listing->groups +i);
    }
    free(variables_listing->groups);
    free(variables_listing);
  }
  return 0;
}

/*
* Private functions
*/ 

/*read_in helper functions */

static MYX_VARIABLES_LISTING* read_in_variables_listing(const xmlNodePtr node)
{
  MYX_VARIABLES_LISTING *variables_listing;
  xmlNodePtr cur;

  variables_listing= calloc(1, sizeof(MYX_VARIABLES_LISTING) );

  variables_listing->groups_num= get_child_count(node,"group");
  if (!variables_listing->groups_num)
  {
    variables_listing->groups= 0;
  }
  else
  {
    MYX_VARIABLES_GROUP * group= variables_listing->groups= 
             calloc(variables_listing->groups_num,sizeof(MYX_VARIABLES_GROUP));

    for (cur= node->children; cur != NULL; cur= cur->next)
    {
      /* read a single group*/
      if (!xmlStrcmp(cur->name, "group"))
      {
        read_in_group(cur, group);
        group++;
      }
    }
  }

  return variables_listing;
}


static void read_in_group(const xmlNodePtr node, MYX_VARIABLES_GROUP *group)
{
  xmlNodePtr cur;
  MYX_VARIABLE_ELEMENT * variable;
  MYX_VARIABLES_SUBGROUP * subgroup;

  /* read in attributes */
  group->pos= atoi_and_free(xmlGetProp(node, "pos"));
  group->name= xmlGetProp(node, "name");
  group->caption_id= xmlGetProp(node, "caption_id");

  /* read variables */
  group->variables_num= get_child_count(node, "variable");
  variable= group->variables= !group->variables_num ? 0 : 
                                calloc(group->variables_num,
                                       sizeof(MYX_VARIABLE_ELEMENT));

  /* read in subgroups */
  group->subgroups_num= get_child_count(node, "subgroup");
  subgroup= group->subgroups= !group->subgroups_num ? 0 :
                                calloc(group->subgroups_num,
                                       sizeof(MYX_VARIABLES_SUBGROUP));

  for (cur= node->children; cur; cur= cur->next)
  {
    if (!xmlStrcmp(cur->name, "variable"))
    {
      read_in_variable_element(cur, variable);
      variable++;
    }
    else if (!xmlStrcmp(cur->name, "subgroup"))
    {
      read_in_subgroup(cur, subgroup);
      subgroup++;
    }
  }
}


static void read_in_subgroup(const xmlNodePtr node,
                             MYX_VARIABLES_SUBGROUP *subgroup)
{
  xmlNodePtr cur;

  /* read in attributes */
  subgroup->pos= atoi_and_free(xmlGetProp(node, "pos"));
  subgroup->name= xmlGetProp(node, "name");
  subgroup->caption_id= xmlGetProp(node, "caption_id");

  /* read variables */
  subgroup->variables_num = get_child_count(node, "variable");
  if (subgroup->variables_num > 0) 
  {
    MYX_VARIABLE_ELEMENT *variable_element= subgroup->variables= 
                  calloc(subgroup->variables_num,sizeof(MYX_VARIABLE_ELEMENT));
    
    for (cur= node->children; cur; cur= cur->next)
    {
      if (!xmlStrcmp(cur->name, "variable"))
      {
        read_in_variable_element(cur, variable_element);
        variable_element++;
      }
    } 
  }
}

static void read_in_variable_element(const xmlNodePtr node, MYX_VARIABLE_ELEMENT *element)
{
  xmlNodePtr description;
  xmlChar *tmp= xmlGetProp(node, "editable");
  element->editable= (tmp && !xmlStrcmp(tmp, "yes")) ? 1 : 0;
  if (tmp)
    xmlFree(tmp);

  element->mysql_id= xmlGetProp(node, "mysql_id");

  // Description is an own sub node.
  description= node->children;
  while (description != NULL)
  {
    if (!xmlStrcmp(description->name, "description"))
    {
      element->desc_id= xmlNodeListGetString(description->doc, description->children, 1);
      break;
    };
    description= description->next;
  };
}


/* free helper functions */

static void free_group_content(MYX_VARIABLES_GROUP *group)
{
  unsigned int i;

  xmlFree(group->name);
  xmlFree(group->caption_id);

  for(i=0; i < group->variables_num; i++)
  {
    free_variable_element_content(group->variables+i);
  }
  free(group->variables);

  for (i=0; i < group->subgroups_num; i++)
  {
    free_subgroup_content(group->subgroups+i);
  }
  free(group->subgroups);
}

static void free_subgroup_content(MYX_VARIABLES_SUBGROUP *subgroup)
{
  unsigned int i;

  xmlFree(subgroup->name);
  xmlFree(subgroup->caption_id);

  for(i=0; i < subgroup->variables_num; i++)
  {
    free_variable_element_content(subgroup->variables+i);
  }
  free(subgroup->variables);
}

static void free_variable_element_content(MYX_VARIABLE_ELEMENT *variable)
{
  xmlFree(variable->mysql_id);
  xmlFree(variable->desc_id);
}


/*Functions for sorting the groups*/
static int compare_groups(const void *a, const void *b)
{
  MYX_VARIABLES_GROUP *g1 = (MYX_VARIABLES_GROUP *) a;
  MYX_VARIABLES_GROUP *g2 = (MYX_VARIABLES_GROUP *) b;

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


static int compare_subgroups(const void *a, const void *b)
{
  MYX_VARIABLES_SUBGROUP *g1 = (MYX_VARIABLES_SUBGROUP *) a;
  MYX_VARIABLES_SUBGROUP *g2 = (MYX_VARIABLES_SUBGROUP *) b;

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

static void sort_groups_and_subgroups(MYX_VARIABLES_LISTING *variables_listing)
{
  unsigned int i;

  if (variables_listing->groups)
  {
    /*sort the groups*/
    qsort(variables_listing->groups, variables_listing->groups_num,
          sizeof(MYX_VARIABLES_GROUP), compare_groups);

    /*sort the subgroups contained in each group*/
    for (i=0; i < variables_listing->groups_num; i++)
    {
      if (variables_listing->groups[i].subgroups)
      {
        qsort(variables_listing->groups[i].subgroups,
              variables_listing->groups[i].subgroups_num,
              sizeof(MYX_VARIABLES_SUBGROUP), compare_subgroups);
      }
    }
  }
}
