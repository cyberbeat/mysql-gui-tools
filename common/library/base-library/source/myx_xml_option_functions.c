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


#include "myx_library.h"

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <stdlib.h>
#include <assert.h>
#include "myx_xml_util_functions.h"

/*
 * Macros
 */

// max valid xml option file size
#define MAX_VALID_OPTION_FILE_SIZE  (512*1024)

/*
 * Forward declarations
 */
static void get_name_value_pairs(xmlNodePtr node,
                                 MYX_OPTION_GROUP *option_group);
static void free_name_value_pair_content(MYX_NAME_VALUE_PAIR *name_value_pair);
static void free_option_group_content(MYX_OPTION_GROUP *option_group);

/*
 * Public functions
 */

///////////////////////////////////////////////////////////////////////////////
/** @ brief loads application options from an XML file
    @param filename Path to an xml file containing the application options (utf-8 encoded).

    @param error_code returned error code, <BR>
           possible values are:
             - \link MYX_LIB_ERROR::MYX_NO_ERROR MYX_NO_ERROR \endlink
             - \link MYX_LIB_ERROR::MYX_ERROR_CANT_OPEN_FILE 
                                             MYX_ERROR_CANT_OPEN_FILE \endlink
             - \link MYX_LIB_ERROR::MYX_XML_PARSE_ERROR 
                                                  MYX_XML_PARSE_ERROR \endlink
             - \link MYX_LIB_ERROR::MYX_XML_EMPTY_DOCUMENT 
                                               MYX_XML_EMPTY_DOCUMENT \endlink
             - \link MYX_LIB_ERROR::MYX_XML_NO_VALID_DOCUMENT
                  MYX_XML_NO_VALID_DOCUMENT \endlink 
                  if the root of the document wasn't equal to
                  "application_options"

    @return loaded MYX_APPLICATION_OPTIONS
*//////////////////////////////////////////////////////////////////////////////
MYX_APPLICATION_OPTIONS* myx_get_application_options(char *filename,
                                                     MYX_LIB_ERROR *error_code)
{
  xmlDocPtr doc;
  xmlNodePtr root,cur;
  MYX_APPLICATION_OPTIONS *app_options;
  MYX_OPTION_GROUP * group;

  char* local_filename;
  *error_code= MYX_NO_ERROR;

  local_filename= g_filename_from_utf8(filename, -1, NULL, NULL, NULL);
  if (local_filename == NULL)
  {
    *error_code= MYX_CHARSET_CONVERSION_ERROR;
    return NULL;
  };

  if (!file_exists(local_filename))
  {
    *error_code= MYX_ERROR_CANT_OPEN_FILE;
    return NULL;
  }

  if(file_size(local_filename) > MAX_VALID_OPTION_FILE_SIZE)
  {
    *error_code= MYX_CANT_READ_FROM_FILE;
    return NULL;
  }

  doc= myx_xmlParseFile(filename);

  if (doc == NULL)
  {
    *error_code= MYX_XML_PARSE_ERROR;
    return NULL;
  }

  root= xmlDocGetRootElement(doc);

  if (root == NULL)
  {
    *error_code= MYX_XML_EMPTY_DOCUMENT;
    xmlFreeDoc(doc);
    return NULL;
  }
  
  if (xmlStrcmp(root->name, (const xmlChar *) "application_options"))
  {
    *error_code= MYX_XML_NO_VALID_DOCUMENT;
    xmlFreeDoc(doc);
    return NULL;
  }

  app_options= g_malloc(sizeof(MYX_APPLICATION_OPTIONS));

  /*count the number of group_elements*/
  app_options->option_groups_num= get_child_count(root,(xmlChar*)"group");

  app_options->option_groups= 
    app_options->option_groups_num==0 ? NULL :
    g_malloc(sizeof(MYX_OPTION_GROUP) * app_options->option_groups_num);

  /*read each group-element together with its name_value_pairs*/
  for (group= app_options->option_groups, cur= root->children; cur != NULL;
       cur= cur->next)
  {
    if (xmlStrcmp(cur->name, (xmlChar*)"group"))
      continue;

    group->name= (char*)xmlGetProp(cur, (xmlChar*)"name");
    get_name_value_pairs(cur, group);
    group++;
  }

  xmlFreeDoc(doc);
  return app_options;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief stores application option in an XML file
    @param options options to store
    @param filename path to file to store in
    @return -1 if file was not written, else 0
*//////////////////////////////////////////////////////////////////////////////
int myx_store_application_options(MYX_APPLICATION_OPTIONS *options,
                                  char *filename)
{
  xmlDocPtr doc;
  xmlNodePtr root_node, group_node, name_value_node;
  int res;
  MYX_OPTION_GROUP * group, * groups_end;
  MYX_NAME_VALUE_PAIR * pair, * pairs_end;

  if (options == NULL || filename == NULL) return -1;

  doc= xmlNewDoc((xmlChar*)"1.0");

  root_node= doc->children= xmlNewDocRawNode(doc,NULL,
                                             (xmlChar*)"application_options", NULL);

  groups_end= options->option_groups + options->option_groups_num;
  for (group= options->option_groups; group!=groups_end; group++)
  {
    group_node= xmlNewTextChild(root_node, NULL, (xmlChar*)"group", NULL);
    xmlNewProp(group_node, (xmlChar*)"name", (xmlChar*)group->name);
    pairs_end= group->name_value_pairs + group->name_value_pairs_num;
    for (pair= group->name_value_pairs; pair!=pairs_end; pair++)
    {
      name_value_node= xmlNewTextChild(group_node, NULL, (xmlChar*)"property", NULL);
      xmlNewProp(name_value_node, (xmlChar*)"name", (xmlChar*)pair->name);
      xmlNewProp(name_value_node, (xmlChar*)"value", (xmlChar*)pair->value);
    }
  }
  
  res= myx_xmlSaveFile(filename, doc);
  xmlFreeDoc(doc);
  return (res == -1) ? -1 : 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for application options
    @param options application options to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
MYX_PUBLIC_FUNC
             int myx_free_application_options(MYX_APPLICATION_OPTIONS *options)
{
  if (options)
  {
    MYX_OPTION_GROUP * group, * groups_end;
    groups_end= options->option_groups + options->option_groups_num;
    for (group= options->option_groups; group!=groups_end; group++)
      free_option_group_content(group);
    g_free(options->option_groups);
    g_free(options);
  }
  return 0;
}

/*
 * Private functions
 */

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for name-value pair
    @param name_value_pair the pair to free
*//////////////////////////////////////////////////////////////////////////////
static void free_name_value_pair_content(MYX_NAME_VALUE_PAIR *name_value_pair)
{
  xmlFree(name_value_pair->name);
  xmlFree(name_value_pair->value);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for option group
    @param option_group the option group to free
*//////////////////////////////////////////////////////////////////////////////
static void free_option_group_content(MYX_OPTION_GROUP *option_group)
{
  MYX_NAME_VALUE_PAIR * pair, * pairs_end;
  xmlFree(option_group->name);
  pairs_end= option_group->name_value_pairs + option_group->name_value_pairs_num;
  for (pair= option_group->name_value_pairs; pair!=pairs_end; pair++)
    free_name_value_pair_content(pair);
  g_free(option_group->name_value_pairs);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief Reads in the name-value pairs for a given group
    @param node xml-node to read from
    @param option_group option group to read for
*//////////////////////////////////////////////////////////////////////////////
static void get_name_value_pairs(xmlNodePtr node,
                                 MYX_OPTION_GROUP *option_group)
{
  xmlNodePtr cur;
  MYX_NAME_VALUE_PAIR * pair;

  /*count the number of name-value pairs*/
  option_group->name_value_pairs_num= get_child_count(node,(xmlChar*)"property");

  option_group->name_value_pairs = 
    option_group->name_value_pairs_num <= 0 ? NULL :
      g_malloc(sizeof(MYX_NAME_VALUE_PAIR)*option_group->name_value_pairs_num);

  /*read in the name-value pairs*/
  for (pair= option_group->name_value_pairs, cur= node->children; cur;
       cur= cur->next)
  {
    if (xmlStrcmp(cur->name, (xmlChar*)"property") ) /*ignore wrong elements*/
      continue;

    pair->name= (char*)xmlGetProp(cur, (xmlChar*)"name");
    pair->value= (char*)xmlGetProp(cur, (xmlChar*)"value");
    pair++;
  }
}

