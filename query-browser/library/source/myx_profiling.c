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


#ifdef notdef

#include <glib.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include "myx_xml_aux_functions.h"
#include "myx_public_interface.h"
#include "myx_profiling.h"


#define DBG(s,...) g_message("QBPROF:"s, ##__VA_ARGS__)


static int store_query_log_query(xmlDocPtr doc, xmlNodePtr parent, MYX_QP_QUERY *obj);
static int store_query_log_query_log(xmlDocPtr doc, xmlNodePtr parent, MYX_QP_QUERY_LOG *obj);

static int read_query_log_query(xmlDocPtr doc, xmlNodePtr node, MYX_QP_QUERY *obj);
static int read_query_log_query_log(xmlDocPtr doc, xmlNodePtr node, MYX_QP_QUERY_LOG *obj);

static int free_query_log_query(MYX_QP_QUERY *obj);


MYX_QP_QUERY_LOG *myx_load_query_log(const char *filename)
{
  xmlDocPtr doc;
  xmlNodePtr parent;
  MYX_QP_QUERY_LOG *obj;
                                                                                             
  if (!file_exists(filename))
  {
//    *error_code = MYX_ERROR_CANT_OPEN_FILE;
    return NULL;
  }
  if (!(doc= xmlParseFile(filename)))
  {
//    *error_code = MYX_XML_PARSE_ERROR;
    return NULL;
  }

  parent = xmlDocGetRootElement(doc);
  if (parent == NULL)
  {
//    *error_code = MYX_XML_EMPTY_DOCUMENT;
    xmlFreeDoc(doc);
    return NULL;
  }

  obj= g_malloc0(sizeof(MYX_QP_QUERY_LOG));

  if (read_query_log_query_log(doc, parent, obj) < 0)
  {
    myx_free_query_log(obj);
    xmlFreeDoc(doc);
    return NULL;
  }

  xmlFreeDoc(doc);

  return obj;
}


static int store_query_log_query(xmlDocPtr doc, xmlNodePtr parent, MYX_QP_QUERY *obj)
{
  xmlNodePtr node;
  unsigned int i;

  node= xmlNewTextChild(parent, NULL, "query", NULL);

  NewTextChild_int_content(node, NULL, "source_api", obj->source_api);
  xmlNewTextChild(node, NULL, "source_name", obj->source_name);
  NewTextChild_int_content(node, NULL, "line_nr", obj->line_nr);
  xmlNewTextChild(node, NULL, "stack_info", obj->stack_info);
  xmlNewTextChild(node, NULL, "query", obj->query);
  NewTextChild_int_content(node, NULL, "execution_time_ms", obj->execution_time_ms);
  NewTextChild_int_content(node, NULL, "mysql_thread_id", obj->mysql_thread_id);
  NewTextChild_int_content(node, NULL, "mysql_state_code", obj->mysql_state_code);
  NewTextChild_int_content(node, NULL, "mysql_num_rows", obj->mysql_num_rows);
  NewTextChild_int_content(node, NULL, "mysql_affected_rows", obj->mysql_affected_rows);
  NewTextChild_int_content(node, NULL, "mysql_error_nr", obj->mysql_error_nr);
  xmlNewTextChild(node, NULL, "mysql_error", obj->mysql_error);
  for (i= 0; i < obj->mysql_warnings_num; i++)
  {
    xmlNewTextChild(node, NULL, "mysql_warnings", obj->mysql_warnings[i]);
  }
  NewTextChild_int_content(node, NULL, "mysql_insert_id", obj->mysql_insert_id);
  NewTextChild_int_content(node, NULL, "index_usage", obj->index_usage);
  return 0;
}

static int store_query_log_query_log(xmlDocPtr doc, xmlNodePtr parent, MYX_QP_QUERY_LOG *obj)
{
  xmlNodePtr node;
  unsigned int i;

  node= xmlNewTextChild(parent, NULL, "query_log", NULL);

  for (i= 0; i < obj->query_num; i++)
  {
    store_query_log_query(doc, node, obj->query+i);
  }
  return 0;
}



static int read_query_log_query(xmlDocPtr doc, xmlNodePtr node, MYX_QP_QUERY *obj)
{
  xmlNodePtr cur;
  unsigned int i;

  for (cur= node->xmlChildrenNode; cur != NULL; cur= cur->next)
  {
    if (!xmlStrcmp(cur->name, "source_api"))
    {
      obj->source_api= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    if (!xmlStrcmp(cur->name, "source_name"))
    {
      obj->source_name= xmlNodeListGetString(doc, cur->children, 1);
    }
    if (!xmlStrcmp(cur->name, "line_nr"))
    {
      obj->line_nr= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    if (!xmlStrcmp(cur->name, "stack_info"))
    {
      obj->stack_info= xmlNodeListGetString(doc, cur->children, 1);
    }
    if (!xmlStrcmp(cur->name, "query"))
    {
      obj->query= xmlNodeListGetString(doc, cur->children, 1);
    }
    if (!xmlStrcmp(cur->name, "execution_time_ms"))
    {
      obj->execution_time_ms= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    if (!xmlStrcmp(cur->name, "mysql_thread_id"))
    {
      obj->mysql_thread_id= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    if (!xmlStrcmp(cur->name, "mysql_state_code"))
    {
      obj->mysql_state_code= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    if (!xmlStrcmp(cur->name, "mysql_num_rows"))
    {
      obj->mysql_num_rows= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    if (!xmlStrcmp(cur->name, "mysql_affected_rows"))
    {
      obj->mysql_affected_rows= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    if (!xmlStrcmp(cur->name, "mysql_error_nr"))
    {
      obj->mysql_error_nr= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    if (!xmlStrcmp(cur->name, "mysql_error"))
    {
      obj->mysql_error= xmlNodeListGetString(doc, cur->children, 1);
    }
    if (!xmlStrcmp(cur->name, "mysql_insert_id"))
    {
      obj->mysql_insert_id= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    if (!xmlStrcmp(cur->name, "index_usage"))
    {
      obj->index_usage= atoi_and_free(xmlNodeListGetString(doc, cur->children, 1));
    }
    if (!xmlStrcmp(cur->name, "mysql_warnings"))
    {
      i= obj->mysql_warnings_num; obj->mysql_warnings_num++;
      obj->mysql_warnings= g_realloc(obj->mysql_warnings, obj->mysql_warnings_num * sizeof(char *));
      obj->mysql_warnings[i]= xmlNodeListGetString(doc, cur->children, 1);
    }
  }
  return 0;
}

static int read_query_log_query_log(xmlDocPtr doc, xmlNodePtr node, MYX_QP_QUERY_LOG *obj)
{
  xmlNodePtr cur;
  unsigned int i;

  for (cur= node->xmlChildrenNode; cur != NULL; cur= cur->next)
  {
    if (!xmlStrcmp(cur->name, "query"))
    {
      i= obj->query_num; obj->query_num++;
      obj->query= g_realloc(obj->query, obj->query_num * sizeof(MYX_QP_QUERY));
      read_query_log_query(doc, node, obj->query+i);
    }
  }
  return 0;
}



static int free_query_log_query(MYX_QP_QUERY *obj)
{
  unsigned int i;
  xmlFree(obj->source_name);
  xmlFree(obj->stack_info);
  xmlFree(obj->query);
  xmlFree(obj->mysql_error);
  for (i= 0; i < obj->mysql_warnings_num; i++)
  {
    xmlFree(obj->mysql_warnings[i]);
  }
  g_free(obj->mysql_warnings);
  return 0;
}

int myx_free_query(MYX_QP_QUERY_LOG *obj)
{
  unsigned int i;
  for (i= 0; i < obj->query_num; i++)
  {
    free_query_log_query(obj->query+i);
  }
  g_free(obj->query);
  return 0;
}


#endif
