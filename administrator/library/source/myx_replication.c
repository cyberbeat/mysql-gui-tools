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
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

#include "myx_admin_library.h"
#include <myx_xml_util_functions.h>
#include <myx_util_functions.h>


/* forward definitions */
static void free_repl_host_content(MYX_REPL_HOST *host);
static void free_repl_user_host_content(MYX_USER_REPL_HOST *rephost);
static int in_sql_query_list(const char *name, MYX_REPL_HOSTS *list);
static int in_userlist(const char *name, MYX_USER_REPL_HOSTS *replist);

/*
 * Public functions
 */

static const char * show_slaves_columns[]=
{
  "Server_id",          // 0
  "Host",               // 1
  "Port",               // 2
  "Rpl_recovery_rank",  // 3
  "Master_id",          // 4
};
static const char ** show_slaves_columns_end=
               show_slaves_columns + sizeof(show_slaves_columns)/sizeof(char*);

/**
 * Asks the database if we are connected to a replication master
 * and if so what slaves are connected.
 * Note: replist may be NULL 
 **/
MYX_REPL_HOSTS *myx_show_repl_hosts_status(MYSQL *mysql,
                                           MYX_USER_REPL_HOSTS *replist,
                                           MYX_ADMIN_LIB_ERROR *error_code)
{
  char *stmt;
  MYSQL_RES *result;
  MYSQL_ROW row;
  MYX_REPL_HOSTS * list;
  MYX_REPL_HOST * host;
  unsigned int i;
  MYX_REPL_HOST master_host;
  MYSQL_FIELD *fields;
  unsigned int num_fields;
  int fi[5];

  *error_code= 0;

  //-------------------------------------------------------
  //get master info

  //server_id
  stmt= "show variables like 'server_id'";
  result= NULL;
  if (myx_mysql_query(mysql, stmt) == 0)
    result= mysql_store_result(mysql);
  if (result == NULL)
  {
    *error_code= MYX_ADMIN_SQL_ERROR;
    return NULL;
  }

  row= mysql_fetch_row(result);
  if (row == NULL)
  {
    mysql_free_result(result);
    return NULL;
  }
  master_host.server_id= atoi(row[1]);
  mysql_free_result(result);

  //Port
  stmt= "show variables like 'port'";
  result= NULL;
  if (myx_mysql_query(mysql, stmt) == 0)
    result= mysql_store_result(mysql);
  if (result == NULL)
  {
    *error_code= MYX_ADMIN_SQL_ERROR;
    return NULL;
  }
  row= mysql_fetch_row(result);
  if (row == NULL)
  {
    mysql_free_result(result);
    return NULL;
  }
  master_host.port= atoi(row[1]);
  mysql_free_result(result);

  //get master info
  stmt= "show master status";
  result= NULL;
  if (myx_mysql_query(mysql, stmt) == 0)
    result= mysql_store_result(mysql);
  if (result == NULL)
  {
    *error_code= MYX_ADMIN_SQL_ERROR;
    return NULL;
  }
  row= mysql_fetch_row(result);
  if (row == NULL)
  {
    mysql_free_result(result);
    return NULL;
  }
  master_host.binlog_file= g_strdup(row[0]);
  master_host.binlog_pos= g_strdup(row[1]);
  mysql_free_result(result);

  master_host.is_master= 1;
  master_host.host= g_strdup(mysql->host);
  master_host.master_id= 0;

  //-------------------------------------------------------
  //add slaves

  stmt= "show slave hosts";
  result= NULL;
  if (myx_mysql_query(mysql, stmt) == 0)
    result= mysql_store_result(mysql);
  if (result == NULL)
  {
    *error_code= MYX_ADMIN_SQL_ERROR;
    free_repl_host_content(&master_host);
    return NULL;
  }

  list= g_malloc0(sizeof(MYX_REPL_HOSTS));
  list->hosts_num= (unsigned int)mysql_num_rows(result)+1;
  host= list->hosts= g_malloc0(sizeof(MYX_REPL_HOST) * (list->hosts_num));

  num_fields= mysql_num_fields(result);
  fields= mysql_fetch_fields(result);

  build_field_subst(show_slaves_columns,show_slaves_columns_end,
                        fields,fields+num_fields,fi);

  //Copy master as first host
  *host= master_host;
  host++;
  do
  {
    row= mysql_fetch_row(result);
    if (row == NULL)
      break;

    host->server_id=         fi[0]==-1 ? 0 : atoi(row[fi[0]]);
    host->host=              fi[1]==-1 ? 0 : g_strdup(row[fi[1]]);
    host->port=              fi[2]==-1 ? 0 : atoi(row[fi[2]]);
    host->rpl_recovery_rank= fi[3]==-1 ? 0 : atoi(row[fi[3]]);
    host->master_id=         fi[4]==-1 ? 0 : atoi(row[fi[4]]);
    host->is_master= 0;
    host->status= MYX_RHS_NEW_HOST;
    host->binlog_file= NULL;
    host->binlog_pos= NULL;
    host++;
  }
  while (1);
    
  mysql_free_result(result);

  /* compare with replist */
  if (replist)
  {
    for (i= 0; i < list->hosts_num; i++)
    {
      list->hosts[i].status= (in_userlist(list->hosts[i].host, replist))
                                  ? MYX_RHS_AVAILABLE : MYX_RHS_NEW_HOST;
    }

    //now traverse the userlist and search the red ones
    for (i= 0; i < replist->hosts_num; i++)
    {
      if ( !in_sql_query_list(replist->hosts[i].name, list) )
      {
        //add this host as a red one
        MYX_REPL_HOST *slave;

        list->hosts_num++;
        list->hosts= g_realloc(list->hosts,
                               sizeof(MYX_REPL_HOST) * list->hosts_num);
        slave= list->hosts + list->hosts_num-1;

        slave->server_id= 0;
        slave->host= g_strdup(replist->hosts[i].name);
        slave->port= 0;
        slave->rpl_recovery_rank= 0;
        slave->master_id= 0;
        slave->status= MYX_RHS_NOT_AVAILABLE;
        slave->binlog_file= NULL;
        slave->binlog_pos= NULL;
        slave->is_master= 0;
      }
    }
  }

  return list;
}


int myx_free_repl_hosts_status(MYX_REPL_HOSTS *list)
{
  unsigned int i;

  if (list)
  {
    for (i= 0; i < list->hosts_num; i++)
    {
      free_repl_host_content(list->hosts+i);
    }
    g_free(list->hosts);
    g_free(list);
  }

  return 0;
}

MYX_USER_REPL_HOSTS *myx_read_repl_user_hosts(const char *filename, MYX_ADMIN_LIB_ERROR *error_code)
{
  MYX_USER_REPL_HOSTS *user_replist;
  xmlDocPtr doc;
  xmlNodePtr root;
  char* local_filename;

  *error_code= MYX_ADMIN_NO_ERROR;

  local_filename= g_filename_from_utf8(filename, -1, NULL, NULL, NULL);
  if (local_filename == NULL)
  {
    *error_code= MYX_CHARSET_CONVERSION_ERROR;
    return NULL;
  };

  if (!file_exists(local_filename))
  {
    *error_code= MYX_ADMIN_ERROR_CANT_OPEN_FILE;
    return NULL;
  }

  doc= myx_xmlParseFile(filename);
  if (doc == NULL)
  {
    *error_code= MYX_ADMIN_XML_PARSE_ERROR;
    return NULL;
  }

  root= xmlDocGetRootElement(doc);

  if (root == NULL)
  {
    *error_code= MYX_ADMIN_XML_EMPTY_DOCUMENT;
    xmlFreeDoc(doc);
    return NULL;
  }

  user_replist= g_malloc0(sizeof(MYX_USER_REPL_HOSTS));
  user_replist->hosts_num= get_child_count(root, "rephost");

  if (user_replist->hosts_num  > 0)
  {
    xmlNodePtr cur;

    MYX_USER_REPL_HOST * host= user_replist->hosts=
               g_malloc0(sizeof(MYX_USER_REPL_HOST) * user_replist->hosts_num);

    for (cur = root->children; cur != NULL; cur= cur->next)
    {
      if (!xmlStrcmp(cur->name, "rephost"))
      {
        host->name=  xmlGetProp(cur, "name");
        host++;
      }
    }
  }

  xmlFreeDoc(doc);
  return user_replist;
}

int myx_save_repl_user_hosts(const MYX_USER_REPL_HOSTS *replist, 
                             const char *filename)
{
  xmlDocPtr doc;
  xmlNodePtr root_node, node;
  int i;

  doc= xmlNewDoc("1.0");

  root_node= doc->children= xmlNewDocRawNode(doc,NULL, "user_replist", NULL);

  for (i=0; i < (int)replist->hosts_num; i++)
  {
    node= xmlNewTextChild(root_node, NULL, "rephost", NULL);
    xmlSetProp(node, "name", replist->hosts[i].name); 
  }

  if (myx_xmlSaveFile(filename, doc) == -1)
  {
    xmlFreeDoc(doc);
    return -1;
  }

  xmlFreeDoc(doc);

  return 0;
}


int myx_free_repl_user_hosts(MYX_USER_REPL_HOSTS *replist)
{
  unsigned int i;

  if (replist)
  {
    for (i= 0; i < replist->hosts_num; i++)
    {
      free_repl_user_host_content(replist->hosts+i);
    }
    g_free(replist->hosts);
    g_free(replist);
  }
  return 0;
}

/*
 * Private functions
 */

static int in_sql_query_list(const char *name, MYX_REPL_HOSTS *list)
{
  unsigned int i;

  for (i= 0; i < list->hosts_num; i++)
  {
    if (!xmlStrcmp(name, list->hosts[i].host))
    {
      return 1;
    }
  }

  return 0;
}

static int in_userlist(const char *name, MYX_USER_REPL_HOSTS *replist)
{
  unsigned int i;

  for (i= 0; i< replist->hosts_num; i++)
  {
    if (!xmlStrcmp(name, replist->hosts[i].name))
    {
      return 1;
    }
  }

  return 0;
}

static void free_repl_host_content(MYX_REPL_HOST *host)
{
  g_free(host->host);
  g_free(host->binlog_file);
  g_free(host->binlog_pos);
}


static void free_repl_user_host_content(MYX_USER_REPL_HOST *rephost)
{
  xmlFree(rephost->name);
}
