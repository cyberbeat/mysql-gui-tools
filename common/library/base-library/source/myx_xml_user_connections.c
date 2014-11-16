/* Copyright (C) 2003,2004 MySQL AB

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


#include <myx_library.h>
#include <glib.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <assert.h>
#include <myx_xml_util_functions.h>
#include <string.h>
#include <stdlib.h>
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <Wincrypt.h>
#endif

/** @defgroup Connection_management_private internal stuff
 *  @ingroup Connection_management */

/** @addtogroup Connection_management_private
 *  @{ */

/*static variables*/
static xmlXPathContextPtr context; /* used by all XPath operations*/

// forward declarations
static xmlChar *obscure(const xmlChar* str);
static xmlChar *retrieve_password(MYX_USER_CONNECTION *conn, xmlChar *password,
                                  enum myx_password_storage_type storage_type);
static xmlChar* save_password(MYX_USER_CONNECTION *conn, xmlChar *password,
                              enum myx_password_storage_type storage_type);
static 
  void parse_user_connection(xmlDocPtr doc, xmlNodePtr user_conn,
                             MYX_USER_CONNECTION *out,
                             enum myx_password_storage_type pwd_storage_type);
static void parse_im_connection(xmlDocPtr doc,
                                xmlNodePtr im_conn,
                                MYX_IM_CONNECTION *out);

static void store_user_connection_attributes(xmlNodePtr ucon_node, 
                                             MYX_USER_CONNECTION *ucon, 
                                             MYX_PASSWORD_STORAGE_TYPE pwd_storage_type);

// needed because we want to avoid calling OSX framework stuff from here
// (thus avoiding the extra linkage)
static char*(*os_password_store_function)(const char *host,
                                          const char *username,
                                          const char *password)= NULL;
static char*(*os_password_retrieve_function)(const char *host,
                                             const char *username,
                                             const char *password_data)= NULL;

/*
 * Public functions
 */

///////////////////////////////////////////////////////////////////////////////
/** @brief get int value stored at xpath of the document
    @param doc document where int is stored
    @param xpath path to node with value (it MUST have single child)
    @return found value

    calls atoi_and_free()
*//////////////////////////////////////////////////////////////////////////////
int get_xpath_int_value(xmlDocPtr doc, const char *xpath)
{
  xmlXPathObjectPtr result= xmlXPathEval((xmlChar*)xpath, context);
  xmlNodeSetPtr node_set= result->nodesetval;
  int res= 0;
  if(node_set && node_set->nodeTab)
    res= atoi_and_free(xmlNodeListGetString(doc,
                                     node_set->nodeTab[0]->children,
                                     1));
  //assert(node_set->nodeNr == 1);
  xmlXPathFreeObject(result);
  return res;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief create a new connections struct and initialize it's fields
    @return created connections struct
*//////////////////////////////////////////////////////////////////////////////
MYX_USER_CONNECTIONS * new_user_connections(void)
{
  MYX_USER_CONNECTIONS * user_conns= 
    (MYX_USER_CONNECTIONS*)g_malloc(sizeof(MYX_USER_CONNECTIONS));
  user_conns->last_connection= -1;
  user_conns->user_connections= NULL;
  user_conns->user_connections_num= 0;
  return user_conns;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief create a new IM connections struct and initialize it's fields
    @return created connections struct
*//////////////////////////////////////////////////////////////////////////////
MYX_IM_CONNECTIONS * new_im_connections(void)
{
  MYX_IM_CONNECTIONS * im_conns= 
    (MYX_IM_CONNECTIONS*)g_malloc(sizeof(MYX_IM_CONNECTIONS));  
  im_conns->im_connections= NULL;
  im_conns->im_connections_num= 0;
  im_conns->last_im_connection= -1;
  return im_conns;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief load predefined connection stored in xml-file (utf-8 encoded)
    @ingroup Connection_management

    @param filename path to xml-file with stored connections
    @param error_code returned error code, <BR>
           possible values are:
             - \link MYX_LIB_ERROR::MYX_NO_ERROR MYX_NO_ERROR \endlink
             - \link MYX_LIB_ERROR::MYX_XML_PARSE_ERROR 
                                                  MYX_XML_PARSE_ERROR \endlink
             - \link MYX_LIB_ERROR::MYX_XML_EMPTY_DOCUMENT 
                                               MYX_XML_EMPTY_DOCUMENT \endlink
             - \link MYX_LIB_ERROR::MYX_XML_NO_VALID_DOCUMENT
                  MYX_XML_NO_VALID_DOCUMENT \endlink 
                  if the root of the document wasn't equal to
                  "user_connections"
    @return loaded MYX_USER_CONNECTIONS

    uses DOM model for parsing <BR>
    calls 
      get_xpath_int_value() and
      parse_user_connection()
*//////////////////////////////////////////////////////////////////////////////
MYX_USER_CONNECTIONS* myx_load_user_connections(const char *filename,
                                                MYX_LIB_ERROR *error_code)
{
  MYX_USER_CONNECTIONS *user_conns;
  xmlDocPtr doc;
  xmlNodePtr root;
  int count, i;
  xmlNodeSetPtr node_set;
  xmlXPathObjectPtr result;
  enum myx_password_storage_type pwd_storage_type;
  char* local_filename;

  *error_code= MYX_NO_ERROR;

  local_filename= g_filename_from_utf8(filename, -1, NULL, NULL, NULL);
  if (local_filename == NULL)
  {
    *error_code= MYX_CHARSET_CONVERSION_ERROR;
    return NULL;
  };

  if (!file_exists(local_filename))
    return new_user_connections();

  doc = myx_xmlParseFile(filename);
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
    
  if (xmlStrcmp(root->name, (const xmlChar *) "user_connections"))
  {
    *error_code= MYX_XML_NO_VALID_DOCUMENT;
    xmlFreeDoc(doc);
    return NULL;
  }

  /*create a new context for use with the XPath functions*/
  context= xmlXPathNewContext(doc); 
  user_conns= (MYX_USER_CONNECTIONS*)g_malloc(sizeof(MYX_USER_CONNECTIONS));

  user_conns->last_connection=
    get_xpath_int_value(doc,"/user_connections/last_connection");
  pwd_storage_type=
    get_xpath_int_value(doc,"/user_connections/password_storage_type");

  /*retrieve the user_connections*/
  result= xmlXPathEval((xmlChar*)"/user_connections/user_connection", context);
  node_set= result->nodesetval;

  count= node_set->nodeNr;
  user_conns->user_connections_num= count;
  if (!count)
  {
    user_conns->user_connections= NULL;
  }
  else
  {
    user_conns->user_connections=
      (MYX_USER_CONNECTION*)g_malloc(sizeof(MYX_USER_CONNECTION) * count);
    for (i=0; i< count; i++)
    {
      parse_user_connection(doc,node_set->nodeTab[i],
                            user_conns->user_connections+i, pwd_storage_type);
    }
  }

  xmlXPathFreeObject(result);
  xmlXPathFreeContext(context);
  xmlFreeDoc(doc);

  return user_conns;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief load predefined IM connection stored in the IM xml-file (utf-8 encoded)
    @ingroup Connection_management

    @param filename path to xml-file with stored connections
    @param error_code returned error code, <BR>
           possible values are:
             - \link MYX_LIB_ERROR::MYX_NO_ERROR MYX_NO_ERROR \endlink
             - \link MYX_LIB_ERROR::MYX_XML_PARSE_ERROR 
                                                  MYX_XML_PARSE_ERROR \endlink
             - \link MYX_LIB_ERROR::MYX_XML_EMPTY_DOCUMENT 
                                               MYX_XML_EMPTY_DOCUMENT \endlink
             - \link MYX_LIB_ERROR::MYX_XML_NO_VALID_DOCUMENT
                  MYX_XML_NO_VALID_DOCUMENT \endlink 
                  if the root of the document wasn't equal to
                  "user_connections"
    @return loaded MYX_USER_CONNECTIONS

    uses DOM model for parsing <BR>
    calls 
      get_xpath_int_value() and
      parse_im_connection()
*//////////////////////////////////////////////////////////////////////////////
MYX_IM_CONNECTIONS* myx_load_im_connections(const char *filename,
                                              MYX_LIB_ERROR *error_code)
{
  MYX_IM_CONNECTIONS *im_conns;
  xmlDocPtr doc;
  xmlNodePtr root;
  int count, i;
  xmlNodeSetPtr node_set;
  xmlXPathObjectPtr result;
  char* local_filename;

  *error_code= MYX_NO_ERROR;

  local_filename= g_filename_from_utf8(filename, -1, NULL, NULL, NULL);
  if (local_filename == NULL)
  {
    *error_code= MYX_CHARSET_CONVERSION_ERROR;
    return NULL;
  };

  if (!file_exists(local_filename))
    return new_im_connections();

  doc = myx_xmlParseFile(filename);
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
    
  if (xmlStrcmp(root->name, (const xmlChar *) "im_connections"))
  {
    *error_code= MYX_XML_NO_VALID_DOCUMENT;
    xmlFreeDoc(doc);
    return NULL;
  }

  /*create a new context for use with the XPath functions*/
  context= xmlXPathNewContext(doc); 
  im_conns= (MYX_IM_CONNECTIONS*)g_malloc(sizeof(MYX_IM_CONNECTIONS));
  im_conns->last_im_connection= get_xpath_int_value(doc,"/im_connections/last_im_connection");
  
  /* retrieve the im_connections */
  result= xmlXPathEval((xmlChar*)"/im_connections/im_connection", context);
  node_set= result->nodesetval;

  count= node_set->nodeNr;
  im_conns->im_connections_num= count;

  if (count == 0)
  {
    im_conns->im_connections= NULL;
  }
  else
  {
    im_conns->im_connections=
      (MYX_IM_CONNECTION *)g_malloc(sizeof(MYX_IM_CONNECTION) * count);
    for (i=0; i< count; i++)
    {
      parse_im_connection(doc,node_set->nodeTab[i], im_conns->im_connections+i);
    }
  }

  xmlXPathFreeObject(result);
  xmlXPathFreeContext(context);
  xmlFreeDoc(doc);

  return im_conns;
}


///////////////////////////////////////////////////////////////////////////////
/** @brief store connections to xml-file
    @ingroup Connection_management

    @param user_connections connections to store
    @param pwd_storage_type method of storing password
    @param filename path to file to store in
    @return -1 if file was not written, else 0

    calls NewTextChild_int_content to store int values
*//////////////////////////////////////////////////////////////////////////////
int myx_store_user_connections(MYX_USER_CONNECTIONS* user_connections,
                               MYX_PASSWORD_STORAGE_TYPE pwd_storage_type,
                               const char *filename)
{
  xmlDocPtr doc;
  xmlNodePtr root_node, ucon_node;
  int res;
  MYX_USER_CONNECTION *ucon, *ucon_end;

  if (user_connections == NULL) return -1;

  doc= xmlNewDoc((xmlChar*)"1.0");
  root_node= doc->children= xmlNewDocRawNode(doc,NULL,
                                             (xmlChar*)"user_connections", NULL);

  NewTextChild_int_content(root_node,NULL,(xmlChar*)"last_connection",
                           user_connections->last_connection);
  NewTextChild_int_content(root_node,NULL,(xmlChar*)"password_storage_type",
                           pwd_storage_type);

  ucon= user_connections->user_connections;
  ucon_end= ucon + user_connections->user_connections_num;
  for (; ucon != ucon_end; ucon++)
  {
    //char *tmp;

    ucon_node= xmlNewTextChild(root_node, NULL, (xmlChar*)"user_connection", NULL);
    store_user_connection_attributes(ucon_node, ucon, pwd_storage_type);
//#define STRNULL(s) ((s)?(s):"")
//    xmlNewTextChild(ucon_node, NULL, (xmlChar*)"connection_name",
//                    (xmlChar*)STRNULL(ucon->connection_name));
//    xmlNewTextChild(ucon_node, NULL, (xmlChar*)"username", (xmlChar*)ucon->username);
//    xmlNewTextChild(ucon_node, NULL, (xmlChar*)"hostname", (xmlChar*)ucon->hostname);
//    NewTextChild_int_content(ucon_node, NULL, (xmlChar*)"port", ucon->port);
//    xmlNewTextChild(ucon_node, NULL, (xmlChar*)"schema", (xmlChar*)ucon->schema);
//
//    node= xmlNewTextChild(ucon_node, NULL, (xmlChar*)"advanced_options", NULL);
//    for (j=0; j < (int)ucon->advanced_options_num; j++)
//      xmlNewTextChild(node, NULL, (xmlChar*)"advanced_option",(xmlChar*)ucon->advanced_options[j]);
//
//    xmlNewTextChild(ucon_node,NULL,(xmlChar*)"storage_path",(xmlChar*)STRNULL(ucon->storage_path));
//    xmlNewTextChild(ucon_node,NULL,(xmlChar*)"notes", (xmlChar*)STRNULL(ucon->notes));
//#undef STRNULL
//    NewTextChild_int_content(ucon_node, NULL, (xmlChar*)"connection_type",
//                             ucon->connection_type);
//    NewTextChild_int_content(ucon_node,NULL,(xmlChar*)"storage_type",ucon->storage_type);
//
//    tmp= (char*)save_password(ucon, (unsigned char*)ucon->password, pwd_storage_type);
//    xmlNewTextChild(ucon_node, NULL, (xmlChar*)"password", (xmlChar*)tmp);
//    g_free(tmp);
  }

  res= myx_xmlSaveFile(filename, doc);
  xmlFreeDoc(doc);
  return (res == -1) ? -1 : 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief store IM connections to xml-file
    @ingroup Connection_management

    @param im_connections connections to store
    @param pwd_storage_type method of storing password
    @param filename path to file to store in
    @return -1 if file was not written, else 0

    calls NewTextChild_int_content to store int values
*//////////////////////////////////////////////////////////////////////////////
MYX_PUBLIC_FUNC int myx_store_im_connections(MYX_IM_CONNECTIONS *im_connections, 
                                             MYX_PASSWORD_STORAGE_TYPE pwd_storage_type, const char *filename)
{
  xmlDocPtr doc;
  xmlNodePtr root_node, ucon_node, imcon_node, mysqldcon_list_node, mysqldcon_node;
  int res;
  MYX_IM_CONNECTION *imcon, *imcon_end;

  if (im_connections == NULL) return -1;

  doc= xmlNewDoc((xmlChar*)"1.0");
  root_node= doc->children= xmlNewDocRawNode(doc,NULL,
                                             (xmlChar*)"im_connections", NULL);

  NewTextChild_int_content(root_node,NULL,(xmlChar*)"last_connection",
                           im_connections->last_im_connection);
  //NewTextChild_int_content(root_node,NULL,(xmlChar*)"password_storage_type",
  //                         pwd_storage_type);

  imcon= im_connections->im_connections;
  imcon_end= imcon + im_connections->im_connections_num;
  for (; imcon != imcon_end; imcon++)
  {
    MYX_MYSQLD_CONNECTION *mysqld_con= imcon->mysqlds->mysqld_connections;
    MYX_MYSQLD_CONNECTION *mysqld_con_end= mysqld_con + imcon->mysqlds->mysqld_connections_num;

    imcon_node= xmlNewTextChild(root_node, NULL, (xmlChar*)"im_connection", NULL);
    ucon_node= xmlNewTextChild(imcon_node, NULL, (xmlChar*)"user_connection", NULL);
    store_user_connection_attributes(ucon_node, imcon->connection, pwd_storage_type);

    mysqldcon_list_node= xmlNewTextChild(imcon_node, NULL, (xmlChar*)"mysqld_connections", NULL);

    for(; mysqld_con != mysqld_con_end; mysqld_con++)
    {
      mysqldcon_node= xmlNewTextChild(mysqldcon_list_node, NULL, (xmlChar*)"mysqld_connection", NULL);
      ucon_node= xmlNewTextChild(mysqldcon_node, NULL, (xmlChar*)"user_connection", NULL);
      store_user_connection_attributes(ucon_node, mysqld_con->connection, pwd_storage_type);
    }
  }

  res= myx_xmlSaveFile(filename, doc);
  xmlFreeDoc(doc);
  return (res == -1) ? -1 : 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for user connections
    @ingroup Connection_management
    @param user_connections user connections to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_user_connections(MYX_USER_CONNECTIONS *user_connections)
{
  if (user_connections != NULL)
  {
    MYX_USER_CONNECTION *ucon= user_connections->user_connections;
    MYX_USER_CONNECTION *ucon_end= ucon+user_connections->user_connections_num;
    for (; ucon != ucon_end; ucon++)
      myx_free_user_connection_content(ucon);
    g_free(user_connections);
  }
  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for IM connections
    @ingroup Connection_management
    @param im_connections IM connections to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
MYX_PUBLIC_FUNC int myx_free_im_connections(MYX_IM_CONNECTIONS *im_connections)
{
  unsigned i, j;
  
  if(im_connections == NULL)
    return 0;

  for (i= 0; i < im_connections->im_connections_num; i++)
  {
    for (j= 0; j < im_connections->im_connections[i].mysqlds->mysqld_connections_num; j++)
    {
      myx_free_user_connection_content(im_connections->im_connections[i].mysqlds->mysqld_connections[j].connection);
      g_free(im_connections->im_connections[i].mysqlds->mysqld_connections[j].connection);
    }
    g_free(im_connections->im_connections[i].mysqlds);
  }
  g_free(im_connections->im_connections);
  g_free(im_connections);

  return 0;
}

// returns the value of to arg
static MYX_USER_CONNECTION * duplicate_user_connection_data(MYX_USER_CONNECTION *to, const MYX_USER_CONNECTION *from)
{
#define DUPN(s) (s != NULL ? xmlMemStrdup(s) : NULL)
    
    to->connection_name= DUPN(from->connection_name);
    to->username= DUPN(from->username);
    to->password= DUPN(from->password);
    to->hostname= DUPN(from->hostname);
    to->port= from->port;
    to->schema= DUPN(from->schema);
    to->advanced_options_num= from->advanced_options_num;
    if (to->advanced_options_num <= 0)
    {
      to->advanced_options= NULL;
    }
    else
    {
      char **option= (char**)g_malloc(sizeof(char*)*to->advanced_options_num);
      char **option_end= option + to->advanced_options_num;
      char **orig_option= from->advanced_options;
      to->advanced_options= option;
      for (; option != option_end ; option++, orig_option++)
        *option= xmlMemStrdup(*orig_option);
    } 
    to->storage_path= DUPN(from->storage_path);
    to->notes= DUPN(from->notes);
    to->connection_type= from->connection_type;
    to->storage_type= from->storage_type;

#undef DUPN
  
  return to;
}

static MYX_MYSQLD_CONNECTION *duplicate_mysqld_connection_data(MYX_MYSQLD_CONNECTION *to, const MYX_MYSQLD_CONNECTION *from)
{
  if(from->connection != NULL)
  {
    to->connection= g_malloc(sizeof(*to->connection));
    duplicate_user_connection_data(to->connection, from->connection);
  }
  else
  {
    to->connection= NULL;
  }
  return to;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief add a new connection to the set of connection
    @ingroup Connection_management
    @param user_connections set of connection to add to
    @param new_connection new connection to add
    @return index of the new connection 
               if there wasn't a connection with the same name
               or index of the existed connection with the same name
*//////////////////////////////////////////////////////////////////////////////
int myx_add_user_connection(MYX_USER_CONNECTIONS **user_connections,
                            MYX_USER_CONNECTION *new_connection)
{
  unsigned int i;
  int found= 0;

  if (!(*user_connections))
  {
    *user_connections= new_user_connections();
  }
  else
  {
    for (i= 0; i < (*user_connections)->user_connections_num; i++)
    {
      if ((*user_connections)->user_connections[i].connection_name &&
          strcmp2((*user_connections)->user_connections[i].connection_name,
                  new_connection->connection_name)==0)
      {
        found= 1;
        break;
      }
    }
  }
  if (!found)
  {
    MYX_USER_CONNECTION *ptr;
    unsigned int count= (*user_connections)->user_connections_num + 1;

    (*user_connections)->user_connections_num= count;
    (*user_connections)->user_connections= (MYX_USER_CONNECTION*)
      g_realloc((*user_connections)->user_connections,
                sizeof(MYX_USER_CONNECTION)*count);
    ptr= (*user_connections)->user_connections + count - 1;

    duplicate_user_connection_data(ptr, new_connection);

//#define DUPN(s) (s != NULL ? xmlMemStrdup(s) : NULL)
//    
//    ptr->connection_name= DUPN(new_connection->connection_name);
//    ptr->username= DUPN(new_connection->username);
//    ptr->password= DUPN(new_connection->password);
//    ptr->hostname= DUPN(new_connection->hostname);
//    ptr->port= new_connection->port;
//    ptr->schema= DUPN(new_connection->schema);
//    ptr->advanced_options_num= new_connection->advanced_options_num;
//    if (ptr->advanced_options_num <= 0)
//    {
//      ptr->advanced_options= NULL;
//    }
//    else
//    {
//      char **option= (char**)g_malloc(sizeof(char*)*ptr->advanced_options_num);
//      char **option_end= option + ptr->advanced_options_num;
//      char **orig_option= new_connection->advanced_options;
//      ptr->advanced_options= option;
//      for (; option != option_end ; option++, orig_option++)
//        *option= xmlMemStrdup(*orig_option);
//    } 
//    ptr->storage_path= DUPN(new_connection->storage_path);
//    ptr->notes= DUPN(new_connection->notes);
//    ptr->connection_type= new_connection->connection_type;
//    ptr->storage_type= new_connection->storage_type;
//
//#undef DUPN

    return (*user_connections)->user_connections_num-1;
  }

  return i;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief add a new connection to the set of IM connections
    @ingroup Connection_management
    @param im_connections set of connection to add to
    @param new_connection new connection to add
    @return index of the new connection 
               if there wasn't a connection with the same name
               or index of the existed connection with the same name
*//////////////////////////////////////////////////////////////////////////////
MYX_PUBLIC_FUNC int myx_add_im_connection(MYX_IM_CONNECTIONS **im_connections, MYX_IM_CONNECTION *new_connection)
{
  unsigned int i, j;
  int found= 0;

  if (!(*im_connections))
  {
    *im_connections= new_im_connections();
  }
  else
  {
    for (i= 0; i < (*im_connections)->im_connections_num; i++)
    {
      if ((*im_connections)->im_connections[i].connection->connection_name &&
        strcmp2((*im_connections)->im_connections[i].connection->connection_name,
          new_connection->connection->connection_name)==0)
      {
        found= 1;
        break;
      }
    }
  }

  if (!found)
  {
    MYX_IM_CONNECTION *ptr;
    unsigned int count= (*im_connections)->im_connections_num + 1;

    (*im_connections)->im_connections_num= count;
    (*im_connections)->im_connections= (MYX_IM_CONNECTION *)
      g_realloc((*im_connections)->im_connections,
                sizeof(MYX_IM_CONNECTION)*count);
    ptr= (*im_connections)->im_connections + count - 1;
    ptr->connection= (MYX_USER_CONNECTION *)g_malloc(sizeof(*ptr->connection));

    duplicate_user_connection_data(ptr->connection, new_connection->connection);
    ptr->mysqlds= g_malloc(sizeof(MYX_MYSQLD_CONNECTIONS));
    ptr->mysqlds->last_connection= new_connection->mysqlds->last_connection;
    ptr->mysqlds->mysqld_connections_num= new_connection->mysqlds->mysqld_connections_num;
    if(ptr->mysqlds->mysqld_connections_num > 0)
    {
      ptr->mysqlds->mysqld_connections= g_malloc(sizeof(MYX_MYSQLD_CONNECTION)*new_connection->mysqlds->mysqld_connections_num);

      for(j= 0; j < ptr->mysqlds->mysqld_connections_num; j++)
        duplicate_mysqld_connection_data(&ptr->mysqlds->mysqld_connections[j], &new_connection->mysqlds->mysqld_connections[j]);
    }
    else
    {
      ptr->mysqlds= NULL;
    }

    return (*im_connections)->im_connections_num-1;
  }

  return i;
}

MYX_PUBLIC_FUNC int myx_add_mysqld_connection(MYX_IM_CONNECTION **im_connection, MYX_MYSQLD_CONNECTION *new_connection)
{
  unsigned int i;
  int found= 0;

  for (i= 0; i < (*im_connection)->mysqlds->mysqld_connections_num; i++)
  {
    if ((*im_connection)->mysqlds->mysqld_connections[i].connection->connection_name &&
      strcmp2((*im_connection)->mysqlds->mysqld_connections[i].connection->connection_name,
        new_connection->connection->connection_name)==0)
    {
      found= 1;
      break;
    }
  }

  if (!found)
  {
    MYX_MYSQLD_CONNECTION *ptr;
    unsigned int count= (*im_connection)->mysqlds->mysqld_connections_num + 1;

    (*im_connection)->mysqlds->mysqld_connections_num= count;
    (*im_connection)->mysqlds->mysqld_connections= (MYX_MYSQLD_CONNECTION *)
      g_realloc((*im_connection)->mysqlds->mysqld_connections,
                sizeof(MYX_MYSQLD_CONNECTION)*count);
    ptr= (*im_connection)->mysqlds->mysqld_connections + count - 1;

    duplicate_user_connection_data(ptr->connection, new_connection->connection);

    return (*im_connection)->mysqlds->mysqld_connections_num - 1;
  }

  return i;

}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for user connection
    @ingroup Connection_management
    @param uc user connection to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_user_connection_content(MYX_USER_CONNECTION *uc)
{
  char **aopt_pos, **aopt_end;

  xmlFree(uc->connection_name);
  xmlFree(uc->username);
  xmlFree(uc->password);
  xmlFree(uc->hostname);
  xmlFree(uc->schema);

  aopt_pos= uc->advanced_options;
  aopt_end= aopt_pos + uc->advanced_options_num;
  for (; aopt_pos != aopt_end; aopt_pos++)
    xmlFree(*aopt_pos);
  xmlFree(uc->storage_path);
  xmlFree(uc->notes);

  g_free(uc->advanced_options);
  return 0;
}

MYX_MYSQLD_CONNECTIONS *myx_new_mysqld_connections(int nconnections)
{
  MYX_MYSQLD_CONNECTIONS *retval= g_malloc(sizeof(MYX_MYSQLD_CONNECTIONS));
  retval->mysqld_connections_num= nconnections;
  retval->mysqld_connections= g_malloc(sizeof(MYX_MYSQLD_CONNECTION)*nconnections);
  return retval;
}

void myx_free_mysqld_connections(MYX_MYSQLD_CONNECTIONS *mcs)
{
  g_free(mcs);
}


/*
 * Private functions
 */

///////////////////////////////////////////////////////////////////////////////
/** @brief Pseudo-encryption
    @param str string to encrypt
    @return encrypted string
    simple binary invertion for now

    This function is used for encrypting as well as decrypting
*//////////////////////////////////////////////////////////////////////////////
static xmlChar *obscure(const xmlChar* str)
{
  if (!str)
  {
    return 0;
  }
  else
  {
    gulong len= (gulong)strlen((char*)str);
    xmlChar *result= (xmlChar*)g_malloc(len+1);
    xmlChar *res_pos= result;
    xmlChar *res_end= result + len;
    for (; res_pos != res_end; res_pos++, str++)
      *res_pos= ~(*str);
    *res_pos= 0;
    return result;
  }
}


void set_os_specific_password_functions(char*(*store_func)(const char *host,
                                                           const char *username,
                                                           const char *password),
                                        char*(*retrieve_func)(const char *host,
                                                              const char *username,
                                                              const char *password_data))
{
  os_password_store_function= store_func;
  os_password_retrieve_function= retrieve_func;
}


#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
///////////////////////////////////////////////////////////////////////////////
/** @brief Crypt uprotected password
    @param cipher uprotected password
    @return crypted string
*//////////////////////////////////////////////////////////////////////////////
static xmlChar* win_load_password(xmlChar *cipher)
{
  DATA_BLOB data_in;
  DATA_BLOB data_out;
  xmlChar *plaintext;

  data_in.pbData= hex_decode(cipher, (int*) &data_in.cbData);

  if (! CryptUnprotectData(&data_in,
                           NULL,
                           NULL,        // Optional entropy
                           NULL,        // Reserved
                           NULL,
                           0,
                           &data_out))
  {
    return NULL;
  }

  g_free(data_in.pbData);

  plaintext= g_strdup(data_out.pbData);
  LocalFree(data_out.pbData);

  return plaintext;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief Crypt protected password
    @param protected password
    @return crypted string

    Stores the password using Microsoft's DPAPI (which is a subset of the 
    Cryptographic-API)
    This means that the encryption key is derived from the user-credentials.
    Only the same user can decrypt it.
    In case of an error NULL is returned.
*//////////////////////////////////////////////////////////////////////////////
static xmlChar* win_store_password(xmlChar *password)
{
  DATA_BLOB data_in;
  DATA_BLOB data_out;
  xmlChar *cipher;

  data_in.pbData= password;
  // include the trailing 0 so we get a null-terminated string back 
  //  after decrypting
  data_in.cbData= (DWORD) strlen(password) +1;
  
  if (!CryptProtectData(&data_in,           
                                                  // A description string:
                        L"This is the description string.", 
                        NULL,                     // Optional entropy not used.
                        NULL,                     // Reserved.
                        NULL,                      
                        CRYPTPROTECT_UI_FORBIDDEN,
                        &data_out))
    return NULL;

  //hex-encode it so we get a string that we can save in
  //an xml-file.
  cipher= hex_encode(data_out.pbData, data_out.cbData);
  LocalFree(data_out.pbData);

  return cipher;
}
#endif

///////////////////////////////////////////////////////////////////////////////
/** @brief used for retreiving of password from xml-plaintext
    @param password string contained password to retrieve
    @param storage_type method of retreiving
    @return retrieved password (allocated by xmlStrdup)

    calls hex_decode() and obscure() to retrieve encrypted password <BR>
*//////////////////////////////////////////////////////////////////////////////
static xmlChar *retrieve_password(MYX_USER_CONNECTION *conn, xmlChar *password,
                                  enum myx_password_storage_type storage_type)
{
  xmlChar *tmp, *tmp2, *plaintext_password;

  if (!password && storage_type != MYX_PASSWORD_OS_SPECIFIC) return NULL;

  switch (storage_type)
  {
  case MYX_PASSWORD_NOT_STORED: return NULL;
  case MYX_PASSWORD_PLAINTEXT:  return xmlStrdup(password);

  case MYX_PASSWORD_OBSCURED:
    {
      //since the old hex_encode function produced a single 0 at
      //the end of the str (instead of two), remove it
      unsigned int password_len= (unsigned int)strlen((char*)password);
      if (password_len % 2 != 0 && password[password_len-1] == '0')
        password[password_len-1]= 0;
    }
    tmp= (xmlChar*)hex_decode((char*)password, NULL);
    tmp2= obscure(tmp);
    plaintext_password= xmlStrdup(tmp2);
    g_free(tmp);
    g_free(tmp2);
    return plaintext_password;

  case MYX_PASSWORD_OS_SPECIFIC:
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
    if (!password) return NULL;
    tmp= win_load_password(password);
    plaintext_password= xmlStrdup(tmp);
    g_free(tmp);
    return plaintext_password;
#else
    if (os_password_retrieve_function)
    {
      xmlChar *tmp;
      char *inst;
      inst= g_strdup_printf("%s:%i", conn->hostname, conn->port);
      tmp= (xmlChar*)(*os_password_retrieve_function)(inst, conn->username, (char*)password);
      g_free(inst);
      return tmp;
    }
#endif
  }

  return xmlStrdup(password);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief store password to xml string
    @param password password to store
    @param storage_type method of storing
    @return glib allocated string

    calls obscure() and hex_encode() to encrypt password
*//////////////////////////////////////////////////////////////////////////////
static xmlChar* save_password(MYX_USER_CONNECTION *conn, xmlChar *password,
                              enum myx_password_storage_type storage_type)
{
  if (!password || !(*password)) return NULL;

  switch (storage_type)
  {
  case MYX_PASSWORD_NOT_STORED: return NULL;
  case MYX_PASSWORD_PLAINTEXT:  return xmlStrdup(password); 

  case MYX_PASSWORD_OBSCURED:
    {
      xmlChar *tmp= obscure(password);
      xmlChar *encrypted_password= (xmlChar*)hex_encode((char*)tmp,-1);
      g_free(tmp);
      return encrypted_password;
    }
  case MYX_PASSWORD_OS_SPECIFIC:
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
    return win_store_password(password);
#else
    if (os_password_store_function)
    {
      xmlChar *tmp;
      char *inst;
      inst= g_strdup_printf("%s:%i", conn->hostname, conn->port);
      tmp= (xmlChar*)(*os_password_store_function)(inst, conn->username, (char*)password);
      g_free(inst);
      return tmp;
    }
#endif
  }
  return xmlStrdup(password);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief used for parsing a user_connection from the xml-dom-tree
    @param doc parsed xml-document
    @param user_conn Node in parsed xml-document contained the Connection
    @param out Connection struct to fill
    @param pwd_storage_type method of reading of passsord

    is called from myx_load_user_connections() <BR>
    calls try_to_get_string_field(), try_to_get_int_field(), 
        retrieve_password()
*//////////////////////////////////////////////////////////////////////////////
static 
  void parse_user_connection(xmlDocPtr doc, xmlNodePtr user_conn,
                             MYX_USER_CONNECTION *out,
                             enum myx_password_storage_type pwd_storage_type)
{
  xmlNodePtr cur;
  int i;
  xmlXPathObjectPtr result;
  xmlNodeSetPtr node_set;

  assert( !xmlStrcmp(user_conn->name, (xmlChar*)"user_connection"));
  memset(out, 0, sizeof(MYX_USER_CONNECTION));
  for (cur= user_conn->children; cur != NULL; cur= cur->next)
  {
    try_to_get_string_field(doc,cur,"connection_name",&out->connection_name);
    try_to_get_string_field(doc,cur,"username",       &out->username);
    try_to_get_string_field(doc,cur,"hostname",       &out->hostname);
    try_to_get_string_field(doc,cur,"schema",         &out->schema);
    try_to_get_string_field(doc,cur,"storage_path",   &out->storage_path);
    try_to_get_string_field(doc,cur,"notes",          &out->notes);

    try_to_get_int_field(doc,cur,"port",           (int*)&out->port);
    try_to_get_int_field(doc,cur,"connection_type",
                         (int*)&out->connection_type);
    try_to_get_int_field(doc,cur,"storage_type",   (int*)&out->storage_type);

    if ( !xmlStrcmp(cur->name, (xmlChar*)"password") )  
    {
      xmlChar *tmp;
      if (out->password)
        xmlFree(out->password);
      tmp= xmlNodeListGetString(doc, cur->children,1);
      out->password= (char*)retrieve_password(out, tmp, pwd_storage_type);
      xmlFree(tmp);
    }
  }

  context->node= user_conn;
  result= xmlXPathEval((xmlChar*)"advanced_options/advanced_option", context);
  node_set= result->nodesetval;
  out->advanced_options_num= !node_set ? 0 : node_set->nodeNr;
  if (out->advanced_options_num == 0)
  {
    out->advanced_options= NULL;
  }
  else
  {
    out->advanced_options= (char**)g_malloc(sizeof(char*) * node_set->nodeNr);
    for (i=0; i< (int)out->advanced_options_num; i++)
    {
      out->advanced_options[i]= (char*)
        xmlNodeListGetString(doc, node_set->nodeTab[i]->children, 1);
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
/** @brief used for parsing a im_connection from the xml-dom-tree
    @param doc parsed xml-document
    @param im_conn Node in parsed xml-document contained the Connection
    @param out Connection struct to fill

*//////////////////////////////////////////////////////////////////////////////

static 
  void parse_im_connection(xmlDocPtr doc, xmlNodePtr im_conn,
                           MYX_IM_CONNECTION *out)
{
  xmlNodePtr cur;
  //xmlXPathObjectPtr result;
  //xmlNodeSetPtr node_set;
  enum myx_password_storage_type pwd_storage_type;

  assert( !xmlStrcmp(im_conn->name, (xmlChar*)"im_connection"));

  memset(out, 0, sizeof(MYX_IM_CONNECTION));

  cur= try_to_get_child(doc, im_conn, "user_connection");
  if(cur != NULL)
  {
    out->connection= (MYX_USER_CONNECTION *)g_malloc(sizeof(*out->connection));
    // todo: get real password storage type
    parse_user_connection(doc, cur, out->connection, MYX_PASSWORD_OBSCURED);  
  }

  cur= try_to_get_child(doc, im_conn, "mysqld_connections");
  if(cur != NULL)
  {
    xmlNodePtr child;
    out->mysqlds= g_malloc(sizeof(MYX_MYSQLD_CONNECTIONS));
    out->mysqlds->mysqld_connections_num= get_child_count(cur, (xmlChar*)"mysqld_connection");
    pwd_storage_type= MYX_PASSWORD_OBSCURED;
    //try_to_get_int_field(doc, cur, "password_storage_type", (int*)&pwd_storage_type);

    if(out->mysqlds->mysqld_connections_num > 0)
    {
      int i= 0;
      out->mysqlds->mysqld_connections= (MYX_MYSQLD_CONNECTION *)g_malloc(sizeof(*out->mysqlds->mysqld_connections) * out->mysqlds->mysqld_connections_num);
      for(child= cur->children; child != NULL; child= child->next)
      {
        xmlNodePtr child_user_conn= try_to_get_child(doc, child, "user_connection");
        if(!child_user_conn)
          continue;
        out->mysqlds->mysqld_connections[i].connection= (MYX_USER_CONNECTION *)g_malloc(sizeof(*out->mysqlds->mysqld_connections[i].connection));
        parse_user_connection(doc, child_user_conn, out->mysqlds->mysqld_connections[i].connection, pwd_storage_type);
        i++;
      }
    }
  }
}

static void store_user_connection_attributes(xmlNodePtr ucon_node, MYX_USER_CONNECTION *ucon, MYX_PASSWORD_STORAGE_TYPE pwd_storage_type)
{
  int j;
  char *tmp;
  xmlNodePtr node;

#define STRNULL(s) ((s)?(s):"")
  xmlNewTextChild(ucon_node, NULL, (xmlChar*)"connection_name",
                  (xmlChar*)STRNULL(ucon->connection_name));
  xmlNewTextChild(ucon_node, NULL, (xmlChar*)"username", (xmlChar*)ucon->username);
  xmlNewTextChild(ucon_node, NULL, (xmlChar*)"hostname", (xmlChar*)ucon->hostname);
  NewTextChild_int_content(ucon_node, NULL, (xmlChar*)"port", ucon->port);
  xmlNewTextChild(ucon_node, NULL, (xmlChar*)"schema", (xmlChar*)ucon->schema);

  node= xmlNewTextChild(ucon_node, NULL, (xmlChar*)"advanced_options", NULL);
  for (j=0; j < (int)ucon->advanced_options_num; j++)
    xmlNewTextChild(node, NULL, (xmlChar*)"advanced_option",(xmlChar*)ucon->advanced_options[j]);

  xmlNewTextChild(ucon_node,NULL,(xmlChar*)"storage_path",(xmlChar*)STRNULL(ucon->storage_path));
  xmlNewTextChild(ucon_node,NULL,(xmlChar*)"notes", (xmlChar*)STRNULL(ucon->notes));
#undef STRNULL
  NewTextChild_int_content(ucon_node, NULL, (xmlChar*)"connection_type",
                            ucon->connection_type);
  NewTextChild_int_content(ucon_node,NULL,(xmlChar*)"storage_type",ucon->storage_type);

  tmp= (char*)save_password(ucon, (unsigned char*)ucon->password, pwd_storage_type);
  NewTextChild_int_content(ucon_node,NULL,(xmlChar*)"password_storage_type",
                           pwd_storage_type);
  xmlNewTextChild(ucon_node, NULL, (xmlChar*)"password", (xmlChar*)tmp);
  g_free(tmp);
}

/** @} */ // end of Connection_management_private

