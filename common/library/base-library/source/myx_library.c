/* Copyright (C) 2003, 2004, 2005 MySQL AB

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

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <winsock2.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#ifdef __GNUC__
#include <unistd.h>
#include <ctype.h>
#include "config.h"
#ifdef ENABLE_NLS
#include <libintl.h>
#endif
#endif

//#if defined (__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <my_global.h>
#include <m_ctype.h>
//#endif

#include "myx_library.h"
#include "myx_network.h"
#include "myx_util_functions.h"
#include "myx_shared_util_functions.h"


#if 0
// needed from my_global.h, cant include it because of conflicts
#ifdef __GNUC__
typedef char    pchar;          /* Mixed prototypes can take char */
typedef char    puchar;         /* Mixed prototypes can take char */
typedef char    pbool;          /* Mixed prototypes can take char */
typedef short   pshort;         /* Mixed prototypes can take short int */
typedef float   pfloat;         /* Mixed prototypes can take float */

#include <m_ctype.h>
#endif
#endif

#include <mysql_version.h>


void myx_get_mysql_version(MYSQL *mysql);
MYX_MYSQL *myx_mysql_get_private(MYSQL *mysql);

/** @defgroup bridge_to_MySQL_API_private internal stuff
 *  @ingroup bridge_to_MySQL_API */

/** @addtogroup bridge_to_MySQL_API_private
 *  @{ */

/*
 * global variables
 */

//enum myx_lib_error error_code;

static char *myx_my_cnf_path= NULL;
static char *mysql_last_error= NULL;
static int mysql_last_errno= 0;

///////////////////////////////////////////////////////////////////////////////
/** @brief parameters of mysql_real_connect()
    used for interpretation of MYX_USER_CONNECTION::advanced_options
*//////////////////////////////////////////////////////////////////////////////
typedef struct
{
  MYSQL         * mysql;
  const char    * unix_socket;
  unsigned long   client_flag;
  signed char     ansi_quotes;
  unsigned char   named_pipe;

  // ssl options
  int             use_ssl;
  const char    * ssl_cert;
  const char    * ssl_key;
  const char    * ssl_ca;

}MYSQL_REAL_CONNECT_PARAMETERS;


typedef struct advanced_option_description ADVANCED_OPTION_DESCRIPTION;

///////////////////////////////////////////////////////////////////////////////
/** @brief function that interpretate value of advanced_option as
           part of parameters for mysql_real_connect() or parameters for
           mysql_options()

    Allowed functions are:
      oi_uint(), oi_client_flag(), oi_unix_socket()
*//////////////////////////////////////////////////////////////////////////////
typedef 
     void advanced_option_interpreter(ADVANCED_OPTION_DESCRIPTION * opt,
                                      const char * value,
                                      MYSQL_REAL_CONNECT_PARAMETERS * pars);

///////////////////////////////////////////////////////////////////////////////
/** @brief description of allowed variant of
           MYX_USER_CONNECTION::advanced_options
*//////////////////////////////////////////////////////////////////////////////
struct advanced_option_description
{
  const char                  * name;      /**< name of option 
                                           (string like "<name_of_option>=") */
  int                           option;    /**< value of option parameter for
                                                         for mysql_options() */
  advanced_option_interpreter * interpret; /**< interpretation method of
                                                                option value */
};

///////////////////////////////////////////////////////////////////////////////
/** @brief interpret value as unsigned int option for mysql_options()
    @param opt description of option (opt->option is the option to set)
    @param value unsigned int value for option
    @param options struct to set option to

    function is used in MYSQL_OPTION_DESCRIPTION struct as value for
      interpret field
*//////////////////////////////////////////////////////////////////////////////
void oi_uint(ADVANCED_OPTION_DESCRIPTION * opt,
             const char * value, MYSQL_REAL_CONNECT_PARAMETERS * options)
{
  int num= atoi(value);
  mysql_options(options->mysql, opt->option, (char*)&num);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief interprets value as flag for client_flag parameter 
            of mysql_real_connect()
    @param opt description of option (opt->option is the option to set)
    @param value unused parameter (only for compatibility with type 
                                   advanced_option_interpreter)
    @param options struct to set option to

    function is used in MYSQL_OPTION_DESCRIPTION struct as value for 
      interpret field..
*//////////////////////////////////////////////////////////////////////////////
void oi_client_flag(ADVANCED_OPTION_DESCRIPTION * opt,
                    const char * value,
                    MYSQL_REAL_CONNECT_PARAMETERS * options)
{
  options->client_flag|= opt->option;
}

void oi_ssl_option(ADVANCED_OPTION_DESCRIPTION * opt,
                   const char * value,
                   MYSQL_REAL_CONNECT_PARAMETERS * options)
{
  if(strcmp(opt->name, "USE_SSL=") == 0)
  {
    options->use_ssl= 1;
    return;
  }
  if(strcmp(opt->name, "SSL_KEY=") == 0)
  {
    options->ssl_key= value;
    return;
  }
  if(strcmp(opt->name, "SSL_CERT=") == 0)
  {
    options->ssl_cert= value;
    return;
  }
  if(strcmp(opt->name, "SSL_CA=") == 0)
  {
    options->ssl_ca= value;
    return;
  }
}


///////////////////////////////////////////////////////////////////////////////
/** @brief interprets value as flag for socket_path parameter 
            of mysql_real_connect()
    @param opt description of option
    @param value unused parameter (for compatibility with type 
                                   advanced_option_interpreter only)
    @param options struct to set option to

    function is used in MYSQL_OPTION_DESCRIPTION struct as value for 
      interpret field..
*//////////////////////////////////////////////////////////////////////////////
void oi_unix_socket(ADVANCED_OPTION_DESCRIPTION * opt,
                    const char * value,
                    MYSQL_REAL_CONNECT_PARAMETERS * options)
{
  options->unix_socket= value;
}


///////////////////////////////////////////////////////////////////////////////
/** @brief interprets value as flag for enabling ansi_quotes in
            the connection.
    @param opt description of option
    @param value unused parameter (for compatibility with type 
                                   advanced_option_interpreter only)
    @param options struct to set option to

    function is used in MYSQL_OPTION_DESCRIPTION struct as value for 
      interpret field..
*//////////////////////////////////////////////////////////////////////////////
void oi_ansi_quotes(ADVANCED_OPTION_DESCRIPTION * opt,
                    const char * value,
                    MYSQL_REAL_CONNECT_PARAMETERS * options)
{
  if (value)
  {
    switch (toupper(*value))
    {
      case 'Y': options->ansi_quotes= 1; break;
      case 'N': options->ansi_quotes= 0; break;
      default:  options->ansi_quotes= -1; break;
    }
  }
  else
    options->ansi_quotes= -1;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief interprets value as flag for enabling named pipes in
            the connection.
    @param opt description of option
    @param value unused parameter (for compatibility with type
                                   advanced_option_interpreter only)
    @param options struct to set option to

    function is used in MYSQL_OPTION_DESCRIPTION struct as value for
      interpret field..
*//////////////////////////////////////////////////////////////////////////////
void oi_named_pipe(ADVANCED_OPTION_DESCRIPTION * opt,
                    const char * value,
                    MYSQL_REAL_CONNECT_PARAMETERS * options)
{
  if (value)
  {
    switch (toupper(*value))
    {
      case 'Y':
        options->named_pipe= 1;
        break;
      default:
        options->named_pipe= 0;
        break;
    };
  }
  else
    options->named_pipe= 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief descriptoins of advanced options                                  */
static ADVANCED_OPTION_DESCRIPTION myx_advanced_options[]=
{
  { "CONNECT_TIMEOUT=", MYSQL_OPT_CONNECT_TIMEOUT, oi_uint        },
  { "COMPRESS=",        CLIENT_COMPRESS,           oi_client_flag },
  { "USE_SSL=",         0,                         oi_ssl_option  },
  { "SSL_CERT=",        0,                         oi_ssl_option  },
  { "SSL_KEY=",         0,                         oi_ssl_option  },
  { "SSL_CA=",          0,                         oi_ssl_option  },  
  { "SOCKET_PATH=",     0,                         oi_unix_socket },
  { "ANSI_QUOTES=",     0,                         oi_ansi_quotes },
  { "NAMED_PIPE=",      0,                         oi_named_pipe }
};
/** @brief last descriptoin of advanced options in array 
           (is used for iteration)                                           */
ADVANCED_OPTION_DESCRIPTION *last_myx_advanced_option= 
        myx_advanced_options + 
        sizeof(myx_advanced_options)/sizeof(ADVANCED_OPTION_DESCRIPTION);

/*
 * functions
 */

int myx_init_library(const char *datadir)
{
#ifdef ENABLE_NLS
  char *path= g_strdup_printf("%s/locale", datadir);
  bindtextdomain(GETTEXT_PACKAGE, path);
  g_free(path);
  bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
  textdomain(GETTEXT_PACKAGE);
#endif
  return 0;
}


///////////////////////////////////////////////////////////////////////////////
/** @brief get version of library
    @ingroup Common_functions
    @return int version number

    uses constant libmysqlx_PUBLIC_INTERFACE_VERSION
*//////////////////////////////////////////////////////////////////////////////
int myx_get_public_interface_version()
{
  return libmysqlx_PUBLIC_INTERFACE_VERSION;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief check if version of mysql is great or equal than given
    @param mysql          connection handler
    @param major_version  given major version of mysql
    @param minor_version  given minor version of mysql
    @return 1 if version of the mysql is greater or equal than given one else 0
    @note convered by unit tests
*//////////////////////////////////////////////////////////////////////////////
int mysql_version_is_later_or_equal_than(MYSQL *mysql,
                                         int major_version, int minor_version)
{
  int mysql_major_version= myx_get_mysql_major_version(mysql);
  if (mysql_major_version > major_version)
  {
    return 1;
  }
  else if (mysql_major_version < major_version)
  {
    return 0;
  }
  else
  {
    return myx_get_mysql_minor_version(mysql) >= minor_version;
  }
}

int mysql_full_version_is_later_or_equal_than(MYSQL *mysql,
                                              int major_version, int minor_version,
                                              int patchlevel)
{
  MYX_MYSQL *priv;
  myx_get_mysql_version(mysql);
  
  priv= myx_mysql_get_private(mysql);

  if (major_version < priv->major_version)
  {
    return 1;
  }
  else if (major_version > priv->major_version)
  {
    return 0;
  }
  else
  {
    if (minor_version < priv->minor_version)
      return 1;
    else if (minor_version > priv->minor_version)
      return 0;
    else
      return patchlevel <= priv->patchlevel;
  }
}

/**
 * Determines if the current server supports case sensitve table names.
 *
 * @param mysql The mysql structure representing the current connection.
 *
 * @return 1 if the server is case sensitive, otherwise 0.
 */
int mysql_is_case_sensitive(MYSQL *mysql)
{
  MYX_MYSQL *priv= myx_mysql_get_private(mysql);

  if (priv->case_sensitive < 0)
  {
    char* var= myx_get_server_variable(mysql, "lower_case_table_names");
    priv->case_sensitive= (strcmp(var, "0") == 0) ? 1 : 0;
    g_free(var);
  }

  return priv->case_sensitive;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get path to the configuration file
            (to use in the myx_mysql_init())
    @ingroup bridge_to_MySQL_API
    @return path to the configure file
    @see myx_set_my_cnf_path(), myx_mysql_init()
*//////////////////////////////////////////////////////////////////////////////
const char *myx_get_my_cnf_path()
{
  return myx_my_cnf_path;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief specify the path to configuration file
           (to use it in the myx_mysql_init())
    @ingroup bridge_to_MySQL_API
    @param path to the configure file
    @return always 0
    @see myx_set_my_cnf_path(), myx_mysql_init()
*//////////////////////////////////////////////////////////////////////////////
int myx_set_my_cnf_path(const char *path)
{
  if (myx_my_cnf_path)
    g_free(myx_my_cnf_path);
  myx_my_cnf_path= path ? g_strdup(path) : NULL;

  return 0;
}


/*
  @note covered by unit tests
*/
MYX_MYSQL *myx_mysql_get_private(MYSQL *mysql)
{
  if (mysql)
    return (MYX_MYSQL*)((char*)mysql + sizeof(MYSQL));
  return NULL;
}


unsigned int myx_mysql_get_resultset_size_limit(MYSQL *mysql)
{
  MYX_MYSQL *data= myx_mysql_get_private(mysql);
  if (data)
    return data->resultset_memory_limit;
  return 0;
}


void myx_mysql_limit_resultset_size(MYSQL *mysql, unsigned int limitMB)
{
  MYX_MYSQL *data= myx_mysql_get_private(mysql);
  if (data)
    data->resultset_memory_limit= limitMB;
}


const char *server_args[] =
{
  "app_dummy",
  "--basedir=./embedded",
  "--datadir=./embedded/data",
  "--default-character-set=utf8",
  "--default-collation=utf8_general_ci",
  "--default-storage-engine=InnoDB",
  "--log-error=./embedded/error.log"
};
static char *server_groups[] = {
  "none",
  (char *)NULL
};

int myx_mysql_embedded_start()
{
  return mysql_server_init(sizeof(server_args) / sizeof(char *), (char **)server_args, (char **)server_groups);
}

void myx_mysql_embedded_prevent_start()
{
  mysql_server_init(-1, NULL, NULL);
}


void myx_mysql_embedded_shutdown()
{
  mysql_server_end();
}

MYSQL * myx_mysql_init_handler(int embedded)
{
  char *data= g_malloc0(sizeof(MYSQL)+sizeof(MYX_MYSQL));

  if (data)
  {
    MYSQL *mysql= (MYSQL*)data;
    MYX_MYSQL *myx_mysql= myx_mysql_get_private(mysql);

    mysql_init(mysql);
  
    if (myx_my_cnf_path)
      mysql_options(mysql, MYSQL_READ_DEFAULT_FILE, myx_my_cnf_path);

    myx_mysql->embedded= embedded;
    myx_mysql->case_sensitive= -1; // Indicate, we have this yet to determine.

    return mysql;
  }
  return NULL;
}


///////////////////////////////////////////////////////////////////////////////
/** @brief create MYSQL handler to work with MySQL API
    @ingroup bridge_to_MySQL_API

    @return prepared MYSQL struct

    calls mysql_init() and reads configuration file (via mysql_options())
     if myx_set_my_cnf_path() was called before
*//////////////////////////////////////////////////////////////////////////////
MYSQL * myx_mysql_init()
{
  return myx_mysql_init_handler(0);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief create MYSQL handler to work with MySQL API over the embedded server
    @ingroup bridge_to_MySQL_API

    @return prepared MYSQL struct

    calls mysql_init() and reads configuration file (via mysql_options())
     if myx_set_my_cnf_path() was called before
*//////////////////////////////////////////////////////////////////////////////
MYSQL * myx_mysql_embedded_init()
{
  return myx_mysql_init_handler(1);
}


void myx_mysql_set_query_hooks(MYSQL *mysql, MYX_QUERY_CALLBACK pre_hook,
                               MYX_QUERY_CALLBACK post_hook, void *client_data)
{
  MYX_MYSQL *priv= myx_mysql_get_private(mysql);
  
  if (priv)
  {
    priv->pre_query_hook= pre_hook;
    priv->post_query_hook= post_hook;
    priv->client_data= client_data;
  }
}

#ifdef CLIENT_MULTI_RESULTS
#define EXTRA_CONNECT_FLAGS CLIENT_INTERACTIVE | CLIENT_LOCAL_FILES | CLIENT_MULTI_RESULTS /*| CLIENT_MULTI_STATEMENTS*/
#else
#define EXTRA_CONNECT_FLAGS CLIENT_INTERACTIVE | CLIENT_LOCAL_FILES 
#endif


static void modify_sql_mode(MYSQL *mysql, int ansi_quotes)
{
  MYSQL_RES *res;
  MYSQL_ROW row;
  char *current_mode= NULL;

  // make sure we're not in ansi_quotes mode, unless asked to
  if (mysql_version_is_later_or_equal_than(mysql,4,1))
  {
    char *query;

    if (!mysql_query(mysql, "SELECT @@sql_mode"))
    {
      res= mysql_store_result(mysql);
      row= mysql_fetch_row(res);
      current_mode= g_strdup(row[0]);
      mysql_free_result(res);
    }
        
    if (ansi_quotes < 0) // not set, keep current value
      ;
    else if (ansi_quotes == 0) // OFF
    {
      char *ptr= strstr(current_mode, "ANSI_QUOTES");
      if (current_mode != NULL && ptr != NULL)
      {
        char **tokens= g_strsplit(current_mode, ",", 0);
        unsigned int i, j;
        for (i= j= 0; tokens[j]; )
        {
          if (strcasecmp(tokens[j], "ANSI_QUOTES")==0)
          {
            g_free(tokens[j]);
            j++;
            continue;
          }
          tokens[i++]= tokens[j++];
        }
        tokens[i]= NULL;
        g_free(current_mode);
        if (tokens[0])
        {
          current_mode= g_strjoinv(",", tokens);
          g_strfreev(tokens);
        }
        else
          current_mode= NULL;
      }
    }
    else // ON
    {
      if (!current_mode || !strstr(current_mode, "ANSI_QUOTES"))
      {
        // add ANSI_QUOTES to mode strstr
        if (current_mode && strlen(current_mode) > 0) 
          current_mode= str_g_append(current_mode, ",ANSI_QUOTES");
        else
          current_mode= str_g_append(current_mode, "ANSI_QUOTES");
      }
    }

    query= g_strdup_printf("SET SESSION sql_mode='%s'", current_mode ? current_mode : "");
    mysql_query(mysql, query);
    g_free(query);
  }
}

///////////////////////////////////////////////////////////////////////////////
/** @brief prepare a connection to a MySQL server
    @ingroup bridge_to_MySQL_API

    @param user_conn connection parameters
    @param mysql     connection handler

    @return Zero if the connection was successful, -1 if the
            connection was unsuccessful

    function parses advanced options of user_conn via myx_advanced_options <BR>
    if version of server is bigger that 4.1 function launches
    "SET CHARACTER SET utf8" query

    function first tries to connect using default schema if any,
    if it fails (e.g. because the schema doesnt exist anymore), then
    it tries to connect w/o a default schema 
*//////////////////////////////////////////////////////////////////////////////
int myx_connect_to_instance(MYX_USER_CONNECTION *user_conn, MYSQL *mysql)
{
  static my_bool true= 1;
  
  MYSQL_REAL_CONNECT_PARAMETERS connect_options= { NULL, 0, 0, -1};
  char *unix_socket= NULL;

  /* setup advanced options */
  char **advanced_option= user_conn->advanced_options;
  char **end_advanced_option= advanced_option+user_conn->advanced_options_num;

  connect_options.mysql= mysql; 
  for (; advanced_option!=end_advanced_option; advanced_option++)
  {
    char *advanced_value;
    ADVANCED_OPTION_DESCRIPTION * option_description= myx_advanced_options;
    for (; option_description!=last_myx_advanced_option; option_description++)
    {
      advanced_value= strchr(*advanced_option, '=');
      if (str_beginswith(*advanced_option, option_description->name) && (advanced_value != NULL))
      {
        advanced_value++;
        (*option_description->interpret)(option_description,advanced_value,
                                         &connect_options);
      }
    }
  }

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  if (connect_options.named_pipe)
    mysql_options(mysql, MYSQL_OPT_NAMED_PIPE, NULL);
#else
  if (!connect_options.unix_socket)
  {
    if (access("/tmp/mysql.sock", F_OK) == 0)
      unix_socket= g_strdup("/tmp/mysql.sock");
    else if (access("/var/lib/mysql/mysql.sock", F_OK) == 0)
      unix_socket= g_strdup("/var/lib/mysql/mysql.sock");
  }
#endif

#if MYSQL_VERSION_ID >= 50013
  // Explicitely enable automatic reconnection when using mysql_ping.
  // This behavior has been disabled by default since 5.0.3 and this option was introduced in 5.0.13.
  mysql_options(mysql, MYSQL_OPT_RECONNECT, &true);
#endif

  //This stmt tells the server that the parameters of
  //of mysql_real_connect are in utf8
  //mysql_options(mysql, MYSQL_SET_CHARSET_NAME, "utf8");

#if MYSQL_VERSION_ID >= 40103
  if (myx_mysql_get_private(mysql)->embedded)
    mysql_options(mysql, MYSQL_OPT_USE_EMBEDDED_CONNECTION, (char *)1);
  else
    mysql_options(mysql, MYSQL_OPT_USE_REMOTE_CONNECTION, (char *)1);
#endif

  mysql_options(mysql, MYSQL_SET_CHARSET_NAME, "utf8");

  if(connect_options.use_ssl != 0)
  {
    mysql_ssl_set(mysql, connect_options.ssl_key, connect_options.ssl_cert,
      connect_options.ssl_ca, NULL, NULL);
  }

  if (!mysql_real_connect(mysql, user_conn->hostname, user_conn->username,
                          user_conn->password, user_conn->schema,
                          user_conn->port,
                          unix_socket ? unix_socket : connect_options.unix_socket,
                          connect_options.client_flag | EXTRA_CONNECT_FLAGS))
  {
    if(!mysql_real_connect(mysql, user_conn->hostname, user_conn->username,
                          user_conn->password, NULL,
                          user_conn->port,
                          unix_socket ? unix_socket : connect_options.unix_socket,
                          connect_options.client_flag | EXTRA_CONNECT_FLAGS))
    {
      g_free(unix_socket);
      return -1;
    }
  }

  g_free(unix_socket);

  modify_sql_mode(mysql, connect_options.ansi_quotes);

  // using SET NAMES utf8;
  if ( (mysql_full_version_is_later_or_equal_than(mysql, 4, 1, 8) &&
        !     mysql_version_is_later_or_equal_than(mysql, 5, 0)) ||
       mysql_full_version_is_later_or_equal_than(mysql, 5, 0, 2))
  {
    if (mysql_query(mysql, "SET NAMES utf8"))
      return -1;
  }
  // using SET CHARACTER SET utf8;
  else if (mysql_version_is_later_or_equal_than(mysql,4,1))
  {
    if (mysql_query(mysql, "SET CHARACTER SET utf8"))
    {
      return -1;
    }
  };

  return 0;
}


///////////////////////////////////////////////////////////////////////////////
/** @brief close a previously opened connection (via myx_mysql_init())
    @ingroup bridge_to_MySQL_API

    @param mysql MYSQL struct to close
    @return always Zero

    deallocates the connection handle pointed to by mysql
*//////////////////////////////////////////////////////////////////////////////
int myx_mysql_close(MYSQL *mysql)
{
  MYX_MYSQL *priv= myx_mysql_get_private(mysql);

  mysql_close(mysql);

  // free up private data
  if (priv && priv->charset)
    g_free(priv->charset);
  
  g_free(mysql);

  return 0;
}


///////////////////////////////////////////////////////////////////////////////
/** @brief retrieves the identifier quote character used for the connection
    @ingroup bridge_to_MySQL_API

    @param mysql MYSQL struct to close
    @param detect 1 if the function should query the server for the character,
       0 if it should use a cached value, if possible
    @return ` or " dependong on SQL_MODE being set to ANSI_QUOTES

    deallocates the connection handle pointed to by mysql
*//////////////////////////////////////////////////////////////////////////////
int myx_get_mysql_quote_char(MYSQL *mysql, int detect)
{
  MYX_MYSQL *priv;
  
  priv= myx_mysql_get_private(mysql);

  if (detect || priv->quote_char==0)
  {
    MYSQL_RES *res;
    MYSQL_ROW row;
    
    if (mysql_query(mysql, "SELECT @@SQL_MODE"))
      return '`';
    
    res= mysql_store_result(mysql);
    if (!res)
      return '`';
    row= mysql_fetch_row(res);
    if (!strstr(row[0],"ANSI_QUOTES"))
      priv->quote_char= '`';
    else
      priv->quote_char= '"';

    mysql_free_result(res);
  }

  return priv->quote_char;
}


///////////////////////////////////////////////////////////////////////////////
/** @brief use schema (database) by default for connection
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @param schema name of schema to use by default

    @return Zero for success. Non-zero if an error occurred
*//////////////////////////////////////////////////////////////////////////////
int myx_use_schema(MYSQL *mysql, const char *schema)
{
  char *schema_name= myx_convert_utf8_dbstr(mysql, schema);
  int res= mysql_select_db(mysql, schema_name);

  g_free(schema_name);

  return res;
}


int myx_push_schema(MYSQL *mysql, const char *schema)
{
  char *schema_name= myx_convert_utf8_dbstr(mysql, schema);
  char *old_schema= myx_get_default_schema(mysql);
  int res= mysql_select_db(mysql, schema_name);
  MYX_MYSQL *priv= myx_mysql_get_private(mysql);
  int i= 0;
  
  if (priv->schema_stack)
    while (priv->schema_stack[i]) i++;
  
  priv->schema_stack= g_realloc(priv->schema_stack, sizeof(char*)*(i+2));
  
  priv->schema_stack[i]= old_schema;
  priv->schema_stack[i+1]= NULL;
  
  g_free(schema_name);
  
  return res;
}

int myx_pop_schema(MYSQL *mysql)
{
  char *schema_name;
  MYX_MYSQL *priv= myx_mysql_get_private(mysql);
  int i;
  
  if (!priv->schema_stack)
    return -1;
  
  for (i= 0; priv->schema_stack[i]; i++);
  
  if (i == 0)
    return -1;
  
  schema_name= myx_convert_utf8_dbstr(mysql, priv->schema_stack[i-1]);
  g_free(priv->schema_stack[i-1]);
  priv->schema_stack[i-1]= NULL;

  i= mysql_select_db(mysql, schema_name);
  
  g_free(schema_name);
  
  return i;
}


///////////////////////////////////////////////////////////////////////////////
/** @brief return the connections default schema (database)
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler

    @return name of default schema that has to be freed with g_free, 
            NULL if an error occurred
*//////////////////////////////////////////////////////////////////////////////
char * myx_get_default_schema(MYSQL *mysql)
{
  return myx_convert_dbstr_utf8(mysql, mysql->db, -1);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief return the connections default schema (database)
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler

    @return the thread id of the given connection
*//////////////////////////////////////////////////////////////////////////////
unsigned long myx_get_thread_id(MYSQL *mysql)
{
  return mysql_thread_id(mysql);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief Checks availability of the connection.
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler

    @return @see mysql_ping
*//////////////////////////////////////////////////////////////////////////////
MYX_PUBLIC_FUNC int myx_ping_server(MYSQL *mysql)
{
  return mysql_ping(mysql);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get mysql_errno() for connection
    @ingroup bridge_to_MySQL_API
    @param mysql connection handler
    @return An error code value for the last mysql_xxx call, if it failed.
            zero means no error occurred.
*//////////////////////////////////////////////////////////////////////////////
int myx_mysql_errno(MYSQL *mysql)
{
  if(mysql_last_error == NULL)
  {
    return mysql_errno(mysql);
  }
  return mysql_last_errno;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get mysql_error() for connection

    warning: this function uses static variables and therefore is not thread-safe

    @ingroup bridge_to_MySQL_API
    @param mysql connection handler
    @return A null-terminated character string that describes the error.
            An empty string if no error occurred.
	    The returned string must be freed with g_free().
*//////////////////////////////////////////////////////////////////////////////
char *myx_mysql_error(MYSQL *mysql)
{
  char *e;
  if(mysql_last_error == NULL)
  {
    return g_strdup(mysql_error(mysql));
  }
  e= mysql_last_error;
  mysql_last_error= NULL;
  return e;
}

void myx_set_mysql_error(char *error, int errn)
{
  mysql_last_error= error;
  mysql_last_errno= errn;
}

MYX_MYSQL_ERROR_MSGS *myx_mysql_error_msgs_fetch(MYSQL *mysql)
{
  MYX_MYSQL_ERROR_MSGS *errors= g_new0(MYX_MYSQL_ERROR_MSGS, 1);
#if MYSQL_VERSION_ID >= 50000
  MYSQL_RES *res;
  MYSQL_ROW row;
  int c;

  c= mysql_warning_count(mysql);
  if (c > 0 && myx_mysql_query(mysql, "SHOW WARNINGS") == 0)
  {
    res= mysql_use_result(mysql);
    if (res != NULL)
    {
      errors->errors= g_realloc(errors->errors, sizeof(MYX_MYSQL_ERROR_MSG) * (errors->errors_num + c));

      do
      {
        MYX_MYSQL_ERROR_MSG *error;

        row= mysql_fetch_row(res);
        if (row == NULL)
          break;

        error= errors->errors + errors->errors_num++;

        if (strcasecmp(row[0], "Note")==0)
          error->level= MYX_QEL_NOTE;
        else
          error->level= MYX_QEL_WARNING;
        error->error= atoi(row[1]);
        error->text= g_strdup(row[2]);
      }
      while (1);

      mysql_free_result(res);
    };
  };  
#endif
  
  if (myx_mysql_errno(mysql) != 0)
  {
    errors->errors_num++;
    errors->errors= g_realloc(errors->errors,
                              sizeof(MYX_MYSQL_ERROR_MSG)*errors->errors_num);
    errors->errors[errors->errors_num-1].level= MYX_QEL_ERROR;
    errors->errors[errors->errors_num-1].error= myx_mysql_errno(mysql);
    errors->errors[errors->errors_num-1].text= myx_mysql_error(mysql);
  }
  
  if (errors->errors_num == 0)
  {
    g_free(errors->errors);
    g_free(errors);
    errors= NULL;
  }

  return errors;
}


void myx_mysql_error_msgs_free(MYX_MYSQL_ERROR_MSGS *errors)
{
  unsigned int i;
  for (i= 0; i < errors->errors_num; i++)
    g_free(errors->errors[i].text);
  g_free(errors->errors);
  g_free(errors);
}


///////////////////////////////////////////////////////////////////////////////
/** @brief get full version information of mysql server
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @return full version of connection in a string that has to be
            freed with g_free
*//////////////////////////////////////////////////////////////////////////////
char * myx_get_mysql_full_version(MYSQL *mysql)
{
  return g_strdup(mysql_get_server_info(mysql));
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get and cache version of mysql server
    @param mysql connection handler

    calls mysql_get_server_info(), initializes major_version and
    minor_version in the private data area
*//////////////////////////////////////////////////////////////////////////////
void myx_get_mysql_version(MYSQL *mysql)
{
  MYX_MYSQL *priv= myx_mysql_get_private(mysql);
  
  if (priv->major_version == 0)
  {
    const char *version;
    int major= 0, minor= 0, pl= 0;
    version= mysql_get_server_info(mysql);
    if ( version && sscanf(version, "%i.%i.%i%*s", &major, &minor, &pl) < 3)
    {
    }
    priv->major_version= major;
    priv->minor_version= minor;
    priv->patchlevel= pl;
  }
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get major version of mysql server
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @return major version of connection

    calls myx_get_mysql_version() and returns major_version
*//////////////////////////////////////////////////////////////////////////////
int myx_get_mysql_major_version(MYSQL *mysql)
{
  MYX_MYSQL *priv;
  myx_get_mysql_version(mysql);
  
  priv= myx_mysql_get_private(mysql);
  
  return priv->major_version;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get minor version of mysql server
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @return major version of connection

    calls myx_get_mysql_version() and returns minor_version
*//////////////////////////////////////////////////////////////////////////////
int myx_get_mysql_minor_version(MYSQL *mysql)
{
  MYX_MYSQL *priv;
  myx_get_mysql_version(mysql);
  
  priv= myx_mysql_get_private(mysql);

  return priv->minor_version;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get release number of mysql server
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @return release number of connection

    calls myx_get_mysql_version() and returns minor_version
*//////////////////////////////////////////////////////////////////////////////
MYX_PUBLIC_FUNC int myx_get_mysql_release(MYSQL *mysql)
{
  MYX_MYSQL *priv;
  myx_get_mysql_version(mysql);
  
  priv= myx_mysql_get_private(mysql);

  return priv->patchlevel;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get information about the server machine
    @ingroup Host_information_management

    @param user_conn connection parameters
    @param mysql connection handler
    @return prepared information
             - MYX_MACHINE_INFO::version is the string in form 
                                          "MySQL x.x via <name of protocol>"
             - MYX_MACHINE_INFO::network_name is the domain name of machine 
                                                cut by 64 symbols
             - MYX_MACHINE_INFO::IP is the ip string in form "xxx.xxx.xxx.xxx"
             - MYX_MACHINE_INFO::OS is NULL
             - MYX_MACHINE_INFO::hardware is NULL

    calls mysql_get_server_info(), myx_get_ip_as_string(), 
          myx_get_network_name_by_ip()
*//////////////////////////////////////////////////////////////////////////////
MYX_MACHINE_INFO * myx_get_server_info(MYX_USER_CONNECTION *user_conn,
                                       MYSQL *mysql)
{
  MYX_MACHINE_INFO *machine_info= g_malloc(sizeof(MYX_MACHINE_INFO));
  char *add_host_info;

  machine_info->network_name= g_malloc(NETWORK_NAME_BUFFER_LENGTH);
  machine_info->IP=           g_malloc(IP_BUFFER_LENGTH);

#if MYSQL_VERSION_ID >= 50000
  switch (mysql->options.protocol)
  {
  case MYSQL_PROTOCOL_MEMORY: add_host_info= "shared memory"; break;
  case MYSQL_PROTOCOL_SOCKET: add_host_info= "socket";        break;
  case MYSQL_PROTOCOL_PIPE:   add_host_info= "named pipe";    break;
  default:                    add_host_info= "TCP/IP";        break;
  }
#else
  add_host_info= "TCP/IP";
#endif

  machine_info->version=
    g_strdup_printf("MySQL %s via %s",
                    (unsigned char *)mysql_get_server_info(mysql),
                    add_host_info);

  myx_get_ip_as_string(user_conn->hostname, machine_info->IP);
  myx_get_network_name_by_ip(machine_info->IP, machine_info->network_name);

  machine_info->OS= NULL;
  machine_info->hardware= NULL;

  return machine_info;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get information about the client machine
    @ingroup Host_information_management

    @param mysql connection handler
    @return prepared information
             - MYX_MACHINE_INFO::version is the string in form
                                          "MySQL Client Version x.x.x"
             - MYX_MACHINE_INFO::network_name is the domain name of machine
                                                cut by 64 symbols
             - MYX_MACHINE_INFO::IP is the ip string in form "xxx.xxx.xxx.xxx"
             - MYX_MACHINE_INFO::OS is the result of get_local_os_name()
             - MYX_MACHINE_INFO::hardware is the result 
                                           of get_local_hardware_info()

 calls mysql_get_server_info(), myx_get_ip_as_string(),
          myx_get_network_name_by_ip()
*//////////////////////////////////////////////////////////////////////////////
MYX_MACHINE_INFO * myx_get_client_info(MYSQL *mysql)
{
  MYX_MACHINE_INFO *machine_info= g_malloc(sizeof(MYX_MACHINE_INFO));
  unsigned long client_version= mysql_get_client_version();

  machine_info->network_name= g_malloc(NETWORK_NAME_BUFFER_LENGTH);
  machine_info->IP=           g_malloc(IP_BUFFER_LENGTH);

  machine_info->version= 
    g_strdup_printf("MySQL Client Version %i.%i.%i",
                    (int)(client_version/10000),
                    (int)((client_version/100)%100),(int)(client_version%100));

  gethostname(machine_info->network_name, NETWORK_NAME_BUFFER_LENGTH);
  myx_get_ip_as_string(machine_info->network_name, machine_info->IP);

  machine_info->OS= get_local_os_name();
  machine_info->hardware= get_local_hardware_info();

  return machine_info;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief deallocates memory of MYX_MACHINE_INFO
    @ingroup Host_information_management
    @param machine_info struct to deallocate
    @return always Zero
*//////////////////////////////////////////////////////////////////////////////
int myx_free_pc_info(MYX_MACHINE_INFO *machine_info)
{
  g_free(machine_info->version);
  g_free(machine_info->network_name);
  g_free(machine_info->IP);
  if (machine_info->OS)
    g_free(machine_info->OS);
  if (machine_info->hardware)
    g_free(machine_info->hardware);
  g_free(machine_info);
  return 0;
}


/*
 *----------------------------------------------------------------------
 * myx_match_pattern
 *
 * SYNOPSIS
 *   Match a string to a search pattern.
 * 
 * DESCRIPTION
 *   Checks whether a text string matches a given search string,
 *   interpreting certain wild characters appropriately.
 *
 * RETURN VALUE
 * 
 * -1 if the search string is empty
 * 1  if the string matches
 * 0  if the string doesn't match
 *
 * NOTES
 * 
 * Supported wild characters:
 *   *  match anything, any number of chars
 *   ?  match a single character
 *
 *----------------------------------------------------------------------
 */
static gunichar dummy(gunichar c)
{
  return c;
}

int myx_match_pattern(const char *text, const char *search, int case_sensitive, int strict)
{
  if (search && *search)
  {
  #ifdef __GNUC__
    const char *stack[strlen(text) + 2];
  #else
    const char** stack= g_malloc(sizeof(char*) * ((unsigned int) strlen(text) + 2));
  #endif
    int stackp= 0;
    const char *search_begin= search;
    const char *tend= text+g_utf8_strlen(text, (gssize)strlen(text));
    const char *send= search+g_utf8_strlen(text, (gssize)strlen(search));

    gunichar (*normcase)(gunichar)= case_sensitive ? dummy : g_unichar_toupper;

    while (*search && *text)
    {
      switch (*search)
      {
       case '*':
        search= g_utf8_next_char(search);
        if (*search != '?')
        {
          gunichar norm_search= normcase(*search);
          stack[stackp++]= search;
          while (*text && normcase(g_utf8_get_char_validated(text, (gssize)(tend - text))) != norm_search)
            text= g_utf8_next_char(text);
          break;
        }

       case '?':
        search= g_utf8_next_char(search);
        text= g_utf8_next_char(text);
        break;
      
       default:
        if (normcase(g_utf8_get_char_validated(text, (gssize)(tend-search))) != normcase(g_utf8_get_char_validated(search, (gssize)(send-search))))
        {
          if (stackp==0)
          {
            return 0;
          }
          else
          {
            // backtrack
            search= stack[--stackp];
            while (*text && *search && normcase(g_utf8_get_char_validated(text, (gssize)(tend-text))) != normcase(g_utf8_get_char_validated(search, (gssize)(send-search))))
              text= g_utf8_next_char(text);
          }
        }
        else
        {
          search= g_utf8_next_char(search);
          text= g_utf8_next_char(text);
        }
        break;
      }
    }

  #ifndef __GNUC__
    g_free(stack);
  #endif

    if (strict)
    {
      if (!*search)
      {
        if (search > search_begin && g_utf8_prev_char(search) && *g_utf8_prev_char(search) == '*')
          return 1;
        return *text ? 0 : 1;
      }
      else
        return 0;
    }
    else
    {
      // If both search string and text string are at end then we found a match.
      if (!*search && !*text)
        return 1;
      else
      {
        // If the search string still contains characters then text was exhausted already.
        // In this case we match only if search contains one or more '*'.
        if (!*search)
          return 0;
        else
        {
          while (*search == '*')
            ++search;
          return !*search;
        };
      };
    };
  }
  else
    return -1;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get and cache charset of mysql server
    @param mysql connection handler

    initializes charset in the private data area if it is not set yet
*//////////////////////////////////////////////////////////////////////////////

const char * sql_charset_select = "SHOW VARIABLES LIKE 'character_set'";
const char * sql_charset_select_5 = "SHOW VARIABLES LIKE 'character_set_server'";

const char * myx_get_mysql_charset(MYSQL *mysql)
{
  MYX_MYSQL *priv= myx_mysql_get_private(mysql);

  if (!priv->charset)
  {
    int result;
    if (mysql_version_is_later_or_equal_than(mysql, 5, 0))
      result= mysql_query(mysql, sql_charset_select_5);
    else
      result= mysql_query(mysql, sql_charset_select);

    if (result == 0)
    {
      MYSQL_RES *res= mysql_store_result(mysql);
      MYSQL_ROW row= mysql_fetch_row(res);
      if (row && row[1])
        priv->charset= g_strdup(row[1]);
      mysql_free_result(res);
    };

    if (!priv->charset)
      priv->charset= g_strdup("latin1");
  }

  return priv->charset;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get charset name of server
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @return charset name 
        (mysql->charset->csname if MYSQL_VERSION_ID > 40000 else "latin1")

    returned value isn't supposed to be deallocated..
*//////////////////////////////////////////////////////////////////////////////
const char * myx_get_server_charset_name(MYSQL *mysql)
{
  return myx_get_mysql_charset(mysql);

/*#if MYSQL_VERSION_ID >= 40100
  if (mysql->charset)
    return mysql->charset->csname;
  else
#else
  if (mysql->charset)
    return mysql->charset->name;
  else
#endif
    return "latin1";*/
}

///////////////////////////////////////////////////////////////////////////////
/** @brief get value of sql-variable from server
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @param name name of variable
    @return value of variable if it exist else NULL

    returned value is supposed to be deallocated via g_free()
*//////////////////////////////////////////////////////////////////////////////
char *myx_get_server_variable(MYSQL *mysql, const char *name)
{
  char *sqlcmd;
  char *value= NULL;
  MYSQL_RES *res;
  MYSQL_ROW row;

  sqlcmd= g_strdup_printf("SHOW VARIABLES LIKE '%s'", name);

  if (!(myx_mysql_query(mysql, sqlcmd)))
  {
    res= mysql_store_result(mysql);
    if (res != NULL)
    {
      row= mysql_fetch_row(res);
      if (row != NULL)
        value= g_strdup(row[1]);
      mysql_free_result(res);
    }
  }

  g_free(sqlcmd);

  return value;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief try to convert str from charset "charsetname" to utf8
    @param str string to convert
    @param charsetname charset to convert from
    @return NULL if conversion was unsucceseful else converted string

    returned value is supposed to be deallocated via g_free()
    @note covered by unit tests
*//////////////////////////////////////////////////////////////////////////////
char * try_convert_from_cs_to_utf8(const char * str, const char * charsetname, const int length)
{
  GError *error= NULL;
  gchar *utf8;
  gsize bytes_read= 0;

  if (str != NULL && charsetname != NULL)
  {
    utf8= g_convert(str, length, "UTF-8", charsetname, &bytes_read, NULL, &error);
    
    // Check if it worked
    if (error == NULL)
      return utf8;

    g_free(utf8);
  };

  return NULL;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief convert the arguments str which comes from the database
             to UTF8 (if it is not already UTF8)
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @param str   string to convert
    @return converted string

    tries to convert 
      - from myx_get_server_charset_name()
      - from "ISO-8859-1"

    returned value is supposed to be deallocated via g_free()
    @note covered by unit tests
*//////////////////////////////////////////////////////////////////////////////
char *myx_convert_dbstr_utf8(MYSQL *mysql, const char* str, int length)
{
  gchar *utf8;
  const char * server_charset; 

  // If string is NULL return NULL.
  if (!str)
    return NULL;

  // If length is 0 then return an empty string (not a NULL value, as this is used for other tasks).
  if (length == 0)
    return g_strdup("");

  // Check if string already in UTF-8.
  if (g_utf8_validate(str, length, NULL) == TRUE)
    return g_strdup(str);

  server_charset= iconv_char_name(myx_get_server_charset_name(mysql));

  utf8= try_convert_from_cs_to_utf8(str, server_charset, length);
  if (utf8 != NULL)
    return utf8;
  utf8= try_convert_from_cs_to_utf8(str, "ISO-8859-1", length);
  if (utf8 != NULL)
    return utf8;  

  // If we cannot convert, return NULL.
  return NULL;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief Converts the argument str which has to be in UTF8
           to a string encoded in the format that the database
           expects.

    @param mysql      connection handler
    @param str        string to convert
    @param str_length length of str
    @param new_length pointer to unsigned int that 
                      is supposed to return the length of converted string to
    @return           a new allocated string in an encoding suitable for the db

    if conversion was unsuccesseful returns g_strndup(str)
          with length cut by 2M <BR>
    returned value is supposed to be deallocated via g_free()
*//////////////////////////////////////////////////////////////////////////////
char *myx_convert_utf8_dbstr_with_length(MYSQL *mysql, const char* str,
                                         int str_length,
                                         unsigned int *new_length)
{
  const int max_returned_length= 2000000;

  //If string is NULL return NULL
  if (!str)
  {
    return NULL;
  }
  else
  {
    GError * error= NULL;
    gchar * res;
    gsize bytes_read= 0;
    gsize nlength;

    //if connected to a server 4.1.x or higher, return UTF8
    if (mysql_version_is_later_or_equal_than(mysql,4,1))
      return g_memdup(str, str_length+1);

    //Get server charset
    res= g_convert(str, str_length,
                   iconv_char_name(myx_get_mysql_charset(mysql)),
                   "UTF-8", &bytes_read, &nlength, &error);
    
    *new_length= nlength;

    //check if it worked
    if (!error)
      return res;

    g_free(res);
    *new_length= str_length > max_returned_length
                  ? max_returned_length : str_length;
    return g_strndup(str, *new_length);
  }  
}


///////////////////////////////////////////////////////////////////////////////
/** @brief Converts the argument str which has to be in UTF8
           to a string encoded in the format that the database
           expects.
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @param str   string to convert

    @return      a new allocated string in an encoding suitable for the db

    if conversion was unsuccesseful returns g_strndup(str) 
          with length cut by 2M <BR>
    returned value is supposed to be deallocated via g_free()
    uses myx_convert_utf8_dbstr_with_length()
*//////////////////////////////////////////////////////////////////////////////
char *myx_convert_utf8_dbstr(MYSQL *mysql, const char* str)
{
  unsigned int new_length;
  return myx_convert_utf8_dbstr_with_length(mysql, str,
                                            (unsigned int)strlen(str),
                                            &new_length);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free string allocated using the library
    @ingroup Common_functions
    @param str string to free

    if (str) g_free(str);
*//////////////////////////////////////////////////////////////////////////////
void myx_free_lib_str(char *str)
{
  g_free(str);
}


static char *escape_query(int quote, const char *cmd, va_list args)
{
  char *sql= g_strdup(cmd);
  char quotestr[8];
  char *query;

  quotestr[0]= (char) quote;
  quotestr[1]= '%';
  quotestr[2]= 's';
  quotestr[3]= (char) quote;
  quotestr[4]= 0;

  //sprintf((char *)quotestr, "%c%%s%c", quote, quote);

  sql= str_g_replace(sql, "%s", quotestr);

  query= g_strdup_vprintf(sql, args);
  
  g_free(sql);
  
  return query;
}


///////////////////////////////////////////////////////////////////////////////
/** @brief escapes identifiers refered in cmd with %s and given in ...
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @param cmd   text of query to execute
    @param ...   database object names

    @return the escaped sql string, has to be freed with g_free
*//////////////////////////////////////////////////////////////////////////////
char * myx_mysql_esc(MYSQL *mysql, const char *cmd, ...)
{
  va_list args;
  char *query;
  int quote;
  
  va_start(args, cmd);
  quote= myx_get_mysql_quote_char(mysql, 0);

  query= escape_query(quote, cmd, args);
  va_end(args);

  return query;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief execute sql query using mysql connection
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @param cmd   text of query to execute
    @param ...   database object names

    @return result of mysql_query
      (Zero if the query was successful. Non-zero if an error occurred)

    The same as myx_mysql_query() but escapes defined in the cmd string with %s and
    given in ...
    cmd should be in UTF8, function tries to myx_convert_utf8_dbstr() it
*//////////////////////////////////////////////////////////////////////////////
int myx_mysql_query_esc(MYSQL *mysql, const char *cmd, ...)
{
  va_list args;
  char *query;
  int quote;
  int res;
  
  va_start(args, cmd);
  quote= myx_get_mysql_quote_char(mysql, 0);

  query= escape_query(quote, cmd, args);
  va_end(args);

  res= myx_mysql_query(mysql, query);

  g_free(query);

  return res;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief fetches resultset of a query as an integer value
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler

    @return the value of resultset

    use myx_query_free_resultset(rs) to release the resultset afterwards
*//////////////////////////////////////////////////////////////////////////////
int myx_get_resultset_as_int(MYSQL *mysql, MYX_LIB_ERROR * error_code)
{
  MYSQL_ROW row;
  MYSQL_RES *res= mysql_store_result(mysql);

  *error_code= MYX_NO_ERROR;

  if(res == NULL)
  {
    *error_code= MYX_SQL_ERROR;
    return 0;
  }

  row= mysql_fetch_row(res);
  if(row == NULL)
    return 0;
  return atoi(row[0]);
}


///////////////////////////////////////////////////////////////////////////////
/** @brief execute sql query using mysql connection
    @ingroup bridge_to_MySQL_API

    @param mysql connection handler
    @param cmd   text of query to execute

    @return result of mysql_query
      (Zero if the query was successful. Non-zero if an error occurred)

    cmd should be in UTF8, function tries to myx_convert_utf8_dbstr() it
*//////////////////////////////////////////////////////////////////////////////
int myx_mysql_query(MYSQL *mysql, const char *cmd)
{
  MYX_MYSQL *priv= myx_mysql_get_private(mysql);
  int rc;
  
#ifdef __GNUC__
  if (getenv("MYX_DEBUG"))
    g_message("Executing Query: %s", cmd);
#endif
    
  if (mysql_version_is_later_or_equal_than(mysql,4,1))
  {
    unsigned int cmdlen= (unsigned int) strlen(cmd);
    
    if (priv && priv->pre_query_hook)
      (*priv->pre_query_hook)(mysql, priv->client_data, cmd, cmdlen);

    rc= mysql_query(mysql, cmd);
    
    if (priv && priv->post_query_hook)
      (*priv->post_query_hook)(mysql, priv->client_data, cmd, cmdlen);
  }
  else
  {
    char *conv_cmd= myx_convert_utf8_dbstr(mysql, cmd);
    if (!conv_cmd)
    {
      rc= -1;
    }
    else
    {
      unsigned int cmdlen= (unsigned int) strlen(conv_cmd);
      
      if (priv && priv->pre_query_hook)
        (*priv->pre_query_hook)(mysql, priv->client_data, conv_cmd, cmdlen);

      rc= mysql_query(mysql, conv_cmd);

      if (priv && priv->post_query_hook)
        (*priv->post_query_hook)(mysql, priv->client_data, conv_cmd, cmdlen);

      g_free(conv_cmd);
    }
  }

  return rc;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief returns the number of affected rows
    @ingroup bridge_to_MySQL_API

    @param mysql  connection handler

    @return the number of affected rows

*//////////////////////////////////////////////////////////////////////////////
bigint myx_mysql_affected_rows(MYSQL *mysql)
{
  return (bigint)mysql_affected_rows(mysql);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief execute sql query with given length using mysql connection
    @ingroup bridge_to_MySQL_API

    @param mysql  connection handler
    @param cmd    text of query to execute
    @param len    length of cmd

    @return result of mysql_query
      (Zero if the query was successful. Non-zero if an error occurred)

    cmd should be in UTF8, function tryes to myx_convert_utf8_dbstr() it
*//////////////////////////////////////////////////////////////////////////////
int myx_mysql_real_query(MYSQL *mysql, const char *cmd, unsigned int len)
{
  int rc;
  MYX_MYSQL *priv= myx_mysql_get_private(mysql);
#ifdef __GNUC__
  if (getenv("MYX_DEBUG"))
    g_message("Executing Query: %s", cmd);
#endif
  //if connected to a server 4.1.x or higher, execute UTF8
  if (mysql_version_is_later_or_equal_than(mysql,4,1))
  {
    if (priv && priv->pre_query_hook)
      (*priv->pre_query_hook)(mysql, priv->client_data, cmd, len);

    //g_assert(g_utf8_validate(cmd, -1, 0));

    rc= mysql_real_query(mysql, cmd, len);
    
    if (priv && priv->post_query_hook)
      (*priv->post_query_hook)(mysql, priv->client_data, cmd, len);
  }
  else // if connected to a server 4.0.x or lower,
  {    //                                       convert cmd to server's charset
    unsigned int new_length;
    char *conv_cmd=
              myx_convert_utf8_dbstr_with_length(mysql, cmd, len, &new_length);

    if (!conv_cmd)
    {
      rc= -1;
    }
    else
    {
      if (priv && priv->post_query_hook)
        (*priv->post_query_hook)(mysql, priv->client_data, conv_cmd, new_length);

      rc= mysql_real_query(mysql, conv_cmd, new_length);
      
      if (priv && priv->post_query_hook)
        (*priv->post_query_hook)(mysql, priv->client_data, conv_cmd, new_length);

      g_free(conv_cmd);
    }
  }
  return rc;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief look for string in a MYX_STRINGLIST

    @param stringlist  list of strings to look through
    @param src         string to look for
    @return            index of found string if it exists, else -1
*//////////////////////////////////////////////////////////////////////////////
int myx_str_in_stringlist(MYX_STRINGLIST *stringlist, const char *src)
{
  char ** pos= stringlist->strings;
  char ** end= pos + stringlist->strings_num;

  for(; pos!=end; pos++)
  {
    if(strcmp(*pos, src) == 0)
      return (int)(pos - stringlist->strings);
  }
  return -1;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief Check whether an UTF8 string can be converted to another encoding.

    @param string      the utf8 string o check.
    @param encoding    the desired target encoding.
    @return            1 if conversion is possible, 0 if not
*//////////////////////////////////////////////////////////////////////////////
int myx_check_utf8_convertible(const char *str, const char *encoding)
{
  GIConv ic= g_iconv_open(encoding, "utf-8");
  char buffer[256];
  gsize in_left, out_left, count;
  char *in, *out;
  if (!ic)
  {
    g_message("can't iconv from utf-8 to %s", encoding);
    return -1;
  }

  in_left= (int)strlen(str);
  in= (char *)str;

  while (in_left > 0)
  {
    out= (char*)buffer;
    out_left= sizeof(out);
    count= (int)g_iconv(ic, &in, &in_left, &out, &out_left);
    if ((int)count != 0)
      return 0;
  }

  g_iconv_close(ic);

  return 1;
}

int myx_free_lib_stringlist(MYX_STRINGLIST *stringlist)
{
  return myx_free_stringlist(stringlist);
}

/** @} */ // end of bridge_to_MySQL_API
