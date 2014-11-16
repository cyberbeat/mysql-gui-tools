/* Copyright (C) 2005 MySQL AB

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


#include <string>
#include <time.h>
#include <glib.h>
#include "myx_util_public_interface.h"
#include <myx_admin_public_interface.h>
   
#define _(s) s

static void help()
{
  printf("Usage: mabackup [OPTIONS] PROFILE\n");
  printf("Executes MySQL Administrator backup profiles.\n");
  printf("\n");
  printf(_("  --help                    Display this help and exit.\n"));
  printf(_("  -u,--user=name            User for login if not current user.\n"));
  printf(_("  -p,--password=s           Password to use when connecting to server.\n"));
  printf(_("  -h,--host=name            Connect to host.\n"));
  printf(_("  -P,--port=#               Port number to use for connection.\n"));
  printf(_("  -S,--socket=name          Socket file to use for connection.\n"));
  printf(_("  -c,--connection=name      Specify the name of a connection to use.\n"));
  printf(_("  -o,--output=path          Specify output path for backup.\n"));
  printf(_("  -d,--directory=directory  Specify directory to place timestamped backup files.\n"));
  printf(_("  -x,--prefix=prefix        Specify filename prefix for timestamped backup files.\n"));
}

// cmd line options
static const char *arg_user= NULL;
static const char *arg_passwd= NULL;
static const char *arg_host= NULL;
static int arg_port= 0;
static const char *arg_socket= NULL;
static const char *arg_connection= NULL;
static const char *arg_output= NULL;
static const char *arg_directory= NULL;
static const char *arg_profile= NULL;
static const char *arg_prefix= "";

static inline void chk(bool cond, const char *errtext)
{
  if(!cond)
  {
    g_print(errtext);
    exit(-1);
  }
}

static char *get_arg(char **argv, int &index, int option_length=1)
{
  char *arg= argv[index];
  char *ptr= strchr(arg,'=');
  if (ptr)
  {
    if (*(ptr+1)) /* --foo=bla */
      return ptr+1;
    else
      return NULL; /* --foo= */
  }
  else
  {
    if (strncmp(arg,"--",2)==0) /* --foo bla */
      return argv[++index];
    else
    {
      if (strlen(arg)==(unsigned)option_length+1) /* -f bla */
        return argv[++index];
      else
        return argv[index]+1+option_length; /* -fbla */
    }
  }
}

static void validate_cmd_line_args()
{
  if(NULL == arg_connection) {
    chk(NULL != arg_user, _("missing parameter: --user\n"));
    chk(NULL != arg_passwd, _("missing parameter: --password\n"));
    chk(NULL != arg_host, _("missing parameter: --host\n"));
  } else {
    chk((NULL == arg_user) && (NULL == arg_passwd) && (NULL == arg_host), 
      _("invalid parameters: you should specify either --connection or --user, --password, --host but not both\n")
    );
  }
  chk((NULL != arg_output) || (NULL != arg_directory), 
    _("missing parameter: --output\n"));
  chk((NULL == arg_output) || (NULL == arg_directory),
    _("invalid parameters: you should specify either --output or --directory  but not both\n"));
  chk(NULL != arg_profile, _("missing parameter: PROFILE\n"));
}

static bool parse_cmd_line_args(int argc, char **argv)
{
  int i;

#define CHECK_OPTION(s,v) ((strncmp(s,v, strlen(v)) == 0) || strcmp(s,v)==0)

  for (i= 1; i < argc; i++)
  {
    if (strncmp(argv[i], "-u", 2)==0 || CHECK_OPTION(argv[i],"--user"))
    {
      arg_user= get_arg(argv, i);
      if (!arg_user)
        goto missing_arg;
    }
    else if (strncmp(argv[i], "-p", 2)==0 || CHECK_OPTION(argv[i],"--password"))
    {
      arg_passwd= get_arg(argv, i);
      if (!arg_passwd)
        arg_passwd= "";
    }
    else if (strncmp(argv[i], "-h", 2)==0 || CHECK_OPTION(argv[i],"--host"))
    {
      arg_host= get_arg(argv, i);
      if (!arg_host)
        goto missing_arg;
    }
    else if (strncmp(argv[i], "-P", 2)==0 || CHECK_OPTION(argv[i],"--port"))
    {
      char *value= get_arg(argv, i);
      if (!value)
        goto missing_arg;
      if (sscanf(value, "%i", &arg_port) != 1 || arg_port < 1 || arg_port > (1<<16))
      {
        g_print(_("Bad port number value '%s' for option\n"), value);
        return false;
      }
    }
    else if (strncmp(argv[i], "-S", 2)==0 || CHECK_OPTION(argv[i],"--socket"))
    {
      arg_socket= get_arg(argv, i);
      if (!arg_socket)
        goto missing_arg;
    }
    else if (strncmp(argv[i], "-c", 2)==0 || CHECK_OPTION(argv[i],"--connection"))
    {
      arg_connection= get_arg(argv, i);
      if (!arg_connection)
        goto missing_arg;
    }
    else if (strncmp(argv[i], "-o", 2)==0 || CHECK_OPTION(argv[i],"--output"))
    {
      arg_output= get_arg(argv, i);
      if (!arg_output)
        goto missing_arg;
    }
    else if (strncmp(argv[i], "-d", 2)==0 || CHECK_OPTION(argv[i],"--directory"))
    {
      arg_directory= get_arg(argv, i);
      if (!arg_directory)
        goto missing_arg;
    }
    else if (strncmp(argv[i], "-x", 2)==0 || CHECK_OPTION(argv[i],"--prefix"))
    {
      arg_prefix= get_arg(argv, i);
      if (!arg_directory)
        goto missing_arg;
    }
    else if (strcmp(argv[i], "--help")==0)
    {
      help();
      exit(0);
    }
    else
    {
      arg_profile = argv[i];
      validate_cmd_line_args();
      return true;
    }
  }

  validate_cmd_line_args();
  return true;
  
missing_arg:
  g_print(_("%s: missing value for option %s\n"), g_get_prgname(), argv[i]);
  return false;
}

static void report_backup_error(MYX_BACKUP_ERROR berr)
{
#define BE_MSG "Backup error: "
  switch(berr) 
  {
  case MYX_BACKUP_SERVER_ERROR:
    g_print(_(BE_MSG"MySQL Error\n"));
    break;
  case MYX_BACKUP_CANT_OPEN_FILE:
    g_print(_(BE_MSG"Cannot open file\n"));
    break;
  case MYX_BACKUP_ILLEGAL_OPTION:
    g_print(_(BE_MSG"Illegal option\n"));
    break;
  case MYX_BACKUP_PCRE_ERROR:
    g_print(_(BE_MSG"PCRE error\n"));
    break;
  case MYX_BACKUP_MALLOC_FAILED:
    g_print(_(BE_MSG"Memory allocation error\n"));
    break;
  case MYX_BACKUP_OUTPUTDEVICE_FULL:
    g_print(_(BE_MSG"Device full\n"));
    break;
  case MYX_BACKUP_CANNOT_FLUSH_TABLES_WITH_READ_LOCK:
    g_print(_(BE_MSG"Cannot flush tables with read lock\n"));
    break;
  case MYX_BACKUP_CANNOT_START_TRANSACTION:
    g_print(_(BE_MSG"Cannot start transaction\n"));
    break;
  case MYX_BACKUP_CANNOT_SET_ANSI_QUOTES:
    g_print(_(BE_MSG"Cannot set ANSI quotes\n"));
    break;
  case MYX_BACKUP_CANT_READ_FROM_FILE:
    g_print(_(BE_MSG"Cannot read from file.\n"));
    break;
  case MYX_BACKUP_XML_PARSE_ERROR:
    g_print(_(BE_MSG"Error parsing XML file.\n"));
    break;
  case MYX_BACKUP_SQL_ERROR:
    g_print(_(BE_MSG"SQL error\n"));
    break;
  case MYX_BACKUP_STOPPED:
    g_print(_(BE_MSG"Stopped by user.\n"));
    break;
  case MYX_BACKUP_CHARSET_CONVERSION:
    g_print(_(BE_MSG"Character set conversion error.\n"));
    break;
  case MYX_BACKUP_WRONG_CHARSET:
    g_print(_(BE_MSG"Wrong character set.\n"));
    break;
  case MYX_BACKUP_UNKNOWN:
    g_print(_(BE_MSG"Unknown error.\n"));
    break;
  case MYX_BACKUP_FILE_IS_NOT_MA_DUMP:
  case MYX_BACKUP_NO_ERROR:
    break;
  }
}

static MYX_USER_CONNECTION *find_user_connection(MYX_USER_CONNECTIONS *pconns, 
                                                 const char *name)
{
  for(unsigned int i = 0; i < pconns->user_connections_num; i++)
  {
    if(NULL == pconns->user_connections[i].connection_name)
    {
      continue;
    }
    if(strcmp(pconns->user_connections[i].connection_name, name) == 0)
    {
      return &pconns->user_connections[i];
    }
  }
  return NULL;
}

static const char *get_backup_file_name(const char *profile, const char *prefix)
{
  // FormatDateTime(' yyyymmdd hhnn', Now) + '.sql'
  time_t tt = time(NULL);
  tm *t = localtime(&tt);

  char *fname = new char[strlen(profile) + 64];
  sprintf(fname, "%s%s_%04d%02d%02d_%02d%02d.sql", 
    prefix, profile, t->tm_year+1900, t->tm_mon+1, t->tm_mday, t->tm_hour, t->tm_min);
  return fname;
}

int main(int argc, char **argv)
{
  std::string dir;
  std::string connfile;
  std::string profpath;
  MYX_ADMIN_LIB_ERROR aerr;

  if(!parse_cmd_line_args(argc, argv))
  {
    return -1;
  }

  MYX_USER_CONNECTION *pconn= NULL;

  if(NULL != arg_connection)
  {
    connfile = getenv("HOME");
#ifdef __APPLE__
    connfile += "/Library/Application Support/MySQL/mysqlx_user_connections.xml";
#else
    connfile += "/.mysqlgui/mysqlx_user_connections.xml";
#endif

    MYX_LIB_ERROR conerr;
    MYX_USER_CONNECTIONS *pconns = myx_load_user_connections(connfile.c_str(), &conerr);
    chk(NULL != pconns, _("Cannot load connections data\n"));
    pconn = find_user_connection(pconns, arg_connection);
    chk(NULL != pconn, _("Cannot load specified connection\n"));
    arg_user = (NULL == pconn->username ? "" : pconn->username);
    arg_passwd = (NULL == pconn->password ? "" : pconn->password);
    arg_host = (NULL == pconn->hostname ? "" : pconn->hostname);
    arg_port = (0 == pconn->port ? 0 : pconn->port);
  }

  if(NULL != arg_directory)
  {
    dir = arg_directory;
    dir += "/";
    dir += get_backup_file_name(arg_profile, arg_prefix);
    arg_output = dir.c_str();
  }

  profpath = getenv("HOME");
#ifdef __APPLE__
  profpath += "/Library/Application Support/MySQL/administrator/profiles/";
#else
  profpath += "/.mysqlgui/backup_profiles/";
#endif
  profpath += arg_profile;
  profpath += ".mbp";

  MYX_BACKUP_PROFILE *profile =
    myx_load_profile(NULL, profpath.c_str(), &aerr);

  if (!profile)
  {
    char *tmp;
    g_print(_("Cannot load profile '%s': %s\n"), profpath.c_str(), tmp=myx_get_backup_error_string((MYX_BACKUP_ERROR)aerr));
    g_free(tmp);
    exit(1);
  }

  MYSQL *mysql = myx_mysql_init();
  //MYSQL *conn = mysql_real_connect(mysql, arg_host, arg_user, arg_passwd,
  //  NULL, arg_port, arg_socket, 0);
  myx_connect_to_instance(pconn, mysql);
  if (!pconn)
  {
    g_print(_("Cannot connect to server: %s\n"), mysql_error(mysql));
    exit(1);
  }

  MYX_BACKUP_ERROR berr;

  berr = 
  myx_make_backup_with_profile(mysql, profile, arg_output, 0, NULL, NULL);

  if(MYX_BACKUP_NO_ERROR != berr) 
  {
    report_backup_error(berr);
  }

  mysql_close(mysql);
  myx_free_profile(profile);

  return MYX_BACKUP_NO_ERROR != berr ? 0 : 1;
}
