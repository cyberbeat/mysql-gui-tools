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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <myx_admin_library.h>
#include <glib.h>

/*
 * global variables
 */

enum myx_lib_error error_code;

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)

static int get_port_number_from_ini(const char *ininame)
{
  // Return a default value of 3306 if no port could be found in the config file.
  // This is a valid value as the server which uses that file also assumes 3306 as default.
  return (int)GetPrivateProfileInt("mysqld", "port", 3306, ininame);
}

//----------------------------------------------------------------------------------------------------------------------

char *myx_get_running_service_name(int port)
{
  SC_HANDLE srv, scm;
  DWORD bufsz= 0;
  DWORD count= 0;
  DWORD resume= 0;
  DWORD i;
  ENUM_SERVICE_STATUS_PROCESS *next;
  char *pbuf, *pname, *ppath, *ppathend, *ininame;
  char delimiter;
  LPQUERY_SERVICE_CONFIG pconf;

  scm= OpenSCManager(NULL, NULL, GENERIC_READ);
  EnumServicesStatusEx(scm, SC_ENUM_PROCESS_INFO, SERVICE_WIN32, SERVICE_ACTIVE, NULL, 0,
    &bufsz, &count, &resume, NULL);
  pbuf= g_malloc(bufsz);
  EnumServicesStatusEx(scm, SC_ENUM_PROCESS_INFO, SERVICE_WIN32, SERVICE_ACTIVE, pbuf, bufsz,
    &bufsz, &count, &resume, NULL);

  for (i= 0; i < count; i++)
  {
    next= ((ENUM_SERVICE_STATUS_PROCESS *)pbuf) + i;
    if (next == NULL) // be defensive
      continue;
  
    pname= strlwr(g_strdup(next->lpServiceName));

    // Find a running MySQL service.
    if ((strstr(pname, "mysql") != NULL) && (next->ServiceStatusProcess.dwCurrentState & SERVICE_RUNNING))
    {
      // Ok, looks good so far. However it might be a service that only has "mysql" in its name so check further.
      srv= OpenService(scm, next->lpServiceName, GENERIC_READ);
      QueryServiceConfig(srv, NULL, 0, &bufsz);
      pconf= g_malloc(bufsz);
      QueryServiceConfig(srv, pconf, bufsz, &bufsz);

      // If the entry has a --defaults-file parameter we can safely assume it is a mysql server.
      ppath= strstr(pconf->lpBinaryPathName, "--defaults-file=");
      if (ppath != NULL)
      {
        ppath += sizeof("--defaults-file=") - 1;
        delimiter= *ppath;

        // Check if the path is quoted otherwise scan for the first space in the remaining string.
        if (delimiter == '\'' || delimiter == '"' )
          ++ppath;
        else
          delimiter= ' ';

        ppathend= ppath;
        do
        {
          ++ppathend;
        } while (*ppathend != '\0' && *ppathend != delimiter);

        // No extract the config path and check the port.
        if (ppathend - ppath > 0)
        {
          ininame= memcpy(g_malloc((gulong)(ppathend - ppath + 1)), ppath, ppathend - ppath);
          ininame[ppathend - ppath]= '\0';
          if (get_port_number_from_ini(ininame) == port)
          {
            g_free(ininame);
            g_free(pconf);
            CloseServiceHandle(srv);
            CloseServiceHandle(scm);
            return pname;
          };
          g_free(ininame);
        };
      };
      g_free(pconf);
      CloseServiceHandle(srv);
    };
    g_free(pname);
  };

  CloseServiceHandle(scm);
  return g_strdup("");
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the config file name for a mysql service running on the given port.
 *
 * @param port The port on which the service is running.
 *
 * @return The name of the service's config file if found or an empty string. Caller is responsible to free that
 *         string using g_free.
 */
char* myx_get_running_service_config_file(int port)
{
  SC_HANDLE srv, scm;
  DWORD bufsz= 0;
  DWORD count= 0;
  DWORD resume= 0, i;
  ENUM_SERVICE_STATUS_PROCESS *next;
  char *pbuf, *pname, *ppath, *ppathend, *ininame;
  char delimiter;
  LPQUERY_SERVICE_CONFIG pconf;
  char* result= NULL;
  
  scm= OpenSCManager(NULL, NULL, GENERIC_READ);
  EnumServicesStatusEx(scm, SC_ENUM_PROCESS_INFO, SERVICE_WIN32, SERVICE_ACTIVE, NULL, 0, &bufsz, &count, &resume, NULL);
  pbuf= g_malloc(bufsz);
  EnumServicesStatusEx(scm, SC_ENUM_PROCESS_INFO, SERVICE_WIN32, SERVICE_ACTIVE, pbuf, bufsz, &bufsz, &count, &resume,
    NULL);

  for (i= 0; i < count; i++)
  {
    next= ((ENUM_SERVICE_STATUS_PROCESS *)pbuf) + i;
    if (next == NULL) // be defensive
      continue;
  
    pname= strlwr(g_strdup(next->lpServiceName));

    // Find a running MySQL service.
    if ((strstr(pname, "mysql") != NULL) && (next->ServiceStatusProcess.dwCurrentState & SERVICE_RUNNING))
    {
      // Ok, looks good so far. However it might be a service that only has "mysql" in its name so check further.
      srv= OpenService(scm, next->lpServiceName, GENERIC_READ);
      QueryServiceConfig(srv, NULL, 0, &bufsz);
      pconf= g_malloc(bufsz);
      QueryServiceConfig(srv, pconf, bufsz, &bufsz);

      // If the entry has a --defaults-file parameter we can safely assume it is a mysql server.
      ppath= strstr(pconf->lpBinaryPathName, "--defaults-file=");
      if (ppath != NULL)
      {
        ppath += sizeof("--defaults-file=") - 1;
        delimiter= *ppath;

        // Check if the path is quoted otherwise scan for the first space in the remaining string.
        if (delimiter == '\'' || delimiter == '"' )
          ++ppath;
        else
          delimiter= ' ';

        ppathend= ppath;
        do
        {
          ++ppathend;
        } while (*ppathend != '\0' && *ppathend != delimiter);

        // No extract the config path and check the port.
        if (ppathend - ppath > 0)
        {
          ininame= memcpy(g_malloc((gulong)(ppathend - ppath + 1)), ppath, ppathend - ppath);
          ininame[ppathend - ppath]= '\0';
          if (get_port_number_from_ini(ininame) == port)
          {
            result= ininame;
            g_free(pconf);
            g_free(pname);
            CloseServiceHandle(srv);
            break;
          }
          g_free(ininame);
        };
      };
      g_free(pconf);
      CloseServiceHandle(srv);
    };
    g_free(pname);
  };

  CloseServiceHandle(scm);

  if (result != NULL)
    return result;
  else
    return g_strdup("");
}
#endif

//----------------------------------------------------------------------------------------------------------------------

int myx_get_admin_public_interface_version()
{
  return libmysqladmin_PUBLIC_INTERFACE_VERSION;
}

MYX_VARIABLES * myx_get_variables(MYSQL *mysql, char *sqlcmd)
{
  MYX_VARIABLES *vars= g_malloc(sizeof(MYX_VARIABLES));
  MYX_NAME_VALUE_PAIR *svp;
  MYSQL_RES *res;
  MYSQL_ROW row;
  unsigned int i;

  if (!(myx_mysql_query(mysql, sqlcmd)))
  {
    res= mysql_store_result(mysql);
    if (res != NULL)
    {
      vars->variables_num= (int)mysql_num_rows(res);
      vars->variables= g_malloc(sizeof(MYX_NAME_VALUE_PAIR)*vars->variables_num);

      i=0;
      do
      {
        row= mysql_fetch_row(res);
        if (row == NULL)
          break;

        svp= vars->variables+i;

        svp->name= g_strdup(row[0]);
        svp->value= g_strdup(row[1]);

        i++;
      }
      while (1);
    }
    else
    {
      g_free(vars);
      vars= NULL;
    }

    mysql_free_result(res);
  }
  else
  {
    g_free(vars);
    vars= NULL;
  }

  return vars;
}

//----------------------------------------------------------------------------------------------------------------------

int myx_free_variables(MYX_VARIABLES *vars)
{
  unsigned int i;
  MYX_NAME_VALUE_PAIR *svp;

  for(i=0;i<vars->variables_num;i++)
  {
    svp= vars->variables+i;
    g_free(svp->name);
    g_free(svp->value);
  }
  g_free(vars->variables);
  g_free(vars);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_VARIABLES* add_innodb_status(MYSQL *mysql, MYX_VARIABLES* vars)
{
  MYX_NAME_VALUE_PAIR *svp;
  MYSQL_RES *res;
  MYSQL_ROW row;
  char *innodb_monitor= NULL;
  int innodb_monitor_length= 0;
  unsigned long *lengths;

  if (!(myx_mysql_query(mysql, "SHOW INNODB STATUS")))
  {
    res= mysql_store_result(mysql);
    if (res != NULL)
    {
      //Get innodb monitor output
      row= mysql_fetch_row(res);
      if (row != NULL)
      {
        lengths= mysql_fetch_lengths(res);

        innodb_monitor= g_strdup(row[0]);
        innodb_monitor_length= lengths[0];
        /*innodb_monitor= g_malloc(sizeof(char)*(lengths[0]+1));
        memcpy(innodb_monitor, row[0], lengths[0]);
        innodb_monitor_length= lengths[0];
        innodb_monitor[lengths[0]]= 0;*/
      }

      //Add variables      
      vars->variables_num+=5;
      vars->variables= g_realloc(vars->variables, sizeof(MYX_NAME_VALUE_PAIR)*vars->variables_num);

      svp= vars->variables+vars->variables_num-5;

      //Find variables

      svp->name= g_strdup("innodb_buffer_pool_size");
      svp->value= get_value_from_text(innodb_monitor, innodb_monitor_length, "Buffer pool size\\s+(\\d+)");
     
      svp++;
      svp->name= g_strdup("innodb_buffer_free_buffers");
      svp->value= get_value_from_text(innodb_monitor, innodb_monitor_length, "Free buffers\\s+(\\d+)");

      svp++;
      svp->name= g_strdup("innodb_buffer_pages_read");
      svp->value= get_value_from_text(innodb_monitor, innodb_monitor_length, "Pages read\\s+(\\d+)");

      svp++;
      svp->name= g_strdup("innodb_buffer_pages_created");
      svp->value= get_value_from_text(innodb_monitor, innodb_monitor_length, ", created\\s+(\\d+)");

      svp++;
      svp->name= g_strdup("innodb_buffer_pages_written");
      svp->value= get_value_from_text(innodb_monitor, innodb_monitor_length, ", created\\s+(\\d+)");
      g_free(innodb_monitor);
    }

    mysql_free_result(res);
  }

  return vars;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_VARIABLES* add_missing_server_variables(MYSQL *mysql, MYX_VARIABLES* vars)
{
  MYX_NAME_VALUE_PAIR *svp;
  unsigned int i;
  int found= 0;

  for(i=0;i<vars->variables_num-1;i++)
  {
    if(strcmp(vars->variables[i].name, "key_cache_block_size")==0)
    {
      found= 1;
      break;
    }
  }
  if(!found)
  {
    //Add variable     
    vars->variables_num+=1;
    vars->variables= g_realloc(vars->variables, sizeof(MYX_NAME_VALUE_PAIR)*vars->variables_num);

    svp= vars->variables+vars->variables_num-1;

    //Find variables

    svp->name= g_strdup("key_cache_block_size");
    svp->value= g_strdup("1024");
  }

  return vars;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_VARIABLES* myx_get_server_variables(MYSQL *mysql)
{
  MYX_VARIABLES *vars;

  if (mysql_full_version_is_later_or_equal_than(mysql, 5, 0, 3))
    vars= myx_get_variables(mysql, "SHOW GLOBAL VARIABLES");
  else
    vars= myx_get_variables(mysql, "SHOW VARIABLES");


  if (vars)
    vars= add_missing_server_variables(mysql, vars);

  return vars;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_VARIABLES* myx_get_status_variables(MYSQL *mysql)
{
  MYX_VARIABLES *vars;

  if (mysql_full_version_is_later_or_equal_than(mysql, 5, 0, 2))
    vars= myx_get_variables(mysql, "SHOW GLOBAL STATUS");
  else
    vars= myx_get_variables(mysql, "SHOW STATUS");

  if (vars)
    vars= add_innodb_status(mysql, vars);

  return vars;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the variable given by mysql_id to the given value. The function first tries to set the value as a string,
 * since we don't know which datatype is required. If that fails with a "wrong datatype" error then a second attempt is
 * made. This time pure value, without any modification is passed on.
 *
 * @param mysql_id The name of the variable to change.
 * @param value The value to be set.
 *
 * @return 0 if all is ok, otherwise 1.
 *
 * @note covered by unit tests.
 */
int myx_set_variable(MYSQL *mysql, const char *mysql_id, const char *value)
{
  char *query= g_strdup_printf("SET GLOBAL %s='%s'", mysql_id, value);
  int res= 0;

  // Try setting the value as string first.
  if (myx_mysql_query(mysql, query) != 0)
  {
    // If there is an error then check if it complains about the datatype.
    int error= mysql_errno(mysql);
    if (error == 1232) // Incorrect argument type to variable ...
    {
      // If we used the wrong datatype (a string) then try without quotes.
      // If that isn't accepted either then let the caller handle the problem.
      g_free(query);
      query= g_strdup_printf("SET GLOBAL %s=%s", mysql_id, value);
      if (myx_mysql_query(mysql, query) != 0)
        res= 1;
    };
  };

  g_free(query);

  return res;
}

//----------------------------------------------------------------------------------------------------------------------

static MYX_TABLE_COMMAND_STATUSES *do_table_command(MYSQL *mysql, const char *command)
{
  MYX_TABLE_COMMAND_STATUSES *status= NULL;
  MYSQL_RES *res= NULL;
  MYSQL_ROW row;

  if (myx_mysql_query(mysql, command) == 0)
    res= mysql_store_result(mysql);
  if (res != NULL)
  {
    int i= 0;

    status= g_malloc0(sizeof(MYX_TABLE_COMMAND_STATUSES));
    status->status_num= (int)mysql_num_rows(res);
    status->status= g_malloc0(sizeof(MYX_TABLE_COMMAND_STATUS)*status->status_num);

    do
    {
      row= mysql_fetch_row(res);
      if (row == NULL)
        break;

      status->status[i].table= myx_convert_dbstr_utf8(mysql, row[0], -1);
      status->status[i].message= myx_convert_dbstr_utf8(mysql, row[3], -1);

      if (strcmp2(row[2],"status")==0)
        status->status[i].message_type= MYX_MESSAGE_STATUS;
      else if (strcmp2(row[2], "error")==0)
        status->status[i].message_type= MYX_MESSAGE_ERROR;
      else if (strcmp2(row[2], "info")==0)
        status->status[i].message_type= MYX_MESSAGE_INFO;
      else if (strcmp2(row[2], "warning")==0)
        status->status[i].message_type= MYX_MESSAGE_WARNING;
      i++;
    }
    while (1);

    mysql_free_result(res);
  }

  return status;
}

//----------------------------------------------------------------------------------------------------------------------

int myx_free_command_status(MYX_TABLE_COMMAND_STATUSES *status)
{
  unsigned int i;
  for (i= 0; i < status->status_num; i++)
  {
    g_free(status->status[i].table);
    g_free(status->status[i].message);
  }
  g_free(status->status);
  g_free(status);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_TABLE_COMMAND_STATUSES *myx_optimize_table(MYSQL *mysql, const char *objects, int type_mask)
{
  MYX_TABLE_COMMAND_STATUSES *status;
  char *query;
  char opt[128];

  opt[0]= 0;
  if (type_mask & MYX_REPAIR_NO_WRITE_TO_BINLOG)
    strcat(opt, " NO_WRITE_TO_BINLOG");

  query= g_strdup_printf("OPTIMIZE %s TABLE %s", opt, objects);
  status= do_table_command(mysql, query);
  g_free(query);

  return status;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_TABLE_COMMAND_STATUSES *myx_check_table(MYSQL *mysql, const char *objects,
                                            int type_mask)
{
  MYX_TABLE_COMMAND_STATUSES *status;
  char types[128];
  char *query;

  types[0]= 0;
  
  if (type_mask & MYX_CHECK_QUICK)
    strcat(types, " QUICK");
  if (type_mask & MYX_CHECK_FAST)
    strcat(types, " FAST");
  if (type_mask & MYX_CHECK_MEDIUM)
    strcat(types, " MEDIUM");
  if (type_mask & MYX_CHECK_EXTENDED)
    strcat(types, " EXTENDED");
  if (type_mask & MYX_CHECK_CHANGED)
    strcat(types, " CHANGED");

  query= g_strdup_printf("CHECK TABLE %s %s", objects, types);
  status= do_table_command(mysql, query);
  g_free(query);

  return status;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_TABLE_COMMAND_STATUSES *myx_repair_table(MYSQL *mysql, const char *objects, int type_mask)
{
  MYX_TABLE_COMMAND_STATUSES *status;
  char types[128], opt[128];
  char *query;

  types[0]= 0;
  opt[0]= 0;

  if (type_mask & MYX_CHECK_QUICK)
    strcat(types, " QUICK");
  if (type_mask & MYX_CHECK_EXTENDED)
    strcat(types, " EXTENDED");
  if (type_mask & MYX_REPAIR_USE_FRM)
    strcat(types, " USE_FRM");
  if (type_mask & MYX_REPAIR_NO_WRITE_TO_BINLOG)
    strcat(opt, " NO_WRITE_TO_BINLOG");

  query= g_strdup_printf("REPAIR %s TABLE %s %s", opt, objects, types);
  status= do_table_command(mysql, query);
  g_free(query);

  return status;
}

//----------------------------------------------------------------------------------------------------------------------

/* expression evaluation */

static int get_precedence(int op)
{
  switch (op)
  {
  case '+':
  case '-':
      return 1;
  case '*':
  case '/':
      return 0;
  }
  //SYNTAX ERROR!
  return -1;
}

//----------------------------------------------------------------------------------------------------------------------

static void add_node(MYX_COMPILED_EXPRESSION *expr)
{
  if (expr->nodes_num == 0)
  {
    expr->nodes_num++;
    expr->nodes= (MYX_EXPRESSION_NODE*)malloc(sizeof(MYX_EXPRESSION_NODE));
  }
  else
  {
    expr->nodes_num++;
    expr->nodes= (MYX_EXPRESSION_NODE*)realloc(expr->nodes,
                                                sizeof(MYX_EXPRESSION_NODE)*expr->nodes_num);
  }
}

//----------------------------------------------------------------------------------------------------------------------

static void add_operator(MYX_COMPILED_EXPRESSION *expr, int op)
{
  add_node(expr);
  expr->nodes[expr->nodes_num-1].ntype= op;
}

//----------------------------------------------------------------------------------------------------------------------

static void add_ioperand(MYX_COMPILED_EXPRESSION *expr, double value)
{
  add_node(expr);
  expr->nodes[expr->nodes_num-1].ntype= 'I';
  expr->nodes[expr->nodes_num-1].immediate= value;
}

//----------------------------------------------------------------------------------------------------------------------

static void add_operand(MYX_COMPILED_EXPRESSION *expr, int type, int index)
{
  add_node(expr);
  expr->nodes[expr->nodes_num-1].ntype= type;
  expr->nodes[expr->nodes_num-1].variable_index= index;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * myx_compile_expression
 *
 * SYNOPSIS
 *    Compiles an expression to an internal format, suitable for
 * evaluation by myx_eval_expression
 *
 * DESCRIPTION
 *    Can take expressions with variable indices, such as:
 *    [1]+([2]/[0])*^[3]
 *
 * RETURN VALUE
 *    The expression structure or NULL on error. Value must be freed
 *    with myx_free_expression
 * 
 * NOTES
 *   
 */
MYX_COMPILED_EXPRESSION *myx_compile_expression(const char *expr,
                                                MYX_EXPRESSION_ERROR *error)
{
  const char *pp= expr;
  char *tmpbuf;
  int stp= 0;
  char *stack; // symbol stack
  MYX_COMPILED_EXPRESSION *cexpr;
  
  tmpbuf= (char*)malloc(strlen(expr)+1);

  cexpr= (MYX_COMPILED_EXPRESSION*)malloc(sizeof(MYX_COMPILED_EXPRESSION));
  memset(cexpr, 0, sizeof(MYX_COMPILED_EXPRESSION));

  stack= (char*)malloc(strlen(expr)*sizeof(char));
  
  while (*pp) 
  {
    int op= *pp;
    
    switch (op) 
    {
    case '(':
      stack[stp++]= op;
      pp++;
      break;

    case ')':
      while (stp > 0 // stack empty
             && stack[stp-1]!='(') 
      {
        stp--;
        add_operator(cexpr, stack[stp]);
      }
      if (stp==0)
      {
        // SYNTAX ERROR: stack underflow
        goto syntax_error;
      }
      stp--; // discard (
      pp++;
      break;

    case '+':
    case '-':
    case '*':
    case '/':
      while (stp > 0 // stack empty
             && get_precedence(op) > get_precedence(stack[stp-1]) // precedence in stack is lower
             && stack[stp-1]!='(') 
      {
        stp--;
        add_operator(cexpr, stack[stp]);
      }      
      stack[stp++]= op;
      pp++;
      break;

    case ' ':
      pp++;
      break;
      
    case '[': // variable index
      {
        int index;
        char *ptr= tmpbuf;

        ++pp;
        while (*pp && *pp != ']')
        {
          *ptr= *pp;
          ptr++;
          pp++;
        }
        *ptr= 0;
        
        index= atoi(tmpbuf);
        add_operand(cexpr, 'V', index);
        
        pp++;
      }
      break;
      
    case '^': // delta variable index
      {
        int index;
        char *ptr= tmpbuf;

        ++pp; // skip ^
        ++pp; // skip [
        while (*pp && *pp != ']')
        {
          *ptr= *pp;
          ptr++;
          pp++;
        }
        *ptr= 0;

        index= atoi(tmpbuf);
        add_operand(cexpr, 'D', index);

        pp++;
      }
      break;

    case '0': // immediate operand
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      {
        double val;
        char *ptr= tmpbuf;

        while (*pp && (isdigit(*pp) || *pp=='.' ))
        {
          *ptr= *pp;
          ptr++;
          pp++;
        }
        *ptr= 0;
        val= strtod(tmpbuf, NULL);
        add_ioperand(cexpr, val);
      }
      break;

    default:
      goto syntax_error;
    }
  }  
  while (stp > 0) {
    stp--;
    add_operator(cexpr, stack[stp]);
  }

  free(stack);
  free(tmpbuf);

  *error= MYX_EXPRESSION_NO_ERROR;
  
  return cexpr;
  
syntax_error:
  free(stack);
  free(tmpbuf);
  free(cexpr);

  *error= MYX_EXPRESSION_SYNTAX_ERROR;

  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * myx_eval_expression
 *
 * SYNOPSIS
 *     Evaluate an expression compiled with myx_compile_expression,
 * doing appropriate variable substitutions.
 *
 * DESCRIPTION
 *
 * RETURN VALUE
 *
 * NOTES
 */
double myx_eval_expression(MYX_COMPILED_EXPRESSION *expr,
                           MYX_VARIABLES *old_vars,
                           MYX_VARIABLES *vars,
                           MYX_EXPRESSION_ERROR *error)
{
  double *stack= malloc(sizeof(double)*expr->nodes_num);
  double total;
  int stp= 0;
  unsigned int i;

  *error= MYX_EXPRESSION_NO_ERROR;
  
  for (i= 0; i < expr->nodes_num; i++)
  {
    switch (expr->nodes[i].ntype)
    {
    case 'V':
      {
        if (expr->nodes[i].variable_index >= (int)vars->variables_num)
        {
          *error= MYX_EXPRESSION_BAD_VARIABLE;
          return 0.0;
        }
        stack[stp++]= strtod((char*)vars->variables[expr->nodes[i].variable_index].value, NULL);
      }
      break;
    case 'D':
      {
        double nval, oval;
        if (expr->nodes[i].variable_index >= (int)vars->variables_num ||
            expr->nodes[i].variable_index >= (int)old_vars->variables_num)
        {
          *error= MYX_EXPRESSION_BAD_VARIABLE;
          return 0.0;
        }
        nval= strtod((char*)vars->variables[expr->nodes[i].variable_index].value, NULL);
        oval= strtod((char*)old_vars->variables[expr->nodes[i].variable_index].value, NULL);
        if (oval < nval)
          stack[stp++]= nval-oval;
        else
          stack[stp++]= 0.0;
      }
      break;
    case 'I':
      {
        stack[stp++]= expr->nodes[i].immediate;
      }
      break;
    case '+':
      {
        double a,b;
        double res;
        if (stp < 2)
        {
          // ERROR
          goto syntax_error;
        }
        b= stack[--stp];
        a= stack[--stp];
        res= a+b;
        stack[stp++]= res;
      }
      break;
    case '-':
      {
        double a,b;
        double res;
        if (stp < 2)
        {
          // ERROR
          goto syntax_error;
        }
        b= stack[--stp];
        a= stack[--stp];
        res= a-b;
        stack[stp++]= res;
      }
      break;
    case '*':
      {
        double a,b;
        double res;
        if (stp < 2)
        {
          // ERROR
          goto syntax_error;
        }
        b= stack[--stp];
        a= stack[--stp];
        res= a*b;
        stack[stp++]= res;
      }
      break;
    case '/':
      {
        double a,b;
        double res;
        if (stp < 2)
        {
          // ERROR
          goto syntax_error;
        }
        b= stack[--stp];
        a= stack[--stp];
        if (b == 0.0)
        {
          // division by zero
          goto math_error;
        }
        res= a/b;
        stack[stp++]= res;
      }
      break;
    }
  }
  
  if (stp > 1)
  {
    goto syntax_error;
  }

  if (stack != NULL)
  {
    total= stack[0];
    free(stack);
  }
  else
    total= 0;

  return total;

syntax_error:
  free(stack);
  *error= MYX_EXPRESSION_SYNTAX_ERROR;
  return 0;

math_error:
  free(stack);
  *error= MYX_EXPRESSION_DIVISION_BY_ZERO;
  return 0;
}


//----------------------------------------------------------------------------------------------------------------------

int myx_free_expression(MYX_COMPILED_EXPRESSION *expr)
{
  if (expr->nodes)
    free(expr->nodes);
  free(expr);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

int myx_free_admin_lib_stringlist(MYX_STRINGLIST *stringlist)
{
  return myx_free_stringlist(stringlist);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Function used for sanity checks. It returns the size of a structure. The caller can then compare it with what
 * the compiler used on its side, detecting so wrong compiler switches and other things.
 *
 * @return The size of the MYX_BS_STATUS structure.
 */
int get_bs_status_size(void)
{
  return sizeof(MYX_BS_STATUS);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Function used for sanity checks. It returns the size of an enum. The caller can then compare it with what
 * the compiler used on its side, detecting so wrong compiler switches and other things.
 *
 * @return The size of the MYX_BS_STATUS structure.
 */
int get_lib_error_size(void)
{
  return sizeof(MYX_ADMIN_LIB_ERROR);
}

//----------------------------------------------------------------------------------------------------------------------


