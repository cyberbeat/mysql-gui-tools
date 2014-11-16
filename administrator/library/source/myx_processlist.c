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

#include <myx_admin_library.h>
#include <myx_util_functions.h>

/*
 * public functions definitions
 */

static const char * show_processes_columns[]=
{
  "Id",       // 0
  "User",     // 1
  "Host",     // 2
  "db",       // 3
  "Command",  // 4
  "Time",     // 5
  "State",    // 6
  "Info",     // 7
};
static const char ** show_processes_columns_end=
         show_processes_columns + sizeof(show_processes_columns)/sizeof(char*);

MYX_PROCESS_LIST * myx_get_process_list(MYSQL *mysql)
{
  MYX_PROCESS_LIST *process_list= g_malloc(sizeof(MYX_PROCESS_LIST));
  MYX_PROCESS_INFO *process_info;
  MYSQL_RES* res;
  MYSQL_ROW row;

  process_list->process_infos_num= 0;

  res= NULL;
  if (myx_mysql_query(mysql, "SHOW FULL PROCESSLIST") == 0)
    res= mysql_store_result(mysql);
  if (res == NULL)
  {
    free(process_list);
    process_list= NULL;
  }
  else
  {
    process_list->process_infos_num= (int)mysql_num_rows(res);
    process_info= process_list->process_infos= g_malloc(sizeof(MYX_PROCESS_INFO)*process_list->process_infos_num);

    do
    {
      unsigned int num_fields;
      MYSQL_FIELD * fields;
      int fi[8];

      row= mysql_fetch_row(res);
      if (row == NULL)
        break;

      // Depending on the version of the server there might be different columns
      num_fields = mysql_num_fields(res);
      fields= mysql_fetch_fields(res);
      build_field_subst(show_processes_columns,show_processes_columns_end, fields, fields + num_fields, fi);

#define GET_STR_FIELD(n) fi[n]==-1 ? NULL : g_strdup(row[fi[n]]?row[fi[n]]:"")
      process_info->id=       GET_STR_FIELD(0);
      process_info->user=     GET_STR_FIELD(1);
      process_info->host=     GET_STR_FIELD(2);
      process_info->db=       GET_STR_FIELD(3);
      process_info->command=  GET_STR_FIELD(4);
      process_info->time=     GET_STR_FIELD(5);
      process_info->state=    GET_STR_FIELD(6);
      process_info->info=     GET_STR_FIELD(7);
#undef GET_STR_FIELD
      process_info++;
    }
    while (1);
      
    mysql_free_result(res);
  }
  return process_list;
}

int myx_free_process_list(MYX_PROCESS_LIST *process_list)
{
  MYX_PROCESS_INFO * process_info= process_list->process_infos;
  MYX_PROCESS_INFO * process_info_end=
                                process_info + process_list->process_infos_num;

  for (;process_info!=process_info_end;process_info++)
  {
    g_free(process_info->id);
    g_free(process_info->user);
    g_free(process_info->host);
    g_free(process_info->db);
    g_free(process_info->command);
    g_free(process_info->time);
    g_free(process_info->state);
    g_free(process_info->info);
  }

  g_free(process_list->process_infos);
  g_free(process_list);

  return 0;
}

int myx_kill_thread(MYSQL *mysql, unsigned long pid)
{
  return mysql_kill(mysql, pid);
}


