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

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <my_global.h>
#include <m_ctype.h>
#endif


#include <myx_util_public_interface.h>
#include <myx_qb_library.h>

// needed from my_global.h, cant include it because of conflicts
#ifdef __GNUC__
typedef char    pchar;          /* Mixed prototypes can take char */
typedef char    puchar;         /* Mixed prototypes can take char */
typedef char    pbool;          /* Mixed prototypes can take char */
typedef short   pshort;         /* Mixed prototypes can take short int */
typedef float   pfloat;         /* Mixed prototypes can take float */
#endif

/*
 * functions
 */

int myx_get_qb_public_interface_version()
{
  return libmysqlqb_PUBLIC_INTERFACE_VERSION;
}

static const char *explain_fields[]= {
  "id",            // 0
  "select_type",   // 1
  "table",         // 2
  "type",          // 3
  "possible_keys", // 4
  "key",           // 5
  "key_len",       // 6
  "ref",           // 7
  "rows",          // 8
  "Extra"          // 9
};

MYX_EXPLAIN_RESULT *myx_query_explain(MYSQL *mysql, const char *query)
{
  MYX_EXPLAIN_RESULT *result= NULL;
  MYSQL_RES *res;
  MYSQL_ROW row;
  char *tmp= g_strdup_printf("EXPLAIN %s", query);
  
  if (myx_mysql_query(mysql, tmp) < 0)
  {
    g_free(tmp);
    return NULL;
  }
  g_free(tmp);
  
  if ((res= mysql_store_result(mysql)) != NULL)
  {
    unsigned int i= 0;

    unsigned int num_fields = mysql_num_fields(res);
    MYSQL_FIELD * fields= mysql_fetch_fields(res);
    int fi[10];
    build_field_subst(explain_fields,explain_fields+sizeof(explain_fields)/sizeof(char*),
                      fields,fields+num_fields,fi);

    result= g_malloc(sizeof(MYX_EXPLAIN_RESULT));
    
    result->rows_num= mysql_num_rows(res);
    result->rows= g_malloc(sizeof(MYX_EXPLAIN_ROW)*(gulong)result->rows_num);

    while ((row= mysql_fetch_row(res)) != NULL)
    {
      MYX_EXPLAIN_ROW *expl= result->rows+i++;

#define GET_STR_FIELD(n) fi[n]==-1 ? NULL : g_strdup(row[fi[n]]?row[fi[n]]:"")
      expl->id= GET_STR_FIELD(0);
      expl->select_type= GET_STR_FIELD(1);
      expl->table= GET_STR_FIELD(2);
      expl->join_type= GET_STR_FIELD(3);
      expl->possible_keys= fi[4]>=0 && row[fi[4]] ?g_strsplit(row[fi[4]], ",", -1):NULL;
      expl->possible_keys_num= 0;
      if (expl->possible_keys)
        while (expl->possible_keys[expl->possible_keys_num])
          expl->possible_keys_num++;
      expl->key= GET_STR_FIELD(5);
      expl->key_len= GET_STR_FIELD(6);
      expl->ref= GET_STR_FIELD(7);
      expl->rows= GET_STR_FIELD(8);
      expl->extra= GET_STR_FIELD(9);
#undef GET_STR_FIELD
    }
  }

  return result;
}


int myx_free_explain_result(MYX_EXPLAIN_RESULT *res)
{
  unsigned int i;

  for (i= 0; i < res->rows_num; i++)
  {
    g_free(res->rows[i].id);
    g_free(res->rows[i].select_type);
    g_free(res->rows[i].table);
    g_free(res->rows[i].join_type);
    if (res->rows[i].possible_keys)
      g_strfreev(res->rows[i].possible_keys);
    g_free(res->rows[i].key);
    g_free(res->rows[i].key_len);
    g_free(res->rows[i].ref);
    g_free(res->rows[i].rows);
    g_free(res->rows[i].extra);
  }
  g_free(res->rows);
  g_free(res);

  return 0;  
}
