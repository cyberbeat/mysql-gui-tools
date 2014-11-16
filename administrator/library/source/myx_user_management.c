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

#include "myx_admin_library.h"

#define USER_INFO_TABLENAME "user_info"

/*
 * public functions declarations
 */


int myx_is_table(MYSQL *mysql, const char *schema, const char *table);
// myx_is_procedure will return false/0 for functions
int myx_is_procedure(MYSQL *mysql, const char *schema, const char *proc);

/*
 * public functions definitions
 */

MYX_USER_NAMES * myx_get_user_names(MYSQL *mysql)
{
  MYSQL_RES* res;

  //Get Total number of users and hosts
  //to allocate one block of memory
  const char *query= "SELECT DISTINCT User FROM mysql.user ORDER BY User";
  res= NULL;
  if (myx_mysql_query(mysql,query) == 0)
    res= mysql_store_result(mysql);
  if (res == NULL)
  {
    return NULL;
  }
  else
  {
    MYSQL_ROW row;
    MYX_USER_NAMES *user_names= g_malloc0(sizeof(MYX_USER_NAMES));
    char ** user_name;

    user_names->user_names_num= (int)mysql_num_rows(res);
    user_name= user_names->user_names= g_malloc0(sizeof(char *)*user_names->user_names_num);

    do
    {
      row= mysql_fetch_row(res);
      if (row == NULL)
        break;

      *user_name= myx_convert_dbstr_utf8(mysql, row[0], -1);
      user_name++;
    }
    while (1);

    mysql_free_result(res);
    return user_names;
  }
}

int myx_free_user_names(MYX_USER_NAMES *user_names)
{
  unsigned int i;

  for(i= 0;i<user_names->user_names_num;i++)
  {
    g_free(user_names->user_names[i]);
  }
  g_free(user_names->user_names);
  g_free(user_names);

  return 0;
}

char * q_create_user_info_base=
  "CREATE TABLE mysql." USER_INFO_TABLENAME " ("
  " User VARCHAR(16) NOT NULL,"
  " Full_name VARCHAR(60),"
  " Description VARCHAR(255),"
  " Email VARCHAR(80),"
  " Contact_information TEXT,"
  " Icon BLOB,"
  " PRIMARY KEY(User),"
  " INDEX user_info_Full_name(Full_name)"
  ")"
  " TYPE=MYISAM"
  " COMMENT='Stores additional user information'";

char * q_create_user_info_utf8=
  "CREATE TABLE mysql." USER_INFO_TABLENAME " ("
  " User VARCHAR(16) NOT NULL,"
  " Full_name VARCHAR(60),"
  " Description VARCHAR(255),"
  " Email VARCHAR(80),"
  " Contact_information TEXT,"
  " Icon BLOB,"
  " PRIMARY KEY(User),"
  " INDEX user_info_Full_name(Full_name)"
  ")"
  " TYPE=MYISAM"
  " COMMENT='Stores additional user information'"
  " CHARACTER SET UTF8";

char * q_create_user_info_utf8_bin=
  "CREATE TABLE mysql." USER_INFO_TABLENAME " ("
  " User VARCHAR(16) NOT NULL,"
  " Full_name VARCHAR(60),"
  " Description VARCHAR(255),"
  " Email VARCHAR(80),"
  " Contact_information TEXT,"
  " Icon BLOB,"
  " PRIMARY KEY(User),"
  " INDEX user_info_Full_name(Full_name)"
  ")"
  " TYPE=MYISAM"
  " COMMENT='Stores additional user information'"
  " CHARACTER SET UTF8 COLLATE utf8_bin";

/**
 * Checks if the user info table already exists in the current schema and creates it if not.
 * Creating this table requires proper rights to change the mysql schema, so this is usually limited to root users.
 *
 * @param mysql The current connection settings.
 * @param check_only If 1 then only the existance of the table and proper access rights are checked. If 0 then the table
 *                   is created if it does not yet exist.
 */
int myx_check_mysql_user_info_table(MYSQL *mysql, int check_only)
{
  MYSQL_RES* res;
  int result;
  char * old_schema;

  if (!use_schema_store_old_one(mysql,"mysql",&old_schema))
    return 0;

  res= mysql_list_tables(mysql, USER_INFO_TABLENAME);
  if (res == NULL)
  {
    result= 0;
  }
  else if (mysql_num_rows(res))
  { 
    result= 1;
  }
  else //If the USER_INFO_TABLE does not exist, create it
  {
    const char *query;

    //Create table
    if (mysql_full_version_is_later_or_equal_than(mysql,5,0,3))
      query= q_create_user_info_utf8_bin;
    else if (mysql_version_is_later_or_equal_than(mysql,5,0))
      query= q_create_user_info_utf8;
    else if (mysql_full_version_is_later_or_equal_than(mysql,4,1,11))
      query= q_create_user_info_utf8_bin;
    else if (mysql_version_is_later_or_equal_than(mysql,4,1))
      query= q_create_user_info_utf8;
    else
      query= q_create_user_info_base;

    mysql_free_result(res);
    result= myx_mysql_query(mysql,query) ? 0 : 1;
  }

  restore_old_schema(mysql,old_schema);

  return result;
}

typedef enum query_variants
{
  q_v_base= 0,
  q_v_4_1= 1,
  q_v_5_0= 2,
  q_v_count
} QUERY_VARIANTS;

char * q_select_user_info[q_v_count]=
{
  "SELECT Full_name, Description, Email, Contact_information, Icon "
  "FROM mysql." USER_INFO_TABLENAME " "
  "WHERE User=@goal_user"
  ,
  "SELECT Full_name, Description, Email, Contact_information, Icon "
  "FROM mysql." USER_INFO_TABLENAME " "
  "WHERE cast(cast(User AS BINARY) AS CHAR CHARACTER SET utf8)=cast(cast(@goal_user AS BINARY) AS CHAR CHARACTER SET utf8)"
  ,
  "SELECT Full_name, Description, Email, Contact_information, Icon "
  "FROM mysql." USER_INFO_TABLENAME " "
  "WHERE cast(cast(User AS BINARY) AS CHAR CHARACTER SET utf8)=cast(cast(@goal_user AS BINARY) AS CHAR CHARACTER SET utf8)"
};

#define WHERE_V_40()\
 " WHERE User=@goal_user"

#define WHERE_V_41()\
 " WHERE cast(cast(User AS BINARY) AS CHAR CHARACTER SET utf8)=cast(cast(@goal_user AS BINARY) AS CHAR CHARACTER SET utf8)"

#define SELECT_USER_PRIV_40(name)\
 "select host as h, NULL as o, '" name "' as pn, "\
                                         name "  as pv from mysql.user"

#define SELECT_USER_PRIV_41(name)\
 "select "\
    "cast(cast(host AS BINARY) AS CHAR CHARACTER SET utf8) as h, "\
    "cast(cast(NULL AS BINARY) AS CHAR CHARACTER SET utf8) as o, "\
    "_utf8'" name "' as pn, "\
    "cast(cast("name " AS BINARY) AS CHAR CHARACTER SET utf8) as pv"\
    " from mysql.user"

#define SELECT_DB_PRIV_40(name)\
 "select host as h, db as o, '" name "' as pn, "\
                                      name "  as pv from mysql.db"

#define SELECT_DB_PRIV_41(name)\
 "select "\
        "cast(cast(host AS BINARY) AS CHAR CHARACTER SET utf8) as h, "\
        "cast(cast(db AS BINARY) AS CHAR CHARACTER SET utf8) as o, "\
        "_utf8'" name "' as pn, "\
        "cast(cast(" name " AS BINARY) AS CHAR CHARACTER SET utf8) as pv"\
        " from mysql.db"

#define SELECT_TABLE_PRIV_40(name)\
 "select host as h, concat(db,'.',table_name) as o, "\
        "'Table_priv_" name "' as pn, "\
        "if(find_in_set('" name "',Table_priv),'Y','N') as pv "\
        "from mysql.tables_priv"

#define SELECT_TABLE_PRIV_41(name)\
 "select "\
    "cast(cast(host AS BINARY) AS CHAR CHARACTER SET utf8) as h, "\
    "concat(cast(cast(db AS BINARY) AS CHAR CHARACTER SET utf8),"\
            "_utf8'.',"\
         "cast(cast(table_name AS BINARY) AS CHAR CHARACTER SET utf8)) as o, "\
    "_utf8'Table_priv_" name "' as pn, "\
    "if(find_in_set(_utf8'" name "', "\
        "cast(cast(Table_priv AS BINARY) AS CHAR CHARACTER SET utf8)),"\
          "_utf8'Y',_utf8'N') as pv "\
    "from mysql.tables_priv"

#define SELECT_PROC_PRIV_50(name)\
 "select "\
    "cast(cast(host AS BINARY) AS CHAR CHARACTER SET utf8) as h, "\
    "concat(cast(cast(db AS BINARY) AS CHAR CHARACTER SET utf8),"\
            "_utf8'.',"\
         "cast(cast(routine_name AS BINARY) AS CHAR CHARACTER SET utf8)) as o, "\
    "_utf8'Proc_priv_" name "' as pn, "\
    "if(find_in_set(_utf8'" name "', "\
        "cast(cast(Proc_priv AS BINARY) AS CHAR CHARACTER SET utf8)),"\
          "_utf8'Y',_utf8'N') as pv "\
    "from mysql.procs_priv"

#define SELECT_TABLE_COLUMNS_PRIV_40(name)\
 "select host as h, concat(db,'.',table_name) as o, "\
                "'Column_priv_" name "' as pn, "\
                "if(find_in_set('" name "',Column_priv),'Y','N') as pv "\
                "from mysql.tables_priv"

#define SELECT_TABLE_COLUMNS_PRIV_41(name)\
 "select "\
        "cast(cast(host AS BINARY) AS CHAR CHARACTER SET utf8) as h, "\
        "concat(cast(cast(db AS BINARY) AS CHAR CHARACTER SET utf8),"\
                "_utf8'.',"\
         "cast(cast(table_name AS BINARY) AS CHAR CHARACTER SET utf8)) as o, "\
        "_utf8'Column_priv_" name "' as pn, "\
        "if(find_in_set(_utf8'" name "',"\
               "cast(cast(Column_priv AS BINARY) AS CHAR CHARACTER SET utf8))"\
               ",_utf8'Y',_utf8'N') as pv "\
     "from mysql.tables_priv"

#define SELECT_TABLE_COLUMN_PRIV_40(name)\
  "select host as h, concat(db,'.',table_name,'.',column_name) as o, "\
                "'Column_priv_" name "' as pn, "\
                "if(find_in_set('" name "',Column_priv),'Y','N') as pv "\
                "from mysql.columns_priv"

#define SELECT_TABLE_COLUMN_PRIV_41(name)\
  "select "\
         "cast(cast(host AS BINARY) AS CHAR CHARACTER SET utf8) as h, "\
         "concat(cast(cast(db AS BINARY) AS CHAR CHARACTER SET utf8),"\
            "_utf8'.',"\
            "cast(cast(table_name AS BINARY) AS CHAR CHARACTER SET utf8),"\
            "_utf8'.',"\
        "cast(cast(column_name AS BINARY) AS CHAR CHARACTER SET utf8)) as o, "\
         "_utf8'Column_priv_" name "' as pn, "\
         "if(find_in_set(_utf8'" name "',"\
             "cast(cast(Column_priv AS BINARY) AS CHAR CHARACTER SET utf8)),"\
             "_utf8'Y',_utf8'N') as pv "\
      "from mysql.columns_priv"

char * q_select_user_40[]=
{
  SELECT_USER_PRIV_40(           "Select_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(           "Insert_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(           "Update_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(           "Delete_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(           "Create_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(             "Drop_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(           "Reload_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(         "Shutdown_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(          "Process_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(             "File_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(            "Grant_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(       "References_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(            "Index_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(            "Alter_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(          "Show_db_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(            "Super_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40( "Create_tmp_table_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(      "Lock_tables_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(          "Execute_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(       "Repl_slave_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40(      "Repl_client_priv" ) WHERE_V_40(),
  SELECT_USER_PRIV_40( "max_questions"         ) WHERE_V_40(),
  SELECT_USER_PRIV_40( "max_updates"           ) WHERE_V_40(),
  SELECT_USER_PRIV_40( "max_connections"       ) WHERE_V_40()
};

char * q_select_user_41[]=
{
  SELECT_USER_PRIV_41(           "Select_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(           "Insert_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(           "Update_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(           "Delete_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(           "Create_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(             "Drop_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(           "Reload_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(         "Shutdown_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(          "Process_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(             "File_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(            "Grant_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(       "References_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(            "Index_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(            "Alter_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(          "Show_db_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(            "Super_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41( "Create_tmp_table_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(      "Lock_tables_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(          "Execute_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(       "Repl_slave_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(      "Repl_client_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41( "max_questions"         ) WHERE_V_41(),
  SELECT_USER_PRIV_41( "max_updates"           ) WHERE_V_41(),
  SELECT_USER_PRIV_41( "max_connections"       ) WHERE_V_41()
};

char * q_select_user_50[]=
{
  SELECT_USER_PRIV_41(           "Select_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(           "Insert_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(           "Update_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(           "Delete_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(           "Create_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(             "Drop_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(           "Reload_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(         "Shutdown_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(          "Process_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(             "File_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(            "Grant_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(       "References_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(            "Index_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(            "Alter_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(          "Show_db_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(            "Super_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41( "Create_tmp_table_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(      "Lock_tables_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(          "Execute_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(       "Repl_slave_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(      "Repl_client_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(      "Create_view_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(        "Show_view_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(   "Create_routine_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41(    "Alter_routine_priv" ) WHERE_V_41(),
  SELECT_USER_PRIV_41( "max_questions"         ) WHERE_V_41(),
  SELECT_USER_PRIV_41( "max_updates"           ) WHERE_V_41(),
  SELECT_USER_PRIV_41( "max_connections"       ) WHERE_V_41(),
  SELECT_USER_PRIV_41( "max_user_connections"  ) WHERE_V_41()  
};

typedef struct 
{
  char ** first_query;
  char ** end_query;
}Query_privs;

Query_privs q_select_user_privs[q_v_count]=
{
  {
    q_select_user_40,
    q_select_user_40 + sizeof(q_select_user_40)/sizeof(*q_select_user_40)
  },
  {
    q_select_user_41,
    q_select_user_41 + sizeof(q_select_user_41)/sizeof(*q_select_user_41)
  },
  {
    q_select_user_50,
    q_select_user_50 + sizeof(q_select_user_50)/sizeof(*q_select_user_50)
  }
};

char * q_select_db_40[]=
{
  SELECT_DB_PRIV_40(           "Select_priv" )   WHERE_V_40(),
  SELECT_DB_PRIV_40(           "Insert_priv" )   WHERE_V_40(),
  SELECT_DB_PRIV_40(           "Update_priv" )   WHERE_V_40(),
  SELECT_DB_PRIV_40(           "Delete_priv" )   WHERE_V_40(),
  SELECT_DB_PRIV_40(           "Create_priv" )   WHERE_V_40(),
  SELECT_DB_PRIV_40(             "Drop_priv" )   WHERE_V_40(),
  SELECT_DB_PRIV_40(            "Grant_priv" )   WHERE_V_40(),
  SELECT_DB_PRIV_40(       "References_priv" )   WHERE_V_40(),
  SELECT_DB_PRIV_40(            "Index_priv" )   WHERE_V_40(),
  SELECT_DB_PRIV_40(            "Alter_priv" )   WHERE_V_40(),
  SELECT_DB_PRIV_40( "Create_tmp_table_priv" )   WHERE_V_40(),
  SELECT_DB_PRIV_40(      "Lock_tables_priv" )   WHERE_V_40()
};

char * q_select_db_41[]=
{
  SELECT_DB_PRIV_41(           "Select_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(           "Insert_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(           "Update_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(           "Delete_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(           "Create_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(             "Drop_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(            "Grant_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(       "References_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(            "Index_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(            "Alter_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41( "Create_tmp_table_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(      "Lock_tables_priv" )   WHERE_V_41()
};

char * q_select_db_50[]=
{
  SELECT_DB_PRIV_41(           "Select_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(           "Insert_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(           "Update_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(           "Delete_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(           "Create_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(             "Drop_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(            "Grant_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(       "References_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(            "Index_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(            "Alter_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41( "Create_tmp_table_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(      "Lock_tables_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(      "Create_view_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(        "Show_view_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(   "Create_routine_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(    "Alter_routine_priv" )   WHERE_V_41(),
  SELECT_DB_PRIV_41(          "Execute_priv" )   WHERE_V_41()
};

Query_privs q_select_db_privs[q_v_count]=
{
  {
    q_select_db_40,
    q_select_db_40 + sizeof(q_select_db_40)/sizeof(*q_select_db_40)
  },
  {
    q_select_db_41,
    q_select_db_41 + sizeof(q_select_db_41)/sizeof(*q_select_db_41)
  },
  {
    q_select_db_50,
    q_select_db_50 + sizeof(q_select_db_50)/sizeof(*q_select_db_50)
  }
};

char * q_select_tables_40[]=
{
  SELECT_TABLE_COLUMNS_PRIV_40( "Select"     )   WHERE_V_40(),
  SELECT_TABLE_COLUMNS_PRIV_40( "Insert"     )   WHERE_V_40(),
  SELECT_TABLE_COLUMNS_PRIV_40( "Update"     )   WHERE_V_40(),
  SELECT_TABLE_COLUMNS_PRIV_40( "References" )   WHERE_V_40(),

  SELECT_TABLE_PRIV_40( "Select"     )           WHERE_V_40(),
  SELECT_TABLE_PRIV_40( "Insert"     )           WHERE_V_40(),
  SELECT_TABLE_PRIV_40( "Update"     )           WHERE_V_40(),
  SELECT_TABLE_PRIV_40( "Delete"     )           WHERE_V_40(),
  SELECT_TABLE_PRIV_40( "Create"     )           WHERE_V_40(),
  SELECT_TABLE_PRIV_40( "Drop"       )           WHERE_V_40(),
  SELECT_TABLE_PRIV_40( "Grant"      )           WHERE_V_40(),
  SELECT_TABLE_PRIV_40( "References" )           WHERE_V_40(),
  SELECT_TABLE_PRIV_40( "Index"      )           WHERE_V_40(),
  SELECT_TABLE_PRIV_40( "Alter"      )           WHERE_V_40()
};

char * q_select_tables_41[]=
{
  SELECT_TABLE_COLUMNS_PRIV_41( "Select"     )   WHERE_V_41(),
  SELECT_TABLE_COLUMNS_PRIV_41( "Insert"     )   WHERE_V_41(),
  SELECT_TABLE_COLUMNS_PRIV_41( "Update"     )   WHERE_V_41(),
  SELECT_TABLE_COLUMNS_PRIV_41( "References" )   WHERE_V_41(),

  SELECT_TABLE_PRIV_41( "Select"     )           WHERE_V_41(),
  SELECT_TABLE_PRIV_41( "Insert"     )           WHERE_V_41(),
  SELECT_TABLE_PRIV_41( "Update"     )           WHERE_V_41(),
  SELECT_TABLE_PRIV_41( "Delete"     )           WHERE_V_41(),
  SELECT_TABLE_PRIV_41( "Create"     )           WHERE_V_41(),
  SELECT_TABLE_PRIV_41( "Drop"       )           WHERE_V_41(),
  SELECT_TABLE_PRIV_41( "Grant"      )           WHERE_V_41(),
  SELECT_TABLE_PRIV_41( "References" )           WHERE_V_41(),
  SELECT_TABLE_PRIV_41( "Index"      )           WHERE_V_41(),
  SELECT_TABLE_PRIV_41( "Alter"      )           WHERE_V_41()
};

Query_privs q_select_tables_privs[q_v_count]=
{
  {
    q_select_tables_40,
    q_select_tables_40 + sizeof(q_select_tables_40)/sizeof(*q_select_tables_40)
  },
  {
    q_select_tables_41,
    q_select_tables_41 + sizeof(q_select_tables_41)/sizeof(*q_select_tables_41)
  },
  {
    q_select_tables_41,
    q_select_tables_41 + sizeof(q_select_tables_41)/sizeof(*q_select_tables_41)
  }
};

char * q_select_columns_40[]=
{
  SELECT_TABLE_COLUMN_PRIV_40( "Select"     )    WHERE_V_40(),
  SELECT_TABLE_COLUMN_PRIV_40( "Insert"     )    WHERE_V_40(),
  SELECT_TABLE_COLUMN_PRIV_40( "Update"     )    WHERE_V_40(),
  SELECT_TABLE_COLUMN_PRIV_40( "References" )    WHERE_V_40()
};

char * q_select_columns_41[]=
{
  SELECT_TABLE_COLUMN_PRIV_41( "Select"     )    WHERE_V_41(),
  SELECT_TABLE_COLUMN_PRIV_41( "Insert"     )    WHERE_V_41(),
  SELECT_TABLE_COLUMN_PRIV_41( "Update"     )    WHERE_V_41(),
  SELECT_TABLE_COLUMN_PRIV_41( "References" )    WHERE_V_41()
};

Query_privs q_select_columns_privs[q_v_count]=
{
  {
    q_select_columns_40,
    q_select_columns_40 +
                       sizeof(q_select_columns_40)/sizeof(*q_select_columns_40)
  },
  {
    q_select_columns_41,
    q_select_columns_41 +
                       sizeof(q_select_columns_41)/sizeof(*q_select_columns_41)
  },
  {
    q_select_columns_41,
    q_select_columns_41 +
                       sizeof(q_select_columns_41)/sizeof(*q_select_columns_41)
  }
};

char * q_select_procs_50[]=
{
  SELECT_PROC_PRIV_50( "Execute"       )    WHERE_V_41(),
  SELECT_PROC_PRIV_50( "Alter routine" )    WHERE_V_41(),
  SELECT_PROC_PRIV_50( "Grant"         )    WHERE_V_41()
};

Query_privs q_select_procs_privs[q_v_count]=
{
  {
    q_select_procs_50,
    q_select_procs_50
  },
  {
    q_select_procs_50,
    q_select_procs_50
  },
  {
    q_select_procs_50,
    q_select_procs_50 +
                      sizeof(q_select_procs_50)/sizeof(*q_select_procs_50)
  }
};

static char *replace_escaped(MYSQL *mysql, const char *query, 
                             const char *var, const char *value)
{
  char *esc_value= g_malloc((gulong)strlen(value)*2+8);
  char *res;
  
  strcpy(esc_value, "'");
  mysql_real_escape_string(mysql, esc_value+1, value, (gulong)strlen(value));
  strcat(esc_value, "'");

  res= str_g_subst(query, var, esc_value);
  g_free(esc_value);
  
  return res;
}

char * myx_resize_vector_block(char * ptr,
                               size_t size_of_block,
                               size_t count_of_blocks)
{
  size_t * reserved_ptr;
  if (!ptr)
  {
    reserved_ptr= g_malloc0((gulong)(sizeof(size_t) +
                                    count_of_blocks * size_of_block));
    if (!reserved_ptr)
      return 0;
    *reserved_ptr= count_of_blocks;
    return (char*)(reserved_ptr + 1);
  }
  else
  {
    reserved_ptr= ((size_t*)ptr)-1;
    if (count_of_blocks <= *reserved_ptr)
    {
      return ptr;
    }
    else
    {
      count_of_blocks+= count_of_blocks/3;
      reserved_ptr= g_realloc(reserved_ptr,
                              (gulong)(sizeof(size_t) +
                                       count_of_blocks * size_of_block));
    }
  }
  if (!reserved_ptr)
    return 0;
  *reserved_ptr= count_of_blocks;
  return (char*)(reserved_ptr + 1);
}

void myx_free_vector_block(char * ptr)
{
  if (ptr)
  {
    size_t * reserved_size_ptr= ((size_t*)ptr)-1;
    g_free(reserved_size_ptr);
  }
}

#ifdef __GNUC__
// same as the alternative version, but this won't trigger warnings from gcc
# define MYX_VECTOR_RESERVE(ptr,size)\
    (ptr)= (typeof(ptr))myx_resize_vector_block((char*)(ptr),sizeof(*(ptr)),\
                                               (size_t)(size))

# define MYX_VECTOR_PUSH_BACK(ptr,num,ptr_to_top)\
      {\
        (num)++;\
        (ptr)= (typeof(ptr))myx_resize_vector_block((char*)(ptr),sizeof(*(ptr)),(num));\
        (ptr_to_top)= (ptr) + (num) - 1;\
      }

# define MYX_VECTOR_FREE(ptr) myx_free_vector_block((char*)ptr)
#else
# define MYX_VECTOR_RESERVE(ptr,size)\
    (((char*)(ptr))= myx_resize_vector_block((char*)(ptr),sizeof(*(ptr)),\
                                               (size_t)(size)))

# define MYX_VECTOR_PUSH_BACK(ptr,num,ptr_to_top)\
      {\
        (num)++;\
        ((char*)(ptr))= myx_resize_vector_block((char*)(ptr),sizeof(*(ptr)),(num));\
        (ptr_to_top)= (ptr) + (num) - 1;\
      }

# define MYX_VECTOR_FREE(ptr) myx_free_vector_block((char*)ptr)
#endif

static const char * select_privileges_names[]=
{
  "h",  // 0 host
  "o",  // 1 object_name
  "pn", // 2 privilege_name
  "pv", // 3 privilege_value
};
static const char ** select_privileges_names_end=
       select_privileges_names + sizeof(select_privileges_names)/sizeof(char*);

MYX_USER_OBJECT_PRIVILEGES * look_for_obj_name(MYX_USER * user, 
                                               char * object_name,
                                               char * host)
{
  MYX_USER_OBJECT_PRIVILEGES * priv= user->user_object_privileges;
  MYX_USER_OBJECT_PRIVILEGES * priv_end=
                                       priv + user->user_object_privileges_num;
  for (; priv!=priv_end; priv++)
  {
    if (!strcmp(priv->object_name,object_name) && 
        !strcmp(priv->host,host))
      return priv;
  }
  return 0;
}

char ** look_for_host(MYX_USER * user, char * host_name)
{
  char ** host= user->hosts;
  char ** hosts_end= host + user->hosts_num;
  for ( ; host!=hosts_end; host++)
  {
    if (!strcmp(*host,host_name))
      return host;
  }
  return 0;
}

MYX_USER_OBJECT_PRIVILEGES * 
      safe_add_object(MYSQL *mysql, MYX_USER * user,
                      char * object_name, char * host, int get_hosts)
{
  char * chost=   myx_convert_dbstr_utf8(mysql,host, -1);
  char * coname=  myx_convert_dbstr_utf8(mysql, object_name, -1);

  MYX_USER_OBJECT_PRIVILEGES * o_priv= look_for_obj_name(user,coname,chost);
  if (o_priv)
  {
    g_free(chost);
    g_free(coname);
  }
  else
  {
    MYX_VECTOR_PUSH_BACK(user->user_object_privileges,
                         user->user_object_privileges_num,o_priv);
    o_priv->object_name=          coname;
    o_priv->host=                 chost;
    o_priv->user_privileges_num=  0;
    o_priv->user_privileges=      0;
    if (!look_for_host(user,host) && get_hosts)
    {
      char ** added_host;
      MYX_VECTOR_PUSH_BACK(user->hosts,user->hosts_num,added_host);
      *added_host= g_strdup(o_priv->host);
    }
  }
  return o_priv;
}

int get_privileges_from_query(MYSQL *mysql, MYX_USER * user,
                                       const char * query, int get_hosts)
{
  MYSQL_RES * res;
  MYSQL_ROW row;
  my_ulonglong num_rows;
  MYSQL_FIELD * fields;
  int fi[4];
  char *q;
  size_t len;
  
  q= replace_escaped(mysql, query, "@goal_user", user->user_name);
  len= strlen(q);
  res= NULL;
  if (myx_mysql_real_query(mysql, q, (unsigned int)len) == 0)
    res= mysql_store_result(mysql);
  if (res == NULL)
  {
    g_free(q);
    return -1;
  }
  g_free(q);

  num_rows= mysql_num_rows(res);
  if (num_rows==0)
    return 0;

  fields= mysql_fetch_fields(res);
  build_field_subst(select_privileges_names,select_privileges_names_end,
                    fields,fields+mysql_num_fields(res),fi);

  do
  {
    char * host;
    char * object_name;
    char * priv_name;


    row= mysql_fetch_row(res);
    if (row == NULL)
      break;

    host=        fi[0]==-1 ? "" : row[fi[0]];
    object_name= fi[1]==-1 ? "" : row[fi[1]];
    priv_name=   fi[2]==-1 ? NULL : myx_convert_dbstr_utf8(mysql, row[fi[2]], -1);
    if (!object_name)
      object_name= "";
    if (priv_name && priv_name[0]!='+')
    {
      char * priv_value=  fi[3]==-1 ? NULL :
                                     myx_convert_dbstr_utf8(mysql, row[fi[3]], -1);
      MYX_USER_OBJECT_PRIVILEGES *o_priv= safe_add_object(mysql,user,
                                                          object_name,
                                                          host,get_hosts);
      char ** priv;
      MYX_VECTOR_PUSH_BACK(o_priv->user_privileges,
                           o_priv->user_privileges_num,priv);
      *priv= g_strdup_printf("%s=%s", priv_name ? priv_name : "", priv_value ? priv_value : "");
      g_free(priv_value);
    }
    g_free(priv_name);
  }
  while (1);

  mysql_free_result(res);
  return num_rows==0 ? 0 : 1;
}

int get_privileges_from_queries(MYSQL *mysql, MYX_USER * user,
                                Query_privs * queries,
                                int get_hosts)
{
  int res= 0;
  const char ** query;
  for (query= (const char**)queries->first_query; query!=(const char**)queries->end_query; query++)
  {
    int query_res= get_privileges_from_query(mysql,user,*query,get_hosts);
    if (query_res<0)
      return -1;
    if (query_res>0)
      res= 1;
  }
  return res;
}

MYX_USER * myx_get_user_opt_privileges(MYSQL *mysql,
                                       const char *user_name,
                                       int get_privileges)
{
  MYX_USER *user= 0;
  unsigned long * field_lengths;
  char *command;
  MYSQL_RES* res;
  MYSQL_ROW row;
  QUERY_VARIANTS v;
  int use_extended_info;

  use_extended_info= !mysql_version_is_later_or_equal_than(mysql, 6, 0);
  v= mysql_version_is_later_or_equal_than(mysql,5,0) ? q_v_5_0 :
     mysql_version_is_later_or_equal_than(mysql,4,1) ? q_v_4_1 : q_v_base;

  if (use_extended_info) {
    /* can't use local variables, because it won't work with replication on 4.0
     * servers */
    command= replace_escaped(mysql, q_select_user_info[v], "@goal_user", user_name);

    // --------------------------------------------------------------------------
    //Get additional user infos
    if (myx_mysql_query(mysql, command))
    { // error
      g_free(command);
      if (!(myx_check_mysql_user_info_table(mysql, 0)))
        return NULL; // Check if user_info tables exists. if not, create it
    }
    else
    {
      g_free(command);

      res= mysql_store_result(mysql);
      if (res == NULL)
        return NULL; // error

      row= mysql_fetch_row(res);
      if (row != NULL)
      {
        user= g_malloc0(sizeof(MYX_USER));
        user->full_name=           myx_convert_dbstr_utf8(mysql, row[0], -1);
        user->description=         myx_convert_dbstr_utf8(mysql, row[1], -1);
        user->email=               myx_convert_dbstr_utf8(mysql, row[2], -1);
        user->contact_information= myx_convert_dbstr_utf8(mysql, row[3], -1);

        field_lengths= mysql_fetch_lengths(res); // Get field lengths
        user->icon_length= field_lengths[4]; // Copy Icon data
        user->icon= !user->icon_length ? 0 : g_memdup(row[4], user->icon_length);
      }
      mysql_free_result(res);
    }
  }

  if (!user)
  {
    user= g_malloc0(sizeof(MYX_USER));
    user->full_name= g_strdup("");
    user->description= g_strdup("");
    user->email= g_strdup("");
    user->contact_information= g_strdup("");
    user->icon_length= 0;
    user->icon= 0;
  }
  user->user_name= g_strdup(user_name);
  user->password= g_strdup("");

  user->hosts_num= 0;
  user->hosts= 0;

  user->user_object_privileges_num= 0;
  user->user_object_privileges= 0;

  if (get_privileges_from_queries(mysql,user,  q_select_user_privs+v,1)> 0 &&
      get_privileges_from_queries(mysql,user,  q_select_db_privs+v,0)>=0 &&
      (!get_privileges || 
       (get_privileges_from_queries(mysql,user,q_select_tables_privs+v,0)>=0 &&
        get_privileges_from_queries(mysql,user,q_select_columns_privs+v,0)>=0 &&
        get_privileges_from_queries(mysql,user,q_select_procs_privs+v,0)>=0))
     )
  {
    return user;
  }

  myx_free_user(user);
  return NULL;
}

MYX_USER * myx_get_user(MYSQL *mysql, const char *user_name)
{
  return myx_get_user_opt_privileges(mysql, user_name, 0);
}

MYX_USER * myx_get_user_with_privileges(MYSQL *mysql, const char *user_name)
{
  return myx_get_user_opt_privileges(mysql, user_name, 1);
}

int myx_free_user(MYX_USER *user)
{
  unsigned int i;

  g_free(user->user_name);
  g_free(user->password);
  g_free(user->full_name);
  g_free(user->description);
  g_free(user->email);
  g_free(user->contact_information);
  g_free(user->icon);

  for(i=0;i<user->hosts_num;i++)
  {
    g_free(user->hosts[i]);
  }
  MYX_VECTOR_FREE(user->hosts);

  for(i=0;i<user->user_object_privileges_num;i++)
  {
    myx_free_user_priv(user->user_object_privileges+i); 
  }
  MYX_VECTOR_FREE(user->user_object_privileges);

  g_free(user);
  return 0;
}

int myx_free_user_priv(MYX_USER_OBJECT_PRIVILEGES *user_priv)
{
  unsigned int i;

  g_free(user_priv->host);
  g_free(user_priv->object_name);

  for(i=0;i<user_priv->user_privileges_num;i++)
  {
    g_free(user_priv->user_privileges[i]);
  }

  MYX_VECTOR_FREE(user_priv->user_privileges);

  return 0;
}


static char *replace_username(MYSQL *mysql, const char *query,
                            const char *new_name, const char *cur_name)
{
  char *cmd1= replace_escaped(mysql, query, "@new_user", new_name);
  char *cmd2= replace_escaped(mysql, cmd1, "@cur_user", cur_name);
  g_free(cmd1);
  
  return cmd2;
}


static int replace_username_execute(MYSQL *mysql, const char *query,
                                      const char *new_name, const char *cur_name)
{
  char *cmd= replace_username(mysql, query, new_name, cur_name);
  int rc;
  
  rc= myx_mysql_query(mysql, cmd);
  g_free(cmd);
  
  return rc;
}


const char * user_info_update_parts[6]=
{
  "UPDATE mysql." USER_INFO_TABLENAME " SET User=@new_user, Full_name='",
  "', Description='",
  "', Email='",
  "', Contact_information='",
  "', Icon='",
  "' WHERE User=@cur_user"
};

const char * user_info_insert_parts[6]=
{
  "INSERT INTO mysql." USER_INFO_TABLENAME 
  "(User, Full_name, Description, Email, Contact_information, Icon) "
  "VALUES(@new_user, '",
  "', '",
  "', '",
  "', '",
  "', '",
  "')"
};

const char ** user_info_replace_parts[2]=
{
  user_info_update_parts,
  user_info_insert_parts
};

int replace_user_info(MYSQL *mysql, MYX_USER *user, unsigned long name_len,
                      const char * replace_query_parts[6],
                      const char *new_user, const char *cur_user)
{
  int res;
  unsigned long full_name_len= (unsigned long)strlen(user->full_name);
  unsigned long descr_len= (unsigned long)strlen(user->description);
  unsigned long email_len= (unsigned long)strlen(user->email);
  unsigned long contact_len= (unsigned long)strlen(user->contact_information);
  char *tmp;
  char* sqlcmd= g_malloc0(name_len+full_name_len+descr_len+
                          email_len+contact_len+user->icon_length*2+200);

  char * sqlcmd_end;
  
  tmp= replace_username(mysql, replace_query_parts[0], new_user, cur_user);
  sqlcmd_end= strmov(sqlcmd, tmp);
  g_free(tmp);
  sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end,
                                        user->full_name, full_name_len);

  tmp= replace_username(mysql, replace_query_parts[1], new_user, cur_user);
  sqlcmd_end= strmov(sqlcmd_end, tmp);
  g_free(tmp);
  sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end,
                                        user->description, descr_len);

  tmp= replace_username(mysql, replace_query_parts[2], new_user, cur_user);
  sqlcmd_end= strmov(sqlcmd_end, tmp);
  g_free(tmp);
  sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end,
                                        user->email, email_len);

  tmp= replace_username(mysql, replace_query_parts[3], new_user, cur_user);
  sqlcmd_end= strmov(sqlcmd_end, tmp);
  g_free(tmp);
  sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end,
                                        user->contact_information,
                                        contact_len);
  tmp= replace_username(mysql, replace_query_parts[4], new_user, cur_user);
  sqlcmd_end= strmov(sqlcmd_end, tmp);
  g_free(tmp);
  sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end,
                                        user->icon, user->icon_length);
  tmp= replace_username(mysql, replace_query_parts[5], new_user, cur_user);
  sqlcmd_end= strmov(sqlcmd_end, tmp);
  g_free(tmp);

  res= myx_mysql_real_query(mysql, sqlcmd,(unsigned int)(sqlcmd_end-sqlcmd));
  g_free(sqlcmd);
  return res;
}


int myx_set_user(MYSQL *mysql, MYX_USER *user,
                 const char *previous_user_name, int is_new_user)
{
  MYX_USER *current_user;
  char sqlcmd[500];
  char field_buf[30];
  char value_buf[30];
  char *schema_name, *table_name, *column_name;
  const char * begin;
  char *s;
  unsigned int i, j;
  int dot_count, u_variant, num_host;
  int icon_changed = 0;
  char *long_sqlcmd, *sqlcmd_end;
  MYSQL_RES* res;
  unsigned long cur_name_len, name_len= (unsigned long)strlen(user->user_name);
  int utf8_bin= 0;
  const char *new_user= user->user_name;
  const char *cur_user= previous_user_name;
  int is_tbl, is_proc, is_col;

  // Ensure user info table exists before we start.
  myx_check_mysql_user_info_table(mysql, 0);

  if (mysql_full_version_is_later_or_equal_than(mysql, 4, 1, 11)
      && (!mysql_version_is_later_or_equal_than(mysql,5, 0)
          || mysql_full_version_is_later_or_equal_than(mysql, 5, 0, 3)))
    utf8_bin= 1;

  if (utf8_bin)
  {
    if (myx_mysql_query(mysql, "SET collation_connection = utf8_bin"))
    {
      g_message("save user: can't set collation_connection to utf8_bin");
      goto error;
    }
  }

  if (!is_new_user) // Update existing User
  {
    current_user= myx_get_user(mysql, previous_user_name);
    if (current_user == NULL)
    {
      g_message("save user: can't retrieve user information");
      goto error;
    }
    cur_user= current_user->user_name;
    cur_name_len= (unsigned long)strlen(cur_user);

    // check if name has changed
    if (cur_name_len != name_len ||
        strncmp(user->user_name, current_user->user_name,cur_name_len))
    {
      // Update user name in mysql.user table.
      if (replace_username_execute(mysql,
                          "UPDATE mysql.user SET User=@new_user"
                          " WHERE User=@cur_user",
                          new_user, cur_user) ||
          replace_username_execute(mysql,
                          "UPDATE mysql.db SET User=@new_user"
                          " WHERE User=@cur_user",
                          new_user, cur_user) ||
          replace_username_execute(mysql,
                          "UPDATE mysql.tables_priv SET User=@new_user"
                          " WHERE User=@cur_user",
                          new_user, cur_user) ||
          replace_username_execute(mysql,
                          "UPDATE mysql.columns_priv SET User=@new_user"
                          " WHERE User=@cur_user",
                          new_user, cur_user))
      {
        g_message("save user: error updating user name or privilege");
        goto error;
      }
    }

    // Check if all user/host combination has already been created.
    for(i= 0; i<user->hosts_num; i++)
    {
      MYX_STRINGLIST hosts;
      hosts.strings_num= current_user->hosts_num;
      hosts.strings= (char**)current_user->hosts;
      num_host= myx_str_in_stringlist(&hosts,user->hosts[i]);

      // If not, insert a new user/host combination. Take password from existing host.
      if ((num_host == -1) && (user->hosts[i] != NULL) && (strlen(user->hosts[i]) > 0))
      {
        char *cmd, *cmd2;

        cmd= replace_escaped(mysql,
                             "INSERT INTO mysql.user(User, Host, Password, ssl_cipher, x509_issuer, x509_subject)"
                             " SELECT @new_user, @host_name, Password, ssl_cipher, x509_issuer, x509_subject"
                             " FROM mysql.user"
                             " WHERE User=@new_user ORDER BY Host LIMIT 1",
                             "@new_user", new_user);
        cmd2= replace_escaped(mysql, cmd, "@host_name", user->hosts[i]);
        g_free(cmd);

        if (myx_mysql_query(mysql, cmd2))
        {
          g_free(cmd2);
          g_message("save user: error adding new user entry");
          goto error;
        }
        g_free(cmd2);
      }
    }

    //Check for obsolete user/host combination
    for(i=0;i<current_user->hosts_num;i++)
    {
      MYX_STRINGLIST hosts;
      hosts.strings_num= user->hosts_num;
      hosts.strings= (char**)user->hosts;
      num_host= myx_str_in_stringlist(&hosts,current_user->hosts[i]);

      //If there is a obsolete row, delete it
      if (num_host==-1)
      {
        char *cmd, *cmd2;
        
        cmd= replace_escaped(mysql, 
                             "DELETE FROM mysql.user"
                             " WHERE User=@new_user AND Host=@host_name",
                             "@new_user", user->user_name);
        cmd2= replace_escaped(mysql, cmd,
                              "@host_name", current_user->hosts[i]);
        g_free(cmd);

        if (myx_mysql_query(mysql, cmd2))
        {
          g_message("save user: error deleting old user entry");
          g_free(cmd2);
          goto error;
        }
        g_free(cmd2);
      }
    }

    //Check if the icon has changed
    icon_changed= (user->icon_length!=current_user->icon_length ||
                   (user->icon_length > 0 && 
                    memcmp(user->icon,current_user->icon,user->icon_length)));

    //check if user information has changed
    if (strcmp(user->user_name,           current_user->user_name)           ||
        strcmp(user->full_name,           current_user->full_name)           ||
        strcmp(user->description,         current_user->description)         ||
        strcmp(user->email,               current_user->email)               ||
        strcmp(user->contact_information, current_user->contact_information) ||
        icon_changed)
    {
      char *cmd;
      //Check if there already is an entry in the mysql.user_info table

      cmd= replace_escaped(mysql, "SELECT User FROM mysql." USER_INFO_TABLENAME " WHERE User=@cur_user", "@cur_user",
        cur_user);

      res= NULL;
      if (myx_mysql_query(mysql, cmd) == 0)
        res= mysql_store_result(mysql);
      if (res != NULL)
      {
        g_free(cmd);
        u_variant= mysql_num_rows(res) ? 0 : 1;
        mysql_free_result(res);
      }
      else
      {
        g_free(cmd);
        g_message("Error executing query %s:\n%s",cmd,
                  myx_mysql_error(mysql));
        goto error;
      }

      if (replace_user_info(mysql,user,name_len,
                            user_info_replace_parts[u_variant],
                            new_user, cur_user))
      {      
        goto error;
      }
    }

    if (strcmp(user->password, "________"))
    {
      char *cmd, *cmd2;
      
      cmd= replace_escaped(mysql, 
                           "UPDATE mysql.user SET Password=Password(@password)"
                           " WHERE User=@new_user",
                           "@new_user", new_user);
      cmd2= replace_escaped(mysql, cmd, "@password", user->password);
      g_free(cmd);
      
      if (myx_mysql_query(mysql, cmd2))
      {
        g_message("save user: error updating password");
        g_free(cmd2);
        goto error;
      }
      g_free(cmd2);
    }
  }
  else //Create new user
  {
    // Insert into mysql.user table, one insert for each host
    unsigned long pass_len= (unsigned long)strlen(user->password);
    for(i= 0; i<user->hosts_num; i++)
    {
      unsigned long host_len= (unsigned long)strlen(user->hosts[i]);
      
      sqlcmd_end= strmov(sqlcmd, 
                         "INSERT INTO mysql.user(User, Host, Password, ssl_cipher, x509_issuer, x509_subject)"
                         " VALUES('");
      sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end,
                                            new_user, (gulong)strlen(new_user));
      sqlcmd_end= strmov(sqlcmd_end, "', '");
      sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end,
                                            user->hosts[i], host_len);
      sqlcmd_end= strmov(sqlcmd_end, "', Password('");
      sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end, user->password,
                                            pass_len);
      sqlcmd_end= strmov(sqlcmd_end, "'), '', '', '')");

      if (myx_mysql_real_query(mysql, sqlcmd,
                               (unsigned int)(sqlcmd_end-sqlcmd)))
      {
        g_message("save user: error adding new user entry");
        goto error;
      }
    }

    //Insert into mysql.user_info table
    if (user->full_name[0] || user->description[0] || user->email[0] ||
        user->contact_information[0] || user->icon_length>0)
    {
      if (replace_user_info(mysql,user,name_len,
                            user_info_insert_parts,
                            new_user, cur_user))
      {
        g_message("save user: error adding user information entry");
        goto error;
      }
    }
  }

  // Username/host and user_info have been updated or create
  // now update/insert privileges
  if (user->user_object_privileges_num>0)
  {
    char *cmd;

    cmd= replace_escaped(mysql, "DELETE FROM mysql.db WHERE User=@new_user", "@new_user", new_user);

    // Delete previous settings from mysql.db table.
    if (myx_mysql_query(mysql, cmd))
    {
      g_free(cmd);

      g_message("save user: error deleting old user privileges");
      goto error;
    }
    g_free(cmd);

    cmd= replace_escaped(mysql, "DELETE FROM mysql.tables_priv WHERE User=@new_user",
                         "@new_user", new_user);
    if (myx_mysql_query(mysql, cmd))
    {
      g_message("save user: error deleting old table privileges");
      g_free(cmd);
      goto error;
    }
    g_free(cmd);

    cmd= replace_escaped(mysql, "DELETE FROM mysql.columns_priv WHERE User=@new_user",
                         "@new_user", new_user); 
    if (myx_mysql_query(mysql, cmd))
    {
      g_message("save user: error deleting old column privileges");
      g_free(cmd);
      goto error;
    }
    g_free(cmd);

    if(mysql_full_version_is_later_or_equal_than(mysql,5,0,3))
    {
      cmd= replace_escaped(mysql, "DELETE FROM mysql.procs_priv WHERE User=@new_user",
                          "@new_user", new_user); 
      if (myx_mysql_query(mysql, cmd))
      {
        g_message("save user: error deleting old SP privileges");
        g_free(cmd);
        goto error;
      }
      g_free(cmd);
    }

    // TODO: make string allocation dynamic
    long_sqlcmd= g_malloc0(sizeof(char) * 32768);

    for(i= 0; i<user->user_object_privileges_num; i++)
    {
      MYX_USER_OBJECT_PRIVILEGES *privs= user->user_object_privileges+i;
      unsigned long host_len= (unsigned long)strlen(privs->host);

      dot_count= !privs->object_name ? -1
                                     : sub_str_count(".", privs->object_name);

      if (dot_count < 0 || !privs->object_name[0]) // Global privileges
      {
        sqlcmd_end= strmov(long_sqlcmd, "UPDATE mysql.user SET ");
        for(j= 0; j<privs->user_privileges_num; j++)
        {
          char * name= name_of_str(field_buf, privs->user_privileges[j]);
          char * value= value_of_str(value_buf, privs->user_privileges[j]);
          sqlcmd_end= strmov(sqlcmd_end, name);
          sqlcmd_end= strmov(sqlcmd_end, "='");
          sqlcmd_end= strmov(sqlcmd_end, value);
          sqlcmd_end= strmov(sqlcmd_end,
                             j<privs->user_privileges_num-1 ? "', " : "' ");
        }
        sqlcmd_end= strmov(sqlcmd_end, "WHERE User='");
        sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end, 
                                              new_user, (gulong)strlen(new_user));
        sqlcmd_end= strmov(sqlcmd_end, "' AND Host='");
        sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end, 
                                              privs->host,host_len);
        sqlcmd_end= strmov(sqlcmd_end, "'");

        if (myx_mysql_real_query(mysql, long_sqlcmd,
                                 (unsigned int)(sqlcmd_end-long_sqlcmd)))
        {
          g_message("save user: error updating user information");
          g_free(long_sqlcmd);
          goto error;
        }
      }
      else if (dot_count==0) // Schema Privileges
      {
        unsigned long obj_len= (unsigned long)strlen(privs->object_name);
        //Insert new settings
        sqlcmd_end= strmov(long_sqlcmd,
                           "INSERT INTO mysql.db(User, Host, Db, ");
        for(j= 0; j<privs->user_privileges_num; j++)
        {
          sqlcmd_end= strmov(sqlcmd_end,
                             name_of_str(field_buf,privs->user_privileges[j]));
          if (j<privs->user_privileges_num-1)
            sqlcmd_end= strmov(sqlcmd_end, ", ");
        }
        sqlcmd_end= strmov(sqlcmd_end, ") VALUES('");
        sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end,
                                              new_user, (gulong)strlen(new_user));
        sqlcmd_end= strmov(sqlcmd_end, "', '");
        sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end,
                                              privs->host, host_len);
        sqlcmd_end= strmov(sqlcmd_end, "', '");
        sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end,
                                              privs->object_name, obj_len);
        sqlcmd_end= strmov(sqlcmd_end, "', '");

        for(j= 0; j<privs->user_privileges_num; j++)
        {
          sqlcmd_end= strmov(sqlcmd_end,
                             value_of_str(value_buf,
                                          privs->user_privileges[j]));
          sqlcmd_end= strmov(sqlcmd_end, j<privs->user_privileges_num-1 
                                          ? "', '" : "')");
        }

        if (myx_mysql_real_query(mysql, long_sqlcmd,
                                 (unsigned int)(sqlcmd_end-long_sqlcmd)))
        {
          g_message("save user: error adding schema privileges");
          g_free(long_sqlcmd);
          goto error;
        }
      }
      else // Schema Table privileges
      {
        unsigned long schema_len, table_len, column_len;
        s= g_malloc0(sizeof(char)*(64+64+64+2+(gulong)strlen(privs->object_name)));
        s= strcpy(s, privs->object_name);
        schema_name= strtok(s, ".");
        schema_len= (unsigned long)strlen(schema_name);
        table_name= strtok(NULL, ".");
        table_len= (unsigned long)strlen(table_name);

        if (dot_count==1)
        {
          if(myx_is_table(mysql, schema_name, table_name)) {
            is_tbl= 1;
            is_col= is_proc= 0;
            sqlcmd_end= strmov(long_sqlcmd,
                              "INSERT INTO mysql.tables_priv"
                              "(Host, Db, User, Table_name, Timestamp, Grantor,"
                                "Table_priv, Column_priv) VALUES('");
          }
          else
          {
            is_col= is_tbl= 0;
            is_proc= myx_is_procedure(mysql, schema_name, table_name);
            sqlcmd_end= strmov(long_sqlcmd,
                              "INSERT INTO mysql.procs_priv"
                              "(Host, Db, User, Routine_name, Routine_type, Grantor, Proc_priv, Timestamp) "
                              "VALUES('");
          }
        }
        else
        {
          is_col= 1;
          is_proc= is_tbl= 0;
          column_name= strtok(NULL, ".");
          column_len= (unsigned long)strlen(column_name);
          sqlcmd_end= strmov(long_sqlcmd,
                             "INSERT INTO mysql.columns_priv"
                             "(Host,Db,User,Table_name,Timestamp,"
                             "Column_name,Column_priv) VALUES('");
        }

        sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end, privs->host,
                                              host_len);
        sqlcmd_end= strmov(sqlcmd_end, "', '");
        sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end, schema_name,
                                              schema_len);
        sqlcmd_end= strmov(sqlcmd_end, "', '");
        sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end, new_user,
                                              (gulong)strlen(new_user));
        sqlcmd_end= strmov(sqlcmd_end, "', '");
        sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end, table_name,
                                              table_len);
        
        if(is_tbl || is_col)
        {
          sqlcmd_end= strmov(sqlcmd_end, "', now(), '");
        }
        else if(is_proc)
        {
          sqlcmd_end= strmov(sqlcmd_end, "', 'PROCEDURE', '");
        }
        else
        {
          sqlcmd_end= strmov(sqlcmd_end, "', 'FUNCTION', '");
        }

        if (dot_count!=1)
        {
          sqlcmd_end+= mysql_real_escape_string(mysql, sqlcmd_end, column_name,
                                                column_len);
        }
        else
        {
          // No grantor at this time
          begin= sqlcmd_end= strmov(sqlcmd_end, "', '");

          if(is_tbl)
          {
            // Get selected Table_privs
            for(j= 0; j<privs->user_privileges_num; j++)
            {
              if ( strstr(privs->user_privileges[j], "Table_priv") &&
                  !strcmp(value_of_str(value_buf, privs->user_privileges[j]),"Y"))
              {
                sqlcmd_end= strmov(sqlcmd_end,
                                  strstr(name_of_str(field_buf,
                                                      privs->user_privileges[j]),
                                          "_priv_")+6);
                sqlcmd_end= strmov(sqlcmd_end, ",");
              }
            }
          }
          else
          {
            // Get selected Proc_privs
            for(j= 0; j<privs->user_privileges_num; j++)
            {
              if ( strstr(privs->user_privileges[j], "Proc_priv") &&
                  !strcmp(value_of_str(value_buf, privs->user_privileges[j]),"Y"))
              {
                sqlcmd_end= strmov(sqlcmd_end,
                                  strstr(name_of_str(field_buf,
                                                      privs->user_privileges[j]),
                                          "_priv_")+6);
                sqlcmd_end= strmov(sqlcmd_end, ",");
              }
            }
          }
          if (sqlcmd_end!=begin) // Remove last ,
            sqlcmd_end--;
        }

        // Get selected Column_privs
        if(is_tbl || is_col)
        {
          begin= sqlcmd_end= strmov(sqlcmd_end, "', '");
          for(j=0;j<privs->user_privileges_num;j++)
          {
            if( strstr(privs->user_privileges[j], "Column_priv")&&
              !strcmp(value_of_str(value_buf, privs->user_privileges[j]),"Y"))
            {
              sqlcmd_end= strmov(sqlcmd_end,
                                strstr(name_of_str(field_buf,
                                                    privs->user_privileges[j]),
                                        "_priv_")+6);
              sqlcmd_end= strmov(sqlcmd_end, ",");
            }
          }
          if(sqlcmd_end!=begin) // Remove last ,
            sqlcmd_end--;
        }

        if(is_tbl || is_col)
        {
          sqlcmd_end= strmov(sqlcmd_end, "')");
        }
        else
        {
          sqlcmd_end= strmov(sqlcmd_end, "', now())");
        }

        g_free(s);

        if (myx_mysql_real_query(mysql, long_sqlcmd,
                                 (unsigned int)(sqlcmd_end-long_sqlcmd)))
        {
          g_free(long_sqlcmd);
          goto error;
        }
      }
    }

    g_free(long_sqlcmd);
  }

  //Flush Privileges
  if (myx_mysql_query(mysql, "FLUSH PRIVILEGES"))
    goto error;

  if (utf8_bin)
  {
    myx_mysql_query(mysql, "SET NAMES utf8");
    myx_mysql_query(mysql, "SET collation_connection = utf8_general_ci");
  }
  return 0;

error:
  if (utf8_bin)
  {
    myx_mysql_query(mysql, "SET NAMES utf8");
    myx_mysql_query(mysql, "SET collation_connection = utf8_general_ci");
  }

  return -1;
}

MYX_USER_OBJECT_PRIVILEGES * myx_get_privilege_struct(MYSQL *mysql, const char *object_name, MYX_USER_OBJECT_PRIVILEGE_TYPE object_type)
                                                      
{
  MYSQL_RES* res;
  MYSQL_FIELD *fields;
  MYSQL_ROW row;
  unsigned int num_fields;

  MYX_USER_OBJECT_PRIVILEGES *user_priv=
                                 g_malloc0(sizeof(MYX_USER_OBJECT_PRIVILEGES));
  char * sqlcmd;
  unsigned char s[255];
  unsigned int j;
  char *token;
  char seps[]= "(',)";
  char **upriv_text;

  user_priv->host= NULL;
  user_priv->object_name= g_strdup(object_name);

  user_priv->user_privileges_num= 0;
  user_priv->user_privileges= NULL;

  switch (object_type)
  { 
    case MYX_UOP_GLOBAL:
      sqlcmd= "SELECT * FROM mysql.user WHERE 1=2"; // Global
      break;
    case MYX_UOP_SCHEMA:
      sqlcmd= "SELECT * FROM mysql.db   WHERE 1=2"; // db
      break;
    case MYX_UOP_TABLE:
      sqlcmd= "SHOW COLUMNS FROM mysql.tables_priv";
      break;
    case MYX_UOP_ROUTINE:
      sqlcmd= "SHOW COLUMNS FROM mysql.procs_priv";
      break;
    case MYX_UOP_COLUMN:
      sqlcmd= "SHOW COLUMNS FROM mysql.columns_priv"; // db.table.column
      break;
  }

  res= NULL;
  if (myx_mysql_query(mysql, sqlcmd) == 0)
    res= mysql_store_result(mysql);
  if (res == NULL)
  {
    g_free(user_priv);
    return NULL;
  }
  else
  {
    if (object_type == MYX_UOP_GLOBAL || object_type == MYX_UOP_SCHEMA)
    {
      // Depending on the version of the server there might be
      num_fields= mysql_num_fields(res); // different columns
      fields= mysql_fetch_fields(res);

      MYX_VECTOR_RESERVE(user_priv->user_privileges,num_fields-2);
      for(j= 0; j<num_fields; j++)
      {
        const char * max_text= "%s=0";
        const char * priv_text= "%s=N";
        char *field_name= myx_convert_dbstr_utf8(mysql, fields[j].name, fields[j].name_length);
        const char * text=
                !strcmp(str_right(s, field_name, 5), "_priv") ? priv_text :
                !strcmp(str_left (s, field_name, 4), "max_" ) ? max_text  : 0;
        if (text)
        {
          MYX_VECTOR_PUSH_BACK(user_priv->user_privileges, user_priv->user_privileges_num, upriv_text)
          *upriv_text= g_strdup_printf(text, field_name);
        }
        g_free(field_name);
      }
    }
    else if (object_type == MYX_UOP_TABLE || object_type == MYX_UOP_ROUTINE || object_type == MYX_UOP_COLUMN)
    {
      MYX_VECTOR_RESERVE(user_priv->user_privileges,mysql_num_rows(res)-4);
      do
      {
        row= mysql_fetch_row(res);
        if (row == NULL)
          break;

        if (strcmp(str_right(s, row[0], 5), "_priv")==0)
        {
          char *row0= myx_convert_dbstr_utf8(mysql, row[0], -1);
          char *row1= myx_convert_dbstr_utf8(mysql, row[1], -1);
          strcpy(s, row1+3);
          for (token= strtok(s, seps); token; token= strtok(NULL, seps))
          {
            MYX_VECTOR_PUSH_BACK(user_priv->user_privileges,
                                 user_priv->user_privileges_num,upriv_text)
            *upriv_text= g_strdup_printf("%s_%s=N", row0, token);
          }
          g_free(row0);
          g_free(row1);
        }
      }
      while (1);
    }
    mysql_free_result(res);
  }

  return user_priv;
}

const char * user_tables_names[]=
{
  "columns_priv",
  "tables_priv",
  "db",
  USER_INFO_TABLENAME,
  "user"
};
const char ** user_tables_names_end=
                   user_tables_names + sizeof(user_tables_names)/sizeof(char*);

int myx_del_user(MYSQL *mysql, const char *user_name)
{
  unsigned char sqlcmd[500], *sqlcmd_end;
  unsigned char esc_user_name[17];
  const char ** user_table;

  mysql_real_escape_string(mysql, esc_user_name, user_name,
                           (unsigned long)strlen(user_name));

  for (user_table= user_tables_names;
       user_table!=user_tables_names_end; user_table++)
  {
    sqlcmd_end= strmov(sqlcmd,      "DELETE FROM mysql.");
    sqlcmd_end= strmov(sqlcmd_end,  *user_table);
    sqlcmd_end= strmov(sqlcmd_end,  " WHERE User='");
    sqlcmd_end= strmov(sqlcmd_end,  esc_user_name);
    sqlcmd_end= strmov(sqlcmd_end,  "'");

    if (myx_mysql_real_query(mysql, sqlcmd, (unsigned int)(sqlcmd_end-sqlcmd)))
      return -1;
  }

  //Flush Privileges
  if (myx_mysql_query(mysql, "FLUSH PRIVILEGES"))
    return -1;

  return 0;
}

int myx_is_table(MYSQL *mysql, const char *schema, const char *table)
{
  MYSQL_RES *res;

  char *q= g_strconcat("SHOW TABLE STATUS FROM ", schema, " LIKE '", table, "'", NULL);
  myx_mysql_query(mysql, q);
  g_free(q);
  res= mysql_store_result(mysql);
  if(res)
  {
    int ret= (mysql_num_rows(res) > 0);
    mysql_free_result(res);
    return ret;
  }
  return 0;
}

int myx_is_procedure(MYSQL *mysql, const char *schema, const char *proc)
{
  MYSQL_RES *res;

  char *q= g_strconcat("SHOW PROCEDURE STATUS LIKE '", proc, "'", NULL);
  myx_mysql_query(mysql, q);
  g_free(q);
  res= mysql_store_result(mysql);
  if(res)
  {
    int ret= (mysql_num_rows(res) > 0);
    mysql_free_result(res);
    return ret;
  }
  return 0;
}

void myx_compact_privs(MYSQL *mysql)
{
  char *q = "DELETE FROM mysql.db WHERE Select_priv='N' AND Insert_priv='N' AND Update_priv='N'"
    " AND Delete_priv='N' AND Create_priv='N' AND Drop_priv='N' AND Grant_priv='N'"
    " AND References_priv='N' AND Index_priv='N' AND Alter_priv='N' AND Create_tmp_table_priv='N'"
    " AND Lock_tables_priv='N' AND Create_view_priv='N' AND Show_view_priv='N' AND Create_routine_priv='N'"
    " AND Alter_routine_priv='N' AND Execute_priv='N'";
  myx_mysql_query(mysql, q);

  q = "FLUSH PRIVILEGES";
  myx_mysql_query(mysql, q);
}
