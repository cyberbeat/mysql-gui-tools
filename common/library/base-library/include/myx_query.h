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

#ifndef myx_query_h
#define myx_query_h

#ifdef __cplusplus

#include <string>
#include <vector>
#include "myx_const_string.h"

struct Query : public MYX_QUERY
{
  Query();
  ~Query();

  std::string m_sql, m_sql_without_comments, m_masked_sql;

  typedef std::vector<const char *> Type_options;
  typedef std::vector<const_string> Type_const_string_options;
  typedef std::vector<MYX_Q_CLAUSE> Type_clauses;
  typedef std::vector<MYX_Q_TABLE>  Type_tables;
  typedef std::vector<MYX_Q_COLUMN>  Type_columns;

  Type_options              m_options;
  Type_const_string_options m_const_string_options;
  Type_clauses              m_clauses;
  Type_tables               m_tables;
  Type_columns              m_columns;

  void prepare_for_delphi();

  const MYX_Q_CLAUSE * get_clause          (MYX_Q_CLAUSE_TYPE c_type)const;
  const MYX_Q_CLAUSE * get_previous_clause (MYX_Q_CLAUSE_TYPE c_type)const;

  MYX_Q_TABLE   * get_table_by_alias(const char * alias,
                                     const char * column= 0);
  MYX_Q_TABLE   * get_first_table();
  MYX_Q_TABLE   * get_table(const char * catalog,
                            const char * schema,
                            const char * table);
  MYX_Q_COLUMN  * get_column(const char * column);

  bool contains_tables_without_aliases()const;
  bool try_to_fix_table_aliases(MYSQL * mysql);
	bool contains_unaliased_table(const char *table_name);

  bool can_contain_clause(MYX_Q_CLAUSE_TYPE clause_type);

  void analyze(MYSQL *mysql, const char *sql);

  MYX_Q_TYPE calc_query_type();
};

// Following functions are listed here for unit-tests.
bool query_is_join(Query *query);

#endif // __cplusplus

//----------------------------------------------------------------------------------------------------------------------

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <winsock2.h>
#endif

#define MAX_COMMAND_LINE_LENGTH 80

MYX_PUBLIC_FUNC MYX_QUERY * query_analyze(MYSQL *mysql, const char *sqlcmd);
MYX_PUBLIC_FUNC void query_free(MYX_QUERY *query);

void query_build_clauses(MYX_QUERY *query);
MYX_QUERY * query_get_query_tables(MYSQL *mysql, MYX_QUERY *query);
MYX_QUERY * query_get_query_tables_columns(MYSQL *mysql, MYX_QUERY *query);
MYX_QUERY * query_get_query_columns(MYX_QUERY *query);
MYX_QUERY * query_set_params(MYX_QUERY *query, MYX_STRINGLIST *parameters);

MYX_PUBLIC_FUNC unsigned int query_is_editable(MYX_QUERY *query, int enforce_editable);

MYX_Q_COLUMN * query_add_column(MYX_QUERY *query, MYX_Q_TABLE *q_table, const char *column, char *sql, MYX_Q_CLAUSE_TYPE clause_type);

MYX_STRINGLIST * myx_check_table_relationship(MYSQL *mysql, const char *schema,
                                              const char *table1,
                                              const char *alias1,
                                              const char *table2,
                                              const char *alias2);

char * myx_query_add_table_aliases(MYSQL *mysql, const char *source_sql, const char *new_tablename);
char * myx_query_get_table_alias(MYX_QUERY *query, const char *new_tablename);
char * myx_query_build_from_clauses(MYX_QUERY *query);

#ifdef __cplusplus
}
#endif /* __cplusplus */

//----------------------------------------------------------------------------------------------------------------------

#endif
