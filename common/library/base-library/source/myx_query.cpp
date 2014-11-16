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

#include "myx_public_interface.h"
#include "myx_library.h"
#include "myx_util_functions.h"

#include "myx_query.h"
#include "myx_const_string.h"
#include "myx_query_reader.h"
extern "C"{
#include "myx_simple_sql_parsing.h"
}

#include <string>
#include <vector>

#include "keywords.h"

using namespace std;

#define SYSTEM_SCHEME "mysql"
#define SYSTEM_CATALOG "system"
#define DEFAULT_CATALOG "def"

static bool is_sql_keyword(const char *str);
static bool contains(const const_string & str, const const_string & infix);

const char * default_catalog(const char * schema)
{
  return (!schema || strcmp(schema,SYSTEM_SCHEME))
            ? DEFAULT_CATALOG : SYSTEM_CATALOG;
}

struct Clause_description
{
  const_string      start_keyword;
  MYX_Q_CLAUSE_TYPE type;
  Clause_description(const const_string & k, MYX_Q_CLAUSE_TYPE t)
    : start_keyword(k), type(t)
  {}
};

static Clause_description select_clauses[]=
{
  Clause_description( CONST_STR("SELECT"),    MYX_QCT_SELECT_CLAUSE ),
  Clause_description( CONST_STR("FROM"),      MYX_QCT_FROM_CLAUSE   ),
  Clause_description( CONST_STR("WHERE"),     MYX_QCT_WHERE_CLAUSE  ),
  Clause_description( CONST_STR("GROUP BY"),  MYX_QCT_GROUP_CLAUSE  ),
  Clause_description( CONST_STR("HAVING"),    MYX_QCT_HAVING_CLAUSE ),
  Clause_description( CONST_STR("ORDER BY"),  MYX_QCT_ORDER_CLAUSE  ),
  Clause_description( CONST_STR("LIMIT"),     MYX_QCT_LIMIT_CLAUSE  )
};

static Clause_description update_clauses[]=
{
  Clause_description( CONST_STR("UPDATE"),    MYX_QCT_UPDATE_CLAUSE ),
  Clause_description( CONST_STR("SET"),       MYX_QCT_SET_CLAUSE    ),
  Clause_description( CONST_STR("WHERE"),     MYX_QCT_WHERE_CLAUSE  ),
  Clause_description( CONST_STR("ORDER BY"),  MYX_QCT_ORDER_CLAUSE  ),
  Clause_description( CONST_STR("LIMIT"),     MYX_QCT_LIMIT_CLAUSE  )
};

static Clause_description delete_clauses[]=
{
  Clause_description( CONST_STR("DELETE"),    MYX_QCT_DELETE_CLAUSE ),
  Clause_description( CONST_STR("FROM"),      MYX_QCT_FROM_CLAUSE   ),
  Clause_description( CONST_STR("USING "),    MYX_QCT_USING_CLAUSE  ),
  Clause_description( CONST_STR("WHERE"),     MYX_QCT_WHERE_CLAUSE  ),
  Clause_description( CONST_STR("ORDER BY"),  MYX_QCT_ORDER_CLAUSE  ),
  Clause_description( CONST_STR("LIMIT"),     MYX_QCT_LIMIT_CLAUSE  )
};

static const_string select_option_keywords[]=
{
  CONST_STR("ALL"),
  CONST_STR("DISTINCT"),
  CONST_STR("DISTINCTROW"),
  CONST_STR("HIGH_PRIORITY"),
  CONST_STR("SQL_SMALL_RESULT"),
  CONST_STR("SQL_BIG_RESULT"),
  CONST_STR("SQL_BUFFER_RESULT"),
  CONST_STR("SQL_CACHE"),
  CONST_STR("SQL_NO_CACHE"),
  CONST_STR("SQL_CALC_FOUND_ROWS")
};

static const_string update_option_keywords[]=
{
  CONST_STR("LOW_PRIORITY"),
  CONST_STR("IGNORE")
};

static const_string delete_option_keywords[]=
{
  CONST_STR("LOW_PRIORITY"),
  CONST_STR("QUICK"),
  CONST_STR("IGNORE")
};

struct Query_type_description
{
  MYX_Q_TYPE type;
  Clause_description * clauses;
  Clause_description * clauses_end;
  const_string * option_keywords;
  const_string * option_keywords_end;
};

#define array_size(name) ((sizeof(name))/sizeof(*name))
#define array_end(name) ((name) + array_size(name))

Query_type_description select_query_description= 
{
  MYX_QT_SELECT,
  select_clauses, array_end(select_clauses),
  select_option_keywords, array_end(select_option_keywords),
};

Query_type_description update_query_description=
{
  MYX_QT_UPDATE,
  update_clauses, array_end(update_clauses),
  update_option_keywords, array_end(update_option_keywords),
};

Query_type_description delete_query_description=
{
  MYX_QT_DELETE,
  delete_clauses, array_end(delete_clauses),
  delete_option_keywords, array_end(delete_option_keywords),
};

//----------------------------------------------------------------------------------------------------------------------

int myx_get_unix_newline_count(const char *s)
{
  int count= 0;
  for(const char *c= s; *c; c++)
  {
    if((*c == '\n') && ((c == s) || (c[-1] != '\r')))
    {
      ++count;
    }
  }
  return count;
}

//----------------------------------------------------------------------------------------------------------------------

Query_type_description * get_query_type_description(MYX_Q_TYPE query_type)
{
  return query_type==MYX_QT_SELECT ? &select_query_description :
         query_type==MYX_QT_UPDATE ? &update_query_description : 
         query_type==MYX_QT_DELETE ? &delete_query_description : 0;
}

#define ISSPACE(c) ((c)==' ' || (c)=='\t' || (c)=='\r' || (c)=='\n')

//----------------------------------------------------------------------------------------------------------------------

const char * myx_cs_identifier_needs_quotes(const const_string & name)
{
  if(is_sql_keyword(name.begin()))
  {
    return name.begin();
  }

  for (const char * pos= name.begin(); pos!=name.end(); pos++)
  {
    if (!(*pos>='a' && *pos<='z' ||
          *pos>='A' && *pos<='Z' ||
          *pos>='0' && *pos<='9' ||
          *pos=='_' ||
          *pos=='.'))
    {
      return pos;
    }
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

int myx_identifier_needs_quotes(const char *name)
{
  const_string n(name,strlen(name));
  const char * pos= myx_cs_identifier_needs_quotes(n);
  return (int)(!pos ? 0 : 1);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Executes the given query (possibly with multiple statements) without returning a result set. Any returned result is
 * simply skipped. This special behavior is used in scripts which are not supposed to return or use result sets.
 *
 * @param mysql The mysql server connection.
 * @param sql The SQL to be executed.
 * @param error_code [out] Used to return an error code to the caller.
 * @param affected_rows [out] Contains the number of affected rows for the first result set (others are not considered if there are more than one).
 *
 * @note Covered by unit tests.
 */
void myx_query_execute_direct(MYSQL* mysql, const char* sql, MYX_LIB_ERROR* error_code, bigint* affected_rows)
{
  MYSQL_RES *res;

  *error_code= MYX_NO_ERROR;

  if (affected_rows != NULL)
    *affected_rows= -2;

  if (myx_mysql_query(mysql, sql))
  {
    *error_code= MYX_SQL_ERROR;
  }
  else
  {
    do
    {
      // Process all results.
      res= mysql_use_result(mysql);
      if (res != NULL)
      {
        // If there is a resultset, fetch it.
        while ((mysql_fetch_row(res)))
          ;

        mysql_free_result(res);
      };
      if ((affected_rows != NULL) && (*affected_rows == -2))
        *affected_rows= myx_mysql_affected_rows(mysql);
    }
    while (!mysql_next_result(mysql));
  };
}

//----------------------------------------------------------------------------------------------------------------------

struct G_string
{
  char * str;
  G_string()
  {
    str= 0;
  }
  G_string(char * str)
  {
    this->str= str;
  }
  G_string(const char * str)
  {
    this->str= (char*)str;
  }
  ~G_string()
  {
    g_free((gpointer)str);
    str= 0;
  }
  G_string & operator = (char * str)
  {
    g_free((gpointer)this->str);
    this->str= str;
    return *this;
  }
  operator const char * () const
  {
    return str;
  }
  operator char * ()
  {
    return str;
  }
  char operator [] (const int index) const
  {
    return str[index];
  }
  char& operator [] (const int index)
  {
    return str[index];
  }
};

//----------------------------------------------------------------------------------------------------------------------

struct G_splitted_strings
{
  char ** strings;
  G_splitted_strings()
  {
    strings= 0;
  }
  G_splitted_strings(char ** strings)
  {
    this->strings= strings;
  }
  ~G_splitted_strings()
  {
    if (strings)
    {
      g_strfreev(strings);
      strings= 0;
    }
  }
  G_splitted_strings & operator = (char ** str)
  {
    if (strings)
      g_strfreev(strings);
    this->strings= strings;
    return *this;
  }
  operator char ** ()
  {
    return strings;
  }
};

//----------------------------------------------------------------------------------------------------------------------

struct PCRE_sub_string
{
  const char * str;
  PCRE_sub_string()
  {
    str= 0;
  }
  PCRE_sub_string(const char * str)
  {
    this->str= str;
  }
  PCRE_sub_string(const char * a, int * b, int c, int d)
  {
    pcre_get_substring(a,b,c,d,&str);
  }
  ~PCRE_sub_string()
  {
    if (str)
    {
      pcre_free_substring(str);
      str= 0;
    }
  }
  PCRE_sub_string & operator = (const char * str)
  {
    pcre_free_substring(this->str);
    this->str= str;
    return *this;
  }
  operator const char * () const
  {
    return str;
  }
};

//----------------------------------------------------------------------------------------------------------------------

std::string & operator << (std::string & dst, const const_string & src)
{
  dst.append(src.data(),src.length());
  return dst;
}

//----------------------------------------------------------------------------------------------------------------------

std::string & operator << (std::string & dst, const std::string & src)
{
  dst.append(src.data(),src.length());
  return dst;
}

//----------------------------------------------------------------------------------------------------------------------

struct const_name : public const_string
{
  char quote_symbol;

  const_name(MYSQL * mysql, const char * name)
    : const_string(name,strlen(name)) 
  {quote_symbol= myx_get_mysql_quote_char(mysql,0);}

  const_name(MYSQL * mysql, const const_string & str)
    : const_string(str)
  {quote_symbol= myx_get_mysql_quote_char(mysql,0);}

  friend std::string & operator << (std::string & dst, const const_name & src)
  {
    // always add quotes because otherwise we'd have to check for reserved
    // words (like table, from, to, select etc)
    // it's easier to just quote everything.
    if (!myx_identifier_needs_quotes(src.data()))
    {
      dst << (const_string&)src;
    }
    else
    {
      dst.append(1,src.quote_symbol);
      for (const char * c= src.begin(); c!=src.end(); c++)
      {
        dst.append(1,*c);
        if (*c==src.quote_symbol || *c=='\\')
          dst.append(1,*c);
      }
      dst.append(1,src.quote_symbol);      
    }
    return dst;
  }
};

//----------------------------------------------------------------------------------------------------------------------

const_string get_left(std::string & str, size_t length)
{
  return const_string(str.c_str(),length);
}

//----------------------------------------------------------------------------------------------------------------------

const_string get_left(const const_string & str, size_t length)
{
  return const_string(str.data(),length);
}

//----------------------------------------------------------------------------------------------------------------------

const_string get_left(const char * str, size_t length)
{
  return const_string(str,length);
}

//----------------------------------------------------------------------------------------------------------------------

const_string get_right(std::string & str, size_t pos)
{
  return const_string(str.c_str()+pos,str.length()-pos);
}

//----------------------------------------------------------------------------------------------------------------------

const_string get_right(const const_string & str, size_t pos)
{
  return const_string(str.data()+pos,str.length()-pos);
}

//----------------------------------------------------------------------------------------------------------------------

MYX_Q_TYPE myx_query_type(const const_string & sql);
char * myx_query_get_table_alias(Query *query, const char * new_tablename);
void s_query_build_clauses(Query *query);

Query        * s_query_get_query_tables        (MYSQL *mysql, Query *query);
Query        * s_query_get_query_tables        (MYSQL *mysql, Query *query,
                                                MYX_Q_CLAUSE_TYPE c_type);
Query        * s_query_get_query_tables_columns(MYSQL * mysql, Query * query);
Query        * s_query_get_query_columns       (Query *query);
MYX_Q_COLUMN * s_query_add_column              (Query *query,
                                                MYX_Q_TABLE *q_table,
                                                const char *column, char *sql,
                                                MYX_Q_CLAUSE_TYPE clause_type);
Query        * s_query_analyze                 (MYSQL *mysql, const char *sql);

//----------------- Query ----------------------------------------------------------------------------------------------

Query::Query()
{
  sql= original_sql= 0;
  query_type= MYX_QT_UNKNOWN;

  options_num= 0;
  options= 0;

  tables_num= 0;
  tables= 0;

  pk_columns_added_num= 0;

  columns_num= 0;
  columns= 0;

  clauses_num= 0;
  clauses= 0;

  subquerys_num= 0;
  subquerys= 0;

  params_num= 0;
}

//----------------------------------------------------------------------------------------------------------------------

void Query::prepare_for_delphi()
{
  sql= (char*)m_sql.c_str();

  options_num= (unsigned int)m_options.size();
  options= (char**)&m_options.front();

  clauses_num= (unsigned int)m_clauses.size();
  clauses= &m_clauses.front();

  tables_num= (unsigned int)m_tables.size();
  tables= &m_tables.front();

  columns_num= (unsigned int)m_columns.size();
  columns= &m_columns.front();
}

//----------------------------------------------------------------------------------------------------------------------

const MYX_Q_CLAUSE * Query::get_clause(MYX_Q_CLAUSE_TYPE clause_type) const
{
  for (Type_clauses::const_iterator iter= m_clauses.begin();
       iter!=m_clauses.end(); iter++)
  {
    if (iter->clause_type == clause_type)
      return &*iter;
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

const MYX_Q_CLAUSE * Query::get_previous_clause(MYX_Q_CLAUSE_TYPE clause_type) const
{
  const Query_type_description * query_descr= get_query_type_description(query_type);
  bool f= false;
  Clause_description * clause;
  for (clause= query_descr->clauses_end-1;
       clause!=query_descr->clauses-1; clause--)
  {
    if (f)
    {
      const MYX_Q_CLAUSE * res= get_clause(clause->type);
      if (res)
        return res;
    }
    else if (clause->type==clause_type)
    {
      f= true;
    }
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

bool Query::can_contain_clause(MYX_Q_CLAUSE_TYPE clause_type)
{
  Query_type_description * qdescription= get_query_type_description(query_type);
  if (qdescription)
  {
    for (Clause_description * cdescription= qdescription->clauses;
         cdescription!=qdescription->clauses_end; cdescription++)
    {
      if (cdescription->type==clause_type)
        return true;
    }
  }
  return false;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_Q_TABLE * Query::get_table_by_alias(const char * alias,
                                        const char * column/*= 0*/)
{
  for (Type_tables::iterator it= m_tables.begin(); it!=m_tables.end(); it++)
  {
    if (!g_utf8_casecollate(alias, it->alias))
    {
      if (!column)
      {
        return &*it;
      }
      else
      {
        for (size_t k= 0; k<it->columns_num; k++)
        {
          MYX_Q_TABLE_COLUMN * q_tbl_col= it->columns+k;
          if (!g_utf8_casecollate(column, q_tbl_col->column) ||
              !strcmp(column, "*"))
          {
            return &*it;
          }
        }
      }
    }
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_Q_COLUMN * Query::get_column(const char * column)
{
  for (Type_columns::iterator it= m_columns.begin(); it!=m_columns.end(); it++)
  {
    if (!g_utf8_casecollate(column, it->column))
      return &*it;
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

void replace(std::string * res,
             const const_string & old_substr, const const_string & new_substr)
{
  size_t pos, offset= 0;
  while ((pos= res->find(old_substr.data(),offset,old_substr.length()))!=(size_t)-1)
  {
    res->replace(pos,old_substr.length(),
                 new_substr.data(),new_substr.length());
    offset= pos+new_substr.length();
  }
}

//----------------------------------------------------------------------------------------------------------------------

bool Query::contains_tables_without_aliases() const
{
  for (Type_tables::const_iterator it= m_tables.begin();
       it!=m_tables.end(); it++)
  {
    if (!it->alias || !it->alias[0])
    {
      return true;
    }
  }
  return false;
}

//----------------------------------------------------------------------------------------------------------------------

bool Query::contains_unaliased_table(const char *table_name)
{
  for (Type_tables::iterator it= m_tables.begin(); it!=m_tables.end(); it++)
  {
    if((!strcmp3(it->name, table_name)) && ((!it->alias) || (strlen(it->alias) == 0)))
    {
      return true;
    }
  }
  return false;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_Q_TABLE *Query::get_first_table()
{
  if (m_tables.empty())
    return 0;
  else
    return &*m_tables.begin();
}

//----------------------------------------------------------------------------------------------------------------------

MYX_Q_TABLE *Query::get_table(const char * catalog, const char * schema, const char * table)
{
  if (!catalog || !*catalog)
    catalog= default_catalog(schema);
  for (Type_tables::iterator it= m_tables.begin(); it!=m_tables.end(); it++)
  {
    if (! strcmp3(it->catalog, catalog) &&
        ((!schema) || (! strcmp3(it->schema, schema))) &&
        ! strcmp3(it->name, table))
    {
      return &*it;
    }
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

void Query::analyze(MYSQL *mysql, const char *sql)
{
  m_sql_without_comments= sql;
  m_sql= sql;
  kill_comments((char*)m_sql_without_comments.c_str());
  m_masked_sql= m_sql_without_comments.c_str();
  mask_quotas_and_brackets((char*)m_masked_sql.c_str());
  myx_get_mysql_quote_char(mysql,1);
  query_type= calc_query_type();

  s_query_build_clauses(this);
  switch (query_type)
  {
  case MYX_QT_SELECT:
    s_query_get_query_tables          ( mysql,  this  );
    s_query_get_query_tables_columns  ( mysql,  this  );
    s_query_get_query_columns         (         this  );
    break;
  case MYX_QT_UPDATE:
    s_query_get_query_tables          ( mysql,  this, MYX_QCT_UPDATE_CLAUSE  );
    s_query_get_query_tables_columns  ( mysql,  this  );
    s_query_get_query_columns         (         this  );
    break;
  case MYX_QT_DELETE:
    s_query_get_query_tables          ( mysql,  this, MYX_QCT_FROM_CLAUSE  );
    s_query_get_query_tables          ( mysql,  this, MYX_QCT_USING_CLAUSE );
    s_query_get_query_tables_columns  ( mysql,  this  );
    s_query_get_query_columns         (         this  );
    break;
  default:
    // silcence warnings of unhandled values
    break;
  }
}

//----------------------------------------------------------------------------------------------------------------------

MYX_QUERY * query_analyze(MYSQL *mysql, const char *sql)
{
  return s_query_analyze(mysql,sql);
}

//----------------------------------------------------------------------------------------------------------------------

Query * s_query_analyze(MYSQL *mysql, const char *sql)
{
  Query * query= new Query();
  query->analyze(mysql,sql);
  query->prepare_for_delphi();
  return query;
}

//----------------------------------------------------------------------------------------------------------------------

Query::~Query()
{
  unsigned int i, j;
  MYX_Q_TABLE *q_table;
  MYX_Q_TABLE_COLUMN *tbl_col;
  MYX_Q_COLUMN *col;
  MYX_Q_CLAUSE *clause;

  //Free MYX_Q_TABLE tables
  for (i= 0; i<tables_num; i++)
  {
    q_table= tables+i;
    
    g_free(q_table->catalog);
    g_free(q_table->schema);
    g_free(q_table->name);
    g_free(q_table->alias);
    g_free(q_table->fullname);

    //Free MYX_Q_TABLE_COLUMN table columns
    for(j=0;j<q_table->columns_num;j++)
    {
      tbl_col= q_table->columns+j;
      g_free(tbl_col->column);
    }

    g_free(q_table->columns);
    g_free(q_table->pk_columns);
  }

  //Free MYX_Q_COLUMN Columns
  for (i= 0; i<columns_num; i++)
  {
    col= columns+i;
    g_free(col->column);
    g_free(col->column_alias);
  }

  //Free MYX_Q_CLAUSE clauses
  for(i=0;i<clauses_num;i++)
  {
    clause= clauses+i;
    g_free(clause->clause);
  }
}

//----------------------------------------------------------------------------------------------------------------------

void query_free(MYX_QUERY *query)
{
  if(query)
    delete (Query*) query;
}

//----------------------------------------------------------------------------------------------------------------------

const const_string * find_const_string_ci(const const_string * strings_begin,
                                          const const_string * strings_end,
                                          const const_string & str)
{
  for (const const_string * res= strings_begin; res!=strings_end; res++)
  {
    if (res->equal_case_insensitively_to(str))
    {
      return res;
    }
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_Q_TABLE_COLUMN * get_column(MYX_Q_TABLE *q_table, const char *column_name)
{
  if (!q_table)
    return 0;
  MYX_Q_TABLE_COLUMN *column= q_table->columns;
  MYX_Q_TABLE_COLUMN *columns_end= column + q_table->columns_num;
  for ( ; column!=columns_end; column++)
  {
    if (!strcmp(column->column,column_name))
    {
      return column;
    }
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

static const char *query_get_select_options
                        (Query * query, const char *sql,
                         const Query_type_description & query_type_description)
{
  const char * word_begin= sql;
  for (;;)
  {
    /* skip blanks */
    for ( ; ISSPACE(*word_begin); word_begin++);
    if (!*word_begin)
      break;

    const char * word_end= word_begin + 1;

    // Consider special case here: columns can be given as * followed by _no_ space char (or more).
    if ((*word_begin != '*') || ISSPACE(*word_end))
      for ( ; *word_end && !ISSPACE(*word_end); word_end++);

    const_string word(word_begin,word_end-word_begin);
    const const_string * keyword=
         find_const_string_ci(query_type_description.option_keywords,
                              query_type_description.option_keywords_end,word);
    if (!keyword)
    {
      break;
    }
    else
    {
      query->m_options.push_back(keyword->data());
      query->m_const_string_options.push_back(*keyword);
      word_begin+= word.length();
    }
  }
  return word_begin;
}

//----------------------------------------------------------------------------------------------------------------------

void swap(const char ** p1, const char ** p2)
{
  const char * tmp= *p1;
  *p1= *p2;
  *p2= tmp;
}

//----------------------------------------------------------------------------------------------------------------------

void normalize_pos(const char ** pos1, const char ** pos2)
{
  if (*pos1 > *pos2)
    swap(pos1,pos2);
}

//----------------------------------------------------------------------------------------------------------------------

void init_clause(MYX_Q_CLAUSE * q_clause, MYX_Q_CLAUSE_TYPE clause_type,
                 const char * sql, const const_string & masked_sql,
                 const const_string & clause)
{
  q_clause->clause_type= clause_type;
  q_clause->clause= g_strstrip(g_strndup(sql +
                                         (clause.begin()-masked_sql.begin()),
                                         (gsize)clause.length()));
  q_clause->start_index= (unsigned int)(clause.begin()-masked_sql.begin());
  q_clause->end_index= (unsigned int)(clause.end()-masked_sql.begin());
  char cend= masked_sql[q_clause->end_index-1];
  q_clause->end_with_linebreak= (cend=='\n' || cend=='\r') ? 1 : 0;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_Q_CLAUSE_TYPE get_select_clause_type(size_t clause_num)
{
  switch (clause_num)
  {
  case 0:   return MYX_QCT_SELECT_CLAUSE;
  case 1:   return MYX_QCT_FROM_CLAUSE;
  case 2:   return MYX_QCT_WHERE_CLAUSE;
  case 3:   return MYX_QCT_GROUP_CLAUSE;
  case 4:   return MYX_QCT_HAVING_CLAUSE;
  case 5:   return MYX_QCT_ORDER_CLAUSE;
  case 6:   return MYX_QCT_LIMIT_CLAUSE;
  default:  return MYX_QCT_NO_CLAUSE;
  }
}

//----------------------------------------------------------------------------------------------------------------------

void query_build_clause(Query * query,
                        const Query_type_description  & query_description,
                        const Clause_description      * cur_clause,
                        const const_string &  masked_sql)
{
  MYX_Q_CLAUSE_TYPE clause_type;
  const const_string & start_keyword= cur_clause->start_keyword;
  const char * begin= strfindword(masked_sql.data(),start_keyword.data());
  const char * sql= query->m_sql_without_comments.c_str();
  if (begin && (begin + start_keyword.length())<=masked_sql.end())
  {
    clause_type= cur_clause->type;
    if (cur_clause==query_description.clauses)
    {
      query_get_select_options(query, begin + start_keyword.length() + 1,
                               query_description);
    }

    begin+= start_keyword.length();
    if (*begin)
      begin++;
    const Clause_description * next_clause;
    for (next_clause= cur_clause+1;
         next_clause!=query_description.clauses_end; next_clause++)
    {
      const char * clause_i= strfindword(masked_sql.data(),
                                         next_clause->start_keyword.data());

      if(clause_i)
      {
        while ((unsigned char) clause_i[-1] <= ' ')
        {
          clause_i--;
        }
      }

      if (clause_i)
      {
        normalize_pos(&begin,&clause_i);
        query->m_clauses.push_back(MYX_Q_CLAUSE());
        init_clause(&query->m_clauses.back(),clause_type,sql,masked_sql,
                    const_string(begin,clause_i-begin));
        break;
      }
    }
    if (next_clause==query_description.clauses_end)
    {
      query->m_clauses.push_back(MYX_Q_CLAUSE());
      init_clause(&query->m_clauses.back(),clause_type,sql,masked_sql,
                  const_string(begin,masked_sql.end()-begin));
    }
  }
}

//----------------------------------------------------------------------------------------------------------------------

void query_build_clauses(Query *query)
{
  s_query_build_clauses((Query*)query);
}

//----------------------------------------------------------------------------------------------------------------------

void s_query_build_clauses(Query *query)
{
  Query_type_description * query_type_descr= 
                                 get_query_type_description(query->query_type);
  if (query_type_descr)
  {
    const_string smasked_sql(query->m_masked_sql.data(),
                             query->m_masked_sql.length());
    
    Clause_description * clause_descr;
    for (clause_descr= query_type_descr->clauses;
          clause_descr!=query_type_descr->clauses_end; clause_descr++)
    {
      query_build_clause(query,*query_type_descr,clause_descr,smasked_sql);
    }
  }
}

//----------------------------------------------------------------------------------------------------------------------

MYX_Q_TYPE myx_query_type(const char *sql)
{
  return myx_query_type(const_string(sql,strlen(sql)));
}

//----------------------------------------------------------------------------------------------------------------------

bool start_with(const const_string & str, const const_string & prefix)
{
  return str.length() < prefix.length() ? false :
         g_ascii_strncasecmp(str.begin(),prefix.begin(),
                             (gsize)prefix.length())==0;
}

//----------------------------------------------------------------------------------------------------------------------

//Be aware that sql needs to be trimmed
MYX_Q_TYPE myx_query_type(const const_string & sql)
{
  return  start_with(sql,CONST_STR("SELECT"))  ?
    (contains(sql,CONST_STR("INTO")) &&
    contains(sql,CONST_STR("OUTFILE"))) ? MYX_QT_SELECT_INTO_OUTFILE : MYX_QT_SELECT :
          start_with(sql,CONST_STR("UPDATE"))  ? MYX_QT_UPDATE   :
          start_with(sql,CONST_STR("INSERT"))  ? MYX_QT_INSERT   :
          start_with(sql,CONST_STR("DELETE"))  ? MYX_QT_DELETE   :
          start_with(sql,CONST_STR("SHOW"))    ? MYX_QT_SHOW     :
          start_with(sql,CONST_STR("SET"))     ? MYX_QT_SET      :
          start_with(sql,CONST_STR("DESC"))    ? MYX_QT_DESCRIBE :
          start_with(sql,CONST_STR("EXPLAIN")) ? MYX_QT_EXPLAIN  :
          start_with(sql,CONST_STR("CALL"))    ? MYX_QT_CALL     :
          sql==CONST_STR("")                   ? MYX_QT_EMPTY    :
                                                 MYX_QT_UNKNOWN;
}

//----------------------------------------------------------------------------------------------------------------------

bool contains(const const_string & str, const const_string & infix)
{
  if (str.length()>=infix.length())
  {
    for (const char * pos= str.begin(); pos!=str.end()-infix.length(); pos++)
    {
      if (const_string(pos,infix.length()).equal_case_insensitively_to(infix))
        return true;
    }
  }
  return false;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_Q_TYPE Query::calc_query_type()
{
  const char * pos= m_masked_sql.c_str();
  for ( ; pos!=&*m_masked_sql.end() && isspace(*pos); pos++);
  const_string sql(pos,m_masked_sql.length() - (pos-m_masked_sql.c_str()));
  return  contains(sql,         CONST_STR("UNION"))   ? MYX_QT_UNION    :
  start_with(sql,               CONST_STR("SELECT"))  ?
          (contains(sql, CONST_STR("OUTFILE")) ? MYX_QT_SELECT_INTO_OUTFILE : MYX_QT_SELECT) :
          start_with(sql,       CONST_STR("UPDATE"))  ? MYX_QT_UPDATE   :
          start_with(sql,       CONST_STR("INSERT"))  ? MYX_QT_INSERT   :
          start_with(sql,       CONST_STR("DELETE"))  ? MYX_QT_DELETE   :
          start_with(sql,       CONST_STR("SHOW"))    ? MYX_QT_SHOW     :
          start_with(sql,       CONST_STR("SET"))     ? MYX_QT_SET      :
          start_with(sql,       CONST_STR("DESC"))    ? MYX_QT_DESCRIBE :
          start_with(sql,       CONST_STR("EXPLAIN")) ? MYX_QT_EXPLAIN  :
          start_with(sql,       CONST_STR("CALL"))    ? MYX_QT_CALL     :
          start_with(sql,       CONST_STR("CHECK"))   ? MYX_QT_CHECK    :
          start_with(sql,       CONST_STR("ANALYZE")) ? MYX_QT_ANALYZE  :
          start_with(sql,       CONST_STR("REPAIR"))  ? MYX_QT_REPAIR   :
          start_with(sql,       CONST_STR("OPTIMIZE"))? MYX_QT_OPTIMIZE :
          sql==CONST_STR("")                          ? MYX_QT_EMPTY    :
                                                        MYX_QT_UNKNOWN;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_QUERY * query_get_query_tables(MYSQL *mysql, MYX_QUERY *query)
{
  return s_query_get_query_tables(mysql,(Query*)query);
}

//----------------------------------------------------------------------------------------------------------------------

void init_table_name(MYSQL * mysql,
                     MYX_Q_TABLE * table, const char * table_name)
{
  table->fullname= g_strdup(table_name);

  //set table catalog, schema and name
  G_splitted_strings table_name_parts= g_strsplit(table_name, ".", 3);
  if (table_name_parts)
  {
    size_t k;
    for(k= 0; table_name_parts[k]; k++);
    size_t dot_count= k-1;

    if(dot_count==2)
    {
      table->catalog= unquote_identifier(g_strdup(table_name_parts[0]));
      table->schema=  unquote_identifier(g_strdup(table_name_parts[1]));
      table->name=    unquote_identifier(g_strdup(table_name_parts[2]));
    }
    else if(dot_count==1)
    {
      table->schema=  unquote_identifier(g_strdup(table_name_parts[0]));
      table->name=    unquote_identifier(g_strdup(table_name_parts[1]));
    }
    else
    {
      table->schema=  g_strdup(mysql->db);
      table->name=    unquote_identifier(g_strdup(table_name_parts[0]));
    }
    if (!table->catalog)
      table->catalog= g_strdup(default_catalog(table->schema));
  }
}

//----------------------------------------------------------------------------------------------------------------------

void init_table(MYSQL * mysql, MYX_Q_TABLE * table,
                const char * table_name, const char * alias)
{
  // has to be quoted already, or command would not work anyway
  memset(table, 0, sizeof(MYX_Q_TABLE));  

  table->columns_num= 0;
  table->columns=     NULL;

  table->pk_columns_num= 0;
  table->pk_columns= NULL;

  table->relationship_type= MYX_QTRT_NONE;
  table->relationship= NULL;

  init_table_name(mysql,table,table_name);

  //Check if alias valid
  table->alias= (!strfindword( alias, "JOIN"          ) &&
                 !strfindword( alias, "INNER"         ) &&
                 !strfindword( alias, "CROSS"         ) &&
                 !strfindword( alias, "STRAIGHT_JOIN" ) &&
                 !strfindword( alias, "LEFT"          ) &&
                 !strfindword( alias, "NATURAL"       ) &&
                 !strfindword( alias, "RIGHT"         )
                ) 
                  ? g_strdup(alias) : g_strdup("");  
}

//----------------------------------------------------------------------------------------------------------------------

const char * query_table_regexp_mask= "(?:,|JOIN|STRAIGHT_JOIN)?\\s*(" QUALIFIED_IDENTIFIER_IGNORE_PCRE ")?\\s*(" IDENTIFIER_PCRE ")?";

Query * s_query_get_query_tables(MYSQL *mysql, Query *query, MYX_Q_CLAUSE_TYPE c_type)
{
  const MYX_Q_CLAUSE * q_clause= query->get_clause(c_type);
  if (q_clause)
  {
    size_t q_len= q_clause->start_index - 1;
    const char * start= query->m_sql_without_comments.c_str() + q_len;
    char * masked_start= (char*)query->m_masked_sql.c_str() + q_len;
    char old_masked_start= *masked_start;
    *masked_start= ',';
    size_t clause_size= q_clause->end_index - q_clause->start_index + 1;
    const char *error_str;
    int erroffset, matched[60];
    pcre * pcre_exp= pcre_compile(query_table_regexp_mask, PCRE_CASELESS | PCRE_UTF8, &error_str,
      &erroffset, NULL);

    if (pcre_exp)
    {
      int offset= 0, rc;
      while ((rc= pcre_exec(pcre_exp,NULL, start, (int)clause_size, offset, 0, matched,
        sizeof(matched) / sizeof(*matched))) > 1)
      {
        const char *table_name, *alias;
        pcre_get_substring(start, matched, rc, 1, &table_name);
        if (rc > 2)
          pcre_get_substring(start, matched, rc, 2, &alias);
        else
          alias= "";

        query->m_tables.push_back(MYX_Q_TABLE());
        init_table(mysql, &query->m_tables.back(), table_name, alias);

         // Move offset to after the last matched part.
        offset= matched[2 * rc - 1];
        pcre_free_substring(table_name);
        if (rc > 2)
          pcre_free_substring(alias);
      }
      pcre_free(pcre_exp);
    }
    else
    {
      g_warning("Error compiling pcre expression: %s", error_str);
    }
    *masked_start= old_masked_start;
  }
  return query;
}

//----------------------------------------------------------------------------------------------------------------------

Query * s_query_get_query_tables(MYSQL *mysql, Query *query)
{
  return s_query_get_query_tables(mysql,query,MYX_QCT_FROM_CLAUSE);
}

//----------------------------------------------------------------------------------------------------------------------

const char * prepare_show_full_columns_stmt(MYX_Q_TABLE * table)
{
  return !table->schema
          ? g_strdup_printf("SHOW FULL COLUMNS FROM %s", table->fullname)
          : g_strdup_printf("SHOW FULL COLUMNS FROM `%s`.`%s`",
                            table->schema, table->name);
}

//----------------------------------------------------------------------------------------------------------------------

const char * prepare_show_create_table_stmt(MYX_Q_TABLE * table)
{
  return !table->schema
            ? g_strdup_printf("SHOW CREATE TABLE %s", table->fullname)
            : g_strdup_printf("SHOW CREATE TABLE `%s`.`%s`",
                              table->schema, table->name);
}

//----------------- Query_reader ---------------------------------------------------------------------------------------

Query_reader::Query_reader(MYSQL * mysql, const char * query)
{
  this->mysql= mysql;
  this->query= query;
  this->res= 0;
  this->first= this->last= 0;
}

//----------------------------------------------------------------------------------------------------------------------

Query_reader::~Query_reader()
{
  if (res)
  {
    mysql_free_result(res);
    res= 0;
  }
}

//----------------------------------------------------------------------------------------------------------------------

bool Query_reader::start()
{
  res= NULL;

  if (myx_mysql_query(mysql, query) == 0)
    res= mysql_store_result(mysql);
  if (res == NULL)
    return false;

  fields= mysql_fetch_fields(res);
  MYSQL_FIELD * fields_end= fields + mysql_num_fields(res);
  for (Row_field * row_field= first; row_field!=0; row_field= row_field->next)
  {
    size_t i= 0;
    for (MYSQL_FIELD * field= fields; field!=fields_end; field++, i++)
    {
      const const_string & name= row_field->column_name;
      if (strncmp(field->name, name.data(),name.length())==0)
      {
        row_field->i_field= i;
        goto continue_cycle_through_column_names;
      }
    }
    row_field->i_field= (size_t)-1;
continue_cycle_through_column_names:;
  }
  return true;
}

//----------------------------------------------------------------------------------------------------------------------

bool Query_reader::read()
{
  return (res || start()) && (row= mysql_fetch_row(res))!=0;
}

//----------------------------------------------------------------------------------------------------------------------

DECLARE_QUERY_FORMAT(Show_full_columns_format,

  DECLARE_QUERY_RESULT_COLUMN(Field,
  DECLARE_QUERY_RESULT_COLUMN(Key,
  DECLARE_QUERY_RESULT_COLUMN(Collation,
  DECLARE_QUERY_RESULT_COLUMN(Extra,

  DECLARE_QUERY_FORMAT_BIND()
)))) )

//----------------------------------------------------------------------------------------------------------------------

DECLARE_QUERY_FORMAT(Show_create_table_format,

  DECLARE_QUERY_RESULT_COLUMN_NAME(Create_stmt, CONST_STR("Create Table"),

  DECLARE_QUERY_FORMAT_BIND()
) )

//----------------------------------------------------------------------------------------------------------------------

MYX_QUERY * query_get_query_tables_columns(MYSQL * mysql, MYX_QUERY * query)
{
  return s_query_get_query_tables_columns(mysql,(Query*)query);
}

//----------------------------------------------------------------------------------------------------------------------

void s_query_get_query_tables_columns_for_table(MYSQL * mysql,
                                                MYX_Q_TABLE * q_table)
{
  G_string sqlcmd= prepare_show_full_columns_stmt(&*q_table);
  Show_full_columns_format q(mysql,sqlcmd.str);
  while (q.read())
  {
    if(q_table->columns==0)
    {
      q_table->columns=
          (MYX_Q_TABLE_COLUMN*)g_malloc0(sizeof(MYX_Q_TABLE_COLUMN));
    }
    else
    {
      q_table->columns=
          (MYX_Q_TABLE_COLUMN*)g_realloc(q_table->columns,
                                          sizeof(MYX_Q_TABLE_COLUMN)*
                                          (q_table->columns_num+1));
    }
    MYX_Q_TABLE_COLUMN * q_tbl_col= q_table->columns+q_table->columns_num;
    q_table->columns_num++;

    q_tbl_col->column= !q.Field.is_valid() ? 0 :     
                    myx_convert_dbstr_utf8(mysql, q.Field.get_value(), -1);
    q_tbl_col->is_pk= !q.Key.is_valid() ? 0 :
                                !strcmp2(q.Key.get_value(),"PRI") ? 1 : 0;
    q_tbl_col->is_autoincrement= !q.Extra.is_valid() ? 0 
                                  : g_strstr_len(q.Extra.get_value(),
                                                  q.Extra.get_field()->length,
                                                  "auto_increment") ? 1 : 0;
    q_tbl_col->charset= !q.Collation.is_valid() ? 0 :
              !strchr(q.Collation.get_value(), '_') ? 0 :
              g_strdup(q.Collation.get_value());
    
  }
}

//----------------------------------------------------------------------------------------------------------------------

const char * s_get_table_charset(MYSQL * mysql, MYX_Q_TABLE * q_table)
{
  const char * res= 0;
  G_string sqlcmd= prepare_show_create_table_stmt(&*q_table);
  Show_create_table_format q(mysql,sqlcmd.str);
  if (q.read())
  {
    int matched[12];
    int sc;
    int erroff;
    const char *err;
    
    G_string masked= g_strdup(q.Create_stmt.get_value());
    if (masked)
    {
      mask_quotas(masked);
      pcre * pcre_exp= pcre_compile("DEFAULT CHARSET=(\\w+)",
                                  0, &err, &erroff, NULL);
      if ((sc= pcre_exec(pcre_exp, NULL, masked, (int)strlen(masked),
                         0, 0, matched, sizeof(matched)/sizeof(*matched)))>=0)
      {
        res= g_strdup(PCRE_sub_string(masked, matched, sc, 1));
      }
      pcre_free(pcre_exp);
    }
  }
  return res;
}

//----------------------------------------------------------------------------------------------------------------------

Query * s_query_get_query_tables_columns(MYSQL * mysql, Query * query)
{
  for (Query::Type_tables::iterator q_table= query->m_tables.begin();
       q_table!=query->m_tables.end(); q_table++)
  {
    s_query_get_query_tables_columns_for_table(mysql,&*q_table);
    q_table->charset= (char*)s_get_table_charset(mysql,&*q_table);
  }
  return query;
}

//----------------------------------------------------------------------------------------------------------------------

void s_query_add_all_table_columns(Query *query, MYX_Q_TABLE *q_found_tbl)
{  // Add all columns from the table
  for (size_t j=0;j<q_found_tbl->columns_num;j++)
  {
    MYX_Q_TABLE_COLUMN * q_tbl_col= q_found_tbl->columns+j;

    query->m_columns.push_back(MYX_Q_COLUMN());
    MYX_Q_COLUMN *q_column= &query->m_columns.back();

    q_column->table_column= q_tbl_col;
    q_column->column= g_strdup(q_tbl_col->column);
    q_column->column_alias= g_strdup(q_tbl_col->column);
    q_column->table= q_found_tbl;
  }
}

//----------------------------------------------------------------------------------------------------------------------

void s_query_add_all_table_columns(Query *query)
{ //Add all columns from all tables
  for (Query::Type_tables::iterator it= query->m_tables.begin();
        it!=query->m_tables.end(); it++)
  {
    s_query_add_all_table_columns(query,&*it);
  }
}

//----------------------------------------------------------------------------------------------------------------------

void query_add_all_table_columns(MYX_QUERY *query, MYX_Q_TABLE *q_found_tbl)
{
  if (q_found_tbl) // if the user selects alias.*
  {
    s_query_add_all_table_columns((Query*)query,q_found_tbl);
  }
  else
  {
    s_query_add_all_table_columns((Query*)query);
  }
}

//----------------------------------------------------------------------------------------------------------------------

void query_add_table_column(Query * query, MYX_Q_TABLE * q_found_tbl,
                            const char *column, const char *column_alias)
{
  query->m_columns.push_back(MYX_Q_COLUMN());

  MYX_Q_COLUMN * q_column= & query->m_columns.back();

  q_column->table_column= get_column(q_found_tbl, column);
  q_column->column=       g_strdup(column);
  q_column->column_alias= g_strdup(column_alias);

  // if no table found, and there's only one table, use it
  q_column->table= q_found_tbl ? q_found_tbl :
                   query->tables_num!=1 ? 0 : query->tables;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_QUERY * query_get_query_columns(MYX_QUERY * query)
{
  return s_query_get_query_columns((Query*)query);
}

//----------------------------------------------------------------------------------------------------------------------
//now use the following regex to get the column infos
//(((\w*)\.)?(\*))|(((\w*)\.)+(\w+)(\s+as\s+(\w+))?)|((.*)\s+as\s+(\w+))|(.*)
// 3: table alias (optional)
// 4: *
//
// 7: table alias (optional)
// 8: column name
// 10: column alias
//
// 12: column or function or subselect
// 13: column alias
//
// 14: column name without table or function or subselect without as
// an identifier, with optional ` or " quotes, but grouping without the quotes
#define RE_NAME "(?:\\w+|`[^`]+`|\"[^\"]+\")"
const char * query_column_regexp_mask=
  "((("RE_NAME")\\.)?(\\*))|" // [X].Y
  "((("RE_NAME")\\.)+("RE_NAME")(\\s+as\\s+(\\w+))?)|" // [X].Y as Z
  "((.*)\\s+as\\s+(\\w+))|" // ? as Y
  "(.*)"; // X
#undef RE_NAME

//----------------------------------------------------------------------------------------------------------------------

// table_alias.* or *
// "((("RE_NAME")\\.)?(\\*))|"
void add_regexp_columns_with_asteric(Query *query, const char * columns,
                                     int rc, int * matched)
{
  PCRE_sub_string table_alias (columns, matched, rc, 3);
  PCRE_sub_string column      (columns, matched, rc, 4);

  G_string ta= unquote_identifier(g_strdup(table_alias));

  query_add_all_table_columns(query,
                              !*ta ? 0 : query->get_table_by_alias(ta));
}

//----------------------------------------------------------------------------------------------------------------------

// [table_alias.]column_name as column_alias
// "((.*)\\s+as\\s+(\\w+))|"
void add_regexp_columns_with_as(Query *query, const char * columns,
                                int rc, int * matched)
{
  PCRE_sub_string table_alias (columns, matched, rc, 7);
  PCRE_sub_string column      (columns, matched, rc, 8);
  PCRE_sub_string column_alias(columns, matched, rc, 10);

  G_string ta=         unquote_identifier(g_strdup(table_alias));
  G_string column_unq= unquote_identifier(g_strdup(column));

  query_add_table_column(query,
                          !*ta ? 0 : query->get_table_by_alias(ta),
                          column_unq, column_alias);
}

//----------------------------------------------------------------------------------------------------------------------

void add_regexp_columns_v_9(Query *query, const char * columns,
                            int rc, int * matched)
{
  PCRE_sub_string table_alias (columns, matched, rc, 7);
  PCRE_sub_string column      (columns, matched, rc, 8);

  G_string ta=         unquote_identifier(g_strdup(table_alias));
  G_string column_unq= unquote_identifier(g_strdup(column));

  query_add_table_column(query,
                          !*ta ? 0 : query->get_table_by_alias(ta),
                          column_unq, column_unq);
}

//----------------------------------------------------------------------------------------------------------------------

void add_regexp_columns_v_14(Query *query,
                             const char * columns, int rc, int * matched)
{
  PCRE_sub_string column        (columns, matched, rc, 12);
  PCRE_sub_string column_alias  (columns, matched, rc, 13);

  G_string column_unq= unquote_identifier(g_strdup(column));

  // this column has no table specification, so it should mean that
  // there's only one table in the query. 
  // we'll pick the 1st table, which is supposed to be the only one

  query_add_table_column(query, query->get_first_table(), column_unq, column_alias);
}

//----------------------------------------------------------------------------------------------------------------------

// (.*)
void add_regexp_columns_v_15(Query *query,
                             const char * columns, int rc, int * matched)
{
  PCRE_sub_string column(columns, matched, rc, 14);
  G_string column_unq= unquote_identifier(g_strdup(column));

  // this column has no table specification, so it should mean that
  // there's only one table in the query.
  // we'll pick the 1st table, which is supposed to be the only one

  query_add_table_column(query, query->get_first_table(), column_unq, column_unq);
}

//----------------------------------------------------------------------------------------------------------------------

struct line_delimiter_enumerator
{
  const char * begin;
  const char * end;
  const char * pos;
  char delimiter;

  line_delimiter_enumerator(const const_string & text, char delimiter= ',')
  {
    this->delimiter= delimiter;
    this->begin= text.data();
    this->end= begin + text.length();
    this->pos= begin;
    for ( ; pos!=end && *pos!=delimiter; pos++);
  }

  const_string operator * ()
  {
    return const_string(begin,pos-begin);
  }

  bool is_valid()
  {
    return pos!=0;
  }

  void operator ++ ()
  {
    if (pos==end)
    {
      pos= 0;
      begin= 0;
    }
    else
    {
      pos++;
      begin= pos;
      for (; pos!=end && *pos!=delimiter; pos++);
    }
  }
};

//----------------------------------------------------------------------------------------------------------------------

void strip(const_string * str)
{
  const char *pos;
  for (pos= str->begin();
       pos!=str->end() && isspace(*pos); pos++);
  *str= const_string(pos,str->length()-(pos-str->begin()));
  if (!str->empty())
  {
    for (pos= str->end()-1;
         pos!=str->begin() && isspace(*pos); pos--);
    *str= const_string(str->begin(),pos-str->begin()+1);
  }
}

//----------------------------------------------------------------------------------------------------------------------

Query * s_query_get_query_columns(Query *query)
{
  const MYX_Q_CLAUSE * select_clause;
  const char *error_str;
  int erroffset;
  pcre * pcre_exp;

  select_clause= query->get_clause(MYX_QCT_SELECT_CLAUSE);
  if (select_clause == NULL)
    return query;

  //size_t q_len= select_clause->start_index - 1;
  size_t q_len= select_clause->start_index;
  const char * start= query->m_sql_without_comments.c_str() + q_len;
  char * masked_start= (char*)query->m_masked_sql.c_str() + q_len;
  const_string sclause(masked_start,
                       select_clause->end_index - select_clause->start_index);

  pcre_exp= pcre_compile(query_column_regexp_mask, PCRE_CASELESS, &error_str, &erroffset, NULL);
  if (pcre_exp != NULL)
  {
    for (line_delimiter_enumerator e(sclause);e.is_valid(); ++e)
    {                                                 // Analyse every column
      const_string masked_col= *e;
      strip(&masked_col);
      const_string col(start+(masked_col.begin()-masked_start),
                       masked_col.length());
      char old_end= *col.end();
      *(char*)col.end()= 0;
      int matched[60];
      int rc= pcre_exec(pcre_exp, NULL, masked_col.data(),
                        (int)masked_col.length(), 0,0,
                        matched, sizeof(matched)/sizeof(*matched));
      switch (rc)
      {
      case 5: // table_alias.* or * -> "((("RE_NAME")\\.)?(\\*))|"
           add_regexp_columns_with_asteric(query,col.data(),rc,matched); break;
      case 9:
                    add_regexp_columns_v_9(query,col.data(),rc,matched); break;
      case 11: // [table_alias.]column_name as column_alias -> 
                add_regexp_columns_with_as(query,col.data(),rc,matched); break;
      case 14:
                   add_regexp_columns_v_14(query,col.data(),rc,matched); break;
      case 15: // (.*)
                   add_regexp_columns_v_15(query,col.data(),rc,matched); break;
      }
      *(char*)col.end()= old_end;
    }
    pcre_free(pcre_exp);
  }
  return query;
}

//----------------------------------------------------------------------------------------------------------------------

bool query_contains_bracket_in_a_column(Query *query)
{
  for (Query::Type_columns::iterator iter= query->m_columns.begin();
       iter!=query->m_columns.end(); iter++)
  {
    if (strchr(iter->column, '('))
      return true;
  }
  return false;
}

//----------------------------------------------------------------------------------------------------------------------

bool query_contains_distinct_distinctrow_option(Query *query)
{
  std::vector<const_string>::const_iterator option;
  for (option= query->m_const_string_options.begin();
       option!=query->m_const_string_options.end(); option++)
  {
    if (option->equal_case_insensitively_to(CONST_STR("DISTINCT")) ||
        option->equal_case_insensitively_to(CONST_STR("DISTINCTROW")))
    {
      return true;
    }
  }
  return false;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @note covered by unit tests.
 */
bool query_is_join(Query *query)
{
  for(Query::Type_clauses::iterator it = query->m_clauses.begin();
      it != query->m_clauses.end(); it++)
  {
    MYX_Q_CLAUSE c= *it;
    if(c.clause_type == MYX_QCT_FROM_CLAUSE)
    {
      char *t= g_strdup(c.clause);
      mask_quotas_and_brackets(t);
      bool b= (strchr(t, ',') != NULL);
      if (!b)
      {
        const char* location;
        while ((location= stristr(t, "join")) != NULL)
        {
          // If we find a "join" location then check if this is preceeded by a white space.
          // Otherwise it might be part of an identifier.
          if ((location == t) || (*(location -1) == ' ') || (*(location -1) == '\n') || (*(location -1) == '\r'))
          {
             b= true;
             break;
          };
          t= (char*)(location++);
        };
      };
      g_free(t);
      return b;
    }
  }
  return false;
}

//----------------------------------------------------------------------------------------------------------------------

void build_primary_keys_for_table(Query * query,
                                  MYX_Q_TABLE * q_tbl, int enforce_editable)
{
  MYX_Q_TABLE_COLUMN * q_tbl_col_end= q_tbl->columns + q_tbl->columns_num;
  MYX_Q_TABLE_COLUMN * q_tbl_col;
  for (q_tbl_col= q_tbl->columns; q_tbl_col!=q_tbl_col_end; q_tbl_col++)
  {
    if (q_tbl_col->is_pk)
    {
      MYX_Q_COLUMN * q_col= query->get_column(q_tbl_col->column);
      if (!q_col)
      {
        if (!enforce_editable)
          continue;
        // If PK is not found and enforce_editable is != 0, 
        // add it but increase pk_columns_added_num so it will not be displayed
        // vkolesnikov: it's important to keep added PK columns
        // at the end of the column list as some code (old & new)
        // relies on it
        q_col= query_add_column(query, q_tbl,
                                q_tbl_col->column,
                                query->sql, MYX_QCT_SELECT_CLAUSE);
        query->pk_columns_added_num++;
      }

      //add q_column to q_tbl->pk_columns
      if (!q_tbl->pk_columns)
      {
        q_tbl->pk_columns= (MYX_Q_COLUMN*)g_malloc0(sizeof(MYX_Q_COLUMN));
      }
      else
      {
        q_tbl->pk_columns= (MYX_Q_COLUMN*)
                      g_realloc(q_tbl->pk_columns,
                               sizeof(MYX_Q_COLUMN)*(q_tbl->pk_columns_num+1));
      }

      MYX_Q_COLUMN * q_tbl_pk_col= q_tbl->pk_columns+q_tbl->pk_columns_num;
      *q_tbl_pk_col= *q_col;

      q_tbl->pk_columns_num++;
    }
  }
}

/**
 * Checks whether the result of query can be edited in QB. If the query looks editable this function may modify it
 * by adding PKs.
 *
 * @param query The query being analyzed.
 * @param enforce_editable If 1 then the primary key is added to the columns list (if otherwise editable).
 *
 * @return 0 if not editable, 1 otherwise.
 */
unsigned int s_query_is_editable(Query *query, int enforce_editable)
{
  // if a column contains a '(', indicating it uses a function,
  // do not allow it to be editable. check that here because
  // we dont want it to have PK columns added either
  if (query_contains_bracket_in_a_column(query))
  {
    return 0;
  }
  
  // if the query contains DISTINCT or DISTINCTROW option, we can't edit it
  if (query_contains_distinct_distinctrow_option(query) ||
      query->get_clause(MYX_QCT_GROUP_CLAUSE) ||
      query->get_clause(MYX_QCT_HAVING_CLAUSE))
  {
    return 0;
  }

  // If the query is a join then we can't edit it.
  if (query_is_join(query))
  {
    return 0;
  }

  //only a query with one table is editable
  if (query->tables_num!=1)
    return 0;

  // check if no aggregate functions where used
  // TODO !

  // check if pk column was selected (tables doesn't need to be checked
  // since we only got one)
  build_primary_keys_for_table(query,query->tables,enforce_editable);
  if (query->tables->pk_columns_num <= 0)
    return 0;

  return 1;
}

//----------------------------------------------------------------------------------------------------------------------

unsigned int query_is_editable(MYX_QUERY * query, int enforce_editable)
{
  return s_query_is_editable((Query*)query,enforce_editable);
}

//----------------------------------------------------------------------------------------------------------------------

MYX_Q_COLUMN * query_add_column(MYX_QUERY *q, MYX_Q_TABLE *q_table,
                                const char *column, char *sql,
                                MYX_Q_CLAUSE_TYPE clause_type)
{
  Query * query= (Query*)q;
  MYX_Q_COLUMN * res= s_query_add_column(query,q_table,column,sql,clause_type);
  query->columns= &query->m_columns.front();
  query->columns_num= (unsigned int)query->m_columns.size();
  query->sql= (char*)query->m_sql.c_str();
  return res;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_Q_COLUMN * s_query_add_column(Query *query, MYX_Q_TABLE *q_table,
                                  const char *column, char *sql,
                                  MYX_Q_CLAUSE_TYPE clause_type)
{
  const MYX_Q_CLAUSE *select_clause= query->get_clause(MYX_QCT_SELECT_CLAUSE);
  if (!select_clause)
  {
    return 0;
  }
  else
  {
    query->m_columns.push_back(MYX_Q_COLUMN());
    MYX_Q_COLUMN & q_column= query->m_columns.back();

    q_column.table_column= get_column(q_table, column);
    q_column.column=       g_strdup(column);
    q_column.column_alias= NULL;
    q_column.table=        q_table;

    std::string new_sql;

    new_sql << get_left(query->m_sql,select_clause->end_index)
            << CONST_STR(", `") << const_string(column, strlen(column)) << CONST_STR("` ")
            << get_right(query->m_sql, select_clause->end_index);

    query->m_sql= new_sql;
    return &q_column;
  }
}

//----------------------------------------------------------------------------------------------------------------------

static int compare_strings_by_length (const void* elem1, const void* elem2)
{
  const char *c1= *(const char **)(elem1);
  const char *c2= *(const char **)(elem2);
  size_t len1= (strchr(c1, '=') - c1);
  size_t len2= (strchr(c2, '=') - c2);
  return (int)(len2 - len1);
}

//----------------------------------------------------------------------------------------------------------------------

//Still missing: don't replace parameters in strings
Query * s_query_set_params(Query * query, MYX_STRINGLIST * parameters)
{
  query->params_num= 0;

  if  (!parameters || !parameters->strings_num)
    return query;

  // sort by length to prevent bug #12402
  qsort(parameters->strings, parameters->strings_num, sizeof(char *), compare_strings_by_length);

  char ** parameters_end= parameters->strings + parameters->strings_num;
  for (char ** parameter= parameters->strings; 
       parameter!=parameters_end; parameter++)
  {
    char * pos_equal_char= strchr(*parameter, '=');

    if (!pos_equal_char)
      continue;

    size_t name_len= pos_equal_char - *parameter;
    size_t value_len= strlen(pos_equal_char);

    assert(name_len + 1 <= 256);
    assert(value_len + 1 <= 256);

    G_string name=  (char*)g_malloc((gulong)name_len + 1 + 1);
    G_string value= (char*)g_malloc((gulong)value_len + 1);

    name_of_str((char*)(name + 1), (const char*)*parameter);
    value_of_str(value, *parameter);

    if (name[1])
    {
      // name and value were allocated with extra space to allow fitting these
      name[0]=':'; 
      if (g_strstr_len(query->sql, (gssize)strlen(query->sql), name))
      {
        query->params_num++;
        char * new_sql= str_g_replace(g_strdup(query->m_sql.c_str()), name, value);
        query->m_sql= new_sql;
        query->sql= (char*)query->m_sql.c_str();
        g_free(new_sql);
      }
    }
  }
  return query;
}

//----------------------------------------------------------------------------------------------------------------------

//Still missing: don't replace parameters in strings
MYX_QUERY * query_set_params(MYX_QUERY * query, MYX_STRINGLIST * parameters)
{
  return s_query_set_params((Query*)query,parameters);
}

//----------------------------------------------------------------------------------------------------------------------

static char *strdup_until_semicolon(const char * sql, int * had_semicolon)
{
  char *s= g_strdup(sql);
  char *end= s+strlen(s)-1;

  while (end > s && isspace(*end) && *end!=';') end--;
  if (*end==';')
  {
    *had_semicolon= 1;
    *end= 0;
  }
  return s;
}

//----------------------------------------------------------------------------------------------------------------------

const_string prepare_head(const_string head, const MYX_Q_CLAUSE * clause)
{
  while (head.back()==' ')
    head= const_string(head.data(),head.length()-1);
  return head;
}

//----------------------------------------------------------------------------------------------------------------------

const_string prepare_tail_seperator(const const_string & tail,
                                    const MYX_Q_CLAUSE * clause)
{
  return clause->end_with_linebreak
            ? CONST_STR("\n")
            : (tail.empty() || tail.front()==' ')
                  ? CONST_STR("")
                  : CONST_STR(" ");
}

//----------------------------------------------------------------------------------------------------------------------

void myx_query_add_column_to_clause(Query & query,
                                    const const_string & column,
                                    const const_string & alias,
                                    const const_string & dot,
                                    int * cursor_pos,
                                    const MYX_Q_CLAUSE * clause,
                                    std::string * result)
{
  bool clause_is_empty=                       !strcmp("",  clause->clause);
  bool clause_is_asteric= !clause_is_empty && !strcmp("*", clause->clause);

  const_string head= get_left(query.m_sql,clause->end_index);
  // If SELECT * ..., remove * after first column has been selected
  if(clause_is_asteric && clause->clause_type==MYX_QCT_SELECT_CLAUSE)
    head= const_string(head.data(),head.length()-2);
  head= prepare_head(head,clause);

  const_string infix;
  // when this is the first column in the clause...
  if (clause_is_empty || clause_is_asteric) 
  {
    infix= CONST_STR(" ");
  }
  else
  {
    size_t clause_len= strlen(clause->clause);
    char last_symbol= clause->clause[clause_len-1];
    // When there is a =,<,> simply append column (WHERE iduser=)
    if (last_symbol=='=' || last_symbol=='<'|| last_symbol=='>')
    {
      infix= CONST_STR("");
    }
    else // In WHERE clause connect by AND
    { // !!!TODO: check if the last connection was OR
      infix= (clause->clause_type==MYX_QCT_WHERE_CLAUSE ||
              clause->clause_type==MYX_QCT_HAVING_CLAUSE)
                                        ? CONST_STR(" AND ") : CONST_STR(", ");
    }
  }

  *result << head << infix << alias << dot << CONST_STR("`") << column << CONST_STR("`");
  *cursor_pos= (int)result->length();
  const_string tail= get_right(query.m_sql, clause->end_index);
  *result << prepare_tail_seperator(tail,clause) << tail;
}

//----------------------------------------------------------------------------------------------------------------------

void myx_query_add_column_after_clause(Query & query,
                                       const const_string & schema,
                                       const const_string & table,
                                       MYX_Q_CLAUSE_TYPE clause_type,
                                       std::string * result)
{
  const MYX_Q_CLAUSE *insert_after_clause=
                                        query.get_previous_clause(clause_type);
  if (!insert_after_clause) // There was _no_ clause before this clause
  {
    *result << CONST_STR("SELECT * FROM ")
            << schema << CONST_STR(".") << table;
  }
  else
  {
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
    const_string infix= // Insert new clause
      clause_type==MYX_QCT_WHERE_CLAUSE  ? CONST_STR(" \r\nWHERE ")     :
      clause_type==MYX_QCT_GROUP_CLAUSE  ? CONST_STR(" \r\nGROUP BY ")  :
      clause_type==MYX_QCT_HAVING_CLAUSE ? CONST_STR(" \r\nHAVING ")    :
      clause_type==MYX_QCT_ORDER_CLAUSE  ? CONST_STR(" \r\nORDER BY ")  :
      clause_type==MYX_QCT_SET_CLAUSE    ? CONST_STR(" SET ")         : 
                                           const_string();
#else
    const_string infix= // Insert new clause
      clause_type==MYX_QCT_WHERE_CLAUSE  ? CONST_STR(" \nWHERE ")     :
      clause_type==MYX_QCT_GROUP_CLAUSE  ? CONST_STR(" \nGROUP BY ")  :
      clause_type==MYX_QCT_HAVING_CLAUSE ? CONST_STR(" \nHAVING ")    :
      clause_type==MYX_QCT_ORDER_CLAUSE  ? CONST_STR(" \nORDER BY ")  :
      clause_type==MYX_QCT_SET_CLAUSE    ? CONST_STR(" SET ")         : 
                                           const_string();
#endif
    assert(!infix.empty());

    *result << get_left  (query.m_sql, insert_after_clause->end_index)
            << infix
            << get_right (query.m_sql, insert_after_clause->end_index);
  }
}

//----------------------------------------------------------------------------------------------------------------------

bool is_senseless_query(const char * q)
{
  if (!q)
    return true;
  for (const char * c= q; *c; c++)
  {
    if (!isspace(*c) && *c!='\t' && *c!='\r' && *c!='\n')
      return false;
  }
  return true;
}

//----------------------------------------------------------------------------------------------------------------------

char * add_column_to_empty_sql(MYSQL * mysql, const char *default_schema, const char * catalog, const char * schema,
  const char * table, const char * column, MYX_Q_CLAUSE_TYPE clause_type, int * cursor_pos)
{
  char * sqls;
  MYX_Q_TABLE_ADD_ERROR error;

  switch (clause_type)
  {
  case MYX_QCT_NO_CLAUSE:
  case MYX_QCT_SELECT_CLAUSE:
  case MYX_QCT_FROM_CLAUSE:
  case MYX_QCT_WHERE_CLAUSE:
  case MYX_QCT_GROUP_CLAUSE:
  case MYX_QCT_HAVING_CLAUSE:
  case MYX_QCT_ORDER_CLAUSE:
  case MYX_QCT_LIMIT_CLAUSE:
    sqls= myx_query_add_table_to_sql(mysql, default_schema, catalog, schema,table, "", MYX_QTAT_SELECT, cursor_pos, &error);
    break;
  case MYX_QCT_INTO_CLAUSE:
    sqls= myx_query_add_table_to_sql(mysql, default_schema, catalog, schema,table, "", MYX_QTAT_INSERT, cursor_pos, &error);
    break;
  case MYX_QCT_SET_CLAUSE:
  case MYX_QCT_UPDATE_CLAUSE:
    sqls= myx_query_add_table_to_sql(mysql, default_schema, catalog,schema,table, "", MYX_QTAT_UPDATE, cursor_pos, &error);
    break;
  case MYX_QCT_DELETE_CLAUSE:
  case MYX_QCT_USING_CLAUSE:
    sqls= myx_query_add_table_to_sql(mysql, default_schema, catalog,schema, table, "", MYX_QTAT_DELETE, cursor_pos, &error);
    break;
  default:
    sqls= g_strdup("");
  };

  if (error != MYX_QC_OK)
  {
    g_free(sqls);
    return myx_query_add_column_to_sql(mysql, default_schema, catalog, schema, table, column, "", clause_type, cursor_pos);
  }
  else
  {
    char * res= myx_query_add_column_to_sql(mysql, default_schema, catalog, schema, table, column, sqls, clause_type, cursor_pos);

    g_free(sqls);
    return res;
  }
}

//----------------------------------------------------------------------------------------------------------------------

char * myx_query_add_column_to_sql(MYSQL * mysql, const char *default_schema, const char * catalog, const char * schema,
  const char * table, const char * column, const char * sqls, MYX_Q_CLAUSE_TYPE clause_type, int * cursor_pos)
{
  if (is_senseless_query(sqls))
  {
    return add_column_to_empty_sql(mysql, default_schema, catalog,schema, table, column, clause_type, cursor_pos);
  }
  
  int has_semicolon= 0;
  G_string sql= strdup_until_semicolon(sqls, &has_semicolon);

  Query query;
  query.analyze(mysql, sql);

  MYX_Q_TABLE *q_table= query.get_table(catalog,schema,table);
  if (!q_table)
    return g_strdup_printf("%s", query.m_sql.c_str()); // Add table to query

  const MYX_Q_CLAUSE * existing_clause= query.get_clause(clause_type);
  if (existing_clause) // If the clause already exists
  {
    const_string alias(q_table->alias,
                       !q_table->alias?0:strlen(q_table->alias));
    const_string dot= alias.empty() ? CONST_STR("") : CONST_STR(".");
    std::string result;
    myx_query_add_column_to_clause(query, const_string(column,strlen(column)),
                                   alias,dot,
                                   cursor_pos, existing_clause, &result);
    return g_strdup(result.c_str());
  }
  else if (!query.can_contain_clause(clause_type))
  {
    return g_strdup_printf("%s", query.m_sql.c_str());
  }
  else // If not found, add clause
  {
    std::string new_sql;
    char *s;
    myx_query_add_column_after_clause(query, const_string(schema,strlen(schema)), const_string(table,strlen(table)),
      clause_type,&new_sql);
    s= myx_query_add_column_to_sql(mysql, default_schema, catalog, schema, table, column, (char*)new_sql.c_str(),
      clause_type,cursor_pos);
    return s;
  }
}

//----------------------------------------------------------------------------------------------------------------------

char * myx_query_build_from_clauses(MYX_QUERY *query)
{
  unsigned int i;
  char *sql= NULL;

  for(i= 0; i<query->clauses_num; i++)
  {
    switch (query->clauses[i].clause_type)
    {
      case MYX_QCT_SELECT_CLAUSE: sql= str_g_append(sql, "SELECT ");    break;
      case MYX_QCT_FROM_CLAUSE:   sql= str_g_append(sql, "FROM ");      break;
      case MYX_QCT_WHERE_CLAUSE:  sql= str_g_append(sql, "WHERE ");     break;
      case MYX_QCT_GROUP_CLAUSE:  sql= str_g_append(sql, "GROUP BY ");  break;
      case MYX_QCT_HAVING_CLAUSE: sql= str_g_append(sql, "HAVING ");    break;
      case MYX_QCT_ORDER_CLAUSE:  sql= str_g_append(sql, "ORDER BY ");  break;
      case MYX_QCT_LIMIT_CLAUSE:  sql= str_g_append(sql, "LIMIT ");     break;
      case MYX_QCT_SET_CLAUSE:    sql= str_g_append(sql, "SET ");       break;
      case MYX_QCT_INTO_CLAUSE:   sql= str_g_append(sql, "INTO ");      break;
      case MYX_QCT_UPDATE_CLAUSE: sql= str_g_append(sql, "UPDATE ");    break;
      case MYX_QCT_NO_CLAUSE: break;
      case MYX_QCT_DELETE_CLAUSE: break;
      case MYX_QCT_USING_CLAUSE: break;
    }
    sql= str_g_append(sql, query->clauses[i].clause);
    sql= str_g_append(sql, " ");
  }

  return sql;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief copy the first utf8 symbol of src to the dst
 *
 * @param src a valid utf8 string (@see g_utf8_validate)
 * @param dst output buffer
 * @max_size dst size
 *
 * @return - size of copied symbol
 */
size_t copy_utf8_symbol(char * dst, const char * src, size_t max_size)
{
  char *next= g_utf8_next_char(src);
  size_t sz= next - src;
  if(sz < max_size)
  {
    memcpy(dst, src, sz);
    dst[sz] = 0;
    return sz;
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief strip all non-alpha chars from the string
 *
 * Strip all non-alpha chars from the string, so for example for identifier '_my_table123'
 * result will be 'mytable'
 *
 * @param table_name string
 *
 * @return dynamically allocated string (should be free() -d)
 */
char *get_canonical_table_name(const char *table_name)
{
  const char *eos, *next, *curr;

  if(!g_utf8_validate(table_name, -1, &eos))
  {
    return NULL;
  }

  char *cname= (char *)g_malloc(((gulong)(eos - table_name + 1)));
  memset(cname, 0, eos - table_name + 1);
  char *cnamenext= cname;

  for(curr= table_name, next= g_utf8_next_char(table_name);
      curr != eos;
      curr= next, next= g_utf8_next_char(next))
  {
    if(g_unichar_isalpha(g_utf8_get_char(curr)))
    {
      size_t charsize= next - curr;
      memcpy(cnamenext, curr, charsize);
      cnamenext+= charsize;
    }
  }

  return cname;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief g_tree comparator call-back function
 */
static gint is_comp_func (gconstpointer a, gconstpointer b)
{
	return g_strcasecmp((char *) a, (char *) b);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief checks whether a string is a MySQL keyword (caseinsensitive)
 *
 * Checks whether a string is a MySQL keyword (caseinsensitive). MySQL 5.0 keyword set is used.
 * Warning: this function uses static data and therefore is not thread safe
 *
 * @param str string to check
 */
static bool is_sql_keyword(const char *str)
{
  static GTree *tree= 0;

  if(0==tree)
  {
    tree= g_tree_new(is_comp_func);
    for(size_t i= 0; i < sizeof(symbols)/sizeof(SYMBOL); i++)
    {
      g_tree_insert(tree, strdup(symbols[i].name), strdup(symbols[i].name));
    }
    for(size_t i= 0; i < sizeof(sql_functions)/sizeof(SYMBOL); i++)
    {
      g_tree_insert(tree, strdup(sql_functions[i].name), strdup(sql_functions[i].name));
    }
  }

  return g_tree_lookup(tree, str) != 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief generates table alias
 *
 * returns alias for the new_tablename or empty string if
 * usage of table name is preferrable to alias
 *
 * @param query query, the new table is to be added to
 * @param new_tablename table name to be added
 *
 * @return alias name string (should be g_free() -d)
 */

char * myx_query_get_table_alias(Query *query, const char * new_tablename)
{
  int i= 1;
  gulong alias_size= 16;
  char *alias= (char *)g_malloc(alias_size);
  char *alias_next= alias;

  const char *new_cname= get_canonical_table_name(new_tablename);
  MYX_Q_TABLE *q_table;
  const char *pos1, *pos2;

  alias_next+= copy_utf8_symbol(alias, new_cname, sizeof(alias)-1);

  // try to find an unique alphabetical table name alias
  do
  {
    char *table_cname;

    q_table= query->get_table_by_alias(alias);
    if(q_table != NULL)
    {
      table_cname= get_canonical_table_name(q_table->name);
    }
    else if(is_sql_keyword(alias))
    {
      table_cname= strcpy((char *)g_malloc(((gulong)strlen(alias)+1)), alias);
    }
    else if(query->contains_unaliased_table(alias))
    {
      table_cname= strcpy((char *)g_malloc(((gulong)strlen(alias)+1)), alias);
    }
    else
    {
      goto ret;
    }

    for(pos2= new_cname, pos1= table_cname;
        *pos1 && *pos2 && *pos1 == *pos2;
        pos1++, pos2++);

    g_free(table_cname);

    if (*pos2)
    {
      if((alias + alias_size - 1) < (g_utf8_next_char(alias_next)))
      {
        size_t offs= alias_next - alias;
        char *alias_old = alias;
        alias= (char *)memcpy(g_malloc(alias_size<<1), alias, alias_size);
        g_free(alias_old);
        alias_size <<= 1;
        alias_next= alias + offs;
      }

      alias_next+=
        copy_utf8_symbol(alias_next, pos2, alias + alias_size - alias_next);
    }
  }
  while(*pos2);

  // didn't find an unique alphabetical alias
  if((alias + alias_size - 1) < alias_next+16)
  {
    size_t offs= alias_next - alias;
    char *alias_old = alias;
    alias= (char *)memcpy(g_malloc(alias_size+16), alias, alias_size);
    g_free(alias_old);
    alias_next= alias + offs;
  }

  // create number-based alias
  // this expected to succeed always
  while(true)
  {
    sprintf(alias_next, "%u", i++);

    if(query->get_table_by_alias(alias) || query->get_table(0, 0, alias))
    {
      continue;
    }
    else
    {
      goto ret;
    }
  }

  // here it makes decision whether to use alias
  // or its better to use table name itself w/o an alias
  ret:
  if(g_utf8_collate(alias, new_tablename) == 0)
  {
    return strcpy((char *)g_malloc(1), "");
  }

  if((strlen(alias) < strlen(new_tablename))
  || query->get_table_by_alias(new_tablename)
  || query->contains_unaliased_table(new_tablename)
  || is_sql_keyword(new_tablename)
  || (strlen(new_cname) < strlen(new_tablename)))	// tablename contains weird symbols
  {
    return alias;
  }

  return strcpy((char *)g_malloc(1), "");
}

//----------------------------------------------------------------------------------------------------------------------

struct full_table_name
{
  const_name schema_name, table_name;

  full_table_name(MYSQL * mysql, 
                  const const_string & schema_name_arg,
                  const const_string & table_name_arg)
    : schema_name(mysql,schema_name_arg),
      table_name(mysql,table_name_arg)
  {}

  friend std::string & operator << (std::string & s, const full_table_name & t)
  {
    return s << t.schema_name << CONST_STR(".") << t.table_name;
  }

  size_t table_name_length() const
  {
    return table_name.length();
  }
};

//----------------------------------------------------------------------------------------------------------------------

struct full_table_name_with_def
{
  full_table_name table;
  const_string    default_schema_name;

  full_table_name_with_def(MYSQL *mysql,
                           const char * defalut_schema_name_arg,
                           const const_string & schema_name_arg,
                           const const_string & table_name_arg)
    : table(mysql,schema_name_arg,table_name_arg),
      default_schema_name(defalut_schema_name_arg,
                          strlen(defalut_schema_name_arg))
  {}

  full_table_name_with_def(MYSQL *mysql,
                           const char * defalut_schema_name_arg,
                           const char * schema_name_arg,
                           const char * table_name_arg)
    : table(mysql,const_string(schema_name_arg,strlen(schema_name_arg)),
            const_string(table_name_arg,strlen(table_name_arg))),
      default_schema_name(defalut_schema_name_arg,
                          defalut_schema_name_arg?strlen(defalut_schema_name_arg):0)
  {}

  size_t table_name_length() const
  {
    return table.table_name_length();
  }

  friend std::string & operator << (std::string & s,
                                    const full_table_name_with_def & t)
  {
    return t.default_schema_name.empty() ||
           t.default_schema_name!=t.table.schema_name
                ? s << t.table
                : s << t.table.table_name;
  }
};

//----------------------------------------------------------------------------------------------------------------------

struct aliased_full_table_name_with_def
{
  full_table_name_with_def table;
  const_string             alias;

  aliased_full_table_name_with_def(const full_table_name_with_def & table_arg)
    : table(table_arg) {}

  aliased_full_table_name_with_def(const full_table_name_with_def & table_arg,
                                   const const_string & alias_arg)
    : table(table_arg),
      alias(alias_arg)
  {}

  friend std::string & operator << (std::string & s,
                                    const aliased_full_table_name_with_def & t)
  {
    return s << t.table << CONST_STR(" ") << t.alias;
  }
};

//----------------------------------------------------------------------------------------------------------------------

struct table_name_with_created_alias : public aliased_full_table_name_with_def
{
  table_name_with_created_alias(Query & query,
                                const full_table_name_with_def & t)
    : aliased_full_table_name_with_def(t)
  {
    const char * a=
                   myx_query_get_table_alias(&query,t.table.table_name.data());
    alias= const_string(a,strlen(a));
  }
  ~table_name_with_created_alias()
  {
    g_free((gpointer)alias.data());
  }
};

//----------------------------------------------------------------------------------------------------------------------

bool Query::try_to_fix_table_aliases(MYSQL * mysql)
{
  const MYX_Q_CLAUSE * select_clause= get_clause(MYX_QCT_SELECT_CLAUSE);
  if (!select_clause ||
      get_clause(MYX_QCT_WHERE_CLAUSE) || get_clause(MYX_QCT_GROUP_CLAUSE) ||
      get_clause(MYX_QCT_HAVING_CLAUSE) || get_clause(MYX_QCT_ORDER_CLAUSE) ||
      strcmp(select_clause->clause,"*"))
  {
    return false;
  }

  MYX_Q_CLAUSE * from_clause= (MYX_Q_CLAUSE *)get_clause(MYX_QCT_FROM_CLAUSE);
  if (!from_clause)
    return true;

  std::string from_clause_txt= " ";
  from_clause_txt.append(from_clause->clause);
  std::string fname, old_fname;
  Type_tables::iterator it;
  for (it= m_tables.begin(); it!=m_tables.end(); it++)
  {
    if (strcmp2(it->alias, "") == 0)
    {
      char * new_alias= myx_query_get_table_alias(this, it->name);

      aliased_full_table_name_with_def aftn(
        full_table_name_with_def(mysql, it->schema, it->schema, it->name),
        const_string(new_alias, strlen(new_alias)));
      fname= " ";
      fname << aftn;

      old_fname= " ";
      old_fname.append(it->fullname);

      const_string cold_fname(old_fname.c_str(),old_fname.length());
      const_string cfname(fname.c_str(),fname.length());

      replace(&m_sql,cold_fname,cfname);
      replace(&from_clause_txt,cold_fname,cfname);
      from_clause->end_index+= (unsigned int)fname.length()-old_fname.length();

      it->alias= new_alias;
    }
  }
  g_free(from_clause->clause);
  from_clause->clause= g_strstrip(g_strdup(from_clause_txt.c_str()));
  return true;
}

//----------------------------------------------------------------------------------------------------------------------

const char * look_for_last_breakline(const const_string & buffer)
{
  if (buffer.empty())
  {
    return buffer.begin();
  }
  else
  {
    const char * br_pos;
    for (br_pos= buffer.end()-1;
        br_pos!=buffer.begin() && *br_pos != '\n';
        br_pos--);
    return br_pos;
  }
}

//----------------------------------------------------------------------------------------------------------------------

void add_strings_through_separator(MYX_STRINGLIST * strings,
                                   const const_string & separator,
                                   std::string * res)
{
  size_t pos= &*res->end() -
    look_for_last_breakline(const_string(res->c_str(),res->length()));

  if (strings->strings_num)
  {
    size_t len= strlen(strings->strings[0]);
    res->append(strings->strings[0],len);
    pos+= len;
    for (size_t i= 1; i < strings->strings_num; i++)
    {
      *res << separator;
      pos+= separator.length();
      if (pos > MAX_COMMAND_LINE_LENGTH)
      {
        *res << CONST_STR("\n");
        pos= 0;
      }
      size_t len= strlen(strings->strings[i]);
      res->append(strings->strings[i]);
      pos+= len;
    }
  }
}

//----------------------------------------------------------------------------------------------------------------------

MYX_STRINGLIST * myx_check_all_relationships
                              (Query & query, MYSQL * mysql,
                               const table_name_with_created_alias & table_ref)
{
  const char * schema= table_ref.table.table.schema_name.data();
  const char * table= table_ref.table.table.table_name.data();
  const char * alias= table_ref.alias.data();
	
	if(strlen(alias) == 0)
	{
		alias = table;
	}

  MYX_STRINGLIST * rels= 0;
  Query::Type_tables::iterator it;
  for (it= query.m_tables.begin(); it!=query.m_tables.end(); it++)
  {
    const char *it_alias = it->alias;
		if(strlen(it_alias) == 0)
		{
			it_alias = it->name;
		};

    rels= myx_check_table_relationship(mysql, schema,table, alias, it->name, it_alias);
		if (rels != NULL)
      break;
    else
    {
      rels= myx_check_table_relationship(mysql, schema, it->name, it_alias, table, alias);
      if (rels != NULL)
        break;
    }
  }
  return rels;
}

//----------------------------------------------------------------------------------------------------------------------

int update_query(const full_table_name_with_def & table, std::string * res)
{
  Query query;
  table_name_with_created_alias table_ref(query,table);
  *res << CONST_STR("UPDATE ") << table_ref << CONST_STR(" SET");
  return (int)res->length();
}

//----------------------------------------------------------------------------------------------------------------------

int delete_query(const full_table_name_with_def & table, std::string * res)
{
  *res << CONST_STR("DELETE FROM ") << table << CONST_STR("\nWHERE");
  return (int)res->length();
}

//----------------------------------------------------------------------------------------------------------------------

int insert_query(const full_table_name_with_def & table, std::string * res)
{
  *res << CONST_STR("INSERT INTO ") << table << CONST_STR(" VALUES()");
  return (int)res->length() - 1;
}

//----------------------------------------------------------------------------------------------------------------------

int new_select_query(const full_table_name_with_def & table,
                     std::string * res)
{
  Query query;
  table_name_with_created_alias table_ref(query,table);
  *res << CONST_STR("SELECT * FROM ") << table_ref;
  return (int)res->length();
}

//----------------------------------------------------------------------------------------------------------------------

int insert_query_select_add(Query &query,
                            const table_name_with_created_alias & table_ref,
                            std::string * res, size_t pos)
{
  *res << get_left(query.m_sql,pos)
       << CONST_STR(", ") << table_ref
       << get_right(query.m_sql,pos);
  return (int)res->length();
}

//----------------------------------------------------------------------------------------------------------------------

int insert_query_select_left_outer_join
                              (Query &query,
                               const table_name_with_created_alias & table_ref,
                               std::string * res, size_t pos,
                               MYX_STRINGLIST *rels)
{
  *res << get_left(query.m_sql,pos)
       << CONST_STR(" LEFT OUTER JOIN ") << table_ref << CONST_STR(" ON ");

  add_strings_through_separator(rels,CONST_STR(" AND "),res);
  myx_free_stringlist(rels);

  *res << get_right(query.m_sql,pos);

  return (int)res->length();
}

//----------------------------------------------------------------------------------------------------------------------

int prepare_position_for_insert(const const_string & text, int position)
{
  int res= position;

  // rewind a few chars if there's a \n in the end of the clause
  while (res>0 && ISSPACE(text[res-1])) res--;

  return res;
}

//----------------------------------------------------------------------------------------------------------------------

int insert_query_select_join(Query & query,
                             const table_name_with_created_alias & table_ref,
                             std::string * res, size_t pos,
                             MYX_STRINGLIST *rels)
{
  const MYX_Q_CLAUSE *where_clause;

  *res  << get_left(query.m_sql,pos) << CONST_STR(", ") << table_ref
        << CONST_STR("\nWHERE ");

  where_clause= query.get_clause(MYX_QCT_WHERE_CLAUSE);
  if (where_clause != NULL)
  {
    pos= prepare_position_for_insert(const_string(query.m_sql.c_str(),
                                                  query.m_sql.length()),
                                     where_clause->end_index);
    const_string where_clause_txt(where_clause->clause,
                                  strlen(where_clause->clause));
    *res  << where_clause_txt << CONST_STR(" AND ");
  }

  add_strings_through_separator(rels,CONST_STR(" AND "),res);
  myx_free_stringlist(rels);

  *res << get_right(query.m_sql,pos);

  return (int)res->length();
}

//----------------------------------------------------------------------------------------------------------------------

void query_add_table_to_select(MYSQL *mysql, const const_string & prepared_sql,
                               MYX_Q_TABLE_ADD_TYPE a_type,
                               const full_table_name_with_def & t,
                               std::string * res, int * pos,
                               MYX_Q_TABLE_ADD_ERROR * error)
{
  const MYX_Q_CLAUSE *from_clause;
  Query query;
  query.analyze(mysql, prepared_sql.data());

  if      ( query.query_type == MYX_QT_EMPTY || a_type == MYX_QTAT_SELECT)
  {
    *pos= new_select_query(t,res);
  }
  else
  {
    from_clause= NULL; 
    if (query.query_type == MYX_QT_SELECT && a_type != MYX_QTAT_SELECT)
      from_clause= query.get_clause(MYX_QCT_FROM_CLAUSE);
    if (from_clause == NULL && query.query_type == MYX_QT_UPDATE && a_type != MYX_QTAT_SELECT)
      from_clause= query.get_clause(MYX_QCT_UPDATE_CLAUSE);
    if (from_clause == NULL && query.query_type == MYX_QT_DELETE && a_type != MYX_QTAT_SELECT)
      from_clause= query.get_clause(MYX_QCT_FROM_CLAUSE);
      
    if (from_clause != NULL)
    {
      MYX_STRINGLIST *rels;
      const_string sql(query.m_sql.c_str(),query.m_sql.length());
      int clause_end= prepare_position_for_insert(sql,from_clause->end_index);
      table_name_with_created_alias table_ref(query,t);
      if (a_type==MYX_QTAT_SELECT_ADD)
      {
        *pos= insert_query_select_add(query,table_ref,res,clause_end);
      }
      else
      {
        rels= myx_check_all_relationships(query,mysql,table_ref);
        if (rels == NULL)
          *error= MYX_QC_TABLES_CAN_NOT_BE_JOINED;
        else
        {
          *pos= a_type == MYX_QTAT_SELECT_JOIN
              ? insert_query_select_join(query,table_ref,res,clause_end,rels)
              : insert_query_select_left_outer_join(query,table_ref,
                                                    res,clause_end,rels);
        }
      }
    }
  }
}

//----------------------------------------------------------------------------------------------------------------------

const_string prepare_sql_to_add_table(MYSQL *mysql, const char * sql,
                                      const char *table)
{
  char * cprepared_sql= utf8_str_trim(g_strdup(sql));

  const_string prepared_sql(cprepared_sql,strlen(cprepared_sql));
  size_t new_length= prepared_sql.length()-1;

  if (!prepared_sql.empty() &&
       cprepared_sql[new_length] == ';') // removing terminating ;
  {
    cprepared_sql[new_length]= 0;
    prepared_sql= const_string(prepared_sql.data(),new_length);
  }

  return prepared_sql;
}

//----------------------------------------------------------------------------------------------------------------------

char * myx_query_add_table_to_sql(MYSQL *mysql, const char *default_schema,
                                  const char *catalog, const char *schema, 
                                  const char *table, const char *sql,
                                  MYX_Q_TABLE_ADD_TYPE a_type, int * pos,
                                  MYX_Q_TABLE_ADD_ERROR * error)
{
  *error= MYX_QC_OK;
  const_string prepared_sql= prepare_sql_to_add_table(mysql,sql,table);
  std::string buffer;
  full_table_name_with_def table_name(mysql, default_schema, schema,table);

  switch (a_type)
  {
  case MYX_QTAT_UPDATE: *pos= update_query(table_name,&buffer); break;
  case MYX_QTAT_DELETE: *pos= delete_query(table_name,&buffer); break;
  case MYX_QTAT_INSERT: *pos= insert_query(table_name,&buffer); break;

  case MYX_QTAT_SELECT:       case MYX_QTAT_SELECT_ADD:
  case MYX_QTAT_SELECT_JOIN:  case MYX_QTAT_SELECT_LEFT_OUTER_JOIN:
      query_add_table_to_select(mysql,prepared_sql,
                                a_type,table_name,&buffer,pos,error);
      break;

  case MYX_QTAT_UNKNOWN: break;
  }

  if (!buffer.empty())
  {
    g_free((gpointer)prepared_sql.data());
    return g_strdup(buffer.c_str());
  }
  else
  {
    *pos= (int)prepared_sql.length();
    return (char*)prepared_sql.data();
  }
}

//----------------------------------------------------------------------------------------------------------------------

static
    void add_relation_to_stringlist(MYSQL * mysql, MYX_STRINGLIST *stringlist,
                                    const char *col_table1, const char *alias1,
                                    const char *col_table2, const char *alias2)
{
  full_table_name name1(mysql,const_string(alias1,strlen(alias1)),
                        const_string(col_table1,strlen(col_table1)));
  full_table_name name2(mysql,const_string(alias2,strlen(alias2)),
                        const_string(col_table2,strlen(col_table2)));

  stringlist->strings_num++;
  stringlist->strings= 
      (char**)g_realloc(stringlist->strings,
                        sizeof(char*) * stringlist->strings_num);
  std::string relation;
  relation << name1 << CONST_STR("=") << name2;
  stringlist->strings[stringlist->strings_num-1]= g_strdup(relation.c_str());
}

DECLARE_QUERY_FORMAT(Show_columns_format,

  DECLARE_QUERY_RESULT_COLUMN(Field,
  DECLARE_QUERY_RESULT_COLUMN(Type,
  DECLARE_QUERY_RESULT_COLUMN(Key,

DECLARE_QUERY_FORMAT_BIND()  )))  )

//----------------------------------------------------------------------------------------------------------------------

static MYX_NAME_VALUE_PAIR *get_table_columns(MYSQL *mysql, const char *schema, const char *table, int *column_num)
{
  MYX_NAME_VALUE_PAIR *columns= 0;
  *column_num= 0;

  G_string sql= g_strdup_printf("show columns from `%s`.`%s`", schema, table);
  Show_columns_format q(mysql,sql);
  if (q.read())
  {
    *column_num= (int)q.get_num_rows();
    columns= g_new(MYX_NAME_VALUE_PAIR, *column_num);
    MYX_NAME_VALUE_PAIR *column= columns;
    do
    {
      column->name= g_strdup_printf("%c%s", // column name + pk info
                                    (q.Key.is_valid() && 
                                     strcmp(q.Key.get_value(),"PRI")==0)
                                        ? '*' : ' ' ,
                                     q.Field.get_value());
      column->value= g_strdup(q.Type.get_value());
      column++;
    }
    while (q.read());
  }
  return columns;
}

//----------------------------------------------------------------------------------------------------------------------

static int match_key_names(const char *col1_name, const char *table1, const char *col2_name, const char *table2)
{
  char *col;
  
  if (strcasecmp(col1_name, col2_name)==0)
    return 1;
  
  // table1col1 == col2
  col= g_strdup_printf("%s%s", table1, col1_name);
  if (strcasecmp(col, col2_name)==0)
  {
    g_free(col);
    return 1;
  }
  g_free(col);

  // table1_col1 == col2
  col= g_strdup_printf("%s_%s", table1, col1_name);
  if (strcasecmp(col, col2_name)==0)
  {
    g_free(col);
    return 1;
  }
  g_free(col);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

const char * get_engine(const char * create_stmt, size_t create_stmt_len)
{
  const char * engine= get_value_from_text(create_stmt,
                                           (int)create_stmt_len,
                                           ".*TYPE=(\\w+).*");
  if (!engine)
  {
    engine= get_value_from_text(create_stmt,
                                (int)create_stmt_len, ".*ENGINE=(\\w+).*");
  }
  return engine;
}

//----------------------------------------------------------------------------------------------------------------------

#define OVECTOR_COUNT 12
MYX_STRINGLIST * 
            myx_check_table_relationship(MYSQL *mysql, const char *schema,
                                        const char *table1, const char *alias1,
                                        const char *table2, const char *alias2)
{
  char *create_stmt;
  MYSQL_RES *res;
  MYSQL_ROW row;
  MYX_STRINGLIST *stringlist;
  pcre *regex_fk;
  int ovector[OVECTOR_COUNT];
  int start_pos= 0, rc;
  const char *engine;
  size_t create_stmt_len;
  const char * reg_exp= 
      "FOREIGN\\s+KEY\\s*\\((\\S+)\\)\\s*REFERENCES\\s+(\\S+)\\s+\\((\\S+)\\)";

  G_string sql= g_strdup_printf("show create table `%s`.`%s`", schema, table1);
  res= NULL;
  row= NULL;
  if (mysql_query(mysql, sql) == 0)
  {
    res= mysql_store_result(mysql);
    if (res != NULL)
      row= mysql_fetch_row(res);
  }
  if (row == NULL)
    return NULL;

  create_stmt= row[1];
  G_string masked_stmt= g_strdup(create_stmt);
  mask_quotas(masked_stmt);
  create_stmt_len= strlen(create_stmt);

  const char *error_str;
  int erroffset;    
  regex_fk= pcre_compile(reg_exp,PCRE_CASELESS|PCRE_DOTALL,&error_str,
                         &erroffset, NULL);
  if (!regex_fk)
    return NULL;

  engine= get_engine(masked_stmt,create_stmt_len);
  stringlist= (MYX_STRINGLIST*)g_malloc0(sizeof(MYX_STRINGLIST));

  if (engine && strcasecmp(engine, "innodb")==0)
  {
    while ((rc= pcre_exec(regex_fk, NULL, masked_stmt, (int)create_stmt_len,
                          start_pos, 0, ovector, OVECTOR_COUNT) ) > 0)
    {
      const char *col_table1, *table3, *col_table3;
      
      pcre_get_substring(create_stmt, ovector, rc, 1, &col_table1);
      pcre_get_substring(create_stmt, ovector, rc, 2, &table3);
      pcre_get_substring(create_stmt, ovector, rc, 3, &col_table3);
      
      col_table1= unquote_identifier( (char*)col_table1);
      table3=     unquote_identifier( (char*)table3);
      col_table3= unquote_identifier( (char*)col_table3);
      
      if (!strcmp(table2, table3))
      {
        add_relation_to_stringlist(mysql,stringlist,
                                   col_table1, alias1,
                                   col_table3, alias2);
      }
      
      pcre_free_substring(col_table1);
      pcre_free_substring(table3);
      pcre_free_substring(col_table3);
      
      start_pos= ovector[1];
    }
  }
  else // MyISAM and others
  {
    MYX_NAME_VALUE_PAIR *t1_columns;
    MYX_NAME_VALUE_PAIR *t2_columns;
    int t1_columns_num;
    int t2_columns_num;
    int i, j;

    t1_columns= get_table_columns(mysql, schema, table1, &t1_columns_num);
    t2_columns= get_table_columns(mysql, schema, table2, &t2_columns_num);
    
    if (t1_columns && t2_columns)
    {
      // for each column...
      for (i= 0; i < t1_columns_num; i++)
      {
        // ...that is a PK
        if (t1_columns[i].name[0] != '*') // 1st char indicates whether PK
          continue;
        
        for (j= 0; j < t2_columns_num; j++)
        {
          // if the types match
          if (strcmp(t1_columns[i].value, t2_columns[j].value)==0)
          {
            // check if the names have similar patterns
            if (match_key_names(t1_columns[i].name+1, table1,
                                t2_columns[j].name+1, table2))
            {
              // XXX add regex patterns
              add_relation_to_stringlist(mysql,stringlist,
                                         t1_columns[i].name+1, alias1,
                                         t2_columns[j].name+1, alias2);
            }
          }
        }
      }
    }

    for (i= 0; i < t1_columns_num; i++)
    {
      g_free(t1_columns[i].name);
      g_free(t1_columns[i].value);
    }
    g_free(t1_columns);
    for (i= 0; i < t2_columns_num; i++)
    {
      g_free(t2_columns[i].name);
      g_free(t2_columns[i].value);
    }
    g_free(t2_columns);
  }

  g_free((char*)engine);
  pcre_free(regex_fk);
  mysql_free_result(res);

  //Only return stringlist if there have been strings added
  if(stringlist->strings_num == 0)
  {
    myx_free_stringlist(stringlist);
    stringlist= NULL;
  }

  return stringlist;
}

//----------------------------------------------------------------------------------------------------------------------

char *myx_get_create_table_script(MYSQL *mysql, const char *catalog, const char *schema, const char *table)
{
  MYSQL_RES *res;

  char * sql= g_strdup_printf("show create table `%s`.`%s`", schema, table);
  res= NULL;
  if (mysql_query(mysql, sql) == 0)
    res= mysql_store_result(mysql);
  if (res == NULL)
  {
    g_free(sql);
    return NULL;
  }
  g_free(sql);

  MYSQL_ROW row= mysql_fetch_row(res);
  sql= !row ? NULL : g_strdup(row[1]);

  mysql_free_result(res);

  return sql;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Stops the currently running query in target (if there is one) using a separate connection given in mysql.
 * Important: Call this function from a different thread than where the target query is running!
 *            You cannot kill a query from the same thread in which the query itself is currently running.
 *
 * @param mysql The connection to use for sending the kill command.
 * @param target The connection in which a query is running that should be terminated.
 *
 * @result 0 if all went fine, otherwise <> 0.
 *
 * @note For server versions earlier than 5.0 the entire connection is killed, not only the running query.
 *       Hence you will need to reestablish the connection afterwards (in this case).
 */
int myx_kill_query(MYSQL* mysql, MYSQL* target)
{
  char buffer[30];

  sprintf(buffer, "KILL /*!50000 QUERY */ %d", target->thread_id);
  return mysql_query(mysql, buffer);
}

//----------------------------------------------------------------------------------------------------------------------

struct Image_format
{
  const_string name;
  MYX_IMAGE_FORMAT format;
  Image_format(const_string name, MYX_IMAGE_FORMAT format)
  {
    this->name= name;
    this->format= format;
  }
};

//----------------------------------------------------------------------------------------------------------------------

static Image_format file_headers[]=
{
  Image_format( CONST_STR("\x89PNG"),  MYX_IMG_PNG  ),
  Image_format( CONST_STR("\xd8\xff"), MYX_IMG_JPEG ),
  Image_format( CONST_STR("\xff\xd8"), MYX_IMG_JPEG ),
  Image_format( CONST_STR("GIF8"),     MYX_IMG_GIF  ),
  Image_format( CONST_STR("BM"),       MYX_IMG_BMP  ),
};
static Image_format * file_headers_end=
                     file_headers + sizeof(file_headers)/sizeof(*file_headers);

MYX_IMAGE_FORMAT myx_guess_image_format(const void *data, unsigned int length)
{
  if (length > 0)
  {
    for (Image_format * format= file_headers; format!=file_headers_end; format++)
    {
      if (memcmp(format->name.data(), data,
                 min((size_t)length, format->name.length()))==0)
      {
        return format->format;
      };
    };
  };

  return MYX_IMG_UNKNOWN;
}

//----------------------------------------------------------------------------------------------------------------------

//Remember to add 2 to the substring number since there are two () in this string
#define REGEX_IGNORE_LEADING_COMMENT "^(\\s*(/\\*.*\\*/))*\\s*"

static const char *transaction_breakers[]=
{
  REGEX_IGNORE_LEADING_COMMENT "(ALTER\\s+TABLE)",
    REGEX_IGNORE_LEADING_COMMENT "(BEGIN)",
    REGEX_IGNORE_LEADING_COMMENT "(CREATE\\s+INDEX)",
    REGEX_IGNORE_LEADING_COMMENT "(DROP\\s+DATABASE)",
    REGEX_IGNORE_LEADING_COMMENT "(DROP\\s+INDEX)",
    REGEX_IGNORE_LEADING_COMMENT "(DROP\\s+TABLE)",
    REGEX_IGNORE_LEADING_COMMENT "(LOAD\\s+MASTER DATA)",
    REGEX_IGNORE_LEADING_COMMENT "(LOCK\\s+TABLES)",
    REGEX_IGNORE_LEADING_COMMENT "(RENAME\\s+TABLE)", 
    REGEX_IGNORE_LEADING_COMMENT "(SET\\s+AUTOCOMMIT\\s*=\\s*1)",
    REGEX_IGNORE_LEADING_COMMENT "(START\\s+TRANSACTION)",
    REGEX_IGNORE_LEADING_COMMENT "(TRUNCATE)",
    REGEX_IGNORE_LEADING_COMMENT "(UNLOCK\\s+TABLES)",
//    "CREATE TABLE", // (this commits only if before MySQL 4.0.13 and MySQL binary logging is used).
};
static const char **transaction_breakers_end= 
                    transaction_breakers + 
                    sizeof(transaction_breakers)/sizeof(*transaction_breakers);

//----------------------------------------------------------------------------------------------------------------------

int myx_check_whether_commits_transaction(MYSQL *mysql, const char *stmt)
{
  size_t stmt_len= strlen(stmt);
  for (const char **transaction_breaker= transaction_breakers;
       transaction_breaker != transaction_breakers_end; transaction_breaker++)
  {
    char *tmp= get_value_from_text(stmt, (int)stmt_len, *transaction_breaker);
    g_free(tmp);
    if (tmp)
      return 1;
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------


