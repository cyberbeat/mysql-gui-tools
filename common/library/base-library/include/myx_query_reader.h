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

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <winsock2.h>
#endif

#include "mysql.h"
#include "myx_const_string.h"

#ifdef __cplusplus
extern "C" {   
#endif /* __cplusplus */

class Row_field;

///////////////////////////////////////////////////////////////////////////////
class Query_reader
{
 public:
  Query_reader(MYSQL * mysql, const char * query);
  ~Query_reader();

  bool read  ();

  size_t get_num_rows() {return (size_t)mysql_num_rows(res);}
  
 protected:
  friend class Row_field;
  const char  * query;
  MYSQL       * mysql;
  MYSQL_RES   * res;
  MYSQL_ROW     row;
  MYSQL_FIELD * fields;
  Row_field   * first, * last;

  bool start();
};
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
class Row_field
{
 public:
  Row_field()
  {
    i_field= (size_t)-1;
  }

  void bind(Query_reader * reader, const const_string & column_name)
  {
    this->column_name= column_name;
    this->reader= reader;
    this->next= 0;
    if (reader->last)
    {
      reader->last->next= this;
    }
    else
    {
      reader->first= this;
    }
    reader->last= this;
  }
  MYSQL_FIELD * get_field()
    { return i_field==(size_t)-1 ? 0 : reader->fields + i_field; }
  char * get_value() { return i_field==(size_t)-1 ? 0 : reader->row[i_field]; }
  const const_string  & get_name  () const { return column_name; }
  bool is_valid() const { return reader->row[i_field]!=0 && i_field!=(size_t)-1; }
 protected:
  friend class Query_reader;
  Query_reader  * reader;
  const_string    column_name;
  size_t          i_field;
  Row_field     * next;
};
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
#define DECLARE_QUERY_FORMAT(query_format_name,columns_and_constructor_begin)\
struct query_format_name : public Query_reader\
{\
  query_format_name(MYSQL * mysql, const char * query)\
    : Query_reader(mysql,query){bind();}\
  columns_and_constructor_begin\
  }\
};

///////////////////////////////////////////////////////////////////////////////
#define DECLARE_QUERY_RESULT_COLUMN_NAME(col, name, other_columns)\
  Row_field col;\
  other_columns\
  col.bind(this,name);

///////////////////////////////////////////////////////////////////////////////
#define DECLARE_QUERY_RESULT_COLUMN(col_name, other_columns)\
  DECLARE_QUERY_RESULT_COLUMN_NAME(col_name,\
                                   const_string(#col_name,\
                                                sizeof(#col_name)-1),\
                                   other_columns)

///////////////////////////////////////////////////////////////////////////////
#define DECLARE_QUERY_FORMAT_BIND()\
  void bind()\
  {\

#ifdef __cplusplus
}
#endif /* __cplusplus */
