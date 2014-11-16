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

#ifndef _MYXRESULTSETSOURCE_H_
#define _MYXRESULTSETSOURCE_H_

#include "myx_util_public_interface.h"
#include <vector>
#include <list>

enum MYXRSColumnType {
  MYX_RSCT_INTEGER= 0,
    MYX_RSCT_BIGINT,
    MYX_RSCT_DOUBLE,
    MYX_RSCT_STRING,
    MYX_RSCT_DATE,
    MYX_RSCT_TIME,
    MYX_RSCT_DATETIME,
    MYX_RSCT_BLOB,
    MYX_RSCT_TEXT,
    MYX_RSCT_ENUM,
    MYX_RSCT_SET,
    MYX_RSCT_DECIMAL
};


class MYXResultSetStorage;

class MYXResultSetSource {
  public:
    struct ColumnInfo {
      char *catalog;
      char *schema;
      char *table;
      char *name;
      int length;
      int display_length;
      int display_decimals;
      MYXRSColumnType type;
      char *type_name;
      unsigned int is_pk:1;
      unsigned int not_null:1;
      unsigned int is_autoincrement:1;
      unsigned int is_blob:1;
      
      ~ColumnInfo() {
        g_free(catalog);
        g_free(schema);
        g_free(table);
        g_free(name);
        g_free(type_name);
      };
    };
    
    struct TimeValue {
      int year, month, day;
      int hour, minute, second;
      unsigned long second_frac;
    };
    
    struct QueryParameter {
      MYXRSColumnType type;
      char *value;
      size_t value_length;
      int value_int;
      double value_double;
      bigint value_bigint;
      TimeValue value_time;
      MYXResultSetStorage *master;
      char *master_column;
      bool is_null;
    };

  protected:
    char *_query;

    std::list<char*> _schema_stack;
    
  public:
    virtual ~MYXResultSetSource() {};

    virtual bool prepare(const char *query)= 0;
    virtual bool execute_prepared(QueryParameter *params, int paramCount)= 0;
    virtual bool close_prepared()= 0;
    virtual bool supports_prepared()= 0;

    virtual bool execute(const char *query) { return execute(query, strlen(query)); };
    virtual bool execute(const char *query, size_t length)= 0;

    virtual void push_schema(const char *schema)= 0;
    virtual void pop_schema()= 0;

    virtual int get_error_number()= 0;
    virtual const char *get_error_message()= 0;
    
    virtual bool next()= 0;
    virtual bool get_value(int column, const char *&value, size_t &length)= 0;
    virtual bool get_row(const char **values, size_t *lengths)= 0;
    virtual void reset()= 0;

    virtual std::vector<ColumnInfo*> get_columns()= 0;

    virtual bool eof()= 0;
    virtual unsigned long get_server_version()= 0;
};


#endif /* _MYXRESULTSETSOURCE_H_ */
