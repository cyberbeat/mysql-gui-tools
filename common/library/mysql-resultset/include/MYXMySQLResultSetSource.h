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

#ifndef _MYXMYSQLRESULTSETSOURCE_H_
#define _MYXMYSQLRESULTSETSOURCE_H_

#include "MYXResultSetSource.h"
#include <mysql.h>


class MYXMySQLResultSetSource : public MYXResultSetSource {
    struct ResultColumnData {
      union {
        char *buffer_value;
        long long_value;
        bigint bigint_value;
        double double_value;
#if MYSQL_VERSION_ID >= 50100
        MYSQL_TIME time_value;
#endif
      } data;
      unsigned long buffer_length;
      my_bool is_null;
      unsigned long length;
      my_bool error;
      
      ~ResultColumnData()
      {
        if (buffer_length > 0)
          g_free(data.buffer_value);
      }
    };
    
    MYSQL _mysql;
    
    char *_hostname;
    char *_username;
    char *_password;
    int _port;
    char *_socket_path;

    
    MYSQL_RES *_result;
    MYSQL_ROW _row;
    int _num_columns;
    unsigned long *_row_lengths;
    bool _connected;
    
    std::vector<ColumnInfo*> _columns;

#if MYSQL_VERSION_ID >= 50000
    std::vector<ResultColumnData*> _column_data;
    MYSQL_STMT *_stmt;
    MYSQL_RES *_stmt_metadata;
    MYSQL_BIND *_stmt_results;
    bool _stmt_active;
#endif
    int _last_errno;
    char *_last_error;
    void clear_error();
    void catch_mysql_error();
    bool fetch_stmt_results();
    
    std::vector<ColumnInfo*> get_columns_normal();
    std::vector<ColumnInfo*> get_columns_stmt();
    
  public:
    static MYXMySQLResultSetSource* create() 
    {
      return new MYXMySQLResultSetSource();
    };
    
    MYXMySQLResultSetSource();
    virtual ~MYXMySQLResultSetSource();
    
    virtual bool connect(const char *hostname, int port,
                         const char *username,
                         const char *password,
                         const char *socket_path= NULL);

    virtual void push_schema(const char *schema);
    virtual void pop_schema();

    unsigned long get_server_version();

    virtual std::vector<ColumnInfo*> get_columns();

    virtual bool prepare(const char *query);
    virtual bool execute_prepared(QueryParameter *params, int paramCount);
    virtual bool close_prepared();
    virtual bool supports_prepared();
    
    virtual bool execute(const char *query) { return execute(query, strlen(query)); };
    virtual bool execute(const char *query, size_t length);
    
    virtual void reset();

    virtual int get_error_number();
    virtual const char *get_error_message();
    
    MYSQL *get_mysql() { return &_mysql; };

    virtual bool eof();
    virtual bool next();
    virtual bool get_value(int column, const char *&value, size_t &length);
    virtual bool get_row(const char **values, size_t *lengths);
};

#endif /* _MYXMYSQLRESULTSETSOURCE_H_ */
