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


#ifndef _MYXMYSQLRESULTSETSTORAGE_H_
#define _MYXMYSQLRESULTSETSTORAGE_H_

#include "MYXResultSetStorage.h"
#include "MYXMySQLResultSetSource.h"

class MYXMySQLResultSetStorage : public MYXResultSetStorage {
    
    bool _is_mysql5;
    bool _is_mysql41;
    
    size_t _buffer_size;
    char *_buffer;
    
    size_t min_buffer_size_for_row(RowChange &row);
    void gather_errors(MYXResultSetCommitResult *result, RowChange &row);
    
    virtual size_t prepare_value(char *buffer, MYXField *field, int column);

  protected:
    virtual bool db_insert_row(RowChange &row, MYXResultSetCommitResult *result);
    virtual bool db_update_row(RowChange &row, MYXResultSetCommitResult *result);
    virtual bool db_delete_row(RowChange &row, MYXResultSetCommitResult *result);

  public:
    MYXMySQLResultSetStorage(MYXMySQLResultSetSource *dataSource);
    
    static MYXMySQLResultSetStorage *create(MYXMySQLResultSetSource *source)
    {
      return new MYXMySQLResultSetStorage(source);
    }
};


#endif /* _MYXMYSQLRESULTSETSTORAGE_H_ */
