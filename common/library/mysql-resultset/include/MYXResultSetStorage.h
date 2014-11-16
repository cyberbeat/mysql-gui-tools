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


#ifndef _MYXRESULTSETSTORAGE_H_
#define _MYXRESULTSETSTORAGE_H_

#include "myx_util_public_interface.h"
#include <list>
#include <map>
#include <vector>

#include "MYXResultSetSource.h"

enum MYXRSEditStatus {
  MES_UNCHANGED,
  MES_CHANGED,
  MES_PLACE_HOLDER,
  MES_ADDED,
  MES_DELETED
};

enum MYXRSCompareStatus {
  MCS_MATCH,
  MCS_DIFFERS,
  MCS_ONLY_IN_THIS,
  MCS_ONLY_IN_OTHER
};


class MYXResultSetStorage;

struct MYXResultSetCallbacks {
  void (*row_added)(MYXResultSetStorage *rs, bigint row);
  void (*row_deleted)(MYXResultSetStorage *rs, bigint row);
  void (*row_changed)(MYXResultSetStorage *rs, bigint row);
};


struct MYXRowBlock;


struct MYXField {
  char *value;
  unsigned int length;
};

struct MYXRow {
  MYXField *fields;
};


class MYXResultSetCommitResult;


class MYXResultSetStorage {
  public:
    enum ChangeType {
      CH_ADD,
        CH_EDIT,
        CH_DELETE
    };
    
    enum ChangeStatus {
      ST_PENDING,
        ST_APPLIED,
        ST_FAILED,
        ST_DISCARDED
    };

    struct FieldChange {
      int column;
      MYXField new_value;
      ChangeType type;
      ChangeStatus status;

      struct FieldChange *next_field; // ordered by column

      struct FieldChange *next_change;
      struct FieldChange *prev_change;
    };


  protected:
    struct RowChange {
      bigint row;
      std::vector<MYXField*> fields;
      ChangeType type;
      MYXRow *original_row;
    };

  protected:
    bigint _starting_row;
    bigint _row_count;

    bool _fetch_complete;
    
    unsigned int _row_page_size;

    std::vector<MYXResultSetSource::ColumnInfo*> _columns;

    unsigned int _row_block_size;
    std::vector<MYXRowBlock*> _row_blocks;

    std::map<bigint,FieldChange*> _changes;
    bigint _new_rows;

    FieldChange *_last_change; // top of undo stack
    FieldChange *_undo_position;

    bigint _current_row_index;
    
    MYXResultSetSource *_source;
    
  protected:

    MYXResultSetCallbacks _callbacks;


    bool _placeholder;

    MYXRow *get_row(bigint row) const;
    

    virtual void reset_resultset();
    virtual bool add_result_row(const char **fields, size_t *lengths);

    std::list<RowChange> prepare_commit();
    void finalize_commit(MYXResultSetCommitResult *result);

  protected:

    FieldChange* find_change(bigint row, int column) const;
    
    virtual char *prepare_pk_condition(char *buffer, RowChange &row);
    virtual size_t prepare_value(char *buffer, MYXField *field, int column)= 0;

    virtual bool start_update();
    virtual bool db_insert_row(RowChange &row, MYXResultSetCommitResult *result)= 0;
    virtual bool db_update_row(RowChange &row, MYXResultSetCommitResult *result)= 0;
    virtual bool db_delete_row(RowChange &row, MYXResultSetCommitResult *result)= 0;
    virtual bool end_update();

  public:
    MYXResultSetStorage(MYXResultSetSource *dataSource);
    virtual ~MYXResultSetStorage();

    void set_callbacks(const MYXResultSetCallbacks &cbacks);

    void undo_edit();
    void redo_edit();
    
    // data access
    virtual bigint get_row_count() const;
    virtual bigint get_current_row() const;
    virtual bool get(bigint row, int column, const char *&value, size_t &size) const;

    virtual bool get(int column, const char *&value, size_t &size) const;

    virtual bool is_row_editable(bigint row) const;

    virtual int get_column_count();
    std::vector<MYXResultSetSource::ColumnInfo*> get_columns() { return _columns; };
    virtual MYXRSColumnType get_column_type(int column);
   

    // paging/browsing
    virtual bool fetch_all();
    
    void set_page_size(unsigned int size);
    unsigned int get_page_size() { return _row_page_size; };
    virtual void set_page_start(bigint row);
    virtual unsigned int fetch_next_page(); // returns # of rows fetched

    virtual bool move_first();
    virtual bool next_row(bool auto_fetch= true);
    virtual bool prior_row();
    virtual bool move_last();
    virtual bool seek_row(bigint row);

    void set_placeholder_enabled(bool flag);

    // comparing

    bool compare(MYXResultSetStorage *rset);
    MYXRSCompareStatus get_compare_status(bigint row, int column);


    // resultset modification
    virtual bool is_editable();

    bool row_has_changes(bigint row);

    virtual bigint add_row();
    virtual void delete_row(bigint row);

    virtual bool set(bigint row, int column, const char *value, size_t size);
    virtual bool set(bigint row, int column, const char *value);

    virtual MYXResultSetCommitResult *commit_changes();
    virtual void discard_changes();

    virtual MYXRSEditStatus get_edit_status(bigint row, int column) const;
    
    virtual bool has_changes();
    


};




class MYXResultSetCommitResult {
    friend class MYXResultSetStorage;
  public:
    enum Status {
      Success,
      EOutOfMemory
    };

  private:

    struct InfoRecord {
      bigint row;
      MYXResultSetStorage::ChangeType action;
      int type;
      char *message;
      int code;
      
      ~InfoRecord() { g_free(message); }
    };

    std::list<InfoRecord*> _info;
    Status _status;

  public:
    MYXResultSetCommitResult();
    ~MYXResultSetCommitResult();
    
    void add_error(bigint row, MYXResultSetStorage::ChangeType action, const char *message, int code);
    void add_warning(bigint row, MYXResultSetStorage::ChangeType action, const char *message, int code);
    void add_note(bigint row, MYXResultSetStorage::ChangeType action, const char *message, int code);
    
    void note_row_deletion(bigint row);

    bool has_warnings();
    bool has_errors();
    
    void dump();
    
    void out_of_memory();
};




#endif /* _MYXRESULTSETSTORAGE_H_ */
