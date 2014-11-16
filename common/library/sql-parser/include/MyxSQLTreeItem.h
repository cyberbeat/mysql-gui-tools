#ifndef MYX_SQL_TREE_ITEM
#define MYX_SQL_TREE_ITEM

//#include "../source/myx_sql_parser.tab.hh"
//typedef yytokentype TokenName;

#include <stdlib.h>
#include "myx_sql_parser_public_interface.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void *new_simple_tree_item(const void *name, const void *value);
extern void *new_tree_item(const void *name, const void *value, void *list);
extern void delete_tree_item(void *tree_item_list);

extern void *new_tree_item_list();
extern void *new_tree_item_list_reuse(void *tree_item_from);
extern void tree_item_list_add(void *tree_item_list, void *tree_item);
extern void tree_item_list_add_all(void *tree_item_list, void *tree_item);

#ifdef __cplusplus
}
#endif


#ifdef __cplusplus

#include <list>
#include <iostream>
#include <string.h>

//typedef std::list<const char *> ConstCStringList;

// pattern composite
class MyxSQLTreeItem
{
public:

  typedef std::list<MyxSQLTreeItem *> SubItemList;

protected:

  mutable bool _del_list; // not a part of business object
  char *_name;
  char *_value;
  SubItemList* _subitems;

public:

  MyxSQLTreeItem(const char *name, const char *value, SubItemList *items) 
    : _del_list(true), _name(strcpy(new char[strlen(name)+1], name)), _value(strcpy(new char[strlen(value)+1], value)), _subitems(items) 
  {}
  
  virtual ~MyxSQLTreeItem() 
  {
    delete[] _name;
    delete[] _value; 
    if(_del_list)
    {
      delete _subitems;
    }
  }

  virtual bool name_equals(const char *to) const { return strcmp(_name, to) == 0 ? true: false; }

  virtual bool value_equals(const char *to) const { return strcmp(_value, to) == 0 ? true: false; }

  virtual const char *get_name() const { return _name; }
  
  virtual const char *get_value() const { return _value; }
  
  virtual const SubItemList *get_subitems() const { return _subitems; }

  virtual const MyxSQLTreeItem *get_subitem_by_name(const char *name, int position = 0) const;
  virtual const MyxSQLTreeItem *rget_subitem_by_name(const char *name, int position = 0) const; // reverse
  virtual char * get_subitems_as_string(const char *delim = " ") const;
  virtual const MyxSQLTreeItem *get_subitem_by_path(const char *path) const;

  virtual void del_list(bool del_list) const { _del_list= del_list; }

  friend void *new_tree_item_list();
};

class MyxSQLSimpleTreeItem : public MyxSQLTreeItem 
{
  static SubItemList _empty_list;

public:

  MyxSQLSimpleTreeItem(const char *name, const char *value) : MyxSQLTreeItem(name, value, 0) {} 
  
  virtual const SubItemList *get_subitems() const { return &_empty_list; }

  friend void *new_tree_item_list_reuse(void *tree_item_from);
};

MYX_PUBLIC_FUNC std::ostream& operator << (std::ostream&, const MyxSQLTreeItem&);

//////////////////////////////////////////////////////////////////////////////////////////////////
// Helper Classes

// pattern flyweight
class MyxSchemaTreeItemHelperBase
{
protected:
  static bool check1word(MyxSQLTreeItem *tree, const char *w1);
  static bool check2words(MyxSQLTreeItem *tree, const char *w1, const char *w2);
};

class MYX_PUBLIC_FUNC MyxCreateSchemaTreeItem : public MyxSchemaTreeItemHelperBase
{
  MyxSQLTreeItem *_item;

  const char *schema_name;

public:

  static bool check(MyxSQLTreeItem *tree);

  MyxCreateSchemaTreeItem(MyxSQLTreeItem *tree);
  ~MyxCreateSchemaTreeItem();

  const char *get_schema_name() const { return schema_name; } 
};

// pattern flyweight
class MYX_PUBLIC_FUNC MyxCreateTableTreeItem : public MyxSchemaTreeItemHelperBase
{
public:

  class MYX_PUBLIC_FUNC Column {
    
    const MyxSQLTreeItem *_column;
    
    const char *column_name;
    const char *datatype_name;
    const char *default_value;
    bool default_value_null;
    int precision;
    int scale;
    bool nullable;
    int length;
    const char *charset_name;
    const char *collation_name;
    const char *explicit_params;
    int auto_increment;
    const char *comment;
    bool unsigned_flag;
    bool zerofill_flag;
    bool binary_flag;
    bool unicode_flag;
    bool ascii_flag;

  public:

    Column(const MyxSQLTreeItem *column);
    ~Column();

    const char *get_column_name() const { return column_name; }
    const char *get_datatype_name() const { return datatype_name; }
    const char *get_default_value() const { return default_value; }
    bool is_default_value_null() const { return default_value_null; }
    int get_precision() const { return precision; }
    int get_scale() const { return scale; }
    bool is_nullable() const { return nullable; }
    int get_length() const { return length; }
    const char *get_charset_name() const { return charset_name; }
    const char *get_collation_name() const { return collation_name; }
    const char *get_explicit_params() const { return explicit_params; }
    int get_auto_increment() const { return auto_increment; }
    const char *get_comment() const { return comment; }
    bool is_unsigned() const { return unsigned_flag; }
    bool is_zerofill() const { return zerofill_flag; }
    bool is_binary() const { return binary_flag; }
    bool is_ascii() const { return ascii_flag; }
    bool is_unicode() const { return unicode_flag; }
  };

  typedef std::list<Column> ColumnList;

  class MYX_PUBLIC_FUNC IndexColumn {
  
      int length;
      bool descend;
      const Column *ref_column;
      const char *stored_function;
      const char *comment;

  public:

    IndexColumn(const MyxSQLTreeItem *coldef, bool desc, const ColumnList *tablecols);
    ~IndexColumn() {}

    int get_length() const { return length; }
    bool is_descend() const { return descend; }
    const Column *get_ref_column() const { return ref_column; }
    const char *get_stored_function() const { return stored_function; }
    const char *get_comment() const { return comment; }
  };

  typedef std::list<IndexColumn> IndexColumnList;

  class MYX_PUBLIC_FUNC Index {

    const MyxSQLTreeItem *_index;
#pragma warning(push)
#pragma warning(disable: 4251)
    IndexColumnList _columns;
#pragma warning(pop)

    const char *index_name;
    bool primary;
    bool unique;
    const char *index_type;

  public:

    Index(const MyxSQLTreeItem *keydef, const ColumnList *tablecols);
    ~Index() {}

    bool is_primary() const { return primary; }
    bool is_unique() const { return unique; }
    const char *get_index_type() const { return index_type; }
    const char *get_index_name() const { return index_name; }

    const IndexColumnList * get_columns() const { return &_columns; }
  };

  typedef std::list<Index> IndexList;
  typedef std::list<const char *> CStringList;

  class MYX_PUBLIC_FUNC ForeignKey {

    const MyxSQLTreeItem *_fk;
    const char *fk_name;
    const char *delete_rule;
    const char *update_rule;
    const char *ref_schema_name;
    const char *ref_table_name;
#pragma warning(push)
#pragma warning(disable: 4251)
    ColumnList _columns;
    CStringList _ref_columns;
#pragma warning(pop)
  public:
    ForeignKey(const MyxSQLTreeItem *fk, const ColumnList *tablecols);
    ~ForeignKey() {}

    const char *get_fk_name() const { return fk_name; } 
    const char *get_delete_rule() const { return delete_rule; }
    const char *get_update_rule() const { return update_rule; }
    const char *get_ref_schema_name() const { return ref_schema_name; }
    const char *get_ref_table_name() const { return ref_table_name; }
    const ColumnList *get_columns() const { return &_columns; }
    const CStringList *get_ref_columns() const { return &_ref_columns; }
  };

  typedef std::list<ForeignKey> ForeignKeyList;

private:

  MyxSQLTreeItem *_item;
#pragma warning(push)
#pragma warning(disable: 4251)
  ColumnList _columns;
  IndexList _indices;
  ForeignKeyList _fks;
#pragma warning(pop)

  bool temporary;
  const char *schema_name;
  const char *table_name;
  const char *table_engine;
  const char *row_format;
  const char *next_auto_inc;
  const char *def_collation;
  const char *delay_key_write;
  const char *comment;
  const char *def_charset;
  const char *insert_method;
  const char *data_directory;
  const char *index_directory;
  const char *raid_type;
  const char *raid_chunks;
  const char *raid_chunksize;
  const char *pack_keys;
  const char *avg_row_length;
  const char *min_rows;
  const char *max_rows;
  const char *checksum;

public:

  static bool check(MyxSQLTreeItem *tree);

  MyxCreateTableTreeItem(MyxSQLTreeItem *tree);
  ~MyxCreateTableTreeItem();

  const ColumnList * get_columns() const { return &_columns; }
  const IndexList * get_indices() const { return &_indices; }
  const ForeignKeyList * get_fks() const { return &_fks; }

  bool is_temporary() const { return temporary; }
  const char *get_schema_name() const { return schema_name; } 
  const char *get_table_name() const { return table_name; } 
  const char *get_table_engine() const { return table_engine; } 
  const char *get_row_format() const { return row_format; } 
  const char *get_next_auto_inc() const { return next_auto_inc; } 
  const char *get_def_collation() const { return def_collation; }
  const char *get_def_charset() const { return def_charset; }
  const char *get_delay_key_write() const { return delay_key_write; }
  const char *get_comment() const { return comment; }
  const char *get_insert_method() const { return insert_method; }
  
  const char *get_data_directory() const { return data_directory; }
  const char *get_index_directory() const { return index_directory; }
  const char *get_raid_type() const { return raid_type; }
  const char *get_raid_chunks() const { return raid_chunks; }
  const char *get_raid_chunksize() const { return raid_chunksize; }
  const char *get_pack_keys() const { return pack_keys; }
  const char *get_avg_row_length() const { return avg_row_length; }
  const char *get_min_rows() const { return min_rows; }
  const char *get_max_rows() const { return max_rows; }
  const char *get_checksum() const { return checksum; }
};

class MYX_PUBLIC_FUNC MyxCreateRoutineTreeItem : public MyxSchemaTreeItemHelperBase
{
  MyxSQLTreeItem *_item;
  
  const char *schema_name;
  const char *routine_name;
  const char *routine_type;
  const char *return_type;

public:

  class MYX_PUBLIC_FUNC RoutineParam
  {
    const MyxSQLTreeItem *_param;

    const char *param_name;
    const char *param_type;
    const char *param_dir;

  public:

    RoutineParam(const MyxSQLTreeItem *param);
    ~RoutineParam() {}

    const char *get_param_name () const { return param_name; }
    const char *get_param_type () const { return param_type; }
    const char *get_param_dir () const { return param_dir; }
  };

  typedef std::list<RoutineParam> RoutineParamList;

private:

#pragma warning(push)
#pragma warning(disable: 4251)
  RoutineParamList _params;
#pragma warning(pop)

public:

  static bool check(MyxSQLTreeItem *tree);

  MyxCreateRoutineTreeItem(MyxSQLTreeItem *tree);
  ~MyxCreateRoutineTreeItem();

  const char *get_schema_name() const { return schema_name; }
  const char *get_routine_name() const { return routine_name; }
  const char *get_routine_type() const { return routine_type; }
  const char *get_return_type() const { return return_type; }
  const RoutineParamList *get_params() const { return &_params; }
 // const char *get_routine_code() const { return routine_code; }
};

class MYX_PUBLIC_FUNC MyxCreateViewTreeItem : public MyxSchemaTreeItemHelperBase
{
  MyxSQLTreeItem *_item;
  
  const char *view_name;
  const char *schema_name;
  const char *query;
  bool with_check;
  bool read_only;

public:

  static bool check(MyxSQLTreeItem *tree);

  MyxCreateViewTreeItem(MyxSQLTreeItem *tree);
  ~MyxCreateViewTreeItem();

  const char *get_view_name() const { return view_name; }
  const char *get_schema_name() const { return schema_name; }
  const char *get_query() const { return query; }
  bool is_with_check() const { return with_check; }
  bool is_read_only() const { return read_only; }
};

// pattern flyweight
class MYX_PUBLIC_FUNC MyxUseSchemaTreeItem : public MyxSchemaTreeItemHelperBase
{
  MyxSQLTreeItem *_item;

  const char *schema_name;

public:

  static bool check(MyxSQLTreeItem *tree);

  MyxUseSchemaTreeItem(MyxSQLTreeItem *tree);
  ~MyxUseSchemaTreeItem();

  const char *get_schema_name() const { return schema_name; } 
};

#endif // __cplusplus


#endif // MYX_SQL_TREE_ITEM

