#include <MyxSQLTreeItem.h>
#include <myx_public_interface.h>
#include <myx_grt_public_interface.h>
#include <myx_grt_builtin_module_public_interface.h>

#include "myx_grt_mysql.h"
#include "myx_sql_parser_public_interface.h"
#include "myx_grt_mysql_reveng_script_helper.h"

MYX_GRT_VALUE * add_schema(MYX_GRT_VALUE * schemata, HELPER_ARGS * args, 
                          const char *schema_name)
{
  MYX_GRT_VALUE *schema= myx_grt_dict_new_obj(args->grt, "db.mysql.Schema", schema_name, "", myx_grt_dict_item_get_as_string(args->catalog, "_id"));

  myx_grt_list_item_add(schemata, schema);
  myx_grt_value_release(schema);
  return schema;
}

void process_create_schema_statement(MyxSQLTreeItem *tree, HELPER_ARGS * args)
{
  MYX_GRT_VALUE *schemata= myx_grt_dict_item_get_value(args->catalog, "schemata");
  MyxCreateSchemaTreeItem helper(tree);
  add_schema(schemata, args, helper.get_schema_name());
}

MYX_GRT_VALUE * add_table(MYX_GRT_VALUE * schema_tables, HELPER_ARGS * args, MyxCreateTableTreeItem * helper)
{
  MYX_GRT_VALUE *table= myx_grt_dict_new(args->grt, "db.mysql.Table");

  // add table attributes
  if(helper->get_table_name() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "name", helper->get_table_name());
  }
  if(helper->get_table_engine() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "tableEngine", helper->get_table_engine());
  }
  myx_grt_dict_item_set_value_from_int(table, "isTemporary", helper->is_temporary() ? 1 : 0);
  if(args->sql != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "sql", args->sql);
  }
  if(helper->get_row_format() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "rowFormat", helper->get_row_format());
  }
  if(helper->get_next_auto_inc() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "nextAutoInc", helper->get_next_auto_inc());
  }
  if(helper->get_def_collation() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "defaultCollationName", helper->get_def_collation());
  }
  if(helper->get_comment() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "comment", helper->get_comment());
  }
  if(helper->get_delay_key_write() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "delayKeyWrite", helper->get_delay_key_write());
  }
  if(helper->get_def_charset() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "defaultCharacterSetName", helper->get_def_charset());
  }
  if(helper->get_insert_method() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "mergeInsert", helper->get_insert_method());
  }
  if(helper->get_data_directory() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "tableDataDir", helper->get_data_directory());
  }
  if(helper->get_index_directory() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "tableIndexDir", helper->get_index_directory());
  }
  if(helper->get_raid_type() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "raidType", helper->get_raid_type());
  }
  if(helper->get_raid_chunks() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "raidChunks", helper->get_raid_chunks());
  }
  if(helper->get_raid_chunksize() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "raidChunkSize", helper->get_raid_chunksize());
  }
  if(helper->get_pack_keys() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "packKeys", helper->get_pack_keys());
  }
  if(helper->get_avg_row_length() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "avgRowLength", helper->get_avg_row_length());
  }
  if(helper->get_min_rows() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "minRows", helper->get_min_rows());
  }
  if(helper->get_max_rows() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "maxRows", helper->get_max_rows());
  }
  if(helper->get_checksum() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(table, "checksum", helper->get_checksum());
  }
  
  // build datatype cache
  GHashTable *datatype_cache= g_hash_table_new_full(g_str_hash, g_str_equal, NULL, NULL);
  MYX_GRT_VALUE *datatypes= myx_grt_dict_item_get_value(args->catalog, "simpleDatatypes");
  for (unsigned i= 0; i < myx_grt_list_item_count(datatypes); i++)
  {
    MYX_GRT_VALUE *datatype= myx_grt_list_item_get_reference_value(args->grt, datatypes, i);
    g_hash_table_insert(datatype_cache, (char *)myx_grt_dict_item_get_as_string(datatype, "name"), datatype);
  }

  // add columns  
  MYX_GRT_VALUE *datatype= NULL;
  MYX_GRT_VALUE *columns= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.Column");
  myx_grt_dict_item_set_value(table, "columns", columns);
  myx_grt_value_release(columns);

  for(MyxCreateTableTreeItem::ColumnList::const_iterator it = helper->get_columns()->begin(); it != helper->get_columns()->end(); it++)
  {
    const MyxCreateTableTreeItem::Column& c= *it;
    MYX_GRT_VALUE *column= myx_grt_dict_new(args->grt, "db.mysql.Column");
    myx_grt_dict_generate_id(column);

    if(c.get_column_name() != NULL)
    {
      myx_grt_dict_item_set_value_from_string(column, "name", c.get_column_name());
      myx_grt_dict_item_set_value_from_string(column, "oldName", c.get_column_name());
    }
    if(c.get_datatype_name() != NULL)
    {
      myx_grt_dict_item_set_value_from_string(column, "datatypeName", c.get_datatype_name());
      datatype= static_cast<MYX_GRT_VALUE *>(g_hash_table_lookup(datatype_cache, c.get_datatype_name()));
      if(datatype != NULL)
      {
        myx_grt_dict_item_set_value_from_string(column, "simpleType", myx_grt_dict_id_item_as_string(datatype));
      }
    }
    myx_grt_dict_item_set_value_from_int(column, "precision", c.get_precision());
    myx_grt_dict_item_set_value_from_int(column, "scale", c.get_scale());
    myx_grt_dict_item_set_value_from_int(column, "length", c.get_length());
    myx_grt_dict_item_set_value_from_string(column, "datatypeExplicitParams", c.get_explicit_params());
    myx_grt_dict_item_set_value_from_int(column, "unsigned", c.is_unsigned());
    if(c.get_collation_name() != NULL)
    {
      myx_grt_dict_item_set_value_from_string(column, "collationName", c.get_collation_name());
    }
    if(c.get_collation_name() != NULL)
    {
      myx_grt_dict_item_set_value_from_int(column, "isNullable", c.is_nullable());
    }
    if(c.get_default_value() != NULL)
    {
      myx_grt_dict_item_set_value_from_string(column, "defaultValue", c.get_default_value());
    }
    myx_grt_dict_item_set_value_from_int(column, "defaultValueIsNull", c.is_default_value_null());
    myx_grt_dict_item_set_value_from_int(column, "autoIncrement", c.get_auto_increment());
    if(c.get_comment() != NULL)
    {
      myx_grt_dict_item_set_value_from_string(column, "comment", c.get_comment());
    }

    myx_grt_list_item_add(columns, column);
    myx_grt_value_release(column);
  }

  // add indices
  MYX_GRT_VALUE *indices= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.Index");
  myx_grt_dict_item_set_value(table, "indices", indices);
  myx_grt_value_release(indices);
  
  for(MyxCreateTableTreeItem::IndexList::const_iterator it = helper->get_indices()->begin(); it != helper->get_indices()->end(); it++)
  {
    const MyxCreateTableTreeItem::Index& idx= *it;
    MYX_GRT_VALUE *index= myx_grt_dict_new(args->grt, "db.mysql.Index");
    myx_grt_dict_generate_id(index);

    myx_grt_dict_item_set_value_from_string(index, "name", idx.get_index_name());
    myx_grt_dict_item_set_value_from_string(index, "oldName", idx.get_index_name());
    myx_grt_dict_item_set_value_from_string(index, "owner", myx_grt_dict_item_get_as_string(table, "_id"));
    
    //myx_grt_dict_item_set_value_from_string(index, "comment", idx.);
    myx_grt_dict_item_set_value_from_int(index, "unique", idx.is_unique() ? 1 : 0);
    if(idx.is_primary())
    {
      myx_grt_dict_item_set_value_from_string(index, "indexKind", "PRIMARY");
      myx_grt_dict_item_set_value_from_string(table, "primaryKey", myx_grt_dict_item_get_as_string(index, "_id"));
    }
    if(idx.get_index_type() != NULL)
    {
      myx_grt_dict_item_set_value_from_string(index, "indexType", idx.get_index_type());
    }

    // add index columns
    MYX_GRT_VALUE *index_columns= myx_grt_list_new(MYX_DICT_VALUE, "db.IndexColumn");
    myx_grt_dict_item_set_value(index, "columns", index_columns);
    myx_grt_value_release(index_columns);

    for(MyxCreateTableTreeItem::IndexColumnList::const_iterator colit = idx.get_columns()->begin(); colit != idx.get_columns()->end(); colit++)
    {
      const MyxCreateTableTreeItem::IndexColumn& idxcol= *colit;
      MYX_GRT_VALUE *index_column= myx_grt_dict_new(args->grt, "db.mysql.IndexColumn");
      myx_grt_dict_generate_id(index_column);

      myx_grt_dict_item_set_value_from_string(index_column, "name", idxcol.get_ref_column()->get_column_name());
      myx_grt_dict_item_set_value_from_string(index_column, "owner", myx_grt_dict_item_get_as_string(index, "_id"));
      myx_grt_dict_item_set_value_from_int(index_column, "columnLength", idxcol.get_length());
      myx_grt_dict_item_set_value_from_int(index_column, "descend", idxcol.is_descend() ? 0 : 1);

      const char *index_column_name= idxcol.get_ref_column()->get_column_name();

      for (unsigned i= 0; i < myx_grt_list_item_count(columns); i++)
      {
        MYX_GRT_VALUE *column= myx_grt_list_item_get(columns, i);

        if (strcmp2(myx_grt_dict_item_get_as_string(column, "name"), index_column_name) == 0)
        {
          myx_grt_dict_item_set_value_from_string(index_column, "referedColumn", myx_grt_dict_id_item_as_string(column));
          break;
        }
      }

      myx_grt_list_item_add(index_columns, index_column);
      myx_grt_value_release(index_column);
    }

    myx_grt_list_item_add(indices, index);
    myx_grt_value_release(index);
  }

  // add foreign keys
  MYX_GRT_VALUE *fks= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.ForeignKey");
  myx_grt_dict_item_set_value(table, "foreignKeys", fks);
  myx_grt_value_release(fks);
  
  for(MyxCreateTableTreeItem::ForeignKeyList::const_iterator it = helper->get_fks()->begin(); it != helper->get_fks()->end(); it++)
  {
    const MyxCreateTableTreeItem::ForeignKey& fkey= *it;
    MYX_GRT_VALUE *fk= myx_grt_dict_new(args->grt, "db.mysql.ForeignKey");

    MYX_GRT_VALUE *fk_columns= myx_grt_list_new(MYX_STRING_VALUE, "db.mysql.Column");
    myx_grt_dict_item_set_value(fk, "columns", fk_columns);
    myx_grt_value_release(fk_columns);

    MYX_GRT_VALUE *fk_refered_column_names= myx_grt_list_new(MYX_STRING_VALUE, NULL);
    myx_grt_dict_item_set_value(fk, "referedColumnNames", fk_refered_column_names);
    myx_grt_value_release(fk_refered_column_names);

    MYX_GRT_VALUE *fk_refered_columns= myx_grt_list_new(MYX_STRING_VALUE, "db.mysql.Column");
    myx_grt_dict_item_set_value(fk, "referedColumns", fk_refered_columns);
    myx_grt_value_release(fk_refered_columns);

    myx_grt_dict_generate_id(fk);

    if(fkey.get_fk_name() != NULL)
    {
      myx_grt_dict_item_set_value_from_string(fk, "name", fkey.get_fk_name());
      myx_grt_dict_item_set_value_from_string(fk, "oldName", fkey.get_fk_name());    
    }

    if(fkey.get_ref_schema_name() != NULL)
    {
      myx_grt_dict_item_set_value_from_string(fk, "referedTableSchemaName", fkey.get_ref_schema_name());
    }

    if(fkey.get_ref_table_name())
    {
      myx_grt_dict_item_set_value_from_string(fk, "referedTableName", fkey.get_ref_table_name());
    }

    if(fkey.get_delete_rule() != NULL)
    {
      myx_grt_dict_item_set_value_from_string(fk, "deleteRule", fkey.get_delete_rule());
    }
    if(fkey.get_update_rule() != NULL)
    {
      myx_grt_dict_item_set_value_from_string(fk, "updateRule", fkey.get_update_rule());
    }

    for(MyxCreateTableTreeItem::CStringList::const_iterator it= fkey.get_ref_columns()->begin(); it != fkey.get_ref_columns()->end(); it++)
    {
      myx_grt_list_item_add_as_string(fk_refered_column_names, *it);
    }

    for(MyxCreateTableTreeItem::ColumnList::const_iterator it= fkey.get_columns()->begin(); it != fkey.get_columns()->end(); it++)
    {
      const MyxCreateTableTreeItem::Column& c= *it;
      
      for (unsigned j= 0; j < myx_grt_list_item_count(columns); j++)
      {
        MYX_GRT_VALUE *table_column= myx_grt_list_item_get(columns, j);
        if (strcmp2(myx_grt_dict_item_get_as_string(table_column, "name"), c.get_column_name()) == 0)
        {
          myx_grt_list_item_add_as_string(fk_columns, myx_grt_dict_item_get_as_string(table_column, "_id"));
          break;
        }
      }
    }

    myx_grt_list_item_add(fks, fk);
    myx_grt_value_release(fk);
  }

  myx_grt_list_item_add(schema_tables, table);
  myx_grt_value_release(table);
  return table;
}

MYX_GRT_VALUE * add_routine(MYX_GRT_VALUE * schema_routines, HELPER_ARGS * args, MyxCreateRoutineTreeItem * helper)
{
  MYX_GRT_VALUE *routine= myx_grt_dict_new(args->grt, "db.mysql.Routine");
  myx_grt_dict_generate_id(routine);

  if(helper->get_routine_name() != NULL)
  {
    myx_grt_dict_item_set_value_from_string(routine, "name", helper->get_routine_name());
    myx_grt_dict_item_set_value_from_string(routine, "oldName", helper->get_routine_name());
  }

  myx_grt_dict_item_set_value_from_string(routine, "owner", myx_grt_dict_item_get_as_string(args->used_schema, "_id"));
  myx_grt_dict_item_set_value_from_string(routine, "routineType", helper->get_routine_type());
  
  if(strcmp2(helper->get_routine_type(), "function") == 0)
  {
    myx_grt_dict_item_set_value_from_string(routine, "returnDatatype", helper->get_return_type());
  }
  
  myx_grt_dict_item_set_value_from_string(routine, "routineCode", args->sql);

  MYX_GRT_VALUE *sp_params= myx_grt_list_new(MYX_DICT_VALUE, "db.mysql.RoutineParam");

  myx_grt_dict_item_set_value(routine, "params", sp_params);
  myx_grt_value_release(sp_params);

  for(MyxCreateRoutineTreeItem::RoutineParamList::const_iterator it= helper->get_params()->begin(); it != helper->get_params()->end(); it++)
  {
    const MyxCreateRoutineTreeItem::RoutineParam& p= *it;
    
    MYX_GRT_VALUE *sp_param= myx_grt_dict_new(args->grt, "db.mysql.RoutineParam");
    myx_grt_dict_generate_id(sp_param);    

    myx_grt_dict_item_set_value_from_string(sp_param, "name", p.get_param_name());
    myx_grt_dict_item_set_value_from_string(sp_param, "owner", myx_grt_dict_item_get_as_string(routine, "_id"));
    myx_grt_dict_item_set_value_from_string(sp_param, "datatype", p.get_param_type());
    myx_grt_dict_item_set_value_from_string(sp_param, "paramType", p.get_param_dir());  // in/out/input

    myx_grt_list_item_add(sp_params, sp_param);
    myx_grt_value_release(sp_param);
  }

  myx_grt_list_item_add(schema_routines, routine);
  myx_grt_value_release(routine);

  return routine;
}

MYX_GRT_VALUE * add_view(MYX_GRT_VALUE * schema_views, HELPER_ARGS * args, MyxCreateViewTreeItem * helper)
{
  MYX_GRT_VALUE *view= myx_grt_dict_new(args->grt, "db.mysql.View");
  myx_grt_dict_generate_id(view);
  myx_grt_dict_item_set_value_from_string(view, "name", helper->get_view_name());
  myx_grt_dict_item_set_value_from_string(view, "oldName", helper->get_view_name());
  myx_grt_dict_item_set_value_from_string(view, "owner", myx_grt_dict_item_get_as_string(args->used_schema, "_id"));

  myx_grt_dict_item_set_value_from_string(view, "queryExpression", args->sql);
  myx_grt_dict_item_set_value_from_int(view, "withCheckCondition", helper->is_with_check() ? 1 : 0);  

  myx_grt_list_item_add(schema_views, view);
  myx_grt_value_release(view);

  return view;
}

MYX_GRT_VALUE * set_used_schema(HELPER_ARGS * args, const char *schema_name)
{
  MYX_GRT_VALUE *schemata= myx_grt_dict_item_get_value(args->catalog, "schemata");
  MYX_GRT_VALUE *schema= myx_grt_list_item_get_by_object_name(schemata, schema_name);
  if(schema == NULL)
  {
    schema= add_schema(schemata, args, schema_name);
  }
  args->used_schema= schema;
  return schema;
}

void process_create_table_statement(MyxSQLTreeItem *tree, HELPER_ARGS * args)
{
  MYX_GRT_VALUE *prev_schema= args->used_schema;
  MyxCreateTableTreeItem helper(tree);

  if(helper.get_schema_name() != NULL)
  {
    set_used_schema(args, helper.get_schema_name());  // this will add schema to schemata if nesessary
  }
  
  MYX_GRT_VALUE *schema_tables= myx_grt_dict_item_get_value(args->used_schema, "tables");
  add_table(schema_tables, args, &helper);

  args->used_schema= prev_schema;
}

void process_create_routine_statement(MyxSQLTreeItem *tree, HELPER_ARGS * args)
{
  MYX_GRT_VALUE *prev_schema= args->used_schema;
  MyxCreateRoutineTreeItem helper(tree);

  if(helper.get_schema_name() != NULL)
  {
    set_used_schema(args, helper.get_schema_name());  // this will add schema to schemata if nesessary
  }
  
  MYX_GRT_VALUE *schema_routines= myx_grt_dict_item_get_value(args->used_schema, "routines");
  add_routine(schema_routines, args, &helper);

  args->used_schema= prev_schema;
}

void process_create_view_statement(MyxSQLTreeItem *tree, HELPER_ARGS * args)
{
  MYX_GRT_VALUE *prev_schema= args->used_schema;
  MyxCreateViewTreeItem helper(tree);

  if(helper.get_schema_name() != NULL)
  {
    set_used_schema(args, helper.get_schema_name());  // this will add schema to schemata if nesessary
  }
  
  MYX_GRT_VALUE *schema_views= myx_grt_dict_item_get_value(args->used_schema, "views");
  add_view(schema_views, args, &helper);

  args->used_schema= prev_schema;
}

void process_use_statement(MyxSQLTreeItem *tree, HELPER_ARGS * args)
{
  MyxUseSchemaTreeItem helper(tree);
  set_used_schema(args, helper.get_schema_name()); 
}

extern "C" void convert_parser_dom_to_grt_dom(void *parser_dom_tree, HELPER_ARGS * args)
{
  //MYX_GRT_VALUE * catalog= args->catalog;
  //MYX_GRT * grt= args->grt;
  
  MyxSQLTreeItem *tree = static_cast<MyxSQLTreeItem *>(parser_dom_tree);

  if(MyxCreateSchemaTreeItem::check(tree)) 
  {
    process_create_schema_statement(tree, args);
  }
  else if(MyxCreateTableTreeItem::check(tree)) 
  {
    process_create_table_statement(tree, args);
  }
  else if(MyxCreateRoutineTreeItem::check(tree)) 
  {
    process_create_routine_statement(tree, args);
  }
  else if(MyxCreateViewTreeItem::check(tree)) 
  {
    process_create_view_statement(tree, args);
  }
  else if(MyxUseSchemaTreeItem::check(tree)) 
  {
    process_use_statement(tree, args);
  }
}

