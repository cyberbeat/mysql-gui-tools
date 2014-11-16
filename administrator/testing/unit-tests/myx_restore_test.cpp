
// This file contains common TUT test cases for MySQL Administrator

//----------------------------------------------------------------------------------------------------------------------

#include "test.h"
#include "myx_admin_library.h"
#include "myx_shared_util_functions.h"
#include "myx_util_public_interface.h"

// Private test data.
BEGIN_TEST_DATA_CLASS(module3_restore)
protected:
  Test_connection* connection;
  MYX_BACKUP_CONTENT *restore_content;
  MYX_BACKUP_CONTENT *backup_content;
END_TEST_DATA_CLASS

TEST_MODULE(module3_restore, "MySQL Administrator test suite");

//----------------------------------------------------------------------------------------------------------------------

void fill_table_entry(MYX_BACKUP_TABLE& table, char* name, int flags, char* schema)
{
  table.flags= flags;
  table.catalog= g_strdup("def");
  table.schema= g_strdup(schema);
  table.table= g_strdup(name);
}

//----------------------------------------------------------------------------------------------------------------------

/*
  Prepares all needed data structures etc. for the tests in this module (including a restore context).
*/

TEST_FUNCTION(5)
{
  // Create a restore content structure which is used for the restore test code.
  restore_content= (MYX_BACKUP_CONTENT*) g_malloc0(sizeof(MYX_BACKUP_CONTENT));
  restore_content->tables_num= 12; // 3 tables, 3 views, 2 stored procedures, 2 stored functions and 2 triggers
  restore_content->tables= (MYX_BACKUP_TABLE*) g_malloc0(sizeof(MYX_BACKUP_TABLE) * restore_content->tables_num);

  fill_table_entry(restore_content->tables[0], "get_customer_balance", MYX_BTF_IS_FUNCTION, "sakila");
  fill_table_entry(restore_content->tables[1], "inventory_in_stock", MYX_BTF_IS_FUNCTION, "sakila");
  fill_table_entry(restore_content->tables[2], "ins_film", MYX_BTF_IS_TRIGGER, "sakila");

  fill_table_entry(restore_content->tables[3], "actor", MYX_BTF_IS_TABLE, "sakila");
  fill_table_entry(restore_content->tables[4], "film", MYX_BTF_IS_TABLE, "sakila");
  fill_table_entry(restore_content->tables[5], "sales_by_store", MYX_BTF_IS_VIEW, "sakila");
  fill_table_entry(restore_content->tables[6], "film_in_stock", MYX_BTF_IS_PROCEDURE, "sakila");
  fill_table_entry(restore_content->tables[7], "film_not_in_stock", MYX_BTF_IS_PROCEDURE, "sakila");
  fill_table_entry(restore_content->tables[8], "city", MYX_BTF_IS_TABLE, "sakila");
  fill_table_entry(restore_content->tables[9], "film_list", MYX_BTF_IS_VIEW, "sakila");
  fill_table_entry(restore_content->tables[10], "actor_info", MYX_BTF_IS_VIEW, "sakila");
  fill_table_entry(restore_content->tables[11], "del_film", MYX_BTF_IS_TRIGGER, "sakila");

  connection= test_group_singleton.get_connection();

  ensure("Server connection", connection != NULL);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Call back function for restore processes. Since it is only called for warnings it will always
 * cause the current test to fail.
 */
void report_warning_restore(const char *msg, void *user_data)
{
  //fail("Warning in restore callback: " + std::string(msg));
  std::cout << "Server returned this error: " << msg << "\n\r" ;
}                                                              

//----------------------------------------------------------------------------------------------------------------------

int progress_report_restore(bigint bytes_read, bigint bytes_total, void *user_data)
{
  std::cout << "Restore status: " << bytes_read << " of " << bytes_total << '\r';

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

int progress_report_backup(const char* curr_tbl_name, int num_tables, int num_tables_processed, int num_rows,
  int num_rows_processed, void *user_data)
{
  std::cout << "Current object: " << curr_tbl_name << " (" << num_tables_processed << " of " << num_tables << ", "
    << num_rows << " rows)"
    << "                                                  \r";

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(10)
{
  MYX_SQL_PARSE_ENVIRONMENT environment;

  environment.state= 22;
  environment.bytes_read= 33;
  environment.chars_read= 44;

  myx_init_sql_parse_environment(&environment);
  ensure("Initializing parse environment", environment.state == 1);
  ensure("Initializing parse environment", environment.bytes_read == 0);
  ensure("Initializing parse environment", environment.chars_read == 0);

  MYX_BCS_STATUS status;
  memset(&status, 0, sizeof(status));

  int result;
  result= prepare_line_is_create_statement(&status.re_create_table, &status.pe_create_table);
  ensure("Preparing RE structures", result == 0);
  ensure("Preparing RE structures", status.re_create_table != NULL);
  free_pcre_data(status.re_create_table, status.pe_create_table);

  result= prepare_line_is_create_index_statement(&status.re_create_index, &status.pe_create_index);
  ensure("Preparing RE structures", result == 0);
  ensure("Preparing RE structures", status.re_create_index != NULL);
  free_pcre_data(status.re_create_index, status.pe_create_index);

  result= prepare_line_is_create_view_statement(&status.re_create_view, &status.pe_create_view);
  ensure("Preparing RE structures", result == 0);
  ensure("Preparing RE structures", status.re_create_view != NULL);
  free_pcre_data(status.re_create_view, status.pe_create_view);

  result= prepare_line_is_create_proc_statement(&status.re_create_proc, &status.pe_create_proc);
  ensure("Preparing RE structures", result == 0);
  ensure("Preparing RE structures", status.re_create_proc != NULL);
  free_pcre_data(status.re_create_proc, status.pe_create_proc);

  result= prepare_line_is_create_func_statement(&status.re_create_func, &status.pe_create_func);
  ensure("Preparing RE structures", result == 0);
  ensure("Preparing RE structures", status.re_create_func != NULL);
  free_pcre_data(status.re_create_func, status.pe_create_func);

  result= prepare_line_is_db_use_statement(&status.re_db_use, &status.pe_db_use);
  ensure("Preparing RE structures", result == 0);
  ensure("Preparing RE structures", status.re_db_use != NULL);
  free_pcre_data(status.re_db_use, status.pe_db_use);
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(15)
{
  MYX_RBS_STATUS* restore_status;
  MYX_BACKUP_ERROR error= MYX_BACKUP_UNKNOWN;

  restore_status= myx_new_rbs_status(connection->get_mysql(), "invalid.sql", "utf-8", restore_content,
    "def", NULL, 0, &error, report_warning_restore, NULL);
  ensure("Creating restore status with invalid file name", restore_status == NULL);
  ensure("Creating restore status with invalid file name", error == MYX_BACKUP_CANT_OPEN_FILE);

  std::string source_filename(test_group_singleton.get_full_filename("backup_test_16.sql"));
  restore_status= myx_new_rbs_status(connection->get_mysql(), source_filename.c_str(), "utf-8", restore_content,
    "def", NULL, 0, &error, report_warning_restore, NULL);
  ensure("Creating restore status", restore_status != NULL);
  ensure("Creating restore status", restore_status->intl_file != NULL);
  ensure("Creating restore status", restore_status->buffer != NULL);
  ensure("Creating restore status", restore_status->buffer_len >= 1024 * 1024);
  ensure("Creating restore status", restore_status->re_create_table != NULL);
  ensure("Creating restore status", restore_status->re_create_index != NULL);
  ensure("Creating restore status", restore_status->re_create_view != NULL);
  ensure("Creating restore status", restore_status->re_create_proc != NULL);
  ensure("Creating restore status", restore_status->re_create_func != NULL);
  ensure("Creating restore status", restore_status->re_db_use != NULL);
  ensure("Creating restore status", restore_status->mysql == connection->get_mysql());
  ensure("Creating restore status", strcmp(restore_status->target_catalog, "def") == 0);
  ensure("Creating restore status", restore_status->target_schema == NULL);
  ensure("Creating restore status", restore_status->backup_content == restore_content);
  ensure("Creating restore status", strcmp(restore_status->current_schema_in_sqlfile, "DEFAULT_SCHEMA") == 0);
  ensure("Creating restore status", strcmp(restore_status->current_catalog_in_sqlfile, "DEFAULT_CATALOG") == 0);
  ensure("Creating restore status", strcmp(restore_status->current_schema_in_db, " ") == 0);
  ensure("Creating restore status", restore_status->options == 0);
  ensure("Creating restore status", restore_status->report_warning == report_warning_restore);
  ensure("Creating restore status", restore_status->report_warning_data == NULL);
  ensure("Creating restore status", restore_status->re_drop_table != NULL);
  ensure("Creating restore status", restore_status->re_drop_view != NULL);
  ensure("Creating restore status", restore_status->re_drop_proc != NULL);
  ensure("Creating restore status", restore_status->re_drop_func != NULL);
  ensure("Creating restore status", restore_status->re_drop_database != NULL);
  ensure("Creating restore status", restore_status->re_insert != NULL);
  ensure("Creating restore status", restore_status->re_alter != NULL);
  ensure("Creating restore status", restore_status->re_lock != NULL);
  ensure("Creating restore status", restore_status->re_unlock != NULL);
  ensure("Creating restore status", restore_status->re_charset_set1 != NULL);
  ensure("Creating restore status", restore_status->re_charset_set2 != NULL);
  ensure("Creating restore status", restore_status->re_charset_set3 != NULL);
  ensure("Creating restore status", restore_status->re_charset_set4 != NULL);
  ensure("Creating restore status", restore_status->re_charset_set5 != NULL);
  ensure("Creating restore status", restore_status->re_set != NULL);
  ensure("Creating restore status", restore_status->re_create_database != NULL);
  ensure("Creating restore status", restore_status->re_delimiter != NULL);

  myx_free_rbs_status(restore_status);
}

//----------------------------------------------------------------------------------------------------------------------

typedef enum tagREtype
{
  RE_UNKNOWN,            // 0
  RE_CREATE_TABLE,
  RE_CREATE_INDEX,
  RE_CREATE_VIEW,
  RE_CREATE_PROCEDURE,
  RE_CREATE_FUNCTION,
  RE_CREATE_TRIGGER,
  RE_CREATE_DATABASE,
  RE_USE,
  RE_DROP_TABLE,
  RE_DROP_VIEW,          // 10
  RE_DROP_PROCEDURE,
  RE_DROP_FUNCTION,
  RE_DROP_TRIGGER,
  RE_DROP_INDEX,
  RE_DROP_SCHEMA,
  RE_INSERT,
  RE_ALTER,
  RE_LOCK,
  RE_UNLOCK,
  RE_CHARACTER_SET1,     // 20
  RE_CHARACTER_SET2,
  RE_CHARACTER_SET3,
  RE_CHARACTER_SET4,
  RE_CHARACTER_SET5,
  RE_SET,
  RE_DELIMITER,
  RE_COMMIT              // 27
} RE_TYPE;

void check_RE_with_statement(MYX_RBS_STATUS* status, char* sql, int sql_length, RE_TYPE& type, std::string& name)
{
  type= RE_UNKNOWN;                                
  name= "";

  char* entry= check_statement(sql, sql_length, status->re_create_table, NULL, 10, 2);
  if (entry != NULL)
  {
    type= RE_CREATE_TABLE;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_create_index, NULL, 10, 2);
  if (entry != NULL)
  {
    type= RE_CREATE_INDEX;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_create_view, NULL, 10, 4);
  if (entry != NULL)
  {
    type= RE_CREATE_VIEW;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_create_proc, NULL, 5, 2);
  if (entry != NULL)
  {
    type= RE_CREATE_PROCEDURE;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_create_func, NULL, 5, 2);
  if (entry != NULL)
  {
    type= RE_CREATE_FUNCTION;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_create_trigger, NULL, 10, 4);
  if (entry != NULL)
  {
    type= RE_CREATE_TRIGGER;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_create_database, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_CREATE_DATABASE;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_db_use, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_USE;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_drop_table, NULL, 10, 2);
  if (entry != NULL)
  {
    type= RE_DROP_TABLE;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_drop_view, NULL, 10, 2);
  if (entry != NULL)
  {
    type= RE_DROP_VIEW;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_drop_proc, NULL, 10, 2);
  if (entry != NULL)
  {
    type= RE_DROP_PROCEDURE;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_drop_func, NULL, 10, 2);
  if (entry != NULL)
  {
    type= RE_DROP_FUNCTION;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_drop_trigger, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_DROP_TRIGGER;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_drop_index, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_DROP_INDEX;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_drop_database, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_DROP_SCHEMA;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_insert, NULL, 10, 2);
  if (entry != NULL)
  {
    type= RE_INSERT;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_alter, NULL, 10, 2);
  if (entry != NULL)
  {
    type= RE_ALTER;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_lock, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_LOCK;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_unlock, NULL, 10, 0);
  if (entry != NULL)
  {
    type= RE_UNLOCK;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_charset_set1, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_CHARACTER_SET1;
    name= '-';
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_charset_set2, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_CHARACTER_SET2;
    name= '-';
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_charset_set3, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_CHARACTER_SET3;
    name= '-';
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_charset_set4, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_CHARACTER_SET4;
    name= '-';
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_charset_set5, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_CHARACTER_SET5;
    name= '-';
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_set, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_SET;
    name= '-';
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_delimiter, NULL, 10, 1);
  if (entry != NULL)
  {
    type= RE_DELIMITER;
    name= entry;
    g_free(entry);

    return;
  };

  entry= check_statement(sql, sql_length, status->re_commit, NULL, 10, 0);
  if (entry != NULL)
  {
    type= RE_COMMIT;
    name= '-';
    g_free(entry);

    return;
  };

}

//----------------------------------------------------------------------------------------------------------------------

void check_RE_with_statement_prefix(MYX_RBS_STATUS* status, char* sql, int sql_length, RE_TYPE& type, std::string& name,
  int& has_prefix)
{
  type= RE_UNKNOWN;
  name= "";
                                  
  char* identifier;
  if (check_statement_and_prefix(sql, sql_length, status->re_create_table, NULL, 2, &has_prefix, &identifier))
  {
    type= RE_CREATE_TABLE;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_create_index, NULL, 2, &has_prefix, &identifier))
  {
    type= RE_CREATE_INDEX;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_create_view, NULL, 4, &has_prefix, &identifier))
  {
    type= RE_CREATE_VIEW;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_create_proc, NULL, 2, &has_prefix, &identifier))
  {
    type= RE_CREATE_PROCEDURE;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_create_func, NULL, 2, &has_prefix, &identifier))
  {
    type= RE_CREATE_FUNCTION;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_create_trigger, NULL, 4, &has_prefix, &identifier))
  {
    type= RE_CREATE_TRIGGER;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_drop_table, NULL, 2, &has_prefix, &identifier))
  {
    type= RE_DROP_TABLE;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_drop_view, NULL, 2, &has_prefix, &identifier))
  {
    type= RE_DROP_VIEW;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_drop_proc, NULL, 2, &has_prefix, &identifier))
  {
    type= RE_DROP_PROCEDURE;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_drop_func, NULL, 2, &has_prefix, &identifier))
  {
    type= RE_DROP_FUNCTION;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_drop_trigger, NULL, 1, &has_prefix, &identifier))
  {
    type= RE_DROP_TRIGGER;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_drop_index, NULL, 1, &has_prefix, &identifier))
  {
    type= RE_DROP_INDEX;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_insert, NULL, 2, &has_prefix, &identifier))
  {
    type= RE_INSERT;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_alter, NULL, 2, &has_prefix, &identifier))
  {
    type= RE_ALTER;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_lock, NULL, 1, &has_prefix, &identifier))
  {
    type= RE_LOCK;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_unlock, NULL, 0, &has_prefix, &identifier))
  {
    type= RE_UNLOCK;
    name= identifier;
    g_free(identifier);

    return;
  };

  // The following statement types cannot have a FQI.
  has_prefix= 0;
  int dummy;
  if (check_statement_and_prefix(sql, sql_length, status->re_create_database, NULL, 1, &dummy, &identifier))
  {
    type= RE_CREATE_DATABASE;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_db_use, NULL, 1, &dummy, &identifier))
  {
    type= RE_USE;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_drop_database, NULL, 1, &dummy, &identifier))
  {
    type= RE_DROP_SCHEMA;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_charset_set1, NULL, 1, &dummy, &identifier))
  {
    type= RE_CHARACTER_SET1;
    name= '-';
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_charset_set2, NULL, 1, &dummy, &identifier))
  {
    type= RE_CHARACTER_SET2;
    name= '-';
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_charset_set3, NULL, 1, &dummy, &identifier))
  {
    type= RE_CHARACTER_SET3;
    name= '-';
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_charset_set4, NULL, 1, &dummy, &identifier))
  {
    type= RE_CHARACTER_SET4;
    name= '-';
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_charset_set5, NULL, 1, &dummy, &identifier))
  {
    type= RE_CHARACTER_SET5;
    name= '-';
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_set, NULL, 1, &dummy, &identifier))
  {
    type= RE_SET;
    name= '-';
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_delimiter, NULL, 1, &dummy, &identifier))
  {
    type= RE_DELIMITER;
    name= identifier;
    g_free(identifier);

    return;
  };

  if (check_statement_and_prefix(sql, sql_length, status->re_commit, NULL, 0, &dummy, &identifier))
  {
    type= RE_COMMIT;
    name= '-';
    g_free(identifier);

    return;
  };

}

//----------------------------------------------------------------------------------------------------------------------

#undef CREATE_REFERENCE_DATA

void check_RE_with_file(MYSQL* mysql, MYX_BACKUP_CONTENT* content, std::string data_filename, std::string check_filename,
  char* encoding)
{
#ifdef CREATE_REFERENCE_DATA
  std::ofstream check(test_group_singleton.get_full_filename(check_filename).c_str());
#else
  std::ifstream check(test_group_singleton.get_full_filename(check_filename).c_str());
#endif

  MYX_LIB_ERROR error;
  std::string data_name = test_group_singleton.get_full_filename(data_filename);

  MYX_BACKUP_ERROR backup_error= MYX_BACKUP_NO_ERROR;
  MYX_RBS_STATUS* restore_status= NULL;

  restore_status= myx_new_rbs_status(mysql, data_name.c_str(), encoding, content, "def", NULL, 0,
    &backup_error, report_warning_restore, NULL);

  ensure("Creating restore status", restore_status != NULL);
  ensure("Creating restore status", backup_error == MYX_BACKUP_NO_ERROR);

  MYX_INTL_FILE* data = myx_new_intl_file(data_name.c_str(), encoding, &error);
  ensure("Open test data file", data != NULL);
  bigint size= get_file_size(data_name.c_str());
  ensure("Open test data file", size > 0);

  std::cout << "Test data: " << data_filename << '\n';
  int buffer_length= 10000;
  char* buffer= (char*) g_malloc0(buffer_length);
  do
  {
    RE_TYPE type1, type2;
    std::string name1, name2;
    int prefix;
    RE_TYPE reference_type;
    std::string reference_name;

    int length= ::myx_get_next_sql_statement_file(&restore_status->we, data, &buffer, &buffer_length, 1,
      progress_report_restore, size, NULL, &error);

    if (length == 0)
      break;

    check_RE_with_statement(restore_status, buffer, length, type1, name1);
    check_RE_with_statement_prefix(restore_status, buffer, length, type2, name2, prefix);

    ensure("Comparing identifiers", name1 == name2);
    ensure("Comparing identifiers", type1 == type2);

#ifdef CREATE_REFERENCE_DATA
    if (type1 != RE_UNKNOWN)
      check << type1 << " " << name1 << " " << prefix << '\n';
    else
    {
      std::cout << '\n';
      std::string statement(buffer, length);
      std::cout << "Problem: " << data_filename << ", SQL: \"" << statement << "\"\n";

      fail("Statement not recognized");
    };
#else
    int stored_type;
    int stored_prefix;
    check >> stored_type >> reference_name >> stored_prefix;
    reference_type= RE_TYPE(stored_type);
    ensure_equals("Checking type", type1, reference_type);
    ensure_equals("Checking name", name1, reference_name);
    ensure_equals("Checking prefix", prefix, stored_prefix);
#endif
  } while (true);

  g_free(buffer);

  myx_free_intl_file(data);
  myx_free_rbs_status(restore_status);
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(20)
{
  MYX_RBS_STATUS* restore_status;
  MYX_BACKUP_ERROR error= MYX_BACKUP_UNKNOWN;

  std::string source_filename(test_group_singleton.get_full_filename("backup_test_16.sql"));
  restore_status= myx_new_rbs_status(connection->get_mysql(), source_filename.c_str(), "utf-8", restore_content,
    "def", NULL, 0, &error, report_warning_restore, NULL);
  ensure("Creating restore status", restore_status != NULL);
  ensure("Creating restore status", error == MYX_BACKUP_NO_ERROR);
  myx_free_rbs_status(restore_status);

  check_RE_with_file(connection->get_mysql(), restore_content, "parse_2bookshop.sql", "parse_2bookshop.result",
    "utf-8");
  check_RE_with_file(connection->get_mysql(), restore_content, "parse_1000_tables.sql", "parse_1000_tables.result",
    "utf-8");

  check_RE_with_file(connection->get_mysql(), restore_content, "parse_AllTablesCreate.sql",
    "parse_AllTablesCreate.result", "utf-8");
  check_RE_with_file(connection->get_mysql(), restore_content, "parse_MySQL_SPs.sql", "parse_MySQL_SPs.result",
    "utf-8");
  check_RE_with_file(connection->get_mysql(), restore_content, "parse_world.sql", "parse_world.result", "latin1");
  check_RE_with_file(connection->get_mysql(), restore_content, "parse_sakila-schema.sql", "parse_sakila-schema.result",
    "latin1");
  check_RE_with_file(connection->get_mysql(), restore_content, "parse_sakila-data.sql", "parse_sakila-data.result",
    "latin1");

  /* This is a really big file. It is yet to be clarified how to cope with it (it cannot be committed to Subversion).
  check_RE_with_file(connection->get_mysql(), restore_content, "BIG - test 20051018 2143.sql",
    "BIG - test 20051018 2143.result", "latin1");
  */

  // Clear line.
  std::cout << "                                                                                                    \r";
}

//----------------------------------------------------------------------------------------------------------------------

static void make_backup(MYSQL* mysql, MYX_BACKUP_CONTENT *backup_content, MYX_BACKUP_OPTIONS options,
  const std::string target, const std::string reference)
{
  std::string option_values[] = {
    "MYX_B_NO_CREATES",
    "MYX_B_NO_EXTENDED_INSERT",
    "MYX_B_ADD_DROP_TABLE",
    "MYX_B_COMMENT",
    "MYX_B_DONT_WRITE_FULL_PATH",
    "MYX_B_LOCK_ALL_TABLES",
    "MYX_B_SINGLE_TRANSACTION",
    "MYX_B_DISABLE_KEYS",
    "MYX_B_COMPLETE_INSERTS",
    "MYX_B_ANSI_QUOTES",
    "MYX_B_ADD_LOCKS",
    "MYX_B_COMPLETE_SCHEMATAS",
    "MYX_B_SORT_TABLES",
    "MYX_B_COMPATIBILITY_MODE",
    "MYX_B_POINT_IN_TIME_BACKUP",
    "MYX_B_OPTIMIZED_COMMIT"
  };

  // Convert effective options into a human readable string.
  int mask= 1;
  std::string option_string;
  for (int i = 0; i < 16; i++, mask *= 2)
  {
    if ((options & mask) != 0)
      option_string += option_values[i] + ',';
  };
  // Remove last comma.
  if (option_string.size() > 0)
    option_string.resize(option_string.size() - 1);
  else
    option_string= "no options";
  std::cout << "Doing backup (" << option_string << ")\n";

  myx_make_backup(mysql, target.c_str(), backup_content, MYX_BT_SQL_SCRIPT, options, 100, progress_report_backup, NULL);
  
  std::cout << "\n";

  // Now check what was written out.
  ensure("Checking sakila dump", test_group_singleton.compare_files(target, reference));
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(25)
{
  // Check that the test schema does not exist already.
  if (myx_mysql_query(connection->get_mysql(), "use restore_test") == 0)
    ensure("Removing old test schema", myx_mysql_query(connection->get_mysql(), "drop schema restore_test") == 0);

  // Test only if we get an error message for certain parameters. Content check is done in the next test.
  std::string source_filename(test_group_singleton.get_full_filename("backup_test_15.sql"));

  MYX_BACKUP_ERROR error= MYX_BACKUP_UNKNOWN;
  MYX_BACKUP_CONTENT* content= NULL;

  content= myx_get_backup_content(source_filename.c_str(), "latin1", MYX_BT_SQL_SCRIPT, 100000, progress_report_restore,
    report_warning_restore, NULL, &error, false, false);
  ensure("Getting backup content", error == MYX_BACKUP_NO_ERROR);
  ensure("Getting backup content", content != NULL);
  ensure("Validating backup content", content->tables_num == 35);
  myx_free_backup_content(content);
                                                                     
  // Check with forced schema and FQI.
  content= myx_get_backup_content(source_filename.c_str(), "latin1", MYX_BT_SQL_SCRIPT, 100000, progress_report_restore,
    NULL, NULL, &error, true, false);
  ensure("Backup content with forced schema and FQI", error == MYX_BACKUP_DIFFERENT_SCHEMA_IMPOSSIBLE);
  ensure("Backup content with forced schema and FQI", content == NULL);

  // Check with forced schema and FQI + ignored errors.
  content= myx_get_backup_content(source_filename.c_str(), "latin1", MYX_BT_SQL_SCRIPT, 100000, progress_report_restore,
    NULL, NULL, &error, true, true);
  ensure("Backup content with forced schema, FQI and errors to be ignored.", error == MYX_BACKUP_DIFFERENT_SCHEMA_IMPOSSIBLE);
  ensure("Backup content with forced schema, FQI and errors to be ignored.", content == NULL);

  // Clear line.
  std::cout << "                                                                                                    \r";
}

//----------------------------------------------------------------------------------------------------------------------

typedef struct tagBackupEntry
{
  char* name;
  int flags;
} backup_entry;

const backup_entry restore_entries[35]=
{
  {"actor_info", MYX_BTF_IS_VIEW},
  {"customer_list", MYX_BTF_IS_VIEW},
  {"film_list", MYX_BTF_IS_VIEW},
  {"nicer_but_slower_film_list", MYX_BTF_IS_VIEW},
  {"sales_by_film_category", MYX_BTF_IS_VIEW},
  {"sales_by_store", MYX_BTF_IS_VIEW},
  {"staff_list", MYX_BTF_IS_VIEW},
  {"actor", MYX_BTF_IS_TABLE},
  {"address", MYX_BTF_IS_TABLE},
  {"category", MYX_BTF_IS_TABLE},
  {"city", MYX_BTF_IS_TABLE},
  {"country", MYX_BTF_IS_TABLE},
  {"customer", MYX_BTF_IS_TABLE},
  {"customer_create_date", MYX_BTF_IS_TRIGGER},
  {"film", MYX_BTF_IS_TABLE},
  {"ins_film", MYX_BTF_IS_TRIGGER},
  {"upd_film", MYX_BTF_IS_TRIGGER},
  {"del_film", MYX_BTF_IS_TRIGGER},
  {"film_actor", MYX_BTF_IS_TABLE},
  {"film_category", MYX_BTF_IS_TABLE},
  {"film_text", MYX_BTF_IS_TABLE},
  {"inventory", MYX_BTF_IS_TABLE},
  {"language", MYX_BTF_IS_TABLE},
  {"payment", MYX_BTF_IS_TABLE},
  {"payment_date", MYX_BTF_IS_TRIGGER},
  {"rental", MYX_BTF_IS_TABLE},
  {"rental_date", MYX_BTF_IS_TRIGGER},
  {"staff", MYX_BTF_IS_TABLE},
  {"store", MYX_BTF_IS_TABLE},
  {"get_customer_balance", MYX_BTF_IS_FUNCTION},
  {"inventory_held_by_customer", MYX_BTF_IS_FUNCTION},
  {"inventory_in_stock", MYX_BTF_IS_FUNCTION},
  {"film_in_stock", MYX_BTF_IS_PROCEDURE},
  {"film_not_in_stock", MYX_BTF_IS_PROCEDURE},
  {"rewards_report", MYX_BTF_IS_PROCEDURE}
};

TEST_FUNCTION(30)
{
  std::string source_filename(test_group_singleton.get_full_filename("backup_test_19.sql"));

  MYX_BACKUP_ERROR error= MYX_BACKUP_UNKNOWN;
  MYX_BACKUP_CONTENT* content= NULL;
                                                        
  content= myx_get_backup_content(source_filename.c_str(), "latin1", MYX_BT_SQL_SCRIPT, 10000, progress_report_restore,
    report_warning_restore, NULL, &error, false, false);
  ensure("Getting backup content", error == MYX_BACKUP_NO_ERROR);
  ensure("Getting backup content", content != NULL);
  ensure_equals("Validating backup content", content->tables_num, 35);

  for (unsigned int i= 0; i < content->tables_num; i++)
  {
    ensure_equals("Validating backup content", std::string(content->tables[i].catalog), "DEFAULT_CATALOG");
    ensure_equals("Validating backup content", std::string(content->tables[i].schema), "sakila");
    ensure_equals("Validating backup content", std::string(content->tables[i].table), restore_entries[i].name);
    ensure_equals("Validating backup content", content->tables[i].flags, restore_entries[i].flags);
  };

  // Clear line.
  std::cout << "                                                                                                    \r";

  ensure("Removing old test schema", myx_mysql_query(connection->get_mysql(), "drop schema if exists restore_test") == 0);

  error= MYX_BACKUP_UNKNOWN;
  error= myx_restore_backup(connection->get_mysql(), source_filename.c_str(), "latin1", content, NULL, "restore_test",
    MYX_BT_SQL_SCRIPT, MYX_RBS_FORCE, 2000, progress_report_restore, NULL, report_warning_restore, NULL);
  ensure("Restoring backup", error == MYX_BACKUP_NO_ERROR);
  myx_free_backup_content(content);

  // Now backup the just uploaded data again.

  // Create a temporary backup content structure. Only one table is in it but we do a full backup.
  backup_content= (MYX_BACKUP_CONTENT*) g_malloc0(sizeof(MYX_BACKUP_CONTENT));
  backup_content->tables_num= 1;
  backup_content->tables= (MYX_BACKUP_TABLE*) g_malloc0(sizeof(MYX_BACKUP_TABLE) * backup_content->tables_num);
  fill_table_entry(backup_content->tables[0], "actor", MYX_BTF_IS_FUNCTION, "restore_test");

  // We use the same options here as were used to create the backup reference data.
  MYX_BACKUP_OPTIONS options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_COMPLETE_SCHEMATAS |
    MYX_B_DONT_WRITE_FULL_PATH | MYX_B_DISABLE_KEYS | MYX_B_COMPLETE_INSERTS | MYX_B_ANSI_QUOTES | MYX_B_ADD_LOCKS |
    MYX_B_SORT_TABLES | MYX_B_OPTIMIZED_COMMIT);
  make_backup(connection->get_mysql(), backup_content, options, "backup_test.sql", "backup_test_17.sql");

  ensure("Removing old test schema", myx_mysql_query(connection->get_mysql(), "drop schema restore_test") == 0);

  // Clear line.
  std::cout << "                                                                                                    \r";
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(500)
{
  // Cleanup.
  myx_free_backup_content(restore_content);
  myx_free_backup_content(backup_content);

  int error= unlink("backup_test.sql");
  ensure("Deleting test sql file", error == 0);

  connection= NULL;
}

//----------------------------------------------------------------------------------------------------------------------

END_TESTS;

//----------------------------------------------------------------------------------------------------------------------

