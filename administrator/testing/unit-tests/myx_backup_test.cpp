
// This file contains MA TUT test cases for myx_backup.c
//
//

//----------------------------------------------------------------------------------------------------------------------

#include <fstream>

#include "test.h"
#include "myx_admin_library.h"
#include "myx_shared_util_functions.h"

// Private test data.
BEGIN_TEST_DATA_CLASS(module2_backup)
protected:
  Test_connection* connection;
  MYX_BACKUP_CONTENT *backup_content;
END_TEST_DATA_CLASS

TEST_MODULE(module2_backup, "MySQL Administrator test suite");

//----------------------------------------------------------------------------------------------------------------------

void fill_table_entry(MYX_BACKUP_TABLE& table, char* name, int flags)
{
  table.flags= flags;
  table.catalog= g_strdup("def");
  table.schema= g_strdup("sakila");
  table.table= g_strdup(name);
}

//----------------------------------------------------------------------------------------------------------------------

/*
  Prepares all needed data structures etc. for the tests in this module (including a backup context).
*/

TEST_FUNCTION(5)
{
  // Create a backup content structure which is used for the backup test code.
  backup_content= (MYX_BACKUP_CONTENT*) g_malloc0(sizeof(MYX_BACKUP_CONTENT));
  backup_content->tables_num= 12; // 3 tables, 3 views, 2 stored procedures, 2 stored functions and 2 triggers
  backup_content->tables= (MYX_BACKUP_TABLE*) g_malloc0(sizeof(MYX_BACKUP_TABLE) * backup_content->tables_num);

  fill_table_entry(backup_content->tables[0], "actor", MYX_BTF_IS_TABLE);
  fill_table_entry(backup_content->tables[1], "film", MYX_BTF_IS_TABLE);
  fill_table_entry(backup_content->tables[2], "city", MYX_BTF_IS_TABLE);
  fill_table_entry(backup_content->tables[3], "film_list", MYX_BTF_IS_VIEW);
  fill_table_entry(backup_content->tables[4], "actor_info", MYX_BTF_IS_VIEW);
  fill_table_entry(backup_content->tables[5], "sales_by_store", MYX_BTF_IS_VIEW);
  fill_table_entry(backup_content->tables[6], "film_in_stock", MYX_BTF_IS_PROCEDURE);
  fill_table_entry(backup_content->tables[7], "film_not_in_stock", MYX_BTF_IS_PROCEDURE);
  fill_table_entry(backup_content->tables[8], "get_customer_balance", MYX_BTF_IS_FUNCTION);
  fill_table_entry(backup_content->tables[9], "inventory_in_stock", MYX_BTF_IS_FUNCTION);
  fill_table_entry(backup_content->tables[10], "ins_film", MYX_BTF_IS_TRIGGER);
  fill_table_entry(backup_content->tables[11], "del_film", MYX_BTF_IS_TRIGGER);

  connection= test_group_singleton.get_connection();
  ensure("Server connection", connection != NULL);
}

//----------------------------------------------------------------------------------------------------------------------

/*
  Checks creation of a local backup file (write permissions etc.).
*/
TEST_FUNCTION(10)
{
  // Check that we can create a backup file and write to it.
  FILE* file= fopen("backup_write_test.sql", "w+b");
  ensure("Create local file", file != NULL);

  int count= fprintf(file, "%s\n", "TEST");
  ensure("Write text to local file", count == 5);

  count= fprintf(file, "%s", "A longer text\n");
  ensure("Write another text to local file", count == 14);

  int position= ftell(file);
  ensure("Check file position in local file", position == 19);

  fclose(file);
  unlink("backup_write_test.sql");

}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(15)
{
  MYX_BACKUP_OPTIONS options;
  MYX_BACKUP_ERROR error= MYX_BACKUP_UNKNOWN;
  MYX_BS_STATUS* backup_status;

  MYSQL* mysql= connection->get_mysql();
  MYX_MYSQL* private_data= myx_mysql_get_private(mysql);

  // Try to use lock all tables and single transaction together.
  // At least under BDS 2006 the result of enum | enum is an int, so we have to cast back to enum to avoid warnings.
  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_LOCK_ALL_TABLES | MYX_B_ANSI_QUOTES |
    MYX_B_SINGLE_TRANSACTION);
  backup_status= myx_new_bs_status(mysql, "backup_test.sql", backup_content, options, &error);
  ensure("Creating backup status with invalid options", error == MYX_BACKUP_ILLEGAL_OPTION);
  ensure("Creating backup status with invalid options", backup_status == NULL);

  // Simulate server with old version.
  short last_version= private_data->major_version;
  private_data->major_version= 3;
  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_DISABLE_KEYS | MYX_B_ANSI_QUOTES);
  backup_status= myx_new_bs_status(mysql, "backup_test.sql", backup_content, options, &error);
  ensure("Creating backup status with invalid options", error == MYX_BACKUP_CANNOT_SET_ANSI_QUOTES);
  ensure("Creating backup status with invalid options", backup_status == NULL);
  private_data->major_version= last_version;

  // Check invalid file name.
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  backup_status= myx_new_bs_status(mysql, "Y:\backup_test.sql", backup_content, options, &error);
  ensure("Creating backup status with invalid options", error == MYX_BACKUP_CANT_OPEN_FILE);
  ensure("Creating backup status with invalid options", backup_status == NULL);
#endif
  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_DISABLE_KEYS | MYX_B_ANSI_QUOTES);
  backup_status= myx_new_bs_status(mysql, "backup_test.sql", backup_content, options, &error);
  ensure("Creating backup status with valid options", error == MYX_BACKUP_NO_ERROR);
  ensure("Creating backup status with valid options", backup_status != NULL);

  // Remove reference to our content to avoid freeing it.
  myx_free_bs_status(backup_status);
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(20)
{
  MYX_BACKUP_OPTIONS options;
  MYX_BACKUP_ERROR error= MYX_BACKUP_UNKNOWN;        
  MYX_BS_STATUS* backup_status;

  std::string target_filename("backup_test.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_DISABLE_KEYS | MYX_B_ANSI_QUOTES);
  backup_status= myx_new_bs_status(connection->get_mysql(), target_filename.c_str(), backup_content, options, &error);

  // Since we have not yet the full backup setup it is needed to tweak a few values.
  backup_status->quote_char= '`';

  // Write one table, one SP, one SF, one trigger and one view to the sql file.
  backup_status->current_index= 0; 
  char *current_table= backup_status->backup_content->tables[backup_status->current_index].table;
  backup_status->current_table_quoted= quote_identifier(current_table, backup_status->quote_char);
  char* current_schema= g_strdup(backup_status->backup_content->tables[backup_status->current_index].schema);
  backup_status->current_schema_quoted= quote_identifier(current_schema, backup_status->quote_char);

  write_create_statement_to_file(backup_status, &error);
  g_free(backup_status->current_table_quoted);
  g_free(backup_status->current_schema_quoted);

  // Now dump a view.
  backup_status->current_index= 3;
  current_table= backup_status->backup_content->tables[backup_status->current_index].table;
  backup_status->current_table_quoted= quote_identifier(current_table, backup_status->quote_char);
  current_schema= g_strdup(backup_status->backup_content->tables[backup_status->current_index].schema);
  backup_status->current_schema_quoted= quote_identifier(current_schema, backup_status->quote_char);

  write_create_statement_to_file(backup_status, &error);
  g_free(backup_status->current_table_quoted);
  g_free(backup_status->current_schema_quoted);

  // A stored procedure.
  backup_status->current_index= 6;
  current_table= backup_status->backup_content->tables[backup_status->current_index].table;
  backup_status->current_table_quoted= quote_identifier(current_table, backup_status->quote_char);
  current_schema= g_strdup(backup_status->backup_content->tables[backup_status->current_index].schema);
  backup_status->current_schema_quoted= quote_identifier(current_schema, backup_status->quote_char);

  write_create_statement_to_file(backup_status, &error);
  g_free(backup_status->current_table_quoted);
  g_free(backup_status->current_schema_quoted);

  // A stored function.
  backup_status->current_index= 9;
  current_table= backup_status->backup_content->tables[backup_status->current_index].table;
  backup_status->current_table_quoted= quote_identifier(current_table, backup_status->quote_char);
  current_schema= g_strdup(backup_status->backup_content->tables[backup_status->current_index].schema);
  backup_status->current_schema_quoted= quote_identifier(current_schema, backup_status->quote_char);

  write_create_statement_to_file(backup_status, &error);
  g_free(backup_status->current_table_quoted);
  g_free(backup_status->current_schema_quoted);

  // And finally a trigger.
  backup_status->current_index= 10;
  current_table= backup_status->backup_content->tables[backup_status->current_index].table;
  backup_status->current_table_quoted= quote_identifier(current_table, backup_status->quote_char);
  current_schema= g_strdup(backup_status->backup_content->tables[backup_status->current_index].schema);
  backup_status->current_schema_quoted= quote_identifier(current_schema, backup_status->quote_char);

  write_create_statement_to_file(backup_status, &error);
  fflush(backup_status->sql_file);
  myx_free_bs_status(backup_status);        

  // Now check what was written out.
  ensure("Checking result of writing table, sf, sp, view and trigger to file",
    test_group_singleton.compare_files(target_filename, "backup_test_1.sql"));
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(25)
{
  MYX_BACKUP_OPTIONS options;
  MYX_BACKUP_ERROR error= MYX_BACKUP_UNKNOWN;
  MYX_BS_STATUS* backup_status;

  std::string target_filename("backup_test.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_COMPLETE_INSERTS);
  backup_status= myx_new_bs_status(connection->get_mysql(), target_filename.c_str(), backup_content, options, &error);

  // Write the records of the first table to dump.
  backup_status->current_index= 0;
  char *current_table= backup_status->backup_content->tables[backup_status->current_index].table;
  backup_status->current_table_quoted= quote_identifier(current_table, backup_status->quote_char);
  char* current_schema= g_strdup(backup_status->backup_content->tables[backup_status->current_index].schema);
  backup_status->current_schema_quoted= quote_identifier(current_schema, backup_status->quote_char);

  // Create a resultset whose rows we can dump.
  std::stringstream statement;
  statement << "SELECT /*!40001 SQL_NO_CACHE */ * FROM " << backup_status->current_schema_quoted << '.'
    << backup_status->current_table_quoted;
  backup_status->mysql_result= NULL;
  int result= myx_mysql_query(backup_status->mysql, statement.str().c_str());
  ensure("Creating a resultset", result == 0);

  backup_status->mysql_result= mysql_use_result(backup_status->mysql);
  ensure("Retrieving rows", backup_status->mysql_result != NULL);

  MYSQL_ROW row;
  do
  {
    row= mysql_fetch_row(backup_status->mysql_result);              
    if (row == NULL)
      break;
    write_row_to_file(backup_status, row, &error);
    ensure("Writing row to backup file", error == MYX_BACKUP_NO_ERROR);

  } while (true);

  finalize_extended_insert(backup_status, &error);
  mysql_free_result(backup_status->mysql_result);

  fflush(backup_status->sql_file);
  myx_free_bs_status(backup_status);

  // Now check what was written out.
  ensure("Checking dumped rows", test_group_singleton.compare_files(target_filename, "backup_test_2.sql"));
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(30)
{
  MYX_BACKUP_OPTIONS options;
  MYX_BACKUP_ERROR error= MYX_BACKUP_UNKNOWN;
  MYX_BS_STATUS* backup_status;

  std::string target_filename("backup_test.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_COMPLETE_INSERTS);
  backup_status= myx_new_bs_status(connection->get_mysql(), target_filename.c_str(), backup_content, options, &error);

  error= MYX_BACKUP_UNKNOWN;
  int result= write_sql_file_footer(backup_status, &error);
  ensure("Writing footer to dump file", result == 0);
  ensure("Writing footer to dump file", error == MYX_BACKUP_NO_ERROR);

  fflush(backup_status->sql_file);
  myx_free_bs_status(backup_status);

  // Now check what was written out.
  ensure("Checking dumped footer",
    test_group_singleton.compare_files(target_filename, "backup_test_3.sql"));
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(35)
{
  MYX_BACKUP_OPTIONS options;
  MYX_BACKUP_ERROR error= MYX_BACKUP_UNKNOWN;
  MYX_BS_STATUS* backup_status;

  std::string target_filename("backup_test.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_COMPLETE_INSERTS);
  backup_status= myx_new_bs_status(connection->get_mysql(), target_filename.c_str(), backup_content, options, &error);

  error= MYX_BACKUP_UNKNOWN;
  int result= write_sql_file_header(backup_status, &error);
  ensure("Writing header to dump file", result == 0);
  ensure("Writing header to dump file", error == MYX_BACKUP_NO_ERROR);

  fflush(backup_status->sql_file);
  myx_free_bs_status(backup_status);

  // Now check what was written out.
  ensure("Checking dumped header",
    test_group_singleton.compare_files(target_filename, "backup_test_4.sql"));
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(40)
{
  MYX_BACKUP_OPTIONS options;
  MYX_BACKUP_ERROR error= MYX_BACKUP_UNKNOWN;
  MYX_BS_STATUS* backup_status;

  std::string target_filename("backup_test.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_DISABLE_KEYS | MYX_B_ANSI_QUOTES);
  backup_status= myx_new_bs_status(connection->get_mysql(), target_filename.c_str(), backup_content, options, &error);

  // It's about writing create database statements here so pretend the first entry would be another schema.
  char* temp= backup_status->backup_content->tables[0].schema;
  backup_status->backup_content->tables[0].schema= g_strdup("test");

  backup_status->current_index= 0;
  char *current_table= backup_status->backup_content->tables[backup_status->current_index].table;
  backup_status->current_table_quoted= quote_identifier(current_table, backup_status->quote_char);
  char* current_schema= g_strdup(backup_status->backup_content->tables[backup_status->current_index].schema);
  backup_status->current_schema_quoted= quote_identifier(current_schema, backup_status->quote_char);

  error= MYX_BACKUP_UNKNOWN;
  int result= write_schema_ddl(backup_status, &error);
  ensure("Writing path info of table 0", result == 0);
  ensure("Writing path info of table 0", error == MYX_BACKUP_NO_ERROR);

  g_free(backup_status->current_table_quoted);
  g_free(backup_status->current_schema_quoted);

  backup_status->current_index++;
  current_table= backup_status->backup_content->tables[backup_status->current_index].table;
  backup_status->current_table_quoted= quote_identifier(current_table, backup_status->quote_char);
  current_schema= g_strdup(backup_status->backup_content->tables[backup_status->current_index].schema);
  backup_status->current_schema_quoted= quote_identifier(current_schema, backup_status->quote_char);

  error= MYX_BACKUP_UNKNOWN;
  result= write_schema_ddl(backup_status, &error);
  ensure("Writing path info of table 1", result == 0);
  ensure("Writing path info of table 1", error == MYX_BACKUP_NO_ERROR);

  // Since we do not change the schema in the next index there should be no output but also no error.
  backup_status->current_index++;
  error= MYX_BACKUP_UNKNOWN;
  result= write_schema_ddl(backup_status, &error);
  ensure("Writing path info of table 2", result == 0);                 
  ensure("Writing path info of table 2", error == MYX_BACKUP_NO_ERROR);

  fflush(backup_status->sql_file);

  g_free(backup_status->backup_content->tables[0].schema);
  backup_status->backup_content->tables[0].schema= temp;

  myx_free_bs_status(backup_status);

  // Now check what was written out.
  ensure("Checking dumped path info", test_group_singleton.compare_files(target_filename, "backup_test_6.sql"));
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Call back function for backup processes. Since it is only called for warnings it will always
 * cause the current test to fail.
 */
void report_warning(const char *msg, void *user_data)
{
  fail("Warning in backup callback: " + std::string(msg));
}

//----------------------------------------------------------------------------------------------------------------------

int progress_report(const char* curr_tbl_name, int num_tables, int num_tables_processed, int num_rows,
  int num_rows_processed, void *user_data)
{
  std::cout << "Current object: " << curr_tbl_name << " (" << num_tables_processed << " of " << num_tables << ", "
    << num_rows << " rows)"
    << "                                                  \r";

  return 0;
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

  myx_make_backup(mysql, target.c_str(), backup_content, MYX_BT_SQL_SCRIPT, options, 100, progress_report, NULL);

  std::cout << "\n";

  // Now check what was written out.
  ensure("Checking schema dump", test_group_singleton.compare_files(target, reference));
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(45)
{
  MYX_BACKUP_OPTIONS options;
                                                      
  std::string target_filename("backup_test.sql");

  options= MYX_BACKUP_OPTIONS(0);
  make_backup(connection->get_mysql(), backup_content, options, target_filename, "backup_test_7.sql");
  
  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_DISABLE_KEYS | MYX_B_ANSI_QUOTES);
  make_backup(connection->get_mysql(), backup_content, options, target_filename, "backup_test_8.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_ANSI_QUOTES | MYX_B_COMPATIBILITY_MODE | MYX_B_LOCK_ALL_TABLES);
  make_backup(connection->get_mysql(), backup_content, options, target_filename, "backup_test_9.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_ANSI_QUOTES | MYX_B_COMPATIBILITY_MODE | MYX_B_SINGLE_TRANSACTION);
  make_backup(connection->get_mysql(), backup_content, options, target_filename, "backup_test_10.sql");

  // Single transaction and point in time backup cannot be used at the same time. We check it anyway.
  options= MYX_BACKUP_OPTIONS(MYX_B_ANSI_QUOTES | MYX_B_SINGLE_TRANSACTION | MYX_B_POINT_IN_TIME_BACKUP);
  make_backup(connection->get_mysql(), backup_content, options, target_filename, "backup_test_11.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_DONT_WRITE_FULL_PATH
   | MYX_B_NO_EXTENDED_INSERT);
  make_backup(connection->get_mysql(), backup_content, options, target_filename, "backup_test_12.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_COMMENT | MYX_B_ANSI_QUOTES | MYX_B_NO_CREATES);
  make_backup(connection->get_mysql(), backup_content, options, target_filename, "backup_test_13.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_ADD_LOCKS | MYX_B_SORT_TABLES);
  make_backup(connection->get_mysql(), backup_content, options, target_filename, "backup_test_14.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_COMPLETE_SCHEMATAS | MYX_B_OPTIMIZED_COMMIT);
  make_backup(connection->get_mysql(), backup_content, options, target_filename, "backup_test_15.sql");

  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_COMPLETE_SCHEMATAS |
	MYX_B_DONT_WRITE_FULL_PATH | MYX_B_DISABLE_KEYS | MYX_B_COMPLETE_INSERTS | MYX_B_ANSI_QUOTES | MYX_B_ADD_LOCKS |
	MYX_B_SORT_TABLES | MYX_B_OPTIMIZED_COMMIT);
  make_backup(connection->get_mysql(), backup_content, options, target_filename, "backup_test_16.sql");
}                                                               

//----------------------------------------------------------------------------------------------------------------------

/**
 * Test for bug 24849.
 */
TEST_FUNCTION(50)
{
  static char* sql_test_50_data1= "";

  MYX_BACKUP_OPTIONS options;
  MYX_BACKUP_CONTENT *backup_content; // We need a private backup content.
  Auto_release auto_release;             

  std::ifstream source(test_group_singleton.get_full_filename("backup_test_18_source.sql").c_str());
  ensure("Loading test data", source != NULL);
  connection->multi_query(source);

  // Make sure the test database is removed after the tests.
  auto_release.add_sql(connection, sql_test_50_data1);

  std::string target_filename("backup_test.sql");

  // Create and fill private backup content.
  backup_content= (MYX_BACKUP_CONTENT*) g_malloc0(sizeof(MYX_BACKUP_CONTENT));
  backup_content->tables_num= 1;
  backup_content->tables= (MYX_BACKUP_TABLE*) g_malloc0(sizeof(MYX_BACKUP_TABLE) * backup_content->tables_num);
  backup_content->tables[0].flags= MYX_BTF_IS_TABLE;
  backup_content->tables[0].catalog= g_strdup("def");
  backup_content->tables[0].schema= g_strdup("bug24849");
  backup_content->tables[0].table= g_strdup("container_recv");    

  options= MYX_BACKUP_OPTIONS(MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_DISABLE_KEYS | MYX_B_ANSI_QUOTES);
  make_backup(connection->get_mysql(), backup_content, options, target_filename, "backup_test_18.sql");

  myx_free_backup_content(backup_content);
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(500)
{
  // Cleanup.
  myx_free_backup_content(backup_content);

  int error= unlink("backup_test.sql");
  ensure("Deleting test sql file", error == 0);

  connection= NULL;
}

//----------------------------------------------------------------------------------------------------------------------

END_TESTS;

//----------------------------------------------------------------------------------------------------------------------

