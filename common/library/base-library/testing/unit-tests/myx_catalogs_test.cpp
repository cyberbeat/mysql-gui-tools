
// This file contains common TUT test cases for myx_catalogs.c
//
//

//----------------------------------------------------------------------------------------------------------------------

#include "test.h"
#include "myx_public_interface.h"

// Private test data.
BEGIN_TEST_DATA_CLASS(module2_catalog_test)
protected:
  Test_connection* connection;
END_TEST_DATA_CLASS

//----------------------------------------------------------------------------------------------------------------------

TEST_MODULE(module2_catalog_test, "Common test suite, base library");

//----------------------------------------------------------------------------------------------------------------------

static const char * show_master_status_fields[]=
{
  "File",               // 0
  "Position",           // 1
  "Binlog_Do_DB",       // 2
  "Binlog_Ignore_DB"    // 3
};
static const char ** show_master_status_fields_end= show_master_status_fields +
  sizeof(show_master_status_fields) / sizeof(char*);

TEST_FUNCTION(5)
{
  connection= test_group_singleton.get_connection();
  ensure("Valid server connection", connection != NULL);

  Resultset* resultset = connection->new_resultset("SHOW MASTER STATUS");
  ensure("Query SHOW MASTER STATUS", resultset != NULL);

  connection->query("DROP SCHEMA IF EXISTS `common_test`");
  connection->query("CREATE SCHEMA `common_test`");

  MYSQL_FIELD *fields;
  int num_fields;
  int fi[4];

  num_fields= resultset->get_column_count();
  fields= resultset->get_column_fields();
  build_field_subst(show_master_status_fields, show_master_status_fields_end, fields, fields + num_fields, fi);

  for (int i = 0; i < sizeof(show_master_status_fields) / sizeof(char*); i++)
  {
    ensure("build_field_subst", fi[i] == i);
  }

  delete resultset;
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(10)
{
  connection->query("DROP FUNCTION IF EXISTS `common_test`.`test_17096`");
  connection->query("CREATE FUNCTION `common_test`.`test_17096`() RETURNS int(11) BEGIN return 1; END");

  Auto_release ar;
  MYX_SCHEMA_STORED_PROCEDURES *sps= ar.add(myx_get_schema_sps(connection->get_mysql(), "def", "common_test"));

  ensure("Number of created routines", sps->schema_sps_num == 1);
  ensure("Properly initialized parameter fields", sps->schema_sps[0].params_num == 0);
  ensure("Properly initialized parameter fields", sps->schema_sps[0].params == NULL);

  for (unsigned int i= 0; i < sps->schema_sps_num; i++)
  {
    if (strcmp2(sps->schema_sps[i].name, "test_17096") == 0)
    {
      ensure("Return data types", strcmp2(sps->schema_sps[i].return_datatype, "int(11)") == 0);
      connection->query("DROP FUNCTION IF EXISTS `common_test`.`test_17096`");
      break;
    }
  }
}

//----------------------------------------------------------------------------------------------------------------------

typedef struct
{
  std::string name;
  std::string type;
} table_test_entry;

static const table_test_entry table_test_data[]=
{
  {"actor", "InnoDB"},
  {"address", "InnoDB"},
  {"category", "InnoDB"},
  {"city", "InnoDB"},
  {"country", "InnoDB"},
  {"customer", "InnoDB"},
  {"film", "InnoDB"},
  {"film_actor", "InnoDB"},
  {"film_category", "InnoDB"},
  {"film_text", "MyISAM"},
  {"inventory", "InnoDB"},
  {"language", "InnoDB"},
  {"payment", "InnoDB"},
  {"rental", "InnoDB"},
  {"staff", "InnoDB"},
  {"store", "InnoDB"}
};
unsigned int table_test_entries= sizeof(table_test_data) / sizeof(table_test_entry);

TEST_FUNCTION(15)
{
  MYSQL* mysql= connection->get_mysql();
  Auto_release releaser;

  // Test getting schema tables.
  MYX_SCHEMA_TABLE_STATUS* table_status;

  // Start with invalid schema.
  table_status= myx_get_schema_table_status(mysql, NULL, "dummy");
  ensure("Invalid schema", table_status == NULL);

  // Empty schema. Ensure the test schema is deleted even in case of a failure.
  releaser.add_sql(connection, "drop schema if exists catalogs_test");

  connection->query("create schema catalogs_test");
  connection->query("use catalogs_test"); // Throws a logic error if it fails.
  table_status= myx_get_schema_table_status(mysql, NULL, "catalogs_test");
  ensure("Empty schema", table_status != NULL);
  ensure("Empty schema", table_status->schema_tables == NULL);
  connection->query("drop schema catalogs_test");
  myx_free_schema_table_status(table_status);

  // Finally check a schema with content.
  table_status= myx_get_schema_table_status(mysql, NULL, "sakila");
  ensure("Table count", table_status->schema_tables_num == table_test_entries);
  for (unsigned int i = 0; i < table_test_entries; i++)
  {
    ensure("Table name", table_status->schema_tables[i].table_name == table_test_data[i].name);
    ensure("Table type", table_status->schema_tables[i].table_type == table_test_data[i].type);
  };

  myx_free_schema_table_status(table_status);
}

//----------------------------------------------------------------------------------------------------------------------

static const std::string view_test_data[]=
{
  "actor_info",
  "customer_list",
  "film_list",
  "nicer_but_slower_film_list",
  "sales_by_film_category",
  "sales_by_store",
  "staff_list"
};
unsigned int view_test_entries= sizeof(view_test_data) / sizeof(std::string);

TEST_FUNCTION(20)
{
  MYSQL* mysql= connection->get_mysql();
  Auto_release releaser;

  // Test getting schema views.
  MYX_SCHEMA_VIEW_STATUS* view_status;

  // Start with invalid schema.
  view_status= myx_get_schema_view_status(mysql, NULL, "dummy");
  ensure("Invalid schema", view_status == NULL);

  // Empty schema. Ensure the test schema is deleted even in case of a failure.
  releaser.add_sql(connection, "drop schema if exists catalogs_test");

  connection->query("create schema catalogs_test");
  connection->query("use catalogs_test"); // Throws a logic error if it fails.
  view_status= myx_get_schema_view_status(mysql, NULL, "catalogs_test");
  ensure("Empty schema", view_status != NULL);
  ensure("Empty schema", view_status->schema_views == NULL);
  connection->query("drop schema catalogs_test");
  myx_free_schema_view_status(view_status);

  // Finally check a schema with content.
  view_status= myx_get_schema_view_status(mysql, NULL, "sakila");
  ensure("View count", view_status->schema_views_num == view_test_entries);
  for (unsigned int i = 0; i < view_test_entries; i++)
  {
    ensure("View name", view_status->schema_views[i].view_name == view_test_data[i]);
  };

  myx_free_schema_view_status(view_status);
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(500)
{
  connection->query("DROP SCHEMA IF EXISTS `common_test`");
}

//----------------------------------------------------------------------------------------------------------------------

END_TESTS;

//----------------------------------------------------------------------------------------------------------------------

