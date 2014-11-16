//---------------------------------------------------------------------------

#pragma hdrstop

#include "test.h"
//---------------------------------------------------------------------------

/*
  Test suite for routines from sql_helpers.h
*/

TEST_MODULE(Sql_helpers_test, "Test suite for routines from sql_helpers.h");

/*
  Test member functions of class Resultset

  DESCRIPTION
    Test functions save/load and operator ==
*/

TEST_FUNCTION(1)
{
  Test_connection *c;
  ensure("Server connection", c= test_group_singleton.get_connection());

  c->query("drop table test.sql_helpers_test");

  static const char *create_table_q=
    "create table test.sql_helpers_test "
    "(f_int int, f_varchar varchar(255)) engine=myisam";

  ensure("Create table test.sql_helpers_test", c->query(create_table_q) == 0);

  static const char *insert_test_values_q=
    "insert into test.sql_helpers_test values "
    "(1, 'string1'), (2, 'string2'), (3, 'string3'), (4, 'string4')";

  ensure("Insert test values into test.sql_helpers_test",
    c->query(insert_test_values_q) == 0);


  static const char *select_q=
    "select * from test.sql_helpers_test";

  Resultset rs1;

  ensure("Select test values from test.sql_helpers_test",
    rs1.query(c, select_q));

  Resultset rs2;

  ensure("Select test values from test.sql_helpers_test",
    rs2.query(c, select_q));

  ensure("Test operator ==", rs1 == rs2);

  static const char *dump_file= "sql_helpers_test_dump";
  rs1.save(dump_file);

  Resultset rs3;
  rs3.load(dump_file);

  ensure("Test save/load", rs1 == rs3);
}

END_TESTS
