
// This file contains common TUT test cases for myx_recordet.c
//
//

//----------------------------------------------------------------------------------------------------------------------

#include "test.h"
#include "myx_public_interface.h"
#include "myx_library.h"


#include <m_ctype.h> 
// Private test data.
BEGIN_TEST_DATA_CLASS(module6_recordset_test)
protected:
  Test_connection* connection;
END_TEST_DATA_CLASS

//----------------------------------------------------------------------------------------------------------------------

TEST_MODULE(module6_recordset_test, "Recordset test, base library");


//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(5)
{
  connection= test_group_singleton.get_connection();

  ensure("Valid server connection", connection != NULL);
}

//----------------------------------------------------------------------------------------------------------------------

static const char *big_rs_query= "select * from sakila.payment, sakila.staff_list";
static const char *not_so_big_rs_query= "select * from sakila.film";

// bug #11070
// memory usage limit handler
TEST_FUNCTION(10)
{
  unsigned int old_limit= myx_mysql_get_resultset_size_limit(connection->get_mysql());
  MYX_RESULTSET *rset;
  MYX_LIB_ERROR err;

  myx_mysql_limit_resultset_size(connection->get_mysql(), 1); // 1MB

  // check if it aborts the query
  rset= myx_query_execute(connection->get_mysql(),
                          big_rs_query,
                          0,
                          NULL,
                          &err, NULL,
                          NULL,
                          NULL,
                          NULL,
                          NULL);

  ensure_equals("big query with memory limit", err, MYX_MEMORY_LIMIT_EXCEEDED);
  ensure("big query with memory limit", rset != NULL);
  myx_query_free_resultset(rset);

  // check if a smaller resultset runs ok
  rset= myx_query_execute(connection->get_mysql(),
                          not_so_big_rs_query, 
                          0,
                          NULL,
                          &err, NULL,
                          NULL,
                          NULL,
                          NULL,
                          NULL);

  ensure_equals("small query with memory limit", err, MYX_NO_ERROR);
  ensure("small query with memory limit", rset != NULL);
  puts(mysql_error(connection->get_mysql()));
  myx_query_free_resultset(rset);

  myx_mysql_limit_resultset_size(connection->get_mysql(), old_limit);

  // check if it runs ok without the limit
  rset= myx_query_execute(connection->get_mysql(),
                          big_rs_query, 
                          0,
                          NULL,
                          &err, NULL,
                          NULL,
                          NULL,
                          NULL,
                          NULL);

  ensure_equals("big query without memory limit", err, MYX_NO_ERROR);
  ensure("big query without memory limit", rset != NULL);
  myx_query_free_resultset(rset);
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(15)
{
  ubigint result= myx_bit_to_int(NULL);
  ensure_equals("Convert BIT to integer", result, (ubigint)-1LL);

  result= myx_bit_to_int("");
  ensure_equals("Convert BIT to integer", result, (ubigint)-1LL);

  result= myx_bit_to_int("31");
  ensure_equals("Convert BIT to integer", result, 31ULL);

  result= myx_bit_to_int("b'11100'");
  ensure_equals("Convert BIT to integer", result, 28ULL);

  result= myx_bit_to_int("0xFFFE");
  ensure_equals("Convert BIT to integer", result, 65534ULL);

  result= myx_bit_to_int("0xabcde");
  ensure_equals("Convert BIT to integer", result, 703710ULL);

  result= myx_bit_to_int("010");
  ensure_equals("Convert BIT to integer", result, 10ULL);

  result= myx_bit_to_int("0x");
  ensure_equals("Convert BIT to integer", result, 0ULL);

  result= myx_bit_to_int("0xFFFF0000AAAA0000");
  ensure_equals("Convert BIT to integer", result, 18446462601596108800ULL);

}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(20)
{
  // Integer to BIT string conversion.
  // The conversion is based on baseconv from the utilities lib, which has been tested completely in the
  // util functions test module. Hence we only test what is added by myx_int_to_bit.
  char* result= myx_int_to_bit(0);
  ensure_equals("Convert integer to BIT", result, std::string("b'0'"));
  g_free(result);

  result= myx_int_to_bit(1020389);
  ensure_equals("Convert integer to BIT", result, std::string("b'11111001000111100101'"));
  g_free(result);
                                                                            
}

//----------------------------------------------------------------------------------------------------------------------

// bug #19238
// the problem with wrong UPDATE statement generated was because of
// incorrectly determined PK columns is the preceeding SELECT.
// This test checks the correctness of SELECT resultset 
TEST_FUNCTION(25)
{
  static const char *init_query=
    "DROP TABLE IF EXISTS test.test_19238;"
    "CREATE TABLE test.test_19238 (id INT PRIMARY KEY, d VARCHAR(255)) ENGINE=MYISAM;"
  ;

  static const char *cleanup_query=
    "DROP TABLE IF EXISTS test.test_19238"
  ;

  static const char *test_query[]= {
    "SELECT * FROM test.test_19238",
    "SELECT * \nFROM test.test_19238",
    "SELECT * \r\nFROM test.test_19238",
    "SELECT \r\n* \r\nFROM test.test_19238",
    "SELECT \n* \nFROM test.test_19238",
    NULL
  };

  MYX_RESULTSET *rset;
  MYX_LIB_ERROR err;

  Auto_release ar;
  ar.add_sql(connection, cleanup_query);

  connection->multi_query(init_query);

  for(int i= 0; test_query[i] != NULL; i++)
  {
    rset= ar.add(myx_query_execute(connection->get_mysql(),
                                  test_query[i], 1,
                                  NULL, &err, NULL,
                                  NULL, NULL, NULL, NULL));

    ensure("Resultset is NULL", rset);
    ensure("Invalid column number in resultset", rset->columns_num == 2);
    ensure("PK not marked", rset->columns[0].table_column->is_pk == 1);
    ensure("Invalid PK flag", rset->columns[1].table_column->is_pk == 0);
  }
}

//----------------------------------------------------------------------------------------------------------------------

// test for #19000 - PK field(s) is always present in HTML export
// test that output html file doesnt contain PK fields if they where not
// present in original query

TEST_FUNCTION(30)
{
  static const char *init_query=
    "DROP TABLE IF EXISTS test.test_19000;"
    "CREATE TABLE test.test_19000 (id INT PRIMARY KEY, data VARCHAR(255)) ENGINE=MYISAM;"
    "INSERT INTO test.test_19000 VALUES (1, 'data1'), (2, 'data2');"
  ;

  static const char *cleanup_query=
    "DROP TABLE IF EXISTS test.test_19000"
  ;

  static const char *test_query=
      "SELECT data FROM test.test_19000"
  ;

  MYX_RESULTSET *rset;
  MYX_LIB_ERROR err;

  Auto_release ar;
  ar.add_sql(connection, cleanup_query);

  connection->multi_query(init_query);

  MYX_TABLE_EXPORTER_INFO *exp_info= myx_get_table_exporter_info("HTML");

  ensure("Export info for HTML format == NULL", exp_info != NULL);

  rset= ar.add(myx_query_execute(connection->get_mysql(),
                                test_query, 1,
                                NULL, &err, NULL,
                                NULL, NULL, NULL, NULL));

  myx_export_resultset(connection->get_mysql(), exp_info,
                         "../test-data/test_19000.html",
                         "Query $QUERY$, $DATE$",
                         rset,
                         "");

  // need to close the file 
  myx_free_table_exporter_info(exp_info);

  // file is very small
  std::fstream f("../test-data/test_19000.html", std::ios::in | std::ios::binary);

  char *data1= ar.add_delete(new char[200000]);
  f.read(data1, 200000-1);
  size_t count= (size_t)f.gcount();
  f.close();
  remove("../test-data/test_19000.html");

  data1[count]= '\0';
  ensure("Output file contains redundant PK data", strstr(data1, "id") == NULL);
}

//----------------------------------------------------------------------------------------------------------------------

END_TESTS;
