
// This file contains common TUT test cases for myx_query.cpp
//
//

//----------------------------------------------------------------------------------------------------------------------

#include "test.h"
#include "myx_query.h"

// Private test data.
BEGIN_TEST_DATA_CLASS(module5_query_test)
protected:
  Test_connection* connection;
END_TEST_DATA_CLASS

//----------------------------------------------------------------------------------------------------------------------

TEST_MODULE(module5_query_test, "Common test suite, base library");

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(5)
{
  connection= test_group_singleton.get_connection();
  ensure("Valid server connection", connection != NULL);
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(10)
{
  Query* query = new Query();
  query->analyze(connection->get_mysql(), "select * from table1");
  ensure("Check if query is a join", !query_is_join(query));
  delete query;

  query = new Query();
  query->analyze(connection->get_mysql(), "select cd.titel, f.name from cd join  (verleihtabelle t JoIn freunde f on"
    "t.freundenr=f.nummer) on cd.nummer=t.cdnummer;");
  ensure("Check if query is a join", query_is_join(query));
  delete query;

  query = new Query();
  query->analyze(connection->get_mysql(), "SELECT Continent, Population, Name FROM Country AS C \nJOin "
    "(SELECT Continent AS Cont, MAX(Population) AS Pop, Code FROM Country GROUP BY Continent) AS M ON C.Continent = "
    "M.Cont AND Population = Pop AND C.Code = M.Code");
  ensure("Check if query is a join", query_is_join(query));
  delete query;
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(15)
{
  Auto_release auto_release;
  MYX_LIB_ERROR error= MYX_NO_ERROR;
  bigint affected_rows;

  // Create test schema and other objects.
  std::string filename= test_group_singleton.get_full_filename("test_m5_t15_1.sql");
  std::ifstream is(filename.c_str());
  connection->multi_query(is);

  // Make sure the test database is removed after the tests.
  auto_release.add_sql(connection, "drop schema bug23012;");

  affected_rows= -5;
  myx_query_execute_direct(connection->get_mysql(), "USE test; SET @a = 1; SET @b = 2; CALL a(@a); CALL b(@b); select * from tab;",
    &error, &affected_rows);
  ensure("Executing multiple statements in one query.", error == MYX_NO_ERROR);
  ensure("Number of rows affected in first of several queries.", affected_rows == 0);

  MYX_CATALOGS* catalogs= myx_get_catalogs(connection->get_mysql());
  ensure("Executing query after multi statement query.", catalogs != NULL);
  myx_free_catalogs(catalogs);
}

//----------------------------------------------------------------------------------------------------------------------

END_TESTS;

//----------------------------------------------------------------------------------------------------------------------

