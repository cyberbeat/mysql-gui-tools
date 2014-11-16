
// This file contains MA TUT test cases for myx_backup.c
//
//

//----------------------------------------------------------------------------------------------------------------------

#include <fstream>

#include "test.h"
#include "myx_admin_library.h"
#include "myx_shared_util_functions.h"

// Private test data.
BEGIN_TEST_DATA_CLASS(myx_admin_library_test_module)
protected:
  Test_connection* connection;
END_TEST_DATA_CLASS

TEST_MODULE(myx_admin_library_test_module, "MySQL Administrator test suite");

//----------------------------------------------------------------------------------------------------------------------

/*
  Prepares all needed data structures etc. for the tests in this module (including a backup context).
*/

TEST_FUNCTION(5)
{
  connection= test_group_singleton.get_connection();
  ensure("Server connection", connection != NULL);
}

//----------------------------------------------------------------------------------------------------------------------

/*
  Test for bugs #23325 + #23930.
*/
TEST_FUNCTION(10)
{
  int result= myx_set_variable(connection->get_mysql(), "tx_isolation", "REPEATABLE-READ");
  ensure("Setting global string variable", result == 0);

  result= myx_set_variable(connection->get_mysql(), "wait_timeout", "1000");
  ensure("Setting global numeric variable", result == 0);

  result= myx_set_variable(connection->get_mysql(), "flush", "on");
  ensure("Setting global boolean variable", result == 0);
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(500)
{
}

//----------------------------------------------------------------------------------------------------------------------

END_TESTS;

//----------------------------------------------------------------------------------------------------------------------

