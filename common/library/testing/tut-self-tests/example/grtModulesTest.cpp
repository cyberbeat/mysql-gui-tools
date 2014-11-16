//---------------------------------------------------------------------------

#pragma hdrstop

#include "test.h"
//---------------------------------------------------------------------------

/*
  GRT Workbench Module Library test suite
*/

TEST_MODULE(grtModulesTest, "GRT Workbench Module Library test suite");

/*
  Test bug #1001

  DESCRIPTION
    See bug description in Eventum
*/

TEST_FUNCTION(1)
{
  ensure("1 == 0 == 0 == 1", 1 == 0 == 0 == 1);
}

/*
  Test server connection class

  DESCRIPTION
    Ensure that it is possible to create a server connection
    and perform queries using Test_connection class provided
    that test_params contains valid connection data.
*/

TEST_FUNCTION(2)
{
  Test_connection *c;
  ensure("Server connection", c= test_group_singleton.get_connection());
  ensure("Query SHOW DATABASES", c->query("SHOW DATABASES") == 0);
}

/*
  Test error reporting
*/

TEST_FUNCTION(3)
{
  ensure("test if 1 == 0", 1 == 0);
}

//---------------------------------------------------------------------------
