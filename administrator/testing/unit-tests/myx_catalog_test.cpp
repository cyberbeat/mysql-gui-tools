
// This file contains MA TUT test cases for myx_backup.c
//
//

//----------------------------------------------------------------------------------------------------------------------

#include <fstream>

#include "test.h"
#include "myx_admin_library.h"
#include "myx_shared_util_functions.h"

// Private test data.
BEGIN_TEST_DATA_CLASS(mysql_administrator_catalog)
protected:
  Test_connection* connection;
END_TEST_DATA_CLASS

TEST_MODULE(mysql_administrator_catalog, "MySQL Administrator test suite");

//----------------------------------------------------------------------------------------------------------------------

/**
  Test for bug http://bugs.mysql.com/bug.php?id=19824
  Using a connection with non-existing default schema
*/

TEST_FUNCTION(15)
{
  connection= test_group_singleton.get_connection();
  ensure("Server connection", connection != NULL);
  MYSQL* mysql= connection->get_mysql();

  MYX_USER_CONNECTION *uc= new MYX_USER_CONNECTION;
  uc->connection_name= "test_connection_19824";
  uc->username= const_cast<char *>(test_params->get_user_name());
  uc->password= const_cast<char *>(test_params->get_password());
  uc->hostname= const_cast<char *>(test_params->get_host_name());
  uc->port= test_params->get_port();
  uc->schema= "test_19824_nonexistent_schema";
  uc->advanced_options_num= 0;
  uc->advanced_options= NULL;
  uc->storage_path= NULL;
  uc->notes= NULL;
  uc->connection_type= MYX_MYSQL_CONN;
  uc->storage_type= MYX_HISTORY_USER_CONNECTION;

  // auto-releaser is not used here, because MYX_USER_CONNECTION
  // is inited not in it's usual way, so having a common function
  // for this case would be confusing
  try
  {
    ensure("connecting to server with non-existing default schema",
      myx_connect_to_instance(uc, mysql) == 0);
    delete uc;      
  }
  catch(...)
  {
    delete uc;
    throw;
  }
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(500)
{
  // Cleanup
}

//----------------------------------------------------------------------------------------------------------------------

END_TESTS;

//----------------------------------------------------------------------------------------------------------------------

