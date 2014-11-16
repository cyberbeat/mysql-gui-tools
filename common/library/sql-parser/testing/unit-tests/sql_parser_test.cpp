
// This file contains common TUT test cases for myx_database_model.c
//
//

//----------------------------------------------------------------------------------------------------------------------

#include "test.h"
//#include "myx_public_interface.h"
#include "myx_sql_parser_public_interface.h"
//#include "myx_library.h"
//#include <fstream>

// Private test data.
BEGIN_TEST_DATA_CLASS(sql_parser_test)
protected:
  Test_connection* connection;
END_TEST_DATA_CLASS

//----------------------------------------------------------------------------------------------------------------------

TEST_MODULE(module_sql_parser_test, "SQL parser test suite");

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(5)
{
  //connection= test_group_singleton.get_connection();
  //ensure("Server connection", connection != NULL);
}

//----------------------------------------------------------------------------------------------------------------------

/*
*/


static bool test_10_data[5]= {false, false, false, false, true};

int process_split_sql_commands(const char *sql, void *user_data)
{
  static int c= 0;

  static const char *sqls[] = {
    "DROP TABLE IF EXISTS `db1`.`customer's orders`",

    "CREATE TABLE `db1`.`customer's orders` ("
    "`Level` VARCHAR(6) NOT NULL,"
    "`LevelDescription` VARCHAR(255) NULL,"
    "`LevelRank` VARCHAR(6) NULL,"
    "`LevelText` LONGTEXT NULL,"
    "PRIMARY KEY (`Level`)) ENGINE = INNODB",

    "DROP TABLE IF EXISTS `db1`.`customer`",

    "CREATE TABLE `db1`.`customer` (`idcustomer`"
    " INT(10) NOT NULL,`Last Year's Sales` "
    "DECIMAL(19, 4) NULL, PRIMARY KEY (`idcustomer`)) "
    "ENGINE = INNODB"
  };

  if((c >= 0) && (c < 5))
  {
    test_10_data[c]= (strcmp(sqls[c], sql) == 0);
    ++c;
  }
  else
  {
    test_10_data[4]= false;
  }

  return 0;
}

TEST_FUNCTION(10)
{
  MYX_LIB_ERROR error_code= MYX_NO_ERROR;

  static const char *sql1=
    "DROP TABLE IF EXISTS `db1`.`customer's orders`;"
    "CREATE TABLE `db1`.`customer's orders` ("
    "`Level` VARCHAR(6) NOT NULL,"
    "`LevelDescription` VARCHAR(255) NULL,"
    "`LevelRank` VARCHAR(6) NULL,"
    "`LevelText` LONGTEXT NULL,"
    "PRIMARY KEY (`Level`)) ENGINE = INNODB;";


  static const char *sql2=
    "DROP TABLE IF EXISTS `db1`.`customer`;"
    "CREATE TABLE `db1`.`customer` (`idcustomer`"
    " INT(10) NOT NULL,`Last Year's Sales` "
    "DECIMAL(19, 4) NULL, PRIMARY KEY (`idcustomer`)) "
    "ENGINE = INNODB;";


    myx_process_sql_statements(sql1, &process_split_sql_commands, NULL, MYX_SPM_DELIMS_REQUIRED);
    myx_process_sql_statements(sql2, &process_split_sql_commands, NULL, MYX_SPM_DELIMS_REQUIRED);    

  ensure("Parser quote test failed", test_10_data[0] && test_10_data[1] && test_10_data[2] && test_10_data[3] && test_10_data[4]);
}

//----------------------------------------------------------------------------------------------------------------------

END_TESTS;

//----------------------------------------------------------------------------------------------------------------------

