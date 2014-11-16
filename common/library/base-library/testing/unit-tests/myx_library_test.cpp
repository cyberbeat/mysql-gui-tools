
// This file contains common TUT test cases for myx_library.c
//
//

//----------------------------------------------------------------------------------------------------------------------

#include "test.h"
#include "myx_public_interface.h"
#include "myx_library.h"


#include <m_ctype.h> 
// Private test data.
BEGIN_TEST_DATA_CLASS(module3_library_test)
protected:
  Test_connection* connection;
END_TEST_DATA_CLASS

//----------------------------------------------------------------------------------------------------------------------

TEST_MODULE(module3_library_test, "Common test suite, base library");

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(5)
{
  // Server related tests.
  connection= test_group_singleton.get_connection();
  ensure("Valid server connection", connection != NULL);
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(10)
{
  // Check private server data and version number handling.
  MYSQL* mysql= connection->get_mysql();
  MYX_MYSQL* priv= myx_mysql_get_private(connection->get_mysql());
  ensure("Computing correct private data offset in MYSQL", ((char*)priv - (char*)mysql) == sizeof(MYSQL));

  myx_get_mysql_version(mysql);
  ensure("Major server version", priv->major_version == 5);
  ensure("Minor server version", priv->minor_version == 0);
  // ensure("Patch level version", priv->patchlevel == 69); don't check patch lvl, makes testing with upgraded servers complicated

  int result= mysql_version_is_later_or_equal_than(mysql, -1, -1);
  ensure("Negative version numbers against 5.0", result == 1);

  result= mysql_version_is_later_or_equal_than(mysql, 0, 0);
  ensure("Zero version numbers against 5.0", result == 1);

  result= mysql_version_is_later_or_equal_than(mysql, 11000000, 1200000);
  ensure("Very big version numbers against 5.0", result == 0);

  result= mysql_version_is_later_or_equal_than(mysql, 5, 0);
  ensure("Version 5.0 against 5.0", result == 1);

  result= mysql_version_is_later_or_equal_than(mysql, 5, 1);
  ensure("Version 5.1 against 5.0", result == 0);

  result= mysql_version_is_later_or_equal_than(mysql, 4, 1);
  ensure("Version 4.1 against 5.0", result == 1);

  result= mysql_version_is_later_or_equal_than(mysql, 6, 0);
  ensure("Version 6.0 against 5.0", result == 0);
}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(15)
{
  // String conversion tests.
  char* result= try_convert_from_cs_to_utf8(NULL, "KOI-8", -1);
  ensure("Convert empty string to UTF-8", result == NULL);

  const char utf8[] = {0x53, 0x63, 0x68, 0xC3, 0xB6, 0x6E, 0x65, 0x20, 0x47, 0x72, 0xC3, 0xBC, 0xC3, 0x9F, 0x65, 0x2C,
    0x20, 0xD0, 0x94, 0xD0, 0x95, 0xD1, 0xA4, 0xD1, 0xAC, 0x2C, 0x20, 0xE1, 0xBE, 0x8D, 0xE1, 0xBE, 0xAA, 0xE2, 0x82,
    0xA1, 0xEF, 0xAD, 0x97, 0};

  result= try_convert_from_cs_to_utf8(utf8, "UTF-8", -1);
  ensure("Convert UTF-8 string to UTF-8", strcmp(result, utf8) == 0);
  g_free(result);

  result= try_convert_from_cs_to_utf8("Schöne Grüße", "Latin1", -1);
  ensure("Convert Latin1 string to UTF-8", strncmp(result, utf8, 15) == 0);
  g_free(result);

  result= try_convert_from_cs_to_utf8("Schöne Grüße", "big5", -1);
  ensure("Convert Latin1 string to UTF-8", result == NULL);

  MYSQL* mysql= connection->get_mysql();
  result= myx_convert_dbstr_utf8(mysql, NULL, -1);
  ensure("Convert empty DB string to UTF-8", result == NULL);

  result= myx_convert_dbstr_utf8(mysql, utf8, -1);
  ensure("Convert UTF-8 DB string to UTF-8", strcmp(result, utf8) == 0);
  g_free(result);

  result= myx_convert_dbstr_utf8(mysql, "Schöne Grüße", -1);
  ensure("Convert UTF-8 DB string to UTF-8", strncmp(result, utf8, 15) == 0);
  g_free(result);

  // Fake a special server charset.
  const char* old_csname= mysql->charset->csname;
  mysql->charset->csname= "Big5";

  // Use a string that can be converted as ISO-8859-1 encoded. That should work (the conv method takes care).
  result= myx_convert_dbstr_utf8(mysql, "Schöne Grüße", -1);
  ensure("Convert DB string to UTF-8 (server charset Big5)", strncmp(result, utf8, 15) == 0);
  g_free(result);

  // Restore old charset name, to allow deallocation.
  mysql->charset->csname= old_csname;
}

//----------------------------------------------------------------------------------------------------------------------

END_TESTS;

//----------------------------------------------------------------------------------------------------------------------

