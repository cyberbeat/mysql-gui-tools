
// This file contains common TUT test cases for myx_utils_function.c
//
//

//----------------------------------------------------------------------------------------------------------------------

#include "test.h"
#include "myx_util_public_interface.h"

// Private test data.
BEGIN_TEST_DATA_CLASS(module1_utility_functions)
protected:
END_TEST_DATA_CLASS

//----------------------------------------------------------------------------------------------------------------------

TEST_MODULE(module1_utility_functions, "Common test suite, utility library");

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(5)
{
  char* result= escape_string(NULL);
  ensure("Escaping NULL string", result == NULL);

  result= escape_string("");
  ensure("Escaping empty string", result != NULL);
  ensure("Escaping empty string", strlen(result) == 0);         
  g_free(result);

  result= escape_string("a");
  ensure("Escaping single letter string", strcmp(result, "a") == 0);
  g_free(result);

  result= escape_string("'");           
  ensure("Escaping single letter string", strcmp(result, "\\'") == 0);
  g_free(result);

  result= escape_string("''''''''''");
  ensure("Escaping string with all the same special character", strcmp(result, "\\'\\'\\'\\'\\'\\'\\'\\'\\'\\'") == 0);
  g_free(result);

  result= escape_string("'\"\x8\n\r\t\x1a\\");
  ensure("Escaping string containing all recoginized special chars", strcmp(result, "\\'\\\"\\b\\n\\r\\t\\Z\\\\") == 0);
  g_free(result);

  result= escape_string("'A\"B\x8²\nD\rE\tF\x1aG\\H");
  ensure("Escaping string with all special chars interleaved with normal chars",
    strcmp(result, "\\'A\\\"B\\b²\\nD\\rE\\tF\\ZG\\\\H") == 0);
  g_free(result);

  result= escape_string("ABCDEFG\t");
  ensure("Escaping string with only one special char", strcmp(result, "ABCDEFG\\t") == 0);
  g_free(result);

  result= escape_string("\nG\t");
  ensure("Escaping string with only one normal char", strcmp(result, "\\nG\\t") == 0);
  g_free(result);

  result= escape_string("abcdefg");
  ensure("Escaping string with only normal chars", strcmp(result, "abcdefg") == 0);
  g_free(result);

  result= escape_string("abc%d_efg");
  ensure("Escaping string with wild chars not for search", strcmp(result, "abc%d_efg") == 0);
  g_free(result);

  result= escape_string_for_search("abc%d_efg");
  ensure("Escaping string with wild chars for search", strcmp(result, "abc\\%d\\_efg") == 0);
  g_free(result);

}

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(10)
{
  char* result= baseconv(0, 0);
  ensure("Integer to string conversion", result == NULL);

  result= baseconv(0, 100);
  ensure("Integer to string conversion", result == NULL);

  result= baseconv(0, 10);
  ensure("Integer to string conversion", strcmp(result, "0") == 0);
  g_free(result);

  // A small number.
  result= baseconv(3, 2);
  ensure("Integer to string conversion", strcmp(result, "11") == 0);
  g_free(result);

  result= baseconv(3, 3);
  ensure("Integer to string conversion", strcmp(result, "10") == 0);
  g_free(result);

  result= baseconv(3, 4);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 5);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 6);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 7);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 8);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 9);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 10);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 11);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 12);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 13);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 14);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 15);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  result= baseconv(3, 16);
  ensure("Integer to string conversion", strcmp(result, "3") == 0);
  g_free(result);

  // A prime number.
  result= baseconv(1020389, 2);
  ensure("Integer to string conversion", strcmp(result, "11111001000111100101") == 0);
  g_free(result);

  result= baseconv(1020389, 3);
  ensure("Integer to string conversion", strcmp(result, "1220211201012") == 0);
  g_free(result);

  result= baseconv(1020389, 4);
  ensure("Integer to string conversion", strcmp(result, "3321013211") == 0);
  g_free(result);

  result= baseconv(1020389, 5);
  ensure("Integer to string conversion", strcmp(result, "230123024") == 0);
  g_free(result);

  result= baseconv(1020389, 6);
  ensure("Integer to string conversion", strcmp(result, "33512005") == 0);
  g_free(result);

  result= baseconv(1020389, 7);
  ensure("Integer to string conversion", strcmp(result, "11446616") == 0);
  g_free(result);

  result= baseconv(1020389, 8);
  ensure("Integer to string conversion", strcmp(result, "3710745") == 0);
  g_free(result);

  result= baseconv(1020389, 9);
  ensure("Integer to string conversion", strcmp(result, "1824635") == 0);
  g_free(result);

  result= baseconv(1020389, 10);
  ensure("Integer to string conversion", strcmp(result, "1020389") == 0);
  g_free(result);

  result= baseconv(1020389, 11);
  ensure("Integer to string conversion", strcmp(result, "6376a7") == 0);
  g_free(result);

  result= baseconv(1020389, 12);
  ensure("Integer to string conversion", strcmp(result, "412605") == 0);
  g_free(result);

  result= baseconv(1020389, 13);
  ensure("Integer to string conversion", strcmp(result, "2995a6") == 0);
  g_free(result);

  result= baseconv(1020389, 14);
  ensure("Integer to string conversion", strcmp(result, "1c7c0d") == 0);
  g_free(result);

  result= baseconv(1020389, 15);
  ensure("Integer to string conversion", strcmp(result, "15250e") == 0);
  g_free(result);

  result= baseconv(1020389, 16);
  ensure("Integer to string conversion", strcmp(result, "f91e5") == 0);
  g_free(result);

  // A straight big number.
  result= baseconv(1000000, 2);
  ensure("Integer to string conversion", strcmp(result, "11110100001001000000") == 0);
  g_free(result);

  result= baseconv(1000000, 3);
  ensure("Integer to string conversion", strcmp(result, "1212210202001") == 0);
  g_free(result);

  result= baseconv(1000000, 4);
  ensure("Integer to string conversion", strcmp(result, "3310021000") == 0);
  g_free(result);

  result= baseconv(1000000, 5);
  ensure("Integer to string conversion", strcmp(result, "224000000") == 0);
  g_free(result);

  result= baseconv(1000000, 6);
  ensure("Integer to string conversion", strcmp(result, "33233344") == 0);
  g_free(result);

  result= baseconv(1000000, 7);
  ensure("Integer to string conversion", strcmp(result, "11333311") == 0);
  g_free(result);

  result= baseconv(1000000, 8);
  ensure("Integer to string conversion", strcmp(result, "3641100") == 0);
  g_free(result);

  result= baseconv(1000000, 9);
  ensure("Integer to string conversion", strcmp(result, "1783661") == 0);
  g_free(result);

  result= baseconv(1000000, 10);
  ensure("Integer to string conversion", strcmp(result, "1000000") == 0);
  g_free(result);

  result= baseconv(1000000, 11);
  ensure("Integer to string conversion", strcmp(result, "623351") == 0);
  g_free(result);

  result= baseconv(1000000, 12);
  ensure("Integer to string conversion", strcmp(result, "402854") == 0);
  g_free(result);

  result= baseconv(1000000, 13);
  ensure("Integer to string conversion", strcmp(result, "290221") == 0);
  g_free(result);

  result= baseconv(1000000, 14);
  ensure("Integer to string conversion", strcmp(result, "1c0608") == 0);
  g_free(result);

  result= baseconv(1000000, 15);
  ensure("Integer to string conversion", strcmp(result, "14b46a") == 0);
  g_free(result);

  result= baseconv(1000000, 16);
  ensure("Integer to string conversion", strcmp(result, "f4240") == 0);
  g_free(result);

  // A really big number (actually the biggest possible).
  result= baseconv(18446744073709551615LL, 2);
  ensure("Integer to string conversion", strcmp(result, "1111111111111111111111111111111111111111111111111111111111111111") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 3);
  ensure("Integer to string conversion", strcmp(result, "11112220022122120101211020120210210211220") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 4);
  ensure("Integer to string conversion", strcmp(result, "33333333333333333333333333333333") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 5);
  ensure("Integer to string conversion", strcmp(result, "2214220303114400424121122430") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 6);
  ensure("Integer to string conversion", strcmp(result, "3520522010102100444244423") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 7);
  ensure("Integer to string conversion", strcmp(result, "45012021522523134134601") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 8);
  ensure("Integer to string conversion", strcmp(result, "1777777777777777777777") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 9);
  ensure("Integer to string conversion", strcmp(result, "145808576354216723756") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 10);
  ensure("Integer to string conversion", strcmp(result, "18446744073709551615") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 11);
  ensure("Integer to string conversion", strcmp(result, "335500516a429071284") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 12);
  ensure("Integer to string conversion", strcmp(result, "839365134a2a240713") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 13);
  ensure("Integer to string conversion", strcmp(result, "219505a9511a867b72") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 14);
  ensure("Integer to string conversion", strcmp(result, "8681049adb03db171") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 15);
  ensure("Integer to string conversion", strcmp(result, "2c1d56b648c6cd110") == 0);
  g_free(result);

  result= baseconv(18446744073709551615LL, 16);
  ensure("Integer to string conversion", strcmp(result, "ffffffffffffffff") == 0);
  g_free(result);
}

//----------------------------------------------------------------------------------------------------------------------

END_TESTS;

//----------------------------------------------------------------------------------------------------------------------

