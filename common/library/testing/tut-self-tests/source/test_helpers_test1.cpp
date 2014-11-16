//---------------------------------------------------------------------------

#pragma hdrstop

#include "test.h"
//---------------------------------------------------------------------------

/*
  Test suite for routines from test_helpers.h
*/

namespace tut
{

class Test_helpers_test1;

template<> class Test_object_base<Test_helpers_test1>
{
protected:
  int memb;
public:
  Test_object_base<Test_helpers_test1>()
  : memb(1)
  {}
};

} // namespace tut

TEST_MODULE(Test_helpers_test1, "A part of test suite for test_helpers.h");

/*
  Test instantination of local subclasses of Test_object

  DESCRIPTION
    Access members of a local Test_object descendant

*/

TEST_FUNCTION(1)
{
  ensure("class is derived from local template specializaton", memb == 1);
}

END_TESTS
