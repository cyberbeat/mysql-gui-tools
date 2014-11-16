//---------------------------------------------------------------------------

#pragma hdrstop

#include <memory>
#include "test.h" // test.h is located in common/library/test/include

//---------------------------------------------------------------------------

namespace tut
{
  test_runner_singleton runner;
}

Global_test_parameters *test_params= NULL;

int main(int argc, char* argv[])
{
  test_params= new Global_test_parameters(argc, argv);

  tut::mysql_reporter rep(test_params);
  tut::runner.get().set_callback(&rep);
  tut::runner.get().run_tests();

  return 0;
}

//---------------------------------------------------------------------------

