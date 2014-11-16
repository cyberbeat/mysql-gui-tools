//---------------------------------------------------------------------------

#pragma hdrstop

#include <memory>

#include "test.h" // test.h is located in common/library/test/include

//---------------------------------------------------------------------------

#include <stdio.h>

#ifdef __WIN__
#include <conio.h>
#endif

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

#ifdef __WIN__
  getch();
#endif

  return 0;
}

//---------------------------------------------------------------------------

