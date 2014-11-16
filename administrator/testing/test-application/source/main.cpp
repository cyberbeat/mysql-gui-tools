//---------------------------------------------------------------------------

#pragma hdrstop

#include <memory>
#include "test.h" // test.h is located in common/library/test/include

//---------------------------------------------------------------------------

#include <stdio.h>

namespace tut
{
  test_runner_singleton runner;
}

Global_test_parameters *test_params= NULL;

int main(int argc, char* argv[])
{
  test_params= new Global_test_parameters(argc, argv);

  tut::mysql_reporter reporter(test_params);

  tut::runner.get().set_callback(&reporter);
  tut::runner.get().run_tests();

  if (test_params->wait())
  {
    printf("Tests finished. Press <enter>");
    getc(stdin);
  };

  return (reporter.total_failed == 0) ? 0 : 1;
}

//---------------------------------------------------------------------------

