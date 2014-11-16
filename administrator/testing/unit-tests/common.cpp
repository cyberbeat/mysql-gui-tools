
// This file contains common TUT test cases for MySQL Administrator
//
//

//----------------------------------------------------------------------------------------------------------------------

#include "test.h"
#include "myx_admin_library.h"

TEST_MODULE(module1_common, "MySQL Administrator test suite");

//----------------------------------------------------------------------------------------------------------------------

TEST_FUNCTION(5)
{
  // Sanity checks.
  ensure("Checking correct data structure size", get_bs_status_size() == sizeof(MYX_BS_STATUS));
  ensure("Checking correct data structure size", get_lib_error_size() == sizeof(MYX_ADMIN_LIB_ERROR));

}

//----------------------------------------------------------------------------------------------------------------------

END_TESTS;

//----------------------------------------------------------------------------------------------------------------------

