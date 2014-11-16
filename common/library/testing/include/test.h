#ifndef _TEST_H_
#define _TEST_H_

#include <string.h>
#include <string>

#include "tut.h"

#include "test_helpers.h"

#include "tut_helpers.h"

#include "tcp_helpers.h"

#define __TOSTR(c) ___TOSTR(c)
#define ___TOSTR(c) #c

#define BEGIN_TEST_DATA_CLASS(module)   \
   namespace tut {                      \
   class module;                        \
   template<> class Test_object_base<module> {  \

#define TEST_DATA_CONSTRUCTOR(module)   \
  Test_object_base<module>()

#define TEST_DATA_DESTRUCTOR(module)   \
  ~Test_object_base<module>()

#define END_TEST_DATA_CLASS };}

#define TEST_MODULE(module, moddesc)  \
  namespace tut {\
  class module {}; \
  typedef Test_group<module>::object Test_object; \
  static Test_group<module> test_group_singleton(&test_params, __TOSTR(module), moddesc);

#define DISABLED_TEST_MODULE(module, moddesc)  \
  namespace tut {\
  class module {}; \
  typedef Test_group<module>::object Test_object; \
  static Test_group<void> test_group_singleton(&test_params, __TOSTR(module)" [DISABLED]", moddesc);

#define TEST_FUNCTION(number) \
  template<>  \
  template<>  \
  void Test_object::test<number>()

#ifdef _WINDOWS
#define DIR_DELIMITER  '\\'
#else
#define DIR_DELIMITER  '/'
#endif

#define __LOCATION (strrchr(__FILE__":"__TOSTR(__LINE__), DIR_DELIMITER)+1)

#define ensure(text, test) ensure(std::string(__LOCATION).append(": ").append(text).c_str(), (test))
#define ensure_equals(text, v1, v2) ensure_equals(std::string(__LOCATION).append(": ").append(text).c_str(), (v1), (v2))

extern Global_test_parameters *test_params;


// these are needed because g++ complains something about stuff being
// defined in different namespace than declaration
#define END_TESTS }


#endif // _TEST_H_

