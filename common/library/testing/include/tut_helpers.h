#ifndef _TUT_HELPERS_H_
#define _TUT_HELPERS_H_

#ifdef _WINDOWS
#include <windows.h>
#else
#include <sys/time.h>
#endif

#include "tut.h"

/*
namespace tut
{
  template <class Data,int MaxTestsInGroup = 50>
  class test_group_with_desc : public test_group<Data, MaxTestsInGroup>
  {
    const char* desc_;

  public:

    test_group_with_desc(const char* name, const char *desc)
    : test_group(name), desc_(desc)
    {}
  }
}
*/

namespace tut
{
  class test_time_point
  {
#ifdef _WINDOWS
    long ticks_;
    test_time_point(DWORD t)
    : ticks_(t)
    {}
  public:
    test_time_point()
    : ticks_(GetTickCount())
    {}
    friend test_time_point operator - (const test_time_point& t1,
                                       const test_time_point& t2);

    friend std::ostream& operator << (ostream& os, const test_time_point& ttp);
#else
  public:
    long ticks_;
    test_time_point(long t)
    : ticks_(t)
    {}
    test_time_point()
    {
      struct timeval tv;
      gettimeofday(&tv, NULL);
      ticks_= tv.tv_sec * 1000 + tv.tv_usec / 1000;
    }
#endif

  };

  inline test_time_point operator - (const test_time_point& t1,
                              const test_time_point& t2)
  {
    return test_time_point(t1.ticks_ - t2.ticks_);
  }

  inline std::ostream& operator << (std::ostream& os, const test_time_point& ttp)
  {
    long sec= ttp.ticks_ / 1000;
    long ms=  ttp.ticks_ % 1000;

    os << sec << "." << ms << " sec" << std::endl;

    return os;
  }

  /*
    debug_stringbuf is used by debug_ostream to
    perform output to an OS debugging output facility
  */

  class debug_stringbuf :
    public std::basic_stringbuf<char, std::char_traits<char> >
  {
  public:

    virtual int sync()
    {
#ifdef _WINDOWS
      OutputDebugString(str().c_str());
#else
      std::cerr << str();  // todo: change this?
#endif
      str("");  // set empty string - clean the buffer
      return 0;
    }
  };

  class debug_ostream : public std::ostream
  {
  public:
    debug_ostream()
    : std::ostream(new debug_stringbuf())
    {}

    ~debug_ostream()
    {
      delete rdbuf();
    }
  };

  class mysql_reporter : public tut::callback
  {
    std::ostream *os;

    size_t per_group_failed;
    size_t per_group_passed;

    test_time_point start_time;
    Global_test_parameters *test_params;

  public:
    size_t total_failed;
    size_t total_passed;

    mysql_reporter(Global_test_parameters *p)
    : os(NULL), test_params(p)
    {
      if(test_params->get_output_mode() == Global_test_parameters::DEBUGGER)
      {
        os= new debug_ostream;
      }
      else
      {
        os= &std::cout;
      }
    }

    ~mysql_reporter()
    {
      if(os != &std::cout)
        delete os;
    }


    void run_started()
    {
      total_failed= total_passed= 0;
      start_time= test_time_point();

      if(group_base::get_disabled_groups_count() > 0)
        *os << "*** Disabled modules: " << group_base::get_disabled_groups_count() << std::endl;
    }

    void run_completed()
    {
      *os << std::endl << "Run complete. [";
      if(test_params->report_passed())
        *os << "passed: " << total_passed;

      if(total_failed > 0)
        *os <<  " failed: " << total_failed;

      *os << "]"
        << std::endl << "Duration: " << (test_time_point() - start_time)
        << std::endl;
    }

    void group_started(const std::string& name)
    {
      per_group_failed= 0;
      per_group_passed= 0;
      *os << std::endl << "Module " << name << std::endl << std::endl;
    }

    void test_completed(const test_result& tr)
    {
      if((tr.result == test_result::ok))
      {
        per_group_passed++;
        if(test_params->report_passed())
          *os << tr.test << ": passed" << std::endl;
      }
      else
      {
        per_group_failed++;
        *os << tr.test << ": failed (" << tr.message << ")" << std::endl;
      }
    }

    void group_completed(const std::string& name)
    {
      *os << std::endl << "Module Results [";
      if(test_params->report_passed())
        *os << "passed: " << per_group_passed;
      if(per_group_failed > 0)
        *os << " failed: " << per_group_failed;
      *os << "]" << std::endl;

      total_failed += per_group_failed;
      total_passed += per_group_passed;
    }
  };
};

#endif // _TUT_HELPERS_H_
