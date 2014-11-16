#ifndef _TEST_HELPERS_H_
#define _TEST_HELPERS_H_

#include <vector>
#include <list>
#include <stdexcept>
#include <fstream>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <my_global.h>
#include <mysql.h>

// needed for class Auto_release
#include "myx_public_interface.h"

/*
#define ENUM_SIZE_EQUALS_INT_SIZE(enm) (sizeof(enm) == sizeof(int))

#if !ENUM_SIZE_EQUALS_INT_SIZE(library_error)
#error sizeof(enum) < sizeof(int)
#endif
*/

/*
  Global_test_parameters keeps global test environment runtime parameters
  such as test serer connection information
*/

class Global_test_parameters
{
public:
  // result output mode
  enum Output_mode { DEBUGGER, CONSOLE };

private:
  // connection parameters
  const char *host_name;
  short port;
  const char *user_name;
  const char *password;
  std::vector <std::string> pathes;

  // reporting parameters
  bool rep_passed;
  Output_mode out_mode;
  bool wait_on_finish;

  void print_usage(const char *exe_name);
public:
  Global_test_parameters(int argc, char *argv[]);
  ~Global_test_parameters();

  std::string get_full_filename(const std::string& name);
  const char *get_host_name() const;
  short get_port() const;
  const char *get_user_name() const;
  const char *get_password() const;
  bool report_passed() const;
  Output_mode get_output_mode() const;
  bool wait() { return wait_on_finish; };
};

class Resultset;

/*
  Test_connection encapsulates one active test server connection.
  This class serves as factory for result set instances.
*/

class Test_connection
{
  MYSQL *mysql;

public:
   Test_connection(const char *host, short port, const char *user, const char *pass);
  ~Test_connection();

  // Try to avoid using get_mysql() if that's possible. if you lack a function that uses server connection
  // consider adding it to the Test_connection class.
  MYSQL* get_mysql(void) { return mysql; };

  void query(const std::string& query);

  // In multi_query the query string can contain several delimited statements and DELIMITER statements.
  void multi_query(std::istream& is);
  void multi_query(const std::string& q);
  Resultset* new_resultset(const std::string& query);
};

class Resultset
{
  std::vector<char**> rows;
  MYSQL_RES *res;
  int column_count;
  MYSQL_FIELD* fields;
  bool is_active;

  friend bool operator == (const Resultset &rs1, const Resultset &rs2);

protected:
  bool fetch_resultset();

public:
  Resultset(MYSQL_RES *resultset);
  ~Resultset();

  bool compare_to(const Resultset &resultset);
  char *escape(const char *str);
  int get_column_count(void) {return column_count; };
  MYSQL_FIELD* get_column_fields(void) {return fields; };
  bool load(const std::string &file);
  bool save(const std::string &file);
  char *unescape(const char *str);
};

// Every TEST_FUNCTION is a member of a Test_object_base-derived class instance. The class could contain any shared info.
//  Note: template class is fake, it's just to get different template specializations.
namespace tut
{

template<class> class Test_object_base
{
};

}

// Test_group class encapsulates testgroup-wide information. For example it keeps a server connection
// which can be shared among the tests in group.
template<class module>
class Test_group : public tut::test_group<tut::Test_object_base<module> >
{
  Test_connection *c_;
  Global_test_parameters **params_;

  void inc_if_disabled();

public:
  Test_group(Global_test_parameters **params, const char* name, const char *desc)
    : tut::test_group<tut::Test_object_base<module> >(name), c_(NULL), params_(params)
  {
    inc_if_disabled();
  }

  virtual ~Test_group()
  {
    delete c_;
  }

  Test_connection *get_connection()
  {
    if (c_ == NULL)
    {
      c_= new Test_connection(
        (*params_)->get_host_name(),
        (*params_)->get_port(),
        (*params_)->get_user_name(),
        (*params_)->get_password());
    }
    return c_;
  }

  std::string get_full_filename(const std::string& name)
  {
    return (*params_)->get_full_filename(name);
  }

  bool compare_files(const std::string& created, const std::string& reference)
  {
    std::fstream file1(created.c_str(), std::ios::in | std::ios::binary);
    std::string name2 = (*params_)->get_full_filename(reference);
    std::fstream file2(name2.c_str(), std::ios::in | std::ios::binary);
    int line_number= 0;
    std::string line1, line2;

    while (!file1.eof() && !file2.eof()) {

      line_number++;
      getline(file1, line1, '\n');
      getline(file2, line2, '\n');

      // Remove a carriage return character if there is one at the end.
      if (line1[line1.length() - 1] == '\r')
         line1.erase(line1.end() - 1);
      if (line2[line2.length() - 1] == '\r')
         line2.erase(line2.end() - 1);

      // Now compare the lines.
      if (line1 != line2) {
        std::cout << "Files differ at line " << line_number << '\n';
        return false;
      }
    }

    if (!file1.eof() || !file2.eof()) {
      std::cout << "Files have different line count.\n";
      return false;
    }
    return true;
  }

  bool compare_files_old(const std::string& created, const std::string& reference)
  {
    std::fstream file1(created.c_str(), std::ios::in | std::ios::binary);
    std::string name2 = (*params_)->get_full_filename(reference);
    std::fstream file2(name2.c_str(), std::ios::in | std::ios::binary);
    int line= 0;

    bool result= true;
    if (file1 && file2)
    {
      unsigned char *data1= new char[200000];
      unsigned char *data2= new char[200000];
      int current_offset= 0;

      while (!file1.eof() && !file2.eof())
      {
        file1.read(data1, 200000);
        int size1= file1.gcount();
        file2.read(data2, 200000);
        int size2= file2.gcount();
        if (size1 != size2)
        {
          int count= size1 < size2 ? size1 : size2;
          int i= 0;
          while ((count > 0) && (data1[i] == data2[i]))
          {
            --count;
            ++i;
          };

          int point= current_offset + i;
          std::cout << "Data differs at position: " << point << '\n';
          result= false;
          break;
        }
        else
        {

          int i= 0;
          while ((size1 > 0) && (data1[i] == data2[i]))
          {
            --size1;
            ++i;
          };
          if (size1 > 0)
          {
            int point= current_offset + i;
            std::cout << "Data differs at position: " << point << '\n';
            result= false;
            break;
          };
        };

        current_offset += size1;
      };

      delete[] data1;
      delete[] data2;
    }
    else
      result= false;
      
    if (!file1.eof() || !file2.eof())
      result= false;

    file1.close();
    file2.close();
    
    return result;
  }
};

template<class module> void Test_group<module>::inc_if_disabled()
{
  set_disabled(false);
}

#ifdef _WINDOWS
template<> void Test_group<void>::inc_if_disabled()
{
  inc_disabled_groups_count();
  set_disabled(true);
}
#else
template<> void Test_group<void>::inc_if_disabled();
#endif

// Auto_release can be used to automatically free dynamically allocated objects. The usage pattern is the following:
//
// void f()
// {
//   Auto_release ar;
//   char *c= ar.add_g_free((char *)g_malloc(1000));
// }
//
// now c will be automatically freed (g_free-d in this case) when ar goes out of scope.
//
// At the moment only a limited set of datatypes is supported, so feel free to extend the class as needed.

class Auto_release
{
  std::list<char *> pointers;
  std::list<char *> cpp_pointers;
  std::list<MYX_DBM_SERVER_VERSION *> versions;
  std::list<MYX_DBM_TABLE_DATA *> tabledatas;
  std::list<MYX_DBM_DATATYPES *> datatypes;
  std::list<MYX_SCHEMA_STORED_PROCEDURES *> schema_sps;
  std::list<MYX_RESULTSET *> resultsets;
  std::list<MYX_TABLE_EXPORTER_INFO *> expinfos;
  std::list<MYX_ENGINES *> engines;

  typedef std::pair<Test_connection *, std::string> Connection_sql_pair;

  std::list<Connection_sql_pair> sql_list;

public:

  char *add_g_free(char *p) { pointers.push_back(p); return p; }
  char *add_delete(char *p) { cpp_pointers.push_back(p); return p; }
  MYX_DBM_SERVER_VERSION *add(MYX_DBM_SERVER_VERSION *v) { versions.push_back(v); return v; }
  MYX_DBM_TABLE_DATA *add(MYX_DBM_TABLE_DATA *d) { tabledatas.push_back(d); return d; }
  MYX_DBM_DATATYPES *add(MYX_DBM_DATATYPES *t) { datatypes.push_back(t); return t; }
  MYX_SCHEMA_STORED_PROCEDURES *add(MYX_SCHEMA_STORED_PROCEDURES *sp) { schema_sps.push_back(sp); return sp; }
  MYX_RESULTSET *add(MYX_RESULTSET *rs) { resultsets.push_back(rs); return rs; }
  MYX_TABLE_EXPORTER_INFO *add(MYX_TABLE_EXPORTER_INFO *info) { expinfos.push_back(info); return info; }
  MYX_ENGINES *add(MYX_ENGINES *data) { engines.push_back(data); return data; }

  void add_sql(Test_connection *conn, const char *filename) { sql_list.push_back(Connection_sql_pair(conn, std::string(filename))); }


  // TODO: check for NULL before passing to myx_free_*
  ~Auto_release()
  {
    for(std::list<char *>::iterator it= pointers.begin(); it != pointers.end(); it++)
      g_free(*it);
    for(std::list<char *>::iterator it= cpp_pointers.begin(); it != cpp_pointers.end(); it++)
      delete *it;
    for(std::list<MYX_DBM_SERVER_VERSION *>::iterator it= versions.begin(); it != versions.end(); it++)
      myx_dbm_free_server_version(*it);
    for(std::list<MYX_DBM_TABLE_DATA *>::iterator it= tabledatas.begin(); it != tabledatas.end(); it++)
      myx_dbm_free_table_data(*it);
    for(std::list<MYX_DBM_DATATYPES *>::iterator it= datatypes.begin(); it != datatypes.end(); it++)
      if (*it) myx_free_datatype(*it);
    for(std::list<MYX_SCHEMA_STORED_PROCEDURES *>::iterator it= schema_sps.begin(); it != schema_sps.end(); it++)
      myx_free_schema_sps(*it);
    for(std::list<MYX_RESULTSET *>::iterator it= resultsets.begin(); it != resultsets.end(); it++)
      myx_query_free_resultset(*it);
    for(std::list<MYX_TABLE_EXPORTER_INFO *>::iterator it= expinfos.begin(); it != expinfos.end(); it++)
      myx_free_table_exporter_info(*it);

    for(std::list<Connection_sql_pair>::iterator it= sql_list.begin(); it != sql_list.end(); it++)
    {
      Connection_sql_pair pair= *it;
      pair.first->multi_query(pair.second);
    }

    for(std::list<MYX_ENGINES *>::iterator it= engines.begin(); it != engines.end(); it++)
      if (*it) myx_free_engines(*it);
  }
};

#endif // _TEST_HELPERS_H_
