#include "tut.h"
#include "test_helpers.h"
#include "myx_library.h"
#include "MyxStatementParser.h"

//----------------- Static data ----------------------------------------------------------------------------------------

int tut::group_base::disabled_groups_count_= 0;

//----------------- Common definitions -----------------------------------------------------------------------------

#ifndef _WINDOWS
template<> void Test_group<void>::inc_if_disabled()
{
  inc_disabled_groups_count();
  set_disabled(true);
}
#endif

//----------------- Global_test_parameters -----------------------------------------------------------------------------

Global_test_parameters::Global_test_parameters(int argc, char *argv[])
{
  // assign initial values
  host_name = strcpy(new char[sizeof("localhost")], "localhost");
  port = 3306;
  user_name = strcpy(new char[sizeof("root")], "root");
  password = strcpy(new char[sizeof("")], "");

  rep_passed= true;
  wait_on_finish= false;
  out_mode= CONSOLE;

  // read args
  size_t sz;
  for(int i= 1; i < argc; i++) {
    sz= sizeof("--host=")-1;
    if(strncmp("--host=", argv[i], sz) == 0) {
      delete[] host_name;
      host_name= strcpy(new char[strlen(argv[i]) - sz], argv[i] + sz);
      continue;
    }
    sz= sizeof("--port=")-1;
    if(strncmp("--port=", argv[i], sz) == 0) {
      port= static_cast<short>(atoi(argv[i] + sz));
      continue;
    }
    sz= sizeof("--user-name=")-1;
    if(strncmp("--user-name=", argv[i], sz) == 0) {
      delete[] user_name;
      user_name= strcpy(new char[strlen(argv[i]) - sz], argv[i] + sz);
      continue;
    }
    sz= sizeof("--password=")-1;
    if(strncmp("--password=", argv[i], sz) == 0) {
      delete[] password;
      password= strcpy(new char[strlen(argv[i]) - sz], argv[i] + sz);
      continue;
    }
    sz= sizeof("--hide-passed=")-1;
    if(strncmp("--hide-passed=", argv[i], sz) == 0) {
      rep_passed= (static_cast<short>(atoi(argv[i] + sz)) == 0);
      continue;
    }
    sz= sizeof("--output=debugger")-1;
    if(strncmp("--output=debugger", argv[i], sz) == 0) {
      out_mode= DEBUGGER;
      continue;
    }
    sz= sizeof("--data=")-1;
    if(strncmp("--data=", argv[i], sz) == 0) {
      std::string data_path= argv[i] + sz;
      if (data_path.size() == 0)
        data_path = '/';
      char delimiter= data_path[data_path.size() - 1];
      if (delimiter != '/' && delimiter != '\\')
        data_path += '/';
      pathes.push_back(data_path);
      continue;
    }
    if(strcmp("--wait", argv[i]) == 0) {
      wait_on_finish= true; // Wait when all tests have been finished (used for debugging).
      continue;
    }
    if(strcmp("--help", argv[i]) == 0) {
      print_usage(argv[0]); // this function calls exit(0)
      continue;
    }
  }
}

//----------------------------------------------------------------------------------------------------------------------

Global_test_parameters::~Global_test_parameters()
{
  delete[] host_name;
  delete[] user_name;
  delete[] password;
}

//----------------------------------------------------------------------------------------------------------------------

void Global_test_parameters::print_usage(const char *exe_name)
{
  std::cout
       << std::endl
       << exe_name << " - a GUI Tools Test Suite" << std::endl << std::endl
       << "Usage: " << exe_name << " <options>" << std::endl << std::endl
       << "Options:" << std::endl
       << "    --host=<host> - test server host name (default is localhost)" << std::endl << std::endl
       << "    --port=<port> - test server port name (default is 3306)" << std::endl << std::endl
       << "    --user-name=<name> - server user name (default is root)" << std::endl << std::endl
       << "    --password=<password> - server password (default is empty)" << std::endl << std::endl
       << "    --hide-passed=[1|0] - if 1 then dont show info about passed tests" << std::endl
       << "      (default is 0)" << std::endl << std::endl
       << "    --output=[debugger|console] - perform ouput to either debugger interface" << std::endl
       << "      (system dependent, on Windows this is OutputDebugString() )" << std::endl
       << "      or to console (cout) (default is console)" << std::endl << std::endl
       << "    --data=<path-to-data> - Folder to test data used to verify results" << std::endl << std::endl
       << "    --help - show this message" << std::endl
       ;
  exit(1);

}

//----------------------------------------------------------------------------------------------------------------------

const char *Global_test_parameters::get_host_name() const
{
  return host_name;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Tries to find the given file in any of the registered data pathes. If it is found its full path is returned.
 *
 * @param name The name of the file to find. Can even contain a relative path.
 *
 * @return The complete path under which the file was found or an empty string if not. 
 */
std::string Global_test_parameters::get_full_filename(const std::string& name)
{
  for (std::vector<std::string>::const_iterator iterator= pathes.begin(); iterator != pathes.end(); ++iterator)
  {
    std::ifstream file((*iterator + name).c_str());
    if (file)
      return *iterator + name;
  };

  return std::string("");
}

//----------------------------------------------------------------------------------------------------------------------

short Global_test_parameters::get_port() const
{
  return port;
}

//----------------------------------------------------------------------------------------------------------------------

const char *Global_test_parameters::get_user_name() const
{
  return user_name;
}

//----------------------------------------------------------------------------------------------------------------------

const char *Global_test_parameters::get_password() const
{
  return password;
}

//----------------------------------------------------------------------------------------------------------------------

bool Global_test_parameters::report_passed() const
{
  return rep_passed;
}

//----------------------------------------------------------------------------------------------------------------------

Global_test_parameters::Output_mode Global_test_parameters::get_output_mode() const
{
  return out_mode;
}

//----------------- Test_connection ------------------------------------------------------------------------------------

Test_connection::Test_connection(const char *host, short port, const char *user, const char *pass)
{
  // Allocate data plus our private area.
  mysql= (MYSQL*) malloc(sizeof(MYSQL) + sizeof(MYX_MYSQL));
  memset(mysql, 0, sizeof(MYSQL) + sizeof(MYX_MYSQL));

  mysql_init(mysql);

  if (!mysql_real_connect(mysql, host, user, pass, NULL, port, NULL, CLIENT_MULTI_RESULTS | CLIENT_MULTI_STATEMENTS))
    throw std::logic_error(mysql_error(mysql));
}

//----------------------------------------------------------------------------------------------------------------------

Test_connection::~Test_connection()
{
  mysql_close(mysql);
  free(mysql);
}

//----------------------------------------------------------------------------------------------------------------------

void Test_connection::query(const std::string& query)
{
  if (mysql_real_query(mysql, query.c_str(), query.size()) != 0)
    throw std::logic_error(mysql_error(mysql));
}

//----------------------------------------------------------------------------------------------------------------------

static int multi_query_callback(const char* sql, void* user_data)
{
  static_cast<Test_connection *>(user_data)->query(std::string(sql));
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

void Test_connection::multi_query(std::istream& is)
{
  MyxStatementParser p;
  p.process(is, multi_query_callback, this, MYX_SPM_NORMAL_MODE);
}

//----------------------------------------------------------------------------------------------------------------------

void Test_connection::multi_query(const std::string& q)
{
  MyxStatementParser p;
  std::istringstream tmp(q);
  p.process(tmp, multi_query_callback, this, MYX_SPM_NORMAL_MODE);
}

//----------------------------------------------------------------------------------------------------------------------

Resultset* Test_connection::new_resultset(const std::string& q)
{
  query(q);

  MYSQL_RES *res= mysql_use_result(mysql);
  if (!res)
    return NULL;

  return new Resultset(res);
}

//----------------- Resultset ------------------------------------------------------------------------------------------

Resultset::Resultset(MYSQL_RES *resultset): res(resultset)
{
  if(!fetch_resultset())
    throw std::logic_error("Cannot fetch resultset");
}

//----------------------------------------------------------------------------------------------------------------------

Resultset::~Resultset()
{
  for (std::vector<char**>::iterator iter= rows.begin();
     iter != rows.end(); ++iter)
  {
    char **cells= *iter;
    for (int i= 0; i < column_count; i++)
    {
      if (cells[i])
        free(cells[i]);
    }
    if (cells)
      free(cells);
  }
  mysql_free_result(res);
}

//----------------------------------------------------------------------------------------------------------------------

bool Resultset::fetch_resultset()
{
  MYSQL_ROW row;

  column_count= mysql_num_fields(res);
  fields= mysql_fetch_fields(res);

  while ((row= mysql_fetch_row(res)) != NULL)
  {
    char **cells= (char**)calloc(sizeof(char*), column_count);
    for (int i= 0; i < column_count; i++)
      cells[i]= escape(row[i]);
    rows.push_back(cells);
  }
  return true;
}

//----------------------------------------------------------------------------------------------------------------------

char* Resultset::escape(const char *str)
{
  int len= 0;
  char *res;
  static char hexchars[]= "0123456789abcdef";

  for (int i= 0; str[i]; i++)
  {
    if (str[i]==';' || str[i]=='\r' || str[i]=='\n' || str[i]=='\\')
      len+= 4;
    else
      len++;
  }
  res= (char*)malloc(len+1);
  int j= 0;
  for (int i= 0; str[i]; i++)
  {
    if (str[i]==';' || str[i]=='\r' || str[i]=='\n' || str[i]=='\\')
    {
      res[j++]= '\\';
      res[j++]= 'x';
      res[j++]= hexchars[((unsigned char*)str)[i]&0xf];
      res[j++]= hexchars[((unsigned char*)str)[i]>>4];
    }
    else
      res[j++]= str[i];
  }
  res[j]= 0;
  return res;
}

//----------------------------------------------------------------------------------------------------------------------

char* Resultset::unescape(const char *str)
{
  char *res= (char*)malloc(strlen(str)+1);
  int j= 0;
  for (int i= 0; str[i]; i++)
  {
    if (str[i]=='\\' && str[i+1]=='x' && str[i+2] && str[i+3])
  {
    res[j++]= (char)strtoul(str+2, NULL, 16);
  }
  else
    res[j++]= str[i];
  }
  res[j]= 0;
  return res;
}

//----------------------------------------------------------------------------------------------------------------------

bool Resultset::load(const std::string &file)
{
  FILE *f= fopen(file.c_str(), "r");
  char *buffer;
  int buflen= 10000;
  if (!f)
    return false;

  fields= NULL;
  buffer= (char*)malloc(buflen);
  if (!fgets(buffer, buflen, f))
    goto error;
  if (sscanf(buffer, "%i", &column_count)!=1)
    goto error;
  if (column_count == 0)
    return false;

  while (fgets(buffer, buflen, f))
  {
    char **row= (char**)calloc(sizeof(char*), column_count);
    char *ptr= strtok(buffer, ";\n");
    int i= 0;
    do
    {
      row[i++]= unescape(ptr);
      ptr= strtok(NULL, ";\n");
    } while (ptr);
    rows.push_back(row);
  }
  fclose(f);
  free(buffer);
  return true;

error:
  fclose(f);
  free(buffer);
  return false;
}

//----------------------------------------------------------------------------------------------------------------------

bool Resultset::save(const std::string &file)
{
  FILE *f= fopen(file.c_str(), "w+");
  if (!f)
    return false;

  fprintf(f, "%i\n", column_count);

  for (std::vector<char**>::iterator iter= rows.begin();
     iter != rows.end(); ++iter)
  {
    char **fields= *iter;
    for (int i= 0; i < column_count; i++)
    {
      if (i == 0)
        fprintf(f, "%s", fields[i]);
      else
        fprintf(f, ";%s", fields[i]);
    }
    fprintf(f, "\n");
  }
  fclose(f);

  return true;
}

//----------------------------------------------------------------------------------------------------------------------

bool Resultset::compare_to(const Resultset &resultset)
{
  return *this == resultset;
}

//----------------------------------------------------------------------------------------------------------------------

inline bool operator == (const Resultset &rs1, const Resultset &rs2)
{
  if (rs1.column_count != rs2.column_count)
    return false;
  if (rs1.rows.size() != rs2.rows.size())
    return false;
  int c= rs1.rows.size();
  for (int i= 0; i < c; i++)
  {
    for (int j= 0; j < rs1.column_count; j++)
    {
      char *v1= rs2.rows[i][j];
      char *v2= rs1.rows[i][j];
      if ((!v1 && !v2) || (v1 && v2 && strcmp(v1, v2)==0))
        ;
      else
        return false;
    }
  }
  return true;
}

//----------------------------------------------------------------------------------------------------------------------


