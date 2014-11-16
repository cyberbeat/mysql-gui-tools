
function check(server,query,version,test_prompt,test_prefixes)
{
  if (!server.enabled)
  {
    print_test_skipped(test_prompt)
  }
  else
  {
    var unzip_fname= path_to_top + "mysql-gui-win-res\\bin\\unzip.exe"
    if (!fso.FileExists(unzip_fname))
      unzip_fname=      path_to_top + "\\utils\\unzip.exe"
    unzip_fname= "..\\" + unzip_fname
    var zip_name=         query + ".test.zip"
    var connection_name=      "t\\connection_to_" + version + ".test.connection.xml"
    var prepared_connection=  "t\\connection_to_" + version + ".test.prepared.connection.xml"
    var test_name=            "t\\" + query + ".test.sql"
    var name_result_file=     "r\\" + query + "_" + version + ".result"
    var name_etalon_file=     "r\\" + query + "_" + version + ".result.etalon"
    var name_diff_file=       "r\\" + query + "_" + version + ".diff"

    var old_directory= shell.CurrentDirectory
    shell.CurrentDirectory= old_directory + "\\t"
    shell.Run(unzip_fname + " -o " + zip_name,0,true)
    shell.CurrentDirectory= old_directory

    server.prepare_connection_file(connection_name,prepared_connection)
    exec_to_file("Debug\\test_query_analyze.exe " + prepared_connection + " " + test_name + " " + test_prefixes,name_result_file)
    fso.GetFile(prepared_connection).Delete()

    if (fso.FileExists(test_name))
      fso.GetFile(test_name).Delete()

    var fc_result= compare_results(name_etalon_file,name_result_file,name_diff_file)
    print_test_res(test_prompt,fc_result)
  }
}

copy_dlls()

print_title("test mysql query analyze")

server_40.safe_start()
server_41.safe_start()
server_50.safe_start()

check(server_40,"bugs", "mysql40", "problem queries from bugs  on mysql-4.0")
check(server_41,"bugs", "mysql41", "problem queries from bugs  on mysql-4.1")
check(server_50,"bugs", "mysql50", "problem queries from bugs  on mysql-5.0")

check(server_40,"simple_select_user", "mysql40", "simple select_user         on mysql-4.0")
check(server_41,"simple_select_user", "mysql41", "simple select_user         on mysql-4.1")
check(server_41,"large_select",       "mysql41", "select from list of tables on mysql-4.1")
check(server_41,"is_editable",        "mysql41", "is editable                on mysql-4.1")

check(server_50,"simple_select_user", "mysql50", "simple select_user         on mysql-5.0")
check(server_50,"large_select",       "mysql50", "select from list of tables on mysql-5.0")
check(server_50,"is_editable",        "mysql50", "is editable                on mysql-5.0")

check(server_41,"native",             "mysql41", "native queries             on mysql-4.1")
check(server_41,"swap_clauses",       "mysql41", "swap clauses               on mysql-4.1")

check(server_50,"native",             "mysql50", "native queries             on mysql-5.0")
check(server_50,"swap_clauses",       "mysql50", "swap clauses               on mysql-5.0")

server_41.execute_script("t/prepare_innodb.sql")
server_50.execute_script("t/prepare_innodb.sql")

check(server_41,"quote_no",     "mysql41", "queries without quotas     on mysql-4.1")
check(server_41,"quote_mysql",  "mysql41", "queries with mysql quotas  on mysql-4.1")
check(server_41,"quote_ansi",   "mysql41", "queries with ansi quotas   on mysql-4.1")
check(server_41,"comments",     "mysql41", "query with comments        on mysql-4.1")

check(server_50,"quote_no",     "mysql50", "queries without quotas     on mysql-5.0")
check(server_50,"quote_mysql",  "mysql50", "queries with mysql quotas  on mysql-5.0")
check(server_50,"quote_ansi",   "mysql50", "queries with ansi quotas   on mysql-5.0")
check(server_50,"comments",     "mysql50", "query with comments        on mysql-5.0")

print_title("test mysql query analyze with ansi quotas")

check(server_41,"quote_no",     "mysql41_ansi", "queries without quotas     on mysql-4.1")
check(server_41,"quote_mysql",  "mysql41_ansi", "queries with mysql quotas  on mysql-4.1")
check(server_41,"quote_ansi",   "mysql41_ansi", "queries with ansi quotas   on mysql-4.1")

check(server_41,"utf8",         "mysql41",      "queries with utf8 symbols  on mysql-4.1")

check(server_50,"quote_no",     "mysql50_ansi", "queries without quotas     on mysql-5.0")
check(server_50,"quote_mysql",  "mysql50_ansi", "queries with mysql quotas  on mysql-5.0")
check(server_50,"quote_ansi",   "mysql50_ansi", "queries with ansi quotas   on mysql-5.0")

check(server_50,"utf8",         "mysql50",      "queries with utf8 symbols  on mysql-5.0")

server_41.execute_script("t/clean_innodb.sql")
server_50.execute_script("t/clean_innodb.sql")

server_40.safe_stop()
server_41.safe_stop()
server_50.safe_stop()

clear_dlls()
print_footer()
