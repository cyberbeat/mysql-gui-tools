
function check(server,query,version,test_prompt)
{
  if (!server.enabled)
  {
    print_test_skipped(test_prompt)
  }
  else
  {
    var connection_name=      "t\\connection_to_" + version + ".test.connection.xml"
    var prepared_connection=  "t\\connection_to_" + version + ".test.prepared.connection.xml"
    var test_name=            "t\\" + query + ".test.sql"
    var name_result_file=     "r\\" + query + "_" + version + ".result"
    var name_etalon_file=     "r\\" + query + "_" + version + ".result.etalon"
    var name_diff_file=       "r\\" + query + "_" + version + ".diff"

    server.prepare_connection_file(connection_name,prepared_connection)
    exec_to_file("Debug\\test_query_composition.exe " + prepared_connection + " " + test_name,name_result_file)
    fso.GetFile(prepared_connection).Delete()

    var fc_result= compare_results(name_etalon_file,name_result_file,name_diff_file)
    print_test_res(test_prompt,fc_result)
  }
}

copy_dlls()

print_title("test mysql query composition")

server_41.safe_start()
server_41.execute_script("t/prepare_innodb.sql")

check(server_41,"create_new_query",           "mysql41", "simple new query                     on mysql-4.1")
check(server_41,"quoted_names",               "mysql41", "using name to quote                  on mysql-4.1")
check(server_41,"using_schema",               "mysql41", "using schema in table name           on mysql-4.1")
check(server_41,"select_add",                 "mysql41", "add table to select list             on mysql-4.1")
check(server_41,"select_left_outer_join",     "mysql41", "add left outer join table to select  on mysql-4.1")
check(server_41,"select_join",                "mysql41", "add table to select                  on mysql-4.1")
check(server_41,"select_join_without_where",  "mysql41", "add table to select without where    on mysql-4.1")
check(server_41,"test_query_without_aliases", "mysql41", "test query with aliasless tables     on mysql-4.1")

check(server_41,"add_column_from",            "mysql41", "add column from                      on mysql-4.1")
check(server_41,"add_column_select",          "mysql41", "add column select                    on mysql-4.1")
check(server_41,"add_column_where",           "mysql41", "add column where                     on mysql-4.1")
check(server_41,"add_column_group",           "mysql41", "add column group                     on mysql-4.1")
check(server_41,"add_column_having",          "mysql41", "add column having                    on mysql-4.1")
check(server_41,"add_column_order",           "mysql41", "add column order                     on mysql-4.1")
check(server_41,"add_column_set",             "mysql41", "add column set                       on mysql-4.1")

check(server_41,"add_column_to_empty",        "mysql41", "add column to empty query            on mysql-4.1")
check(server_41,"test_aliases",               "mysql41", "test aliases assignment              on mysql-4.1")
check(server_41,"innodb_relations",           "mysql41", "test innodb relations                on mysql-4.1")
check(server_41,"using_complex_names",        "mysql41", "test complex names                   on mysql-4.1")
check(server_41,"queries_with_utf8",          "mysql41", "test utf8 names                      on mysql-4.1")
check(server_41,"queries_with_comments",      "mysql41", "test queries with comments           on mysql-4.1")

check(server_41,"using_complex_names",        "mysql41_ansi", "test complex names with ansi mode    on mysql-4.1")

server_41.execute_script("t/clean_innodb.sql")
server_41.safe_stop()

server_50.safe_start()
server_50.execute_script("t/prepare_innodb.sql")

check(server_50,"create_new_query",           "mysql50", "simple new query                     on mysql-5.0")
check(server_50,"quoted_names",               "mysql50", "using name to quote                  on mysql-5.0")
check(server_50,"using_schema",               "mysql50", "using schema in table name           on mysql-5.0")
check(server_50,"select_add",                 "mysql50", "add table to select list             on mysql-5.0")
check(server_50,"select_left_outer_join",     "mysql50", "add left outer join table to select  on mysql-5.0")
check(server_50,"select_join",                "mysql50", "add table to select                  on mysql-5.0")
check(server_50,"select_join_without_where",  "mysql50", "add table to select without where    on mysql-5.0")
check(server_50,"test_query_without_aliases", "mysql50", "test query with aliasless tables     on mysql-5.0")

check(server_50,"add_column_from",            "mysql50", "add column from                      on mysql-5.0")
check(server_50,"add_column_select",          "mysql50", "add column select                    on mysql-5.0")
check(server_50,"add_column_where",           "mysql50", "add column where                     on mysql-5.0")
check(server_50,"add_column_group",           "mysql50", "add column group                     on mysql-5.0")
check(server_50,"add_column_having",          "mysql50", "add column having                    on mysql-5.0")
check(server_50,"add_column_order",           "mysql50", "add column order                     on mysql-5.0")
check(server_50,"add_column_set",             "mysql50", "add column set                       on mysql-5.0")

check(server_50,"add_column_to_empty",        "mysql50", "add column to empty query            on mysql-5.0")
check(server_50,"test_aliases",               "mysql50", "test aliases assignment              on mysql-5.0")
check(server_50,"innodb_relations",           "mysql50", "test innodb relations                on mysql-5.0")
check(server_50,"using_complex_names",        "mysql50", "test complex names                   on mysql-5.0")
check(server_50,"queries_with_utf8",          "mysql50", "test utf8 names                      on mysql-5.0")
check(server_50,"queries_with_comments",      "mysql50", "test queries with comments           on mysql-5.0")

check(server_50,"using_complex_names",        "mysql50_ansi", "test complex names with ansi mode    on mysql-4.1")

server_50.execute_script("t/clean_innodb.sql")
server_50.safe_stop()

clear_dlls()
print_footer()
