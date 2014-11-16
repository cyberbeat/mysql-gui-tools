
check()
{
  server=$1
  query=$2
  version=$3
  test_prompt=$4

  load_members $server

  if [ "x$this_enabled" = "x0" ]; then
    print_test_skipped "$test_prompt"
  else
    connection_name="t/connection_to_${version}.test.connection.xml"
    prepared_connection="t/connection_to_${version}.test.prepared.connection.xml"
    test_name="t/${query}.test.sql"
    name_result_file="r/${query}_${version}.result"
    name_etalon_file="r/${query}_${version}.result.etalon"
    name_diff_file="r/${query}_${version}.diff"

    server_prepare_connection_file $server $connection_name $prepared_connection
    ./test_query_composition $prepared_connection $test_name > $name_result_file
    rm $prepared_connection

    compare_results $name_etalon_file $name_result_file $name_diff_file
    fc_result=$?
    print_test_res "$test_prompt" $fc_result
  fi
}

print_title "test mysql query analyze"

server_safe_start server_41

server_execute_script server_41 "t/prepare_innodb.sql"

check server_41 create_new_query           mysql41 "simple new query                     on mysql-4.1"
check server_41 quoted_names               mysql41 "using name to quote                  on mysql-4.1"
check server_41 using_schema               mysql41 "using schema in table name           on mysql-4.1"
check server_41 select_add                 mysql41 "add table to select list             on mysql-4.1"
check server_41 select_left_outer_join     mysql41 "add left outer join table to select  on mysql-4.1"
check server_41 select_join                mysql41 "add table to select                  on mysql-4.1"
check server_41 select_join_without_where  mysql41 "add table to select without where    on mysql-4.1"
check server_41 test_query_without_aliases mysql41 "test query with aliasless tables     on mysql-4.1"

check server_41 add_column_from            mysql41 "add column from                      on mysql-4.1"
check server_41 add_column_select          mysql41 "add column select                    on mysql-4.1"
check server_41 add_column_where           mysql41 "add column where                     on mysql-4.1"
check server_41 add_column_group           mysql41 "add column group                     on mysql-4.1"
check server_41 add_column_having          mysql41 "add column having                    on mysql-4.1"
check server_41 add_column_order           mysql41 "add column order                     on mysql-4.1"
check server_41 add_column_set             mysql41 "add column set                       on mysql-4.1"

check server_41 add_column_to_empty        mysql41 "add column to empty query            on mysql-4.1"
check server_41 test_aliases               mysql41 "test aliases assignment              on mysql-4.1"
check server_41 innodb_relations           mysql41 "test innodb relations                on mysql-4.1"
check server_41 using_complex_names        mysql41 "test complex names                   on mysql-4.1"
check server_41 queries_with_utf8          mysql41 "test utf8 names                      on mysql-4.1"
check server_41 queries_with_comments      mysql41 "test queries with comments           on mysql-4.1"

check server_41 using_complex_names        mysql41_ansi "test complex names with ansi mode    on mysql-4.1"

server_execute_script server_41 "t/clean_innodb.sql"

server_safe_stop server_41

print_footer
exit $one_of_tests_failed
