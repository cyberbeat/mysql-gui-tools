
check()
{
  server=$1
  query=$2
  version=$3
  test_prompt=$4
  test_prefixes=$5

  load_members $server

  if [ "x$this_enabled" = "x0" ]; then
    print_test_skipped "$test_prompt"
  else
    zip_name="${query}.test.zip"
    connection_name="t/connection_to_${version}.test.connection.xml"
    prepared_connection="t/connection_to_${version}.test.prepared.connection.xml"
    test_name="t/${query}.test.sql"
    name_result_file="r/${query}_${version}.result"
    name_etalon_file="r/${query}_${version}.result.etalon"
    name_diff_file="r/${query}_${version}.diff"

    cd t
    unzip $zip_name > /dev/null
    cd ..

    server_prepare_connection_file $server $connection_name $prepared_connection
    ./test_query_analyze $prepared_connection $test_name $test_prefixes > $name_result_file
    rm $prepared_connection

    rm $test_name

    compare_results $name_etalon_file $name_result_file $name_diff_file
    fc_result=$?
    print_test_res "$test_prompt" $fc_result
  fi
}

print_title "test mysql query analyze"

server_safe_start server_40

check server_40 simple_select_user mysql40 "simple select_user         on mysql-4.0"

server_safe_stop server_40

server_safe_start server_41

check server_41 simple_select_user mysql41 "simple select_user         on mysql-4.1"
check server_41 large_select       mysql41 "select from list of tables on mysql-4.1"
check server_41 is_editable        mysql41 "is editable                on mysql-4.1"

check server_41 native             mysql41 "native queries             on mysql-4.1"
check server_41 swap_clauses       mysql41 "swap clauses               on mysql-4.1"

server_execute_script server_41 "t/prepare_innodb.sql"

check server_41 quote_no     mysql41 "queries without quotas     on mysql-4.1"
check server_41 quote_mysql  mysql41 "queries with mysql quotas  on mysql-4.1"
check server_41 quote_ansi   mysql41 "queries with ansi quotas   on mysql-4.1"
check server_41 comments     mysql41 "query with comments        on mysql-4.1"

print_title "test mysql query analyze with ansi quotas"

check server_41 quote_no     mysql41_ansi "queries without quotas     on mysql-4.1"
check server_41 quote_mysql  mysql41_ansi "queries with mysql quotas  on mysql-4.1"
check server_41 quote_ansi   mysql41_ansi "queries with ansi quotas   on mysql-4.1"

check server_41 utf8         mysql41      "queries with utf8 symbols  on mysql-4.1"

server_execute_script server_41 "t/clean_innodb.sql"

server_safe_stop server_41

print_footer
exit $one_of_tests_failed
