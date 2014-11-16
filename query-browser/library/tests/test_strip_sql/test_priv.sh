
check()
{
  file_name=$1
  test_prompt=$2

  name_test_file="t/${file_name}.test"
  name_result_file="r/${file_name}.result"
  name_etalon_file="r/${file_name}.result.etalon"
  name_diff_file="r/${file_name}.diff"

  ./test_strip_sql $name_test_file > $name_result_file

  compare_results $name_etalon_file $name_result_file $name_diff_file
  fc_result=$?
  print_test_res "$test_prompt" $fc_result
}

print_title "test strip sql-query from 3d language"

check common "common tests"

print_footer
exit $one_of_tests_failed
