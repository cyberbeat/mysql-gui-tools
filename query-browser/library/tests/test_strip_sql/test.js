
function check(file_name,test_prompt)
{
  var name_test_file=   "t\\" + file_name + ".test"
  var name_result_file= "r\\" + file_name + ".result"
  var name_etalon_file= "r\\" + file_name + ".result.etalon"
  var name_diff_file=   "r\\" + file_name + ".diff"

  exec_to_file("Debug\\test_strip_sql.exe " + name_test_file,name_result_file)

  var fc_result= compare_results(name_etalon_file,name_result_file,name_diff_file)
  print_test_res(test_prompt,fc_result)
}

print_title("test strip sql-query from 3d language")

copy_dlls()

check("common", "common tests")

clear_dlls()
print_footer()
