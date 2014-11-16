


#include "myx_public_interface.h"

#if 0
void do_test(MYSQL *mysql)
{
  MYX_STRINGLIST *res;
  
  res=myx_check_table_relationship(mysql, "product_order", NULL, "customer", NULL);
  if (res)
  {
    int i;
    g_message("result:");
    for (i= 0; i < res->strings_num; i++)
      g_message("%s",res->strings[i]);
  }
}
#endif


char *myx_join_table_to_sql(MYSQL *mysql, char *script,
                            char *table)
{
  char *nscript= "";
  
  
  
  
  return nscript;
}


void test_subst()
{  
  g_message("%s",
	subst_pcre("'([^']*)'", "value=\\1", 0,8, "blabla'hooray'ewqeq"));


  g_message(mask_out_string_re(g_strdup(
	"select * from bla where ((a=b) and bla in ( select a from b where a in (select x from z (a=0) and (b=1)))) and bla blo;"), 
	"\\(\\s*select\\s",'(',')','x'));

}


void test_export(MYSQL *mysql)
{
  MYX_RESULTSET *rs;
  MYX_LIB_ERROR error;
  MYX_TABLE_EXPORTER *exporter= myx_get_table_exporter("XML");
  MYX_TABLE_EXPORTER_INFO *info;
  MYX_STRINGLIST sl;

  if (!exporter) puts("No exporter found"), exit(1);

  sl.strings_num= 0;

  rs= myx_query_execute(mysql, "select * from Country", 0,
                        &sl, &error,
                        NULL, NULL, NULL, NULL);
  if (!rs) puts("query has no result"), exit(1);

  info= (*exporter->init)();

  myx_export_resultset(mysql, info, "/tmp/rs.html", "", rs, 
			"select * from City where CountryCode=:Code");
}


static int output(const char *text, void *ud)
{
  printf("%s", text);
  return 0;
}

void test_shell(MYSQL *mysql)
{
  MYX_TEXT_SHELL *shell= myx_init_text_shell(mysql);
  char command[1024];

  myx_ts_set_output_callback(shell, NULL, output);

  for (;;)
  {
    printf(">"); fflush(stdout);
    if (!fgets(command, sizeof(command), stdin)) break;
    myx_ts_execute_command(shell, command);
  }
}


#include "myx_simple_sql_parsing.h"
void test_sql_parser()
{
MYX_SQL_PARSE_ENVIRONMENT *env;
MYX_STRING_WRAPPER *wra;
char *buf= (char*)g_malloc(200);
int siz=200;
int err=0;
char *text="DELIMITER $$\n"
"\n"
"DROP PROCEDURE IF EXISTS `test`.`tproc`$$\n"
"CREATE PROCEDURE `test`.`tproc` ()\n"
"BEGIN\n"
"	select 'bla';\n"
"	select 'ble';\n"
"	select 'blo';\n"
"END$$\n"
"\n"
"DELIMITER ;\n";

  wra=  myx_new_string_wrapper(text);
  env= myx_new_sql_parse_environment();

  while (myx_get_next_sql_statement(env, &buf, &siz, 1, &err, myx_read_char_from_string,
					wra))
  {
	g_message("%i %i %.*s", (int)env->stmt_begin_char, (int)env->stmt_end_char,
		(int)env->stmt_end_char-(int)env->stmt_begin_char, 
		text+env->stmt_begin_char);

  }
  exit(1);
}


int main()
{
  MYSQL *mysql= myx_mysql_init();
  MYX_USER_CONNECTION con;
MYX_RESULTSET *rs;
MYX_STRINGLIST sl; sl.strings_num=0;
MYX_LIB_ERROR error;


  test_sql_parser();


  test_subst();


  memset(&con, 0, sizeof(MYX_USER_CONNECTION));
  con.username="root";
  con.password="";
  con.hostname="localhost";
  
  myx_connect_to_instance(&con, mysql);

  myx_use_schema(mysql, "world");

puts("query");
  rs= myx_query_execute(mysql, "call test.simpleproc()", 0,
                        &sl, &error,
                        NULL, NULL, NULL, NULL);

 // test_shell(mysql);
//  sql= "select name from Country";

  test_export(mysql);

  return 0;
}
