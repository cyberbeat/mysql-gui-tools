// test_query_analyze.cpp : Defines the entry point for the console application.
//

#ifdef _WINDOWS_
#include "io.h"
#endif
#include <fcntl.h>
#include "stdio.h"
#include "stdlib.h"
#include "myx_public_interface.h"
#include "myx_query.h"

const char * connection_path;
const char * path_to_query;

enum Kind_of_test
{
  kt_simple_test=              0,
  kt_test_all_prefixes=        1,
  kt_test_clauses_permutation= 2
};

Kind_of_test kind_of_test= kt_simple_test;

Kind_of_test parse_kind_of_test(const char *s)
{
  if (!strcmp(s,"simple_test"))          return kt_simple_test;
  if (!strcmp(s,"all_prefixes"))         return kt_test_all_prefixes;
  if (!strcmp(s,"clauses_permutation"))  return kt_test_clauses_permutation;

  return (Kind_of_test)atoi(s);
}

void print_usage()
{
  printf("usage : test_query_analyze.exe <path_to_connection_file> "
         "<path_to_queries> [kind_of_test]\n");
}

void print_tab(unsigned int tab)
{
  for (int i= 0; i!=tab; i++)
    printf(" ");
}

void trace_column(MYX_Q_TABLE_COLUMN * column, unsigned int tab)
{
  print_tab(tab); 
  printf("       column \"%s\", cs(%s) %s\n",
    column->column,column->charset,column->is_pk?"is     pk":"is not pk");
}


void trace_table(MYX_Q_TABLE * table, unsigned int tab)
{
  print_tab(tab); printf("    -------------------------------\n");
  print_tab(tab); printf("    table \"%s\".\"%s\".\"%s\" (%s)\n", 
                         table->catalog,table->schema,table->name,table->alias);
  print_tab(tab); printf("     fullname= \"%s\"\n", table->fullname);
  print_tab(tab); printf("     charset= \"%s\"\n", table->charset);

  print_tab(tab); printf("     columns(%u) : \n", table->columns_num);
  MYX_Q_TABLE_COLUMN * column= table->columns;
  MYX_Q_TABLE_COLUMN * columns_end= column + table->columns_num;
  for (; column!=columns_end; column++)
    trace_column(column,tab);

  print_tab(tab); printf("     pk_columns_num(%u) : \n", table->pk_columns_num);
  MYX_Q_COLUMN * pkcolumn= table->pk_columns;
  MYX_Q_COLUMN * pkcolumns_end= pkcolumn + table->pk_columns_num;
  for (; pkcolumn!=pkcolumns_end; pkcolumn++)
  {
    print_tab(tab);
    printf("    |%s|\n",pkcolumn->column_alias);
  }

  print_tab(tab); printf("     relationship_type=%s\n",
     table->relationship_type==MYX_QTRT_NONE         ? "MYX_QTRT_NONE"   :
     table->relationship_type==MYX_QTRT_ONE_TO_ONE   ? "MYX_QTRT_ONE_TO_ONE" :
     table->relationship_type==MYX_QTRT_ONE_TO_MANY  ? "MYX_QTRT_ONE_TO_MANY" :
                                                       "<wrong>"  );
  print_tab(tab); 
  if (!table->relationship)
  {
    printf("       null relation \n");
  }
  else
  {
    printf("       relation with \"%s\".\"%s\".\"%s\" (%s)\n", 
          table->relationship->catalog,
          table->relationship->schema,
          table->relationship->name,table->relationship->alias);
  }
  print_tab(tab); printf("    -------------------------------\n");
}

void trace_column(MYX_Q_COLUMN * column, unsigned int tab)
{
  print_tab(tab); 
  printf("     column \"%s\".\"%s\".\"%s\" .\"%s\" (alias: %s, is_pk: %d)\n",
    !column->table ? "<<nil>>" : column->table->catalog,
    !column->table ? "<<nil>>" : column->table->schema,
    !column->table ? "<<nil>>" : column->table->name,
    column->column,column->column_alias,
    !column->table_column?0:column->table_column->is_pk);
}

void trace_clause(MYX_Q_CLAUSE * clause, unsigned int tab)
{
  print_tab(tab); printf("    clause (%u,%u) %s\n",
                         clause->start_index,clause->end_index,
                         clause->end_with_linebreak ? 
                         "ends with linebreak" : 
                         "doesn't end with linebreak");
  print_tab(tab); printf("      clause_type %s\n",
         clause->clause_type==MYX_QCT_NO_CLAUSE     ? "MYX_QCT_NO_CLAUSE"     :
         clause->clause_type==MYX_QCT_SELECT_CLAUSE ? "MYX_QCT_SELECT_CLAUSE" :
         clause->clause_type==MYX_QCT_FROM_CLAUSE   ? "MYX_QCT_FROM_CLAUSE"   :
         clause->clause_type==MYX_QCT_WHERE_CLAUSE  ? "MYX_QCT_WHERE_CLAUSE"  :
         clause->clause_type==MYX_QCT_GROUP_CLAUSE  ? "MYX_QCT_GROUP_CLAUSE"  :
         clause->clause_type==MYX_QCT_HAVING_CLAUSE ? "MYX_QCT_HAVING_CLAUSE" :
         clause->clause_type==MYX_QCT_ORDER_CLAUSE  ? "MYX_QCT_ORDER_CLAUSE"  :
         clause->clause_type==MYX_QCT_LIMIT_CLAUSE  ? "MYX_QCT_LIMIT_CLAUSE"  :
         clause->clause_type==MYX_QCT_SET_CLAUSE    ? "MYX_QCT_SET_CLAUSE"    :
         clause->clause_type==MYX_QCT_INTO_CLAUSE   ? "MYX_QCT_INTO_CLAUSE"   :
         clause->clause_type==MYX_QCT_UPDATE_CLAUSE ? "MYX_QCT_UPDATE_CLAUSE" :
                                                      "<wrong>");
  print_tab(tab); printf("      clause : -------------------------\n");
  printf("%s\n",clause->clause);
  print_tab(tab); printf("      ------------------------------------\n");
}

void trace_query(MYX_QUERY * query, unsigned int tab= 0)
{
  print_tab(tab); printf("query : ****************************\n");
  print_tab(tab); printf("  sql : ----------------------------\n");
  printf("%s\n",query->sql);
  if (query->query_type==MYX_QT_SELECT)
  {
    print_tab(tab); printf("------------------------------------\n");
    if (query_is_editable(query,1))
    {
      printf("query_is_editable(query,1)\n");
    }
    else
    {
      printf("!query_is_editable(query,1)\n");
    }
  }
  print_tab(tab); printf("------------------------------------\n");
  print_tab(tab); printf("  query_type : %s\n",
                         query->query_type==MYX_QT_SELECT ? "MYX_QT_SELECT" : 
                         query->query_type==MYX_QT_UPDATE ? "MYX_QT_UPDATE" :
                         query->query_type==MYX_QT_INSERT ? "MYX_QT_INSERT" :
                         query->query_type==MYX_QT_DELETE ? "MYX_QT_DELETE" :
                         query->query_type==MYX_QT_SCRIPT ? "MYX_QT_SCRIPT" :
                         query->query_type==MYX_QT_SHOW   ? "MYX_QT_SHOW"   :
                         query->query_type==MYX_QT_SET    ? "MYX_QT_SET"    : 
                         query->query_type==MYX_QT_UNION  ? "MYX_QT_UNION"  : 
                                                                  "<wrong>");

  print_tab(tab); printf("  options(%u) : \n", query->options_num);
  char ** option= query->options;
  char ** options_end= option + query->options_num;
  for (; option!=options_end; option++)
  {
    print_tab(tab); 
    printf("    |%s|\n",*option);
  }

  print_tab(tab); printf("  tables(%u) : \n", query->tables_num);
  MYX_Q_TABLE * table= query->tables;
  MYX_Q_TABLE * tables_end= table + query->tables_num;
  for (; table!=tables_end; table++)
    trace_table(table,tab);

  print_tab(tab); printf("  pk_columns_added_num %u\n",query->pk_columns_added_num);

  print_tab(tab); printf("  columns(%u)\n", query->columns_num);
  MYX_Q_COLUMN * column= query->columns;
  MYX_Q_COLUMN * columns_end= column + query->columns_num;
  for (; column!=columns_end; column++)
    trace_column(column, tab);

  print_tab(tab); printf("  clauses(%u)\n", query->clauses_num);
  MYX_Q_CLAUSE * clause= query->clauses;
  MYX_Q_CLAUSE * clauses_end= clause + query->clauses_num;
  for (; clause!=clauses_end; clause++)
    trace_clause(clause, tab);

  print_tab(tab); printf("  subqueries(%u)\n", query->subquerys_num);
  MYX_QUERY * subquery= query->subquerys;
  MYX_QUERY * subquerys_end= subquery + query->subquerys_num;
  for (; subquery!=subquerys_end; subquery++)
    trace_query(subquery, tab + 2);

  print_tab(tab); printf("  params_num %u\n",query->params_num);
}

void prepare_line(char * line)
{
  if (line)
  {
    size_t len= strlen(line);
    while (len && (line[len-1]=='\r' || line[len-1]=='\n'))
      line[--len]= 0;
  }
}

void simple_test(MYSQL * mysql, char * query_buffer)
{
  if (!query_buffer || !*query_buffer)
    return;

  MYX_QUERY * query= query_analyze(mysql,query_buffer);
  trace_query(query);
  query_free(query);
}

void test_all_prefixes(MYSQL * mysql, char * query_buffer)
{
  for (char * pos= query_buffer; *pos!=0; pos++)
  {
    char old_char= *pos;
    *pos= 0;
    simple_test(mysql,query_buffer);
    *pos= old_char;
  }
}

char permutation_buffer[16000];
void test_clause_permutation(MYSQL * mysql,
                             const char * query_buffer,
                             MYX_Q_CLAUSE ** permuted_clause,
                             MYX_Q_CLAUSE ** permuted_clauses_end)
{
  permutation_buffer[0]= 0;
  for (MYX_Q_CLAUSE ** pc= permuted_clause;
        pc!=permuted_clauses_end; pc++)
  {
    switch ((*pc)->clause_type)
    {
    case MYX_QCT_SELECT_CLAUSE: strcat(permutation_buffer,"select ");   break;
    case MYX_QCT_FROM_CLAUSE:   strcat(permutation_buffer,"from ");     break;
    case MYX_QCT_WHERE_CLAUSE:  strcat(permutation_buffer,"where ");    break;
    case MYX_QCT_GROUP_CLAUSE:  strcat(permutation_buffer,"group by "); break;
    case MYX_QCT_HAVING_CLAUSE: strcat(permutation_buffer,"having ");   break;
    case MYX_QCT_ORDER_CLAUSE:  strcat(permutation_buffer,"order by "); break;
    case MYX_QCT_LIMIT_CLAUSE:  strcat(permutation_buffer,"limit ");    break;
    case MYX_QCT_SET_CLAUSE:    strcat(permutation_buffer,"set ");      break;
    case MYX_QCT_INTO_CLAUSE:   strcat(permutation_buffer,"into ");     break;
    case MYX_QCT_UPDATE_CLAUSE: strcat(permutation_buffer,"update ");   break;
    }
    strcat(permutation_buffer,(*pc)->clause);
    strcat(permutation_buffer," ");
  }
  simple_test(mysql,permutation_buffer);
}

void test_clauses_permutations(MYSQL * mysql, char * query_buffer)
{
  MYX_QUERY * query= query_analyze(mysql,query_buffer);
  trace_query(query);

  const size_t max_clauses= 100;
  if (query->clauses_num>0 && query->clauses_num<=max_clauses)
  {
    MYX_Q_CLAUSE * permuted_clauses[max_clauses];

    size_t len= 0;
    MYX_Q_CLAUSE * clause= query->clauses;
    MYX_Q_CLAUSE * clauses_end= clause + query->clauses_num;

    for (; clause!=clauses_end; clause++)
      len+= strlen(clause->clause);

    if (len<sizeof(permutation_buffer))
    {
      clause= query->clauses;
      MYX_Q_CLAUSE ** permuted_clause= permuted_clauses;
      MYX_Q_CLAUSE ** permuted_clauses_end=
                                         permuted_clauses + query->clauses_num;

      *permuted_clause= query->clauses;
      for (;;)
      {
        permuted_clause++;
        if (permuted_clause!=permuted_clauses_end)
        {
          *permuted_clause= query->clauses;
        }
        else
        {
          test_clause_permutation(mysql,query_buffer,
                                  permuted_clauses,permuted_clauses_end);
          do
          {
            if (permuted_clause==permuted_clauses)
              goto finish;
            permuted_clause--;
            (*permuted_clause)++;
          }
          while (*permuted_clause==clauses_end);
        }
      }
    }
  }
finish:
  query_free(query);
}

char separator_buffer[100];
char buffer[500];
char query_buffer[160000];
int test_with_mysql(MYSQL * mysql)
{
  FILE * file= fopen(path_to_query,"rb");
  if (file==NULL)
  {
    fprintf(stderr,"can't open file \"%s\"\n",path_to_query);
    return -1;
  }
  else
  {
#ifdef _WINDOWS_
    _setmode( _fileno( stdout ), _O_BINARY );
#endif
    while (!feof(file))
    {
      while (fgets(separator_buffer,sizeof(separator_buffer)-1,file) && 
             (separator_buffer[0] == '\r' || separator_buffer[0] == '\n'));
      if (separator_buffer[0] == '\r' || separator_buffer[0] == '\n' || separator_buffer[0]==0)
        break;
      query_buffer[0]= 0;
      while (fgets(buffer,sizeof(buffer)-1,file))
      {
        if (!strcmp(buffer,separator_buffer))
          break;
        if (strlen(buffer)+strlen(query_buffer) < sizeof(query_buffer))
          strcat(query_buffer,buffer);
      }
      prepare_line(query_buffer);
      switch (kind_of_test)
      {
      case kt_test_all_prefixes:  test_all_prefixes(mysql,query_buffer); break;
      case kt_test_clauses_permutation:  
                          test_clauses_permutations(mysql,query_buffer); break;
      default:                    simple_test(mysql,query_buffer);       break;
      }        
    }

    fclose(file);
    return 0;
  }
}


int test_with_connecton(MYX_USER_CONNECTION * user_conn)
{
  char * var;
  if ((var= getenv("my_cnf_path")))
    myx_set_my_cnf_path(var);

  MYSQL * mysql= myx_mysql_init();
  if (!mysql)
  {
    fprintf(stderr,"can't init mysql!\n");
    return -1;
  }
  else
  {
    int res;
    if (!myx_connect_to_instance(user_conn,mysql))
    {
      res= test_with_mysql(mysql);
    }
    else
    {
      fprintf(stderr,"can't connect to mysql!\n");
      res= -1;
    }
    myx_mysql_close(mysql);
    return res;
  }
}

int main(int argc, char* argv[])
{
  if (argc<3)
  {
    print_usage();
    return 0;
  }
  else
  {
    connection_path= argv[1];
    path_to_query= argv[2];
    if (argc>3)
      kind_of_test= parse_kind_of_test(argv[3]);

    MYX_LIB_ERROR err;
    MYX_USER_CONNECTIONS * connections=
                               myx_load_user_connections(connection_path,&err);
    if (!connections)
    {
      fprintf(stderr,"Can't read connection file \"%s\", error=%d\n",
              connection_path,(int)err);
      return -1;
    }
    else
    {
      int res;
      if (connections->user_connections_num)
      {
        res= test_with_connecton(connections->user_connections);
      }
      else
      {
        fprintf(stderr,"there aren't connections in the connection file\n");
        res= -1;
      }
      myx_free_user_connections(connections);
      return res;
    }
  }
}

