
// test_query_composition.cpp : Defines the entry point for the console application.

#ifdef _WINDOWS_
#include "io.h"
#endif
#include <fcntl.h>
#include "stdio.h"
#include "stdlib.h"
#include "myx_public_interface.h"

const char * connection_path;
const char * path_to_query;

void print_usage()
{
  printf("usage : test_query_composition.exe <path_to_connection_file> "
         "<path_to_queries>\n");
}

char separator_buffer[100];
char buffer[500];
char query_buffer[160000];

char str_kind[30];
char arg1[200];
char arg2[200];
char arg3[200];
char arg4[200];
char str_clause_type[200];
MYX_Q_CLAUSE_TYPE clause_type;

bool read_separator(FILE * file, char * buffer, size_t buffer_size)
{
  while (fgets(buffer,(int)(buffer_size-1),file) &&
         (buffer[0] == '\r' || buffer[0] == '\n'));
  return buffer[0] != '\r' && buffer[0] != '\n' && buffer[0]!=0;
}

size_t prepare_line(char * line, size_t len)
{
  if (line)
  {
    while (len && (line[len-1]=='\r' || line[len-1]=='\n'))
      line[--len]= 0;
  }
  return len;
}

size_t read_text_till_separator(FILE * file,
                                char * text_buffer, size_t text_buffer_size,
                                char * line_buffer, size_t line_buffer_size,
                                const char * separator, size_t separator_len)
{
  text_buffer[0]= 0;
  size_t text_len= 0;
  while (fgets(line_buffer,(int)(line_buffer_size-1),file))
  {
    size_t line_len= strlen(line_buffer);
    if (separator_len==line_len && !strncmp(line_buffer,separator,line_len))
      break;
    if (line_len + text_len < text_buffer_size)
    {
      strcat(text_buffer+text_len,line_buffer);
      text_len += line_len;
    }
  }
  return prepare_line(text_buffer,text_len);
}

int parse_add_type(const char *str)
{
  if (!strcmp(str,"MYX_QTAT_SELECT"))         return MYX_QTAT_SELECT;
  if (!strcmp(str,"MYX_QTAT_SELECT"))         return MYX_QTAT_SELECT;
  if (!strcmp(str,"MYX_QTAT_SELECT_ADD"))     return MYX_QTAT_SELECT_ADD;
  if (!strcmp(str,"MYX_QTAT_SELECT_JOIN"))    return MYX_QTAT_SELECT_JOIN;
  if (!strcmp(str,"MYX_QTAT_SELECT_LEFT_OUTER_JOIN")) 
    return MYX_QTAT_SELECT_LEFT_OUTER_JOIN;
  if (!strcmp(str,"MYX_QTAT_UPDATE"))         return MYX_QTAT_UPDATE;
  if (!strcmp(str,"MYX_QTAT_INSERT"))         return MYX_QTAT_INSERT;
  if (!strcmp(str,"MYX_QTAT_DELETE"))         return MYX_QTAT_DELETE;

  if (!strcmp(str,"MYX_QCT_NO_CLAUSE"))       return MYX_QCT_NO_CLAUSE;
  if (!strcmp(str,"MYX_QCT_SELECT_CLAUSE"))   return MYX_QCT_SELECT_CLAUSE;
  if (!strcmp(str,"MYX_QCT_FROM_CLAUSE"))     return MYX_QCT_FROM_CLAUSE;
  if (!strcmp(str,"MYX_QCT_WHERE_CLAUSE"))    return MYX_QCT_WHERE_CLAUSE;
  if (!strcmp(str,"MYX_QCT_GROUP_CLAUSE"))    return MYX_QCT_GROUP_CLAUSE;
  if (!strcmp(str,"MYX_QCT_HAVING_CLAUSE"))   return MYX_QCT_HAVING_CLAUSE;
  if (!strcmp(str,"MYX_QCT_ORDER_CLAUSE"))    return MYX_QCT_ORDER_CLAUSE;
  if (!strcmp(str,"MYX_QCT_LIMIT_CLAUSE"))    return MYX_QCT_LIMIT_CLAUSE;
  if (!strcmp(str,"MYX_QCT_SET_CLAUSE"))      return MYX_QCT_SET_CLAUSE;
  if (!strcmp(str,"MYX_QCT_INTO_CLAUSE"))     return MYX_QCT_INTO_CLAUSE;
  if (!strcmp(str,"MYX_QCT_UPDATE_CLAUSE"))   return MYX_QCT_UPDATE_CLAUSE;
  if (!strcmp(str,"MYX_QCT_DELETE_CLAUSE"))   return MYX_QCT_DELETE_CLAUSE;

  return MYX_QTAT_UNKNOWN;
}

void test_query_add_x_to_sql(bool add_table, MYSQL *mysql,
                             const char *arg1, const char *arg2,
                             const char *arg3, const char *arg4, 
                             char *sqls, int clause_type, 
                             int * cursor_pos, 
                             char * separator, size_t separator_len)
{
  MYX_Q_TABLE_ADD_ERROR error= MYX_QC_OK;
  char * result= add_table
             ? myx_query_add_table_to_sql(mysql,arg1,arg2,arg3,arg4,
                                          query_buffer,
                                          (MYX_Q_TABLE_ADD_TYPE)clause_type,
                                          cursor_pos, &error)
             : myx_query_add_column_to_sql(mysql,arg1,arg2,arg3,arg4,
                                           query_buffer,
                                           (MYX_Q_CLAUSE_TYPE)clause_type,
                                           cursor_pos);
  switch (error)
  {
  case MYX_QC_TABLES_WITHOUT_ALIAS: 
    printf("ERROR! : MYX_QC_TABLES_WITHOUT_ALIAS!\n"); break;
  case MYX_QC_OK:
    {
      char old_cur_pos_content= result[*cursor_pos];
      result[*cursor_pos]= 0;
      prepare_line(separator,separator_len);
      printf("%s\n",separator);
      printf("%s\n",result);
      printf("%s\n",separator);
      result[*cursor_pos]= old_cur_pos_content;
      printf("%s\n",result + *cursor_pos);
      printf("%s\n",separator);
      myx_free_lib_str(result);
      break;
    }
  }
}

int test_with_mysql(MYSQL * mysql)
{
#ifdef _WINDOWS_
  _setmode( _fileno( stdout ), _O_BINARY );
#endif
  FILE * file= fopen(path_to_query,"rb");
  if (file==NULL)
  {
    fprintf(stderr,"can't open file \"%s\"\n",path_to_query);
    return -1;
  }
  else
  {
    while (!feof(file))
    {
      if (read_separator(file,separator_buffer,sizeof(separator_buffer)) &&
          fgets(str_kind,sizeof(str_kind)-1,file) &&
          fgets(arg1,sizeof(arg1)-1,file) &&
          fgets(arg2,sizeof(arg2)-1,file) &&
          fgets(arg3,sizeof(arg3)-1,file) &&
          fgets(arg4,sizeof(arg4)-1,file) &&
          fgets(str_clause_type,sizeof(str_clause_type)-1,file))
      {
        prepare_line(str_kind,strlen(str_kind));
        prepare_line(arg1,strlen(arg1));
        prepare_line(arg2,strlen(arg2));
        prepare_line(arg3,strlen(arg3));
        prepare_line(arg4,strlen(arg4));
        prepare_line(str_clause_type,strlen(str_clause_type));
        size_t separator_len= strlen(separator_buffer);
        int cursor_pos= (int)
          read_text_till_separator(file,
                                   query_buffer,sizeof(query_buffer),
                                   buffer,sizeof(buffer),
                                   separator_buffer,separator_len);
        size_t len= cursor_pos +
          read_text_till_separator(file,
                                   query_buffer+cursor_pos,
                                   sizeof(query_buffer)-cursor_pos,
                                   buffer,sizeof(buffer),
                                   separator_buffer,separator_len);
        test_query_add_x_to_sql(!strcmp(str_kind,"add_table"),
                                mysql,arg1,arg2,arg3,arg4,
                                query_buffer,parse_add_type(str_clause_type),
                                &cursor_pos,separator_buffer,
                                separator_len);
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

int main(int argc, char * argv[])
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

