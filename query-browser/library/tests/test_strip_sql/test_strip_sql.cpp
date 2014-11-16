// test_strip_sql.cpp : Defines the entry point for the console application.
//

#ifdef _WINDOWS_
#include "io.h"
#endif
#include <fcntl.h>
#include "myx_public_interface.h"
#include "myx_qb_public_interface.h"

///////////////////////////////////////////////////////////////////////////////
void print_usage()
{
  printf("usage : test_strip_sql.exe "
     " <unstripped_query_file> <copy_mode> <language> <edited_query_file>\n");
}

///////////////////////////////////////////////////////////////////////////////
int read_file(const char * filename, char * buf, size_t buf_size)
{
  FILE * file= fopen(filename,"rb");
  if (file==NULL)
  {
    fprintf(stderr,"can't open file \"%s\"\n",filename);
    return -1;
  }
  else
  {
    size_t read= fread(buf,1,buf_size-1,file);
    buf[read]= 0;
    return 0;
  }
}

char unstripped_query[10000];
char edited_query[10000];

///////////////////////////////////////////////////////////////////////////////
void test(const char * unstripped_query,
          MYX_Q_SQL_STRIPPED_COPY_MODE copy_mode,
          MYX_Q_SQL_STRIPPED_CODE_LANGUAGE language,
          const char * edited_query)
{  
  printf("---------------------------------------------- unstripped_query :\n"
         "-----------------------------------------------------------------\n"
         "%s\n"
         "-----------------------------------------------------------------\n",
         unstripped_query);

  MYX_Q_SQL_STRIPPED * stripped_sql= myx_strip_embedded_sql(unstripped_query,
                                                            language,
                                                            copy_mode);
  printf("------------------------------------------------ stripped_query :\n"
         "-----------------------------------------------------------------\n"
         "%s\n"
         "-----------------------------------------------------------------\n",
         stripped_sql->query_stripped);
  char * result_text= myx_reconstruct_embedded_sql(stripped_sql,edited_query);

  printf("-------------------------------------------------- edited_query :\n"
         "-----------------------------------------------------------------\n"
         "%s\n"
         "-----------------------------------------------------------------\n",
         edited_query);
  
  printf("-------------------------------------------------------- result :\n"
         "-----------------------------------------------------------------\n"
         "%s\n"
         "-----------------------------------------------------------------\n",
         result_text);
  g_free(result_text);
  myx_free_stripped_sql(stripped_sql);
}

///////////////////////////////////////////////////////////////////////////////
int test_strings(const char * unstripped_query,
                 const char * str_copy_mode,
                 const char * str_language,
                 const char * edited_query)
{
  printf("%s,%s\n",str_copy_mode,str_language);

  MYX_Q_SQL_STRIPPED_CODE_LANGUAGE language= 
    !strcmp(str_language,"PHP")  ? MYX_QSSCL_PHP  :
    !strcmp(str_language,"JAVA") ? MYX_QSSCL_JAVA :
                                  (MYX_Q_SQL_STRIPPED_CODE_LANGUAGE)-1;
  if (language==-1)
  {
    fprintf(stderr,"wrong language \"%s\", "
                   "should be MYX_QSSCL_PHP | MYX_QSSCL_JAVA\n",
                   str_language);
    return -1;
  }

  MYX_Q_SQL_STRIPPED_COPY_MODE copy_mode=
    !strcmp(str_copy_mode,"QB_MENU")    ? MYX_QSSCM_QB_MENU :
    !strcmp(str_copy_mode,"KEYSTROKES") ? MYX_QSSCM_KEYSTROKES :
    !strcmp(str_copy_mode,"MESSAGE")    ? MYX_QSSCM_MESSAGE :
                                         (MYX_Q_SQL_STRIPPED_COPY_MODE)-1;
  if (copy_mode==-1)
  {
    fprintf(stderr,"wrong copy_mode \"%s\", "
                   "should be MYX_QSSCM_QB_MENU | MYX_QSSCM_KEYSTROKES "
                   "| MYX_QSSCM_MESSAGE\n", str_copy_mode);
    return -1;
  }

#ifdef _WINDOWS_
  _setmode( _fileno( stdout ), _O_BINARY );
#endif
  test(unstripped_query,copy_mode,language,edited_query);

  return 0;
}

///////////////////////////////////////////////////////////////////////////////
int test_files(const char * unstripped_query_file,
               const char * str_copy_mode,
               const char * str_language,
               const char * edited_query_file)
{
  int res;
  if ((res= read_file(unstripped_query_file,
                      unstripped_query,sizeof(unstripped_query))) || 
      (res= read_file(edited_query_file,edited_query,sizeof(edited_query))))
  {
    return res;
  }
  
  return test_strings(unstripped_query,str_copy_mode,
                      str_language,edited_query);
}

///////////////////////////////////////////////////////////////////////////////
void prepare_line(char * line)
{
  if (line)
  {
    size_t len= strlen(line);
    while (len && (line[len-1]=='\r' || line[len-1]=='\n'))
      line[--len]= 0;
  }
}

///////////////////////////////////////////////////////////////////////////////
char separator_buffer[100];
char buffer[1000];
char copy_mode_buffer[100];
char language_buffer[100];

int test_file(FILE * file)
{
  while (!feof(file))
  {
    while (fgets(separator_buffer,sizeof(separator_buffer)-1,file) && 
           (separator_buffer[0] == '\r' || separator_buffer[0] == '\n'));
    if (separator_buffer[0] == '\r' || separator_buffer[0] == '\n')
      return 1;
    unstripped_query[0]= 0;
    while (fgets(buffer,sizeof(buffer)-1,file))
    {
      if (!strcmp(buffer,separator_buffer))
        break;
      strcat(unstripped_query,buffer);
    }
    prepare_line(unstripped_query);
    if (!fgets(copy_mode_buffer,sizeof(copy_mode_buffer)-1,file) ||
        !fgets(language_buffer,sizeof(language_buffer)-1,file) ||
        !fgets(separator_buffer,sizeof(separator_buffer)-1,file))
    {
      return 1;
    }
    prepare_line(copy_mode_buffer);
    prepare_line(language_buffer);
    
    edited_query[0]= 0;
    while (fgets(buffer,sizeof(buffer)-1,file))
    {
      if (!strcmp(buffer,separator_buffer))
        break;
      strcat(edited_query,buffer);
    }
    prepare_line(edited_query);
    test_strings(unstripped_query,language_buffer,
                 copy_mode_buffer,edited_query);
    printf("\n\n");
  }
  return 0;
}

///////////////////////////////////////////////////////////////////////////////
int test_file(const char * filename)
{
  FILE * file= fopen(filename,"rb");
  if (!file)
  {
    printf("can't open file \'%s\'\n",filename);
    return 1;
  }
  else
  {
    int res= test_file(file);
    fclose(file);
    return 0;
  }
}

///////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[])
{
  switch (argc)
  {
  case 2:   return test_file(argv[1]);
  case 5:   return test_files(argv[1],argv[2],argv[3],argv[4]);
  default:  print_usage();  return 0;
  }
}

///////////////////////////////////////////////////////////////////////////////
