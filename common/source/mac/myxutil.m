
#include "myxutil.h"
#include <glib.h>

MYX_USER_CONNECTION *myx_copy_user_connection(MYX_USER_CONNECTION *info)
{
  MYX_USER_CONNECTION *copy;
  
  copy= g_malloc0(sizeof(MYX_USER_CONNECTION));
  copy->connection_name= g_strdup(info->connection_name);
  copy->username= g_strdup(info->username);
  copy->password= g_strdup(info->password);
  copy->hostname= g_strdup(info->hostname);
  copy->port= info->port;
  copy->schema= g_strdup(info->schema);
  copy->storage_path= g_strdup(info->storage_path);
  if (info->advanced_options_num > 0)
  {
    unsigned int i;
    copy->advanced_options= g_malloc0(sizeof(char*)*info->advanced_options_num);
    for (i= 0; i < info->advanced_options_num; i++)
      copy->advanced_options[i]= g_strdup(info->advanced_options[i]);
    copy->advanced_options_num= info->advanced_options_num;
  }
  copy->notes= g_strdup(info->notes);
  copy->connection_type= info->connection_type;
  copy->storage_type= info->storage_type;
  
  return copy;
}


int myx_same_user_connections(MYX_USER_CONNECTION *conn1,
                              MYX_USER_CONNECTION *conn2)
{
  unsigned int i;
  
#define Check(field) strcmp3(conn1->field, conn2->field)==0
  if (!Check(username) 
      || !Check(password)
      || !Check(hostname)
      || conn1->port != conn2->port
      || !Check(schema)
      || !Check(storage_path)
      || !Check(notes)
      || conn1->connection_type != conn2->connection_type
      || conn1->storage_type != conn2->storage_type
      || conn1->advanced_options_num != conn2->advanced_options_num)
    return 0;
  
  for (i= 0; i < conn1->advanced_options_num; i++)
  {
    if (strcmp3(conn1->advanced_options[i],
                conn2->advanced_options[i])!=0)
      return 0;
  }
#undef Check
  return 1;
}


NSString *myx_get_available_filename(NSString *directory, NSString *base, NSString *suffix)
{
  NSFileManager *fman= [NSFileManager defaultManager];
  NSString *fname;
  int i= 1;
  
  fname= [base stringByAppendingString:suffix?suffix:@""];
  while ([fman fileExistsAtPath:[directory stringByAppendingFormat:@"/%@",fname]])
    fname= [NSString stringWithFormat:@"%@ %i%@",base,i++,suffix?suffix:@""];

  return fname;
}


const char *myx_get_option_value(const char **list, unsigned int count, const char *option)
{
  unsigned int i;
  for (i= 0; i < count; i++)
  {
    int len= strlen(option);
    if (list[i] && strncmp(list[i], option, len)==0 && list[i][len] == '=')
    {
      len++;
      while (list[i][len]!=0 && isspace(list[i][len])) len++;
      return list[i]+len;
    }
  }
  return NULL;
}


