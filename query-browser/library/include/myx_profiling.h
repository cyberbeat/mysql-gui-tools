

typedef enum {
  MYX_QP_NO_ERROR=0,
    MYX_QP_SYSTEM_ERROR,
    MYX_QP_NETWORK_ERROR,
} MYX_QP_ERROR;

typedef struct MYX_QP_CLIENT {
  int sock;
  char *host;
} MYX_QP_CLIENT;

typedef struct MYX_QP_NET MYX_QP_NET;

typedef enum MYX_QP_SOURCE
{
  MYX_QP_SOURCE_PHP,
    MYX_QP_SOURCE_JAVA
} MYX_QP_SOURCE;


typedef enum MYX_QP_INDEX_USAGE
{
  MYX_QP_IU_NO_INDEX_USED,
    MYX_QP_IU_BAD_INDEX
} MYX_QP_INDEX_USAGE;


typedef struct MYX_QP_QUERY {
  MYX_QP_SOURCE source_api;
  
  
  char *source_name;
  unsigned int line_nr;
  
  
  char *stack_info;
  
  char *query;
  unsigned int execution_time_ms;
  
  unsigned int mysql_thread_id; 
  unsigned int mysql_state_code;
  unsigned int mysql_num_rows;
  unsigned int mysql_affected_rows;
  
  
  unsigned int mysql_error_nr;
  char *mysql_error;
  unsigned int mysql_warnings_num;
  char **mysql_warnings;
  
  
  unsigned int mysql_insert_id;
  
  
  MYX_QP_INDEX_USAGE index_usage;
} MYX_QP_QUERY;


typedef struct MYX_QP_QUERY_LOG {
  unsigned int query_num;
  MYX_QP_QUERY *query;
} MYX_QP_QUERY_LOG;

