
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>

#include "mahelper.h"
#include "mahelper_priv.h"


static int write_fd(int sockfd, const char *data, size_t datalen, int fd)
{
  struct msghdr msg;
  struct iovec iov[1];
  union {
    struct cmsghdr cm;
    char control[CMSG_SPACE(sizeof(int))];
  } control_un;
  struct cmsghdr *cmptr;
  
  msg.msg_control= control_un.control;
  msg.msg_controllen= sizeof(control_un.control);
  
  cmptr= CMSG_FIRSTHDR(&msg);
  cmptr->cmsg_len= CMSG_LEN(sizeof(int));
  cmptr->cmsg_level= SOL_SOCKET;
  cmptr->cmsg_type= SCM_RIGHTS;
  *((int*)CMSG_DATA(cmptr))= fd;
  
  msg.msg_name= NULL;
  msg.msg_namelen= 0;
  
  iov[0].iov_base= (char*)data;
  iov[0].iov_len= datalen;
  msg.msg_iov= iov;
  msg.msg_iovlen= 1;
  
  DEBUG("sending...");
  return sendmsg(sockfd, &msg, 0);
}


static bool
sendReply(int sockfd, const char *message, int fd)
{
  write(1, "", 1);
  return write_fd(sockfd, message, strlen(message)+1, fd);
}


static bool
sendError(int sockfd, const char *message)
{
  write(1, message, strlen(message)+1);
  return true;
}


static char *get_cnf_value(const char *path, const char *section, const char *option)
{
  FILE *file= fopen(path, "r");
  char *secstr;
  char *value= NULL;
  char buffer[1024];
  if (!file)
    return NULL;
  
  secstr= malloc(strlen(option)+8);
  sprintf(secstr, "[%s]", section); 
  
  while (fgets(buffer, sizeof(buffer), file))
  {
    char *end= buffer+strlen(buffer)-1;
    while (isspace(*end)) --end;
    end++;
    *end= 0;
    if (strcmp(secstr, buffer)==0)
    {
      while (fgets(buffer, sizeof(buffer), file))
      {
        end= buffer+strlen(buffer)-1;
        while (isspace(*end)) --end;
        end++;
        *end= 0;
        if (strncmp(buffer, option, strlen(option))==0 && 
            (isspace(buffer[strlen(option)]) || buffer[strlen(option)]=='='))
        {
          char *p= strchr(buffer, '=');
          if (!p)
            continue;
          p++;
          while (isspace(*p)) ++p;
          value= strdup(p);
          break;
        }
      }
      break;
    }
  }
  free(secstr);
  fclose(file);
  
  return value;
}


static char *
guessLogPath(int type)
{
  const char *suf;
  char host[1024];
  char buffer[1024];
  char *value;
  char *ptr;
  
  switch (type)
  {
    case 0: // error
      value= get_cnf_value("/etc/my.cnf", "mysqld", "log-error");
      if (value && *value)
        return value;
        suf= ".err";
      break;
    case 1: // general
      value= get_cnf_value("/etc/my.cnf", "mysqld", "log");
      if (value && *value)
        return value;
        suf= ".log";
      break;
    case 2: // slow
      value= get_cnf_value("/etc/my.cnf", "mysqld", "log-slow-queries");
      if (value && *value)
        return value;		
        suf= "-slow.log";
      break;
    default:
      return NULL;
  }
  
  if (gethostname(host, sizeof(host)) < 0)
  {
    DEBUG("can't determine hostname");
    return NULL;
  }
  
  snprintf(buffer, sizeof(buffer), "/usr/local/mysql/data/%s%s", host, suf);
  if (close(open(buffer, O_RDONLY)) >= 0)
    return strdup(buffer);
  
  snprintf(buffer, sizeof(buffer), "/var/log/mysql/%s%s", host, suf);
  if (close(open(buffer, O_RDONLY)) >= 0)
    return strdup(buffer);
  
  snprintf(buffer, sizeof(buffer), "/var/log/mysql%s", suf);
  if (close(open(buffer, O_RDONLY)) >= 0)
    return strdup(buffer);
  
  ptr= strrchr(host, '.');
  if (ptr && strcmp(ptr, ".local")==0)
  {
    *ptr= 0;

    snprintf(buffer, sizeof(buffer), "/usr/local/mysql/data/%s%s", host, suf);
    if (close(open(buffer, O_RDONLY)) >= 0)
      return strdup(buffer);

    snprintf(buffer, sizeof(buffer), "/var/log/mysql/%s%s", host, suf);
    if (close(open(buffer, O_RDONLY)) >= 0)
      return strdup(buffer);    
  }
  
  return NULL;
}


static bool doOpenLog(int sockfd, int n)
{
  char *fn= guessLogPath(n);
  int fd;
  
  DEBUG("trying to open %s  %i", fn, sockfd);
  
  if (!fn)
    return sendError(sockfd, "Can't locate log file.");
  
  fd= open(fn, O_RDONLY);
  if (fd < 0)
    return sendError(sockfd, strerror(errno));
  else
    return sendReply(sockfd, fn, fd);
}


bool performOpenCommand(const MAHelperCommand *cmd, int sockfd)
{
  int fd;
  char *msg;
  char new_path[sizeof(MYSQL_CONFIG_FILE)+10];
  char bak_path[sizeof(MYSQL_CONFIG_FILE)+10];
  
  sprintf(new_path, "%s.new", MYSQL_CONFIG_FILE);
  sprintf(bak_path, "%s.old", MYSQL_CONFIG_FILE);
  
  switch (cmd->authorizedCommandId)
  {
    case MAHelperOpenMyCnf:
    {
      DEBUG("opening config file ro");
      fd= open(new_path, O_RDONLY, 0644);
      if (fd < 0)
      {
        msg= strerror(errno);
        
        DEBUG("Could not open file '%s': %s", new_path, msg);
        return sendError(sockfd, msg);
      }
      else
        sendReply(sockfd, new_path, fd);
      return true;
    }
      
      // Create temporary file where conf data will be stored until it's
      // renamed to my.cnf
    case MAHelperOpenNewMyCnf: // this must match working of myx_update_mysql_cnf_filef
    {
      DEBUG("opening config file");
      fd= open(new_path, O_RDWR|O_CREAT, 0644);
      
      if (fd < 0)
      {
        msg= strerror(errno);
        DEBUG("Could not open file '%s': %s", new_path, msg);
        return sendError(sockfd, msg);
      }
      else
        return sendReply(sockfd, new_path, fd);
    }
      
      // Backup current my.cnf file to my.cnf.old and 
    case MAHelperCommitMyCnf: // this must match working of myx_update_mysql_cnf_filef
    {
      if (access(MYSQL_CONFIG_FILE, F_OK)==0 &&
          rename(MYSQL_CONFIG_FILE, bak_path) < 0)
      {
        char message[1024];
        DEBUG("failed backuping file");		
        sprintf(message, "Could not create backup of file %s:%s",
                MYSQL_CONFIG_FILE, strerror(errno));		
        return sendError(sockfd, message);
      }
      
      if (rename(new_path, MYSQL_CONFIG_FILE) < 0)
      {
        char message[1024];
        DEBUG("failed renaming to my.cnf");
        sprintf(message, "Could not rename temp. configuration file to %s:%s",
                MYSQL_CONFIG_FILE, strerror(errno));
        return sendError(sockfd, message);
      }
      
      return sendError(sockfd, "OK");
    } 
      
    case MAHelperOpenErrorLog:
      return doOpenLog(sockfd,0);
    case MAHelperOpenGeneralLog:
      return doOpenLog(sockfd,1);
    case MAHelperOpenSlowLog:
      return doOpenLog(sockfd,2);
      
    default:
      return false;
  }
}

