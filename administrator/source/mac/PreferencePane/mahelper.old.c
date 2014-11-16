/*
 *  mahelper.c
 *  MySQL Administrator
 *
 *  Created by Alfredo Kojima on Tue Aug 03 2004.
 *  Copyright (c) 2004 MySQL AB. All rights reserved.
 *
 */

#include "mahelper.h"

#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <mach-o/dyld.h>

#define DEBUG(fmt, ...) fprintf(stderr, "mahelper: "fmt"\n", ##__VA_ARGS__)

#define HOSTCONFIG_PATH "/etc/hostconfig"
#define MYSQL_COMMAND "/usr/local/mysql/support-files/mysql.server"

static const char *
rightNameForCommand(const MAHelperCommand *cmd)
{
  switch (cmd->authorizedCommandId)
  {
    case MAHelperStartMySQL:
    case MAHelperStopMySQL:
    case MAHelperToggleAutoStart:
      return "com.mysql.administrator.helper";
  }
  return "system.unknown";
}

/*
static char *
getStartupItem()
{
  if (access("/Library/StartupItems/MySQLCOM", F_OK)==0)
    return "/Library/StartupItems/MySQLCOM/MySQL";
  else if (access("/Library/StartupItems/MySQL", F_OK)==0)
    return "/Library/StartupItems/MySQL/MySQL";
  else
    return NULL;
}
*/

static bool performExternalCommand(const MAHelperCommand *cmd)
{
  char *args[4];
  int argc;
  pid_t pid;
  int status;
  char *const envp[] = { "PATH=/bin:/usr/bin:/sbin:/usr/sbin", NULL };

  switch (cmd->authorizedCommandId)
  {
    case MAHelperStartMySQL:
      args[0]= MYSQL_COMMAND;
      args[1]= "start";
      argc= 2;
      break;
    case MAHelperStopMySQL:
      args[0]= MYSQL_COMMAND;
      args[1]= "stop";
      argc= 2;
      break;
    default:
      return false;
  }
  
  DEBUG("will execute %s %s", args[0], args[1]);
  
  args[argc]= NULL;
  if ((pid= fork()) == 0)
  {
    execve(args[0], args, envp);
    exit(-1);
  }
  else if (pid < 0)
    return false;

  wait(&status);
  if (pid == -1 || ! WIFEXITED(status))
    return false;

  return true;
}


static bool toggleAutoStart(bool enable)
{
  FILE *file= fopen(HOSTCONFIG_PATH, "r");
  FILE *nfile= fopen(HOSTCONFIG_PATH".tmp", "w");
  char buffer[1024];
  bool found= 0;
  
  if (!file)
  {
    DEBUG("couldn't open config file %s", HOSTCONFIG_PATH);
    return false;
  }
  if (!nfile)
  {
    fclose(file);
    DEBUG("couldn't open config file %s.tmp", HOSTCONFIG_PATH);
    return false;
  }
  
  for (;;)
  {
    if (!fgets(buffer, sizeof(buffer), file))
    {
      if (ferror(file)!=0)
      {
        DEBUG("error reading from hostconfig file");
        goto error;
      }
      break;
    }
    if (strncmp(buffer, "MYSQLCOM=", sizeof("MYSQLCOM=")-1)==0)
    {
      char *newval= NULL;
      if (strstr(buffer, "-YES-"))
      {
        if (!enable)
          newval= "MYSQLCOM=-NO-\n";
      }
      else if (strstr(buffer, "-NO-"))
      {
        if (enable)
          newval= "MYSQLCOM=-YES-\n";
      }
      
      if (!fprintf(nfile, "%s", newval ? newval : buffer))
      {
        DEBUG("error writing to temp. hostconfig file");
        goto error;
      }
      found= 1;
    }
    else
    {
      if (!fprintf(nfile, "%s", buffer))
      {
        DEBUG("error writing to temp. hostconfig file");
        goto error;
      }
    }
  }
  
  if (!found)
  {
    if (!fprintf(nfile, "MYSQLCOM=-%s-", enable?"YES":"NO"))
    {
      DEBUG("error writing to temp. hostconfig file");
      goto error;
    }    
  }

  fclose(nfile);
  fclose(file);
  if (rename(HOSTCONFIG_PATH".tmp", HOSTCONFIG_PATH) != 0)
  {
    DEBUG("error renaming hostconfig file (%s)", strerror(errno));
    return false;
  }
  return true;

error:
  fclose(nfile);
  fclose(file);
  unlink(HOSTCONFIG_PATH".tmp");
  return false;
}

/*
static bool readLogFileData(const MAHelperCommand *cmd)
{
  FILE *file= NULL;
  ssize_t count;
  char *path;
  
  switch (cmd)
  {
    case MALogError:
      path= ;
      break;
    default:
      return false;
  }
  file= fopen(path, "r");
  free(path);
  if (!file)
    return false;
  
  buffer= malloc(cmd->bytes+1);
  if (!buffer)
  {
    DEBUG("error allocating memory for log file buffer");
    fclose(file);
    return false;
  }
  fseek(file, SEEK_SET, cmd->offset);
  count= fread(buffer, 1, cmd->bytes, file);
  if (count > 0)
    fwrite(buffer, 1, count, stdout);

  free(buffer);
  
  fclose(file);
  
  return true;
}
*/

static bool performCommand(const MAHelperCommand *cmd)
{
  switch (cmd->authorizedCommandId)
  {
    case MAHelperStartMySQL:
    case MAHelperStopMySQL:
      return performExternalCommand(cmd);
    case MAHelperToggleAutoStart:
      return toggleAutoStart(cmd->enable);
//    case MAHelperReadLogFile:
//      return readLogFileData(cmd);
    default:
      return false;
  }
}


int main(int argc, char **argv)
{
  char mypath[MAXPATHLEN];
  unsigned long mypath_size= sizeof(mypath);
  OSStatus status;
  AuthorizationRef auth;
  int bytesRead;
  MAHelperCommand command;
  
  if (_NSGetExecutablePath(mypath, &mypath_size) < 0)
  {
    DEBUG("could not get my path");
    exit(MAHelperCommandInternalError);
  }

  if (argc == 2 && !strcmp(argv[1], "--self-repair")) // make ourself suid root
  {    
    struct stat st;
    int fd_tool;
    
    DEBUG("self repairing");
    
    /* Recover the passed in AuthorizationRef. */
    if (AuthorizationCopyPrivilegedReference(&auth, kAuthorizationFlagDefaults))
      exit(MAHelperCommandInternalError);
    
    /* Open tool exclusively, so noone can change it while we bless it */
    fd_tool = open(mypath, O_NONBLOCK|O_RDONLY|O_EXLOCK, 0);
    
    if (fd_tool == -1)
    {
      DEBUG("Exclusive open while repairing helper failed: %d.\n", errno);
      exit(MAHelperCommandInternalError);
    }
    
    if (fstat(fd_tool, &st))
      exit(MAHelperCommandInternalError);
    
    if (st.st_uid != 0)
      fchown(fd_tool, 0, st.st_gid);
    
    fchmod(fd_tool, (st.st_mode & (~(S_IWGRP|S_IWOTH))) | S_ISUID);
    
    close(fd_tool);
    
    DEBUG("self-repair done.");
  }
  else
  {
    AuthorizationExternalForm extAuth;
    
    // read bytestream with Auth data
    if (read(0, &extAuth, sizeof(extAuth)) != sizeof(extAuth))
      exit(MAHelperCommandInternalError);

    // bytestream --*poof*--> auth
    if (AuthorizationCreateFromExternalForm(&extAuth, &auth))
      exit(MAHelperCommandInternalError);

    // if we're not being ran as root, spawn a copy that will make us suid root
    if (geteuid() != 0)
    {
      int status;
      int pid;
      FILE *commPipe = NULL;
      char *arguments[] = { "--self-repair", NULL };
      char buffer[1024];
      int bytesRead;
            
      if (AuthorizationExecuteWithPrivileges(auth, mypath, kAuthorizationFlagDefaults, arguments, &commPipe))
        exit(MAHelperCommandInternalError);

      for (;;)
      {
        bytesRead = read(0, buffer, 1024);
        if (bytesRead < 1) break;
        fwrite(buffer, 1, bytesRead, commPipe);
      }
      
      fflush(commPipe);
      fclose(commPipe);
  
      pid = wait(&status);
      if (pid == -1 || ! WIFEXITED(status))
        exit(MAHelperCommandInternalError);
      
      /* Exit with the same exit code as the child spawned by AuthorizationExecuteWithPrivileges() */
      exit(WEXITSTATUS(status));
    }
  }
  
  /* Read a command object from stdin. */
  bytesRead = read(0, &command, sizeof(MAHelperCommand));
  
  if (bytesRead == sizeof(MAHelperCommand))
  {
    const char *rightName = rightNameForCommand(&command);
    AuthorizationItem right = { rightName, 0, NULL, 0 } ;
    AuthorizationRights rights = { 1, &right };
    AuthorizationFlags flags = kAuthorizationFlagDefaults | kAuthorizationFlagInteractionAllowed
      | kAuthorizationFlagExtendRights;
        
    if (status = AuthorizationCopyRights(auth, &rights, kAuthorizationEmptyEnvironment, flags, NULL))
    {
      DEBUG("failed authorization in helper: %ld.\n", status);
      exit(MAHelperCommandAuthFailed);
    }
    
    /* Peform the requested command */
    if (!performCommand(&command))
      exit(MAHelperCommandOperationFailed);
  }
  else
  {
    exit(MAHelperCommandChildError);
  }
  
  return MAHelperCommandSuccess;
}
