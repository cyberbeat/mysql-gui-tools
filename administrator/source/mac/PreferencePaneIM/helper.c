/*
 *  mahelper.c
 *  MySQL Administrator
 *
 *  Created by Alfredo Kojima on Tue Aug 03 2004.
 *  Copyright (c) 2004 MySQL AB. All rights reserved.
 *
 */

#include "helper.h"

#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <mach-o/dyld.h>

#include "myx_instanceconf_public_interface.h"
#include "mahelper_priv.h"

//#define ENABLE_LOGGING

#ifdef ENABLE_LOGGING
static FILE *logf= 0;
#undef DEBUG
#define DEBUG(fmt, ...) fprintf(logf, "mahelper: "fmt"\n", ##__VA_ARGS__), fflush(logf)
#endif


static const char *
rightNameForCommand(const MAHelperCommand *cmd)
{
  switch (cmd->authorizedCommandId)
  {
    case MAHelperStartMySQL:
    case MAHelperStopMySQL:
    case MAHelperShutdownMySQL:
      return "com.mysql.prefPane.server.startStop";

    case MAHelperToggleUseMySQLD:
    case MAHelperToggleAutoStart:
    case MAHelperSetIMPassword:
      return "com.mysql.prefPane.server.changeSettings";

    case MAHelperCreateMyCnf:
      return "com.mysql.prefPane.server.configure";      
  }
  return "system.unknown";
}





static bool performExternalCommand(const MAHelperCommand *cmd)
{
  char *args[8];
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
    case MAHelperShutdownMySQL:
      argc= 0;
      args[argc++]= MYSQLADMIN_COMMAND;
      args[argc++]= "shutdown";
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

static bool performCommand(MAHelperCommand *cmd)
{
  switch (cmd->authorizedCommandId)
  {
    case MAHelperStartMySQL:
    case MAHelperStopMySQL:
    case MAHelperShutdownMySQL: 
      DEBUG("start/stop mysql");
      return performExternalCommand(cmd);

    case MAHelperToggleUseMySQLD:
      DEBUG("toggle usemysqld");
      if (mim_instance_manager_make_active() < 0)
        return false;
      return true;

    case MAHelperToggleAutoStart:
      DEBUG("toggle autostart");
      if (mim_set_auto_starts(cmd->enable) < 0)
        return false;
      return true;
      
    case MAHelperSetIMPassword:
      cmd->username[sizeof(cmd->username)-1]= 0;
      cmd->password[sizeof(cmd->password)-1]= 0;
      DEBUG("setting IM password to %s %s", cmd->username, cmd->password);
      if (mim_reset_password(cmd->username, cmd->password, "/etc/mysqlmanager.passwd") < 0)
        return false;
      return true;
  }
  return false;
}


int main(int argc, char **argv)
{
  char mypath[MAXPATHLEN];
  unsigned int mypath_size= sizeof(mypath);
  OSStatus status;
  AuthorizationRef auth;
  int bytesRead;
  MAHelperCommand command;
  int i;
  bool selfRepair= false;
  char *sockarg= NULL;

#ifdef ENABLE_LOGGING
  logf= fopen("/tmp/log", "a+");
  DEBUG("created logf");
#endif
  if (_NSGetExecutablePath(mypath, &mypath_size) < 0)
  {
    DEBUG("could not get my path");
    exit(MAHelperCommandInternalError);
  }
  
  for (i= 1; i < argc; i++)
  {
    if (strcmp(argv[i], "--self-repair")==0)
      selfRepair= true;
    else
      sockarg= argv[i];
  }
  
  if (selfRepair) // make ourself suid root
  {    
    struct stat st;
    int fd_tool;

    DEBUG("self repairing..");

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
      fchown(fd_tool, 0, 0);
    
    fchmod(fd_tool, (st.st_mode & (~(S_IWGRP|S_IWOTH))) | S_ISUID | S_ISGID);
    
    close(fd_tool);
    
    DEBUG("self-repair done, returning.");
    
    return MAHelperCommandNeedsRestart;
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
      char *arguments[] = { "--self-repair", sockarg, NULL };
      
      DEBUG("requesting auto-repair");
      if (AuthorizationExecuteWithPrivileges(auth, mypath, kAuthorizationFlagDefaults, arguments, NULL))
        exit(MAHelperCommandInternalError);
      DEBUG("waiting to auto-repairing child to finish");
      pid = wait(&status);
      DEBUG("finished.");
      if (pid == -1 || ! WIFEXITED(status))
        exit(MAHelperCommandInternalError);
      DEBUG("will exit with status= %i", status);
      /* Exit with the same exit code as the child spawned by AuthorizationExecuteWithPrivileges() */
      exit(WEXITSTATUS(status));
    }
  }
  
  DEBUG("reading command...");
  
  /* Read a command object from stdin. */
  bytesRead = read(0, &command, sizeof(MAHelperCommand));
  
  if (bytesRead == sizeof(MAHelperCommand))
  {
    const char *rightName = rightNameForCommand(&command);

    if (rightName)
    {
      AuthorizationItem right = { rightName, 0, NULL, 0 } ;
      AuthorizationRights rights = { 1, &right };
      AuthorizationFlags flags = kAuthorizationFlagDefaults | kAuthorizationFlagInteractionAllowed
        | kAuthorizationFlagExtendRights;
      AuthorizationRights *authorizedRights= NULL;
      int i;
      
      DEBUG("command ok, checking rights");
    
      if ((status = AuthorizationCopyRights(auth, &rights, kAuthorizationEmptyEnvironment, flags, &authorizedRights)))
      {
        DEBUG("failed authorization in helper: %ld.\n", status);
        exit(MAHelperCommandAuthFailed);
      }
    
      if (authorizedRights)
      {
        for (i= 0; i < authorizedRights->count; i++)
          if (strcmp(authorizedRights->items[i].name, rightName)==0)
            break;
        if (i == authorizedRights->count)
        {
          DEBUG("missing authorization for right %s in helper\n", rightName);
          exit(MAHelperCommandAuthFailed);
        }
      }
      else
      {
        DEBUG("missing authorization for right %s in helper\n", rightName);
        exit(MAHelperCommandAuthFailed);
      }
    }
    
    /* Peform the requested command */
    if (command.authorizedCommandId < 100)
    {
      if (!performCommand(&command))
        exit(MAHelperCommandOperationFailed);
    }
    else
    {
      /* argv[1] must be fd of socketpair */
      if (!performOpenCommand(&command, atoi(sockarg)))
        exit(MAHelperCommandOperationFailed);
    }
  }
  else
  {
    DEBUG("error reading command.");
    exit(MAHelperCommandChildError);
  }
  
  DEBUG("exiting normally...");
  
  return MAHelperCommandSuccess;
}
