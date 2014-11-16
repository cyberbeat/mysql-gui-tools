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

#include "mahelper_priv.h"


static const char *
rightNameForCommand(const MAHelperCommand *cmd)
{
  switch (cmd->authorizedCommandId)
  {
    case MAHelperStartMySQL:
    case MAHelperStopMySQL:
#ifdef MYSQL_PREFPANE
    case MAHelperShutdownMySQL:
#endif
      return "com.mysql.administrator.server";
    case MAHelperToggleAutoStart:
      return "com.mysql.administrator.chconfig";

#ifndef MYSQL_PREFPANE
	case MAHelperCommitMyCnf:
	case MAHelperOpenMyCnf:
	case MAHelperOpenNewMyCnf:
	  return "com.mysql.administrator.write_mycnf";

	case MAHelperOpenErrorLog:
	case MAHelperOpenGeneralLog:
	case MAHelperOpenSlowLog:
	  return "com.mysql.administrator.read_log";
#endif
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
  char *args[8];
  int argc;
  pid_t pid;
  int status;
  char *const envp[] = { "PATH=/bin:/usr/bin:/sbin:/usr/sbin", NULL };

	DEBUG("entering performExternalCommand as uid=%i euid=%i",
				getuid(), geteuid());
	
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
#ifdef MYSQL_PREFPANE
    case MAHelperShutdownMySQL:
      argc= 0;
      args[argc++]= MYSQLADMIN_COMMAND;
      /*
       if (cmd->my_username[0])
       {
         args[argc++]= malloc(strlen(cmd->my_username)+8);
         sprintf(args[argc-1], "-u%s", cmd->my_username);
       }
       if (cmd->my_password[0])
       {
         char *password= malloc(strlen(cmd->my_password)+8);
         sprintf(password, "-p%s", cmd->my_password);
         args[argc++]= password;
       }
       */
        args[argc++]= "shutdown";
      break;
#endif
    default:
      return false;
  }
  
  DEBUG("will execute %s %s", args[0], args[1]);
  
  args[argc]= NULL;
  if ((pid= fork()) == 0)
  { 
		setsid();

		// this is apparently needed otherwise the script won't execute in leopard
		DEBUG("setting uid to 0");
		setuid(0);
		DEBUG("new uid=%i euid=%i",
					getuid(), geteuid());
  
    execve(args[0], args, envp);
    exit(222);
  }
  else if (pid < 0)
    return false;

  wait(&status);
	DEBUG("return status of %s is %i", args[0], status);
  if (pid == 222 || ! WIFEXITED(status))
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


static bool performCommand(MAHelperCommand *cmd)
{
  switch (cmd->authorizedCommandId)
  {
    case MAHelperStartMySQL:
    case MAHelperStopMySQL:
#ifdef MYSQL_PREFPANE
    case MAHelperShutdownMySQL:  
#endif
      return performExternalCommand(cmd);
    case MAHelperToggleAutoStart:
      return toggleAutoStart(cmd->enable);
    default:
      return false;
  }
}

int main(int argc, char **argv)
{
  char mypath[MAXPATHLEN];
  size_t mypath_size= sizeof(mypath);
  OSStatus status;
  AuthorizationRef auth;
  int bytesRead;
  MAHelperCommand command;
  int i;
  bool selfRepair= false;
  char *sockarg= NULL;
 
	DEBUG("entering mahelper...");
	
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
    AuthorizationItem right = { rightName, 0, NULL, 0 } ;
    AuthorizationRights rights = { 1, &right };
    AuthorizationFlags flags = kAuthorizationFlagDefaults | kAuthorizationFlagInteractionAllowed
      | kAuthorizationFlagExtendRights;
	
		DEBUG("command ok, checking rights");

    if ((status = AuthorizationCopyRights(auth, &rights, kAuthorizationEmptyEnvironment, flags, NULL)))
    {
      DEBUG("failed authorization in helper: %ld.\n", status);
      exit(MAHelperCommandAuthFailed);
    }

		DEBUG("authorized command %i", command.authorizedCommandId);
    /* Peform the requested command */
    if (command.authorizedCommandId < 100)
		{
			if (!performCommand(&command))
				exit(MAHelperCommandOperationFailed);
		}
#ifndef MYSQL_PREFPANE
		else
		{
			/* argv[1] must be fd of socketpair */
			if (!performOpenCommand(&command, atoi(sockarg)))
				exit(MAHelperCommandOperationFailed);
		}
#endif
  }
  else
  {
	DEBUG("error reading command.");
    exit(MAHelperCommandChildError);
  }
  
  DEBUG("exiting normally...");
  
  return MAHelperCommandSuccess;
}
