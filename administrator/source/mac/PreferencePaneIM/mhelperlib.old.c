/*
 *  mhelperlib.c
 *  MySQLStartup
 *
 *  Created by Alfredo Kojima on 1/4/05.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#include "mahelper.h"

#include <Security/Authorization.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>

int
mhelperPerformCommand(AuthorizationRef authorizationRef, 
                      const char *helperPath, MAHelperCommand command)
{
  int tohelper[2], fromhelper[2];
  int childStatus = 0;
  int written;
  pid_t pid;
//  char buffer[1024];
  AuthorizationExternalForm extAuth;
  
  // authref --> bytestream
  if (AuthorizationMakeExternalForm(authorizationRef, &extAuth))
    return MAHelperCommandInternalError;
  
  if (pipe(tohelper) < 0)
    return MAHelperCommandInternalError;
  
  if (pipe(fromhelper) < 0)
  {
    close(tohelper[0]);
    close(tohelper[1]);
    return MAHelperCommandInternalError;
  }
  
  if ((pid = fork()) < 0)
  {
    close(tohelper[0]);
    close(tohelper[1]);
    close(fromhelper[0]);
    close(fromhelper[1]);
    return MAHelperCommandInternalError;
  }
  else if (pid == 0)
  {
    char *const envp[] = { NULL };
    
    close(0);
    dup2(tohelper[0], 0);
    close(tohelper[0]);
    close(tohelper[1]);
    
    close(1);
	close(2);
    dup2(fromhelper[1], 1);
    dup2(fromhelper[1], 2);
    close(fromhelper[0]);
    close(fromhelper[1]);
    
    execle(helperPath, helperPath, NULL, envp);
    _exit(MAHelperCommandHelperNotFound);
  }
  signal(SIGPIPE, SIG_IGN);
  
  close(tohelper[0]);
  close(fromhelper[1]);
  
  if (write(tohelper[1], &extAuth, sizeof(extAuth)) != sizeof(extAuth))
  {
    close(tohelper[1]);
    close(fromhelper[0]);
    return MAHelperCommandInternalError;
  }
  
  written= write(tohelper[1], &command, sizeof(MAHelperCommand));
  
  close(tohelper[1]);
  
  
  if (written != sizeof(MAHelperCommand))
  {
    close(fromhelper[0]);
    return MAHelperCommandInternalError;
  }

//  read(fromhelper[0], buffer, 1);

  close(fromhelper[0]);
  
  if (waitpid(pid, &childStatus, 0) != pid)
    return MAHelperCommandInternalError;
  
  if (!WIFEXITED(childStatus))
    return MAHelperCommandInternalError;
  
  return WEXITSTATUS(childStatus);
}


int 
mautoStartState()
{
  int autoStart= 0;
  
  // check if autostart is enabled
  {
    char buffer[1024];
    FILE *f;
    f= fopen("/etc/hostconfig","r");
    if (f)
    {
      while (fgets(buffer, sizeof(buffer), f))
      {
        if (strncmp(buffer,"MYSQLCOM=", sizeof("MYSQLCOM=")-1)==0)
        {
          if (strstr(buffer, "-YES-"))
          {
            autoStart= 1;
            break;
          }
        }
      }
      fclose(f);
    }
  }
  return autoStart;
}
