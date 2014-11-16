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
#include <sys/uio.h>
#include <sys/socket.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>


int
mhelperPerformCommand3(AuthorizationRef authorizationRef, 
					   const char *helperPath, MAHelperCommand command,
					   void(*output_callback)(const char*, void*),
					   void *callback_data,
					   int *fdret, char **error_message);


static int read_fd(int fd, char *buf, size_t bufsize, int *retfd)
{
  struct msghdr msg;
  struct iovec iov[1];
  union {
	struct cmsghdr cm;
	char control[CMSG_SPACE(sizeof(int))];
  } control_un;
  struct cmsghdr *cmptr;
  int rc;
  
  msg.msg_control= control_un.control;
  msg.msg_controllen= sizeof(control_un.control);

  msg.msg_name= NULL;
  msg.msg_namelen= 0;

  memset(buf, 0, bufsize);
  
  iov[0].iov_base= buf;
  iov[0].iov_len= bufsize-1;
  
  msg.msg_iov= iov;
  msg.msg_iovlen= 1;

  if ((rc= recvmsg(fd, &msg, 0)) <= 0)
  {
	DEBUG("Error reading data from helper: %s", strerror(errno));
	return rc;
  }
  
  if ((cmptr = CMSG_FIRSTHDR(&msg)) != NULL &&
	  cmptr->cmsg_len == CMSG_LEN(sizeof(int)))
  {
	if (cmptr->cmsg_level != SOL_SOCKET
		|| cmptr->cmsg_type != SCM_RIGHTS)
	{
	  DEBUG("Received socket smg has invalid parameters");
	  return -1;
	}
	*retfd= *((int*)CMSG_DATA(cmptr));
  }
  else
  {
	DEBUG("Invalid message received from helper");
	*retfd= -1;
  }
  
  return rc;
}


int
mhelperPerformCommand(AuthorizationRef authorizationRef, 
					   const char *helperPath, MAHelperCommand command)
{
  return mhelperPerformCommand2(authorizationRef, helperPath, command, NULL, NULL);
}


int
mhelperPerformOpenCommand(AuthorizationRef authorizationRef, 
						  const char *helperPath, MAHelperCommand command,
						  int *fdret, char **error_message)
{
  return mhelperPerformCommand3(authorizationRef, helperPath, command, NULL, NULL,
								fdret, error_message);
}
					   
int
mhelperPerformCommand3(AuthorizationRef authorizationRef, 
					   const char *helperPath, MAHelperCommand command,
					   void(*output_callback)(const char*, void*),
					   void *callback_data,
					   int *fdret, char **error_message)
{
  int tohelper[2], fromhelper[2];
  int childStatus;
  int written;
  pid_t pid;
  int rc;
  int sv[2];
  int ok;
  AuthorizationExternalForm extAuth;
	
	DEBUG("entering performCommand");
	
restart:
  ok= 1;
  childStatus= 0;
  
  if (error_message)
	*error_message= NULL;

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
  
  if (fdret)
  {
	if (socketpair(AF_LOCAL, SOCK_STREAM, 0, sv) < 0)
	{
	  close(tohelper[0]);
	  close(tohelper[1]);
	  close(fromhelper[0]);
	  close(fromhelper[1]);
	  return MAHelperCommandInternalError;
	}
  }

  if ((pid = fork()) < 0)
  {
	if (fdret)
	{
	  close(sv[0]);
	  close(sv[1]);
	}
    close(tohelper[0]);
    close(tohelper[1]);
    close(fromhelper[0]);
    close(fromhelper[1]);
    return MAHelperCommandInternalError;
  }
  else if (pid == 0)
  {
    char *const envp[] = { NULL };
	char sockfd[32];
	
	if (fdret)
	{
	  sprintf(sockfd, "%i", sv[1]);
	  close(sv[0]);
    }
    close(0);
    dup2(tohelper[0], 0);
    close(tohelper[0]);
    close(tohelper[1]);
    
    close(1);
    dup2(fromhelper[1], 1);
#ifndef DO_DEBUG
	close(2);
	dup2(fromhelper[1], 2);
#endif
    close(fromhelper[0]);
    close(fromhelper[1]);
    
	if (fdret)
	  execle(helperPath, helperPath, sockfd, (char*)NULL, envp);
	else
	  execle(helperPath, helperPath, (char*)NULL, envp);
    _exit(MAHelperCommandHelperNotFound);
  }
  signal(SIGPIPE, SIG_IGN);
  
  close(tohelper[0]);
  close(fromhelper[1]);
  if (fdret)
	close(sv[1]);

  DEBUG("sending auth...");
  if (write(tohelper[1], &extAuth, sizeof(extAuth)) != sizeof(extAuth))
	ok= 0;
  else
  {
    DEBUG("sending command...");
	written= write(tohelper[1], &command, sizeof(MAHelperCommand));  
	if (written != sizeof(MAHelperCommand))
	  ok= 0;
  }
  DEBUG("end sending.");
  if (ok && fdret)
  {
	char buffer[1024];
	
	// helper will send an error message in case of error
	// otherwise, an empty string and the fd later
	DEBUG("reading reply code...");
	*fdret= -1;
	// read reply code 
	if (read(fromhelper[0], buffer, sizeof(buffer)) < 0)
	  ok= false;
	else
	{
	  DEBUG("reply from helper was '%s'", buffer);
	  if (buffer[0])
	  {
		if (error_message)
		  *error_message= strdup(buffer);
	  }
	  else
	  {
		DEBUG("waiting for fd from socket...");
		// read the fd from the socket to the helper
		if (read_fd(sv[0], buffer, sizeof(buffer), fdret) < 0 || *fdret < 0)
		{
		}
		// this is the file name (not error)
		buffer[sizeof(buffer)-1]= 0;
		if (error_message)
		  *error_message= strdup(buffer);
		
		DEBUG("fd arrived %i (%s)", *fdret, buffer);
	  }
	}
  }

  if (ok && output_callback) 
  {
	char line[1024];
	int count;
    DEBUG("waiting for process to finish");
	while ((rc= waitpid(pid, &childStatus, WNOHANG)) == 0)
	{
	  count= read(fromhelper[0], line, sizeof(line));
	  if (count > 0)
	  {
		line[count]= 0;
		(*output_callback)(line, callback_data);
	  }
	}
  }
  else
  {
    DEBUG("waiting for error process to finish");
	rc= waitpid(pid, &childStatus, 0);
  }
  DEBUG("end");
  close(fromhelper[0]);
  if (fdret)
	close(sv[0]);
  close(tohelper[1]);
  
  if (WEXITSTATUS(childStatus) == MAHelperCommandNeedsRestart)
	goto restart;
	
  if (!ok)
	return MAHelperCommandInternalError;
  
  if (rc < 0)
	return MAHelperCommandInternalError;
  
  if (!WIFEXITED(childStatus))
    return MAHelperCommandInternalError;
  
  return WEXITSTATUS(childStatus);
}


int
mhelperPerformCommand2(AuthorizationRef authorizationRef, 
					   const char *helperPath, MAHelperCommand command,
					   void(*output_callback)(const char*, void*),
					   void *callback_data)
{
  return mhelperPerformCommand3(authorizationRef, helperPath, command,
								output_callback, callback_data, 
								NULL, NULL);
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
