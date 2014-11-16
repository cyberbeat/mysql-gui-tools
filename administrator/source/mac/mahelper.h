/*
 *  mahelper.h
 *  MySQL Administrator
 *
 *  Created by Alfredo Kojima on Tue Aug 03 2004.
 *  Copyright (c) 2004, 2005, 2006 MySQL AB. All rights reserved.
 *
 */

#ifndef __MAHELPER_H__
#define __MAHELPER_H__

#include <Security/Authorization.h>
#include <sys/param.h>


#define DO_DEBUG

#ifdef DO_DEBUG
#include <syslog.h>
//#define DEBUG(fmt, ...) fprintf(stderr, "mahelper: "fmt"\n", ##__VA_ARGS__)
#define DEBUG(fmt, ...) 	syslog(LOG_DEBUG, fmt, ##__VA_ARGS__)
#else
#define DEBUG(fmt, ...) 
#endif


typedef enum
{
  MAHelperStartMySQL = 1,         // executes mysql.server start
  MAHelperStopMySQL = 2,          // executes mysql.server stop
#ifdef MYSQL_PREFPANE
  MAHelperShutdownMySQL = 4,          // executes mysqladmin shutdown
#endif
  MAHelperToggleAutoStart = 3,    // edits /etc/hostconfig
  
#ifndef MYSQL_PREFPANE
  // following commands will send the open file descriptor through the socket
  MAHelperOpenMyCnf= 100,         // opens /etc/my.cnf readonly
  MAHelperOpenNewMyCnf= 101,      // opens /etc/my.cnf.new writable
  MAHelperCommitMyCnf= 102,       // backup my.cnf, rename my.cnf.new to my.cnf
  
  MAHelperOpenErrorLog= 105,      // opens the error log file
  MAHelperOpenGeneralLog= 106,    // opens the geenral log file
  MAHelperOpenSlowLog= 107        // opens the slow log file
#endif
} MAHelperCommandType;


typedef struct
{
  MAHelperCommandType authorizedCommandId;
  bool enable;  
} MAHelperCommand;


// Exit codes (positive values) and return codes from exec function
enum
{
  MAHelperCommandInternalError = -1,
  MAHelperCommandSuccess = 0,
  MAHelperCommandExecFailed,
  MAHelperCommandChildError,
  MAHelperCommandAuthFailed,
  MAHelperCommandOperationFailed,
  MAHelperCommandCancelled,
  MAHelperCommandHelperNotFound,
  MAHelperCommandNeedsRestart
};


int mhelperPerformCommand2(AuthorizationRef authorizationRef, 
						   const char *helperPath, MAHelperCommand command,
						   void(*output_callback)(const char*, void*),
						   void *callback_data);

int mhelperPerformCommand(AuthorizationRef authorizationRef, 
                          const char *helperPath, MAHelperCommand command);

int mhelperPerformOpenCommand(AuthorizationRef authorizationRef, 
							  const char *helperPath, MAHelperCommand command,
							  int *fdret, char **error_message);

int mautoStartState();

#endif
