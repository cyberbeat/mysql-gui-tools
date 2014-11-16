/*
 *  helper.h
 *  MySQL PreferencePane Helper
 *
 *  Created by Alfredo Kojima on Tue Aug 03 2004.
 *  Copyright (c) 2004, 2005, 2006 MySQL AB. All rights reserved.
 *
 */

#ifndef __HELPER_H__
#define __HELPER_H__

#include <Security/Authorization.h>
#include <sys/param.h>


typedef enum
{
  MAHelperStartMySQL = 1,         // executes mysql.server start
  MAHelperStopMySQL = 2,          // executes mysql.server stop
  MAHelperToggleAutoStart = 3,    // edits /etc/hostconfig
  MAHelperShutdownMySQL = 4,      // executes mysqladmin shutdown
  
  MAHelperToggleUseMySQLD, 
  MAHelperSetIMPassword,
  
  // following commands will send the open file descriptor through the socket
  MAHelperCreateMyCnf= 100      // opens /etc/my.cnf.new writable
  
} MAHelperCommandType;


typedef struct
{
  MAHelperCommandType authorizedCommandId;
  char username[33];
  char password[33];
  bool enable;
} MAHelperCommand;


// Exit codes (positive values) and return codes from exec function
enum
{
  MAHelperCommandInternalError = -1,
  MAHelperCommandSuccess = 0,
  MAHelperCommandSuccessTrue,
  MAHelperCommandSuccessFalse,
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
