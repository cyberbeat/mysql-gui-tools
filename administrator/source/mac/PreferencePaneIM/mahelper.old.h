/*
 *  mahelper.h
 *  MySQL Administrator
 *
 *  Created by Alfredo Kojima on Tue Aug 03 2004.
 *  Copyright (c) 2004 MySQL AB. All rights reserved.
 *
 */

#ifndef __MAHELPER_H__
#define __MAHELPER_H__

#include <Security/Authorization.h>
#include <sys/param.h>


typedef enum
{
  MAHelperStartMySQL = 1,
  MAHelperStopMySQL = 2,
  MAHelperToggleAutoStart = 3
} MAHelperCommandType;


typedef struct
{
  MAHelperCommandType authorizedCommandId;
  
  char argumentPath[1024];
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
  MAHelperCommandHelperNotFound
};



int mhelperPerformCommand(AuthorizationRef authorizationRef, 
                          const char *helperPath, MAHelperCommand command);
int mautoStartState();

#endif
