//
//  utils.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Fri Jul 09 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>
#include "myx_admin_public_interface.h"

#include <MySQLToolsCommon/mxUtils.h>

#define _(s) s
#define N_(s) s

void MARunAlertPanelWithError(NSString *title, NSString *message, MYX_ADMIN_LIB_ERROR error);

