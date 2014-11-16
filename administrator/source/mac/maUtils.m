//
//  utils.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Fri Jul 09 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "maUtils.h"


void MARunAlertPanelWithError(NSString *title, NSString *message, MYX_ADMIN_LIB_ERROR error)
{
  static NSString *msgs[]= {
    @"",
    N_(@"Can't open file."),
    N_(@"Error parsing XML file."),
    N_(@"Error parsing XML file (parse error)."),
    N_(@"Error parsing XML file (bad document)."),
    N_(@"Error parsing XML file (empty document)."),
    N_(@"Error parsing INI file."),
    N_(@"General error."),
    N_(@"SQL error.")
  };
  
  NSRunAlertPanel(title, @"%@\n%@", nil, nil, nil, message, 
                  NSLocalizedString(msgs[error],nil));
}
