//
//  MAXMLGUIController.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Wed Jul 21 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#include "myx_admin_public_interface.h"

@class MAOptionWidget;

@interface MAXMLGUIController : NSObject {
  NSMutableDictionary *_optionControls;
  NSMutableDictionary *_optionToggles;
  NSMutableDictionary *_optionCaptions;
  
  NSString *_file;
  NSString *_section;
  
  NSImage *_editableImg;
  NSImage *_disabledImg;
  
  MYX_GUI_DESCRIPTION *_descr;
}

- (MAXMLGUIController*)initInTabView:(NSTabView*)tab
                               mysql:(MYSQL*)mysql
                             version:(const char*)version
                                file:(NSString*)file
                             section:(NSString*)section;

- (void)revertToSaved;
- (BOOL)saveTo:(int)fd;

@end
