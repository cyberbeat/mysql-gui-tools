//
//  MPreferences.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Fri Jul 09 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>

#include "myx_public_interface.h"

@interface MPreferences : NSObject {
  NSUserDefaults *_defaults;
  @public
  MYX_PASSWORD_STORAGE_TYPE passwordStorageType;
}

+ (MPreferences*)preferences;
+ (void)setInstance:(MPreferences*)pref;

+ (NSString*)checkDirectory:(NSString*)path;

- (NSString*)pathForPreferences;
- (NSString*)pathForFile:(NSString*)file;

@end
