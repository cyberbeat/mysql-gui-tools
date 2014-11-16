//
//  MConnectionInfo.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Wed Jun 23 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#include <myx_public_interface.h>

@interface MConnectionInfo : NSObject {
  id _owner;
  
  NSString *name;
  NSString *username;
  NSString *password;
  NSString *hostname;
  int port;
  NSString *schema;
  NSMutableArray *advancedOptions;
//  NSString *storage_path;
//  NSString *notes;
//  enum myx_user_connection_type connection_type;
//  enum myx_user_connection_storage_type storage_type;
}

- (void)_setOwner:(id)owner;

+ (MConnectionInfo*)connectionInfoFromUserConnection: (MYX_USER_CONNECTION*)info;
- (id)initWithUserConnection:(MYX_USER_CONNECTION*)info;

- (NSString*)username;
- (NSString*)hostname;
- (NSString*)password;
- (int)port;
- (NSString*)schema;
- (void)setSchema:(NSString*)schema;

- (NSString*)socketPath;

- (NSString*)description;

- (MYSQL*)connect;
- (BOOL)reconnect:(MYSQL*)mysql;
- (MYX_USER_CONNECTION*)createUserConnection;

@end

