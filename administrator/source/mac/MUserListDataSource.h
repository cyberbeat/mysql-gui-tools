//
//  MUserListDataSource.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Tue Jun 29 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>
#include "myx_admin_public_interface.h"

@class MUserHostItem;

@interface MUserItem : NSObject {  
 @public
  BOOL _userInfoSet;
 @protected
  BOOL _newUser;
  MYX_USER *_user;
  NSImage *_icon;
  NSString *_username;
  NSString *_orig_username;
  NSMutableArray *_hostArray;
}

- (id)initNewUser;
- (id)initWithName:(NSString*)name;
- (void)setUserInfo: (MYX_USER*)info;
- (MYX_USER*)userInfo;
- (NSArray*)hosts;
- (MUserHostItem*)addHost:(NSString*)host;
- (MUserHostItem*)itemForHost:(NSString*)host;
- (void)removeHost:(NSString*)host;
- (void)setOriginalName:(NSString*)name;
- (NSString*)originalName;
- (BOOL)isNewUser;
- (void)setSaved;

- (void)setName:(NSString*)name;
- (NSString*)name;

- (NSImage*)icon;

- (MYX_USER_OBJECT_PRIVILEGES*)findPrivilegesForObject:(NSString*)object
                                                  host:(NSString*)hostname;
- (MYX_USER_OBJECT_PRIVILEGES*)addPrivileges:(MYX_USER_OBJECT_PRIVILEGES*)privs
                                   forObject:(NSString*)object
                                        host:(NSString*)hostname;
@end


@interface MUserHostItem : NSObject {
  MUserItem *_user;
  NSString *_host;
}

- (id)initWithUser:(MUserItem*)user host:(NSString*)host;
- (MUserItem*)user;
- (NSString*)hostname;
@end

@interface MUserListDataSource : NSObject {
  NSMutableArray *_users;
  NSMutableArray *_filtered;
  NSImage *_icon;
  id _target;
  SEL _fetcher;
  NSString *_filter;
}

- (void)setDefaultUserIcon:(NSImage*)icon;
- (NSImage*)defaultUserIcon;

- (MUserItem*)itemForUser:(NSString*)name;

- (void)addUser:(MUserItem*)user;
- (void)removeUser:(MUserItem*)user;

- (void)setUserNames: (NSArray*)names;

- (void)filterUsers: (NSString*)filter;

- (void)setUserInfoFetcher: (id)target selector:(SEL)sel;

- (id)outlineView:(NSOutlineView *)outlineView 
            child:(int)index 
           ofItem:(id)item;

- (BOOL)outlineView:(NSOutlineView *)outlineView 
   isItemExpandable:(id)item;

- (int)outlineView:(NSOutlineView *)outlineView 
numberOfChildrenOfItem:(id)item;

- (id)outlineView:(NSOutlineView *)outlineView 
objectValueForTableColumn:(NSTableColumn *)tableColumn 
           byItem:(id)item;

@end
