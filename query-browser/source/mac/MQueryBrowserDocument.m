//
//  MQueryBrowserDocument.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/22.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <MySQLToolsCommon/MConnectionInfo.h>
#import "MQueryBrowserDocument.h"
#import "MQueryBrowser.h"

@implementation MQueryBrowserDocument


- (void)makeWindowControllers
{
  MQueryBrowser *browser= [[MQueryBrowser alloc] init];  
  [self addWindowController:browser];
  [browser setDocument:self];
  [browser loadWindow];
  [browser synchronizeWindowTitleWithDocumentName];
  [[browser window] makeKeyAndOrderFront:nil];
  [browser release];
}


- (id)initWithConnection: (MYSQL*)mysql
                    info: (MConnectionInfo*)info
{
  self = [super init];
  if (self)
  {
    _mysql=mysql;
    _info= [info retain];
    
    [_info _setOwner:self];
 
    _dispatcher= [[MMySQLDispatcher alloc] initWithMySQL:mysql instance:info];
    
    if (mysql->unix_socket)
      _connectedInstance= [[NSString alloc] initWithFormat:@"%@ via socket", [info hostname]];
    else
      _connectedInstance= [[NSString alloc] initWithFormat:@"%@:%i", [info hostname], [info port]];    
  }
  return self;
}

- (IBAction) saveDocument:(id)sender
{
  MQueryBrowser *qb= [[self windowControllers] objectAtIndex: 0];
  [qb saveQueryOrScript];
}

- (BOOL)validateMenuItem:(NSMenuItem *)anItem
{
  // any better way to detect Save menu item?
  if([anItem action] == @selector(saveDocument:))
    return YES;
  return [super validateMenuItem: anItem];
}

- (void)dealloc
{
  if (_mysql)
    myx_mysql_close(_mysql);
  [_info release];
  [_dispatcher release];

  [super dealloc];
}


- (NSData *)dataRepresentationOfType:(NSString *)type 
{
  NSLog(@"repr for %@", type);
  return nil;
}

- (BOOL)loadDataRepresentation:(NSData *)data ofType:(NSString *)type 
{
  NSLog(@"load data %@", type);
  return YES;
}

- (void)unlockMySQL
{
  [_mysqlLock unlock];
}

- (MYSQL*)mysql
{
  return _mysql;
}

- (MYSQL*)mysqlLock
{
  [_mysqlLock lock];
  return _mysql;
}

- (MConnectionInfo*)serverInfo
{
  return _info;
}


- (MMySQLDispatcher*)dispatcher
{
  return _dispatcher;
}


- (void)killConnection:(unsigned long)pid
{
  mysql_kill(_mysql, pid);
}

- (NSString*)connectedInstance
{
  return _connectedInstance;
}

- (NSString*)lastComponentOfFileName
{
  return _connectedInstance;
}

- (BOOL)connectedMySQLIsAtLeastMajor:(int)major minor:(int)minor
{
  return mysql_version_is_later_or_equal_than(_mysql,major,minor);
}

- (void)setDefaultSchema:(NSString*)schema
{
  [_info setSchema:schema];
  
  if (schema)
    myx_use_schema(_mysql, [schema UTF8String]);
}


- (NSString*)defaultSchema
{
  return [_info schema];
}


- (BOOL)menu:(NSMenu *)menu
  updateItem:(NSMenuItem *)item
	 atIndex:(int)index
shouldCancel:(BOOL)shouldCancel
{
  return [[[self windowControllers] lastObject] menu:menu
                                          updateItem:item
                                             atIndex:index
                                        shouldCancel:shouldCancel];
}


@end
