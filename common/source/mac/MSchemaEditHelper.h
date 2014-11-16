//
//  MSchemaEditHelper.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 3/23/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#include "myx_public_interface.h"

@class MSchemaItem;

@interface MSchemaEditHelper : NSObject 
{
  IBOutlet NSWindow *parentWindow;
  IBOutlet NSOutlineView *outline;
  IBOutlet id delegate;

  NSMutableArray *_editors;
  
  MYSQL *_mysql;
}

- (void)setConnection:(MYSQL*)mysql;
- (void)setDelegate:(id)deleg;

- (IBAction)createSchema:(id)sender;
- (IBAction)createTable:(id)sender;
- (IBAction)createSP:(id)sender;
- (IBAction)createView:(id)sender;

- (IBAction)editSelection:(id)sender;

- (IBAction)dropSelection:(id)sender;

- (IBAction)copySelectionSQL:(id)sender;

- (BOOL)editStoredProceduresInCatalog:(NSString*)catalog schema:(NSString*)schema;

- (NSString*)getCreateCommandForItem:(MSchemaItem*)item;
@end
