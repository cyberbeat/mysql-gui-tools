//
//  MGRTRevEngFilterPane.h
//  MySQL Workbench
//
//  Created by Alfredo Kojima on 05/8/30.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLGRT/MGRT.h>

@interface MGRTRevEngFilterPane : NSObject 
{
  IBOutlet NSView *view;
  IBOutlet NSBox *box;
  IBOutlet NSTableView *objectList;
  IBOutlet NSTableView *ignoreList;
  
  NSMutableArray *_sourceObjects;
  
  MGRT *_grt;
  NSString *_structName;
}

- (IBAction)showDetailed:(id)sender;
- (IBAction)hideDetailed:(id)sender;
- (IBAction)addItem:(id)sender;
- (IBAction)deleteItem:(id)sender;
- (IBAction)toggleMigrate:(id)sender;

- (id)initWithMGRT:(MGRT*)grt forStruct:(NSString*)structName;
- (NSView*)topView;

- (void)setSelected:(BOOL)flag;

- (void)refreshLists;

@end
