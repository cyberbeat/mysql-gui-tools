//
//  MQQueryController.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/31/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLToolsCommon/MSourceTextEditor.h>

@class MQResultSetTab;
@class MQueryBrowser;
@class MSchemaItem;
#include <myx_public_interface.h>


@interface MQQueryAreaEditor : MSourceTextEditor 
{
}
@end

@interface MQQueryController : NSObject 
{
  IBOutlet MQueryBrowser *owner;
  
  IBOutlet NSImageView *animbox;
  IBOutlet MMenuButton *backButton;
  IBOutlet MMenuButton *executeButton;
  IBOutlet MMenuButton *nextButton;
  IBOutlet MQQueryAreaEditor *queryEditor;
  IBOutlet NSButton *stopButton;  
  
  IBOutlet NSMenu *backMenu;
  IBOutlet NSMenu *executeMenu;
  IBOutlet NSMenu *nextMenu;  
  
  IBOutlet NSView *bigToolbar;
  IBOutlet NSView *toolbar;
  
  id queryArea;
  
  NSImage *_idleImage;
  NSImage *_busyImage;

  MYX_SYN *_syntaxInfo;
  
  NSMutableArray *_localHistory;
  int _localHistoryPos;
  
  NSMutableArray *_queryViews;
  
  id _lastSelectedPage;
  NSRect _queryAreaFrame;

  BOOL _switching;
}

- (IBAction)compareQuery:(id)sender;
- (IBAction)explainQuery:(id)sender;

- (IBAction)executeQuery:(id)sender;
- (IBAction)stopQuery:(id)sender;
- (IBAction)goBack:(id)sender;
- (IBAction)goNext:(id)sender;

- (IBAction)rollbackTransaction:(id)sender;
- (IBAction)startTransaction:(id)sender;
- (IBAction)commitTransaction:(id)sender;


- (IBAction)splitV:(id)sender;
- (IBAction)splitH:(id)sender;
- (IBAction)unsplit:(id)sender;

- (void)exportResultSet:(id)sender;

- (void)selectQueryArea:(id)sender;
- (void)willShowPage:(id)page;
- (void)didHidePage;

- (NSString*)currentQuery;
- (void)performQuery:(NSString*)query;
- (BOOL)setupQueryWithItem:(MSchemaItem*)item;
- (void)setupSelectQueryWithItem:(MSchemaItem*)item rowLimit:(unsigned int)limit;

- (MQResultSetTab*)createPage;
- (MQResultSetView*)addNewResultSet:(BOOL)vertical toPage:(MQResultSetTab*)page;
- (MQResultSetView*)currentResultSetView;

- (void)setDefaultSchema:(NSString*)schema;
- (void)switchToolbar:(BOOL)small;
- (void)resizeToolbar:(float)height;

- (void)updateHistoryMenus;

- (BOOL)stateOfMenuItem:(NSMenuItem*)item atIndex:(int)index inMenu:(NSMenu*)menu;
- (void) saveQuery: (id) sender;

@end
