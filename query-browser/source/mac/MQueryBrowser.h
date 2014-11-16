//
//  MQueryBrowser.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on Sat Mar 5 2005.
//  Copyright (c) 2005 MySQL AB. All rights reserved.
//


#import <Cocoa/Cocoa.h>

#import <MySQLToolsCommon/MSchemaDataSource.h>
#include <myx_public_interface.h>
#include <myx_qb_public_interface.h>

@class MTabView;
@class MSchemaDataSource;
@class MSchemaEditHelper;
@class MMenuButton;
@class MQParameters;

@class MQScriptController;
@class MQQueryController;
@class MQSyntaxHelp;

@interface MQueryBrowser : NSWindowController
{
  IBOutlet NSOutlineView *bookmarkList;
  IBOutlet NSOutlineView *catalogList;
  IBOutlet NSDrawer *drawer;
  IBOutlet NSOutlineView *historyList;
  IBOutlet NSOutlineView *parameterList;
  IBOutlet NSOutlineView *syntaxList;
  IBOutlet NSOutlineView *functionList;
  IBOutlet MTabView *tabView;
  IBOutlet NSMenu *catalogMenu;
  IBOutlet NSMenu *bookmarkMenu;
  IBOutlet NSMenu *historyMenu;

  IBOutlet MQScriptController *scriptCtl;
  IBOutlet MQQueryController *queryCtl;

  IBOutlet MSchemaEditHelper *schemaHelper;
    
  NSImage *_historyGroupIcon;
  NSImage *_historyItemIcon;
  
  NSImage *_bookmarkGroupIcon;
  NSImage *_bookmarkItemIcon;
  
  NSImage *_groupIcon;
  NSImage *_paramIcon;

  NSFont *_normalFont;
  NSFont *_boldFont;

  MYX_CATALOGS *_catalogs;
  MSchemaDataSource *_columnDS;

  MQParameters *_parameters;
  
  float _toolbarHeight;
  BOOL _smallToolbar;
}
- (IBAction)addQueryTab:(id)sender;
- (IBAction)addScriptTab:(id)sender;

- (IBAction)toggleDrawer:(id)sender;

- (IBAction)schemataDoubleClicked:(id)sender;

- (IBAction)refreshSchemata:(id)sender;
- (IBAction)setDefaultSchema:(id)sender;

- (IBAction)copyBookmark:(id)sender;
- (IBAction)executeBookmark:(id)sender;
- (IBAction)newBookmarkGroup:(id)sender;
- (IBAction)deleteBookmarkItem:(id)sender;
- (IBAction)renameBookmarkItem:(id)sender;
- (IBAction)bookmarkQuery:(id)sender;

- (IBAction)copyHistory:(id)sender;
- (IBAction)executeHistory:(id)sender;
- (IBAction)deleteHistory:(id)sender;

- (IBAction)switchToolbarType:(id)sender;

- (IBAction)addParameter:(id)sender;
- (IBAction)deleteParameter:(id)sender;

- (void)setToolbarHeight:(float)height;

- (id)currentTab;

- (void)setSmallToolbar:(BOOL)flag;
- (BOOL)smallToolbar;

- (MSchemaEditHelper*)schemaHelper;
- (MQParameters*)parameters;
- (NSOutlineView*)parameterList;

- (NSString *) queryString;
- (void) saveScript: (id)sender;
- (void) saveQueryOrScript;

@end
