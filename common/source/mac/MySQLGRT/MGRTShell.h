//
//  MGRTShell.h
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/7/13.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLGRT/MGRT.h>
#import "myx_grt_public_interface.h"


@class MShellTextView;
@class MGRTValueTreeDataSource;

@interface MGRTShell : NSObject
{
  IBOutlet NSWindow *window;
  IBOutlet MShellTextView *textView;
  IBOutlet NSOutlineView *valueTree;
  IBOutlet NSTableView *valueList;
  IBOutlet NSTextField *valueText;
  IBOutlet NSOutlineView *structTree;
  IBOutlet NSOutlineView *moduleTree;
  IBOutlet NSComboBox *rootCombo;
  IBOutlet NSTextField *moduleInfo;

  NSToolbar *_toolbar;
  
  GHashTable *_nodeTable;
  MGRTValueTreeDataSource *_valueDS;
  
  NSImage *_packageIcon;
  NSImage *_structIcon;
  NSImage *_valueIcon;
  
  NSImage *_moduleIcon;
  NSImage *_funcIcon;

  MYX_GRT_VALUE *_detailValue;
  
  MGRT *_grt;
}

- (IBAction)changeRoot:(id)sender;
- (IBAction)reloadTree:(id)sender;
- (IBAction)reloadItem:(id)sender;
- (IBAction)reloadDetails:(id)sender;

- (id)initWithMGRT:(MGRT*)grt;

- (void)show:(id)sender;
- (void)hide:(id)sender;

@end
