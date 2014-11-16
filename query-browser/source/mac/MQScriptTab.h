//
//  MQScriptTab.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/25/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#import <MySQLToolsCommon/MTabView.h>
#import "MQScriptTextView.h"

#include <myx_public_interface.h>

extern NSString *MQScriptEditStatusChangedNotification;

@class MSQLSyntaxColoring;
@class MQScriptExecutionHelper;
@class MSplitView;
@class MConnectionInfo;

@interface MQScriptTab : MTabViewItem
{
  IBOutlet MQScriptEditor *editor;
  IBOutlet MSplitView *splitView;
  IBOutlet NSTableView *messageTable;
  IBOutlet NSTextField *statusText;
  IBOutlet NSImageView *statusImage;
  
  MSQLSyntaxColoring *_colorer;
  
  MQScriptExecutionHelper *_helper;

  NSString *_defaultSchema;
  MYSQL *_mysql;
  MConnectionInfo *_info;
  
  MYX_SYN *_syn;

  MYX_MYSQL_ERROR_MSGS *_errors;

  NSString *_filename;
  BOOL _documentEdited;
}

- (id)initWithIdentifier:(NSString*)identifier withConnectionTo:(MConnectionInfo*)info;

- (IBAction)saveScript:(id)sender;

- (IBAction)copyMessage:(id)sender;
- (IBAction)clearMessages:(id)sender;

- (MQScriptExecutionHelper*)scriptHelper;
- (void)setDefaultSchema:(NSString*)schema;

- (void)setDocumentEdited:(BOOL)flag;
- (BOOL)isDocumentEdited;
- (NSString*)documentName;
- (NSString*)defaultSchema;

- (MQScriptEditor*)scriptEditor;
- (void)setStatusText:(NSString*)text icon:(NSImage*)icon;

- (void)setFilename:(NSString*)name;

- (void)setScript:(NSString*)script;
- (NSString*)script;
@end
