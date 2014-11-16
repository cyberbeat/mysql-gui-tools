//
//  MQResultSetTab.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 3/13/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#import <MySQLToolsCommon/MTabView.h>

@class MQResultSetView;
@class MSplitView;

@interface MQResultSetTab : MTabViewItem 
{
  MSplitView *mainSplit;
  NSMutableArray *_rslist;
  MSplitView *splitView;
  NSTextField *statusText;
  NSImageView *statusImage;

  NSView *topViewBox;
  NSView *topView;
}

- (void)setTopView:(NSView*)view;

- (void)addResultSet:(MQResultSetView*)rsview;
- (void)closeResultSet:(MQResultSetView*)rsview;

- (void)setActiveResultSet:(MQResultSetView*)rsview;
- (MQResultSetView*)activeResultSet;

- (BOOL)hasUnsavedChanges;

- (void)exportResultSet:(const char*)format;

- (NSArray*)resultSetsWithMaster:(MQResultSetView*)rsview;

- (void)setDefaultSchema:(NSString*)schema;
- (void)setStatusText:(NSString*)text icon:(NSImage*)icon;

- (void)setVertical:(BOOL)flag;
- (BOOL)vertical;

- (void)compareResultsets:(id)sender;

- (void)setEmbeddedQueryArea:(BOOL)flag;
@end
