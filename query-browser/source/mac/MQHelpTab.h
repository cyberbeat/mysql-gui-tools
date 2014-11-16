//
//  MQHelpTab.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/5.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>
#import <MySQLToolsCommon/MTabView.h>

@interface MQHelpTab : MTabViewItem
{
  IBOutlet WebView *wview;
  NSString *_currentFile;
}


- (void)loadFile:(NSString*)file topic:(NSString*)topic;
- (void)showQuickStartHelp;

@end
