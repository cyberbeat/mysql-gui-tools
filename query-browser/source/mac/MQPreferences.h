//
//  MQPreferences.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/14.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class MQHistory;

@interface MQPreferences : NSResponder
{
  IBOutlet id view;
  IBOutlet NSTextField *fontText;
  
  MQHistory *_history;
}

- (IBAction)modifyFont:(id)sender;
- (IBAction)resetHistory:(id)sender;

- (void)setHistory:(MQHistory*)history;

@end
