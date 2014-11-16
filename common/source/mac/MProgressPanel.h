//
//  MProgressPanel.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 12/15/04.
//  Copyright 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MProgressPanel : NSWindowController {
  IBOutlet NSTextField *messageText;
  IBOutlet NSProgressIndicator *progress;
  
  id _target;
  SEL _stopAction;
}

- (IBAction)stop:(id)sender;

- (void)setStopAction:(SEL)sender;
- (void)setTarget:(id)target;
- (void)centerOverWindow:(NSWindow*)window;
- (id)initAndLoad;
- (void)showWithTitle:(NSString*)text;
@end
