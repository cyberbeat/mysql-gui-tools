//
//  MGRTRoutineEditor.h
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/9/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLToolsCommon/MSourceTextEditor.h>

@class MGRT;
class MGRTValue;

@interface MGRTRoutineEditor : NSObject 
{
  IBOutlet NSView *baseView;
  IBOutlet NSTextField *nameText;
  IBOutlet MSourceTextEditor *editor;

  id _delegate;

  MGRT *_grt;
  NSRect _lastFrame;
  
  MGRTValue *_routine;
}


- (IBAction)deleteRoutine:(id)sender;
- (IBAction)toggleView:(id)sender;
- (NSView*)baseView;

- (void)showObject;
- (void)saveChanges;

- (void)setMGRT:(MGRT*)grt;
- (void)setRoutine:(MGRTValue*)routine;
- (void)setDelegate:(id)delegate;

- (MGRTValue*)routine;

@end
