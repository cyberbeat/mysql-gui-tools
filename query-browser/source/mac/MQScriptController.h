//
//  MQScriptController.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/31/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class MQueryBrowser;
@class MQScriptTab;
@class MMenuButton;

@interface MQScriptController : NSObject 
{
  IBOutlet MQueryBrowser *owner;
  
  IBOutlet NSImageView *animbox;
  
  IBOutlet NSPopUpButton *objectsPop;
  IBOutlet MMenuButton *executeButton;
  IBOutlet NSButton *saveButton;
  IBOutlet NSButton *openButton;
  IBOutlet NSButton *stopButton;
  IBOutlet NSButton *undoButton;
  IBOutlet NSButton *redoButton;
  IBOutlet NSButton *pauseButton;
  IBOutlet NSButton *continueButton;
  IBOutlet NSButton *stepButton;

  IBOutlet NSView *bigToolbar;
  IBOutlet NSView *toolbar;

  NSImage *_idleImage;
  NSImage *_busyImage;
}

- (IBAction)executeScript:(id)sender;
- (IBAction)stopScript:(id)sender;
- (IBAction)pauseScript:(id)sender;
- (IBAction)continueScript:(id)sender;
- (IBAction)stepNextScript:(id)sender;

- (IBAction)saveScript:(id)sender;
- (IBAction)openScript:(id)sender;
- (IBAction)openScriptInCurrentEditor:(id)sender;

- (void)willShowPage:(id)page;
- (void)didHidePage;

- (MQScriptTab*)createPage;
- (void)switchToolbar:(BOOL)small;
- (void)resizeToolbar:(float)height;

- (void)setupEditorWithScript:(NSString*)script;

- (BOOL)stateOfMenuItem:(NSMenuItem*)item atIndex:(int)index inMenu:(NSMenu*)menu;

@end
