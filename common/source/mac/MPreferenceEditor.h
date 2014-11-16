//
//  MPreferenceEditor.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 1/8/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#include "myx_public_interface.h"

@class MConnectionEditor;

@interface MPreferenceEditor : NSObject {
  IBOutlet NSWindow *window;
  IBOutlet NSTabView *tabView;
  
  IBOutlet NSPopUpButton *passwordStorage;
  
  NSMutableArray *_pages;

  BOOL _inModalLoop;
  MConnectionEditor *_connectionEditor;
}

- (id)initForConnectionsFile:(NSString*)file;

- (id)registerPageNib:(NSString*)nibName withLabel:(NSString*)label;
- (id)registerMultiPageNib:(NSString*)nibName withLabels:(NSArray*)labels;

- (NSWindow*)window;

- (void)show;
- (int)runConnectionEditor;

@end
