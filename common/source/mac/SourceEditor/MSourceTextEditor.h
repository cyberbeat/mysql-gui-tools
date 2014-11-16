//
//  MSourceTextEditor.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/19/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLToolsCommon/MSyntaxColoring.h>
#import <MySQLToolsCommon/MSourceTextView.h>

@class MTextGutterView;

@interface MSourceTextEditor : NSView 
{
  NSScrollView *_gutterScroll;
  MTextGutterView *_gutterView;
 
  NSScrollView *_textScroll;
  MSourceTextView *_textView;
  
  float _gutterWidth;
  BOOL _showGutter;
  
  id _delegate;
  
  NSUserDefaults *_defaults;
  NSDictionary *_defaultKeyMapping;
}

- (void)setShowGutter:(BOOL)flag;

- (void)setDelegate:(id)deleg;

- (NSScrollView*)textScrollView;
- (MSourceTextView*)textView;
- (MTextGutterView*)gutterView;

@end
