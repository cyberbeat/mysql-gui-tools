//
//  MSourceTextView.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/19/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLToolsCommon/MSyntaxColoring.h>
#import <MySQLToolsCommon/MTextGutterView.h>

@interface MSourceTextView : NSTextView
{
  MTextGutterView *_gutterView;
  MSyntaxColoring *_syntaxColor;
  
  NSImage *_bpGutter;
  NSImage *_pcGutter;
  NSImage *_stGutter;
  
  BOOL _breakpointsEnabled;
}

- (void)setSyntaxColorer:(MSyntaxColoring*)colorer;
- (MSyntaxColoring*)syntaxColorer;

//- (void)setHasGutterView:(BOOL)flag;
- (void)setGutterView:(MTextGutterView*)gutter;

- (void)enableBreakpoints:(BOOL)flag;
@end
