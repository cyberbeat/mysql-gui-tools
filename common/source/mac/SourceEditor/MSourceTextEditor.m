//
//  MSourceTextEditor.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/19/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MSourceTextEditor.h"
#import "MTextGutterView.h"
#import "MSourceTextView.h"
#include "mxUtils.h"

@implementation MSourceTextEditor


- (void)applyDefaults
{
  [_textView setBackgroundColor:[_defaults objectForKey:@"BackgroundColor"]?:[NSColor whiteColor]];
  [_textView setFont:[_defaults objectForKey:@"TextFont"]?:[NSFont fontWithName:@"Monaco" size:11]];
}

- (MSourceTextView*)createTextViewWithFrame:(NSRect)frame
{
  return [[MSourceTextView alloc] initWithFrame:frame];
}

- (id)initWithFrame:(NSRect)frame
{
  self= [super initWithFrame:frame];
  if (self)
  { 
    _defaults= [[NSUserDefaults standardUserDefaults] retain];

    _textScroll= [[NSScrollView alloc] initWithFrame:NSMakeRect(0, 0, NSWidth(frame), NSHeight(frame))];
    [_textScroll setHasVerticalScroller:YES];
    [_textScroll setHasHorizontalScroller:YES];
    [_textScroll setBorderType:NSBezelBorder];
    [_textScroll setAutoresizingMask:NSViewHeightSizable|NSViewWidthSizable];
    [[NSNotificationCenter defaultCenter] addObserver:self
											 selector:@selector(viewBoundsDidChange:)
												 name:NSViewBoundsDidChangeNotification
											   object:[_textScroll contentView]];

    _textView= [self createTextViewWithFrame:MXRectWithSize([_textScroll contentSize])];
    [_textView setEditable:YES];
    [_textView setMinSize:NSMakeSize(0.0, [_textScroll contentSize].height)];
    [_textView setMaxSize:NSMakeSize(FLT_MAX, FLT_MAX)];
    [_textView setVerticallyResizable:YES];
    [_textView setHorizontallyResizable:YES];
    [_textView setAutoresizingMask:NSViewHeightSizable|NSViewWidthSizable];
    [[_textView textContainer] setContainerSize:NSMakeSize(FLT_MAX, FLT_MAX)];
    [[_textView textContainer] setWidthTracksTextView:NO];
    [_textScroll setDocumentView:_textView];

    [self addSubview:_textScroll];
    
    [self applyDefaults];
    _gutterWidth= 30.0;    
  }
  return self;
}

- (void)dealloc
{
  [_defaults release];
  [_defaultKeyMapping release];
  [super dealloc];
}

- (void)setDelegate:(id)deleg
{
  _delegate= deleg;
}


- (void)resizeViews
{
  NSRect rect= [self frame];
  
  if (_gutterScroll)
  {
    NSRect grect, trect;

    rect.origin= NSMakePoint(0, 0);
    
    NSDivideRect(rect, &grect, &trect, _gutterWidth, NSMinXEdge);
    grect.size.width+= 1.0;
    [_gutterScroll setFrame:grect];
    [_textScroll setFrame:trect];
  }
  else
    [_textScroll setFrame:rect];
}

- (MSourceTextView*)textView
{
  return _textView;
}

- (void)setShowGutter:(BOOL)flag
{
  if (_showGutter != flag)
  {
    if (!flag)
    {
      [_gutterScroll removeFromSuperview];
      [_gutterScroll release];
      [_gutterView release];
      _gutterScroll= nil;
      _gutterView= nil;
    }
    else
    {
      _gutterScroll= [[NSScrollView alloc] initWithFrame:NSMakeRect(0,0,40,200)];
      _gutterView= [[MTextGutterView alloc] initWithFrame:NSMakeRect(0,0,40,200)];
      [_gutterScroll setDocumentView:_gutterView];
      [_gutterScroll setAutoresizingMask:NSViewHeightSizable];
      [_gutterScroll setAutoresizesSubviews:YES];
      [_gutterScroll setBorderType:NSBezelBorder];
      
      [_textView setGutterView:_gutterView];
      
      [self addSubview:_gutterScroll];
    }

    _showGutter= flag;

    [self resizeViews];
  }
}

- (MTextGutterView*)gutterView
{
  return _gutterView;
}


- (NSScrollView*)textScrollView
{
  return _textScroll;
}

- (void)viewBoundsDidChange:(NSNotification*)notif
{
  [_gutterView setFrameSize:NSMakeSize(_gutterWidth, NSHeight([_textView frame])+[NSScroller scrollerWidth])];
  [_gutterView scrollPoint:NSMakePoint(0, [[_textScroll contentView] bounds].origin.y)];
}

@end
