//
//  MQTextViewer.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 4/28/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQTextViewer.h"


@implementation MQTextViewer

- (id)initWithData:(NSData*)data
{
  self= [super init];
  if (self)
  {
    _sview= [[NSScrollView alloc] initWithFrame:NSMakeRect(0,0,200,200)];
    [_sview setHasHorizontalScroller:YES];
    [_sview setHasVerticalScroller:YES];
    [_sview setHasHorizontalRuler:YES];
    [_sview setHasVerticalRuler:YES];
    [_sview setBorderType: NSGrooveBorder];
    [_sview setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [[_sview contentView] setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [[_sview contentView] setAutoresizesSubviews:YES];
    
    _text= [[NSTextView alloc] initWithFrame:[[_sview contentView] frame]];
    [_text setHorizontallyResizable: YES];
    [_text setVerticallyResizable: YES];
    [_text setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [_text setAllowsUndo:YES];
    [_sview setDocumentView:_text];
    
    [_text setString:[[[NSString alloc] initWithData:data 
                                            encoding:NSUTF8StringEncoding] autorelease]];

    [_text setDelegate:self];
      
  }
  return self;
}


- (void)textDidChange:(NSNotification*)notif
{
  _edited= YES;
}


- (void)setData:(NSData*)data
{
  _edited= NO;
  [_text setString:[[[NSString alloc] initWithData:data 
                                          encoding:NSUTF8StringEncoding] autorelease]];
}


- (NSData*)editedData
{
  if (_edited)
    return [[_text string] dataUsingEncoding:NSUTF8StringEncoding];
  
  return nil;
}


- (NSString*)label
{
  return @"Text";
}


- (NSView*)view
{
  return _sview;
}


- (void)setEditable:(BOOL)flag
{
  [_text setEditable:flag];
}


@end
