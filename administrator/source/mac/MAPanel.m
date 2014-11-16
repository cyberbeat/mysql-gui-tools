//
//  MAPanel.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Thu Jun 24 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MAPanel.h"


@implementation MAPanel

+ (NSImage*)icon
{
  return nil;
}

+ (NSString*)label
{
  return @"";
}

+ (NSString*)toolTip
{
  return @"";
}

+ (BOOL)needsConnection
{
  return YES;
}

- (id)initWithNibFile: (NSString*)file
           panelOwner: (id<MAdministratorProtocol>)owner
{
  self= [super init];
  if (!self)
    return nil;
  
  _owner= owner;
  
  if (![NSBundle loadNibNamed:file owner:self])
  {
    NSLog(@"Could not load nib file %@", file);
    [self release];
    return nil;
  }
  
  return self;
}

- (id)initWithOwner:(id<MAdministratorProtocol>)owner
{
  // must be overriden
  return nil;
}

- (NSView*)topView
{
  return topBox;
}

- (NSView*)sideView
{
  return nil;
}

- (BOOL)willShow
{
  return YES;
}

- (void)didShow
{
}

- (BOOL)willHide
{
  return YES;
}

- (void)didHide
{
}

- (BOOL)willClose
{
  return YES;
}

- (NSRect)defaultFrame
{
  return _defaultFrame;
}

- (void)setNeedsSave:(BOOL)flag
{
  _needsSave= flag;
  [[topBox window] setDocumentEdited:flag];
}

- (BOOL)needsSave
{
  return _needsSave;
}


- (id)initWithMessage:(NSString*)message owner:(id)owner
{
  NSTextField *text;

  self= [super init];
  _owner= owner;
  topBox= [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 500, 100)];
  text= [[NSTextField alloc] initWithFrame:NSMakeRect(0, 40, 500, 30)];
  _defaultFrame= [topBox frame];
  [text setBordered:NO];
  [text setDrawsBackground:NO];
  [text setEditable:NO];
  [text setAlignment:NSCenterTextAlignment];
  [text setTextColor:[NSColor grayColor]];
  [text setFont:[NSFont systemFontOfSize:18]];
  [text setStringValue:message];
  [topBox addSubview:text];
  [text setAutoresizingMask:NSViewWidthSizable|NSViewMinYMargin|NSViewMaxYMargin];

  return self;
}


- (void)showMessageSheet:(NSString*)message
                    info:(NSString*)info
{
  NSAlert *alert = [[[NSAlert alloc] init] autorelease];
  
  [alert addButtonWithTitle:@"OK"];
  [alert setMessageText:message];
  if (info)
    [alert setInformativeText:info];
  
  [alert setAlertStyle:NSWarningAlertStyle];
  
  [alert beginSheetModalForWindow:[topBox window] modalDelegate:self 
                   didEndSelector:nil contextInfo:nil];
}

@end
