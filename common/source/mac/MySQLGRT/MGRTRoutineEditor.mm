//
//  MGRTRoutineEditor.m
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/9/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MGRTRoutineEditor.h"
#import "MGRT.h"
#include "MGRTValue.h"
#import <MySQLToolsCommon/mxUtils.h>

@implementation MGRTRoutineEditor

- (id)init
{
  self= [super init];
  if (self)
  {
    NSNib *nib= [[NSNib alloc] initWithNibNamed:@"GRTRoutineEditor"
                                         bundle:[NSBundle bundleForClass:[self class]]];
        
    if (![nib instantiateNibWithOwner:self topLevelObjects:nil])
      NSLog(@"Error instantiating nib");
    [nib release];    
  }
  return self;
}


- (void)dealloc
{
  delete _routine;
  [_grt release];
  [super dealloc];
}


- (void)awakeFromNib
{
  [[editor textScrollView] setHasHorizontalScroller:NO];
  [editor setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
}


- (NSView*)baseView
{
  return baseView;
}


- (void)showObject
{
  [nameText setStringValue:NSStr(_routine->get("name", "new_routine"))];
  [[editor textView] setString:NSStr(_routine->get("routineCode", ""))];
}


- (void)setMGRT:(MGRT*)grt
{
  [_grt release];
  _grt= [grt retain];
}


- (void)saveChanges
{
  NSString *sql= [[editor textView] string];
  NSString *name, *type;

  type= [_grt performModule:@"DbUtils"
             stringFunction:@"getRoutineType"
                  arguments:[NSArray arrayWithObject:sql]];

  if (type)
  {
    _routine->set("routineType", [type UTF8String]);
  }

  name= [_grt performModule:@"DbUtils"
             stringFunction:@"getRoutineName"
                  arguments:[NSArray arrayWithObject:sql]];

  if (name)
  {
    _routine->set("name", [name UTF8String]);
  }

  [nameText setStringValue:[NSString stringWithFormat:@"%@ %@", type?:@"routine", name?:@"?"]];

  _routine->set("routineCode", [sql UTF8String]);
}


- (void)setRoutine:(MGRTValue*)routine
{
  _routine= new MGRTValue(*routine);
  
  [self showObject];
}


- (MGRTValue*)routine
{
  return _routine;
}


- (void)setDelegate:(id)delegate
{
  _delegate= delegate;
}


- (IBAction)deleteRoutine:(id)sender
{
  [_delegate performSelector:@selector(routineEditorDelete:) withObject:self];
}


- (IBAction)toggleView:(id)sender
{
  NSRect frame= [baseView frame];

  if ([sender state] == NSOffState)
  {
    [editor setHidden:YES];
    frame.size.height= 15;
    _lastFrame= [editor frame];
    [baseView setFrame:frame];
  }
  else
  {
    [editor setHidden:NO];
    frame.size.height= 15 + NSHeight(_lastFrame);
    [baseView setFrame:frame];
    _lastFrame.size.width= NSWidth(frame);
    [editor setFrame:_lastFrame];
  }
}

@end
