//
//  MProgressPanel.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 12/15/04.
//  Copyright 2004 MySQL AB. All rights reserved.
//

#import "MProgressPanel.h"


@implementation MProgressPanel


- (IBAction)stop:(id)sender
{
  if (_stopAction && _target)
	[_target performSelector:_stopAction withObject:self];
}


- (void)setStopAction:(SEL)sender
{
  _stopAction= sender;
  
  if (_stopAction && _target)
	[[[[self window] contentView] viewWithTag:3] setEnabled:YES];
}


- (void)setTarget:(id)target
{
  _target= target;
  
  if (_stopAction && _target)
	[[[[self window] contentView] viewWithTag:3] setEnabled:YES];
}


- (void)centerOverWindow:(NSWindow*)window
{
  NSRect rect= [window frame];
  NSRect myrect;
  
  myrect= [[self window] frame];
    
  myrect.origin.x= rect.origin.x + ceil((NSWidth(rect)-NSWidth(myrect))/2);
  myrect.origin.y= rect.origin.y + ceil((NSHeight(rect)-NSHeight(myrect))/2);
  
  [[self window] setFrame:myrect display:YES];
}


- (id)initAndLoad
{
  self= [super initWithWindowNibName:@"ProgressPanel" owner:self];
  
  [self loadWindow];
  
  [[[[self window] contentView] viewWithTag:3] setEnabled:NO];
  
  return self;
}

- (void)showWithTitle:(NSString*)text
{
  [messageText setStringValue:text];
  [progress startAnimation:self];
  [self showWindow:self];
}

@end
