//
//  MMenuButton.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/16/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MMenuButton.h"

@implementation MMenuButton

- (void)displayMenu:(NSTimer *)theTimer
{
  NSEvent *theEvent=[timer userInfo];
  NSEvent *click = [NSEvent mouseEventWithType:[theEvent type]
                                      location:[self convertPoint:NSMakePoint(0, NSMaxY([self frame]))
                                                           toView:nil]
                                 modifierFlags:[theEvent modifierFlags] 
                                     timestamp:[theEvent timestamp] 
                                  windowNumber:[theEvent windowNumber] 
                                       context:[theEvent context] 
                                   eventNumber:[theEvent eventNumber] 
                                    clickCount:[theEvent clickCount] 
                                      pressure:[theEvent pressure]]; 
  [NSMenu popUpContextMenu:[self menu] withEvent:click forView:self];
  [timer invalidate];
  timer= nil;
  [self highlight:NO];
}   


/*- (void)drawRect:(NSRect)aRect
{
  [super drawRect:aRect];
}
*/

- (void)mouseDown:(NSEvent *)theEvent 
{ 
  if ([self isEnabled])
  {
    [self highlight:YES]; 
    timer=[NSTimer timerWithTimeInterval:0.3 target:self selector:@selector(displayMenu:) userInfo:theEvent repeats:NO];
    [[NSRunLoop currentRunLoop] addTimer:timer forMode:NSDefaultRunLoopMode];    
  }
}

- (void)mouseUp:(NSEvent *)theEvent 
{ 
  if ([self isEnabled])
  {
    [self highlight:NO];
    [timer invalidate];
    timer= nil;
    [self sendAction:[self action] to:[self target]];
  }
}

@end
