//
//  MTableView.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 12/15/04.
//  Copyright 2004 MySQL AB. All rights reserved.
//

#import "MTableView.h"


@implementation MTableView

- (void)keyDown:(NSEvent *)event 
{ 
  unichar key = [[event charactersIgnoringModifiers] characterAtIndex:0]; 
//  unsigned int flags = [event modifierFlags];

  if (key == NSDeleteCharacter && [self numberOfRows] > 0 && 
      [self selectedRow] != -1) 
  {
    [[self delegate] tableViewDeleteRow:self];
  }
  else
    [super keyDown:event];
}

//XXX implement tooltips, compatible with tiger's
- (void)setTooltipsEnabled:(BOOL)flag
{
  _tooltips= flag;
}

@end
