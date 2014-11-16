//
//  MOutlineView.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 12/15/04.
//  Copyright 2004 MySQL AB. All rights reserved.
//

#import "MOutlineView.h"


@implementation MOutlineView

- (void)keyDown:(NSEvent *)event 
{ 
  unichar key = [[event charactersIgnoringModifiers] characterAtIndex:0]; 
  unsigned int flags = [event modifierFlags]; 

  if (key == NSDeleteCharacter && (flags & ~0xffff) == 0 && [self numberOfRows] > 0 && 
      [self selectedRow] != -1) 
  { 
    [self sendAction:_deleteAction to:[self target]];
  }
  else
    [super keyDown:event];
} 


- (void)setDeleteAction:(SEL)action
{
  _deleteAction= action;
}

@end
