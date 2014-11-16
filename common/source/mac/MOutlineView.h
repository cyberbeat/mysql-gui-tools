//
//  MOutlineView.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 12/15/04.
//  Copyright 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MOutlineView : NSOutlineView {
  SEL _deleteAction;
}

- (void)keyDown:(NSEvent *)event;
- (void)setDeleteAction:(SEL)action;

@end
