//
//  MTableView.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 12/15/04.
//  Copyright 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MTableView : NSTableView 
{
  BOOL _tooltips;
}

- (void)keyDown:(NSEvent *)event;

@end
