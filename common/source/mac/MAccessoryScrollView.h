//
//  MAccessoryScrollView.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 9/3/04.
//  Copyright 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MAccessoryScrollView : NSScrollView 
{
  NSMutableArray *_vAccessoryViews;  
  NSMutableArray *_hAccessoryViews;
}

- (void)addVerticalAccessoryView:(NSView*)view;
- (void)addHorizontalAccessoryView:(NSView*)view;
- (void)removeAccessoryViews;

@end
