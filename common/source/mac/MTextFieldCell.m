//
//  MTextFieldCell.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 06/4/30.
//  Copyright 2006 MySQL AB. All rights reserved.
//

#import "MTextFieldCell.h"


@implementation MTextFieldCell

- (id)initCopyingCell:(NSTextFieldCell*)cell
{
  self= [super initTextCell:[cell stringValue]];
  
  [self setFont:[cell font]];
  [self setAlignment:[cell alignment]];
  [self setBordered:[cell isBordered]];
  [self setBezeled:[cell isBezeled]];
  [self setBackgroundColor:[cell backgroundColor]];
  [self setTextColor:[cell textColor]];
  return self;
}

- (void)drawInteriorWithFrame:(NSRect)cellFrame inView:(NSView *)controlView
{
  NSRect rect;
  rect.origin= NSMakePoint(0, 0);
  rect.size= [self cellSizeForBounds:cellFrame];
  
  if (NSHeight(rect) < NSHeight(cellFrame))
  {
    cellFrame.origin.y-= (NSHeight(rect) - NSHeight(cellFrame)) / 2;
  }
  
  [super drawInteriorWithFrame:cellFrame inView:controlView];
}

@end
