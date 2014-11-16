//
//  MQTableView.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 4/29/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MQTableView : NSTableView 
{
  int theColumn;

  id _handlerTarget;
  NSDictionary *_handlers;
  
  BOOL _highlight;
}

- (void)setDragHandlers:(NSDictionary*)handlers
                 target:(id)target;

- (int)lastClickedColumn;
- (void)selectCellAtRow:(unsigned int)row
                 column:(unsigned int)column;

@end
