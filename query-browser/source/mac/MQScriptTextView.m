//
//  MQScriptTextView.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/5.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQScriptTextView.h"
#import <MySQLToolsCommon/MSchemaDataSource.h>
#import <MySQLToolsCommon/MSchemaEditHelper.h>

@implementation MQScriptTextView

- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender
{
  return NSDragOperationCopy;
}

- (void)draggingExited:(id <NSDraggingInfo>)sender
{
}

- (NSDragOperation)draggingUpdated:(id <NSDraggingInfo>)sender
{
  return NSDragOperationCopy;
}

- (BOOL)prepareForDragOperation:(id <NSDraggingInfo>)sender
{
  MSchemaDataSource *source= [[sender draggingSource] dataSource];
  if (source && [source isKindOfClass:[MSchemaDataSource class]] && [source draggedItem])
    return YES;
  return NO;
}

- (BOOL)performDragOperation:(id <NSDraggingInfo>)sender
{
  MSchemaDataSource *source= [[sender draggingSource] dataSource];
  MSchemaItem *item;
  
  if ([source isKindOfClass:[MSchemaDataSource class]])
  {
    item= [source draggedItem];
    if (item)
    {
      NSString *sql= [_schemaHelper getCreateCommandForItem:item];
      if (sql)
      {
        if ([item type] == MSPItemType)
          [self insertText:[NSString stringWithFormat:@"DELIMITER $$\n%@\n$$\n", sql]];
        else
          [self insertText:[NSString stringWithFormat:@"%@;\n", sql]];
        [[self window] makeFirstResponder:self];
      }
      return YES;
    }
  }
  return NO;
}

- (void)setHelper:(MSchemaEditHelper*)helper
{
  _schemaHelper= helper;
}

@end



@implementation MQScriptEditor

- (MSourceTextView*)createTextViewWithFrame:(NSRect)frame
{
  return [[MQScriptTextView alloc] initWithFrame:frame];
}

@end