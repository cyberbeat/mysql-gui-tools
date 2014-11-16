//
//  MTreeDataSource.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Tue Jul 27 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "MTreeDataSource.h"



@implementation MTreeItem

+ (MTreeItem*)itemWithTag:(int)tag repr:(NSString*)str
{
  return [[MTreeItem alloc] initWithTag:tag repr:str];
}

- (id)initWithTag:(int)tag repr:(NSString*)str
{
  self= [super init];
  if (self)
  {
    _tag= tag;
    _repr= [str retain];
  }
  return self;
}

- (int)tag
{
  return _tag;
}

- (void)dealloc
{
  [_repr release];
  [_children release];
  [super dealloc];
}

- (MTreeItem*)parent
{
  return _parent;
}

- (void)addChild:(MTreeItem*)item
{
  if (!_children)
    _children= [[NSMutableArray alloc] init];
  
  item->_parent= self;
  [_children addObject:item];
}

- (void)removeChild:(MTreeItem*)item
{
  [_children removeObject:item];
}

- (NSArray*)children
{
  return _children;
}

- (NSString*)repr
{
  return _repr;
}

- (void)setValue:(id)value forIdentifier:(NSString*)identifier
{
  NSLog(@"MTreeItem setValue:forIdentifier: Not implemented!");
}

- (id)valueForIdentifier:(NSString*)identifier
{
  return [self repr];
}

@end


@implementation MSelectableTreeItem
{
  @protected
  int _selected;
}

- (void)setSelected:(BOOL)flag
{
  unsigned int i, c;
  _selected= flag ? NSOnState : NSOffState;
  if (_children)
  {
    c= [_children count];
    for (i= 0; i < c; i++)
    {
      [[_children objectAtIndex:i] setSelected:flag];
    }
  }
}

- (int)selected
{  
  if (_children)
  {
    unsigned int i, c= [_children count];
    int s= 0, ns= 0;
    for (i= 0; i < c; i++)
    {
      if ([[_children objectAtIndex:i] selected]==1)
        s++;
      else
        ns++;
    }
    if (s == 0)
      return NSOffState;
    else if (ns == 0)
      return NSOnState;
    else
      return NSMixedState;
  }
  return _selected;
}
@end



@implementation MTreeDataSource

- (void)setRoot:(MTreeItem*)root
{
  if (_root != root)
  {
    [_root release];
    _root= [root retain];
  }
}

- (MTreeItem*)root
{
  return _root;
}

- (void)dealloc
{
  [_root release];
  [super dealloc];
}

- (id)outlineView:(NSOutlineView *)outlineView 
            child:(int)index 
           ofItem:(id)item
{
  if (item == nil)
    return [[_root children] objectAtIndex:index];
  else
    return [[item children] objectAtIndex:index];
}

- (BOOL)outlineView:(NSOutlineView *)outlineView 
   isItemExpandable:(id)item
{
  if (item == nil)
    return _root ? YES : NO;
  else
    return [item children] ? YES : NO;
}

- (int)outlineView:(NSOutlineView *)outlineView 
numberOfChildrenOfItem:(id)item
{
  if (item == nil)
    return [[_root children] count];
  else
    return [[item children] count];
}

- (void)outlineView:(NSOutlineView *)outlineView 
     setObjectValue:(id)object 
     forTableColumn:(NSTableColumn *)tableColumn 
             byItem:(id)item
{
  [item setValue:object forIdentifier:[tableColumn identifier]];
}

- (id)outlineView:(NSOutlineView *)outlineView 
objectValueForTableColumn:(NSTableColumn *)tableColumn 
           byItem:(id)item
{
  return [item valueForIdentifier:[tableColumn identifier]];
}

@end
