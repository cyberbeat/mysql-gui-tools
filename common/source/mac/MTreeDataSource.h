//
//  MTreeDataSource.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Tue Jul 27 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface MTreeItem : NSObject
{
@protected
  NSString *_repr;
  MTreeItem *_parent;
  NSMutableArray *_children;
  int _tag;
}

+ (MTreeItem*)itemWithTag:(int)tag repr:(NSString*)str;
- (id)initWithTag:(int)tag repr:(NSString*)str;
- (int)tag;
- (void)addChild:(MTreeItem*)item;
- (void)removeChild:(MTreeItem*)item;
- (NSArray*)children;
- (MTreeItem*)parent;
- (NSString*)repr;
- (void)setValue:(id)value forIdentifier:(NSString*)identifier;
- (id)valueForIdentifier:(NSString*)identifier;
@end


@interface MSelectableTreeItem : MTreeItem
{
  @protected
  int _selected;
}
- (int)selected;
- (void)setSelected:(BOOL)flag;
@end


@interface MTreeDataSource : NSObject 
{
  @protected
  MTreeItem *_root;
}

- (void)setRoot:(MTreeItem*)root;
- (MTreeItem*)root;

- (id)outlineView:(NSOutlineView *)outlineView 
            child:(int)index 
           ofItem:(id)item;

- (BOOL)outlineView:(NSOutlineView *)outlineView 
   isItemExpandable:(id)item;

- (int)outlineView:(NSOutlineView *)outlineView 
numberOfChildrenOfItem:(id)item;


// default implementation will call [item valueForIdentifier:] and setValue:forIdentifier:
// override to change behaviour
- (void)outlineView:(NSOutlineView *)outlineView 
     setObjectValue:(id)object 
     forTableColumn:(NSTableColumn *)tableColumn 
             byItem:(id)item;

- (id)outlineView:(NSOutlineView *)outlineView 
objectValueForTableColumn:(NSTableColumn *)tableColumn 
           byItem:(id)item;

@end
