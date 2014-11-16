//
//  MSchemaDataSource.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Sun Jun 27 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>
#include "myx_public_interface.h"


extern NSString *MSchemaItemPboardType;

typedef enum {
  MCatalogItemType,
  MSchemaItemType,
  MViewItemType,
  MTableItemType,
  MColumnItemType,
  MSPItemType,
  MObjectItemType // includes table, views, columns, sps
} MSchemaDSItemType;

@class NSImage;

@interface MSchemaItem : NSObject {
@public
  MSchemaDSItemType type;
  NSImage *icon;
  MSchemaItem *parent;
@protected
  union {
    MYX_CATALOG *catalog;
    MYX_SCHEMA *schema;
    MYX_SCHEMA_TABLE *table;
    MYX_SCHEMA_TABLE_COLUMN *column;
    MYX_SCHEMA_STORED_PROCEDURE *sp;
  } _object;
  NSString *_repr;
  id _identifier;
}

- (id)initWithCatalog: (MYX_CATALOG*)catalog
                 icon: (NSImage*)icon;
- (id)initWithSchema: (MYX_SCHEMA*)schema
	   parentCatalog: (MSchemaItem*)parent
                icon: (NSImage*)icon;
- (id)initWithTable: (MYX_SCHEMA_TABLE*)table
	   parentSchema: (MSchemaItem*)parent
               icon: (NSImage*)icon;
- (id)initWithColumn: (MYX_SCHEMA_TABLE_COLUMN*)column
		 parentTable: (MSchemaItem*)parent
                icon: (NSImage*)icon;
- (id)initWithSP: (MYX_SCHEMA_STORED_PROCEDURE*)column
    parentSchema: (MSchemaItem*)parent
            icon: (NSImage*)icon;

- (void)dealloc;

- (id)identifier;

- (NSString*)repr;
- (NSImage*)icon;
- (MSchemaDSItemType)type;

- (MYX_CATALOG*)catalog;
- (MYX_SCHEMA*)schema;
- (MYX_SCHEMA_TABLE*)table;
- (MYX_SCHEMA_TABLE_COLUMN*)column;
- (MYX_SCHEMA_STORED_PROCEDURE*)sp;

- (void)resetCatalog:(MYX_CATALOG*)catalog;
- (void)resetSchema:(MYX_SCHEMA*)schema;
- (void)resetTable:(MYX_SCHEMA_TABLE*)table;
- (void)resetColumn:(MYX_SCHEMA_TABLE_COLUMN*)column;
- (void)resetSP:(MYX_SCHEMA_STORED_PROCEDURE*)sp;

@end


@interface MSchemaDataSource : NSObject {
  NSImage *_catalogIcon;
  NSImage *_schemaIcon;
  NSImage *_tableIcon;
  NSImage *_viewIcon;
  NSImage *_spIcon;
  NSImage *_columnIcon;
  NSImage *_keyIcon;
  
  MSchemaItem *_draggedItem;

  MYX_CATALOGS *_catalogs;
@public
  MSchemaDSItemType _rootType;
  MSchemaDSItemType _leafType;

  id _tableFetcher;
  SEL _tableFetcherAction;

  NSMutableDictionary *_children; // maps identifier -> NSArray of identifiers
  NSMutableDictionary *_items; // maps identifier -> MSchemaItem
}

- (id)initWithRoot: (MSchemaDSItemType)root
              leaf: (MSchemaDSItemType)leaf;
- (id)initWithRoot: (MSchemaDSItemType)root
              leaf: (MSchemaDSItemType)leaf
			 icons: (NSDictionary*)icons;
- (void)dealloc;

- (void)resetTree;

- (void)setTableFetcher: (id)target selector:(SEL)sel;

- (MSchemaItem*)findItem: (NSString*)name;
- (MSchemaItem*)findChild: (NSString*)name
				   ofItem: (MSchemaItem*)item;


- (id)outlineView:(NSOutlineView *)outlineView 
            child:(int)index 
           ofItem:(id)item;

- (BOOL)outlineView:(NSOutlineView *)outlineView 
   isItemExpandable:(id)item;

- (int)outlineView:(NSOutlineView *)outlineView 
numberOfChildrenOfItem:(id)item;

- (id)outlineView:(NSOutlineView *)outlineView 
objectValueForTableColumn:(NSTableColumn *)tableColumn 
           byItem:(id)item;

- (void)updateWithCatalogs:(MYX_CATALOGS*)catalog;
- (void)resetSchemaChildren:(MSchemaItem*)schema;
- (void)updateSchema:(MSchemaItem*)schema
		  withTables:(MYX_SCHEMA_TABLES*)tables;
- (void)updateSchema:(MSchemaItem*)schema
             withSPs:(MYX_SCHEMA_STORED_PROCEDURES*)sps;
- (MYX_CATALOGS*)catalogs;

- (MSchemaItem*)draggedItem;
@end



@interface MFilteredSchemaDataSource : MSchemaDataSource {
  NSString *_filter;
  MSchemaDataSource *_parentDS;
}

- (id)initWithDataSource:(MSchemaDataSource*)parent;
- (void)performSearch:(NSString*)filter;


@end






