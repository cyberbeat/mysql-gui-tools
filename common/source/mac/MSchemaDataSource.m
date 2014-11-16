//
//  MSchemaDataSource.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Sun Jun 27 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "MSchemaDataSource.h"
#import "mxUtils.h"

NSString *MSchemaItemPboardType= @"MSchemaItemPboardType";


@implementation MSchemaItem

- (id)initWithCatalog: (MYX_CATALOG*)catalog
                 icon: (NSImage*)aicon
{
  if (!(self= [super init]))
    return nil;
  
  type= MCatalogItemType;
  _object.catalog= catalog;
  _repr= nil;
  icon= [aicon retain];
  
  return self;
}

- (id)initWithSchema: (MYX_SCHEMA*)schema
       parentCatalog: (MSchemaItem*)parentItem
                icon: (NSImage*)aicon
{
  if (!(self= [super init]))
    return nil;
  
  type= MSchemaItemType;
  _object.schema= schema;
  parent= parentItem;
  _repr= nil;
  icon= [aicon retain];
  
  return self;
}


- (id)initWithSP: (MYX_SCHEMA_STORED_PROCEDURE*)sp
    parentSchema: (MSchemaItem*)parentItem
            icon: (NSImage*)aicon
{
  if (!(self= [super init]))
    return nil;
  
  type= MSPItemType;
  _object.sp= sp;
  parent= parentItem;
  _repr= nil;
  icon= [aicon retain];
  
  return self;
}


- (id)initWithTable: (MYX_SCHEMA_TABLE*)table
       parentSchema: (MSchemaItem*)parentItem
               icon: (NSImage*)aicon
{
  if (!(self= [super init]))
    return nil;
  
  if (table->table_type == MSTT_VIEW)
    type= MViewItemType;
  else
    type= MTableItemType;
  _object.table= table;
  parent= parentItem;
  _repr= nil;
  icon= [aicon retain];
  
  return self;
}

- (id)initWithColumn: (MYX_SCHEMA_TABLE_COLUMN*)column
         parentTable: (MSchemaItem*)parentItem
                icon: (NSImage*)aicon
{
  if (!(self= [super init]))
    return nil;
  
  type= MColumnItemType;
  _object.column= column;
  _repr= nil;
  parent= parentItem;
  icon= [aicon retain];
  
  return self;
}

- (void)dealloc
{
  [_identifier release];
  [_repr release];
  [icon release];
  [super dealloc];
}

- (NSImage*)icon
{
  return icon;
}

- (MSchemaDSItemType)type
{
  return type;
}

- (id)identifier
{
  if (!_identifier)
    _identifier= [[NSString stringWithFormat:@"%p",self] retain];
  return _identifier;
}


- (BOOL)isExpandableForType:(MSchemaDSItemType)aType
{ 
  if (aType == MObjectItemType && (type == MTableItemType || 
                                   type == MSchemaItemType ||
                                   type == MCatalogItemType))
    return YES;
  
  if (aType == MSchemaItemType && (type == MCatalogItemType))
    return YES;

  if (aType == MTableItemType && (type == MSchemaItemType ||
                                  type == MCatalogItemType))
    return YES;

  if (aType == MViewItemType && (type == MSchemaItemType ||
                                  type == MCatalogItemType))
    return YES;

  if (aType == MSPItemType && (type == MSchemaItemType ||
                               type == MCatalogItemType))
    return YES;
  
  if (aType == MColumnItemType && (type == MTableItemType || 
                                   type == MViewItemType ||
                                   type == MSchemaItemType ||
                                   type == MCatalogItemType))
    return YES;
  
  return NO;
}

- (NSString*)repr
{
  if (!_repr)
  {
    switch (type)
    {
      case MCatalogItemType:
        _repr= [NSString stringWithUTF8String: _object.catalog->catalog_name?:"?"];
        break;
      case MSchemaItemType:
        _repr= [NSString stringWithUTF8String: _object.schema->schema_name?:""];
        break;
      case MTableItemType:
        _repr= [NSString stringWithUTF8String: _object.table->table_name?:""];
        break;
      case MColumnItemType:
        _repr= [NSString stringWithUTF8String: _object.column->column_name?:""];
        break;
      case MViewItemType:
        _repr= [NSString stringWithUTF8String: _object.table->table_name?:""];
        break;
      case MSPItemType:
        _repr= [NSString stringWithUTF8String: _object.sp->name?:""];
        break;
      case MObjectItemType:
        NSLog(@"Invalid object type in %@", self);
        break;
    }
    [_repr retain];
  }
  return _repr;
}

- (MYX_CATALOG*)catalog
{
  MSchemaItem *item= nil;
  switch (type)
  {
    case MCatalogItemType:
      item= self;
      break;
    case MSchemaItemType:
      item= self->parent;
      break;
    case MSPItemType:
    case MViewItemType:
    case MTableItemType:
      item= self->parent->parent;
      break;
    case MColumnItemType:
      item= self->parent->parent->parent;
      break;
    case MObjectItemType:
      break;
  }
  return item ? item->_object.catalog : NULL;
}

- (MYX_SCHEMA*)schema
{
  MSchemaItem *item= nil;
  switch (type)
  {
    case MCatalogItemType:
      return NULL;
      break;
    case MSchemaItemType:
      item= self;
      break;
    case MSPItemType:  
    case MViewItemType:
    case MTableItemType:
      item= self->parent;
      break;
    case MColumnItemType:
      item= self->parent->parent;
      break;
    default:
      NSAssert(0,@"invalid type (table)");
      break;      
  }
  return item ? item->_object.schema : NULL;
}

- (MYX_SCHEMA_TABLE*)table
{
  MSchemaItem *item= nil;
  switch (type)
  {
    case MViewItemType:
    case MTableItemType:
      item= self;
      break;
    case MColumnItemType:
      item= self->parent;
      break;
    default:
      NSAssert(0,@"invalid type (table)");
      break;
  }
  return item ? item->_object.table : NULL;  
}

- (MYX_SCHEMA_TABLE_COLUMN*)column
{
  MSchemaItem *item= nil;
  switch (type)
  {
    case MColumnItemType:
      item= self;
      break;
    default:
      NSAssert(0,@"invalid type (column)");
      break;
  }
  return item ? item->_object.column : NULL;  
}


- (MYX_SCHEMA_STORED_PROCEDURE*)sp
{
  MSchemaItem *item= nil;
  switch (type)
  {
    case MSPItemType:
      item= self;
      break;
    default:
      NSAssert(0,@"invalid type (sp)");
      break;
  }
  return item ? item->_object.sp : NULL;  
}


- (void)resetCatalog:(MYX_CATALOG*)catalog
{
  NSAssert(type == MCatalogItemType, @"Invalid type");
  
  _object.catalog= catalog;
}


- (void)resetSchema:(MYX_SCHEMA*)schema
{
  NSAssert(type == MSchemaItemType, @"Invalid type");
  
  _object.schema= schema;
}


- (void)resetTable:(MYX_SCHEMA_TABLE*)table
{
  NSAssert(type == MTableItemType || type == MViewItemType, @"Invalid type");
  
  _object.table= table;
}


- (void)resetColumn:(MYX_SCHEMA_TABLE_COLUMN*)column
{
  NSAssert(type == MColumnItemType, @"Invalid type");
  
  _object.column= column;
}


- (void)resetSP:(MYX_SCHEMA_STORED_PROCEDURE*)sp
{
  NSAssert(type == MSPItemType, @"Invalid type");
  
  _object.sp= sp;
}

@end


//==============================================================================

@implementation MSchemaDataSource

- (MSchemaItem*)findItem: (NSString*)name
{
  NSArray *rootList= [_children objectForKey:@"root"];
  int i, c= [rootList count];
  for (i= 0; i < c; i++)
  {
    id item= [_items objectForKey:[rootList objectAtIndex:i]];
    if ([[item repr] isEqualToString:name])
      return item;
  }
  
  return nil;
}

- (MSchemaItem*)findChild: (NSString*)name
                   ofItem: (MSchemaItem*)item
{
  NSArray *array= [_children objectForKey:[item identifier]];
  int i, c= [array count];
  for (i= 0; i < c; i++)
  {
    id item= [_items objectForKey:[array objectAtIndex:i]];
    if ([[item repr] isEqualToString:name])
      return item;
  }
  return nil;
}


- (id)initWithRoot: (MSchemaDSItemType)root
              leaf: (MSchemaDSItemType)leaf
{
  self= [super init];
  if (self)
  {
    _rootType= root;
    _leafType= leaf;
    _children= [[NSMutableDictionary alloc] init];
    _items= [[NSMutableDictionary alloc] init];  
    
    _catalogIcon= [MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"16x16_Catalog.png") retain];
    _schemaIcon= [MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"myx_schema_16x16.png") retain];
    _tableIcon= [MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"myx_schema_table_16x16.png") retain];
    _viewIcon= [MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"myx_schema_view_16x16.png") retain];
    _spIcon= [MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"myx_schema_sp_16x16.png") retain];
    _columnIcon= [MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"16x16_Field.png") retain];
    _keyIcon= [MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"16x16_KeyColumn.png") retain];	
  }    
  return self;
}

- (id)initWithRoot: (MSchemaDSItemType)root
              leaf: (MSchemaDSItemType)leaf
             icons: (NSDictionary*)icons
{
  self= [super init];
  if (self)
  {
    _rootType= root;
    _leafType= leaf;
    _children= [[NSMutableDictionary alloc] init];
    _items= [[NSMutableDictionary alloc] init];  
    
    _catalogIcon= [[icons objectForKey:@"catalog"] retain];
    _schemaIcon= [[icons objectForKey:@"schema"] retain];
    _tableIcon= [[icons objectForKey:@"table"] retain];
    _viewIcon= [[icons objectForKey:@"view"] retain];
    _columnIcon= [[icons objectForKey:@"column"] retain];
    _spIcon= [[icons objectForKey:@"sp"] retain];
    _keyIcon= [[icons objectForKey:@"key"] retain];
  }  
  return self;
}


- (void)dealloc
{
  [_children release];
  [_items release];
  
  [_catalogIcon release];
  [_schemaIcon release];
  [_tableIcon release];
  [_viewIcon release];
  [_columnIcon release];
  [_keyIcon release];
  [_spIcon release];
  
  [super dealloc];
}


- (void)setTableFetcher:(id)target selector:(SEL)sel
{
  _tableFetcher= target;
  _tableFetcherAction= sel;
}


- (void)resetTree
{
  [_children removeAllObjects];
  [_items removeAllObjects];
}


- (void)addColumn:(MYX_SCHEMA_TABLE_COLUMN*)column
            table:(MSchemaItem*)table
          toArray:(NSMutableArray*)tabArray
{
  MSchemaItem *citem= [[MSchemaItem alloc] initWithColumn: column
                                              parentTable: table
                                                     icon: column->primary_key ? _keyIcon : _columnIcon];
  
  [_items setObject:citem forKey:[citem identifier]];
  
  [tabArray addObject: [citem identifier]];
  [citem release];  
}


- (void)addSP:(MYX_SCHEMA_STORED_PROCEDURE*)sp
       schema:(MSchemaItem*)schema
      toArray:(NSMutableArray*)spArray
{
  MSchemaItem *citem= [[MSchemaItem alloc] initWithSP: sp
                                         parentSchema: schema
                                                 icon: _spIcon];
  
  [_items setObject:citem forKey:[citem identifier]];
  
  [spArray addObject: [citem identifier]];
  [citem release];  
}


- (void)addTable:(MYX_SCHEMA_TABLE*)table
          schema:(MSchemaItem*)schema
         toArray:(NSMutableArray*)schArray
{
  NSMutableArray *tabArray= [NSMutableArray arrayWithCapacity: table->columns_num];
  MSchemaItem *titem= [[MSchemaItem alloc] initWithTable: table
                                            parentSchema: schema
                                                    icon: table->table_type==MSTT_BASE_TABLE?_tableIcon:_viewIcon];
  unsigned int j;

  for (j= 0; j < table->columns_num; j++)
  {
    [self addColumn:table->columns+j
              table:titem
            toArray:tabArray];
  }
  
  [_items setObject:titem forKey:[titem identifier]];
  [_children setObject:tabArray forKey:[titem identifier]];
  
  [schArray addObject: [titem identifier]];
  [titem release];
}


- (void)addSchema:(MYX_SCHEMA*)schema
          catalog:(MSchemaItem*)catalog
          toArray:(NSMutableArray*)schArray
{ 
  MSchemaItem *item= [[MSchemaItem alloc] initWithSchema: schema
                                           parentCatalog: catalog
                                                    icon: _schemaIcon];
  [_items setObject:item forKey:[item identifier]];
  
  [schArray addObject: [item identifier]];
  [item release];
}


- (void)addCatalog:(MYX_CATALOG*)catalog
           toArray:(NSMutableArray*)catArray
{
  NSMutableArray *schArray= [NSMutableArray arrayWithCapacity: catalog->schemata_num];
  MSchemaItem *citem= [[MSchemaItem alloc] initWithCatalog: catalog
                                                      icon: _catalogIcon];
  unsigned int j;
  
  for (j= 0; j < catalog->schemata_num; j++)
  {
    [self addSchema:catalog->schemata+j 
            catalog:citem
            toArray:schArray];
  }
  
  [_items setObject:citem forKey:[citem identifier]];
  [_children setObject:schArray forKey:[citem identifier]];
  
  [catArray addObject: [citem identifier]];
  [citem release];
}


- (MSchemaItem*)search:(NSMutableArray*)array
       forItemWithName:(const char*)name
{
  unsigned int i, c= [array count];
  
  for (i= 0; i < c; i++)
  {
    id ident= [array objectAtIndex:i];
    MSchemaItem *item= [_items objectForKey:ident];
    if (strcmp([[item repr] UTF8String], name)==0)
      return item;
  }
  return nil;
}


- (void)removeSubtree:(id)identifier
{
  NSMutableArray *children= [_children objectForKey:identifier];
  
  if (children)
  {
    unsigned int i, c= [children count];
    for (i= 0; i < c; i++)
    {
      id childid= [children objectAtIndex:i];
      [self removeSubtree:childid];
    }
  }
  [_children removeObjectForKey:identifier];
  [_items removeObjectForKey:identifier];
}


- (MYX_CATALOGS*)catalogs
{
  return _catalogs;
}

- (void)updateWithCatalogs:(MYX_CATALOGS*)catalogs
{
  int i;
  NSMutableArray *catArray;
  
  _catalogs= catalogs;
  
  catArray= [_children objectForKey:@"root"];
  if (catArray)
  {
    unsigned int sc, c= [catArray count];
    int s;
    // traverse catalogs and delete any missing items in the old list
    for (i= c-1; i>=0; i--)
    {
      unsigned int ci;
      MSchemaItem *item= [_items objectForKey: [catArray objectAtIndex:i]];
      const char *name= [[item repr] UTF8String];
      NSMutableArray *schArray= [_children objectForKey: [item identifier]];
      
      // first try catalog itself
      for (ci= 0; ci < catalogs->catalogs_num; ci++)
      {
        if (strcmp(catalogs->catalogs[ci].catalog_name, name)==0)
          break;
      }
      if (ci == catalogs->catalogs_num) // old item not found in new list
      {
        // delete everything related
        [self removeSubtree:[item identifier]];
        [catArray removeObjectAtIndex: i];
      }
      
      // then try schemas inside each catalog
      sc= [schArray count];
      for (s= sc-1; s>=0; s--)
      {
        MSchemaItem *sitem= [_items objectForKey: [schArray objectAtIndex:s]];
        unsigned int si;
        const char *sname= [[sitem repr] UTF8String];
        
        for (si= 0; si < catalogs->catalogs[ci].schemata_num; si++)
        {
          if (strcmp(catalogs->catalogs[ci].schemata[si].schema_name, sname)==0)
            break;
        }
        if (si == catalogs->catalogs[ci].schemata_num) // old item not found in new list
        {
          [self removeSubtree:[sitem identifier]];
          [schArray removeObjectAtIndex: s];
        }
      }
    }
    
    // traverse all catalogs and add anything new	
    for (i= 0; i < catalogs->catalogs_num; i++)
    {
      MSchemaItem *citem;
      
      if (!(citem= [self search:catArray
                forItemWithName:catalogs->catalogs[i].catalog_name]))
      {
        // add the catalog here
        [self addCatalog:catalogs->catalogs+i toArray:catArray];
      }
      else
      {
        NSMutableArray *schArray= [_children objectForKey:[citem identifier]];
        
        // traverse all schemas in the catalog
        for (s= 0; s < catalogs->catalogs[i].schemata_num; s++)
        {
          MSchemaItem *sitem;
          
          if (!(sitem= [self search:schArray
                    forItemWithName:catalogs->catalogs[i].schemata[s].schema_name]))
          {
            // add the schema here
            [self addSchema:catalogs->catalogs[i].schemata+s
                    catalog:citem
                    toArray:schArray];
          }
          else
          {
            MYX_SCHEMA *schema= [sitem schema];
            catalogs->catalogs[i].schemata[s].schema_tables= schema->schema_tables;
            schema->schema_tables= NULL;
            [sitem resetSchema:catalogs->catalogs[i].schemata+s];
          }
        }
      }
    }
  }
  else
  {
    catArray= [NSMutableArray arrayWithCapacity:catalogs->catalogs_num];
    
    for (i= 0; i < catalogs->catalogs_num; i++)
    {
      [self addCatalog:catalogs->catalogs+i 
               toArray:catArray];
    }
    [_children setObject:catArray forKey:@"root"];
  }
}


- (void)resetSchemaChildren:(MSchemaItem*)schema
{
  NSMutableArray *array= [_children objectForKey:[schema identifier]];
  
  if (array)
    [array removeAllObjects];
  else
  {
    array= [NSMutableArray arrayWithCapacity:0];
    [_children setObject:array forKey:[schema identifier]];
  }  
}

- (void)updateSchema:(MSchemaItem*)schema
          withTables:(MYX_SCHEMA_TABLES*)tables
{
  NSMutableArray *tabArray;
  unsigned int i;
  
  tabArray= [_children objectForKey:[schema identifier]];
  NSAssert(tabArray != nil, @"Array for schema does not exist. Did you call resetSchemaChildren:?");
  
  //XXX optimize so that it doesn't recreate the whole thing
  // whenever the list is updated (as it's done on schema/catalog level)
  
  for (i= 0; i < tables->schema_tables_num; i++)
  {
    [self addTable:tables->schema_tables+i
            schema:schema
           toArray:tabArray];
  }
}


- (void)updateSchema:(MSchemaItem*)schema
             withSPs:(MYX_SCHEMA_STORED_PROCEDURES*)sps
{
  NSMutableArray *spArray;
  unsigned int i;
  
  spArray= [_children objectForKey:[schema identifier]];
  NSAssert(spArray != nil, @"Array for schema does not exist. Did you call resetSchemaChildren:?");
  
  if (sps)
  {
    for (i= 0; i < sps->schema_sps_num; i++)
    {
      [self addSP:sps->schema_sps+i
           schema:schema
          toArray:spArray];
    }
  }
}


- (id)outlineView:(NSOutlineView *)outlineView 
            child:(int)index 
           ofItem:(id)item
{
  if (item == nil)
    return [_items objectForKey:[[_children objectForKey:@"root"] objectAtIndex: index]];
  else
  {
    id tmp= [_children objectForKey:[item identifier]];
    if (!tmp)
    {
      [_tableFetcher performSelector:_tableFetcherAction
                          withObject:self
                          withObject:item];
      return @"Loading...";
    }
    else
    {
      return [_items objectForKey:[tmp objectAtIndex:index]];
    }
  }
}

- (BOOL)outlineView:(NSOutlineView *)outlineView 
   isItemExpandable:(id)item
{
  if ([item isMemberOfClass:[MSchemaItem class]] && [item isExpandableForType:_leafType])
    return YES;
  else
    return NO;
}

- (int)outlineView:(NSOutlineView *)outlineView 
numberOfChildrenOfItem:(id)item
{
  if (item == nil)
    return [[_children objectForKey:@"root"] count];
  else if ([item isKindOfClass: [NSString class]]) // the Loading... string
    return 0;
  else
  {
    id tmp= [_children objectForKey:[item identifier]];
    if (!tmp)
      return 1; // for Loading...
    else
      return [tmp count];
  }
}

- (id)outlineView:(NSOutlineView *)outlineView 
objectValueForTableColumn:(NSTableColumn *)tableColumn 
           byItem:(id)item
{
  if ([item isMemberOfClass: [MSchemaItem class]])
    return [item repr];
  else
    return item; // NSString for Loading...
}


- (MSchemaItem*)draggedItem
{
  return _draggedItem;
}

- (BOOL)outlineView:(NSOutlineView *)outlineView
         writeItems:(NSArray *)items
       toPasteboard:(NSPasteboard *)pboard
{
  MSchemaItem *item= [items lastObject];
  
  [pboard declareTypes:[NSArray arrayWithObjects:MSchemaItemPboardType,nil] owner:nil];
  
  _draggedItem= item;
  
  [pboard setData:[NSData data] forType:MSchemaItemPboardType];
  return YES;
}

@end


//==============================================================================

@implementation MFilteredSchemaDataSource

- (id)initWithDataSource:(MSchemaDataSource*)parent
{
  self= [super initWithRoot:parent->_rootType leaf:parent->_leafType];
  if (self)
  {
    _parentDS= parent;
  }
  return self;
}


- (void)setRootList: (NSMutableArray*)array
{
  NSLog(@"NSFilteredSchemaDataSource should not have its rootList changed");
}


- (id)outlineView:(NSOutlineView *)outlineView 
            child:(int)index 
           ofItem:(id)item
{
  if (!_filter)
    return [_parentDS outlineView:outlineView child:index ofItem:item];
  else
    return [super outlineView:outlineView child:index ofItem:item];
  
}

- (BOOL)outlineView:(NSOutlineView *)outlineView 
   isItemExpandable:(id)item
{
  if (!_filter)
    return [_parentDS outlineView:outlineView isItemExpandable:item];
  else
    return [super outlineView:outlineView isItemExpandable:item];
}

- (int)outlineView:(NSOutlineView *)outlineView 
numberOfChildrenOfItem:(id)item
{
  if (!_filter)
    return [_parentDS outlineView:outlineView numberOfChildrenOfItem:item];
  else
    return [super outlineView:outlineView numberOfChildrenOfItem:item];
}

- (id)outlineView:(NSOutlineView *)outlineView 
objectValueForTableColumn:(NSTableColumn *)tableColumn 
           byItem:(id)item
{
  if (!_filter)
    return [_parentDS outlineView:outlineView
	objectValueForTableColumn:tableColumn
                           byItem:item];
  else
    return [super outlineView:outlineView
    objectValueForTableColumn:tableColumn
                       byItem:item];
}


- (BOOL)filterChildren:(NSArray*)children
                ofItem:(MSchemaItem*)item
              matching:(NSString*)filter
{
  unsigned int i, c= [children count];
  NSMutableArray *array= [NSMutableArray arrayWithCapacity:1];
  
  for (i= 0; i < c; i++)
  {
    id ident= [children objectAtIndex:i];
    id obj= [_parentDS->_items objectForKey:ident];
    NSArray *ch= [_parentDS->_children objectForKey:ident];
    BOOL ok= NO;
    
    if ([[obj repr] rangeOfString:filter].location!=NSNotFound)
      ok= YES;
    
    if (ch && [self filterChildren:ch ofItem:obj matching:filter])
      ok= YES;
    
    if (ok)
      [array addObject:ident];
  }
  
  [_children setObject:array forKey:[item identifier]];
  
  return [array count]>0;
}


- (BOOL)removeChildren:(NSMutableArray*)children
                ofItem:(MSchemaItem*)item
           notMatching:(NSString*)filter
{
  int i, c= [children count];
  int matches= 0;
  
  for (i= c-1; i >= 0; i--)
  {
    id ident= [children objectAtIndex:i];
    id obj= [_parentDS->_items objectForKey:ident];
    NSMutableArray *ch= [_parentDS->_children objectForKey:ident];
    
    if (ch && [self removeChildren:ch ofItem:obj notMatching:filter])
    {
      matches++;
    }
    else if ([[obj repr] rangeOfString:filter].location!=NSNotFound)
    {
      matches++;
    }
    else
    {
      [children removeObject:ident];
    }
  }
  
  return matches>0;
}


- (void)performSearch:(NSString*)filter
{
  int i, c;
  
  //  NSLog(@"filter by %@", filter);
  if (!filter || [filter length] == 0)
  {
    [_filter release];
    _filter= nil;
  }
  else if (!_filter || [filter rangeOfString:_filter].location == NSNotFound)
  {
    NSArray *array;
    NSMutableArray *rootArray= nil;
    
    [_filter release];
    _filter= [filter retain];
    
    [_children removeAllObjects];
    array= [_parentDS->_children objectForKey:@"root"];
    c= [array count];
    for (i= 0; i < c; i++)
    {
      if ([self filterChildren:[_parentDS->_children objectForKey:[array objectAtIndex:i]]
                        ofItem:[_parentDS->_items objectForKey:[array objectAtIndex:i]]
                      matching:filter])
      {
        if (!rootArray)
          rootArray= [NSMutableArray arrayWithCapacity:1];
        
        [rootArray addObject:[array objectAtIndex:i]];
      }
    }
    if (rootArray)
    {
      [_children setObject:rootArray forKey:@"root"];
      _items= _parentDS->_items;
    }
  }
  else // incremental search
  {
    NSMutableArray *array;
    
    [_filter release];
    _filter= [filter retain];
    
    array= [_children objectForKey:@"root"];
    c= [array count];
    for (i= c-1; i >= 0; i--)
    {
      if (![self removeChildren:[_children objectForKey:[array objectAtIndex:i]]
                         ofItem:[_parentDS->_items objectForKey:[array objectAtIndex:i]]
                    notMatching:filter])
      {
        [array removeObject:[array objectAtIndex:i]];
      }
    }
  }
}

@end
