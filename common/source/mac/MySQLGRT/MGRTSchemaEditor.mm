//
//  MGRTSchemaEditor.mm
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/9/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MGRTSchemaEditor.h"
#import <MySQLToolsCommon/mxUtils.h>

@implementation MGRTSchemaEditor

- (id)init
{
  self= [super initWithWindowNibName: @"GRTSchemaEditor"];
  if (self)
  {
    
  }
  return self;
}


- (BOOL)commit
{
  _schemaData->setName([[nameText stringValue] UTF8String]);
  _schemaData->setComment([[[commentText textStorage] string] UTF8String]);
  
  _schemaData->setCollation([[collationPop titleOfSelectedItem] UTF8String]);
  
  _schemaData->commit();

  return YES;
}


- (void)revert
{
  _schemaData->revert();
}


- (void)showObject
{
  [self fillCollationPopUp:collationPop];
  
  [nameText setStringValue:NSStr(_schemaData->name())];
  [collationPop selectItemWithTitle:NSStr(_schemaData->collation())];
  [commentText setString:NSStr(_schemaData->comment())];
}


- (void)dealloc
{
  delete _schemaData;
  [super dealloc];
}


- (MYX_GRT_VALUE*)editedObject
{
  return _schemaData->value().grtValue();
}


- (void)editObject:(MYX_GRT_VALUE*)value
{
  _schemaData= new MGRTSchema([_grt grt], value);
  
  [self showObject];
}


- (void)createNew
{
  const char *schemaStruct= (*_catalogs)["schemata"].listContentStruct();
  _schemaData= new MGRTSchema([_grt grt], MGRTValue::createObject([_grt grt], schemaStruct, "newSchema", *_catalogs).grtValue());

  (*_catalogs)["schemata"].append(_schemaData->value().grtValue());
  
  [self showObject];
}


@end
