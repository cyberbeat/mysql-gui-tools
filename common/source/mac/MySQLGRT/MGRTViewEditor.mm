//
//  MGRTViewEditor.mm
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/9/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MGRTViewEditor.h"
#import <MySQLToolsCommon/myxutil.h>
#import <MySQLToolsCommon/MSourceTextEditor.h>

@implementation MGRTViewEditor

- (id)init
{
  self= [super initWithWindowNibName: @"GRTViewEditor"];
  if (self)
  {
    
  }
  return self;
}


- (BOOL)commit
{
  _viewData->setName([[nameText stringValue] UTF8String]);
  _viewData->setComment([[[commentText textStorage] string] UTF8String]);
  
  _viewData->setCode([[[editor textView] string] UTF8String]);
  
  _viewData->commit();

  return YES;
}


- (void)revert
{
  _viewData->revert();
}


- (void)showObject
{
  [nameText setStringValue:NSStr(_viewData->name())];

  [[editor textView] setString:NSStr(_viewData->code())];

  [commentText setString:NSStr(_viewData->comment())];
}


- (void)dealloc
{
  delete _viewData;
  [super dealloc];
}


- (MYX_GRT_VALUE*)editedObject
{
  return _viewData->value().grtValue();
}


- (void)editObject:(MYX_GRT_VALUE*)value
{
  _viewData= new MGRTView([_grt grt], value);
  
  [self showObject];
}

@end
