//
//  MGRTSchemaEditor.h
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/9/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLGRT/MGRTObjectEditor.h>
#include <MySQLGRT/MGRTSchema.h>


@interface MGRTSchemaEditor : MGRTObjectEditor 
{
  IBOutlet NSTextView *commentText;
  IBOutlet NSTextField *nameText;
  IBOutlet NSPopUpButton *collationPop;
  
  MGRTSchema *_schemaData;
}

- (void)editObject:(MYX_GRT_VALUE*)value;
- (void)createNew;

@end
