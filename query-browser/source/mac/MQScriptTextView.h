//
//  MQScriptTextView.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/5.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLToolsCommon/MSourceTextEditor.h>
#import <MySQLToolsCommon/MSourceTextView.h>

@class MSchemaEditHelper;

@interface MQScriptTextView : MSourceTextView
{
  MSchemaEditHelper *_schemaHelper;
}

- (void)setHelper:(MSchemaEditHelper*)helper;

@end



@interface MQScriptEditor : MSourceTextEditor
{
}
@end