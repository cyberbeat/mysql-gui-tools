//
//  MGRTViewEditor.h
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/9/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLGRT/MGRTObjectEditor.h>
#include <MySQLGRT/MGRTView.h>

@class MSourceTextEditor;

@interface MGRTViewEditor : MGRTObjectEditor 
{
  IBOutlet NSTextView *commentText;
  IBOutlet NSTextField *nameText;
  IBOutlet MSourceTextEditor *editor;
  
  MGRTView *_viewData;
}

- (void)editObject:(MYX_GRT_VALUE*)value;

@end
