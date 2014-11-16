//
//  MGRTRoutineGroupEditor.h
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/9/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLGRT/MGRTObjectEditor.h>
#import <MySQLToolsCommon/MVerticalBox.h>

#include <MySQLGRT/MGRTRoutine.h>

@class MAccessoryScrollView;
@class MSourceTextEditor;

@interface MGRTRoutineGroupEditor : MGRTObjectEditor
{
  IBOutlet NSScrollView *scrollView;
  IBOutlet NSTextField *nameText;
  IBOutlet NSTableView *routineTable;
  
  MVerticalBox *_content;
  NSMutableArray *_editors;
  
  MGRTRoutineGroup *_groupObject;
}

- (IBAction)addRoutine:(id)sender;

- (void)editObject:(MYX_GRT_VALUE*)value;


@end
