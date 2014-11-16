//
//  MSQLEditor.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 05/7/6.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#import <MySQLToolsCommon/MSourceTextEditor.h>

#include <myx_public_interface.h>

@class MSQLSyntaxColoring;

@interface MSQLEditor : NSWindowController 
{
  IBOutlet MSourceTextEditor *editor;
  MYX_SQL_HIGHLIGHTING *_hl;
  MYX_SYN *_syn;
  
  MSQLSyntaxColoring *_colorer;
}

- (IBAction)performOK:(id)sender;
- (IBAction)performCancel:(id)sender;

- (id)initWithMySQL:(MYSQL*)mysql;

- (void)setScript:(NSString*)script;
- (NSString*)script;


@end
