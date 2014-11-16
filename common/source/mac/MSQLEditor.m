//
//  MSQLEditor.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 05/7/6.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MSQLEditor.h"
#import "MSQLSyntaxColoring.h"

@implementation MSQLEditor

- (IBAction)performOK:(id)sender
{
  [NSApp stopModalWithCode:NSOKButton];
}

- (IBAction)performCancel:(id)sender
{
  [NSApp stopModalWithCode:NSCancelButton];
}

- (id)initWithMySQL:(MYSQL*)mysql
{
  self= [super initWithWindowNibName:@"SQLEditor"];
  if (self != nil) 
  {
    [self setShouldCascadeWindows:NO];
    [self window];
    
    _syn= myx_refresh_dbinfo(mysql,NULL);
    _hl= myx_init_sql_parsing(_syn);
  }
  return self;
}

- (void)dealloc 
{
  myx_free_sql_highlighting(_hl);
  myx_free_syn(_syn);
  [_colorer release];
  [super dealloc];
}

- (void)awakeFromNib
{
  [self setWindowFrameAutosaveName:@"SQLEditor"];
  [[self window] setFrameUsingName:@"SQLEditor"];
  [[self window] setFrameAutosaveName:@"SQLEditor"];
  
  
  [editor setShowGutter:YES];
  _colorer= [[MSQLSyntaxColoring alloc] initForTextView:[editor textView]
                                             syntaxInfo:_syn];
  [[editor textView] enableBreakpoints:NO];
  [[editor textView] setSyntaxColorer:_colorer];

  [_colorer addMarkerName:@"statement"];
  [_colorer addMarkerName:@"pc"];
}

- (void)setScript:(NSString*)script
{
  [[editor textView] setString:script];
}

- (NSString*)script
{
  return [[editor textView] string];
}


- (void)windowWillClose:(NSNotification *)aNotification
{
  [self performCancel:nil];
}

@end
