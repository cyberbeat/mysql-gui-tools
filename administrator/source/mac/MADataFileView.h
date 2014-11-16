//
//  MADataFileView.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on 1/2/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class MADataFileEditor;

@interface MADataFileView : NSView
{
  NSScrollView *sview;
  NSTableView *table;
  NSButton *addB;
  NSButton *delB;
  NSButton *checkB;

  NSMutableArray *rows;
  
  NSString *lastParams;
  
  MADataFileEditor *editor;
}
- (IBAction)addFile:(id)sender;
- (IBAction)removeFile:(id)sender;
- (void)setStringValue:(NSString*)value;
- (NSString*)stringValue;
@end
