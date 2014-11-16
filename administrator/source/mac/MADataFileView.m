//
//  MADataFileView.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on 1/2/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MADataFileView.h"
#import "MADataFileEditor.h"
#import <MySQLToolsCommon/NSView_extras.h>

@implementation MADataFileView

- (id)init
{
  self= [super init];
  if (self)
  {
    NSTableColumn *column;
    NSRect rect;
    
    sview= [[NSScrollView alloc] initWithFrame:NSMakeRect(0,32,380,68)];
    [sview setHasVerticalScroller:YES];
    [sview setBorderType:NSBezelBorder];
    
    rect.origin= NSMakePoint(0,0);
    rect.size= [sview contentSize];
    [self setFrameSize:NSMakeSize(380,100)];
    table= [[NSTableView alloc] initWithFrame:rect];
    [table setDelegate:self];
    [table setDataSource:self];
    
    [sview setDocumentView:table];
    
    column= [[[NSTableColumn alloc] initWithIdentifier:@"path"] autorelease];
    [column setWidth:250];
    [[column headerCell] setStringValue:@"Path"];
    [column setEditable:YES];
    [table addTableColumn:column];
    
    column= [[[NSTableColumn alloc] initWithIdentifier:@"size"] autorelease];
    [column setWidth:100];
    [[column headerCell] setStringValue:@"Size"];    
    [column setEditable:YES];
    [table addTableColumn:column];
    
    addB= [[NSButton alloc] initWithFrame:NSMakeRect(0,0,24,24)];
    [addB setButtonType:NSMomentaryChangeButton];
    [addB setBordered:NO];
    [[addB cell] setImageDimsWhenDisabled:NO];
    [addB setImage:[NSImage imageNamed:@"add_normal.png"]];
    [addB setAlternateImage:[NSImage imageNamed:@"add_pressed.png"]];
    [addB setTarget:self];
    [addB setAction:@selector(addFile:)];

    delB= [[NSButton alloc] initWithFrame:NSMakeRect(23,0,24,24)];
    [delB setButtonType:NSMomentaryChangeButton];
    [delB setBordered:NO];
    [[delB cell] setImageDimsWhenDisabled:NO];
    [delB setImage:[NSImage imageNamed:@"del_normal.png"]];
    [delB setAlternateImage:[NSImage imageNamed:@"del_pressed.png"]];
    [delB setTarget:self];
    [delB setAction:@selector(removeFile:)];
    
    checkB= [[NSButton alloc] initWithFrame:NSMakeRect(75,3,300,18)];
    [checkB setButtonType:NSSwitchButton];
    [checkB setTitle:@"Extend last data file automatically."];
    
    editor= [[MADataFileEditor alloc] init];

    rows= [[NSMutableArray alloc] init];
    
    [self addSubview:sview];
    [self addSubview:addB];
    [self addSubview:delB];
    [self addSubview:checkB];
  }
  return self;
}


- (void)dealloc
{
  [editor release];
  [rows release];
  [super dealloc];
}


- (void)setEnabled:(BOOL)flag
{
  if (flag)
  {
    [addB setImage:[NSImage imageNamed:@"add_normal.png"]];
    [delB setImage:[NSImage imageNamed:@"del_normal.png"]];
  }
  else
  {
    [addB setImage:[NSImage imageNamed:@"add_disabled.png"]];
    [delB setImage:[NSImage imageNamed:@"del_disabled.png"]];
  }
  [addB setEnabled:flag];
  if ([table selectedRow] < 0)
  {
    [delB setEnabled:NO];
    [delB setImage:[NSImage imageNamed:@"del_disabled.png"]];
  }
  else
    [delB setEnabled:flag];
  [checkB setEnabled:flag];
}


- (IBAction)addFile:(id)sender
{
  [editor showWindow:nil];
  if ([NSApp runModalForWindow:[editor window]] == NSOKButton)
  {
    NSString *value= [editor stringValue];
    [rows addObject:value];
    [table reloadData];
  }
}


- (IBAction)removeFile:(id)sender
{
  int row= [table selectedRow];
  if (row >= 0)
  {
    [rows removeObjectAtIndex:row];
    [table reloadData];
  }
}



- (void)tableViewSelectionDidChange:(NSNotification *)aNotification
{
  if (![addB isEnabled])
    return;
  
  if ([table selectedRow] < 0)
  {
    [delB setImage:[NSImage imageNamed:@"del_disabled.png"]];
    [delB setEnabled:NO];
  }
  else
  {
    [delB setImage:[NSImage imageNamed:@"del_normal.png"]];
    [delB setEnabled:YES];
  }
}


- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  return [rows count];
}


- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(int)rowIndex
{
  NSString *row= [rows objectAtIndex:rowIndex];
  NSArray *array= [row componentsSeparatedByString:@":"];
  
  if ([[aTableColumn identifier] isEqualToString:@"path"])
    return [array objectAtIndex:0];
  else
    return [array objectAtIndex:1];
}


- (void)tableView:(NSTableView *)aTableView setObjectValue:(id)anObject 
   forTableColumn:(NSTableColumn *)aTableColumn row:(int)rowIndex
{
  NSString *row;
  NSArray *array= [[rows objectAtIndex:rowIndex] componentsSeparatedByString:@":"];
  
  if ([[aTableColumn identifier] isEqualToString:@"path"])
    row= [NSString stringWithFormat:@"%@:%@",anObject,[array objectAtIndex:1]];
  else
    row= [NSString stringWithFormat:@"%@:%@",[array objectAtIndex:0],anObject];
  
  [rows replaceObjectAtIndex:rowIndex withObject:row];
}


- (void)setStringValue:(NSString*)value
{
  NSArray *parts= [value componentsSeparatedByString:@";"];
  unsigned int i;

  [checkB setState:NSOffState];
  [rows removeAllObjects];
  for (i= 0; i < [parts count]; i++)
  {
    NSString *config= [parts objectAtIndex:i];
    NSArray *p= [config componentsSeparatedByString:@":"];
    
    if ([p count] > 2)
    {
      if ([[p objectAtIndex:2] isEqualToString:@"autoextend"])
      {
        [checkB setState:NSOnState];
        if (lastParams)
          [lastParams release];
        lastParams= [[[p subarrayWithRange:NSMakeRange(2, [p count]-2)] componentsJoinedByString:@":"] retain];
        
        [rows addObject:[[p subarrayWithRange:NSMakeRange(0, 2)] componentsJoinedByString:@":"]];
      }
    }
    else
      [rows addObject:config];
  }
}


- (NSString*)stringValue
{
  return nil;
}

@end
