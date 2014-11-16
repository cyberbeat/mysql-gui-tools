//
//  MQResultSetTab.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 3/13/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQResultSetTab.h"
#import <MySQLToolsCommon/MQResultSetView.h>
#import <MySQLToolsCommon/MSplitView.h>
#import <MySQLToolsCommon/mxUtils.h>

@implementation MQResultSetTab

- (id)initWithIdentifier:(NSString*)identifier
{
  self= [super initWithIdentifier:identifier];
  if (self)
  {
    NSView *box;

    _rslist= [[NSMutableArray alloc] initWithCapacity:1];

    box= [[NSView alloc] initWithFrame:NSMakeRect(0,0,400,400)];
    [box setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];	
    [box setAutoresizesSubviews:YES];

	splitView= [[MSplitView alloc] initWithFrame:NSMakeRect(0, 20, 400, 380)];
    [splitView setDelegate:self];
    [splitView setDividerColor:[NSColor colorWithDeviceWhite:0.8 alpha:1.0]];
    [splitView setDividerThickness:6.0];
	[splitView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];	
    [box addSubview:splitView];
    [splitView release];

    statusText= [[NSTextField alloc] initWithFrame:NSMakeRect(17, 1, 380, 16)];
    [statusText setFont:[NSFont systemFontOfSize:[NSFont smallSystemFontSize]]];
    [statusText setBordered:NO];
    [statusText setDrawsBackground:NO];
    [statusText setEditable:NO];
    [statusText setAutoresizingMask:NSViewMaxXMargin|NSViewMaxYMargin];
    [box addSubview:statusText];
    [statusText release];
    
    statusImage= [[NSImageView alloc] initWithFrame:NSMakeRect(1, 0, 20, 20)];
    [statusImage setImageScaling:NSScaleNone];
    [statusImage setImageFrameStyle:NSImageFrameNone];
    [statusText setAutoresizingMask:NSViewWidthSizable|NSViewMaxYMargin];
    [box addSubview:statusImage];
    [statusImage release];
    
    [self setView:box];
  }

  return self;
}


- (void)dealloc
{
  [_rslist release];
  [super dealloc];
}


- (void)setDefaultSchema:(NSString*)schema
{
  unsigned int i;
  for (i= 0; i < [_rslist count]; i++)
    [[_rslist objectAtIndex:i] setDefaultSchema:schema];
  
  if (schema)
    [self setStatusText:[NSString stringWithFormat:@"Selected schema '%@'.", schema]
                   icon:nil];
  else
    [self setStatusText:@"No schema selected."
                   icon:MXGetCommonImage(MXMiniNoticeImage)];
}

- (void)setVertical:(BOOL)flag
{
  [splitView setVertical:flag];
}

- (void)setStatusText:(NSString*)text icon:(NSImage*)icon
{
  [statusText setStringValue:text];
  [statusImage setImage:icon];
}

- (BOOL)vertical
{
  if ([_rslist count]>1)
    return [splitView isVertical];
  else
    return NO;
}


- (void)exportResultSet:(const char*)format
{
  MQResultSetView *rsview= [self activeResultSet];
  MYX_TABLE_EXPORTER *exporter;
  MYX_RESULTSET *rset= [rsview resultset];
  NSSavePanel *panel;
  char *detailQuery= NULL;
  
  if (!rsview || !rset)
    return;
    
  exporter= myx_get_table_exporter(format);
  if (!exporter)
    return;
  
  if (strcmp(format, "XML")==0 || strcmp(format, "HTML")==0)
  {
    if ([_rslist count] > 1)
    {
      unsigned int i;
      for (i= 0; i < [_rslist count]; i++)
      {
        if ([[_rslist objectAtIndex:i] masterView] == rsview)
        {
          detailQuery= g_strdup([[[_rslist objectAtIndex:i] query] UTF8String]);
          break;
        }
      }
    }
  }
  
  panel= [NSSavePanel savePanel];
  
  [panel setTitle:[NSString stringWithUTF8String:exporter->description]];
  
  if ([panel runModalForDirectory:nil file:[NSString stringWithFormat:@"resultset.%s", exporter->file_extension]] == NSFileHandlingPanelOKButton)
  {
    NSString *filename= [panel filename];
    MYX_TABLE_EXPORTER_INFO *info;
    
    [self setStatusText:@"Exported resultset..." icon:nil];
    
    info= (*exporter->init)();
    
    if (myx_export_resultset([rsview mysql], info,
                             [filename fileSystemRepresentation],
                             "$QUERY$",
                             rset, detailQuery)< 0)
    {
      NSRunAlertPanel(@"Export Error", @"There was an error while exporting the resultset.",
                      @"OK", nil, nil);
      
      [self setStatusText:@"Could not export resultset." icon:MXGetCommonImage(MXMiniErrorImage)];
    }
    else
      [self setStatusText:@"Resultset exported." icon:nil];
  
    (*exporter->free)(info);
  }
  g_free(detailQuery);
}

- (void)setEmbeddedQueryArea:(BOOL)flag
{
  if (flag && !topViewBox)
  {
    topViewBox= [[NSView alloc] initWithFrame:NSMakeRect(0,0,100,100)];
    [topViewBox setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];

    mainSplit= [[MSplitView alloc] initWithFrame:[splitView frame]];
    [mainSplit setDelegate:self];
    [mainSplit setDividerColor:[NSColor colorWithDeviceWhite:0.8 alpha:1.0]];
    [mainSplit setDividerThickness:6.0];
    [mainSplit setAutoresizingMask:NSViewHeightSizable|NSViewWidthSizable];

    [splitView retain];

    [splitView removeFromSuperview];
    [mainSplit addSubview:topViewBox];
    [mainSplit addSubview:splitView];

    [[self view] addSubview:mainSplit];

    [splitView release];
  }
  else if (!flag && topViewBox)
  {
    NSRect rect= [mainSplit frame];

    [splitView retain];
    [splitView removeFromSuperview];
    [mainSplit removeFromSuperview];
    [mainSplit release];
    mainSplit= nil;

    [topViewBox removeFromSuperview];
    [topViewBox release];
    topViewBox= nil;

    [splitView setFrame:rect];
    [[self view] addSubview:splitView];

    [splitView release];
  }
}

- (void)setTopView:(NSView*)aView
{
  if (aView)
  {
    [aView retain];
    [aView removeFromSuperview];
    [topViewBox addSubview:aView];
    [aView setFrame:[topViewBox frame]];
    [aView release];
    topView= aView;
  }
  else if (topView)
  {
    topView= nil;
  }
}

- (void)addResultSet:(MQResultSetView*)rsview
{
  unsigned int i;
  [_rslist insertObject:rsview atIndex:0];

  if ([_rslist count] == 1)
    [splitView addSubview:[rsview view]];
  else
    [splitView addSubview:[rsview view] 
           positioned:NSWindowAbove 
           relativeTo:[_rslist objectAtIndex:1]];
  [[rsview view] setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
  [splitView adjustSubviews];

  if ([_rslist count] > 1)
  {
    for (i= 0; i < [_rslist count]; i++)
    {
      [[_rslist objectAtIndex:i] setCloseable:YES];
      [[_rslist objectAtIndex:i] changeSplitDirection:[splitView isVertical]?MQDVertical:MQDHorizontal];
    }
  }
}

- (void)closeResultSet:(MQResultSetView*)rsview
{
  [[rsview view] removeFromSuperview];
  [_rslist removeObject:rsview];
  
  if ([_rslist count] == 1)
  {
    [[_rslist lastObject] setCloseable:NO];
    [[_rslist lastObject] changeSplitDirection:MQDNone];
  }
  [self setActiveResultSet:[_rslist lastObject]];
}


- (BOOL)hasUnsavedChanges
{
  unsigned int i;
  for (i= 0; i < [_rslist count]; i++)
  {
    if ([[_rslist objectAtIndex:i] hasChanges])
      return YES;
  }
  return NO;
}

- (void)compareResultsets:(id)sender
{  
  if ([_rslist count]==2)
  {
    MYX_RESULTSET *rs1, *rs2;
    rs1= [[_rslist objectAtIndex:0] resultset];
    rs2= [[_rslist objectAtIndex:1] resultset];
    if (rs1 && rs2 && myx_query_compare_possible(rs1,rs2))
    {
      if (myx_query_compare_results(rs1, rs2) < 0)
      {
        NSRunAlertPanel(@"Error", @"Resultset comparison failed.", @"OK", nil, nil);
      }
      else
      {
        if (![self vertical])
          [self setVertical:YES];

        [[[_rslist objectAtIndex:0] tableView] reloadData];
        [[[_rslist objectAtIndex:1] tableView] reloadData];
      }
      return;
    }
  }
  NSRunAlertPanel(@"Compare Resultsets",@"You must have two resultsets displayed in the same tab, with the same column names and types to be able to compare them.",
                  @"OK",nil,nil);  
}


- (NSArray*)resultSetsWithMaster:(MQResultSetView*)rsview
{
  NSMutableArray *array= [NSMutableArray array];
  unsigned int i, c= [_rslist count];
  for (i= 0; i < c; i++)
  {
    MQResultSetView *rsv= [_rslist objectAtIndex:i];
    if ([rsv masterView] == rsview)
    {
      [array addObject:rsv];
    }
  }
  if ([array count]==0)
    return nil;
  return array;
}


- (void)setActiveResultSet:(MQResultSetView*)rsview
{
  unsigned int i, c= [_rslist count];
  for (i= 0; i < c; i++)
  {
    MQResultSetView *rsv= [_rslist objectAtIndex:i];
    if (rsv != rsview)
    {
      [rsv setActive:NO];
    }
  }
  [rsview setActive:YES];
}


- (MQResultSetView*)activeResultSet
{
  unsigned int i, c= [_rslist count];
  for (i= 0; i < c; i++)
  {
    MQResultSetView *rsv= [_rslist objectAtIndex:i];
    if ([rsv active])
      return rsv;
  }
  return nil;
}


- (float)splitView:(NSSplitView *)sender
 constrainMinCoordinate:(float)proposedMin
       ofSubviewAt:(int)offset
{
  if (sender == splitView)
  {
    NSRect r= [[[sender subviews] objectAtIndex:offset] frame];
    if (![sender isVertical])
      return r.origin.y + 100;
    else
      return r.origin.x + 200;
  }
  else
    return proposedMin;
}

- (float)splitView:(NSSplitView *)sender
 constrainMaxCoordinate:(float)proposedMax
       ofSubviewAt:(int)offset
{
  if (sender == splitView)
  {
    NSRect r= [[[sender subviews] objectAtIndex:offset+1] frame];
    if (![sender isVertical])
      return (r.origin.y + r.size.height) - 100 - [sender dividerThickness];
    else
      return (r.origin.x + r.size.width) - 200 - [sender dividerThickness];
  }
  else
    return proposedMax;
}

@end
