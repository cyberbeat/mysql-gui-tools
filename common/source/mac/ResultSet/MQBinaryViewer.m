//
//  MQBinaryViewer.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 4/28/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQBinaryViewer.h"

@interface MQBVDataSource : NSObject
{
  NSData *_data;
}
- (void)setData:(NSData*)data;
@end

@implementation MQBVDataSource
- (void)setData:(NSData*)data
{
  if (_data != data)
  {
    [_data release];
    _data= [data retain];
  }
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  return ([_data length] + 15) / 16;
}


- (id)tableView:(NSTableView *)aTableView 
objectValueForTableColumn:(NSTableColumn *)aTableColumn
            row:(int)rowIndex
{
  if ([[aTableColumn identifier] isEqualTo:@"offset"])
    return [NSString stringWithFormat:@"%08x", rowIndex*16];
  else
  {
    int index= [[aTableColumn identifier] intValue];
    char *ptr= (char*)[_data bytes];

    if (index < 100)
    {
      if (rowIndex*16 + index < [_data length])
        return [NSString stringWithFormat:@"%02x", (unsigned char)ptr[rowIndex*16+index]];
      else
        return @"";
    }
    else
    {
      index-= 100;
      if (rowIndex*16 + index*4 < [_data length])
      {
        int i;
        char buffer[5];
        for (i= 0; i < 4; i++)
          buffer[i]= isprint(ptr[rowIndex*16+index*4+i])?ptr[rowIndex*16+index*4+i]:'.';
        buffer[4]= 0;
        return [NSString stringWithFormat:@"%s", buffer];
      }
      else
        return @"";
    }
  }
}

@end


@implementation MQBinaryViewer

- (id)initWithData:(NSData*)data
{
  self= [super init];
  if (self)
  {
    unsigned int i;
    NSTableColumn *column;
    NSFont *font;
    NSColor *color1, *color2;

    _sview= [[NSScrollView alloc] initWithFrame:NSMakeRect(0,0,200,200)];
    [_sview setHasHorizontalScroller:NO];
    [_sview setHasVerticalScroller:YES];
    [_sview setBorderType: NSGrooveBorder];
    [_sview setAutoresizingMask:NSViewMinXMargin|NSViewMaxXMargin|NSViewHeightSizable];
    [[_sview contentView] setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [[_sview contentView] setAutoresizesSubviews:YES];
    
    _table= [[NSTableView alloc] initWithFrame:[[_sview contentView] frame]];
//    [_table setDrawsGrid:YES];
    [_table setHeaderView:nil];
    [_table setRowHeight:14];
    [_table setIntercellSpacing:NSMakeSize(0,0)];
    [_sview setDocumentView:_table];
    
    _dataSource= [[MQBVDataSource alloc] init];
    [_dataSource setData:data];
    [_table setDataSource:_dataSource];
    [_table reloadData];
    
    font= [NSFont fontWithName:@"Courier" size:11.0];
  
    column= [[[NSTableColumn alloc] initWithIdentifier:@"offset"] autorelease];
    [column setWidth:[font widthOfString:@"00000000"]+10];
    [[column dataCell] setFont:font];
    [_table addTableColumn:column];
    [column setEditable:NO];
    
    color1= [NSColor colorWithDeviceRed:0.9 green:1.0 blue:0.9 alpha:1.0];
    color2= [NSColor colorWithDeviceRed:0.9 green:0.9 blue:1.0 alpha:1.0];
    
    for (i= 0; i < 16; i++)
    {
      column= [[[NSTableColumn alloc] initWithIdentifier:[NSNumber numberWithInt:i]] autorelease];
      [column setWidth:[font widthOfString:@"00"]+((i==15)?10:4)];
      [column setEditable:NO];
      [_table addTableColumn:column];
      if ((i / 4) & 1)
        [[column dataCell] setBackgroundColor:color1];
      else
        [[column dataCell] setBackgroundColor:color2];
      [[column dataCell] setDrawsBackground:YES];
      [[column dataCell] setFont:font];
    }
    
    for (i= 0; i < 4; i++)
    {
      column= [[[NSTableColumn alloc] initWithIdentifier:[NSNumber numberWithInt:100+i]] autorelease];
      [column setWidth:[font widthOfString:@"XXXX"]+4];
      [column setEditable:NO];
      [_table addTableColumn:column];
      if (i & 1)
        [[column dataCell] setBackgroundColor:color1];
      else
        [[column dataCell] setBackgroundColor:color2];
      [[column dataCell] setDrawsBackground:YES];
      [[column dataCell] setFont:font];
      [[column dataCell] setWraps:NO];
    }
  }
  return self;
}

- (void)setData:(NSData*)data
{
  [_dataSource setData:data];
  [_table reloadData];
}


- (NSString*)label
{
  return @"Binary";
}

- (NSView*)view
{
  return _sview;
}

- (void)setEditable:(BOOL)flag
{
}

@end
