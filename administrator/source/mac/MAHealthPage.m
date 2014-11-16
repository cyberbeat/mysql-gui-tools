//
//  MAHealthPage.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on 2/7/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MAHealthPage.h"
#import "MAHealthGraph.h"
#import "MAMenuContextView.h"

#define GRAPH_SPACING 5

@implementation MAHealthPage

- (id)initWithProperties:(NSDictionary*)props
{
  self= [super initWithIdentifier:[props objectForKey:@"title"]];
  if (self)
  {
    MAMenuContextView *view= [[[MAMenuContextView alloc] init] autorelease];

    [self setLabel:[props objectForKey:@"title"]];
    /// description
    _props= [props retain];
    _groups= [[NSMutableArray alloc] init];

    [view setAttachedObject:self];
    [self setView:view];
  }
  return self;
}

- (void)dealloc
{
  [_props release];
  [_groups release];
  
  [super dealloc];
}

- (void)addGroup:(MAHealthGroup*)group
{
  [_groups addObject:group];
  [[self view] addSubview:group];
  [group setPage:self];
}


- (void)removeGroup:(MAHealthGroup*)group
{
  [group setPage:nil];
  [_groups removeObject:group];
  [group removeFromSuperview];
  [self rearrange];
}

- (void)rearrange
{
  unsigned int i;
  float y= [[self view] frame].size.height-10;
  
  for (i= 0; i < [_groups count]; i++)
  {
    id group= [_groups objectAtIndex:i];
    y-= [group frame].size.height+5;
    [group setFrame:NSMakeRect(17.0, y,
                               [[self view] frame].size.width-17.0*2, [group frame].size.height)];
  }
  [[self view] setNeedsDisplay:YES];
}

- (void)setMenu:(NSMenu*)menu
{
  [[self view] setMenu:menu];
}

- (void)setProperties:(NSDictionary*)props
{
  if (_props != props)
  {
    [_props release];
    _props= [props retain];
  }
  
  [self setLabel:[props objectForKey:@"title"]];
}

- (NSDictionary*)properties
{
  return _props;
}

- (NSArray*)groups
{
  return _groups;
}

@end

//=============================================================================

@implementation MAHealthGroup

- (id)initWithProperties:(NSDictionary*)props
{
  self= [super init];
  if (self)
  {
    MAMenuContextView *view= [[[MAMenuContextView alloc] init] autorelease];
    NSSize margins;

    [view setAttachedObject:self];

    margins.width= 15;
    margins.height= 15;
    [self setContentViewMargins:margins];
    
    [self setBoxType:NSBoxPrimary];
    [self setTitle:[props objectForKey:@"title"]];
    [self setContentView:view];
    [self setAutoresizingMask:NSViewWidthSizable|NSViewMinYMargin];

    NSSize size= [self contentSize];  
    size.height+= 15+[self contentViewMargins].height*2;
    [self setFrameSize:size];  
    
    _graphs= [[NSMutableArray alloc] init];

    _props= [props retain];
  }
  return self;
}


- (NSSize)contentSize
{
  NSSize size= [[self contentView] frame].size;
  unsigned int i;
  
  size.height= 0.0;
  for (i= 0; i < [_graphs count]; i++)
  {
    size.height+= [[_graphs objectAtIndex:i] frame].size.height;
    size.height+= GRAPH_SPACING;
  }
  return size;
}

- (void)addGraph:(MAHealthGraph*)graph
{
  NSRect rect= [graph frame];
  NSSize size= [self contentSize];
  float y= size.height;
  
  [_graphs addObject:graph];

  [graph setAutoresizingMask:NSViewWidthSizable|NSViewMaxYMargin];
  [[self contentView] addSubview:graph];

  size.height= y + rect.size.height + [self contentViewMargins].height*2 + 15.0;

  [graph setFrame:NSMakeRect(0.0, y, size.width, rect.size.height)];
  
  [self setFrameSize:size];
}


- (void)removeGraph:(MAHealthGraph*)graph
{
  [graph removeFromSuperview];
  [_graphs removeObject:graph];
  
  NSSize size= [self contentSize];  
  size.height+= 15+[self contentViewMargins].height*2;
  [self setFrameSize:size];  
}


- (void)setProperties:(NSDictionary*)props
{
  if (_props != props)
  {
    [_props release];
    _props= [props retain];
  }
  
  [self setTitle:[props objectForKey:@"title"]];
}

- (NSDictionary*)properties
{
  return _props;
}

- (void)setMenu:(NSMenu*)menu
{
  [super setMenu:menu];
  [[self contentView] setMenu:menu];
}

- (NSMenu *)menuForEvent:(NSEvent *)theEvent
{
  int i;
  NSMenu *menu= [self menu];
  
  for (i= 0; i < [menu numberOfItems]; i++)
  {
    [[menu itemAtIndex:i] setRepresentedObject:self];
  }
  
  return menu;
}

- (void)dealloc
{
  [_props release];
  [_graphs release];
  [super dealloc];
}

- (void)setPage:(MAHealthPage*)page
{
  if (_page != page)
  {
    [_page release];
    _page= [page retain];
  }
}

- (MAHealthPage*)page
{
  return _page;
}

- (NSArray*)graphs
{
  return _graphs;
}

@end
