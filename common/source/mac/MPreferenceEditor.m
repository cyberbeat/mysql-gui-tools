//
//  MPreferenceEditor.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 1/8/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MPreferenceEditor.h"
#import "MConnectionEditor.h"
#import "MNibOwner.h"

@implementation MPreferenceEditor


- (id)initForConnectionsFile:(NSString*)file
{
  self= [super init];
  if (self)
  {
    if (![NSBundle loadNibNamed:@"PreferencesEditor" owner:self])
    {
      NSLog(@"Could not load PreferencesEditor nib file");
      [self release];
      return nil;
    }
    _connectionEditor= [[MConnectionEditor alloc] initForFile:file];
    [[tabView tabViewItemAtIndex:0] setView: _connectionEditor->topView];

    _pages= [[NSMutableArray alloc] init];
  }
  return self;
}


- (id)registerPageNib:(NSString*)nibName withLabel:(NSString*)label
{
  MNibOwner *owner= [[MNibOwner alloc] init];
  NSTabViewItem *tabItem= [[NSTabViewItem alloc] initWithIdentifier:label];
  
  if (![NSBundle loadNibNamed:nibName owner:owner])
    NSLog(@"Can't load nib");

  [tabItem setLabel:label];
  [tabItem setView:owner->view];
  [tabView addTabViewItem:tabItem];
  [tabItem release];
  
  [_pages addObject:owner];
  [owner release];
  
  return owner->controller;
}


- (id)registerMultiPageNib:(NSString*)nibName withLabels:(NSArray*)labels
{
  MNibOwner *owner= [[MNibOwner alloc] init];
  NSTabViewItem *tabItem;
  unsigned int i;
  
  if (![NSBundle loadNibNamed:nibName owner:owner])
    NSLog(@"Can't load nib");
  
  for (i= 0; i < [labels count]; i++)
  {
    tabItem= [[NSTabViewItem alloc] initWithIdentifier:[labels objectAtIndex:i]];
    [tabItem setLabel:[labels objectAtIndex:i]];
    switch (i)
    {
      case 0:
        [tabItem setView:owner->view];
        break;
      case 1:
        [tabItem setView:owner->view1];
        break;
      case 2:
        [tabItem setView:owner->view2];
        break;
      case 3:
        [tabItem setView:owner->view3];
        break;
      case 4:
        [tabItem setView:owner->view4];
        break;
      case 5:
        [tabItem setView:owner->view5];
        break;        
    }
    [tabView addTabViewItem:tabItem];
    [tabItem release];
  
    [_pages addObject:owner];
    [owner release];
  }
  return owner->controller;
}


- (void)dealloc
{
  [_pages release];
  [_connectionEditor release];
  [super dealloc];
}


- (NSWindow*)window
{
  return window;
}

- (void)windowWillClose:(NSNotification *)aNotification
{
  [_connectionEditor save:self];
  
  if (_inModalLoop)
    [NSApp stopModal];
  
  [window orderOut:nil];
}


- (void)show
{
  unsigned int i;
  
  for (i= 0; i < [_pages count]; i++)
  {
    MNibOwner *owner= [_pages objectAtIndex:i];
    
    if (owner->controller && [owner->controller respondsToSelector:@selector(willShow)])
      [owner->controller performSelector:@selector(willShow)];
  }
  [window makeKeyAndOrderFront:nil];
}


- (int)runConnectionEditor
{
  [window makeKeyAndOrderFront:nil];
  _inModalLoop= YES;
  return [NSApp runModalForWindow:window];
}

@end
