//
//  MDialogs.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Sat Jul 10 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MDialogs.h"
 
@implementation MStringRequestSheet

+ (MStringRequestSheet*)sheetWithTitle:(NSString*)title labels:(NSArray*)labels;
{
  return [[[MStringRequestSheet alloc] initWithTitle:title labels:labels] autorelease];
}


+ (MStringRequestSheet*)sheetWithTitle:(NSString*)title label:(NSString*)label
{
  return [[[MStringRequestSheet alloc] initWithTitle:title labels:[NSArray arrayWithObject:label]] autorelease];
}


- (id)initWithTitle:(NSString*)title labels:(NSArray*)labels
{
  return [self initWithTitle:title labels:labels buttons:nil];
}


- (id)initWithTitle:(NSString*)title labels:(NSArray*)labels buttons:(NSArray*)buttons
{
  NSRect contentRect;
  
  contentRect= NSMakeRect(0.0, 0.0, 420.0, 128.0+([labels count]-1)*(22.0+8.0));
  
  self= [super initWithContentRect:contentRect 
                         styleMask:NSTitledWindowMask
                           backing:NSBackingStoreBuffered 
                             defer:YES];
  if (self)
  {
    NSView *contentView= [[NSView alloc] initWithFrame:contentRect];
	id w;
	unsigned int i;
	
    [self setContentView:contentView];
    [contentView release];
    [self setHasShadow:YES];
    
	w= [[NSTextField alloc] initWithFrame:NSMakeRect(17.0, contentRect.size.height-34.0, 387.0, 17.0)];
    [w setStringValue:title];
    [contentView addSubview:w];
    [w release];
	[w setFont:[NSFont boldSystemFontOfSize:[NSFont systemFontSize]]];
    [w setAlignment:NSCenterTextAlignment];
    [w setEditable:NO];
    [w setDrawsBackground:NO];
    [w setBordered:NO];

	form= [[NSForm alloc] initWithFrame:NSMakeRect(20.0, 59.0, 
												   380.0, 22+([labels count]-1)*(22.0+8.0))];
	[form setCellSize:NSMakeSize([form cellSize].width, 22.0)];
	[contentView addSubview:form];
	[form release];
	for (i= 0; i < [labels count]; i++)
	{
	  [form addEntry:[labels objectAtIndex:i]];
	}
    
    if (!buttons)
    {
      w= [[NSButton alloc] initWithFrame:NSMakeRect(316.0, 11.0, 90.0, 32.0)];
      [w setTag:0];
      [contentView addSubview:w];
      [w setTitle:@"OK"];
      [w release];
      [w setAction:@selector(buttonClicked:)];
      [w setTarget:self];
      [[w cell] setBezelStyle:NSRoundedBezelStyle];
      [self setDefaultButtonCell:[w cell]];
    
      w= [[NSButton alloc] initWithFrame:NSMakeRect(224.0, 11.0, 90.0, 32.0)];
      [w setTag:-1];
      [contentView addSubview:w];
      [w setTitle:@"Cancel"];
      [w setKeyEquivalent:@"\E"];
      [w release];
      [w setAction:@selector(buttonClicked:)];
      [w setTarget:self];
      [[w cell] setBezelStyle:NSRoundedBezelStyle];
    }
    else
    {
      unsigned int i;
      float x= 420-14;
      NSFont *font= [NSFont systemFontOfSize:[NSFont systemFontSize]];
      
      for (i= 0; i < [buttons count]; i++)
      {
        float size;
        
        size= [font widthOfString:[buttons objectAtIndex:i]];
        if (size < 90.0-20.0)
          size= 90.0;
        else
          size+= 35.0;
        
        w= [[NSButton alloc] initWithFrame:NSMakeRect(x-size, 11.0, size, 32.0)];
        [w setTag:i];
        [contentView addSubview:w];
        [w setTitle:[buttons objectAtIndex:i]];
        [w release];
        [w setAction:@selector(buttonClicked:)];
        [w setTarget:self];
        [[w cell] setBezelStyle:NSRoundedBezelStyle];
        
        x-= size+2;
      }
      // Last is supposed to be Escape
      [w setKeyEquivalent:@"\E"];
    }
  }
  return self;
}


- (void)buttonClicked:(id)sender
{
  [NSApp endSheet:self];
  button= [sender tag];
}

- (void)sheetDidEnd:(NSWindow *)sheet returnCode:(int)returnCode contextInfo:(void *)contextInfo
{
  [NSApp abortModal];
  [sheet orderOut:self];
}

- (BOOL)acceptsFirstResponder
{
  return YES;
}

- (NSArray*)runModal:(NSWindow*)window
{
  NSMutableArray *array;
  
  [NSApp beginSheet:self
     modalForWindow:window
      modalDelegate:self
     didEndSelector:@selector(sheetDidEnd:returnCode:contextInfo:)
        contextInfo:self];

  [NSApp runModalForWindow:self];

  if (button == -1)
    return nil;
  else
  {
	unsigned int i;
	array= [NSMutableArray arrayWithCapacity:[form numberOfRows]];
	
	for (i= 0; i < [form numberOfRows]; i++)
	  [array addObject:[[form cellAtIndex:i] stringValue]];
    return array;
  }
}

- (void)setDefaultValues:(NSArray*)array
{
  unsigned int i;
  
  for (i= 0; i < [form numberOfRows]; i++)
	[[form cellAtIndex:i] setStringValue:[array objectAtIndex:i]];
}


- (int)clickedButton
{
  return button;
}

@end
