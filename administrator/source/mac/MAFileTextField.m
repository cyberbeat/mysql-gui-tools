//
//  MAFileTextField.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Mon Aug 02 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MAFileTextField.h"


@implementation MAFileTextField

- (void)_fileDidChoose:(NSOpenPanel *)sheet
            returnCode:(int)returnCode
           contextInfo:(void *)contextInfo
{
  if (returnCode == NSOKButton)
    [_textf setStringValue:[sheet filename]];
}

- (void)_chooseFile:(id)sender
{
  NSOpenPanel *panel= [NSOpenPanel openPanel];
  [panel setCanChooseDirectories:_directory];
  [panel setCanChooseFiles:!_directory];
  
  [panel beginSheetForDirectory:_directory?[_textf stringValue]:[[_textf stringValue] stringByDeletingLastPathComponent]
                           file:_directory?nil:[[_textf stringValue] lastPathComponent]
                          types:nil
                 modalForWindow:[self window]
                  modalDelegate:self
                 didEndSelector:@selector(_fileDidChoose:returnCode:contextInfo:)
                    contextInfo:NULL];
}

- (id)initWithFrame:(NSRect)frame 
{
    self = [super initWithFrame:frame];
    if (self) 
    {
      NSRect tr, br;
      _button= [[NSButton alloc] init];
      [_button setTitle:@"..."];
      [_button setBezelStyle:NSRoundedBezelStyle];
      [[_button cell] setFont:[NSFont systemFontOfSize:[NSFont smallSystemFontSize]]];
      [[_button cell] setControlSize:NSSmallControlSize];
      [_button sizeToFit];
      [_button setAutoresizingMask:NSViewMinXMargin|NSViewMaxYMargin];
      NSDivideRect(frame,&tr,&br,frame.size.width-[_button frame].size.width,NSMinXEdge);
      [_button setFrame:br];
      [self addSubview:_button];
      _textf= [[NSTextField alloc] initWithFrame:tr];
      [_textf setAutoresizingMask:NSViewWidthSizable|NSViewMaxYMargin];
      [self addSubview:_textf];
      
      [_button setAction:@selector(_chooseFile:)];
      [_button setTarget:self];
    }
    return self;
}

- (void)dealloc
{
  [_textf release];
  [_button release];
  [super dealloc];
}

- (void)setPickDirectory:(BOOL)flag
{
  _directory= flag;
}

- (void)setPath:(NSString*)path
{
  [_textf setStringValue:path];
}

- (NSString*)path
{
  return [_textf stringValue];
}

@end
