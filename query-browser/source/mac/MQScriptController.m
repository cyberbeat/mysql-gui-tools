//
//  MQScriptController.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/31/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQueryBrowserDocument.h"
#import "MQScriptController.h"
#import "MQueryBrowser.h"
#import "MQScriptTab.h"
#import "MQScriptExecutionHelper.h"
#import <MySQLToolsCommon/MMenuButton.h>

@implementation MQScriptController

- (void)awakeFromNib
{
  NSRect wrect= [[owner window] contentRectForFrameRect:[[owner window] frame]];
  NSRect trect;
  trect= [toolbar frame];
  [toolbar setFrameOrigin:NSMakePoint(-1,NSHeight(wrect)-NSHeight(trect))];

  trect= [bigToolbar frame];
  [bigToolbar setFrameOrigin:NSMakePoint(-1,NSHeight(wrect)-NSHeight(trect))];
  
  _idleImage= [[NSImage imageNamed:@"sakila.png"] retain];
  _busyImage= [[NSImage imageNamed:@"dolphin_anim.gif"] retain];
  
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(scriptEditChanged:)
                                               name:MQScriptEditStatusChangedNotification
                                             object:nil];
}


- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [_idleImage release];
  [_busyImage release];
  
  [super dealloc];
}


- (MQScriptTab*)createPage
{
  MQScriptTab *page= [[MQScriptTab alloc] initWithIdentifier:@"bb" 
                                            withConnectionTo:[[owner document] serverInfo]];
  if (page)
  {
    [page setIcon:[NSImage imageNamed:@"tabsheet_icon_script.png"]];
    [page setLabel:@"Script"];
    [page setDefaultSchema:[[owner document] defaultSchema]];
    [(id)[[page scriptEditor] textView] setHelper:[owner schemaHelper]];

    [[page scriptHelper] setDelegate:self];
  }
  return [page autorelease];
}



- (void)switchToolbar:(BOOL)small
{
  if ([[owner currentTab] isKindOfClass:[MQScriptTab class]])
  {
    [bigToolbar setHidden:small?YES:NO];
    [toolbar setHidden:small?NO:YES];
  }
}


- (void)resizeToolbar:(float)height
{
  [bigToolbar setFrameSize:NSMakeSize(NSWidth([bigToolbar frame]), height)];
}


- (void)updateToolbarState
{
  MQScriptTab *page= [owner currentTab];
  MQScriptState state= [[page scriptHelper] state];

  switch (state)
  {
    case ScriptBreakpoint:
    case ScriptWaiting:
      switch (state)
      {
        case ScriptBreakpoint:
          [page setStatusText:@"Breakpoint reached." icon:nil];
          break;
        default:      
          [page setStatusText:@"Press Step or Continue." icon:nil];
          break;
      }
      [executeButton setEnabled:NO];
      [[toolbar viewWithTag:24] setEnabled:NO];
      [stopButton setEnabled:YES];
      [[toolbar viewWithTag:25] setEnabled:YES];
      [pauseButton setEnabled:NO];
      [[toolbar viewWithTag:26] setEnabled:NO];
      [continueButton setEnabled:YES];
      [[toolbar viewWithTag:27] setEnabled:YES];
      [stepButton setEnabled:YES];
      [[toolbar viewWithTag:28] setEnabled:YES];
      [animbox setImage:_idleImage];
      break;
    case ScriptError:
      [executeButton setEnabled:NO];
      [[toolbar viewWithTag:24] setEnabled:NO];
      [stopButton setEnabled:YES];
      [[toolbar viewWithTag:25] setEnabled:YES];
      [pauseButton setEnabled:NO];
      [[toolbar viewWithTag:26] setEnabled:NO];
      [continueButton setEnabled:YES];
      [[toolbar viewWithTag:27] setEnabled:YES];
      [stepButton setEnabled:YES];
      [[toolbar viewWithTag:28] setEnabled:YES];
      [animbox setImage:_idleImage];
      break;      
    case ScriptExecuting:
      [executeButton setEnabled:NO];
      [[toolbar viewWithTag:24] setEnabled:NO];
      [stopButton setEnabled:YES];
      [[toolbar viewWithTag:25] setEnabled:YES];
      [pauseButton setEnabled:YES];
      [[toolbar viewWithTag:26] setEnabled:YES];
      [continueButton setEnabled:NO];
      [[toolbar viewWithTag:27] setEnabled:NO];
      [stepButton setEnabled:NO];
      [[toolbar viewWithTag:28] setEnabled:NO];
      [animbox setImage:_busyImage];
      [page setStatusText:@"Executing..." icon:nil];
      break;
    case ScriptFinished:
    case ScriptStopped:
      if (state == ScriptFinished)
        [page setStatusText:@"Script executed." icon:nil];
      else
        [page setStatusText:@"Script stopped." icon:nil];
    case ScriptIdle:
      [executeButton setEnabled:YES];
      [[toolbar viewWithTag:24] setEnabled:YES];
      [stopButton setEnabled:NO];
      [[toolbar viewWithTag:25] setEnabled:NO];
      [pauseButton setEnabled:NO];
      [[toolbar viewWithTag:26] setEnabled:NO];
      [continueButton setEnabled:NO];
      [[toolbar viewWithTag:27] setEnabled:NO];
      [stepButton setEnabled:NO];
      [[toolbar viewWithTag:28] setEnabled:NO];
      [animbox setImage:_idleImage];
      break;
  }
}


- (IBAction)saveScript:(id)sender
{
  [[owner currentTab] saveScript:sender];
}


- (void)openScript
{
  NSOpenPanel *panel= [NSOpenPanel openPanel];
  NSView *group= [[[NSView alloc] initWithFrame:NSMakeRect(0, 0, 235, 26)] autorelease];
  NSPopUpButton *popup= [[[NSPopUpButton alloc] initWithFrame:NSMakeRect(100,0,130,26)] autorelease];
  NSTextField *caption= [[[NSTextField alloc] initWithFrame:NSMakeRect(0,4,100,17)] autorelease];
  static struct {
    NSString *name;
    NSStringEncoding encoding;
  } encodings[]= {
  { @"UTF-8",      NSUTF8StringEncoding},
  { @"Latin1",     NSISOLatin1StringEncoding},
  { @"Latin2",     NSISOLatin2StringEncoding},
  { @"ISO 2022JP", NSISO2022JPStringEncoding},
  { @"EUC-JP",     NSJapaneseEUCStringEncoding},
  { @"Shift-JIS",  NSShiftJISStringEncoding},
  { @"MacRoman",   NSMacOSRomanStringEncoding},
  { @"CP1250",     NSWindowsCP1250StringEncoding},
  { @"CP1251",     NSWindowsCP1251StringEncoding},
  { @"CP1252",     NSWindowsCP1252StringEncoding},
  { @"CP1253",     NSWindowsCP1253StringEncoding},
  { @"CP1254",     NSWindowsCP1254StringEncoding},
  };
  unsigned int i;
  for (i= 0; i < sizeof(encodings)/sizeof(*encodings); i++)
    [popup addItemWithTitle:encodings[i].name];
  
  [caption setBordered:NO];
  [caption setStringValue:@"Text Encoding:"];
  [caption setEditable:NO];
  [caption setDrawsBackground:NO];
  [caption setAutoresizingMask:NSViewMinXMargin];
  [popup setAutoresizingMask:NSViewWidthSizable];
  [group addSubview:caption];
  [group addSubview:popup];
  
  [panel setAccessoryView:group];
  [panel setTitle:@"Open SQL Script"];
  
  if ([panel runModalForTypes:[NSArray arrayWithObjects:@"txt", @"sql", @"", nil]] == NSFileHandlingPanelOKButton)
  {
    NSData *data;
    NSString *text;
    NSString *filename= [panel filename];
        
    data= [[NSData alloc] initWithContentsOfFile:filename];
    if (!data)
    {
      NSRunAlertPanel(@"Error",
                      [NSString stringWithFormat:@"Could not open file '%@'.", filename],
                      @"OK",nil,nil);
      return;
    }
    
    if (!(text=[[NSString alloc] initWithData:data encoding:encodings[[popup indexOfSelectedItem]].encoding]))
    {
      NSRunAlertPanel(@"Error",
                      [NSString stringWithFormat:@"Could not convert file data to %@ encoding.", [popup titleOfSelectedItem]],
                      @"OK",nil,nil);
      [data release];
      return;
    }
    [data release];

    [(MQScriptTab*)[owner currentTab] setScript:text];
    [(MQScriptTab*)[owner currentTab] setDocumentEdited:NO];
    [(MQScriptTab*)[owner currentTab] setFilename:filename];
    
    [text release];
  }
}


- (void)openScriptDidEnd:(NSAlert*)alert returnCode:(int)returnCode contextInfo:(void*)info
{
  MQScriptTab *page= (MQScriptTab*)info;
  
  switch (returnCode)
  {
    case NSAlertDefaultReturn:
      [page saveScript:nil];
      if ([page isDocumentEdited]) // this means the Save was cancelled
        return;
      break;
    case NSAlertAlternateReturn:
      break;
    case NSAlertOtherReturn:
      return;
  }
  [self openScript];
}


- (IBAction)openScriptInCurrentEditor:(id)sender
{
  MQScriptTab *page= (MQScriptTab*)[owner currentTab];
  
  if ([page isDocumentEdited])
  {
    NSAlert *alert= [NSAlert alertWithMessageText:@"Save script before loading a new one in its place?"
                                    defaultButton:@"Save"
                                  alternateButton:@"Don't Save"
                                      otherButton:@"Cancel"
                        informativeTextWithFormat:@"The current tab sheet contains changes that will be lost if you do not save."];

    [alert beginSheetModalForWindow:[owner window]
                      modalDelegate:self
                     didEndSelector:@selector(openScriptDidEnd:returnCode:contextInfo:)
                        contextInfo:page];
  }
  else
    [self openScript];
}


- (IBAction)openScript:(id)sender
{
  [self setupEditorWithScript:@""];
  [self openScript];
}


- (IBAction)executeScript:(id)sender
{
  [[(MQScriptTab*)[owner currentTab] scriptHelper] executeScript:sender];
}

- (IBAction)stopScript:(id)sender
{
  [[(MQScriptTab*)[owner currentTab] scriptHelper] stopScript:sender];
}

- (IBAction)continueScript:(id)sender
{
  [[(MQScriptTab*)[owner currentTab] scriptHelper] continueScript:sender];
}

- (IBAction)pauseScript:(id)sender
{
  [[(MQScriptTab*)[owner currentTab] scriptHelper] pauseScript:sender];
}

- (IBAction)stepNextScript:(id)sender
{
  [[(MQScriptTab*)[owner currentTab] scriptHelper] stepScript:sender];
}

- (void)willShowPage:(id)page
{
  [animbox setImage:_idleImage];

  [bigToolbar setHidden:[owner smallToolbar]?YES:NO];
  [toolbar setHidden:[owner smallToolbar]?NO:YES];
  
  [owner setDocumentEdited:[page isDocumentEdited]];
  //XXX
//  [owner setTitleForDocumentName:[NSString stringWithFormat:[NSString stringWithUTF8String:"%@ — %@"],
//                                       [page documentName]?:@"Untitled", [page defaultSchema]]];
  
  [self updateToolbarState];

  // somehow, the dragged types is being lost when tabs are switched
  if ([[owner currentTab] isKindOfClass:[MQScriptTab class]])
    [[[[owner currentTab] scriptEditor] textView] registerForDraggedTypes:[NSArray arrayWithObject:MSchemaItemPboardType]];
}

- (void)didHidePage
{
  [toolbar setHidden:YES];
  [bigToolbar setHidden:YES];
}


- (void)setupEditorWithScript:(NSString*)script
{
  MQScriptTab *page;
  
  if (![[owner currentTab] isKindOfClass:[MQScriptTab class]]
      || [[[owner currentTab] script] length] > 0)
    [owner addScriptTab:nil];

  page= [owner currentTab];
  
  [page setScript:script];
}


- (void)scriptEditChanged:(NSNotification*)notif
{
  MQScriptTab *tab= [notif object];
  if (tab == [owner currentTab])
  {
    [owner setDocumentEdited:[tab isDocumentEdited]];
    
  //XXX  
//    [owner setTitleForDocumentName:[NSString stringWithFormat:[NSString stringWithUTF8String:"%@ — %@"],
//                                          [tab documentName]?:@"Untitled", [tab defaultSchema]]];
  }
}


- (void)scriptChangedState:(MQScriptExecutionHelper*)script state:(MQScriptState)state
{
  if ([[owner currentTab] scriptHelper] == script)
    [self updateToolbarState];
}


- (BOOL)stateOfMenuItem:(NSMenuItem*)item atIndex:(int)index inMenu:(NSMenu*)menu
{
  if ([[[NSApp mainMenu] itemAtIndex:[[NSApp mainMenu] indexOfItemWithSubmenu:menu]] tag]== 106)
  {
    MQScriptTab *page= [owner currentTab];
    MQScriptState state;
    BOOL flag= NO;
    
    if (![page isKindOfClass:[MQScriptTab class]])
    {
      [item setEnabled:NO];
      return YES;
    }
    state= [[page scriptHelper] state];

    switch (index)
    {
      case 0: // Execute Script
      case 1: // Execute Selection
      case 2: // Execute Stepping
        if (state == ScriptFinished || state == ScriptStopped || state == ScriptIdle)
          flag= YES;
        break;
      case 4: // Pause
        if (state == ScriptExecuting)
          flag= YES;
        break;
      case 5: // Continue
        if (state == ScriptError || state == ScriptBreakpoint || state == ScriptWaiting)
          flag= YES;
        break;
      case 6: // Step
        if (state == ScriptError || state == ScriptBreakpoint || state == ScriptWaiting)
          flag= YES;
        break;
      case 8: // Stop
        if (state != ScriptFinished && state != ScriptStopped && state != ScriptIdle)
          flag= YES;
        break;
    }
    [item setEnabled:flag];
  }
  return YES;
}

@end
