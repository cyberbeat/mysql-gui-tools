//
//  MQPreferences.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/14.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQPreferences.h"
#import "MQHistory.h"

@implementation MQPreferences

- (void)willShow
{
  NSUserDefaults *defaults= [NSUserDefaults standardUserDefaults];
  NSFont *font= [NSUnarchiver unarchiveObjectWithData:[defaults objectForKey:@"EditorFont"]];

  [fontText setStringValue:[NSString stringWithFormat:@"%@ %.0f", [font fontName], [font pointSize]]];
}


- (IBAction)modifyFont:(id)sender
{
  NSFontManager *manager= [NSFontManager sharedFontManager];
  [[view window] makeFirstResponder:self];
  [manager setSelectedFont:[NSUnarchiver unarchiveObjectWithData:[[NSUserDefaults standardUserDefaults] objectForKey:@"EditorFont"]] isMultiple:NO];

  [manager orderFrontFontPanel:nil];
}


-(void)changeFont:(id)sender
{
  NSFontManager *manager= [NSFontManager sharedFontManager];
  NSFont *font= [manager convertFont:[manager selectedFont]];
  NSUserDefaults *defaults= [NSUserDefaults standardUserDefaults];
  
  [fontText setStringValue:[NSString stringWithFormat:@"%@ %.0f", [font fontName], [font pointSize]]];
  
  [defaults setObject:[NSArchiver archivedDataWithRootObject:font] forKey:@"EditorFont"];
}


- (IBAction)resetHistory:(id)sender
{
  if (NSRunAlertPanel(@"Reset Query History?",
                      @"Resetting the query history will permanently remove all stored history entries.\nBookmarks will not be affected.",
                      @"Reset",@"Cancel",nil) == NSAlertDefaultReturn)
  {
    [_history reset];
  }
}


- (void)setHistory:(MQHistory*)history
{
  _history= history;
}

@end
