//
//  MQHelpTab.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/5.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQHelpTab.h"


@implementation MQHelpTab

- (id)initWithIdentifier:(id)ident
{
  self= [super initWithIdentifier:ident];
  if (self)
  {
    [self setLabel:@"Help"];
    [self setIcon:[NSImage imageNamed:@"tabsheet_icon_inlinehelp.png"]];
    [NSBundle loadNibNamed:@"HelpTab" owner:self];
  }
  return self;
}

- (void)showQuickStartHelp
{
  NSString *url= [[NSBundle mainBundle] pathForResource:@"mysqlqb_quickstart"
                                                 ofType:@"html"];
  if ([url isEqualTo:_currentFile])
    return;
  [_currentFile release];
  _currentFile= [url retain];

  url= [NSString stringWithFormat:@"file://%@",[url stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding]];
  
  [[wview mainFrame] loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:url]]];
}


- (void)loadFile:(NSString*)file topic:(NSString*)topic
{
  NSString *url= [[NSBundle mainBundle] pathForResource:file
                                                 ofType:@"html"];
  if ([url isEqualTo:_currentFile])
  {
//    [[wview mainFrame] loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"file:#%@",topic]]]];
//    return;
  }
  [_currentFile release];
  _currentFile= [url retain];
  
  url= [NSString stringWithFormat:@"file://%@#%@",[url stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding], topic];
  
  [[wview mainFrame] loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:url]]];
}

@end
