//
//  MQActionBar.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/9.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQActionBar.h"
#import "MQDropActionView.h"
#import <MySQLToolsCommon/MSchemaDataSource.h>

@implementation MQActionBar

- (void)setup
{
  MQDropActionView *view;
  NSFont *font= [NSFont systemFontOfSize:[NSFont smallSystemFontSize]];
  NSRect rect;
  float x= 0;
  
  rect.origin.y= 0;
  rect.size.height= 20;
  rect.origin.x= 0;
  x+= rect.size.width= [font widthOfString:@"SELECT"] + 16;
  view= [[[MQDropActionView alloc] initWithFrame:rect] autorelease];
  [view setFont:font];
  [view setTarget:self];
  [view setText:@"SELECT"];
  [view setDropAction:@selector(handleSelect:item:)];
  [self addSubview:view];

  rect.origin.y= 0;
  rect.size.height= 20;
  rect.origin.x= x;
  x+= rect.size.width= [font widthOfString:@"Add Table"] + 16;  
  view= [[[MQDropActionView alloc] initWithFrame:rect] autorelease];
  [view setFont:font];
  [view setTarget:self];
  [view setText:@"Add Table"];
  [view setDropAction:@selector(handleAddTable:item:)];
  [self addSubview:view];

  rect.origin.y= 0;
  rect.size.height= 20;
  rect.origin.x= x;
  x+= rect.size.width= [font widthOfString:@"JOIN Table"] + 16;  
  view= [[[MQDropActionView alloc] initWithFrame:rect] autorelease];
  [view setFont:font];
  [view setTarget:self];
  [view setText:@"JOIN Table"];
  [view setDropAction:@selector(handleJoin:item:)];
  [self addSubview:view];

  rect.origin.y= 0;
  rect.size.height= 20;
  rect.origin.x= x;
  x+= rect.size.width= [font widthOfString:@"L.OUTER JOIN"] + 16;  
  view= [[[MQDropActionView alloc] initWithFrame:rect] autorelease];
  [view setFont:font];
  [view setTarget:self];
  [view setText:@"L.OUTER JOIN"];
  [view setDropAction:@selector(handleLOJoin:item:)];
  [self addSubview:view];

  rect.origin.y= 0;
  rect.size.height= 20;
  rect.origin.x= x;
  x+= rect.size.width= [font widthOfString:@"INSERT"] + 16;  
  view= [[[MQDropActionView alloc] initWithFrame:rect] autorelease];
  [view setFont:font];
  [view setTarget:self];
  [view setText:@"INSERT"];
  [view setDropAction:@selector(handleInsert:item:)];
  [self addSubview:view];

  rect.origin.y= 0;
  rect.size.height= 20;
  rect.origin.x= x;
  x+= rect.size.width= [font widthOfString:@"UPDATE"] + 16;  
  view= [[[MQDropActionView alloc] initWithFrame:rect] autorelease];
  [view setFont:font];
  [view setTarget:self];
  [view setText:@"UPDATE"];
  [view setDropAction:@selector(handleUpdate:item:)];
  [self addSubview:view];

  rect.origin.y= 0;
  rect.size.height= 20;
  rect.origin.x= x;
  x+= rect.size.width= [font widthOfString:@"DELETE"] + 16;
  view= [[[MQDropActionView alloc] initWithFrame:rect] autorelease];
  [view setFont:font];
  [view setTarget:self];
  [view setText:@"DELETE"];
  [view setDropAction:@selector(handleDelete:item:)];
  [self addSubview:view];
  
  [self setFrame:NSMakeRect(0, 0, x, 20)];
}

- (id)init
{
  self = [super init];
  if (self)
  {
    _actions= [[NSMutableArray alloc] init];
    [self setup];
  }
  return self;
}


- (void)setTextView:(NSTextView*)textView
{
  _textView= textView;
}


- (void)setMySQL:(MYSQL*)mysql
{
  _mysql= mysql;
}


- (void)setupQueryWithItem:(MSchemaItem*)item ofType:(MYX_Q_TABLE_ADD_TYPE)type
{
  int cursor;
  MYX_Q_TABLE_ADD_ERROR error;
  char *query;
  query= myx_query_add_table_to_sql(_mysql,
                                    _mysql->db,
                                    [item schema]->catalog_name,
                                    [item schema]->schema_name,
                                    [item table]->table_name,
                                    g_strdup([[_textView string] UTF8String]),
                                    type,
                                    &cursor,
                                    &error);
  if (query)
  {
    [_textView setString:[NSString stringWithUTF8String:query]];
    [_textView setSelectedRange:NSMakeRange(cursor, 0)];
    [[_textView window] makeFirstResponder:_textView];
    g_free(query);  
  }
}


- (void)handleSelect:(MQDropActionView*)av item:(MSchemaItem*)item
{
  if (item)
    [self setupQueryWithItem:item ofType:MYX_QTAT_SELECT];
  else
    [_textView setString:_originalQuery];
}

- (void)handleAddTable:(MQDropActionView*)av item:(MSchemaItem*)item
{
  if (item)
    [self setupQueryWithItem:item ofType:MYX_QTAT_SELECT_ADD];
  else
    [_textView setString:_originalQuery];

}

- (void)handleJoin:(MQDropActionView*)av item:(MSchemaItem*)item
{
  if (item)
    [self setupQueryWithItem:item ofType:MYX_QTAT_SELECT_JOIN];
  else
    [_textView setString:_originalQuery];
}


- (void)handleLOJoin:(MQDropActionView*)av item:(MSchemaItem*)item
{
  if (item)
    [self setupQueryWithItem:item ofType:MYX_QTAT_SELECT_LEFT_OUTER_JOIN];
  else
    [_textView setString:_originalQuery];
}


- (void)handleInsert:(MQDropActionView*)av item:(MSchemaItem*)item
{
  if (item)
    [self setupQueryWithItem:item ofType:MYX_QTAT_INSERT];
  else
    [_textView setString:_originalQuery];
}


- (void)handleUpdate:(MQDropActionView*)av item:(MSchemaItem*)item
{
  if (item)
    [self setupQueryWithItem:item ofType:MYX_QTAT_UPDATE];
  else
    [_textView setString:_originalQuery];
}


- (void)handleDelete:(MQDropActionView*)av item:(MSchemaItem*)item
{
  if (item)
    [self setupQueryWithItem:item ofType:MYX_QTAT_DELETE];
  else
    [_textView setString:_originalQuery];
}


- (void)popupAt:(NSPoint)pos
{
  _originalQuery= [[NSString stringWithString:[_textView string]] retain];
  [self setFrameOrigin:pos];
  [self setHidden:NO];
  if (_timer)
  {
    [_timer invalidate];
    _timer= nil;
  }
}


- (void)show
{
  if (_timer)
  {
    [_timer invalidate];
    _timer= nil;
  }
}


- (void)doHide:(id)arg
{
  [[self superview] setNeedsDisplayInRect:[self frame]];
  [self setHidden:YES];
  _timer= nil;
}


- (void)hide
{
  [self doHide:nil];
}


- (void)delayedHide
{
  if (!_timer)
  {
    _timer= [NSTimer scheduledTimerWithTimeInterval:0.3
                                             target:self
                                           selector:@selector(doHide:)
                                           userInfo:nil
                                            repeats:NO];
  }
}

@end
