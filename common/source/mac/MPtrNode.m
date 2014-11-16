//
//  MPtrNode.m
//  MySQLGUICommon
//
//  Copyright (c) 2005 MySQL AB. All rights reserved.
//

#import "MPtrNode.h"


@implementation MPtrNode

- (void)dealloc
{
  if (_pointer && _destroyer)
    (*_destroyer)(_pointer);
  [_caption release];
  [super dealloc];
}

- (void)setCaption:(NSString*)caption
{
  if (_caption != caption)
  {
    [_caption release];
    _caption= [caption retain];
  }
}

- (NSString*)caption
{
  return _caption;
}

- (void)setPointer:(void*)ptr
{
  _pointer= ptr;
}

- (void)setPointer:(void*)ptr destroyer:(MPtrFree)destroyer
{
  _pointer= ptr;
  _destroyer= destroyer;
}

- (void*)pointer
{
  return _pointer;
}

- (int)tag
{
  return _tag;
}

- (void)setTag:(int)tag
{
  _tag= tag;
}

- (void) setSubnodes: (NSMutableArray *)subnodes;
{
  _subnodes= subnodes;
}

- (unsigned) subnodeCount
{
  return [_subnodes count];
}

- (NSArray *)subnodes
{
  return _subnodes;
}


@end
