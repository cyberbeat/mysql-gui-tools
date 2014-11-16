//
//  MBoxLayout.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Fri Jul 23 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MBoxLayout.h"


@implementation MBoxLayout

- (id)initWithFrame:(NSRect)frame
{
  self= [super initWithFrame:frame];
  if (self)
  {
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(handleFrameChange:)
                                                 name:NSViewFrameDidChangeNotification
                                               object:self];
    [self setPostsFrameChangedNotifications:YES];
  }  
  return self;
}

- (id)init
{
  return [self initWithFrame:NSZeroRect];
}

- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [super dealloc];
}

- (void)handleFrameChange:(NSNotification*)notif
{
  [self updateLayout];
}

- (void)setVertical:(BOOL)vertical
{
  _vertical= vertical;
}

- (void)setLeftBorder:(float)leftWidth
            topBorder:(float)topWidth
          rightBorder:(float)rightWidth
         bottomBorder:(float)bottomWidth
{
  _topBorder= topWidth;
  _bottomBorder= bottomWidth;
  _leftBorder= leftWidth;
  _rightBorder= rightWidth;
}

- (void)setAllBorders:(float)width
{
  _topBorder= width;
  _bottomBorder= width;
  _leftBorder= width;
  _rightBorder= width;
}

- (void)setSpacing:(float)spacing
{
  _spacing= spacing;
}

- (void)pack:(NSView*)subview
      expand:(BOOL)expand
{
  [self pack:subview expand:expand minSize:1.0];
}

- (void)pack:(NSView*)subview
      expand:(BOOL)expand
     minSize:(float)minSize
{
  [self addSubview:subview];
  _subviewCount++;
  
  _flags= NSZoneRealloc(NSDefaultMallocZone(), _flags, sizeof(MBoxLayoutAttributes)*_subviewCount);
  _flags[_subviewCount-1].expand= expand;
  _flags[_subviewCount-1].minSize= minSize;
  _flags[_subviewCount-1].normalSize= [subview frame].size;
  
  [self updateLayout];
}

- (void)sizeToFit
{
  int i;
  float s;
  float totalSize;
  float maxOSize;
  NSRect myRect= [self frame];
  
#define SIZE(s) (_vertical ? (s).height : (s).width)
  totalSize= 0.0;
  s= 0.0;
  maxOSize= 0.0;
  for (i= 0; i < _subviewCount; i++)
  {
    MBoxLayoutAttributes *attr= _flags+i;
    if (_vertical)
    {
      totalSize+= attr->normalSize.height + s;
      maxOSize= MAX(attr->normalSize.width, maxOSize);
    }
    else
    {
      totalSize+= attr->normalSize.width + s;
      maxOSize= MAX(attr->normalSize.height, maxOSize);
    }
    s= _spacing;
  }
  if (_vertical)
  {
    myRect.size.height= totalSize + _topBorder+_bottomBorder;
    myRect.size.width= maxOSize + _leftBorder+_rightBorder;
  }
  else
  {
    myRect.size.width= totalSize + _leftBorder+_rightBorder;
    myRect.size.height= maxOSize + _topBorder + _bottomBorder;
  }
  [self setFrame:myRect];
}


- (void)resizeSubviewsWithOldSize:(NSSize)osize
{
  NSLog(@"resize subviews");
}

- (void)updateLayout
{
  int i;
  NSArray *children= [self subviews];
  int expandableViews;
  float totalSize, minSize, slackSize;
  NSRect myRect= [self frame];
  float mySize= _vertical ? myRect.size.height : myRect.size.width;
  float myAvailSize;
  float p, s;
  
  NSLog(@"update layout %@", NSStringFromSize(myRect.size));

  myAvailSize= mySize - (_subviewCount-1) * _spacing;
  if (_vertical)
    myAvailSize-= _topBorder+_bottomBorder;
  else
    myAvailSize-= _leftBorder+_rightBorder;

  expandableViews= 0;
  
  totalSize= 0.0;
  minSize= 0.0;
  for (i= 0; i < _subviewCount; i++)
  {
    id child= [children objectAtIndex:i];
    MBoxLayoutAttributes *attr= _flags+i;
    NSRect rect;
    
    rect= [child frame];
    
    totalSize+= SIZE(attr->normalSize);
    minSize+= attr->minSize;
    
    if (attr->expand)
      expandableViews++;
  }
  if (totalSize <= myAvailSize)
    slackSize= (myAvailSize - totalSize);
  else
    slackSize= (myAvailSize - minSize);

  if (_vertical)
    p= myRect.size.height - _topBorder;
  else
    p= _leftBorder;
  for (i= 0; i < _subviewCount; i++)
  {
    id child= [children objectAtIndex:i];
    MBoxLayoutAttributes *attr= _flags+i;
    NSRect rect= [child frame];

    if (slackSize < 0)
      s= attr->minSize;
    else
    {
      if (attr->expand)
      {
        if (totalSize <= myAvailSize)
          s= SIZE(attr->normalSize) + (slackSize / expandableViews);
        else
          s= attr->minSize + (slackSize / expandableViews);
      }
      else
        s= SIZE(attr->normalSize);
    }
    if (_vertical)
    {
      rect.origin.x= _leftBorder;
      rect.origin.y= p - s;
      rect.size.width= myRect.size.width - (_leftBorder + _rightBorder);
      rect.size.height= s;
    }
    else
    {
      rect.origin.x= p;
      rect.origin.y= _bottomBorder;
      rect.size.width= s;
      rect.size.height= myRect.size.height - (_topBorder + _bottomBorder);
    }

    [child setFrame:rect];
    [child setNeedsDisplay:YES];
    if (_vertical)
      p-= s + _spacing;
    else
      p+= s + _spacing;
  }
  [self setNeedsDisplay:YES];
}

@end
