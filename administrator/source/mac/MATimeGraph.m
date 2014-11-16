//
//  MATimeGraph.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Thu Jul 22 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MATimeGraph.h"
#include <glib.h>

@implementation MATimeGraph

- (id)initWithFrame:(NSRect)frame 
{
    self = [super initWithFrame:frame];
    if (self) 
    {
      _gridColor= [[NSColor colorWithDeviceRed:0.162 green:0.288 blue:0.358 alpha:0.8] retain];
      _graphColor= [[NSColor colorWithDeviceRed:0.2 green:0.6 blue:1.0 alpha:1.0] retain];

      _maxSamples= 1024;
      _samples= g_new(MATimeGraphSample, _maxSamples);
      
      _attr= [[NSDictionary alloc] init];
      _caption= nil;
      _rangeMin= 0;
      _rangeMax= 100;
      _gridWidth= 20;
      _pixelsPerSecond= 1;
    }
    return self;
}

- (void)dealloc
{
  g_free(_samples);
  [_attr release];
  [_caption release];
  [_grid release];
  [_graph release];
  [_gridColor release];
  [_graphColor release];
  [super dealloc];
}


- (void)setRangeMin:(float)min max:(float)max
{
  _rangeMin= min;
  _rangeMax= max;
}

- (void)drawRect:(NSRect)rect 
{
  [[NSColor blackColor] set];
  NSRectFill(rect);

  [_gridColor set];
  [_grid stroke];
  
  [_graphColor set];
  [_graph stroke];

  if (_caption)
  {
    [_caption drawAtPoint:NSMakePoint(5,3) 
           withAttributes:[NSDictionary dictionaryWithObjectsAndKeys:[NSColor whiteColor],NSForegroundColorAttributeName,
             [NSFont systemFontOfSize:9.0], NSFontAttributeName,nil]];
  }
}


- (void)addValue:(double)value
       timestamp:(time_t)ts
{
  MATimeGraphSample sample;

  sample.value= value;
  sample.ts= ts;
  if (_sampleCount < _maxSamples)
    _sampleCount++;
  memmove(_samples+1, _samples, sizeof(*_samples)*(_sampleCount-1));
  _samples[0]= sample;
 
  [self updateGraph];
  [self setNeedsDisplay:YES];
}


- (void)setCaption:(NSString*)caption
{
  if (_caption != caption)
  {
    [_caption release];
    _caption= [caption retain];
  }
}
  
  
- (void)updateGraph
{
  int parts;
  int i;
  time_t time0;
  NSSize size= [self frame].size;
  float x,y;
  float gridHeight;
  time_t prevTime;

  if (_grid) [_grid release];
  if (_graph) [_graph release];
  _grid= [[NSBezierPath alloc] init];
  [_grid setLineWidth:0.0];
  _graph= [[NSBezierPath alloc] init];
  [_graph setLineWidth:0.0];

  
  gridHeight= (_rangeMax-_rangeMin)/5;
  
  
  // horiz grid
  parts= (_rangeMax - _rangeMin) / gridHeight;
  for (i= 0; i < parts; i++)
  {
    [_grid moveToPoint:NSMakePoint(1,(size.height/parts)*i+0.5)];
    [_grid lineToPoint:NSMakePoint(size.width-1,(size.height/parts)*i+0.5)];
  }
  
  if (_sampleCount==0)
    time0= 0;
  else
    time0= _samples[0].ts;
  
  // vert grid    
  if (_lastTime != time0)
  {
    if (_lastTime > 0)
    {
      _gridOffset+= (int)((time0-_lastTime)*_pixelsPerSecond);
    }
    _gridOffset%= (int)_gridWidth;
    _lastTime= time0;
  }
  for (i= size.width-1-_gridOffset; i > 0; i-= _gridWidth)
  {
    [_grid moveToPoint:NSMakePoint(i+0.5, 1)];
    [_grid lineToPoint:NSMakePoint(i+0.5, size.height-2)];
  }
  
  // graph
  x= size.width-2;
  prevTime= 0;
  [_graph moveToPoint:NSMakePoint(x, 2)];
  for (i= 0; i < _sampleCount; i++)
  {
    double xdelta;
    
    xdelta= (prevTime ? (_samples[i].ts - prevTime) : 0.0);
    
    x = x + xdelta * _pixelsPerSecond;
    y= 1 + ((_samples[i].value - _rangeMin) * (size.height-2)) / (_rangeMax - _rangeMin);
    
    if (y < 1)
      y= 1;
    if (x < 0)
      break;
    [_graph lineToPoint:NSMakePoint(x,y)];
    prevTime= _samples[i].ts;
  }
  if (x > 0)
  {
    // start at 0
    x -= 1;
    y = 0.0;
    [_graph lineToPoint:NSMakePoint(x,y)];
  }
}

- (void)getStatsMin:(double*)min max:(double*)max average:(double*)avg
{
  int i;
  double mn, mx, av;
  mn= 0;
  mx= 0;
  av= 0;
  for (i= 0; i < _sampleCount; i++)
  {
    if (mn > _samples[i].value)
      mn= _samples[i].value;
    if (mx < _samples[i].value)
      mx= _samples[i].value;
    av+= _samples[i].value;
  }
  if (_sampleCount > 0)
    av/= _sampleCount;
  else
    av= 0;
  *min= mn;
  *max= mx;
  *avg= av;
}

@end
