//
//  MATimeGraph.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Thu Jul 22 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <AppKit/AppKit.h>
#import "MAMenuContextView.h"

typedef struct {
  double value;
  time_t ts;
} MATimeGraphSample;

@interface MATimeGraph : MAMenuContextView {
  MATimeGraphSample *_samples;
  int _maxSamples;
  int _sampleCount;
  
  double _statMin;
  double _statMax;
  double _statAvg;
  
  NSBezierPath *_grid;
  NSBezierPath *_graph;
  time_t _lastTime;
  int _gridOffset;
  
  int _pixelsPerSecond;
  float _gridWidth;
  
  float _rangeMin;
  float _rangeMax;

  NSDictionary *_attr;
  NSString *_caption;
  
  NSColor *_gridColor;
  NSColor *_graphColor;
}

- (void)getStatsMin:(double*)min max:(double*)max average:(double*)avg;

- (void)setRangeMin:(float)min max:(float)max;

- (void)addValue:(double)value
       timestamp:(time_t)ts;
- (void)setCaption:(NSString*)caption;

- (void)updateGraph;

@end
