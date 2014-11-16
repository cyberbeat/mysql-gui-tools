//
//  MAHealthGraph.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Thu Jul 22 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>
#include "myx_admin_public_interface.h"

#import "MAMenuContextView.h"

@class MATimeGraph;

@interface MAHealthGraph : MAMenuContextView
{
@protected  
  NSDictionary *_props;

  MYX_COMPILED_EXPRESSION *_expr;
  MYX_VARIABLES *_oldValues;
  MYX_VARIABLES *_values;
  MYX_VARIABLES *_serverValues;
  BOOL _serverValuesChanged;
}

- (void)setProperties:(NSDictionary*)props;
- (NSDictionary*)properties;

- (void)updateServerVariables:(MYX_VARIABLES*)vars;
- (void)updateValues:(MYX_VARIABLES*)vars;

- (double)evaluate:(NSString*)s;

- (NSString*)prepareExpression:(NSString*)expr
           serverVariablesOnly:(BOOL)serverOnly;
@end

@class MAMeterGraph;

@interface MALineHealthGraph : MAHealthGraph
{
  MATimeGraph *_graph;
  NSMatrix *_infoMatrix;
  double _max;
  
  NSCell *_cells[4];
  
  MAMeterGraph *_meter;
}

- (id)initWithProperties:(NSDictionary*)props;
- (void)setMeterImage:(NSImage*)image;
- (void)setProperties:(NSDictionary*)properties;

- (void)setMenu:(NSMenu*)menu;

@end


@interface MABarHealthGraph : MAHealthGraph
{
  NSImage *_images[2];
  
  double _value;
  double _max;
}

- (id)initWithProperties:(NSDictionary*)props;
- (void)setProperties:(NSDictionary*)properties;

@end
