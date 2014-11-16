//
//  MAHealthGraph.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Thu Jul 22 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MAHealthGraph.h"
#import <MySQLToolsCommon/NSString_extras.h>

#import "MATimeGraph.h"
 
#define GRAPH_HEIGHT 80
#define METER_GRAPH_WIDTH 65
#define GRAPH_SPACING 5
#define STATUS_HEIGHT 16

@implementation MAHealthGraph 

- (void)dealloc
{
  [_props release];
  [super dealloc];
}

- (void)setProperties:(NSDictionary*)properties
{
  if (_props != properties)
  {
    [_props release];
    _props= [properties retain];
  }
}


- (NSDictionary*)properties
{
  return _props;
}


- (void)updateServerVariables:(MYX_VARIABLES*)vars
{
  _serverValues= vars;
  _serverValuesChanged= YES;
}

- (void)updateValues:(MYX_VARIABLES*)vars
{
  // implemented by subclasses
}

- (double)evaluate:(NSString*)s
{
  MYX_COMPILED_EXPRESSION *expr;
  MYX_EXPRESSION_ERROR err;
  double res;
  
  expr= myx_compile_expression([[self prepareExpression:s serverVariablesOnly:NO] UTF8String], &err);
  if (!expr)
    return 0.0;
  res= myx_eval_expression(expr, _oldValues, _values, &err);
  myx_free_expression(expr);
  
  return res;
}

- (NSString*)prepareExpression:(NSString*)expr
           serverVariablesOnly:(BOOL)serverOnly
{
  NSMutableString *s= [[[NSMutableString alloc] init] autorelease];
  unsigned int i;
  
  [s setString:expr];
  
  if (_values && !serverOnly)
  {
    // replace status variables with their indices
    for (i= 0; i < _values->variables_num; i++)
    {
      [s replaceOccurrencesOfString:[NSString stringWithFormat:@"[%s]",(char*)_values->variables[i].name]
                         withString:[NSString stringWithFormat:@"[%i]",i]
                            options:NSCaseInsensitiveSearch
                              range:NSMakeRange(0,[s length])];
    }
  }

  // replace server variable static values
  if (_serverValues)
  {
    for (i= 0; i < _serverValues->variables_num; i++)
    {
      [s replaceOccurrencesOfString:[NSString stringWithFormat:@"[%s]",(char*)_serverValues->variables[i].name]
                         withString:[NSString stringWithFormat:@"%s",(char*)_serverValues->variables[i].value?:"0"]
                            options:NSCaseInsensitiveSearch
                              range:NSMakeRange(0,[s length])];
    }
  }
  return s;
}

@end

//======================================================================

@interface MAMeterGraph : MAMenuContextView
{
  NSImage *_image;
  float _value;
  NSString *_format;
}
- (void)setImage:(NSImage*)image;
- (void)setValue:(float)value;
- (void)setCaptionFormat:(NSString*)format;
@end

@implementation MAMeterGraph
- (void)dealloc
{
  [_format release];
  [_image release];
  [super dealloc];
}

- (void)setImage:(NSImage*)image
{
  if (_image != image)
  {
    [_image release];
    _image= [image retain];
  }
}

- (void)setCaptionFormat:(NSString*)format
{
  if (_format != format)
  {
    [_format release];
    _format= [format retain];
  }
}

- (void)setValue:(float)value
{
  _value= value;
  [self setNeedsDisplay:YES];
}

- (void)drawRect:(NSRect)aRect
{
  NSRect rect;
  NSRect frame= [self frame];
  NSSize isize;
  NSPoint pos;
  NSString *cap;
  NSFont *font;
  float y;
  int i;
  
  [[NSColor blackColor] set];
  NSRectFill(aRect);
  
  rect.origin= NSMakePoint(0,0);
  isize= rect.size= [_image size];
  
  pos= NSMakePoint((frame.size.width - rect.size.width)/2,
                   10 + (frame.size.height - rect.size.height)/2);
  y= pos.y;

  [_image dissolveToPoint:pos
                 fromRect:rect
                 fraction:0.2];

  rect.origin= NSMakePoint(0,0);
  rect.size= [_image size];
  rect.size.height = ceil(((ceil(_value)*rect.size.height)/10)*10/100.0);
  [_image dissolveToPoint:pos
                 fromRect:rect
                 fraction:1.0];

  font= [NSFont systemFontOfSize:8];
  cap= [NSString stringWithFormat:_format,_value];
  pos.x= (frame.size.width - [font widthOfString:cap])/2;
  pos.y= 5;

  [cap drawAtPoint:pos 
    withAttributes:[NSDictionary dictionaryWithObjectsAndKeys:[NSColor whiteColor],NSForegroundColorAttributeName,
      font, NSFontAttributeName,nil]];
  
  [[NSColor blackColor] set];
  for (i= 0; i < 10; i++)
  {
    [NSBezierPath strokeLineFromPoint:NSMakePoint(0,y+isize.height/10*i+0.5)
                               toPoint:NSMakePoint(frame.size.width,y+isize.height/10*i+0.5)];
  }
}

@end

// ============================================================================

@implementation MALineHealthGraph 

- (id)initWithProperties:(NSDictionary*)properties
{
  self= [super init];
  if (self)
  {
    NSTextFieldCell *cell;
    int i;

	[self setAutoresizingMask:NSViewWidthSizable];
	[self setFrame:NSMakeRect(0, 0, 100+METER_GRAPH_WIDTH+GRAPH_SPACING, GRAPH_HEIGHT+STATUS_HEIGHT)];

    _meter= [[MAMeterGraph alloc] init];
    [_meter setFrame:NSMakeRect(0,STATUS_HEIGHT,METER_GRAPH_WIDTH,GRAPH_HEIGHT)];
    [_meter setAutoresizingMask:NSViewMaxXMargin];

    _graph= [[MATimeGraph alloc] init];
    [_graph setFrame:NSMakeRect(METER_GRAPH_WIDTH+GRAPH_SPACING,STATUS_HEIGHT,
								100,GRAPH_HEIGHT)];
    [_graph setAutoresizingMask:NSViewWidthSizable];
    
    cell= [[[NSTextFieldCell alloc] initTextCell:@""] autorelease];
    [cell setFont:[NSFont systemFontOfSize:[NSFont smallSystemFontSize]]];
    [cell setBordered:YES];
    [cell setDrawsBackground:YES];

    _infoMatrix= [[NSMatrix alloc] initWithFrame:NSMakeRect(METER_GRAPH_WIDTH+GRAPH_SPACING, 0, 
															100, STATUS_HEIGHT)
                                            mode:NSTrackModeMatrix
                                       prototype:cell
                                    numberOfRows:1
                                 numberOfColumns:4];
    [_infoMatrix setAutosizesCells:YES];
    for (i= 0; i < 4; i++)
      _cells[i]= [_infoMatrix cellAtRow:0 column:i];

    [_infoMatrix setAutoresizingMask:NSViewWidthSizable];
    [_infoMatrix setIntercellSpacing:NSMakeSize(0,0)];

    [self addSubview:_meter];
    [self addSubview:_graph];
    [self addSubview:_infoMatrix];

    [_graph release];
    [_meter release];
    [_infoMatrix release];

    [self setAttachedObject:self];
    [_meter setAttachedObject:self];
    [_graph setAttachedObject:self];

    [self setProperties: properties];
  }
  
  return self;
}


- (void)setMenu:(NSMenu*)menu
{
  [super setMenu:menu];
  [_meter setMenu:menu];
  [_graph setMenu:menu];
  [_infoMatrix setMenu:menu];
}


- (void)setMeterImage:(NSImage*)image
{
  [_meter setImage:[image retain]];
}

- (void)setProperties:(NSDictionary*)properties
{
  NSRect base= [self frame];
  
  [super setProperties:properties];

  if ([properties objectForKey:@"valueCaption"])
  {
    [_meter setHidden:NO];
    [_graph setFrame:NSMakeRect(METER_GRAPH_WIDTH+GRAPH_SPACING, [_graph frame].origin.y, base.size.width-(METER_GRAPH_WIDTH+GRAPH_SPACING), GRAPH_HEIGHT)];
    [_infoMatrix setFrame:NSMakeRect(METER_GRAPH_WIDTH+GRAPH_SPACING, [_infoMatrix frame].origin.y, base.size.width-(METER_GRAPH_WIDTH+GRAPH_SPACING), STATUS_HEIGHT)];
  }
  else
  {
    [_meter setHidden:YES];
    [_graph setFrame:NSMakeRect(0, [_graph frame].origin.y, base.size.width, GRAPH_HEIGHT)];
    [_infoMatrix setFrame:NSMakeRect(0, [_infoMatrix frame].origin.y, base.size.width, STATUS_HEIGHT)]; 
  }

  if (_meter)
    [_meter setCaptionFormat:
      [NSString stringWithFormat:@"%@ %%.01f%%", [_props objectForKey:@"valueCaption"]]];

  if ([[_props objectForKey:@"showTitle"] boolValue])
    [_graph setCaption:[_props objectForKey:@"title"]];
  else
    [_graph setCaption:nil];

  _max= [[_props objectForKey:@"maxValue"] doubleValue];
  [_graph setRangeMin:[[_props objectForKey:@"minValue"] doubleValue]
                  max:_max];

  if (_expr)
  {
    myx_free_expression(_expr);
    _serverValuesChanged= YES;
  }
  _expr= NULL;
  
  [self setNeedsDisplay:YES];
}


- (void)updateValues:(MYX_VARIABLES*)vars
{
  MYX_EXPRESSION_ERROR err;

  _oldValues= _values;
  _values= vars;
      
  if (_serverValuesChanged)
  {
    NSString *expr= [self prepareExpression:[_props objectForKey:@"valueFormula"]
                        serverVariablesOnly:NO];
    if (_expr)
      myx_free_expression(_expr);

    _expr= myx_compile_expression([expr UTF8String], &err);
    if (!_expr)
    {
      NSLog(@"could not compile expression %@", expr);
    }
    _serverValuesChanged= NO;
  }
  
  if (_expr && _oldValues)
  {
    double value;
    value= myx_eval_expression(_expr, _oldValues, _values, &err);
    if (err != MYX_EXPRESSION_NO_ERROR)
    {
      value= 0.0;
      //NSLog(@"Expression error: %i (%@)", err, [_props objectForKey:@"valueFormula"]);
    }
	
	if ([_props objectForKey:@"maxFormula"])
    {
      _max= [self evaluate:[_props objectForKey:@"maxFormula"]];
      [_graph setRangeMin:[[_props objectForKey:@"minValue"] doubleValue]
                      max:_max];
    }
	
    if (value > _max && [[_props objectForKey:@"autoExtend"] boolValue])
    {
      _max= value;
      [_graph setRangeMin:[[_props objectForKey:@"minValue"] doubleValue]
                      max:_max];
    }
	[_graph addValue:value timestamp:time(NULL)];
    if (_meter)
    {
      [_meter setValue:((value-[[_props objectForKey:@"minValue"] doubleValue])*100/_max)];
    }

    {
      double max, min, avg;
      [_graph getStatsMin:&min max:&max average:&avg];
      [_cells[0] setStringValue:[NSString stringWithFormat:@"Current: %.0f", value]];
      [_cells[1] setStringValue:[NSString stringWithFormat:@"Maximal: %.0f", max]];
      [_cells[2] setStringValue:[NSString stringWithFormat:@"Minimal: %.0f", min]];
      [_cells[3] setStringValue:[NSString stringWithFormat:@"Average: %.0f", avg]];
    }
  }
}

@end

// ============================================================================

@implementation MABarHealthGraph

- (id)initWithProperties:(NSDictionary*)props
{
  self= [super init];
  if (self)
  {
	_images[0]= [[NSImage imageNamed:@"health_bar_empty.png"] retain];
	_images[1]= [[NSImage imageNamed:@"health_bar_filled.png"] retain];
	
	[self setFrameSize:NSMakeSize(100.0, [_images[0] size].height+2.0)];

	[self setAttachedObject:self];
	
	[self setProperties:props];
  }
  return self;
}

- (void)dealloc
{
  [_images[0] release];
  [_images[1] release];
  [super dealloc];
}


- (void)setProperties:(NSDictionary*)properties
{
  _max= [[_props objectForKey:@"maxValue"] doubleValue];
  
  [super setProperties:properties];
}


- (void)drawRect:(NSRect)rect 
{
  NSSize size= [self frame].size;
  float x;
  NSFont *font= [NSFont systemFontOfSize:10.0];
  NSMutableDictionary *attr= [NSMutableDictionary dictionaryWithObjectsAndKeys:
	[NSColor blackColor], NSForegroundColorAttributeName,
	font, NSFontAttributeName,
	nil];
  NSString *str;
  float xoffs= 0.0;
  NSRect imgrect;
  
  /*
  if ([[_props objectForKey:@"showTitle"] boolValue])
  {
	NSRect r= rect;
	str= [_props objectForKey:@"title"];
	xoffs= [font widthOfString:str]+5.0;
	NSDrawWindowBackground(r);
	[str drawAtPoint:NSMakePoint(0.0, 3.0) withAttributes:attr];
  }
   */  
  [attr setObject:[NSColor whiteColor] forKey:NSForegroundColorAttributeName];
  
  [[NSColor blackColor] set];
  rect.origin.x= xoffs;
  NSRectFill(rect);
  
  imgrect.origin= NSMakePoint(0.0,0.0);
  imgrect.size= [_images[0] size];

  for (x= 0.5; x < (size.width-2.0) * _value / _max; x+= imgrect.size.width)
  {
	[_images[1] drawAtPoint:NSMakePoint(x, 1.0)
				   fromRect:imgrect
				  operation:NSCompositeCopy
				   fraction:1.0];	
  }
  for (x= ceil((size.width-2.0) * _value / _max); x < size.width-2.0; x+= imgrect.size.width)
  {
	[_images[0] drawAtPoint:NSMakePoint(x, 1.0)
				   fromRect:imgrect
				  operation:NSCompositeCopy
				   fraction:1.0];
  }
  
  str= [NSString stringWithFormat:@"%@ %@", [_props objectForKey:@"valueCaption"],
	[NSString stringWithMultNumber:(long long)_value]];
  [str drawAtPoint:NSMakePoint(5.0+xoffs, 2.0) withAttributes:attr];

  str= [NSString stringWithFormat:@"%@ %@", [_props objectForKey:@"maxCaption"],
	[NSString stringWithMultNumber:(long long)_max]];
  [str drawAtPoint:NSMakePoint(size.width-[font widthOfString:str]-5.0, 2.0)
	withAttributes:attr];
}


- (void)updateValues:(MYX_VARIABLES*)vars
{
  MYX_EXPRESSION_ERROR err;
  
  _oldValues= _values;
  _values= vars;
  
  if (_serverValuesChanged)
  {
    NSString *expr= [self prepareExpression:[_props objectForKey:@"valueFormula"]
                        serverVariablesOnly:NO];
    if (_expr)
      myx_free_expression(_expr);
	
    _expr= myx_compile_expression([expr UTF8String], &err);
    if (!_expr)
    {
      NSLog(@"could not compile expression %@", expr);
    }
    _serverValuesChanged= NO;
	
    if ([_props objectForKey:@"maxFormula"])
    {
      _max= [self evaluate:[_props objectForKey:@"maxFormula"]];
    }
  }
  
  if (_expr && _oldValues)
  {
    _value= myx_eval_expression(_expr, _oldValues, _values, &err);
    if (err != MYX_EXPRESSION_NO_ERROR)
    {
      _value= 0.0;
    }
	[self setNeedsDisplay:YES];
  }
}

@end
