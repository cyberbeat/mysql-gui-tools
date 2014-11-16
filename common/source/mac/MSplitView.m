//
//  MSplitView.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/6/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MSplitView.h"


static void blendFunction(void *info, const float *in, float *out)
{
  float *fromColors= (float*)info;
  float *toColors= (float*)info + 4;
  
  out[0] = (1.0 - *in) * fromColors[0] + *in * toColors[0];
  out[1] = (1.0 - *in) * fromColors[1] + *in * toColors[1];
  out[2] = (1.0 - *in) * fromColors[2] + *in * toColors[2];
  out[3] = (1.0 - *in) * fromColors[3] + *in * toColors[3];
}

static const CGFunctionCallbacks blendCallbacks = {0, &blendFunction, &free};

static const float domainAndRange[8] = {0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0};

@implementation MSplitView

- (void)dealloc
{
  [_inactiveColor release];
  [super dealloc];
}

- (float)dividerThickness
{
  if (_thickness > 0.0)
    return _thickness;
  else
    return [super dividerThickness];
}

- (void)drawDividerInRect:(NSRect)aRect
{
  if (_thickness < 4.0)
  {
    NSRect topR, botR;
    
    NSDivideRect(aRect,&topR,&botR,1.0,NSMaxYEdge);

    [[NSColor lightGrayColor] set];
    NSRectFill(botR);
    [[NSColor darkGrayColor] set];
    NSRectFill(topR);
  }
  else if (_dividerColor)
  {
    CGContextRef context = [[NSGraphicsContext currentContext] graphicsPort];
    CGColorSpaceRef colorSpace= CGColorSpaceCreateDeviceRGB();    
    float hue, saturation, brightness, alpha;
    [[_dividerColor colorUsingColorSpaceName:NSDeviceRGBColorSpace] getHue:&hue saturation:&saturation brightness:&brightness alpha:&alpha];
    float *colors= malloc(sizeof(float)*6);
    NSColor *light = [NSColor colorWithDeviceHue:hue saturation:MAX(0.0, saturation-0.1) brightness:MIN(1.0, brightness+0.1) alpha:alpha];
    NSColor *dark = [NSColor colorWithDeviceHue:hue saturation:MIN(1.0, (saturation > 0.04) ? saturation+0.10 : 0.0) brightness:brightness alpha:alpha];

    [[light colorUsingColorSpaceName:NSDeviceRGBColorSpace] getRed:colors+0 green:colors+1 blue:colors+2 alpha:colors+3];
    [[dark colorUsingColorSpaceName:NSDeviceRGBColorSpace] getRed:colors+4 green:colors+5 blue:colors+6 alpha:colors+7];

    CGFunctionRef functionRef = CGFunctionCreate(colors, 1, domainAndRange, 4, domainAndRange, &blendCallbacks);

    CGContextSaveGState(context);
    {
      CGRect crect= {{NSMinX(aRect), NSMinY(aRect)+1.0}, {NSWidth(aRect), NSHeight(aRect)-1.0}};
      CGContextClipToRect(context, crect);
      CGShadingRef shade;
      
      if ([self isVertical])
        shade= CGShadingCreateAxial(colorSpace, 
                                    CGPointMake(NSMinX(aRect), 0),
                                    CGPointMake(NSMaxX(aRect), 0),
                                    functionRef, NO, NO);
      else
        shade= CGShadingCreateAxial(colorSpace, 
                                    CGPointMake(0, NSMinY(aRect)), 
                                    CGPointMake(0, NSMaxY(aRect)), 
                                    functionRef, NO, NO);
      CGContextDrawShading(context, shade);
      CGShadingRelease(shade);
    } 
    CGContextRestoreGState(context);
    
    CGFunctionRelease(functionRef);
    CGColorSpaceRelease(colorSpace);
    
    [super drawDividerInRect:aRect];
  }
  else
    [super drawDividerInRect:aRect];
}

- (void)setDividerThickness:(float)value
{
  _thickness= value;
}

- (void)setDividerColor:(NSColor*)color
{
  if (_dividerColor != color)
  {
    [_dividerColor release];
    _dividerColor= [color retain];
  }
}

- (void)setActive:(BOOL)active
{
  _active= active;
}

- (void)setInactiveColor:(NSColor*)color
{
  if (_inactiveColor != color)
  {
    [_inactiveColor release];
    _inactiveColor= [color retain];
  }
}

- (void)drawRect:(NSRect)aRect
{
  if (_inactiveColor)
  {
    if (_active)
      [[NSColor whiteColor] set];
    else
      [_inactiveColor set];
    NSRectFill(aRect);
  }
  [super drawRect:aRect];
}

- (void)resizeSubview:(id)view toSize:(float)size
{
  NSArray *subviews= [self subviews];
  NSMutableArray *ratios= [NSMutableArray arrayWithCapacity:[subviews count]];
  unsigned int i, c= [subviews count];
  float s, offs, totalSize;
  float divThick= [self dividerThickness];
  float leftOver;

  totalSize= 0.0;
  // 1st calculate how much space are the subviews using relative to each other
  for (i= 0; i < c; i++)
  {
    id subview= [subviews objectAtIndex:i];
    float t;
    if (![self isVertical])
      t= NSHeight([subview frame]);
    else
      t= NSWidth([subview frame]);
    totalSize+= t;
  }
  totalSize+= (c-1)*divThick;

  for (i= 0; i < c; i++)
  {
    id subview= [subviews objectAtIndex:i];
    float t;
    if (![self isVertical])
      t= NSHeight([subview frame]);
    else
      t= NSWidth([subview frame]);
    [ratios addObject:[NSNumber numberWithFloat:t/totalSize]];
  }
  
  if (size > totalSize)
    size= totalSize;
  
  leftOver= totalSize - size;

  offs= 0.0;
  // then resize all subviews
  for (i= 0; i < c; i++)
  {
    id subview= [subviews objectAtIndex:i];
    NSRect f= [subview frame];

    s= [[ratios objectAtIndex:i] floatValue] * totalSize;
    
    if (subview == view)
    {
      if (![self isVertical])
      {
        f.origin.y= offs;
        f.size.height= size;
      }
      else
      {
        f.origin.x= offs;
        f.size.width= size;
      }
    }
    else
    {
      if (![self isVertical])
      {
        f.origin.y= offs;
        f.size.height= s;
      }
      else
      {
        f.origin.x= offs;
        f.size.width= s;
      }
    }
    [subview setFrame:f];
    offs+= s + divThick;
  }
  [self setNeedsDisplay:YES];
}

@end
