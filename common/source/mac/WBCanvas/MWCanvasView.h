//
//  MWCanvasView.h
//  WBCanvas
//
//  Created by Alfredo Kojima on 05/9/8.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <WBCanvas/MGenericCanvasView.h>

@class MGRT;

@interface MWCanvasView : MGenericCanvasView
{
  MGRT *_grt;
}

- (void)setMGRT:(MGRT*)grt;

- (void)setMarker:(id)sender;
- (void)goToMarker:(id)sender;
- (BOOL)hasMarker:(int)index;

@end
