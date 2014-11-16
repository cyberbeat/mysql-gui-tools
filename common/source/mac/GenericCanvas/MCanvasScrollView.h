//
//  MCanvasScrollView.h
//  GenericCanvas
//
//  Created by Alfredo Kojima on 05/6/17.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class MGenericCanvasView;

@interface MCanvasScrollView : NSView
{
  NSScroller *_hScroller;
  NSScroller *_vScroller;
  NSView *_hAccessoryView;
  
  MGenericCanvasView *_contentView;
}

- (void)setContentCanvas:(MGenericCanvasView*)canvas;

- (NSSize)contentSize;

- (NSScroller*)verticalScroller;
- (NSScroller*)horizontalScroller;

- (void)setHAccessory:(NSView*)view;

- (void)reflectContentRect;

@end
