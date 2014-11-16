//
//  MTabView.h
//
//  Created by Alfredo Kojima on 2/16/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


typedef enum {
  ISBack,
  ISFront,
  ISHover,
  ISPushed
} MTabViewItemState;


@interface MTabViewItem : NSResponder {
  @private
  IBOutlet NSView *view;
  
  @protected
  NSString *_identifier;

  MTabViewItemState _state;
  MTabViewItemState _closeState;
  
  NSImage *_icon;
  NSString *_label;
}

- (id)initWithIdentifier:(NSString*)identifier;

- (MTabViewItemState)tabState;
- (void)setTabState:(MTabViewItemState)state;

- (void)setIcon:(NSImage*)icon;
- (NSImage*)icon;

- (NSString*)label;
- (void)setLabel:(NSString*)label;

- (NSView*)view;
- (void)setView:(NSView*)view;

@end


@interface MTabView : NSView 
{
  IBOutlet id delegate;

  NSMutableArray *_pages;
  NSMutableDictionary *_tabAttrs;

  BOOL _tabsOnBottom;
  BOOL _brushedMetal;
  
  NSTrackingRectTag *_trackingRects;
  
  NSImage *_closeImage[3];

  NSColor *_backColor[3];
  NSColor *_lborderColor;
  NSColor *_borderColor;
  
  NSView *_contentView;

  float _tabY;
  float _tabWidth;
  float _maxTabWidth;
  int _visibleTabCount;

  int _currentPage;
  
  BOOL _hasBorder;
}

- (void)setTabsOnBottom:(BOOL)flag;
- (void)setCommonContent:(NSView*)content;

- (int)addTabViewItem:(MTabViewItem*)item;
- (void)removeTabViewItem:(MTabViewItem*)item;
- (void)selectPage:(int)page;
- (MTabViewItem*)selectedPage;
- (unsigned int)numberOfPages;
- (void)selectNextPage;
- (void)selectPreviousPage;

- (void)setHasBorder:(BOOL)flag;

- (NSSize)contentSize;

- (MTabViewItem*)pageAtIndex:(int)page;

- (void)setDelegate:(id)delegate;

- (void)setMaxTabSize:(float)size;

- (void)recalcTabSizes;
@end
