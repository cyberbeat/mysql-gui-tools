//
//  MAPanel.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Thu Jun 24 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#import "MAdministratorProtocol.h";

@interface MAPanel : NSObject {
  IBOutlet NSView *topBox;
  NSRect _defaultFrame;
  BOOL _needsSave;
  id<MAdministratorProtocol> _owner;
}

+ (NSImage*)icon;
+ (NSString*)label;
+ (NSString*)toolTip;
+ (BOOL)needsConnection;

- (id)initWithNibFile:(NSString*)file
           panelOwner:(id<MAdministratorProtocol>)owner;

- (id)initWithMessage:(NSString*)message owner:(id)owner;

// must be overriden
- (id)initWithOwner:(id<MAdministratorProtocol>)owner;

- (NSView*)topView;
- (NSView*)sideView;

- (BOOL)willShow;
- (void)didShow;

- (BOOL)willHide;
- (void)didHide;

- (BOOL)willClose;

- (void)setNeedsSave:(BOOL)flag;
- (BOOL)needsSave;

- (void)showMessageSheet:(NSString*)message
                    info:(NSString*)info;

- (NSRect)defaultFrame;

@end
