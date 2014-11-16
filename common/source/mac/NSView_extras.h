//
//  NSView_extras.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Tue Aug 17 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <AppKit/AppKit.h>


@interface NSView(NSView_MExtras)

- (void)setEnabledRecursive:(BOOL)flag;
- (void)setEnabledRecursive:(BOOL)flag exceptTagsInRange:(NSRange)range;

- (void)setChildrenEnabled:(BOOL)flag exceptTagsInRange:(NSRange)range;

@end

