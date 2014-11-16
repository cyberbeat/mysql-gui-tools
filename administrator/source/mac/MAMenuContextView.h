//
//  MAMenuContextView.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on 2/6/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MAMenuContextView : NSView {
  id _attachedObject;
  BOOL _highlighted;
}

- (id)attachedObject;
- (void)setAttachedObject:(id)obj;

- (void)setHighlighted:(BOOL)flag;

@end
