//
//  MQIndicatorCell.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/9/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MQIndicatorCell : NSTextFieldCell {
  BOOL _selected;
  BOOL _placeholder;
  NSString *_arrow;
  NSDictionary *_attribs;
}

- (void)setSelected:(BOOL)flag;
- (void)setPlaceholder:(BOOL)flag;

@end
