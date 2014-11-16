//
//  MQResultSetCell.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/13/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MQResultSetCell : NSTextFieldCell {
  NSImage *_blobIcon;
  NSImage *_nullIcon;

  BOOL _blob;
  BOOL _null;
  BOOL _placeholder;
}

- (void)setIsNull:(BOOL)flag;
- (void)setIsBlob:(BOOL)flag;
- (void)setPlaceholder:(BOOL)flag;

@end
