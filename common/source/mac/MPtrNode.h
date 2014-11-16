//
//  MPtrNode.h
//  MySQLGUICommon
//
//  Copyright (c) 2005 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>

typedef int (*MPtrFree)(void*);

@interface MPtrNode : NSObject {
  NSString *_caption;   // function caption
  void *_pointer;
  MPtrFree _destroyer;
  int _tag;
  NSMutableArray *_subnodes;
}

- (void)setCaption:(NSString*)caption;
- (NSString*)caption;
- (void*)pointer;
- (void)setPointer:(void*)ptr;
- (void)setPointer:(void*)ptr destroyer:(MPtrFree)destroyer;
- (int)tag;
- (void)setTag:(int)tag;

- (void) setSubnodes: (NSMutableArray *)subnodes;
- (unsigned) subnodeCount;
- (NSArray *)subnodes;

@end
