//
//  MPtrArray.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Sun Jul 11 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface MPtrArray : NSObject {
  void **values;
  int count;
}

- (id)init;
- (void)addPointer:(void*)ptr;
- (void*)pointerAtIndex:(int)index;
- (int)count;
@end
