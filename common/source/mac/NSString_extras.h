//
//  NSString_extras.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Tue Jul 20 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface NSString(NSString_MExtras)
+ (NSString*)stringWithMultNumber:(long long)number;
- (NSString*)initWithMultNumber:(long long)number;
@end

