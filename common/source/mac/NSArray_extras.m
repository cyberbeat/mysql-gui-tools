//
//  NSArray_extras.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 4/25/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "NSArray_extras.h"


@implementation NSArray(NSArray_MExtras)

- (NSArray*)arrayWithResultsOfSelector:(SEL)selector
{
  unsigned int i, c= [self count];
  NSMutableArray *array= [NSMutableArray arrayWithCapacity:c];
  for (i= 0; i < c; i++)
  {
    [array addObject:[[self objectAtIndex:i] performSelector:selector]];
  }
  return array;
}

@end
