//
//  NSString_extras.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Tue Jul 20 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "NSString_extras.h"




@implementation NSString(NSString_MExtras)
+ (NSString*)stringWithMultNumber:(long long)number
{
  return [[[NSString alloc] initWithMultNumber:number] autorelease];
}

- (NSString*)initWithMultNumber:(long long)number
{
  if (number <= 1024)
    return [self initWithFormat:@"%lli", number];
  else
  {
    char mult;
    double value;
    
    if (number < 1024*1024LL)
    {
      value= number / 1024.0;
      mult= 'k';
    }
    else if (number < 1024*1024*1024LL)
    {
      value= number / (1024.0*1024.0);
      mult= 'M';
    }
    else
    {
      value= number / (1024.0*1024.0*1024.0);
      mult= 'G';
    }    
    return [self initWithFormat:@"%.2f %c", value, mult];
  }
}

@end
