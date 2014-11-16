//
//  MonitorClass.m
//  MySQLMonitor
//
//  Created by Alfredo Kojima on 05/8/1.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MonitorClass.h"


@implementation MonitorClass

-(id)initWithWebView:(WebView*)w 
{
	self = [super init];
	return self;
}


-(void)windowScriptObjectAvailable:(WebScriptObject*)wso 
{
	[wso setValue:self forKey:@"MySQLMonitor"];
}

+(NSString*)webScriptNameForSelector:(SEL)aSel {
	NSString *retval = nil;

	if (aSel == @selector(getStatus:)) {
		retval = @"getStatus";
  } else if (aSel == @selector(queryStatus)) {
    retval = @"queryStatus";
	} else if (aSel == @selector(connectHost:username:password:)) {
		retval = @"connect";
  } else if (aSel == @selector(testConnection:username:password:)) {
		retval = @"testConnection";
	} else {
		NSLog(@"\tunknown selector");
	}
	
	return retval;
}


+(BOOL)isSelectorExcludedFromWebScript:(SEL)aSel 
{	
	if (aSel == @selector(getStatus:) 
      || aSel == @selector(connectHost:username:password:)
      || aSel == @selector(testConnection:username:password:)
      || aSel == @selector(queryStatus)) {
		return NO;
	}
	return YES;
}

+(BOOL)isKeyExcludedFromWebScript:(const char*)k 
{
	return YES;
}


- (NSString*)queryStatus
{
  NSMutableDictionary *dict= [[[NSMutableDictionary alloc] init] autorelease];
  const char *query;
  MYSQL_RES *res;
  MYSQL_ROW row;
    
  if (_oldMySQL)
    query= "SHOW STATUS";
  else
    query= "SHOW GLOBAL STATUS";
  
  if (mysql_query(_mysql, query) != 0)
    return nil;
  
  if ((res= mysql_store_result(_mysql)))
  {
    while ((row= mysql_fetch_row(res)))
    {
      if (row && row[0] && row[1])
      {
        NSNumber *number= [NSNumber numberWithDouble:strtod(row[1], NULL)];

        [dict setObject: number
                 forKey: [NSString stringWithUTF8String:row[0]]];
      }
    }
    mysql_free_result(res);
  }
  
  if (_ostats)
    [_ostats release];
  _ostats= _stats;
  _stats= [dict retain];
  
  return @"1";
}


- (double)getDelta:(NSString*)stat
{
  double v1, v2;
  v1= [[_ostats objectForKey:stat] doubleValue];
  v2= [[_stats objectForKey:stat] doubleValue];
  return v2-v1;
}


- (NSString*)getStatus:(NSString*)stat
{
  NSString *result= nil;
  
  if (!_mysql)
    return nil;
  
  if ([stat isEqualTo:@"Qcache_hitrate"])
  {
    double hits, inserts, notcached;

    result= @"0";
    if (_ostats)
    {
      hits= [self getDelta:@"Qcache_hits"];
      inserts= [self getDelta:@"Qcache_inserts"];
      notcached= [self getDelta:@"Qcache_not_cached"];
    
      if ((hits+inserts+notcached) > 0)
        result= [NSString stringWithFormat:@"%.0f", (hits/(hits+inserts+notcached))*100];
    }
  }
  else
  {
    result= [NSString stringWithFormat:@"%f", [[_stats objectForKey:stat] doubleValue]];
  }
  return result;
}


- (NSString*)testConnection:(NSString*)host
                   username:(NSString*)username
                   password:(NSString*)password
{
  MYSQL mysql;

  char *hostname = malloc([host length]+1);
  int port = 3306;
  sscanf([host UTF8String], "%[^:]s:%i", hostname, &port);
  
  mysql_init(&mysql);
  if (mysql_real_connect(&mysql,
                         hostname, 
                         [username UTF8String],
                         [password UTF8String],
                         NULL,
                         port,
                         NULL,
                         0) != 0)
  {
    free(hostname);
    mysql_close(&mysql);
    return @"";
  }
  else
  {
    NSString *msg;
    msg= [NSString stringWithUTF8String:mysql_error(&mysql)];
    free(hostname);
    mysql_close(&mysql);
    return msg;
  }
}



- (NSNumber*)connectHost:(NSString*)host
                username:(NSString*)username
                password:(NSString*)password
{
  char *hostname = malloc([host length]+1);
  int port = 3306;
  
  sscanf([host UTF8String], "%[^:]s:%i", hostname, &port);
  
  if (_mysql)
  {
    mysql_close(_mysql);
    
    [_stats release];
    [_ostats release];
    _stats= nil;
    _ostats= nil;
  }

  _mysql= mysql_init(NULL);
  if (mysql_real_connect(_mysql,
                         hostname, 
                         [username UTF8String],
                         [password UTF8String],
                         NULL,
                         port,
                         NULL,
                         0) != 0)
  {
    free(hostname);
    
    if (mysql_get_server_version(_mysql) < 50000)
      _oldMySQL= YES;
    else
      _oldMySQL= NO;
    
    return [NSNumber numberWithInt:1];
  }
  else
  {
    free(hostname);
    return [NSNumber numberWithInt:0];
  }
}




@end
