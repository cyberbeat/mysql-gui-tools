//
//  MonitorClass.h
//  MySQLMonitor
//
//  Created by Alfredo Kojima on 05/8/1.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

#include <mysql.h>

@interface MonitorClass : NSObject {
  MYSQL *_mysql;
  BOOL _oldMySQL;
  NSDictionary *_stats;
  NSDictionary *_ostats;
}

@end
