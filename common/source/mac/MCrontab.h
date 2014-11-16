//
//  MCrontab.h
//  MySQLGUICommon
//
//  Created by Vlad on 06.05.05.
//  Copyright 2005 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface MCrontabEntry : NSObject
{
@public
	int _minute;
	int _hour;
	int _month;
	int _weekday;
	
	NSString *_day;
	NSString *_command;
}

- (id)init;

@end

@interface MCrontab : NSObject 
{
	NSMutableArray *_lines;
}

- (id) init;
- (NSString*) formatEntry: (MCrontabEntry *) entry;
- (MCrontabEntry *) findEntryByComment: (NSString *) comment;
- (BOOL) removeCommand: (NSString *) command withComment: (NSString *) comment;
- (void) addEntry: (MCrontabEntry *) entry withComment: (NSString *) comment; 

- (BOOL) load;
- (BOOL) installTable;

- (BOOL) parseLine: (NSString *)line toEntry: (MCrontabEntry *) entry;
- (NSArray *) lines;

@end