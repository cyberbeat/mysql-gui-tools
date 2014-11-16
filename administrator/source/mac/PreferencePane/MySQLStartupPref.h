//
//  MySQLStartupPref.h
//  MySQLStartup
//
//  Created by Alfredo Kojima on 1/4/05.
//  Copyright (c) 2005 MySQL AB. All rights reserved.
//

#import <PreferencePanes/PreferencePanes.h>
#include <Security/Authorization.h>

typedef enum {
	Stopped,
	Running,
	NotFound
} ServerState;



@interface MySQLStartupPref : NSPreferencePane 
{
  IBOutlet NSButton *autoStartCheck;
  IBOutlet NSButton *button;
  IBOutlet NSTextField *descrText;
  IBOutlet NSTextField *stateText;
  IBOutlet NSTextField *warningText;
  IBOutlet NSImageView *stateImage;
  
	NSString *originalWarningText;
	
  AuthorizationRef authRef;
  ServerState curState;
  BOOL firstTime;
}

- (IBAction)toggleServer:(id)sender;
- (IBAction)changeAutoStart:(id)sender;

- (BOOL)isAutoStarting;
- (ServerState)isRunning;
- (BOOL)checkDataDirOwner;

- (void) mainViewDidLoad;

@end
