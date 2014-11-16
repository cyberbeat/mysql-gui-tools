/* MAServerLogs */

#import <Cocoa/Cocoa.h>
#import "MAPanel.h"
#include <Security/Authorization.h>

#import "MAServerLogView.h"

@interface MAServerLogs : MAPanel
{
  IBOutlet NSTextField *bigText;
  IBOutlet NSTabView *tabView;
  NSMutableArray *_pages;
  
  BOOL _cancelAuth;
  BOOL _retryAuth;
  
  AuthorizationRef _authRef;
}

- (void)createPage:(NSString*)name
          withType:(int)type
     parseFunction:(MALogParserFunction)func;

@end
