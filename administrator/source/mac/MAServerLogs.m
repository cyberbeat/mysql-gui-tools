#import "MAServerLogs.h"
#import "MAServerLogView.h"
#include "mahelper.h"


@implementation MAServerLogs

- (char*)performOpenCommand:(MAHelperCommandType)command
                    forFile:(int*)fd
                  withRight:(const char*)rightName
                  cancelled:(BOOL*)cancelled
{
  NSString *helperPath;
  AuthorizationItem right = { rightName, 0, NULL, 0 };
  AuthorizationRights rightSet = { 1, &right };
  OSStatus status;
  MAHelperCommand cmd= {command, 0};
  
  *cancelled= NO;
  
  helperPath= [[NSBundle bundleForClass:[self class]] pathForResource:@"mahelper"
                                                               ofType:@""];
  if (!_authRef)
  {
    if (AuthorizationCreate(NULL, kAuthorizationEmptyEnvironment, kAuthorizationFlagDefaults, 
                            &_authRef)!=errAuthorizationSuccess)
    {
      NSLog(@"Could not create authorization reference object.");
      return NULL;
    }
  }
  
  status = AuthorizationCopyRights(_authRef, &rightSet, kAuthorizationEmptyEnvironment, 
                                   kAuthorizationFlagDefaults | kAuthorizationFlagPreAuthorize
                                   | kAuthorizationFlagInteractionAllowed | kAuthorizationFlagExtendRights,
                                   NULL);
  
  if (status == errAuthorizationSuccess)
  {
    int hstatus;
    char *error_message= NULL;
    
    hstatus = mhelperPerformOpenCommand(_authRef, [helperPath fileSystemRepresentation], cmd,
                                        fd, &error_message);
    
    if (hstatus != MAHelperCommandSuccess || fd < 0)
    {
      *fd= -1;
    }
    return error_message;
  }
  else
  {
    if (status == errAuthorizationCanceled)
      *cancelled= YES;
    
    AuthorizationFree(_authRef, kAuthorizationFlagDestroyRights);
    _authRef= 0;
    *fd= -1;
    return NULL;
  }
}

+ (NSImage*)icon
{
  return [NSImage imageNamed:@"OSX-Icons_43.png"];
}

+ (NSString*)label
{
  return @"Logs";
}

+ (NSString*)toolTip
{
  return @"View MySQL Server Logs.";
}

+ (BOOL)needsConnection
{
  return NO;
}

- (id)initWithOwner: (id<MAdministratorProtocol>)owner
{
  _retryAuth= NO;
  _pages= [[NSMutableArray alloc] init];
  self= [super initWithNibFile: @"ServerLogs" panelOwner: owner];
  if (self)
  {
    _defaultFrame= [[self topView] frame];
    
    [tabView removeTabViewItem:[tabView tabViewItemAtIndex:0]];  
    
    if (![_owner isLocal])
    {
      [tabView setHidden:YES];
      [bigText setHidden:NO];
    }
    else
    {
      [tabView setHidden:NO];
      [bigText setHidden:YES];
    }
  }
  return self;
}

- (void)dealloc
{
  if (_authRef)
  	AuthorizationFree(_authRef, kAuthorizationFlagDestroyRights);
  [_pages release];
  
  [super dealloc];
}

- (void)didShow
{
  if ((![_pages count] || _cancelAuth) && [_owner isLocal])
  {
    NSString *host;
    char buffer[1024];
    _retryAuth= _cancelAuth;
    _cancelAuth= NO;

    
    if (gethostname(buffer, sizeof(buffer)) < 0)
    {
      // Very slow! so only use as a fallback
      host= [[NSHost currentHost] name];
    }
    else
      host= [NSString stringWithCString:buffer];
    
    [self createPage: @"Error Log"
            withType: MAHelperOpenErrorLog
       parseFunction: myx_parse_error_logf];
    
    [self createPage: @"General Log"
            withType: MAHelperOpenGeneralLog
       parseFunction: myx_parse_general_logf];
    
    [self createPage: @"Slow Log"
            withType: MAHelperOpenSlowLog
       parseFunction: myx_parse_slow_logf];	

    _retryAuth= _cancelAuth;
  }
}

- (void)createPage:(NSString*)name
          withType:(int)type
     parseFunction:(MALogParserFunction)func
{
  char *error= NULL;
  int fd= -1;
  MAServerLogView *view;
  NSString *errorMessage, *path;
  
  if (!_cancelAuth)
    error= [self performOpenCommand:type
                            forFile:&fd
                          withRight:"com.mysql.administrator.read_log"
                          cancelled:&_cancelAuth];
  
  if (fd < 0 || !error)
    path= nil;
  else
    path= [NSString stringWithUTF8String:error];
  
  if (fd < 0 && error)
    errorMessage= [NSString stringWithUTF8String:error];
  else if (_cancelAuth && !_retryAuth)
  {
    path= @"Unreadable";
    errorMessage= @"Authentication cancelled.";
  }
  else
    errorMessage= nil;
  
  if(_retryAuth && !_cancelAuth)
  {
    int i;
    int c= [_pages count];
    for(i= 0; i < c; i++)
    {
      MAServerLogView *v= (MAServerLogView *)[_pages objectAtIndex: i];
      if([v logType] == type)
      {
        [v  setFile:fd < 0 ? NULL : fdopen(fd, "r")
               path:path ?: nil
             parser:func
              error:errorMessage];
        if(fd >= 0)
          [v changePage: v];
      }
    }
  }
  else if(!_retryAuth)
  {
    view= [[[MAServerLogView alloc] initWithFile:fd < 0 ? NULL : fdopen(fd, "r")
                                            path:path ?: nil
                                           error:errorMessage
                                          parser:func] autorelease];
    [view setLogType: type];
    if (![NSBundle loadNibNamed:@"ServerLogView" owner:view])
    {
      NSRunAlertPanel(nil,@"Could not instantiate interface for log viewing.",
                      nil, nil, nil);
    }
    else
    {
      NSTabViewItem *item= [[[NSTabViewItem alloc] initWithIdentifier:name] autorelease];
  
      [item setLabel:name];
      [_pages addObject:view];
      [item setView:[view topView]];
      [tabView addTabViewItem:item];
    }
  }
  
  
  if (error)
    free(error);
}

@end
