#import "MAStartupVariables.h"
#import "MAXMLGUIController.h"
#include <fcntl.h>
#include <errno.h>
#import <MySQLToolsCommon/NSView_extras.h>
#import <MySQLToolsCommon/MDialogs.h>
#include "mahelper.h"

@implementation MAStartupVariables

- (int)performOpenCommand:(MAHelperCommand)command
                withRight:(const char*)rightName
{
  NSString *helperPath;
  AuthorizationItem right = { rightName, 0, NULL, 0 };
  AuthorizationRights rightSet = { 1, &right };
  OSStatus status;
  int fd= -1;
  
  helperPath= [[NSBundle bundleForClass:[self class]] pathForResource:@"mahelper"
                                                               ofType:@""];
  if (!_authRef)
  {
    if (AuthorizationCreate(NULL, kAuthorizationEmptyEnvironment, kAuthorizationFlagDefaults, 
                            &_authRef)!=errAuthorizationSuccess)
    {
      NSLog(@"Could not create authorization reference object.");
      return -1;
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
    
    hstatus = mhelperPerformOpenCommand(_authRef, [helperPath fileSystemRepresentation], command,
                                        &fd, &error_message);
    
    if (hstatus != MAHelperCommandSuccess || fd < 0)
    {
      if (!(error_message && strcmp(error_message, "OK")==0))
      {
        NSLog(@"error_message %s", error_message);
        NSRunAlertPanel(nil, [NSString stringWithFormat:@"Could not open /etc/my.cnf.\n%s",error_message?:""],
                        @"OK", NULL, NULL);
        return -1;
      }
    }
    if (error_message)
      free(error_message);
  }
  else
  {
    AuthorizationFree(_authRef, kAuthorizationFlagDestroyRights);
    _authRef= 0;
  }
  return fd;
}


- (IBAction)openMerlinSite:(id)sender
{
  [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"http://www.mysql.com/products/enterprise/"]];
}

- (IBAction)discardChanges:(id)sender
{
  [_xmlGUI revertToSaved];
}

- (IBAction)saveChanges:(id)sender
{
  MAHelperCommand command= { MAHelperOpenNewMyCnf, NO };
  int fd;
  
  fd= [self performOpenCommand:command
                     withRight:"com.mysql.administrator.write_mycnf"];
  if (fd >= 0)
  {
    if ([_xmlGUI saveTo:fd])
    {
      command.authorizedCommandId= MAHelperCommitMyCnf;
      
      if ([self performOpenCommand:command
                         withRight:"com.mysql.administrator.write_mycnf"] < 0)
      {
        NSLog(@"Error writing my.cnf changes");
      }
    }
    
    close(fd);
  }
}

- (void)setupWithSection:(NSString*)section
                  ofFile:(NSString*)file
{
  char *version= [_owner mysql] ? myx_get_mysql_full_version([_owner mysql]) : g_strdup("5.0");
  int i;
  
  strtok(version, "-");
  
  _xmlGUI= [[MAXMLGUIController alloc] initInTabView:tabView
                                               mysql:[_owner mysql]
                                             version:version
                                                file:file
                                             section:section];
  [popup removeAllItems];
  for (i= 0; i < [tabView numberOfTabViewItems]; i++)
  {
    [popup addItemWithTitle:[[tabView tabViewItemAtIndex:i] label]];
  }
  
  NSTabViewItem *advisorItem= [[NSTabViewItem alloc] initWithIdentifier:@"advisor"];
  [advisorItem setView: adView];
  [tabView addTabViewItem:[advisorItem autorelease]];
  
  [popup addItemWithTitle:@"Administration & Security Advisors"];

  [noteText setStringValue:
    [NSString stringWithFormat:@"Editing file \"%@\", section \"%@\"",
      file, section]];
  [topBox setEnabledRecursive:YES];
  
  // force refresh
  [tabView selectLastTabViewItem: self];
  [tabView selectFirstTabViewItem: self];  

  g_free(version);
}


- (IBAction)moreInfo:(id)sender
{
  [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"http://www.mysql.com/network"]];
}


- (bool)openConfigFile:(NSString*)file
{
  MYX_STRINGLIST *sections;
  MYX_ADMIN_LIB_ERROR error;
  bool retval= true;
  
  sections= myx_get_all_cnf_sections([file fileSystemRepresentation], &error);
  if (!sections)
  {
    [noteText setStringValue:@"Could not read configuration file."];
  }
  else
  {	
    NSString *section= nil;    
    unsigned int i;
    
    [sampleText setString:[NSString stringWithContentsOfFile:@"/etc/my.cnf"]];
    
    [sectionPop removeAllItems];
    for (i= 0; i < sections->strings_num; i++)
    {
      if (strncmp(sections->strings[i], "mysqld", 6)==0)
        [sectionPop addItemWithTitle:[NSString stringWithUTF8String:sections->strings[i]]];
    }
    
    if ([sectionPop numberOfItems] == 0)
    {
      section= @"mysqld";
    }
    else if ([sectionPop numberOfItems] == 1)
    {
      section= [NSString stringWithUTF8String:sections->strings[0]];
    }
    else
    {
      [pickPanel center];
      [pickPanel makeKeyAndOrderFront:self];
      
      if ([NSApp runModalForWindow:pickPanel] == NSOKButton)
      {
        section= [[sectionPop selectedItem] title];
      }
      else
      {
        retval= false;  // user pressed CANCEL - dont show the panel
      }
    }
    
    if (section)
      [self setupWithSection:section ofFile:file];
    else
      [noteText setStringValue:@"No file open."];
  }
  
  myx_free_stringlist(sections);
  
  return retval;
}

- (IBAction)openFile:(id)sender
{
}


- (void)windowWillClose:(NSNotification *)aNotification
{
  [NSApp stopModalWithCode:NSCancelButton];
}

- (IBAction)sectionOK:(id)sender
{
  [NSApp stopModalWithCode:NSOKButton];
  [pickPanel orderOut:self];
}

+ (NSImage*)icon
{
  return [NSImage imageNamed:@"OSX-Icons_37.png"];
}

+ (NSString*)label
{
  return @"Options";
}

+ (NSString*)toolTip
{
  return @"Configurable Startup Variables of the MySQL Server.";
}

+ (BOOL)needsConnection
{
  return NO;
}

- (void)alertDidEnd:(NSAlert *)alert
         returnCode:(int)returnCode
        contextInfo:(void *)contextInfo
{
  [[alert window] orderOut:self];
  
  if (returnCode == NSAlertDefaultReturn)
  {
    FILE *f= fopen("/etc/my.cnf", "w+");
    if (!f && errno == EACCES)
    {
      MAHelperCommand command= { MAHelperOpenNewMyCnf, NO };
      int fd= [self performOpenCommand:command
                             withRight:"com.mysql.administrator.write_mycnf"];
      
      if (fd < 0)
        return;
      else
      {
        f= fdopen(fd, "w+");
        command.authorizedCommandId= MAHelperCommitMyCnf;
        [self performOpenCommand:command
                       withRight:"com.mysql.administrator.write_mycnf"];
        fclose(f);
      }
    }
    else if (!f)
    {
      NSRunAlertPanel(nil,
                      [NSString stringWithFormat:@"Could not create file /etc/my.cnf\n%s", strerror(errno)],
                      @"OK",nil,nil);
      return;
    }
    else
    {
      fprintf(f, "[mysqld]\n");
      fclose(f);
    }
    [self openConfigFile:@"/etc/my.cnf"];
  }
}

- (id)initWithOwner: (id<MAdministratorProtocol>)owner
{
  self= [super initWithNibFile: @"StartupVariables" panelOwner: owner];
  if (self)
  {
    if ([owner isLocal])
    {
      [bigText setHidden:YES];
      [tabView removeTabViewItem:[tabView tabViewItemAtIndex:0]];
      [topBox setEnabledRecursive:NO];
      
      _defaultFrame= [[self topView] frame];
      if (access("/etc/my.cnf", F_OK)!=0)
      {
        NSAlert *alert= [NSAlert alertWithMessageText:@"Configuration File not Found"
                                        defaultButton:@"Create"
                                      alternateButton:@"Cancel"
                                          otherButton:nil
                            informativeTextWithFormat:@"The my.cnf file could not be located at /etc/my.cnf\nClick \"Create\" to initialize a new one or \"Cancel\"."];
        
        [alert beginSheetModalForWindow:[_owner window]
                          modalDelegate:self
                         didEndSelector:@selector(alertDidEnd:returnCode:contextInfo:)
                            contextInfo:NULL];
      }
      else
      {
        if([self openConfigFile:@"/etc/my.cnf"] == false)
          return nil;
      }
    }
    else
    {
      [tabView setHidden:YES];
      [bigText setHidden:NO];
      [popup removeAllItems];
      [popup setEnabled:NO];
      [topBox setEnabledRecursive:NO];
    }
  }
  return self;
}

- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  if (_authRef)
  	AuthorizationFree(_authRef, kAuthorizationFlagDestroyRights);
  [_xmlGUI release];
  [super dealloc];
}

@end
