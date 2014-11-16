
#include <glib.h>
#include <mysqld_error.h>

#import "MAUserAdministration.h"
#import "MUserListDataSource.h"
#import <MySQLToolsCommon/MSchemaDataSource.h>
#import <MySQLToolsCommon/MMySQLDispatcher.h>
#import <MySQLToolsCommon/NSView_extras.h>

#import "PrivilegeDataSource.h"

#import <MySQLToolsCommon/MOutlineView.h>
#import <MySQLToolsCommon/MTextImageCell.h>

#import <MySQLToolsCommon/mxUtils.h>

//XXX make a protocol with the exported stuff from MAdministrator and include it here
extern NSString *MASchemaDataSourceChanged;

#define TAG_UserAdd 200
#define TAG_UserRemove 201
#define TAG_HostAdd 202
#define TAG_HostRemove 203


@interface MAUserAdministration (MAUserAdministrationPriv)
- (void)schemaDSChanged: (NSNotification*)notif;

- (void)requestUserInfo:(NSString*)name;
- (void)userInfoArrived:(id)arg info:(void*)result;
- (void)userNamesArrived:(id)arg names:(void*)result;

- (MUserItem*)selectedUser;
- (NSString*)selectedHost;

- (BOOL)commitUserChanges:(MUserItem*)user;
- (void)discardUserChanges:(MUserItem*)user;

- (void)deleteUserPressed:(id)sender;

- (void)setPage:(int)page enabled:(BOOL)enabled;

- (void)setButton:(int)tag enabled:(BOOL)flag image:(NSString*)image ;

@end

@implementation MAUserAdministration (MAUserAdministrationPriv)

- (void)deleteUserPressed:(id)sender
{
  if ([userOutline levelForRow:[userOutline selectedRow]] == 0)
    [self removeUser:sender];
  else
    [self removeHost:sender];
}


- (void)setButton:(int)tag enabled:(BOOL)flag image:(NSString*)image 
{
  NSButton *btn= [topBox viewWithTag:tag];

  if (flag)
    [btn setImage:[NSImage imageNamed:[NSString stringWithFormat:@"%@_normal.png", image]]];
  else
    [btn setImage:[NSImage imageNamed:[NSString stringWithFormat:@"%@_disabled.png", image]]];

  [btn setEnabled:flag];
}


- (void)setPage:(int)page enabled:(BOOL)enabled
{
  NSArray *subviews;
  int c;
  int j;
  BOOL buttonsEnabled= enabled;
  
  if (page == 1) // global
    subviews= [[globalPageView view] subviews];
  else if (page == 3) // table
    subviews= [[tablePageView view] subviews];
  else
    subviews= [[[tabView tabViewItemAtIndex: page] view] subviews];
  c= [subviews count];
  
  if (page == 2) // schema
  {
    if ([schemaOutline selectedRow]<0)
      buttonsEnabled= NO;
    else if ([schemaOutline levelForRow: [schemaOutline selectedRow]]<1)
      buttonsEnabled= NO;
    else if (![self selectedUser] || ![self selectedHost])
      buttonsEnabled= NO;
  }
  else if (page == 3) // table
  {
    if ([tableOutline selectedRow]<0)
      buttonsEnabled= NO;
    else if ([tableOutline levelForRow: [tableOutline selectedRow]]<2)
      buttonsEnabled= NO;
  }
  
  for (j= 0; j < c; j++)
  {
    id obj= [subviews objectAtIndex:j];

    if ([obj isMemberOfClass: [NSScrollView class]])
    {
      [[obj documentView] setEnabled: enabled];
    }
    else if ([obj isMemberOfClass: [NSButton class]])
      [obj setEnabled: buttonsEnabled];
    else
      [obj setEnabled: enabled];
  }
}

- (void)schemaDSChanged: (NSNotification*)notif
{
  if ([[notif userInfo] objectForKey:@"ds"] == [_owner sharedSchemaDS])
  {
    [schemaOutline reloadData];
    MXExpandOutline(schemaOutline, NO);
  }
  else
  {
    [tableOutline reloadData];
    MXExpandOutline(tableOutline, NO);
  }
}


- (void)userNamesArrived:(id)arg names:(void*)result
{
  NSMutableArray *array;
  MYX_USER_NAMES *names= (MYX_USER_NAMES*)result;
  unsigned int i;

  if (!names)
  {
    if (myx_mysql_errno([_owner mysql]) == ER_TABLEACCESS_DENIED_ERROR)
      NSRunAlertPanel(@"Error", @"Insufficient privileges for user account administration. Try reconnecting with the 'root' account.", 
                      @"OK", nil, nil);
    else
      MXRunAlertPanelWithMySQLError(@"Error", @"Could not retrieve user list:", 
                                    [_owner mysql]);
    return;
  }
  array= [[[NSMutableArray alloc] init] autorelease];
  for (i= 0; i < names->user_names_num; i++)
  {
    [array addObject:[NSString stringWithUTF8String:names->user_names[i]]];
  }
  myx_free_user_names(names);
  [_userDS setUserNames:array];
  [userOutline reloadData];
}

- (void)userNamesArrivedForRefresh:(id)arg names:(void*)result
{
  [self userNamesArrived: arg names: result];
  [userOutline selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO];  
}


- (void)requestUserInfo:(NSString*)name
{
  char *ns= g_strdup([name UTF8String]);

  [[_owner dispatcher] performCallback:(void*(*)(MYSQL*,void*))myx_get_user_with_privileges
                              argument:ns
                      finishedSelector:@selector(userInfoArrived:info:)
                              argument:(id)ns
                                target:self];
}


- (void)userInfoArrived:(id)arg info:(void*)infoptr
{
  MYX_USER *info= (MYX_USER*)infoptr;
  MUserItem *item;
  
  if (!infoptr)
  {
    if (![[_owner dispatcher] checkConnection])
      [[_owner dispatcher] reconnect];
    return;
  }

  item= [_userDS itemForUser:[NSString stringWithUTF8String:(char*)arg]];
  if (item)
  {
    if (info)
    {
      NSString *oldHost= [self selectedHost];
      MUserHostItem *hitem;

      [item setUserInfo:info];
      [userOutline reloadItem:item reloadChildren:YES];
      
      hitem= [item itemForHost:oldHost];
      if (hitem)
        [userOutline selectRow:[userOutline rowForItem:hitem]
          byExtendingSelection:NO];
      
      // if the item is selected, display it
      if ([self selectedUser] == item)
        [self showUser:item host:[self selectedHost]];
    }
    else
    {
      [item setUserInfo:NULL];
      [userOutline reloadItem:item reloadChildren:YES];
    }
  }
  g_free((char*)arg);
  
  if (!info)
  {
    NSRunAlertPanel(@"Error", @"Error retrieving user information.", nil, nil, nil);
  }
}


- (BOOL)commitUserChanges:(MUserItem*)item
{
  MYX_USER *info= [item userInfo];
  int result;
  char *username;
  
  // do some validation
  if (!item) return NO;
  
  if (![item hosts] || [[item hosts] count] == 0)
  {
    [self showMessageSheet:@"No hosts defined for user" 
                      info:@"You need to add at least one host from where the user may connect from."];
    return NO;
  }
  if (![[passwordText stringValue] isEqualToString:[confirmText stringValue]])
  {
    [self showMessageSheet:@"Passwords don't match"
                      info:@"The password and it's confirmation must match."];
    return NO;
  }
  
  // save changes to MYX_USER
  g_free(info->password);
  info->password= g_strdup([[passwordText stringValue] UTF8String]);
  
  g_free(info->full_name);
  info->full_name= g_strdup([[fullNameText stringValue] UTF8String]);
  
  g_free(info->description);
  info->description= g_strdup([[descriptionText stringValue] UTF8String]);
  
  g_free(info->email);
  info->email= g_strdup([[emailText stringValue] UTF8String]);
  
  g_free(info->contact_information);
  info->contact_information= g_strdup([[contactText string] UTF8String]);
  
  // save it in a thread and wait
retry:
  username= g_strdup([[item name] UTF8String]);
  result= (long)[[_owner dispatcher] performCallback:(void*(*)(MYSQL*,void*,void*,void*))myx_set_user
                                           argument:info
                                           argument:username
                                           argument:(void*)([item isNewUser]?1:0)
                                      waitForWindow:[topBox window]
                                            message:@"Commiting changes to user..."];
  g_free(username);
  if (result < 0)
  {
    if (![[_owner dispatcher] checkConnection] && [[_owner dispatcher] reconnect])
      goto retry;
    
    [self showMessageSheet:@"Error"
                      info:@"Could not save changes to the user."];
    NSLog(@"Error: %s", myx_mysql_error([_owner mysql]));
  }
  
  [item setSaved];

  [self setNeedsSave:NO];
  return YES;
}

- (void)discardUserChanges:(MUserItem*)user
{
  [self setNeedsSave:NO];
  if (user)
  {
    if ([user isNewUser])
    {
      [_userDS removeUser:user];
      [userOutline reloadData];
    }
    else
      [self requestUserInfo:[user name]];
  }
}

- (MUserItem*)selectedUser
{
  int row= [userOutline selectedRow];
  id item;
  if (row < 0)
    return nil;
  item= [userOutline itemAtRow: row];
  
  if ([item isMemberOfClass: [MUserHostItem class]]) // selected a host
  {
    return [(MUserHostItem*)item user];
  }
  else if ([item isMemberOfClass: [MUserItem class]]) // selected a user
  {
    return item;
  }
  return nil;
}

- (NSString*)selectedHost
{
  int row= [userOutline selectedRow];
  id item;
  if (row < 0)
    return nil;
  item= [userOutline itemAtRow: row];
  if ([item isMemberOfClass: [MUserHostItem class]]) // selected a host
  {
    return [item hostname];
  }
  return nil;
}

@end


@implementation MAUserAdministration


static BOOL grantSelectedRows(NSTableView *table, PrivilegeDataSource *ds)
{
  NSIndexSet *rows= [table selectedRowIndexes];
  int i, c= [table numberOfRows];
  int left= [rows count];
  NSMutableArray *tmparr= [NSMutableArray arrayWithCapacity:[rows count]];

  for (i= 0; left > 0 && i < c; i++)
  {
    if ([rows containsIndex:i])
    {
      left--;
      [tmparr addObject:[ds tableView:table objectValueForTableColumn:nil row:i]];
    }
  }
  c= [tmparr count];
  for (i= 0; i < c; i++)
  {
    [ds grant: [tmparr objectAtIndex:i]];
  }
  return c>0;
}


static BOOL revokeSelectedRows(NSTableView *table, PrivilegeDataSource *ds)
{
  NSIndexSet *rows= [table selectedRowIndexes];
  int i, c= [table numberOfRows];
  int left= [rows count];
  for (i= c-1; left > 0 && i >= 0; i--)
  {
    if ([rows containsIndex:i])
    {
      left--;
      [ds revoke: [ds tableView:table objectValueForTableColumn:nil row:i]];
    }
  }
  return c>0;
}

- (IBAction)refreshUserList:(id)sender
{
    [[_owner dispatcher] performCallback:(void*(*)(MYSQL*))myx_get_user_names
                      finishedSelector:@selector(userNamesArrivedForRefresh:names:)
                              argument:nil
                                target:self];
}

- (IBAction)addUser:(id)sender
{
  NSString *name= @"newuser";
  MUserItem *item;
  int i= 0;
  while ([_userDS itemForUser:name])
    name= [NSString stringWithFormat:@"newuser_%i",++i];
  
  item= [[MUserItem alloc] initNewUser];
  [item setName:[name retain]];

  [_userDS addUser:item];
  [userOutline reloadData];
  
  [userOutline selectRow:[userOutline rowForItem:item] byExtendingSelection:NO];
  [item addHost: @"%"];
  
  [[topBox window] makeFirstResponder:usernameText];
}


- (IBAction)removeUser:(id)sender
{ 
  MUserItem *item= [self selectedUser];
  if (item)
  {
    if ([item originalName])
    {
      if ([[item originalName] isEqualTo:@"root"])
      {
        NSRunAlertPanel(@"Warning",@"You cannot remove the root user.",@"OK",nil,nil);
        return;
      }
      
      NSAlert *alert = [NSAlert alertWithMessageText:@"Remove Account?"
                                       defaultButton:@"Remove" alternateButton:@"Cancel" otherButton:nil        
                           informativeTextWithFormat:@"The account '%@' will be permanently removed.", 
                                                     [item originalName]];
      
      [alert beginSheetModalForWindow:[topBox window] modalDelegate:self 
                       didEndSelector:@selector(removeUserAlertDidEnd:returnCode:contextInfo:) contextInfo:item];
    }
    else
    {
      [self removeUserAlertDidEnd:nil returnCode:NSAlertDefaultReturn contextInfo:item];
    }
  }
}


- (void)removeUserAlertDidEnd:(NSAlert *)alert returnCode:(int)returnCode
        contextInfo:(void *)contextInfo
{
  MUserItem *item= (MUserItem*)contextInfo;

  [[alert window] orderOut:self];

  if (returnCode == NSAlertDefaultReturn)
  {
    [self setNeedsSave:NO];

    if ([item originalName])
      [[_owner dispatcher] performCallback:(void*(*)(MYSQL*,void*))myx_del_user
                                  argument:(char*)[[item originalName] UTF8String]
                             waitForWindow:[topBox window]
                                   message:@"Removing user..."];

    [_userDS removeUser:item];
    [userOutline reloadData];
  }
}


- (IBAction)addHost:(id)sender
{
  [NSApp beginSheet:addHostPanel
     modalForWindow:[topBox window] 
                    modalDelegate:self
                   didEndSelector:@selector(addHostSheetDidEnd:returnCode:contextInfo:)
                      contextInfo:NULL];
}


- (void)addHostSheetDidEnd:(NSWindow *)sheet 
                returnCode:(int)returnCode
               contextInfo:(void *)contextInfo
{
  [sheet orderOut:self];
  
  if (returnCode == 1)
  {
    NSString *host;
    MUserItem *user= [self selectedUser];
    MUserHostItem *item;

    if ([[addHostMatrix cellWithTag:0] state] == NSOnState)
      host= @"localhost";
    else if ([[addHostMatrix cellWithTag:1] state] == NSOnState)
      host= @"%";
    else
      host= [addHostText stringValue];

    if (!(item= [user addHost:host]))
      NSRunAlertPanel(@"Error", @"'%@' is already in the list of allowed hosts for the user.",
                      @"OK", nil, nil, host);
    else
    {
      [userOutline reloadItem:user reloadChildren:YES];
      [userOutline selectRow:[userOutline rowForItem:item] byExtendingSelection:NO];
      [self setNeedsSave:YES];
    }
  }
}


- (IBAction)removeHost:(id)sender
{
  MUserItem *user= [self selectedUser];
  if ([self selectedHost])
  {
    int userRow= [userOutline selectedRow];

    if ([[user name] isEqualTo:@"root"] && [[user hosts] count] == 1)
    {
      NSRunAlertPanel(@"Warning",
                      @"You cannot totally remove access from the root user.\nPlease remove manually if you are sure about that.",
                      @"OK",nil,nil);
      return;
    }

    while (userRow >= 0 && [userOutline levelForRow:userRow]!=0)
      userRow--;
    [user removeHost:[self selectedHost]];
    [self setNeedsSave:YES];
    [userOutline selectRow:userRow
      byExtendingSelection:NO];
    [userOutline reloadData];
  }
}

- (IBAction)saveChanges:(id)sender
{
  [self commitUserChanges:[self selectedUser]];
}

- (IBAction)chooseIcon:(id)sender
{
  NSOpenPanel *panel= [NSOpenPanel openPanel];
  MYX_USER *user= [[self selectedUser] userInfo];

  [panel setTitle:@"Open Icon"];

  if ([panel runModalForTypes:[NSArray arrayWithObject:@"png"]] == NSOKButton)
  {
    NSData *data= [NSData dataWithContentsOfFile:[panel filename]];

    if (data)
    {
      NSImage *image= [[[NSImage alloc] initWithData:data] autorelease];
      if (![image isValid])
      {
        [self showMessageSheet:@"Invalid image" info:@"Icon image must be a PNG file."];
      }
      else
      {
        [iconView setImage:image];
      
        user->icon_length= [data length];
        g_free(user->icon);
        user->icon= g_memdup([data bytes],[data length]);
      }
    }
  }
}

- (IBAction)discardChanges:(id)sender
{
  [self discardUserChanges:[self selectedUser]];
}

- (IBAction)globalAddPrivilege:(id)sender
{
  if (grantSelectedRows(globalAvailableTable, _globalPrivDS))
  {
    [globalAvailableTable reloadData];
    [globalAssignedTable reloadData];
    [globalAvailableTable deselectAll: self];
    [self setNeedsSave:YES];
  }
}

- (IBAction)globalRemovePrivilege:(id)sender
{
  if (revokeSelectedRows(globalAssignedTable, _globalPrivDS))
  {
    [globalAvailableTable reloadData];
    [globalAssignedTable reloadData];
    [globalAssignedTable deselectAll: self];
    [self setNeedsSave:YES];
  }
}

- (IBAction)schemaAddPrivilege:(id)sender
{
  if (grantSelectedRows(schemaAvailableTable, _schemaPrivDS))
  {
    [schemaAvailableTable reloadData];
    [schemaAssignedTable reloadData];
    [schemaAvailableTable deselectAll: self];  
    [self setNeedsSave:YES];
  }
}

- (IBAction)schemaRemovePrivilege:(id)sender
{
  if (revokeSelectedRows(schemaAssignedTable, _schemaPrivDS))
  {
    [schemaAvailableTable reloadData];
    [schemaAssignedTable reloadData];
    [schemaAssignedTable deselectAll: self];  
    [self setNeedsSave:YES];
  }
}

- (IBAction)tableAddPrivilege:(id)sender
{
  if (grantSelectedRows(tableAvailableTable, [tableAvailableTable dataSource]))
  {
    [tableAvailableTable reloadData];
    [tableAssignedTable reloadData];
    [tableAvailableTable deselectAll: self];  
    [self setNeedsSave:YES];
  }
}

- (IBAction)tableRemovePrivilege:(id)sender
{
  if (revokeSelectedRows(tableAssignedTable, [tableAvailableTable dataSource]))
  {
    [tableAvailableTable reloadData];
    [tableAssignedTable reloadData];
    [tableAssignedTable deselectAll: self];
    [self setNeedsSave:YES];
  }
}

- (IBAction)addHostAdd:(id)sender
{
  [NSApp stopModal];
  [NSApp endSheet:addHostPanel returnCode:1];
}

- (IBAction)addHostCancel:(id)sender
{
  [NSApp stopModal];
  [NSApp endSheet:addHostPanel returnCode:0];
}

+ (NSImage*)icon
{
  return [NSImage imageNamed:@"OSX-Icons_39.png"];
}

+ (NSString*)label
{
  return @"Accounts";
}

+ (NSString*)toolTip
{
  return @"MySQL User Account and Privilege Administration.";
}

- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [_userDS release];
  [_globalPrivDS release];
  [_schemaPrivDS release];
  [_tablePrivDS release];
  [_columnPrivDS release];
  [_schemaDS release];
  [_objectDS release];

  [globalPageView release];
  [tablePageView release];
  
  if (_globalPrivs)
    myx_free_user_priv(_globalPrivs);
  if (_schemaPrivs)
    myx_free_user_priv(_schemaPrivs);
  if (_tablePrivs)
    myx_free_user_priv(_tablePrivs);
  if (_columnPrivs)
    myx_free_user_priv(_columnPrivs);
  
  [_privilegeIcon release];
  
  [super dealloc];
}


typedef struct {
  MYX_USER_OBJECT_PRIVILEGES *global;
  MYX_USER_OBJECT_PRIVILEGES *schema;
  MYX_USER_OBJECT_PRIVILEGES *table;
  MYX_USER_OBJECT_PRIVILEGES *column;
  MYX_USER_OBJECT_PRIVILEGES *routine;
} ALL_PRIVILEGES;

static ALL_PRIVILEGES *get_all_privileges(MYSQL *mysql)
{
  ALL_PRIVILEGES *privs= g_new0(ALL_PRIVILEGES, 1);
  privs->global= myx_get_privilege_struct(mysql, "", MYX_UOP_GLOBAL);
  privs->schema= myx_get_privilege_struct(mysql, "x", MYX_UOP_SCHEMA);
  privs->table= myx_get_privilege_struct(mysql, "x.x", MYX_UOP_TABLE);
  privs->column= myx_get_privilege_struct(mysql, "x.x.x", MYX_UOP_COLUMN);
  privs->routine= myx_get_privilege_struct(mysql, "x.x", MYX_UOP_ROUTINE);
  return privs;
}


- (void)handleDefaultsChange:(NSNotification*)notif
{
  NSUserDefaults *defaults= [NSUserDefaults standardUserDefaults];

  if ([defaults boolForKey:@"DisplayGlobalPrivileges"] != globalVisible)
  {
    globalVisible= !globalVisible;
    
    if (!globalVisible)
      [tabView removeTabViewItem:globalPageView];
    else
      [tabView insertTabViewItem:globalPageView atIndex:1];
  }
  
  if ([defaults boolForKey:@"DisplayTablePrivileges"] != tableVisible)
  {
    tableVisible= !tableVisible;
	
    if (!tableVisible)
      [tabView removeTabViewItem:tablePageView];  
    else
      [tabView insertTabViewItem:tablePageView atIndex:globalVisible?3:2];
  }
}


- (void)setup
{
  globalPageView= [[tabView tabViewItemAtIndex:1] retain];
  tablePageView= [[tabView tabViewItemAtIndex:3] retain];
  
  globalVisible= YES;
  tableVisible= YES;  

  [self handleDefaultsChange:nil];

  // listen for changes in the schema ds
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(schemaDSChanged:)
                                               name:MASchemaDataSourceChanged
                                             object:_owner];
  
  // setup data sources
  _userDS= [[MUserListDataSource alloc] init];
  [_userDS setDefaultUserIcon: [NSImage imageNamed:@"16x16_User.png"]];
  [_userDS setUserInfoFetcher: self selector:@selector(requestUserInfo:)];  
  [userOutline setDataSource: _userDS];
  [[_owner dispatcher] performCallback:(void*(*)(MYSQL*))myx_get_user_names
                      finishedSelector:@selector(userNamesArrived:names:)
                              argument:nil
                                target:self];
  _schemaDS= [[MFilteredSchemaDataSource alloc] initWithDataSource:[_owner sharedSchemaDS]];
  [schemaOutline setDataSource: _schemaDS];
  MXExpandOutline(schemaOutline, NO);

  _objectDS= [[MFilteredSchemaDataSource alloc] initWithDataSource:[_owner sharedObjectDS]];
  [tableOutline setDataSource: _objectDS];
  MXExpandOutline(tableOutline, NO);
  {
    NSMutableArray *array;
    ALL_PRIVILEGES *privs;
    unsigned int i;
    privs= [[_owner dispatcher] performCallback:(void*(*)(MYSQL*))get_all_privileges
                                  waitForWindow:[_owner window]
                                        message:@"Retrieving privilege templates..."];
    array= [NSMutableArray array];
    if (privs->global)
    {
      for (i= 0; i < privs->global->user_privileges_num; i++)
      {
        char *end= strstr((char*)privs->global->user_privileges[i],"_priv");
        if (end)
          [array addObject: [NSString stringWithCString: (char*)privs->global->user_privileges[i]
                                                 length: end-(char*)privs->global->user_privileges[i]]];
      }
    }
    _globalPrivs= privs->global;
    _globalPrivDS= [[PrivilegeDataSource alloc] initWithPrivileges: array
                                                       sourceTable: globalAvailableTable
                                                              type: PGlobal];
    [globalAssignedTable setDataSource: _globalPrivDS];
    [globalAssignedTable sizeToFit];
    [globalAvailableTable setDataSource: _globalPrivDS];
    [globalAvailableTable sizeToFit];

    array= [NSMutableArray array];
    if (privs->schema)
    {
      for (i= 0; i < privs->schema->user_privileges_num; i++)
      {
        char *end= strstr((char*)privs->schema->user_privileges[i],"_priv");
        if (end)
          [array addObject: [NSString stringWithCString: (char*)privs->schema->user_privileges[i]
                                                 length: end-(char*)privs->schema->user_privileges[i]]];
      }
    }
    _schemaPrivs= privs->schema;
    _schemaPrivDS= [[PrivilegeDataSource alloc] initWithPrivileges: array
                                                       sourceTable: schemaAvailableTable
                                                              type: PSchema];
    [schemaAssignedTable setDataSource: _schemaPrivDS];
    [schemaAssignedTable sizeToFit];
    [schemaAvailableTable setDataSource: _schemaPrivDS];
    [schemaAvailableTable sizeToFit];

    array= [NSMutableArray array];
    if (privs->table)
    {
      for (i= 0; i < privs->table->user_privileges_num; i++)
      {
        char *end= strchr((char*)privs->table->user_privileges[i], '=');
        if (end && g_str_has_prefix((char*)privs->table->user_privileges[i], "Table_priv_"))
        {
          char *begin= (char*)privs->table->user_privileges[i]+sizeof("Table_priv_")-1;
          [array addObject: [NSString stringWithCString: begin
                                                 length: end - begin]];
        }
        else if (end && g_str_has_prefix((char*)privs->table->user_privileges[i], "Column_priv_"))
        {
          char *begin= (char*)privs->table->user_privileges[i]+sizeof("Column_priv_")-1;
          [array addObject: [@"Column " stringByAppendingString: [NSString stringWithCString: begin
                                                 length: end - begin]]];
        }

      }
    }
    _tablePrivs= privs->table;
    _tablePrivDS= [[PrivilegeDataSource alloc] initWithPrivileges: array
                                                      sourceTable: tableAvailableTable
                                                             type: PTable];
    [tableAssignedTable setDataSource: _tablePrivDS];
    [tableAvailableTable setDataSource: _tablePrivDS];

    array= [NSMutableArray array];
    if (privs->routine)
    {
      for (i= 0; i < privs->routine->user_privileges_num; i++)
      {
        char *end= strchr((char*)privs->routine->user_privileges[i], '=');
        if (end && g_str_has_prefix((char*)privs->routine->user_privileges[i], "Proc_priv_"))
        {
          char *begin= (char*)privs->routine->user_privileges[i]+sizeof("Proc_priv_")-1;
          [array addObject: [NSString stringWithCString: begin
                                                 length: end - begin]];
        }
      }
    }
    _routinePrivs= privs->routine;
    _routinePrivDS= [[PrivilegeDataSource alloc] initWithPrivileges: array
	                                                      sourceTable: tableAvailableTable
                                                               type: PRoutine];
    
    array= [NSMutableArray array];
    if (privs->column)
    {
      for (i= 0; i < privs->column->user_privileges_num; i++)
      {
        char *end= strchr((char*)privs->column->user_privileges[i], '=');
        if (end && g_str_has_prefix((char*)privs->column->user_privileges[i], "Column_priv_"))
        {
          char *begin= (char*)privs->column->user_privileges[i]+sizeof("Column_priv_")-1;
          [array addObject: [NSString stringWithCString: begin
                                                 length: end - begin]];
        }
      }
    }
    _columnPrivs= privs->column;
    _columnPrivDS= [[PrivilegeDataSource alloc] initWithPrivileges: array
                                                       sourceTable: tableAvailableTable
                                                              type: PColumn];

    g_free(privs);
  }
  
  _privilegeIcon= [[NSImage imageNamed:@"16x16_Right.png"] retain];

  [self showUser:nil host:nil];
}


- (id)initWithOwner: (id<MAdministratorProtocol>)owner
{
  self= [super initWithNibFile: @"UserAdministration" panelOwner: owner];
  _defaultFrame= [[self topView] frame];
  
  if ([owner mysql])
    [self setup];
  else
    [[topBox viewWithTag:1000] setStringValue:@"Not Connected"];
  return self;
}


- (void)awakeFromNib
{
  [userOutline setTarget:self];
  [userOutline setDeleteAction:@selector(deleteUserPressed:)];
  
  [[[topBox viewWithTag:TAG_UserAdd] cell] setImageDimsWhenDisabled:NO];
  [[[topBox viewWithTag:TAG_UserRemove] cell] setImageDimsWhenDisabled:NO];
  [[[topBox viewWithTag:TAG_HostAdd] cell] setImageDimsWhenDisabled:NO];
  [[[topBox viewWithTag:TAG_HostRemove] cell] setImageDimsWhenDisabled:NO];
  
  [[topBox viewWithTag:300] setEnabled:NO];
  [[topBox viewWithTag:301] setEnabled:NO];
  [tabView setEnabledRecursive:NO];
      
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(handleDefaultsChange:)
                                               name:NSUserDefaultsDidChangeNotification
                                             object:nil];
    

  [self setButton:TAG_UserAdd enabled:YES image:@"adduser"];
  [self setButton:TAG_UserRemove enabled:NO image:@"deluser"];
  [self setButton:TAG_HostAdd enabled:NO image:@"addhost"];
  [self setButton:TAG_HostRemove enabled:NO image:@"delhost"];
  
  [[topBox viewWithTag:1000] setHidden:NO];
  [tabView setHidden:YES];
}


- (void)showUser:(MUserItem*)anUser host:(NSString*)host
{
  [self showUser:anUser host:host keepAccountInfo:NO];
}

- (void)showUser:(MUserItem*)anUser host:(NSString*)host keepAccountInfo:(BOOL)keep
{  
  MYX_USER *user= [anUser userInfo];
  NSString *tmp;
  int i;
  BOOL enabled= host!=nil;
  BOOL flag;
  
  flag= anUser!=nil;
  
  [tabView setEnabledRecursive:flag];
//  [[topBox viewWithTag:300] setEnabled:flag && [self needsSave]];
//  [[topBox viewWithTag:301] setEnabled:flag && [self needsSave]];  
  
#define STRVALUE(v) (user ? (user->v ?[NSString stringWithUTF8String:(char*)(user->v)]:@"") : @"")
  [usernameText setStringValue: STRVALUE(user_name)];
  if (!keep)
  {
    [passwordText setStringValue:@"________"]; // this is for telling the password didn't change
    [confirmText setStringValue:@"________"];
    [fullNameText setStringValue: STRVALUE(full_name)];
    [descriptionText setStringValue: STRVALUE(description)];
    [emailText setStringValue: STRVALUE(email)];
    [contactText setString: STRVALUE(contact_information)];
    if ([anUser icon])
      [iconView setImage: [anUser icon]];
    else if (user && user->icon)
      [iconView setImage: 
        [[[NSImage alloc] initWithData:[NSData dataWithBytes:user->icon 
                                                      length:user->icon_length]] autorelease]];
    else
      [iconView setImage: nil];
  }
  
  
  // enable/disable controls
  for (i=1; i < 4; i++)
  {
    [self setPage:i enabled:enabled];
  }
  
  if (user)
  {
    if (host)
      tmp= [NSString stringWithFormat:@"%s@%@",user->user_name, host];
    else
      tmp= @"Not Applicable (no host selected)";
  }
  else
    tmp= @"No user selected";
  [userLabel1 setStringValue: tmp];
  [userLabel1 setTextColor: user&&host ? [NSColor blackColor] : [NSColor redColor]];
  [userLabel2 setStringValue: tmp];
  [userLabel2 setTextColor: user&&host ? [NSColor blackColor] : [NSColor redColor]];
  [userLabel3 setStringValue: tmp];
  [userLabel3 setTextColor: user&&host ? [NSColor blackColor] : [NSColor redColor]];
  [userLabel4 setStringValue: tmp];
  [userLabel4 setTextColor: user&&host ? [NSColor blackColor] : [NSColor redColor]];
#undef VALUE

  if (user && host)
  {
    MYX_USER_OBJECT_PRIVILEGES *privs= [anUser findPrivilegesForObject:nil host:host];
    if (!privs)
      privs= [anUser addPrivileges:_globalPrivs  forObject:nil  host:host];
    [_globalPrivDS setPrivileges: privs];
    [globalAssignedTable reloadData];
    [globalAvailableTable reloadData];
    [globalAssignedTable deselectAll:self];
    [globalAvailableTable deselectAll:self];

    [_schemaPrivDS setPrivileges: NULL];
    [schemaOutline deselectAll:nil];
    [schemaAssignedTable reloadData];
    [schemaAvailableTable reloadData];
    [schemaAssignedTable deselectAll:self];
    [schemaAvailableTable deselectAll:self];
    [_tablePrivDS setPrivileges: NULL];
    [_columnPrivDS setPrivileges: NULL];
    [_routinePrivDS setPrivileges: NULL];
    [tableOutline deselectAll:nil];
    [tableAssignedTable reloadData];
    [tableAvailableTable reloadData];
    [tableAssignedTable deselectAll:self];
    [tableAvailableTable deselectAll:self];
  }
}

//===================== Delegate for TextFields =====================
- (void)controlTextDidChange:(NSNotification *)aNotification
{
  if ([aNotification object] == addHostText)
  {
    if ([[addHostMatrix cellWithTag:2] state] != NSOnState)
      [[addHostMatrix cellWithTag:2] performClick:self];
  }
  else
    [self setNeedsSave:YES];
}

- (void)controlTextDidEndEditing:(NSNotification *)aNotification
{
  if ([aNotification object] == usernameText)
  {
    MUserItem *item= [self selectedUser];
    [item setName:[usernameText stringValue]];
    [userOutline reloadItem:item reloadChildren:NO];
  }
}

- (BOOL)control:(NSControl *)control textShouldEndEditing:(NSText *)fieldEditor
{  
  if (control == usernameText)
  {
    id item= [_userDS itemForUser:[fieldEditor string]]; 
    if (item != nil && item != [self selectedUser])
    {
      NSRunAlertPanel(nil, @"The typed username is already in use by another account. Please choose a different one.",
                      @"OK", nil, nil);
      return NO;
    }
  }
  return YES;
}


- (void)textDidChange:(NSNotification *)aNotification
{
  [self setNeedsSave:YES];
}


- (IBAction)filterUserList:(id)sender
{
  [_userDS filterUsers:[sender stringValue]];
  [userOutline reloadData];
}


- (IBAction)filterTableList:(id)sender
{
  [_objectDS performSearch:[sender stringValue]];
  [tableOutline reloadData];
}


- (IBAction)filterSchemaList:(id)sender
{
  [_schemaDS performSearch:[sender stringValue]];
  [schemaOutline reloadData];
}


- (void)setNeedsSave:(BOOL)flag
{
  [super setNeedsSave:flag];
  [[topBox viewWithTag:300] setEnabled:flag];
  [[topBox viewWithTag:301] setEnabled:flag];
}


//===================== Delegate for Outlines =======================


- (void)outlineViewSelectionDidChange:(NSNotification *)notification
{
  NSString *host= [self selectedHost];
  MUserItem *user= [self selectedUser];

  if ([notification object] == userOutline)
  {  
    int row;
    BOOL changedUser= NO;
    
    // select a different user from current one
    if (_oldUser && _oldUser != user)
    {
      changedUser= YES;
      if ([self needsSave])
      {
        MYX_USER *user= [_oldUser userInfo];
        NSString *tmp;
        BOOL cancel= NO;
        
        if (_oldHost)
          tmp= [NSString stringWithFormat:@"Save modifications made to the account '%s' connecting from host '%@'?",
            user->user_name, _oldHost];
        else
          tmp= [NSString stringWithFormat:@"Save modifications made to the account '%s'?",
            user->user_name];
        
        switch (NSRunAlertPanel(@"Save Changes", tmp,
                                @"Save Changes", @"Discard Changes", @"Cancel"))
        {
          case NSAlertDefaultReturn:
            if (![self commitUserChanges:_oldUser])
              cancel= YES;
            break;
          case NSAlertAlternateReturn:
            [self discardUserChanges:_oldUser];
            break;
          case NSAlertOtherReturn:
            cancel= YES;
            break;
        }

        if (cancel)
        {
          [userOutline selectRow:_oldRow byExtendingSelection:NO];
          return;
        }
      }
    }
    [_oldUser release];
    _oldUser= [user retain];
    [_oldHost release];
    _oldHost= [host retain];

    _oldRow= row= [userOutline selectedRow];
    if (row < 0)
    {
      [self showUser:nil host:nil];
      
      [self setButton:TAG_UserRemove enabled:NO image:@"deluser"];
      [self setButton:TAG_HostAdd enabled:NO image:@"addhost"];
      [self setButton:TAG_HostRemove enabled:NO image:@"delhost"];
      
      [[topBox viewWithTag:1000] setHidden:NO];
      [tabView setHidden:YES];
    }
    else
    {
      id item= [userOutline itemAtRow: row];
      if ([item isMemberOfClass: [MUserHostItem class]]) // selected a host
      {
        MUserItem *info= [(MUserHostItem*)item user];
        [self showUser:info host:[self selectedHost] keepAccountInfo:!changedUser];
      }
      else if ([item isMemberOfClass: [MUserItem class]]) // selected a user
      {
        if (![(MUserItem*)item userInfo])
        {
          [self showUser:nil host:nil];
          [self requestUserInfo:[item name]];
        }
        else
          [self showUser:item host:nil keepAccountInfo:!changedUser];        
      }
      [self setButton:TAG_HostRemove enabled:YES image:@"delhost"];
      [self setButton:TAG_UserRemove enabled:YES image:@"deluser"];
      [self setButton:TAG_HostAdd enabled:YES image:@"addhost"];      
    }
    [schemaOutline deselectAll:nil];
    [tableOutline deselectAll:nil];

    [[topBox viewWithTag:1000] setHidden:YES];
    [tabView setHidden:NO];
  }
  else if ([notification object] == schemaOutline)
  {
    MSchemaItem *item= nil;

    int row= [schemaOutline selectedRow];
    if (row >= 0)
      item= [schemaOutline itemAtRow:row];

    if (item && item->type != MSchemaItemType)
      item= nil;
    if (item)
    {
      MYX_SCHEMA *schema= [item schema];
      MYX_USER_OBJECT_PRIVILEGES *privs= NULL;

      if (schema && host && user)
      {
        NSString *object= [NSString stringWithFormat:@"%s",schema->schema_name];

        privs= [user findPrivilegesForObject:object host:host];
        if (!privs)
          privs= [user addPrivileges:_schemaPrivs forObject:object host:host];
        [_schemaPrivDS setPrivileges:privs];
      }
      else
      {
        [_schemaPrivDS setPrivileges:NULL];
      }
      [schemaAssignedTable reloadData];
      [schemaAvailableTable reloadData];
    }
    // refresh enabled controls
    [self setPage:2 enabled:YES];
  }
  else if ([notification object] == tableOutline)
  {
    MSchemaItem *item= nil;
    MYX_USER_OBJECT_PRIVILEGES *privs= NULL;
    BOOL editable= NO;

    int row= [tableOutline selectedRow];
    if (row >= 0)
      item= [tableOutline itemAtRow:row];
    if (item && item->type == MTableItemType)
    {
      MYX_SCHEMA *schema= [item schema];
      MYX_SCHEMA_TABLE *table= [item table];
      if (table && host && user)
      {
        NSString *object=[NSString stringWithFormat:@"%s.%s",schema->schema_name,table->table_name];
        privs= [user findPrivilegesForObject:object host:host];
        if (!privs)
          privs= [user addPrivileges:_tablePrivs forObject:object host:host];
        [_tablePrivDS setPrivileges:privs];
        editable= YES;
      }
      else
      {
        [_tablePrivDS setPrivileges:NULL];
      }
      [tableAssignedTable setDataSource:_tablePrivDS];
      [tableAvailableTable setDataSource:_tablePrivDS];
      [tableAssignedTable reloadData];
      [tableAvailableTable reloadData];
    }
    else if (item && item->type == MColumnItemType)
    {
      MYX_SCHEMA_TABLE_COLUMN *column= [item column];
      MYX_SCHEMA *schema= [item schema];
      MYX_SCHEMA_TABLE *table= [item table];
      
      if (column && host && user)
      {
        NSString *object= [NSString stringWithFormat:@"%s.%s.%s",schema->schema_name,table->table_name,column->column_name];
        privs= [user findPrivilegesForObject:object host:host];
        if (!privs)
          privs= [user addPrivileges:_columnPrivs forObject:object host:host];
        [_columnPrivDS setPrivileges:privs];
        editable= YES;
      }
      else
      {
        [_columnPrivDS setPrivileges:NULL];
      }
      [tableAssignedTable setDataSource:_columnPrivDS];
      [tableAvailableTable setDataSource:_columnPrivDS];
      [tableAssignedTable reloadData];
      [tableAvailableTable reloadData];
    }
    else if (item && item->type == MSPItemType)
    {
      MYX_SCHEMA *schema= [item schema];
      MYX_SCHEMA_STORED_PROCEDURE *proc= [item sp];
      if (proc && host && user)
      {
        NSString *object=[NSString stringWithFormat:@"%s.%s",schema->schema_name,proc->name];
        privs= [user findPrivilegesForObject:object host:host];
        if (!privs)
          privs= [user addPrivileges:_routinePrivs forObject:object host:host];
        [_routinePrivDS setPrivileges:privs];
        editable= YES;
      }
      else
      {
        [_routinePrivDS setPrivileges:NULL];
      }
      [tableAssignedTable setDataSource:_routinePrivDS];
      [tableAvailableTable setDataSource:_routinePrivDS];
      [tableAssignedTable reloadData];
      [tableAvailableTable reloadData];
    }
    // refresh enabled controls
    [self setPage:3 enabled:editable];
  }
}

- (void)outlineView:(NSOutlineView *)outlineView 
    willDisplayCell:(id)cell 
     forTableColumn:(NSTableColumn *)tableColumn 
               item:(id)item
{
  if (outlineView == schemaOutline || outlineView == tableOutline)
  {
    if ([item respondsToSelector:@selector(icon)])
      [cell setImage:[item icon]];
    

  }
  else if (outlineView == userOutline)
  {
    if ([item isMemberOfClass:[MUserItem class]])
      [cell setImage: [_userDS defaultUserIcon]];
    else
      [cell setImage: nil];
  }
}

- (void)tableView:(NSTableView *)aTableView 
  willDisplayCell:(id)aCell
   forTableColumn:(NSTableColumn *)aTableColumn
              row:(int)rowIndex
{
  [aCell setImage: _privilegeIcon];
}

@end
