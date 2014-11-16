#import "MAServerConnections.h"
#import <MySQLToolsCommon/MMySQLDispatcher.h>

@interface UserDataSource: NSObject
{
  NSDictionary *_users;
}

- (void)setUsers:(NSDictionary*)users;

- (int)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(int)rowIndex;

@end

@implementation UserDataSource

- (void)dealloc
{
  [_users release];
  [super dealloc];
}

- (void)setUsers:(NSDictionary*)users
{
  if (_users != users)
  {
    if (_users) [_users release];
    _users= [users retain];
  }
}


- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{  
  if (!_users)
    return 0;
  
  return [_users count];
}


- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(int)rowIndex
{
  NSString *user;
  NSEnumerator *en;

  en= [_users keyEnumerator];
  while ((user= [en nextObject]))
  {
    if (rowIndex-- == 0)
    {
      if (!aTableColumn || [[aTableColumn identifier] isEqualToString:@"user"])
        return user;
      else if ([[aTableColumn identifier] isEqualToString:@"threads"])
        return [[_users objectForKey:user] stringValue];
      else
        return nil;
    }
  }
  return nil;
}

@end



@interface ProcessDataSource : NSObject
{
  MYX_PROCESS_LIST *_plist;
  NSMutableArray *_users;
}

- (void)setProcessList:(MYX_PROCESS_LIST*)plist;
- (void)setUserFilter:(NSMutableArray*)users;
- (NSMutableArray*)userFilter;

- (int)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(int)rowIndex;
@end


@implementation ProcessDataSource

- (void)dealloc
{  
  [_users release];
  [super dealloc];
}

- (void)setProcessList:(MYX_PROCESS_LIST*)plist
{
  _plist= plist;
}

- (void)setUserFilter:(NSMutableArray*)users
{
  if (_users != users)
  {
    if (_users) [_users release];
    _users= [users retain];
  }
}

- (NSMutableArray*)userFilter
{
  return _users;
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  if (!_plist)
    return 0;
  
  if (!_users)
    return _plist->process_infos_num;
  else
  {
    unsigned int i;
    int count= 0;
    for (i= 0; i < _plist->process_infos_num; i++)
    {
      if ([_users indexOfObject:[NSString stringWithUTF8String:(char*)_plist->process_infos[i].user?:""]]!=NSNotFound)
        count++;
    }
    return count;
  }
}

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(int)rowIndex
{
  MYX_PROCESS_INFO *info= NULL;
  if (!_users || [_users count] == 0)
    info= _plist->process_infos+rowIndex;
  else
  {
    unsigned int i;
    int count= 0;
    for (i= 0; i < _plist->process_infos_num; i++)
    {
      if ([_users indexOfObject:[NSString stringWithUTF8String:(char*)_plist->process_infos[i].user]]!=NSNotFound)
      {
        if (count++ == rowIndex)
        {
          info= _plist->process_infos+i;
          break;
        }
      }
    }
  }  
  
  if (info)
  {
    id ident= [aTableColumn identifier];
    if (!aTableColumn || [ident isEqualToString:@"id"])
    {
      return [NSString stringWithUTF8String:(char*)info->id];
    }
    else if ([ident isEqualToString:@"user"])
    {
      return [NSString stringWithUTF8String:(char*)info->user];
    }
    else if ([ident isEqualToString:@"host"])
    {
      return [NSString stringWithUTF8String:(char*)info->host];
    }
    else if ([ident isEqualToString:@"db"])
    {
      return [NSString stringWithUTF8String:(char*)info->db];
    }
    else if ([ident isEqualToString:@"command"])
    {
      return [NSString stringWithUTF8String:(char*)info->command];
    }
    else if ([ident isEqualToString:@"time"])
    {
      return [NSString stringWithUTF8String:(char*)info->time];
    }
    else if ([ident isEqualToString:@"state"])
    {
      return [NSString stringWithUTF8String:(char*)info->state];
    }
    else if ([ident isEqualToString:@"info"])
    {
      if (info->info)
        return [[[NSString alloc] initWithBytesNoCopy:(char*)info->info
                                               length:MIN(strlen((char*)info->info), 1024)
                                             encoding:NSUTF8StringEncoding
                                         freeWhenDone:NO] autorelease];
      else
        return @"";
    }
    
  }
  return nil;
}

@end


@interface MAServerConnections(Private)
- (void)updateProcessList:(id)arg list:(MYX_PROCESS_LIST*)plist;
@end

@implementation MAServerConnections(Private)
- (void)updateProcessList:(id)arg list:(MYX_PROCESS_LIST*)plist
{
  if (!plist && ![[_owner dispatcher] checkConnection] && [[_owner dispatcher] reconnect])
  {
    [self refresh:nil];
    return;
  }
  
  if (_plist)
    myx_free_process_list(_plist);
  _plist= plist;
  
  if (_plist)
  {
    unsigned int i;
    NSMutableArray *oldSelected= [NSMutableArray arrayWithCapacity:1];
    NSIndexSet *indices= [userTable selectedRowIndexes];
    
    for (i= [indices firstIndex]; i != [indices lastIndex]; i= [indices indexGreaterThanIndex:i])
    {
      [oldSelected addObject:[_userDS tableView:userTable
                      objectValueForTableColumn:nil
                                            row:i]];
    }

    [_users removeAllObjects];

    for (i= 0; i < _plist->process_infos_num; i++)
    {
      NSString *s= [NSString stringWithUTF8String:(char*)_plist->process_infos[i].user];
      NSNumber *n;
      if (!(n= [_users objectForKey:s]))
        [_users setObject:[NSNumber numberWithInt:1] forKey:s];
      else
        [_users setObject:[NSNumber numberWithInt:1+[n intValue]] forKey:s];
    }
    
    for (i= 0; i < (unsigned int)[_userDS numberOfRowsInTableView:userTable]; i++)
    {
      if ([oldSelected indexOfObject:[_userDS tableView:userTable 
                              objectValueForTableColumn:nil
                                                    row:i]]!=NSNotFound)
        [userTable selectRow:i byExtendingSelection:YES];
    }
  }
  
  [_connectionsDS setProcessList:_plist];
  [_connectionsDS setUserFilter:[[[NSMutableArray alloc] init] autorelease]];
  [_fullDS setProcessList:_plist];
  [_userDS setUsers:_users];
  [connectionsTable reloadData];
  [fullTable reloadData];
  [userTable reloadData];
  
  [self tableViewSelectionDidChange:nil];
}

@end

@implementation MAServerConnections

- (IBAction)killConnection:(id)sender
{
  NSIndexSet *rows;
  unsigned int row;
  id table;
  id ds;
  
  if ([tabView indexOfTabViewItem:[tabView selectedTabViewItem]]==0)
    table= fullTable;
  else
    table= connectionsTable;
  
  rows= [table selectedRowIndexes];
  ds= [table dataSource];

  for (row= [rows firstIndex]; row <= [rows lastIndex]; row= [rows indexGreaterThanIndex:row])
  {
    id pid= [ds tableView:table objectValueForTableColumn:nil row:row];
    [[_owner dispatcher] performCallback:(void*(*)(MYSQL*,void*))myx_kill_thread
                                argument:(void*)[pid intValue]
                        finishedSelector:nil
                                argument:nil
                                  target:nil];
    if (![[_owner dispatcher] checkConnection]) [[_owner dispatcher] reconnect];
  }
  [self refresh:self];
}

- (IBAction)killUser:(id)sender
{
  int row;
  id ds;
  int count;
  
  if ([tabView indexOfTabViewItem:[tabView selectedTabViewItem]]==0)
    return;
  
  ds= [connectionsTable dataSource];
  
  count= [ds numberOfRowsInTableView:connectionsTable];
  for (row= 0; row < count; row++)
  {
    id pid= [ds tableView:connectionsTable objectValueForTableColumn:nil row:row];
    [[_owner dispatcher] performCallback:(void*(*)(MYSQL*,void*))myx_kill_thread
                                argument:(void*)[pid intValue]
                        finishedSelector:nil
                                argument:nil
                                  target:nil];
  }
  [self refresh:self];
  
  [self tableViewSelectionDidChange:nil];
}

- (IBAction)refresh:(id)sender
{
  [[_owner dispatcher] performCallback:(void*(*)(MYSQL*))myx_get_process_list
                      finishedSelector:@selector(updateProcessList:list:)
                              argument:nil
                                target:self];
}


+ (NSImage*)icon
{
  return [NSImage imageNamed:@"OSX-Icons_41.png"];
}

+ (NSString*)label
{
  return @"Connections";
}

+ (NSString*)toolTip
{
  return @"Current connections to the MySQL server.";
}

- (void)handleDefaultsChange:(NSNotification*)notif
{
  NSUserDefaults *defaults= [NSUserDefaults standardUserDefaults];
  int delay= [defaults integerForKey:@"ConnectionListRefreshDelay"];
  
  if (!_timer || (int)[_timer timeInterval] != delay)
  {
    [_timer invalidate];
    [_timer release];
    _timer= nil;
  }
  
  if (!_timer && delay > 0)
  {
    _timer= [NSTimer scheduledTimerWithTimeInterval:delay
                                             target:self
                                           selector:@selector(refresh:)
                                           userInfo:nil
                                            repeats:YES];
    [_timer retain];
  }
}


- (void)awakeFromNib
{
  _connectionsDS= [[ProcessDataSource alloc] init];
  _fullDS= [[ProcessDataSource alloc] init];
  _userDS= [[UserDataSource alloc] init];
  [fullTable setDataSource:_fullDS];
  [connectionsTable setDataSource:_connectionsDS];
  [userTable setDataSource:_userDS];
  
  [self handleDefaultsChange:nil];
  
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(handleDefaultsChange:)
                                               name:NSUserDefaultsDidChangeNotification
                                             object:nil];
}


- (id)initWithOwner: (id<MAdministratorProtocol>)owner
{
  self= [super initWithNibFile: @"ServerConnections" panelOwner: owner];
  
  if (self)
  {
    _defaultFrame= [[self topView] frame];
  
    _users= [[NSMutableDictionary alloc] init]; 
    [killUserButton setEnabled:NO];
    [killButton setEnabled:NO];
  }
  return self;
}

- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];

  if (_timer)
    [_timer invalidate];
  [_timer release];

  [_connectionsDS release];
  [_fullDS release];
  [_userDS release];
  [_users release];
  if (_plist)
    myx_free_process_list(_plist);
  [super dealloc];
}

- (void)didShow
{
  [self refresh:self];
}

- (BOOL)willClose
{
  [_timer invalidate];
  return YES;
}

//==================== Delegates =======================

- (void)tableViewSelectionDidChange:(NSNotification *)aNotification
{
  if (!aNotification || [aNotification object] == userTable)
  {
    NSIndexSet *iset= [userTable selectedRowIndexes];
    NSMutableArray *array= [_connectionsDS userFilter];
    NSEnumerator *en;
    NSString *user;
    int i;
        
    [array removeAllObjects];
    en= [_users keyEnumerator];
    i= 0;
    while ((user= [en nextObject]))
    {
      if ([iset containsIndex:i])
        [array addObject: user];
      i++;
    }

    [connectionsTable reloadData];
    
    if ([iset count] == 0)
      [killUserButton setEnabled:NO];
    else
      [killUserButton setEnabled:[tabView indexOfTabViewItem:[tabView selectedTabViewItem]]==1];
  }
  else
  {
    if ([tabView indexOfTabViewItem:[tabView selectedTabViewItem]] == 0)
    {
      [killButton setEnabled:[fullTable numberOfSelectedRows]>0];
    }
    else
    {
      [killButton setEnabled:[connectionsTable numberOfSelectedRows]>0];
    }
  }
}


- (void)tabView:(NSTabView *)tab didSelectTabViewItem:(NSTabViewItem *)tabViewItem
{
  if ([tab indexOfTabViewItem:tabViewItem] == 0)
    [killUserButton setEnabled:NO];
  else
    [killUserButton setEnabled:[userTable numberOfSelectedRows]>0];
}

@end
