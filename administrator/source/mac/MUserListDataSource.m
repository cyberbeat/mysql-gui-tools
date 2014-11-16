//
//  MUserListDataSource.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Tue Jun 29 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MUserListDataSource.h"

static void copy_privileges(MYX_USER_OBJECT_PRIVILEGES *dst, MYX_USER_OBJECT_PRIVILEGES *src)
{
  unsigned int i;
  
  dst->user_privileges_num= src->user_privileges_num;
  dst->user_privileges= myx_resize_vector_block(NULL, sizeof(char*), src->user_privileges_num);
  for (i= 0; i < src->user_privileges_num; i++)
    dst->user_privileges[i]= g_strdup((char*)src->user_privileges[i]);
}

@implementation MUserItem

- (id)initNewUser
{
  self= [super init];
  if (self)
  {
    _newUser= YES;
    _user= g_new0(MYX_USER, 1);
    _hostArray= [[NSMutableArray alloc] init];
    _icon = nil;
  }
  return self;
}

- (id)initWithName:(NSString*)name
{
  self= [super init];
  if (self)
  {
    _newUser= NO;
    _username= [name retain];
    _orig_username= [name retain];
    _icon= nil;
  }
  return self;
}

- (void)dealloc
{
  if (_user)
    myx_free_user(_user);
  [_orig_username release];
  [_username release];
  [_hostArray release];
  [_icon release];
  [super dealloc];
}

- (BOOL)isNewUser
{
  return _newUser;
}

- (NSImage*)icon
{
  return _icon;
}

- (NSArray*)hosts
{
  return _hostArray;
}

- (MUserHostItem*)addHost:(NSString*)host
{
  unsigned int i, c= [_hostArray count];
  MUserHostItem *hitem;
  
  for (i= 0; i < c; i++)
  {
    if ([[[_hostArray objectAtIndex:i] hostname] isEqualToString:host])
      return nil;
  }
  
  hitem= [[MUserHostItem alloc] initWithUser:self host:host];
  [_hostArray addObject:hitem];
  [hitem release];
  
  _user->hosts_num++;
  _user->hosts= (char**)myx_resize_vector_block((char*)_user->hosts, sizeof(char*), _user->hosts_num);
  _user->hosts[_user->hosts_num-1]= g_strdup([host UTF8String]);

  return hitem;
}


- (MUserHostItem*)itemForHost:(NSString*)host
{
  unsigned int i, c= [_hostArray count];
  MUserHostItem *hitem;

  for (i= 0; i < c; i++)
  {
    hitem= [_hostArray objectAtIndex:i];
    if ([[hitem hostname] isEqualToString:host])
      return hitem;
  }
  return nil;
}


- (void)removeHost:(NSString*)host
{
  unsigned int i, c= [_hostArray count];
  MUserHostItem *hitem;
  
  for (i= 0; i < c; i++)
  {
    hitem= [_hostArray objectAtIndex:i];
    if ([[hitem hostname] isEqualToString:host])
    {
      for (i= 0; i < _user->hosts_num; i++)
      {
        if (g_utf8_collate(_user->hosts[i], [host UTF8String])==0)
        {
          memmove(_user->hosts+i, _user->hosts+i+1, (_user->hosts_num-i-1)*sizeof(char*));
          _user->hosts_num--;
          break;
        }
      }
      [_hostArray removeObject:hitem];
      break;
    }
  }
  
}

- (void)setUserInfo:(MYX_USER*)info
{
  unsigned int i;
  if (_user)
    myx_free_user(_user);
  _user= info;
  _userInfoSet= YES;

  if (_user)
  {
    _hostArray= [[NSMutableArray alloc] initWithCapacity: _user->hosts_num];
    for (i= 0; i < _user->hosts_num; i++)
    {
      [_hostArray addObject: 
        [[[MUserHostItem alloc] initWithUser: self
                                        host:[NSString stringWithUTF8String: _user->hosts[i]]] autorelease]];
    }
  
    if (_user->icon)
      _icon= [[NSImage alloc] initWithData:[NSData dataWithBytes:_user->icon 
                                                          length:_user->icon_length]];
  }
}

- (MYX_USER*)userInfo
{
  return _user;
}

- (void)setOriginalName:(NSString*)name
{
  _orig_username= [name retain];
}

- (void)setName:(NSString*)name
{
  if (_username != name)
  {
    [_username release];
    _username= [name retain];
    g_free(_user->user_name);
    _user->user_name= g_strdup([name UTF8String]);
  }
}

- (NSString*)name
{
  return _username;
}

- (NSString*)originalName
{
  return _orig_username;
}

- (MYX_USER_OBJECT_PRIVILEGES*)findPrivilegesForObject:(NSString*)object
                                                  host:(NSString*)hostname
{
  MYX_USER_OBJECT_PRIVILEGES *privs;
  unsigned int i;
  const char *name= object ? [object UTF8String] : NULL;
  const char *host= [hostname UTF8String];
  
  NSAssert(host!=NULL, @"not supposed to reach here in this state.");

  for (i= 0; i < _user->user_object_privileges_num; i++)
  {
    privs= _user->user_object_privileges+i;
	
    if (strcmp(privs->host, host)==0)
    {
      if (privs->object_name && name && strcmp(privs->object_name,name)==0)
        return privs;
      else if (!name && (!privs->object_name || !*privs->object_name))
        return privs;
    }
  }
  return NULL;
}

- (MYX_USER_OBJECT_PRIVILEGES*)addPrivileges:(MYX_USER_OBJECT_PRIVILEGES*)privs
                                   forObject:(NSString*)object
                                        host:(NSString*)hostname
{
  const char *name= object ? [object UTF8String] : NULL;
  const char *host= [hostname UTF8String];
  MYX_USER_OBJECT_PRIVILEGES *myptr;
  
  // realloc and get myptr
  _user->user_object_privileges_num++;
  _user->user_object_privileges= (MYX_USER_OBJECT_PRIVILEGES*)myx_resize_vector_block((char*)_user->user_object_privileges,
                                                  sizeof(MYX_USER_OBJECT_PRIVILEGES),
                                                  _user->user_object_privileges_num);
  myptr= _user->user_object_privileges+(_user->user_object_privileges_num-1);

  myptr->host= g_strdup(host);
  myptr->object_name= g_strdup(name);
  
  copy_privileges(myptr, privs);
  
  return myptr;
}

- (void)setSaved
{
  _newUser= NO;
}

@end

//==============================================================================

@implementation MUserHostItem
- (id)initWithUser:(MUserItem*)auser host:(NSString*)ahost
{
  self= [super init];
  if (self)
  {
    _user= [auser retain];
    _host= [ahost retain];
  }
  return self;
}

- (MUserItem*)user
{
  return _user;
}

- (NSString*)hostname
{
  return _host;
}

- (void)dealloc
{
  [_user release];
  [_host release];
  [super dealloc];
}
@end

//==============================================================================

@implementation MUserListDataSource

- (void)filterUsers: (NSString*)filter
{
  int i, c;
  
  if (!_filtered || !filter || !_filter || [filter rangeOfString:_filter].location == NSNotFound)
  {
	if (!filter || [filter length]==0)
	{
	  [_filtered release];
	  _filtered= nil;
	}
	else
	{
	  _filtered= [[NSMutableArray alloc] initWithCapacity:32];
	  c= [_users count];
	  for (i= 0; i < c; i++)
	  {
		id user= [_users objectAtIndex:i];
		if ([[user name] rangeOfString:filter].location != NSNotFound)
		  [_filtered addObject:user];
	  }
	}
  }
  else
  {
	c= [_filtered count];
	for (i= c-1; i >= 0; i--)
	{
	  id user= [_filtered objectAtIndex:i];
	  if ([[user name] rangeOfString:filter].location == NSNotFound)
		[_filtered removeObject:user];
	}
  }

  [_filter release];
  _filter= [filter retain];  
}

- (void)dealloc
{
  [_filtered release];
  [_filter release];
  [_icon release];
  [_users release];
  [_target release];
  [super dealloc];
}


- (void)setDefaultUserIcon:(NSImage*)icon
{
  if (_icon)
    [_icon release];
  _icon= [icon retain];
}

- (NSImage*)defaultUserIcon
{
  return _icon;
}

- (void)setUserInfoFetcher: (id)target selector:(SEL)sel
{
  if (_target)
    [_target release];
  _target= [target retain];
  _fetcher= sel;
}

- (void)setUserNames: (NSArray*)names
{
  unsigned int i;
  NSParameterAssert([names isKindOfClass:[NSArray class]]);
  
  if (_users)
    [_users release];
  //XXX fazer ele atualizar a lista em vez de criar uma nova
  _users= [[NSMutableArray alloc] initWithCapacity: [names count]];
  for (i= 0; i < [names count]; i++)
  {
    MUserItem *item= [[MUserItem alloc] initWithName:[names objectAtIndex:i]];
    [_users addObject:item];
    [item release];
  }
}

- (MUserItem*)itemForUser: (NSString*)name
{
  int i, c= [_users count];
  for (i= 0; i < c; i++)
  {
    MUserItem *item= [_users objectAtIndex: i];
    if ([[item name] isEqualToString: name])
    {
      return item;
    }
  }
  return nil;
}

- (void)addUser:(MUserItem*)user
{
  [_users addObject:user];
  if (_filtered)
  {
	if ([[user name] rangeOfString:_filter].location != NSNotFound)
	  [_filtered addObject:user];
  }
}

- (void)removeUser:(MUserItem*)user
{
  [_users removeObject:user];
  if (_filtered)
	[_filtered removeObject:user];
}

- (id)outlineView:(NSOutlineView *)outlineView 
            child:(int)index 
           ofItem:(id)item
{
  if (item == nil)
  {
    return _filtered ? [_filtered objectAtIndex:index] : [_users objectAtIndex: index];
  }
  else
  {
    MUserItem *uitem= (MUserItem*)item;
    
    if ([uitem hosts] == nil && !uitem->_userInfoSet)
    {
      [_target performSelector:_fetcher withObject:[uitem name]];
      return @"Loading...";
    }
    else
      return [[uitem hosts] objectAtIndex: index];
  }
}

- (BOOL)outlineView:(NSOutlineView *)outlineView 
   isItemExpandable:(id)item
{
  if ([item isMemberOfClass: [MUserItem class]])
    return YES;
  else
    return NO;
}

- (int)outlineView:(NSOutlineView *)outlineView 
numberOfChildrenOfItem:(id)item
{

  if (item == nil)
    return _filtered ? [_filtered count] : [_users count];
  else
  {
    MUserItem *uitem= (MUserItem*)item;

    if (uitem && [uitem hosts])
      return [[uitem hosts] count];
    else
      return 1; // for Loading...
  }
}

- (id)outlineView:(NSOutlineView *)outlineView 
objectValueForTableColumn:(NSTableColumn *)tableColumn 
           byItem:(id)item
{
  if ([item isMemberOfClass: [MUserItem class]])
  {
    if ([[tableColumn identifier] isEqualToString: @"icon"])
    {
      id icon= [item icon];
  
      return icon ? icon : _icon;
    }
    else
      return [item name];
  }
  else
  {
    if ([[tableColumn identifier] isEqualToString: @"icon"])
      return nil;
    else
    {
      if (![item isKindOfClass: [MUserHostItem class]])
        return item;
      else
        return [item hostname];
    }
  }
}


@end
