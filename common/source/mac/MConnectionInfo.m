//
//  MConnectionInfo.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Wed Jun 23 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MConnectionInfo.h"
#import "myxutil.h"

#include <Security/Security.h> // keychain


@implementation MConnectionInfo



static char *password_retrieve(const char *hostname, const char *username, const char *password)
{
  UInt32 password_length= 0;
  void *password_data= NULL;
  char *plaintext_password= NULL;
  char *name= g_strdup_printf("MySQL@%s", hostname);
  
  SecKeychainFindGenericPassword(NULL,
                                 strlen(name),
                                 name, 
                                 username ? strlen(username) : 0,
                                 username?:"",
                                 &password_length,
                                 &password_data,
                                 NULL);
  g_free(name);
  if (password_data)
  {
    plaintext_password= g_malloc0(password_length+1);
    strncpy((char*)plaintext_password, password_data, password_length);
    SecKeychainItemFreeContent(NULL, password_data);
    plaintext_password[password_length]= 0;
  }
  return plaintext_password;
}


static char *password_store(const char *hostname, const char *username, const char *password)
{
  char *name= g_strdup_printf("MySQL@%s", hostname);

  if (hostname && username && password)
    // store in the keychain
    SecKeychainAddGenericPassword(NULL,
                                  strlen(name),
                                  name, 
                                  strlen(username),
                                  username,
                                  strlen((char*)password),
                                  password,
                                  NULL);
  g_free(name);
  return NULL;  
}


+ (void)initialize
{
  static BOOL done= NO;
  if (!done)
  {
    done= YES;
    
    set_os_specific_password_functions(password_store, password_retrieve);
  }
}


+ (MConnectionInfo*)connectionInfoFromUserConnection: (MYX_USER_CONNECTION*)info
{
  return [[[MConnectionInfo alloc] initWithUserConnection:info] autorelease];
}


- (id)initWithUserConnection:(MYX_USER_CONNECTION*)info
{
  self= [super init];
  if (self)
  {
    unsigned int i;
    
    name= [NSStr(info->connection_name) retain];
    username= [NSStr(info->username) retain];
    password= [NSStr(info->password) retain];
    hostname= [NSStr(info->hostname) retain];
    port= info->port;
    schema= [NSStr(info->schema) retain];
    
    advancedOptions= [[NSMutableArray alloc] initWithCapacity:info->advanced_options_num];
    for (i= 0; i < info->advanced_options_num; i++)
      [advancedOptions addObject:NSStr(info->advanced_options[i])];
  }
  return self;
}


- (void)dealloc
{
  [name release];
  [advancedOptions release];
  [username release];
  [password release];
  [hostname release];
  [schema release];
  [super dealloc];
}


- (NSString*)description
{
  if (name)
    return name;
  else
    return [NSString stringWithFormat:@"%@@%@", username, hostname]; 
}


- (NSString*)username
{
  return username;
}

- (NSString*)hostname
{
  return hostname;
}


- (NSString*)password
{
  return password;
}


- (NSString*)socketPath
{
  unsigned int i;
  for (i= 0; i < [advancedOptions count]; i++)
  {
    NSString *option= [advancedOptions objectAtIndex:i];
    
    if ([option compare:@"socket" options:0 range:NSMakeRange(0, 6)])
      return [option substringFromIndex:6];
  }
  return @"";
}


- (int)port
{
  return port;
}

- (NSString*)schema
{
  return schema;
}


- (void)setSchema:(NSString*)aSchema
{
  if (schema != aSchema)
  {
    [schema release];
    schema= [aSchema retain];
  }
}

- (MYX_USER_CONNECTION*)createUserConnection
{
  MYX_USER_CONNECTION *conn= g_new0(MYX_USER_CONNECTION, 1);
  unsigned int i;
  
  conn->username= g_strdup([username UTF8String]);
  conn->password= g_strdup([password UTF8String]);
  conn->hostname= g_strdup([hostname UTF8String]);
  conn->schema= g_strdup([schema UTF8String]);
  conn->port= port;
  conn->advanced_options_num= [advancedOptions count];
  conn->advanced_options= g_new0(char*, conn->advanced_options_num);
  for (i= 0; i < conn->advanced_options_num; i++)
    conn->advanced_options[i]= g_strdup([[advancedOptions objectAtIndex:i] UTF8String]);
  
  return conn;
}

// AppleScript support
- (void)_setOwner:(id)owner
{
  _owner= owner;
}

- (NSScriptObjectSpecifier *)objectSpecifier
{
  NSScriptObjectSpecifier *specifier= [[NSPropertySpecifier allocWithZone:[self zone]]
    initWithContainerSpecifier:[_owner objectSpecifier]
                           key:@"server"];
  return [specifier autorelease];
}


- (BOOL)reconnect:(MYSQL*)mysql
{
  MYX_USER_CONNECTION conn;
  unsigned int i;
  BOOL ok;
  
  conn.username= (char*)[username UTF8String];
  conn.password= (char*)[password UTF8String];
  conn.hostname= (char*)[hostname UTF8String];
  conn.schema= (char*)[schema UTF8String];
  conn.port= port;
  conn.advanced_options_num= [advancedOptions count];
  conn.advanced_options= g_new0(char*, conn.advanced_options_num);
  for (i= 0; i < conn.advanced_options_num; i++)
    conn.advanced_options[i]= (char*)[[advancedOptions objectAtIndex:i] UTF8String];

  if (myx_connect_to_instance(&conn, mysql) != 0)
    ok= NO;
  else
    ok= YES;
  
  g_free(conn.advanced_options);
  
  return ok;
}


- (MYSQL*)connect
{
  MYSQL *mysql= NULL;

  mysql= myx_mysql_init();
  if (![self reconnect:mysql])
  {
    myx_mysql_close(mysql);
    return NULL;
  }
  else
    return mysql;
}


@end
