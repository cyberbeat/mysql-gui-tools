//
//  MPreferences.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Fri Jul 09 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MPreferences.h"


static MPreferences *instance= nil;

@implementation MPreferences

+ (MPreferences*)preferences
{
  if (!instance)
    instance= [[MPreferences alloc] init];
  return instance;
}

+ (void)setInstance:(MPreferences*)pref
{
  instance= pref;
}

+ (NSString*)checkDirectory:(NSString*)path
{
  NSString *home= NSHomeDirectory();
  NSString *result;
  FSRef ref;
  
  if (FSFindFolder(kUserDomain, 
                   kApplicationSupportFolderType,
                   YES,
                   &ref) == noErr)
  {
    CFURLRef urlRef;
    
    urlRef= CFURLCreateFromFSRef(kCFAllocatorSystemDefault, &ref);
    if (urlRef)
      home= (NSString*)CFURLCopyFileSystemPath(urlRef, kCFURLPOSIXPathStyle);
  }

  if (path)
    result= [NSString stringWithFormat:@"%@/MySQL/%@",home,path];
  else
    result= [NSString stringWithFormat:@"%@/MySQL",home];
  
  if ([[NSFileManager defaultManager] fileExistsAtPath:result])
    return result;
  
  if (path)
  {
    NSArray *parts= [path pathComponents];
    NSMutableString *comp= [[[NSMutableString alloc] initWithFormat:@"%@/MySQL/",home] autorelease];
    int i;
    
    if (![[NSFileManager defaultManager] fileExistsAtPath:comp])
    {
      if (![[NSFileManager defaultManager] createDirectoryAtPath:comp attributes:0])
      {
        NSLog(@"Could not create directory %@", comp);
        return nil;
      }
    }
    for (i= 0; i < [parts count]; i++)
    {
      [comp appendFormat:@"/%@",[parts objectAtIndex:i]];
      if (![[NSFileManager defaultManager] fileExistsAtPath:comp])
      {
        if (![[NSFileManager defaultManager] createDirectoryAtPath:comp attributes:0])
        {
          NSLog(@"Could not create directory %@", comp);
          return nil;
        }
      }
    }
  }
  return result;
}

- (NSString*)pathForPreferences
{
  return [MPreferences checkDirectory:nil];
}


- (NSString*)pathForFile:(NSString*)file
{
  return [NSString stringWithFormat:@"%@/%@", [self pathForPreferences], file];
}


- (void)defaultChanged:(NSNotification*)notif
{
  passwordStorageType= [_defaults integerForKey:@"PasswordStorageType"];
  
}


- (id)init
{
  self= [super init];
  if (self)
  {
    passwordStorageType= MYX_PASSWORD_OS_SPECIFIC;
    
    _defaults= [[NSUserDefaults standardUserDefaults] retain];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(defaultChanged:)
                                                 name:NSUserDefaultsDidChangeNotification
                                               object:_defaults];
    
    [self defaultChanged:nil];
  }
  return self;
}


- (void)dealloc 
{
  [_defaults release];
  [super dealloc];
}


@end
