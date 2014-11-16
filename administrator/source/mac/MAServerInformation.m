//
//  MAServerInformation.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Thu Jun 24 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MAServerInformation.h"
#import <MySQLToolsCommon/MConnectionInfo.h>
#import "MAdministrator.h"

#import <MySQLToolsCommon/MControlForm.h>

@implementation MAServerInformation

+ (NSImage*)icon
{
  return [NSImage imageNamed:@"OSX-Icons_33.png"];
}

+ (NSString*)label
{
  return @"Information";
}

+ (NSString*)toolTip
{
  return @"Information about the connected MySQL server instance.";
}

+ (BOOL)needsConnection
{
  return NO;
}


- (id)initWithOwner: (MAdministrator*)owner
{
  self= [super initWithNibFile: @"ServerInformation" panelOwner: owner];
  
  _defaultFrame= [[self topView] frame];
#if 0
#if 0
  NSMatrix *matrix= [[NSMatrix alloc] initWithFrame:NSMakeRect(10,10,500,500)];
  NSPopUpButtonCell *cell= [[NSPopUpButtonCell alloc] initTextCell:@"X" pullsDown:NO];
//  [cell setEditable:YES];
  [matrix addRow];
  [matrix putCell:cell atRow:0 column:0];
  [topBox addSubview:matrix];
#else
  MControlForm *form= [[MControlForm alloc] initWithFrame:NSMakeRect(10, 10, 400, 400)];
  [topBox addSubview:form];
  [form addTextFieldWithTitle:@"Name" description:@"Name of the something eqwhle hejqeh qeqeqeqewqeqjrjwqerqw."];
  [form addTextFieldWithTitle:@"Address" description:@"Write down the address info for the name."];
  [form addPopUpWithTitle:@"Engine"
                    items:[NSArray arrayWithObjects:@"MyISAM", @"InnoDB", @"Falcon", @"CSV", @"Blackhole", nil]
              description:@"Begin qrqweryewquir weqruwe qrywuer qrh fdjhfg sdahjf  afdaf a end."];
#endif
#endif
  return self;
}

- (void)didShow
{
  [self updateStatus];
}
  
  
- (void)updateStatus
{
  NSImage *image;
  NSString *status;
  MYSQL *mysql;
  MConnectionInfo *conn;
  MYX_MACHINE_INFO *info;

  switch ([_owner serverStatus])
  {
    case 0:
      image= [NSImage imageNamed:@"service_status_stopped.png"];
      status= @"Server is Stopped";
      break;
    case 1:
      image= [NSImage imageNamed: @"service_status_running.png"];
      status= @"Server is Running";
      break;
    default:
      image= [NSImage imageNamed: @"service_status_unknown.png"];
      status= @"Cannot determine server status";
      break;
  }

  [statusImage setImage: image];
  [statusLabel setStringValue: status];
  
  conn= [_owner serverInfo];
  
  if ((mysql= [_owner mysql]))
  {
    MYX_USER_CONNECTION *uconn= [conn createUserConnection];
    
    info= myx_get_server_info(uconn, mysql);
    if (info)
    {      
      [serverInfoLabel setStringValue:
        [NSString stringWithFormat:@"%s\r%s\r%s",
                    info->version?:"", info->network_name?:"", info->IP?:""]];
      myx_free_pc_info(info);
    }
    
    if (mysql->unix_socket)
    {
      [instanceInfoCaption setStringValue: @"User:\rHost:\rSocket:"];
      [instanceInfoLabel setStringValue: 
        [NSString stringWithFormat:@"%@\r%@\r%s",
                   [conn username], [conn hostname], mysql->unix_socket]];      
    }
    else
    {
      [instanceInfoCaption setStringValue: @"User:\rHost:\rPort:"];
      [instanceInfoLabel setStringValue: 
        [NSString stringWithFormat:@"%@\r%@\r%i",
                   [conn username], [conn hostname], [conn port]]];
    }
    
    myx_free_user_connection_content(uconn);
    g_free(uconn);
  }
  else
  {
    [instanceInfoLabel setStringValue: @"\nNot Connected"];
    [serverInfoLabel setStringValue:@"\nNot Connected"];
  }
  
  info= myx_get_client_info(mysql);
  if (info)
  {
    [clientInfoLabel setStringValue:
      [NSString stringWithFormat:@"%s\r%s\r%s\r%s\r%s",
                  info->version?:"", info->network_name?:"", 
                       info->IP?:"", info->OS?:"", info->hardware?:""]];
    //XXX check how to retrieve hw info
    myx_free_pc_info(info);
  }
  else
    [clientInfoLabel setStringValue:@""];
}

@end
