#import "MATableMaintenanceController.h"
#import <MySQLToolsCommon/MMySQLDispatcher.h>
#include "myx_admin_public_interface.h"

@implementation MATableMaintenanceController

- (char*)tableList
{
  unsigned int i;
  NSMutableString *list= [[[NSMutableString alloc] init] autorelease];
  for (i= 0; i < [_tables count]; i++)
  {
    if (i > 0)
      [list appendFormat:@" %@",[_tables objectAtIndex:i]];
    else
      [list appendFormat:@"%@",[_tables objectAtIndex:i]];    
  }
  return g_strdup([list UTF8String]);
}

- (void)operationFinished:(id)arg result:(void*)result
{
  MYX_TABLE_COMMAND_STATUSES *status= (MYX_TABLE_COMMAND_STATUSES*)result;
  unsigned int i;
  NSMutableAttributedString *text= [resultText textStorage];
  NSAttributedString *table, *statu, *warning, *error, *info;
  NSDictionary *bold= [NSDictionary dictionaryWithObject:[NSFont boldSystemFontOfSize:[NSFont systemFontSize]] 
                                                  forKey:NSFontAttributeName];

  table= [[[NSAttributedString alloc] initWithString:@"Table: "
                                          attributes:bold] autorelease];
  statu= [[[NSAttributedString alloc] initWithString:@"Status: "
                                           attributes:bold] autorelease];
  warning= [[[NSAttributedString alloc] initWithString:@"WARNING: "
                                           attributes:bold] autorelease];
  error= [[[NSAttributedString alloc] initWithString:@"ERROR: "
                                           attributes:bold] autorelease];
  info= [[[NSAttributedString alloc] initWithString:@"Info: "
                                           attributes:bold] autorelease];
  
  for (i= 0; i < status->status_num; i++)
  {
    [text appendAttributedString:table];
    [text appendAttributedString:[[[NSAttributedString alloc] initWithString:[NSString stringWithFormat:@" %s ",status->status[i].table]] autorelease]];
    switch (status->status[i].message_type)
    {
      case MYX_MESSAGE_STATUS:
        [text appendAttributedString:statu];
        break;
      case MYX_MESSAGE_ERROR:
        [text appendAttributedString:error];
        break;
      case MYX_MESSAGE_WARNING:
        [text appendAttributedString:warning];
        break;
      case MYX_MESSAGE_INFO:
        [text appendAttributedString:info];
        break;
    }
    [text appendAttributedString:[[[NSAttributedString alloc] initWithString:[NSString stringWithFormat:@"   %s\n",status->status[i].message]] autorelease]];
  }

  [progress stopAnimation:self];
  [continueButton setTitle:@"Close"];

  [continueButton setEnabled:YES];
  [resultLabel2 setStringValue:@""];

  if ([[operationMatrix cellWithTag:0] state] == NSOnState)
  {
    [resultDescription setStringValue:@"The selected tables were optimized."];
  }
  else if ([[operationMatrix cellWithTag:1] state] == NSOnState)
  {
    [resultDescription setStringValue:@"The selected tables were checked."];
  }
  else
  {
    [resultDescription setStringValue:@"The selected tables were repaired."];
  }
  g_free((char*)arg);
}

- (void)dealloc
{
  [_tables release];
  [super dealloc];
}

- (void)runWithTables:(NSArray*)tables dispatcher:(MMySQLDispatcher*)dispatcher
            operation:(int)op
{
  NSMutableString *list= [[NSMutableString alloc] init];
  unsigned int i;
  
  _tables= [tables retain];
  _dispatcher= dispatcher;
  for (i= 0; i < [tables count]; i++)
  {
    if (i > 0)
      [list appendFormat:@"\n%@",[tables objectAtIndex:i]];
    else
      [list appendFormat:@"%@",[tables objectAtIndex:i]];
  }
  [selectionText setString:[NSString stringWithString:list]];
  [list release];
  
  switch (op)
  {
    case 'O':
      [[operationMatrix cellWithTag:0] performClick:nil];
      [self goNext:nil];
      break;
    case 'C':
      [[operationMatrix cellWithTag:1] performClick:nil];
      [self goNext:nil];
      break;
    case 'R':
      [[operationMatrix cellWithTag:2] performClick:nil];
      [self goNext:nil];
      break;
    default:
      break;
  }

  [NSApp runModalForWindow:[self window]];
}

- (IBAction)cancel:(id)sender
{
  [NSApp stopModalWithCode:0];
}

- (IBAction)goNext:(id)sender
{
  NSString *ident= [[tabView selectedTabViewItem] identifier];
  
  if ([ident isEqualToString:@"main"])
  {
    if ([[operationMatrix cellWithTag:0] state] == NSOnState)
    {
      [resultLabel setStringValue:[[operationMatrix cellWithTag:0] title]];
      [resultDescription setStringValue:@"Optimizing selected tables. This may take a few minutes..."];
      [tabView selectTabViewItemWithIdentifier:@"optimize"];
    }
    else if ([[operationMatrix cellWithTag:1] state] == NSOnState)
    {
      [resultLabel setStringValue:[[operationMatrix cellWithTag:1] title]];
      [resultDescription setStringValue:@"Checking selected tables. This may take a few minutes..."];
      [tabView selectTabViewItemWithIdentifier:@"check"];
    }
    else
    {
      [resultLabel setStringValue:[[operationMatrix cellWithTag:2] title]];
      [resultDescription setStringValue:@"Attempting repair of selected tables. This may take a few minutes..."];
      [tabView selectTabViewItemWithIdentifier:@"repair"];
    }
    [resultLabel2 setStringValue:@"Please wait..."];
  }
  else if ([ident isEqualToString:@"check"])
  {
    int mask= 0;
    char *tables= [self tableList];
    
    if ([[repairMatrix cellWithTag:0] state] == NSOnState)
      mask|= MYX_CHECK_QUICK;
    if ([[repairMatrix cellWithTag:1] state] == NSOnState)
      mask|= MYX_CHECK_FAST;
    if ([[repairMatrix cellWithTag:2] state] == NSOnState)
      mask|= MYX_CHECK_MEDIUM;
    if ([[repairMatrix cellWithTag:3] state] == NSOnState)
      mask|= MYX_CHECK_EXTENDED;
    if ([[repairMatrix cellWithTag:4] state] == NSOnState)
      mask|= MYX_CHECK_CHANGED;
    
    [_dispatcher performCallback:(void*(*)(MYSQL*,void*,void*))myx_check_table
                       argument:tables
                       argument:(void*)mask
               finishedSelector:@selector(operationFinished:result:)
                       argument:(id)tables
                         target:self];
    
    [continueButton setEnabled:NO];
    [progress startAnimation:nil];
    [tabView selectTabViewItemWithIdentifier:@"result"];
  }
  else if ([ident isEqualToString:@"repair"])
  {
    int mask= 0;
    char *tables= [self tableList];
    
    if ([[repairMatrix cellWithTag:0] state] == NSOnState)
      mask|= MYX_CHECK_QUICK;
    if ([[repairMatrix cellWithTag:1] state] == NSOnState)
      mask|= MYX_CHECK_EXTENDED;
    if ([[repairMatrix cellWithTag:2] state] == NSOnState)
      mask|= MYX_REPAIR_USE_FRM;
    if ([repairLocalCheck state] == NSOnState)
      mask|= MYX_REPAIR_NO_WRITE_TO_BINLOG;

    [_dispatcher performCallback:(void*(*)(MYSQL*,void*,void*))myx_repair_table
                       argument:tables
                       argument:(void*)mask
               finishedSelector:@selector(operationFinished:result:)
                       argument:(id)tables
                         target:self];
    
    [continueButton setEnabled:NO];
    [progress startAnimation:nil];
    [tabView selectTabViewItemWithIdentifier:@"result"];
  }
  else if ([ident isEqualToString:@"optimize"])
  {
    int mask= 0;
    char *tables= [self tableList];
    
    if ([optimizeLocalCheck state] == NSOnState)
      mask|= MYX_REPAIR_NO_WRITE_TO_BINLOG;
    
    [_dispatcher performCallback:(void*(*)(MYSQL*,void*,void*))myx_optimize_table
                       argument:tables
                       argument:(void*)mask
               finishedSelector:@selector(operationFinished:result:)
                       argument:(id)tables
                         target:self];
    
    [continueButton setEnabled:NO];
    [progress startAnimation:nil];
    [tabView selectTabViewItemWithIdentifier:@"result"];
  }
  else if ([ident isEqualToString:@"result"])
  {
    [NSApp stopModalWithCode:1];    
  }
  else
    [progress startAnimation:nil];
}


-(void)windowDidClose:(NSNotification*)notif
{
  [NSApp abortModal];
}

@end
