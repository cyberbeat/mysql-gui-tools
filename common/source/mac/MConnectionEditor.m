#import "MConnectionEditor.h"

#import "NSView_extras.h"
#import "MTextImageCell.h"
#import "MPreferences.h"

#import "mxUtils.h"
#import "myxutil.h"
#include "myx_public_interface.h"


@interface MConnectionEditor(Private)
- (void)showUser:(MYX_USER_CONNECTION*)user;
- (void)updateUser:(MYX_USER_CONNECTION*)user;
@end

@implementation MConnectionEditor(Private)

- (void)showUser:(MYX_USER_CONNECTION*)user
{
  [name     setStringValue: user ? NSStr(user->connection_name) :@""];
  [username setStringValue: user ? NSStr(user->username)        :@""];
  [password setStringValue: user ? NSStr(user->password)        :@""];
  [hostname setStringValue: user ? NSStr(user->hostname)        :@""];
  [port     setIntValue:    user ? user->port                  :3306];
  [notes    setString:      user ? NSStr(user->notes)           :@""];
  [schema   setStringValue: user ? NSStr(user->schema)          :@""];
  
  [socket   setStringValue: user ? NSStr(myx_get_option_value((const char**)user->advanced_options,
                                                              user->advanced_options_num,
                                                              "SOCKET_PATH")) : @""];
  [compress setState:       user && myx_get_option_value((const char**)user->advanced_options,
                                                         user->advanced_options_num,
                                                         "COMPRESS") ? NSOnState : NSOffState];
}


- (void)updateUser:(MYX_USER_CONNECTION*)user
{
#define GET_IF_DIFF(var, s) do { NSString *str= s; if (!var || strcmp([str UTF8String], var)!=0) if ([str length]==0) var= NULL; else { g_free(var); var= g_strdup([str UTF8String]); } } while (0)
  GET_IF_DIFF(user->connection_name, [name stringValue]);
  GET_IF_DIFF(user->username, [username stringValue]);
  GET_IF_DIFF(user->password, [password stringValue]);
  GET_IF_DIFF(user->hostname, [hostname stringValue]);
  user->port= [port intValue];
  GET_IF_DIFF(user->notes, [notes string]);
  GET_IF_DIFF(user->schema, [schema stringValue]);
  
  {
    char *new_value= NULL;
    
    GET_IF_DIFF(new_value, [socket stringValue]);
    
    if (set_value(user->advanced_options, user->advanced_options_num, "SOCKET_PATH", new_value) < 0)
    {
      if (new_value)
      {
        user->advanced_options= g_realloc(user->advanced_options, ++user->advanced_options_num * sizeof(char*));
        user->advanced_options[user->advanced_options_num-1]= 
          g_strdup_printf("SOCKET_PATH=%s", new_value);
      }
    }
    else
    {
      if (!new_value)
        user->advanced_options_num--;
    }
    g_free(new_value);
    
    if ([compress state] == NSOnState)
    {
      if (set_value(user->advanced_options, user->advanced_options_num, "COMPRESS", "1") < 0)
      {
        user->advanced_options= g_realloc(user->advanced_options, ++user->advanced_options_num * sizeof(char*));
        user->advanced_options[user->advanced_options_num-1]= g_strdup("COMPRESS=1");
      }	  
    }
    else
    {
      if (set_value(user->advanced_options, user->advanced_options_num, "COMPRESS", NULL) == 0)
        user->advanced_options_num--;
    }
  }
  
  if ([[name stringValue] length] > 0)
  {
    user->storage_type= MYX_FAVORITE_USER_CONNECTION;
  }
}
@end

//===========================================================

@implementation MConnectionEditor


- (void)load:(id)sender
{
  MYX_LIB_ERROR error;
  unsigned int i;

  if (!(connections= myx_load_user_connections([filename fileSystemRepresentation],&error)))
  {
    connections= g_new0(MYX_USER_CONNECTIONS, 1);
  }
  
  [indices removeAllObjects];
  for (i= 0; i < connections->user_connections_num; i++)
  {
    [indices addObject:[NSNumber numberWithInt:i]];
  }
  
  [list reloadData];
}


- (void)save:(id)sender
{
  if ([list levelForRow:[list selectedRow]]>0)
    connections->last_connection= [[list itemAtRow:[list selectedRow]] intValue];
  else
    connections->last_connection= 0;
  
  myx_store_user_connections(connections, [MPreferences preferences]->passwordStorageType, 
                             [filename fileSystemRepresentation]);
}


- (IBAction)addConnection:(id)sender
{
  MYX_USER_CONNECTION data;
  id item;
  
  memset(&data, 0, sizeof(data));
  
  data.connection_name= g_strdup("new connection");
  data.port= 3306;
  
  connections->user_connections= vec_insert_resize(connections->user_connections, sizeof(MYX_USER_CONNECTION),
                                                   &connections->user_connections_num, connections->user_connections_num,
                                                   &data);
  
  item= [NSNumber numberWithInt:connections->user_connections_num-1];
  [indices addObject:item];
  
  [list reloadData];
  
  [list selectRow:[list rowForItem:item] byExtendingSelection:NO];
  [self outlineViewSelectionDidChange:nil];
}


- (IBAction)removeConnection:(id)sender
{
  id item= [list itemAtRow:[list selectedRow]];
  int index= [item intValue];
  
  myx_free_user_connection_content(connections->user_connections+index);
  connections->user_connections= 
    vec_remove(connections->user_connections,sizeof(MYX_USER_CONNECTION),
               &connections->user_connections_num, index);
  
  [list reloadData];
  [self outlineViewSelectionDidChange:nil];
}


- (IBAction)toggleOption:(id)sender
{
  id item= [list itemAtRow:[list selectedRow]];
  
  if (item && !([item isEqualTo:@"saved"] || [item isEqualTo:@"history"]))
  {
    [self updateUser: connections->user_connections+[item intValue]];
  }  
}


- (void)textDidChange:(NSNotification *)aNotification
{
  id item= [list itemAtRow:[list selectedRow]];
  
  if (item && !([item isEqualTo:@"saved"] || [item isEqualTo:@"history"]))
  {
    [self updateUser: connections->user_connections+[item intValue]];
  }
}

- (void)controlTextDidEndEditing:(NSNotification *)aNotification
{
  id item= [list itemAtRow:[list selectedRow]];
  
  if ([aNotification object] == name)
  {
    if (item && !([item isEqualTo:@"saved"] || [item isEqualTo:@"history"]))
    {
      unsigned int i;
      MYX_USER_CONNECTION *conn= connections->user_connections+[item intValue];
      
      for (i= 0; i < connections->user_connections_num; i++)
      {
        if (connections->user_connections[i].connection_name &&
            i != [item intValue] &&
            strcmp(connections->user_connections[i].connection_name,
                   [[name stringValue] UTF8String])==0)
        {
          [name setStringValue:[NSString stringWithUTF8String: conn->connection_name?:""]];
          NSRunAlertPanel(@"Error", @"Duplicate connection name.",
                          nil, nil, nil);
          return;
        }
      }
      [self updateUser: conn];
      [list reloadData];
      
      [list selectRow:[list rowForItem:item] byExtendingSelection:NO];
    }
  }
}

- (void)controlTextDidChange:(NSNotification *)aNotification
{
  id item= [list itemAtRow:[list selectedRow]];
  
  if ([aNotification object] != name)
  {
    if (item && !([item isEqualTo:@"saved"] || [item isEqualTo:@"history"]))
    {
      [self updateUser: connections->user_connections+[item intValue]];
      [list reloadItem:item];
    }
  }
}


- (id)initForFile:(NSString*)file;
{
  self= [super init];
  if (self)
  {    
    if (![NSBundle loadNibNamed:@"ConnectionEditor" owner:self])
    {
      NSLog(@"Could not load ConnectionEditor nib file");
      [self release];
      return nil;
    }
    
    indices= [[NSMutableArray alloc] init];
    
    connIcon= [MXGetImageFromBundle([NSBundle bundleForClass:[self class]],
                                    @"networkhost_16x16.png") retain];
    groupIcon= [MXGetImageFromBundle([NSBundle bundleForClass:[self class]],
                                     @"folder_16x16.png") retain];
    {
      id cell= [[[MTextImageCell alloc] init] autorelease];
      [cell setFont:[[[list tableColumnWithIdentifier:@"column"] dataCell] font]];
      [[list tableColumnWithIdentifier:@"column"] setDataCell:cell];
    }
    
    filename= [file retain];
    [self load:nil];
    [list expandItem:@"saved" expandChildren:YES];
    [list expandItem:@"history" expandChildren:YES];
    
    [self outlineViewSelectionDidChange:nil];
  }
  return self;
}


- (void)dealloc
{
  [list setDataSource:nil];
  [indices release];
  [filename release];
  [connIcon release];
  [groupIcon release];
  myx_free_user_connections(connections);
  [super dealloc];
}


- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item
{
  if (item == nil)
    return 2;
  else if ([item isEqualTo:@"saved"] || [item isEqualTo:@"history"])
  {
    int i, count= 0;
    
    for (i= 0; i < connections->user_connections_num; i++)
    {
      if (connections->user_connections[i].connection_name)
        count++;
    }
    
    if ([item isEqualTo:@"saved"])
      return count;
    else
      return connections->user_connections_num-count;
  }
  else
    return 0;
}


- (id)outlineView:(NSOutlineView *)outlineView
 objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
  if ([item isEqualTo:@"saved"])
    return @"Connections";
  else if ([item isEqualTo:@"history"])
    return @"History";
  else
  {
    int index= [item intValue];
    
    return [NSString stringWithUTF8String:connections->user_connections[index].connection_name?:"Connection"];
  }
}


- (void)outlineView:(NSOutlineView *)outlineView willDisplayCell:(id)cell 
     forTableColumn:(NSTableColumn *)tableColumn item:(id)item
{
  if ([item isEqualTo:@"saved"] || [item isEqualTo:@"history"])
    [cell setImage:groupIcon];
  else
    [cell setImage:connIcon];
}


- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)item
{
  if (item == nil)
  {
    if (index == 0)
      return @"saved";
    else
      return @"history";
  }
  else
  {
    int i, count= 0;
    BOOL history= YES;
    
    if ([item isEqualTo:@"saved"])
      history= NO;
    
    for (i= 0; i < connections->user_connections_num; i++)
    {
      if ((!history && connections->user_connections[i].connection_name) ||
          (history && !connections->user_connections[i].connection_name))
      {
        if (count == index)
          return [indices objectAtIndex:i];
        
        count++;
      }
    }    
    
    return nil;
  }
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
  if ([item isEqualTo:@"saved"] || [item isEqualTo:@"history"])
    return YES;
  else
    return NO;
}


- (void)outlineViewSelectionDidChange:(NSNotification *)notification
{
  id item= [list itemAtRow:[list selectedRow]];
  
  if (item && !([item isEqualTo:@"saved"] || [item isEqualTo:@"history"]))
  {
    [self showUser: connections->user_connections+[item intValue]];
    
    [[topView viewWithTag:100] setEnabled:YES];
    [tabview setEnabledRecursive:YES];
  }
  else
  {
    [[topView viewWithTag:100] setEnabled:NO];
    [self showUser: 0];
    [tabview setEnabledRecursive:NO];
  }
}


@end
