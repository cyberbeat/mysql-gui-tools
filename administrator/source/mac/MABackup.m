#import "MABackup.h"
#import "maUtils.h"
#import <MySQLToolsCommon/NSString_extras.h>
#include "myx_admin_public_interface.h"

#import <MySQLToolsCommon/NSView_extras.h>
#import <MySQLToolsCommon/MTextImageCell.h>
#import <MySQLToolsCommon/MButtonImageCell.h>
#import <MySQLToolsCommon/MSchemaDatasource.h>
#import <MySQLToolsCommon/MMySQLDispatcher.h>
#import <MySQLToolsCommon/MTreeDataSource.h>
#import <MySQLToolsCommon/MCrontab.h>
#import <MySQLToolsCommon/MPreferenceEditor.h>
#import <MySQLToolsCommon/MPreferences.h>

#include <MySQLToolsCommon/myxutil.h>

#define COMMAND_NSTEXT @"mabackup"

//XXX make a protocol with the exported stuff from MAdministrator and include it here
extern NSString *MASchemaDataSourceChanged;


static const char *nameOfEntity(MYX_ENTITY_STATUS *status)
{
  switch (status->entity_type)
  {
    case MYX_ENTITY_TABLE:
      return ((MYX_TABLE_STATUS*)status->entity)->table_name;
    case MYX_ENTITY_VIEW:
      return ((MYX_VIEW_STATUS*)status->entity)->view_name;
    case MYX_ENTITY_PROC:
    case MYX_ENTITY_FUNC:
      return ((MYX_SCHEMA_STORED_PROCEDURE*)status->entity)->name;
  }
  return NULL;
}

@interface MABackupSelectionItem : MSelectableTreeItem
{
  MYX_SCHEMA_ENTITY_STATUS *_status;
  
  @public
  char *_catalog;
  char *_schema;
  
  long long rows;
  long long length;
}

- (MABackupSelectionItem*)initWithSchemaItem:(MSchemaItem*)item;
- (void)resetSchemaStatus:(MYX_SCHEMA_ENTITY_STATUS*)status;
- (BOOL)isEqualToItem:(MSchemaItem*)item;

- (BOOL)isTableSelected:(NSString*)table;
- (void)selectTable:(NSString*)table;

- (MYX_SCHEMA_ENTITY_STATUS*)status;
@end


@interface MABackupObjectSelectionItem : MSelectableTreeItem
{
  @public
  MYX_ENTITY_STATUS *_status;
}
- (id)initWithStatus:(MYX_ENTITY_STATUS*)status;
@end


@implementation MABackupSelectionItem

- (void)dealloc
{
  g_free(_catalog);
  g_free(_schema);
  if (_status)
    myx_free_schema_entity_status(_status);
  [super dealloc];
}

- (MYX_SCHEMA_ENTITY_STATUS*)status
{
  return self->_status;
}

- (MABackupSelectionItem*)initWithSchemaItem:(MSchemaItem*)item
{
  MYX_SCHEMA *schema= [item schema];
  self= [super initWithTag:'S' repr:[NSString stringWithFormat:@"%s (%s)",schema->schema_name,schema->catalog_name]];
  if (self)
  {
    _catalog= g_strdup(schema->catalog_name);
    _schema= g_strdup(schema->schema_name);
  }
  return self;
}

- (BOOL)entityIsSelected:(MYX_ENTITY_STATUS*)status
{
  int i, count= [_children count];
  const char *name= nameOfEntity(status);
  
  for (i= 0; i < count; i++)
  {
    MABackupObjectSelectionItem *item= [_children objectAtIndex:i];
    if (strcmp2(name, [[item repr] UTF8String])==0)
      return [item selected]!=NSOffState;
  }
  return NO;
}

- (void)resetSchemaStatus:(MYX_SCHEMA_ENTITY_STATUS*)status
{
  NSMutableArray *ntables;
  unsigned int i, c;
  if (_status)
    myx_free_schema_entity_status(_status);
  _status= status;
  
  ntables= [[NSMutableArray alloc] initWithCapacity: status->schema_entities_num];
  rows= 0;
  length= 0;
  for (i= 0; i < status->schema_entities_num; i++)
  {
    if (status->schema_entities[i].entity_type == MYX_ENTITY_TABLE ||
        status->schema_entities[i].entity_type == MYX_ENTITY_VIEW)
    {
      MABackupObjectSelectionItem *titem= [[MABackupObjectSelectionItem alloc] initWithStatus:status->schema_entities+i];
      if (_children)
        [titem setSelected:[self entityIsSelected:status->schema_entities+i]];
      [ntables addObject:titem];
      [titem release];
    }
  }
  if (_children)
    [_children release];
  _children= nil;
  c= [ntables count];
  for (i= 0; i < c; i++)
    [self addChild:[ntables objectAtIndex:i]];
  [ntables release];
}

- (BOOL)isEqualToItem:(MSchemaItem*)item
{
  MYX_SCHEMA *schema= [item schema];
  if (strcmp(_catalog, schema->catalog_name)==0 && 
      strcmp(_schema, schema->schema_name)==0)
    return YES;
  return NO;
}

- (BOOL)isTableSelected:(NSString*)table
{
  int i, count= [_children count];
  for (i= 0; i < count; i++)
  {
    id obj= [_children objectAtIndex:i];
    if ([[obj repr] isEqualToString:table])
      return [obj selected]==NSOnState;
  }
  return NO;
}


- (void)selectTable:(NSString*)table
{
  int i, count= [_children count];
  for (i= 0; i < count; i++)
  {
    id obj= [_children objectAtIndex:i];
    if ([[obj repr] isEqualToString:table])
    {
      [obj setSelected:YES];
      break;
    }
  }  
}


- (id)valueForIdentifier:(NSString*)ident
{
  if ([ident isEqualToString:@"rows"])
    return [NSString stringWithMultNumber:rows];
  else if ([ident isEqualToString:@"data"])
    return [NSString stringWithMultNumber:length];
  else if ([ident isEqualToString:@"update"])
    return @"";
  else if ([ident isEqualToString:@"object"])
    return [NSNumber numberWithInt:[self selected]];
  else
    return @"";
}

@end


@implementation MABackupObjectSelectionItem

- (id)initWithStatus:(MYX_ENTITY_STATUS*)status
{
  int tag;
  switch (status->entity_type)
  {
    case MYX_ENTITY_TABLE:
      tag= 'T';
      break;
    case MYX_ENTITY_VIEW:
      tag= 'V';
      break;
    case MYX_ENTITY_PROC:
      tag= 'P';
      break;
    case MYX_ENTITY_FUNC:
      tag= 'F';
      break;
  }
  
  self= [super initWithTag:tag repr:[NSString stringWithUTF8String:nameOfEntity(status)]];
  
  if (self)
  {
    _status= status;
  }
  return self;
}

- (void)setSelected:(BOOL)flag
{  
  if (flag != (_selected==NSOnState))
  {
    if (_status->entity_type == MYX_ENTITY_TABLE)
    {
      MYX_TABLE_STATUS *status= _status->entity;
      long long value;
      value= strtoll(status->rows, NULL, 10);
      ((MABackupSelectionItem*)_parent)->rows+= flag ? value : -value;
      value= strtoll(status->data_length, NULL, 10);
      ((MABackupSelectionItem*)_parent)->length+= flag ? value : -value;
    }
  }
  [super setSelected:flag];
}

- (id)valueForIdentifier:(NSString*)ident
{  
  switch ([self tag])
  {
    case 'T':
    {
      MYX_TABLE_STATUS *status= _status->entity;
      if ([ident isEqualToString:@"type"])
        return [NSString stringWithCString:status->table_type];
      else if ([ident isEqualToString:@"rows"])
        return [NSString stringWithMultNumber:strtoll(status->rows,NULL,10)];
      else if ([ident isEqualToString:@"data"])
        return [NSString stringWithMultNumber:strtoll(status->data_length,NULL,10)];
      else if ([ident isEqualToString:@"update"])
        return [NSString stringWithCString:status->update_time];
      else if ([ident isEqualToString:@"object"])
        return [NSNumber numberWithInt:[self selected]];
      else
        return @"";
    }
    case 'V':
      if ([ident isEqualToString:@"type"])
        return @"View";
      else if ([ident isEqualToString:@"object"])
        return [NSNumber numberWithInt:[self selected]];
      else
        return @"";
    case 'P':
      if ([ident isEqualToString:@"type"])
        return @"SP";
      else if ([ident isEqualToString:@"object"])
        return [NSNumber numberWithInt:[self selected]];
      else
        return @"";
    case 'F':
      if ([ident isEqualToString:@"type"])
        return @"Function";
      else if ([ident isEqualToString:@"object"])
        return [NSNumber numberWithInt:[self selected]];
      else
        return @"";
  }
  return @"";
}

@end


//========================================================================

@interface MABackupDataSource : MTreeDataSource
{
}
@end

@implementation MABackupDataSource

- (void)outlineView:(NSOutlineView *)outlineView 
     setObjectValue:(id)object 
     forTableColumn:(NSTableColumn *)tableColumn 
             byItem:(id)item
{
  [item setSelected:[object intValue]!=NSOffState];

  [outlineView reloadData];
}

@end

//========================================================================


@interface MABackup(Private)
- (void)refreshProfiles;
- (void)openProfile:(NSString*)name;
- (void)saveProfile;
- (void)schemaDSChanged:(NSNotification*)notif;
- (MYX_BACKUP_CONTENT*)getContentSelection;
- (void)controlTextDidEndEditing:(NSNotification *)aNotification;
- (MABackupSelectionItem*)addSchemaToContents:(MSchemaItem*)schema;
- (void)doStartBackup;
- (void)backupFinished:(id)data result:(void*)result;
- (void)fileNameSelected:(NSSavePanel *)sheet returnCode:(int)returnCode contextInfo:(void *)contextInfo;
@end

@implementation MABackup(Private)

- (void)refreshProfiles
{
  NSDirectoryEnumerator *en= 
  [[NSFileManager defaultManager] enumeratorAtPath:[[NSUserDefaults standardUserDefaults] stringForKey:@"BackupProfileDirectory"]];
  NSString *file;
  [_profiles removeAllObjects];
  while ((file= [en nextObject]))
  {
    [_profiles addObject:[file stringByDeletingPathExtension]];
  }
  [profileTable reloadData];
}

static struct {
  int tag;
  int option;
} options[]= {
  {100,MYX_B_NO_CREATES},
  {101,MYX_B_NO_EXTENDED_INSERT},
  {102,MYX_B_ADD_DROP_TABLE},
  {103,MYX_B_COMPLETE_INSERTS},
  {104,MYX_B_COMMENT},
  {105,MYX_B_DONT_WRITE_FULL_PATH},
  {106,MYX_B_ANSI_QUOTES},
  {107,MYX_B_DISABLE_KEYS}
};

- (void)openProfile:(NSString*)name
{
  MYX_ADMIN_LIB_ERROR error;
  NSUserDefaults *defaults= [NSUserDefaults standardUserDefaults];
  
  if (_profile)
    myx_free_profile(_profile);
  _profile= NULL;
  
  if (!name)
  {
    [nameText setStringValue:@""];
    [_selectionDS setRoot:nil];
    [contentOutline reloadData];
    
    return;
  }
  
  _profile= myx_load_profile([[name stringByAppendingString:@".mbp"] UTF8String],
                             [[defaults stringForKey:@"BackupProfileDirectory"] UTF8String],
                             &error);
  if (!_profile)
  {
    MARunAlertPanelWithError(@"Error", 
                             [NSString stringWithFormat:_(@"Could not load Project '%@'."),name],
                             error);
  }
  else
  {
    NSButtonCell *btn;
    int i;
    unsigned int s;
    MTreeItem *root;
    NSMutableArray *missing= [[[NSMutableArray alloc] init] autorelease];
    
    [nameText setStringValue:[NSString stringWithUTF8String:_profile->profile_name]];
    
    if (_profile->options & MYX_B_LOCK_ALL_TABLES)
      btn= [modeMatrix cellWithTag:50]; // lock all
    else if (_profile->options & MYX_B_SINGLE_TRANSACTION)
      btn= [modeMatrix cellWithTag:51]; // single trans
    else if (_profile->options & MYX_B_POINT_IN_TIME_BACKUP)
      btn= [modeMatrix cellWithTag:49]; // binlog with pos
    else
      btn= [modeMatrix cellWithTag:52]; // normal
    [btn performClick:self];
    
    [fullBackupButton setState:(_profile->options & MYX_B_COMPLETE_SCHEMATAS)?NSOnState:NSOffState];
    
    for (i= 0; i < 8; i++)
    {
      btn= [optionMatrix cellWithTag:options[i].tag];
      [btn setState:(_profile->options & options[i].option)?NSOnState:NSOffState];
    }
    
    root= [MTreeItem itemWithTag:'R' repr:nil];
    [_selectionDS setRoot:root];
    
    // show selected objects
    for (s= 0; s < _profile->backup_content->tables_num; s++)
    {
      MYX_BACKUP_TABLE *table= _profile->backup_content->tables+s;
      MSchemaItem *cat, *sch;
      MABackupSelectionItem *sitem;
      
      // - look for the schema item in the schema list
      cat= [[schemaOutline dataSource] findItem:[NSString stringWithUTF8String: table->catalog]];
      if (cat)
      {
        sch= [[schemaOutline dataSource] findChild:[NSString stringWithUTF8String: table->schema]
                                            ofItem:cat];
        if (!sch)
        {
          NSLog(@"Invalid schema '%s.%s' in backup project", table->catalog, table->schema);
          [missing addObject:[NSString stringWithFormat:@"%s.%s", table->catalog, table->schema]];
          continue;
        }
      }
      else
      {
        NSLog(@"Invalid catalog '%s' in backup project", table->catalog);
        [missing addObject:[NSString stringWithFormat:@"%s", table->catalog]];
        continue;
      }
      
      // - add the selected schema item
      sitem= [self addSchemaToContents:sch];
      
      // - then add table (if the data was already fetched, otherwise do it when 
      // the data arrives)
      if ([sitem children]!=nil) 
      {
        [sitem selectTable:[NSString stringWithUTF8String:table->table]];
      }
    }
    [contentOutline reloadData];
    
    //    if ([missing count] > 0)
    //    {
    //      if (NSRunAlertPanel(@"Warning", @"The backup project contains objects that do not exist in the connected server. You may choose to make a copy of the project with valid objects only and keep the original intact.",
    //                          @"Copy", @"Cancel", nil) == NSAlertDefaultReturn)
    //      {
    //        NSLog(@"copy");
    //      }
    //      else
    //      {
    //        [profileTable deselectAll:nil];
    //      }
    //    }
  }
}


- (void)saveProfile
{
  MYX_ADMIN_LIB_ERROR error;  
  NSButtonCell *btn;
  int i;
  
  // update the profile according to whats on screen
  g_free(_profile->profile_name);
  _profile->profile_name= g_strdup([[nameText stringValue] UTF8String]);
  
  _profile->options= 0;
  if ([[modeMatrix cellWithTag:50] state] == NSOnState)
    _profile->options|= MYX_B_LOCK_ALL_TABLES;
  else if ([[modeMatrix cellWithTag:51] state] == NSOnState)
    _profile->options|= MYX_B_SINGLE_TRANSACTION;
  else if ([[modeMatrix cellWithTag:49] state] == NSOnState) 
    _profile->options|= MYX_B_POINT_IN_TIME_BACKUP;
  
  if ([fullBackupButton state] == NSOnState)
    _profile->options|= MYX_B_COMPLETE_SCHEMATAS;
  
  for (i= 0; i < 8; i++)
  {
    btn= [optionMatrix cellWithTag:options[i].tag];
    if ([btn state] == NSOnState)
      _profile->options|= options[i].option;
  }
  
  myx_free_backup_content(_profile->backup_content);
  _profile->backup_content= [self getContentSelection];
  
  error= myx_save_profile([[NSString stringWithFormat:@"%s.mbp",_profile->profile_name] UTF8String],
                          [[[NSUserDefaults standardUserDefaults] stringForKey:@"BackupProfileDirectory"] UTF8String],
                          _profile);
  
  if (error != MYX_ADMIN_NO_ERROR)
  {
    MARunAlertPanelWithError(@"Error", _(@"Could not save project."), error);
  }
}

- (void)schemaDSChanged:(NSNotification*)notif
{
  [schemaOutline reloadData];
}

- (MYX_BACKUP_CONTENT*)getContentSelection
{
  MYX_BACKUP_CONTENT *content= g_malloc(sizeof(MYX_BACKUP_CONTENT));
  //  MYX_BACKUP_TABLE *table;
  int i, count;
  int total_tables= 0;
  int index;
  NSArray *schemas= [[_selectionDS root] children];
  
  // count # of selected tables
  count= [schemas count];
  for (i= 0; i < count; i++)
  {
    MABackupSelectionItem *item= [schemas objectAtIndex:i];
    NSArray *tables= [item children];
    int j, jcount= [tables count];
    for (j= 0; j < jcount; j++)
    {
      MABackupObjectSelectionItem *titem= [tables objectAtIndex:j];
      if ([titem selected] == NSOnState)
        total_tables++;
    }
  }
  
  content->tables_num= total_tables;
  content->tables= g_malloc(sizeof(MYX_BACKUP_TABLE)*total_tables);
  count= [schemas count];
  index= 0;
  for (i= 0; i < count; i++)
  {
    MABackupSelectionItem *item= [schemas objectAtIndex:i];
    NSArray *tables= [item children];
    int j, jcount= [tables count];
    for (j= 0; j < jcount; j++)
    {
      MABackupObjectSelectionItem *titem= [tables objectAtIndex:j];
      if ([titem selected] == NSOnState)
      {
        content->tables[index].catalog= g_strdup(item->_catalog);
        content->tables[index].schema= g_strdup(item->_schema);
        content->tables[index].table= g_strdup([[titem repr] UTF8String]);
        switch([item status]->schema_entities[j].entity_type)
        {
        case MYX_ENTITY_TABLE:
          content->tables[index].flags=  MYX_BTF_IS_TABLE;
          break;
        case MYX_ENTITY_VIEW:
          content->tables[index].flags=  MYX_BTF_IS_VIEW;
          break;
        case MYX_ENTITY_PROC:
          content->tables[index].flags=  MYX_BTF_IS_PROCEDURE;
          break;
        case MYX_ENTITY_FUNC:
          content->tables[index].flags=  MYX_BTF_IS_FUNCTION;        
          break;
        }
        index++;
      }
    }
  }
  return content;
}

- (void)controlTextDidEndEditing:(NSNotification *)aNotification
{
  int row= [profileTable selectedRow];
  
  if (row >= 0)
  {
    NSString *path= [NSString stringWithFormat:@"%@/%@.mbp",
      [[NSUserDefaults standardUserDefaults] stringForKey:@"BackupProfileDirectory"],
      [_profiles objectAtIndex:row]];
    [[NSFileManager defaultManager] removeFileAtPath:path handler:nil];
    
    [self saveProfile];
    [_profiles insertObject:[NSString stringWithUTF8String:_profile->profile_name] atIndex:row];
    [_profiles removeObjectAtIndex:row+1];
    [profileTable reloadData];
  }
}


- (MABackupSelectionItem*)addSchemaToContents:(MSchemaItem*)item
{
  NSArray *schemas= [[_selectionDS root] children];
  int i,c= [schemas count];
  MABackupSelectionItem *selItem;
  MYX_SCHEMA_ENTITY_STATUS *status;
  
  for (i= 0; i < c; i++)
  {
    // check whether the schema is already added
    if ([[schemas objectAtIndex:i] isEqualToItem: item])
      return [schemas objectAtIndex:i];
  }
  
  selItem= [[MABackupSelectionItem alloc] initWithSchemaItem: item];
  
retry:
  status= [[_owner dispatcher] performCallback:(void*(*)(MYSQL*,void*,void*))myx_get_schema_entity_status
                                      argument:[item schema]->catalog_name
                                      argument:[item schema]->schema_name
                                 waitForWindow:[_owner window]
                                       message:@"Retrieving object data..."];
  if (!status && ![[_owner dispatcher] checkConnection] && [[_owner dispatcher] reconnect])
    goto retry;
  
  if (status)
  {
    [selItem resetSchemaStatus:status];
    
    if (_profile)
    {
      // go through the current profile and select tables that were previously selected
      for (i= 0; i < (int)_profile->backup_content->tables_num; i++)
      {
        MYX_BACKUP_TABLE *table= _profile->backup_content->tables+i;
        if ((table->flags & (MYX_BTF_IS_PROCEDURE|MYX_BTF_IS_FUNCTION))==0 
            && strcmp(selItem->_catalog, table->catalog)==0
            && strcmp(selItem->_schema, table->schema)==0)
        {
          [selItem selectTable:[NSString stringWithUTF8String:table->table]];
        }
      }
    }
    
    [[_selectionDS root] addChild:selItem];
    
    [contentOutline reloadData];
  }
  
  [selItem autorelease];
  
  return selItem;
}


static int backup_progress_cb(const char *table, 
                              int num_tables_total,
                              int num_tables,
                              int num_rows_total,
                              int num_rows,
                              void *user_data)
{
  MABackup *back= (MABackup*)user_data;
  NSString *tmp;
  NSAutoreleasePool *pool= [[NSAutoreleasePool alloc] init];
  
  [back->backupProgress setDoubleValue:100.0*(double)num_rows/(double)num_rows_total];
  
  tmp= [[NSString alloc] initWithFormat:@"Table %i of %i",num_tables,num_tables_total];
  [back->progressText setStringValue:tmp];
  [tmp release];
  
  tmp= [[NSString alloc] initWithUTF8String:table];
  [back->tableProgressText setStringValue:tmp];
  [tmp release];
  
  [pool release];
  
  return back->_backupAborted;
}

static void *do_backup(MYSQL *mysql, MYX_BACKUP_PROFILE *profile, char *path,
                       void *cdata)
{
  MYX_BACKUP_ERROR err;
  
  err= myx_make_backup_with_profile(mysql, profile, path,
                                    1, backup_progress_cb, cdata);
  g_free(path);
  
  return (void*)err;
}

- (void)doStartBackup
{
  NSSavePanel *panel= [NSSavePanel savePanel];
  NSString *defpath;
  char ts[64];
  time_t now= time(NULL);
  
  //XXX option to not add timestamp
  strftime(ts, sizeof(ts), "_%F_%H.%M.%S", localtime(&now)); 
  
  defpath= [NSString stringWithFormat:@"%s%s.sql", _profile->profile_name, ts];
  
  //XXX save default backup directory
  [panel setTitle:@"Backup"];
  [panel setPrompt:@"Start Backup"];
  [panel beginSheetForDirectory:nil file:defpath modalForWindow:[topBox window]
                  modalDelegate:self
                 didEndSelector:@selector(fileNameSelected:returnCode:contextInfo:)
                    contextInfo:NULL];
}

- (void)fileNameSelected:(NSSavePanel *)sheet returnCode:(int)returnCode contextInfo:(void *)contextInfo
{
  if (returnCode != NSOKButton)
    return;
  
  [self saveProfile];
  
  [startButton setEnabled:NO];
  
  _backupAborted= NO;
  
  [[_owner dispatcher] performCallback:(void*(*)(MYSQL*,void*,void*,void*))do_backup
                              argument:_profile
                              argument:g_strdup([[sheet filename] UTF8String])
                              argument:self
                      finishedSelector:@selector(backupFinished:result:)
                              argument:nil
                                target:self];
  
  [NSApp runModalForWindow:progressPanel];
}


- (void)backupFinished:(id)data result:(void*)result
{
  [progressPanel orderOut:nil];
  [NSApp abortModal];
  
  if ((MYX_BACKUP_ERROR)result != MYX_BACKUP_NO_ERROR)
  {
    NSString *msg= nil;
    
    switch ((MYX_BACKUP_ERROR)result)
    {
      case MYX_BACKUP_NO_ERROR: break;
      case MYX_BACKUP_SQL_ERROR:
        msg= [[_owner dispatcher] lastMySQLError];
        break;
      case MYX_BACKUP_CANT_OPEN_FILE:
        msg= @"Could not open file for writing backup.";
        break;
      case MYX_BACKUP_ILLEGAL_OPTION:
        msg= @"Illegal option.";
        break;
      case MYX_BACKUP_PCRE_ERROR:
        msg= @"Internal error in PCRE call.";
        break;  
      case MYX_BACKUP_MALLOC_FAILED:    
        msg= @"Memory allocation failed during backup.";
        break;
      case MYX_BACKUP_OUTPUTDEVICE_FULL:
        msg= @"Out of disk space during backup write.";
        break;
      case MYX_BACKUP_CANNOT_FLUSH_TABLES_WITH_READ_LOCK:
        msg= @"Cannot flush tables with read lock.";
        break; 
      case MYX_BACKUP_CANNOT_START_TRANSACTION:
        msg= @"Cannot start transaction.";
        break;
      case MYX_BACKUP_CANNOT_SET_ANSI_QUOTES:
        msg= @"Cannot set ANSI quotes.";
        break;
      default:
        msg= @"Internal error."; 
        break;
    }
    NSRunAlertPanel(@"Error", 
                    @"Error Performing Backup:\n%@", 
                    @"OK", nil, nil, msg);
  }
  
  [startButton setEnabled:YES];
}

@end


@implementation MABackup

- (IBAction)addProfile:(id)sender
{
  NSFileManager *fman= [NSFileManager defaultManager];
  NSString *prefix= [[NSUserDefaults standardUserDefaults] stringForKey:@"BackupProfileDirectory"];
  NSString *name= _(@"New Project");
  unsigned int i;
  MYX_BACKUP_PROFILE profile;
  MYX_ADMIN_LIB_ERROR err;
  
  [profileTable deselectAll:nil];
  
  i= 1;
  while ([fman fileExistsAtPath:[NSString stringWithFormat:@"%@/%@.mbp", prefix,name]])
  {
    name= [NSString stringWithFormat:_(@"New Project %i"),i++];
  }
  
  profile.profile_name= (char*)[name UTF8String];
  profile.last_used= NULL;
  profile.options= 0;
  profile.backup_type= MYX_BT_SQL_SCRIPT;
  profile.backup_content= NULL;
  err= myx_save_profile([[NSString stringWithFormat:@"%@.mbp",name] UTF8String], 
                        [prefix UTF8String], &profile);
  if (err)
  {
    MARunAlertPanelWithError(@"Error", @"Could not create project file.", err);
  }
  
  [self refreshProfiles];
  for (i= 0; i < [_profiles count]; i++)
  {
    if ([[_profiles objectAtIndex:i] isEqualToString:name])
    {
      [profileTable selectRow:i byExtendingSelection:NO];
      break;
    }
  }
}


- (void)tableViewDeleteRow:(MTableView*)table
{
  if (table == profileTable)
  {
    [self removeProfile:table];
  }
}


- (NSString *)getActiveProfileName
{
  if ([profileTable selectedRow] >= 0)
    return [_profiles objectAtIndex: [profileTable selectedRow]];
  else
    return nil;
}

- (NSString *)getCommentText
{
  return [NSString stringWithFormat:@"added automatically by MySQL Administrator - do not edit %@", [self getActiveProfileName]];
}


- (NSString *)getMABackupToolPath
{
  return [NSString stringWithFormat: @"\"%@/%@\"", [[NSBundle mainBundle] resourcePath], @"mabackup"];
}


- (IBAction)removeProfile:(id)sender
{
  int row= [profileTable selectedRow];
  if (row >= 0)
  {
    MCrontab *cron = [[[MCrontab alloc] init] autorelease];
    [cron load];
    //    MCrontabEntry *entry = [cron findEntryByComment: [self getCommentText]];
    [cron removeCommand: [self getMABackupToolPath] withComment: [self getCommentText]];
    [cron installTable];
    
    NSString *path= [NSString stringWithFormat:@"%@/%@.mbp",
      [[NSUserDefaults standardUserDefaults] stringForKey:@"BackupProfileDirectory"],
      [_profiles objectAtIndex:row]];
    
    [[NSFileManager defaultManager] removeFileAtPath:path handler:nil];
    
    if (_profile)
      myx_free_profile(_profile);
    _profile= NULL;
    
    [profileTable deselectAll:nil];
    
    [self refreshProfiles];
    [profileTable reloadData];
  }
}

- (IBAction)addContent:(id)sender
{
  NSIndexSet *rows= [schemaOutline selectedRowIndexes];
  unsigned int row;
  NSAssert([rows count] > 0, @"No selected schema!");
  for (row= [rows firstIndex]; row != NSNotFound; row= [rows indexGreaterThanIndex:row])
  {
    MSchemaItem *item= [schemaOutline itemAtRow:row];
    
    if (item->type == MSchemaItemType)
    {
      [self addSchemaToContents:item];
    }
  }
  [schemaOutline deselectAll:nil];
}

- (IBAction)removeContent:(id)sender
{
  NSIndexSet *rows= [contentOutline selectedRowIndexes];
  unsigned int row;
  
  NSAssert([rows count] > 0, @"No selected schema!");
  for (row= [rows firstIndex]; row != NSNotFound; row= [rows indexGreaterThanIndex:row])
  {
    if ([contentOutline levelForRow:row]==0)
    {
      MABackupSelectionItem *item= [contentOutline itemAtRow:row];
      
      [[_selectionDS root] removeChild:item];
    }
    else
    {
      MABackupObjectSelectionItem *item= [contentOutline itemAtRow:row];
      
      [item setSelected:NO];
    }
  }
  [contentOutline reloadData];
  [contentOutline deselectAll:nil];
}

- (IBAction)importProfile:(id)sender
{
  NSOpenPanel *panel= [NSOpenPanel openPanel];
  
  [panel setTitle:@"Import Project"];
  [panel setPrompt:@"Import"];
  if ([panel runModal] == NSFileHandlingPanelOKButton)
  {
    MYX_BACKUP_PROFILE *profile;
    MYX_ADMIN_LIB_ERROR err;
    NSString *newName;
    NSString *profileDir= [[NSUserDefaults standardUserDefaults] stringForKey:@"BackupProfileDirectory"];
    NSString *path= [panel filename];
    int i;
    
    // copy the contents to the profile directory
    profile= myx_load_profile((char*)[path lastPathComponent],
                              (char*)[path stringByDeletingLastPathComponent],
                              &err);
    if (!profile)
    {
      MARunAlertPanelWithError(@"Error", [NSString stringWithFormat:@"Could not read the project file '%@'",path], err);
      return;
    }
    i= 1;
    
    newName= myx_get_available_filename(profileDir, 
                                        [NSString stringWithUTF8String:profile->profile_name],
                                        @".mbp");
    
    g_free(profile->profile_name);
    profile->profile_name= g_strdup([[newName stringByDeletingPathExtension] UTF8String]);
    
    if (myx_save_profile([newName UTF8String],
                         [profileDir UTF8String],
                         profile) < 0)
    {
      NSRunAlertPanel(@"Error", @"Could not copy the project file.", nil, nil, nil, nil);
    }
    myx_free_profile(profile);
  }
}

- (IBAction)startBackup:(id)sender
{
  int row= [profileTable selectedRow];
  
  if (row >= 0)
  {
    [self saveProfile];
    
    if (_profile->backup_content->tables_num == 0)
      NSRunAlertPanel(@"Error", @"There are no tables selected to be backed up.",
                      nil, nil, nil);
    else
      [self doStartBackup];
  }
  else
  {
    NSRunAlertPanel(@"Error", @"You must first select an existing Backup Project or create a new one.",
                    nil, nil, nil);
  }
}

- (IBAction)chooseTargetDirectory:(id)sender
{
  NSOpenPanel *panel = [NSOpenPanel openPanel];
  [panel setAllowsMultipleSelection: NO];
  [panel setCanChooseDirectories: YES];
  [panel setCanChooseFiles: NO];
  if([panel runModalForTypes: nil] == NSOKButton)
  {
    [targetText setStringValue: [[panel filenames] objectAtIndex: 0]];
  }
}

- (void)updateScheduleStatus: (MCrontabEntry *)entry updateControls: (BOOL)controls
{
  static const char *wd[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
  
  if(entry == nil)
  {
    [schedulePopUp selectItemAtIndex: 0];
    [scheduleTab selectTabViewItemAtIndex: 0];
    [statusText setStringValue: @"Backups are currently not scheduled"];
  } 
  else if(entry->_weekday > 0)
  {
    if(controls == YES)
    {
      [schedulePopUp selectItemAtIndex: 2];
      [scheduleTab selectTabViewItemAtIndex: 2];
      [weeklyTimeText setStringValue: [NSString stringWithFormat: @"%02d:%02d", entry->_hour, entry->_minute]];
    }
    
    NSString *weekdays = nil;
    int i;
    for(i = 0; i < 7; i++)
    {
      if(entry->_weekday & (1 << i))
      {
        if(weekdays == nil)
        {
          weekdays = [NSString stringWithCString: wd[i]];
        }
        else
        {
          weekdays = [weekdays stringByAppendingFormat: @", %s", wd[i]];
        }
        if(controls == YES)
        {
          [[[scheduleWeekdays cells] objectAtIndex: i] setIntValue: 1];
        }
      }
    }
    if(weekdays == nil)
    {
      weekdays = @"(none)";
    }
    [statusText setStringValue: [NSString stringWithFormat: @"Backups are currently scheduled weekly on %@ at %02d:%02d", weekdays, entry->_hour, entry->_minute]];
  }
  else if([entry->_day length] > 0)
  {
    if(controls == YES)
    {
      [schedulePopUp selectItemAtIndex: 3];
      [scheduleTab selectTabViewItemAtIndex: 3];
      [monthlyDaysText setStringValue: entry->_day];
      [monthlyTimeText setStringValue: [NSString stringWithFormat: @"%02d:%02d", entry->_hour, entry->_minute]];
    }
    [statusText setStringValue: [NSString stringWithFormat: @"Backups are currently scheduled monthly on %@ at %02d:%02d", entry->_day, entry->_hour, entry->_minute]];
  } 
  else
  {
    if(controls == YES)
    {
      [schedulePopUp selectItemAtIndex: 1];
      [scheduleTab selectTabViewItemAtIndex: 1];
      [scheduleDailyTime setStringValue: [NSString stringWithFormat: @"%02d:%02d", entry->_hour, entry->_minute]];
    }
    [statusText setStringValue: [NSString stringWithFormat: @"Backups are currently scheduled daily at %02d:%02d", entry->_hour, entry->_minute]];
  }
  
}


- (BOOL)exchangeCronEntry: entry
{
  MCrontab *cron = [[MCrontab alloc] init];
  [cron load];
  
  MCrontabEntry *prev = [cron findEntryByComment: [self getCommentText]];
  if(prev != nil)
  {
    [cron removeCommand: [self getMABackupToolPath] withComment: [self getCommentText]];
    prev = nil;
  }
  
  if(entry != nil)
  {
    [cron addEntry: entry withComment: [self getCommentText]];
  }
  [cron installTable];
  [self updateScheduleStatus:entry updateControls:NO];
  return YES;
}


- (NSString *)getCommandText
{
  NSString *retval = [NSString stringWithFormat: @"%@ --directory=\"%@\" --connection=\"%@\"", [self getMABackupToolPath], [targetText stringValue], [[connectionPopUp selectedItem] title]];
  if([[prefixText stringValue] length] > 0)
  {
    retval = [retval stringByAppendingFormat: @" --prefix=\"%@\"", [prefixText stringValue]];
  }
  retval = [retval stringByAppendingFormat: @" \"%@\"", [_profiles objectAtIndex: [profileTable selectedRow]]];
  return retval;
}

- (IBAction)scheduleBackup:(id)sender
{
  MCrontabEntry *entry = nil;
  NSCalendarDate *cd = nil;
  NSArray *weekdays;
  
  switch([schedulePopUp indexOfSelectedItem])
  {
    case 0:
      break;
    case 1:
      cd = [NSCalendarDate dateWithString: [scheduleDailyTime stringValue] calendarFormat: @"%H:%M"];
      entry = [[MCrontabEntry alloc] init];
      entry->_minute = [cd minuteOfHour];
      entry->_hour = [cd hourOfDay];
      entry->_command = [self getCommandText];
      break;
    case 2:
      weekdays = [scheduleWeekdays cells];
      int mask = 0;
      unsigned int i;
      for(i = 0; i < [weekdays count]; i++)
      {
        NSButtonCell *wd = [weekdays objectAtIndex: i];
        if([wd intValue] > 0)
        {
          mask |= (1 << i);
        }
      }
        cd = [NSCalendarDate dateWithString: [scheduleWeeklyTime stringValue] calendarFormat: @"%H:%M"];
      entry = [[MCrontabEntry alloc] init];
      entry->_minute = [cd minuteOfHour];
      entry->_hour = [cd hourOfDay];
      entry->_command = [self getCommandText];
      entry->_weekday = mask;
      break;
    case 3:
      cd = [NSCalendarDate dateWithString: [monthlyTimeText stringValue] calendarFormat: @"%H:%M"];
      entry = [[MCrontabEntry alloc] init];
      entry->_minute = [cd minuteOfHour];
      entry->_hour = [cd hourOfDay];
      entry->_command = [self getCommandText];
      entry->_day = [monthlyDaysText stringValue];
      break;
  }
  
  [self exchangeCronEntry: entry];
  
}

- (IBAction)unscheduleBackup:(id)sender
{
  [self exchangeCronEntry: nil];
}

- (IBAction)abortBackup:(id)sender
{
  _backupAborted= YES;
}


+ (NSImage*)icon
{
  return [NSImage imageNamed:@"OSX-Icons_47.png"];
}

+ (NSString*)label
{
  return @"Backup";
}

+ (NSString*)toolTip
{
  return @"Perform and Schedule Backups of MySQL Databases.";
}

- (id)initWithOwner: (id<MAdministratorProtocol>)owner
{
  self= [super initWithNibFile: @"Backup" panelOwner: owner];
  if (self)
  {
    _defaultFrame= [[self topView] frame];
    _profiles= [[NSMutableArray alloc] init];
    _selectionDS= [[MABackupDataSource alloc] init];
    [contentOutline setDataSource:_selectionDS];
    
    _schemaIcon= [[NSImage imageNamed:@"16x16_Database.png"] retain];
    _tableIcon= [[NSImage imageNamed:@"16x16_Table.png"] retain];
    _viewIcon= [[NSImage imageNamed:@"16x16_View.png"] retain];
    _procIcon= [[NSImage imageNamed:@"16x16_StoredProc.png"] retain];
    _funcIcon= [[NSImage imageNamed:@"16x16_StoredProc.png"] retain];
    
    [self refreshProfiles];
  }
  
  return self;
}

- (void)dealloc
{
  [_schemaIcon release];
  [_tableIcon release];
  [_viewIcon release];
  [_procIcon release];  
  [_funcIcon release];
  [_selectionDS release];
  [_profiles release];
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [super dealloc];
}


- (BOOL)willHide
{
  if (_profile)
    [self saveProfile];
  return YES;
}

- (BOOL)willClose
{
  if (_profile)
    [self saveProfile];
  return YES;
}

- (void)loadConnections
{
  NSString *connFile = [[MPreferences preferences] pathForFile:@"mysqlx_user_connections.xml"];
  [connectionPopUp removeAllItems];
  MYX_LIB_ERROR conerr;
  if(_pconns != NULL)
  {
    myx_free_user_connections(_pconns);
  }
  _pconns = myx_load_user_connections([connFile cString], &conerr);
  if(_pconns != NULL)
  {
    unsigned int i;
    for(i = 0; i < _pconns->user_connections_num; i++)
    {
      if(_pconns->user_connections[i].connection_name != NULL)
      {
        [connectionPopUp addItemWithTitle: [NSString stringWithCString: _pconns->user_connections[i].connection_name]];
        
      } 
    }
  }
}


- (void)awakeFromNib
{
  MTextImageCell *tcell= [[[MTextImageCell alloc] init] autorelease];
  MButtonImageCell *bcell= [[[MButtonImageCell alloc] init] autorelease];
  [tcell setEditable: NO];
  [[schemaOutline tableColumnWithIdentifier:@"text"] setDataCell: tcell];
  
  [bcell setButtonType: NSSwitchButton];
  [bcell setControlSize: NSSmallControlSize];
  [bcell setAllowsMixedState: YES];
  [[contentOutline tableColumnWithIdentifier:@"object"] setDataCell: bcell];  
  
  [schemaOutline setDataSource:[_owner sharedSchemaDS]];
  MXExpandOutline(schemaOutline, NO);

  [nameText setDelegate:self];
  
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(schemaDSChanged:)
                                               name:MASchemaDataSourceChanged
                                             object:_owner];
  
  [[[topBox viewWithTag:201] cell] setImageDimsWhenDisabled:NO];
  [[topBox viewWithTag:201] setImage:[NSImage imageNamed:@"del_disabled.png"]];
  [[topBox viewWithTag:201] setEnabled:NO];
  
  // disable backup scheduling page for now
  //[tabView removeTabViewItem:[tabView tabViewItemAtIndex:2]];
  
  [[[tabView tabViewItemAtIndex: 2] view] setEnabledRecursive: [profileTable selectedRow] >= 0];
  
	[self loadConnections];
	
	MCrontab *cron = [[MCrontab alloc] init];
	[cron load];
	MCrontabEntry *entry = [cron findEntryByComment: [self getCommentText]];
	[self updateScheduleStatus: entry updateControls: YES];
  
  [self tableViewSelectionDidChange:nil];
}


//========================== Table Delegate =====================
- (void)tableViewSelectionDidChange:(NSNotification *)aNotification
{
  int row= [profileTable selectedRow];
  MCrontabEntry *entry = nil;
  
  if (_profile)
    [self saveProfile];
  
  if (row >= 0)
  {
    [self openProfile: [_profiles objectAtIndex:row]];
    [[topBox viewWithTag:201] setEnabled:YES];
    [[topBox viewWithTag:201] setImage:[NSImage imageNamed:@"del_normal.png"]];
    
    [[topBox viewWithTag:10] setEnabled:YES];

    [tabView setHidden:NO];
    [[topBox viewWithTag:50] setHidden:YES];

    MCrontab *cron = [[[MCrontab alloc] init] autorelease];
    [cron load];
    entry = [cron findEntryByComment: [self getCommentText]];
  }
  else
  {
    [self openProfile: nil];
    [[topBox viewWithTag:201] setEnabled:NO];
    [[topBox viewWithTag:201] setImage:[NSImage imageNamed:@"del_disabled.png"]];
    
    [[topBox viewWithTag:10] setEnabled:NO];
    
    [tabView setHidden:YES];
    [[topBox viewWithTag:50] setHidden:NO];
    [scheduleTab selectFirstTabViewItem:nil];
  }
  
  [self updateScheduleStatus: entry updateControls: YES];
  [[[tabView tabViewItemAtIndex: 2] view] setEnabledRecursive: row >= 0];
}

//========================== Outline Delegate =====================

- (void)outlineView:(NSOutlineView *)outlineView 
    willDisplayCell:(id)cell 
     forTableColumn:(NSTableColumn *)tableColumn 
               item:(id)item
{
  if (outlineView == schemaOutline)
  {
    if ([item respondsToSelector:@selector(icon)]) 
      [cell setImage:[item icon]];
    else
      [cell setImage:nil];
  }
  else if (outlineView == contentOutline)
  {
    if ([[tableColumn identifier] isEqualToString:@"object"])
    {
      [cell setTitle:[item repr]];
      if ([item isMemberOfClass:[MABackupSelectionItem class]])
        [cell setIconImage:_schemaIcon];
      else
      {
        switch ([item tag])
        {
          case 'T':
            [cell setIconImage:_tableIcon];
            break;
          case 'F':
            [cell setIconImage:_funcIcon];
            break;
          case 'P':
            [cell setIconImage:_procIcon];
            break;
          case 'V':
            [cell setIconImage:_viewIcon];
            break;
        }
      }
    }
  }
}


- (void)outlineViewSelectionDidChange:(NSNotification *)notification
{
  if ([notification object] == schemaOutline)
  {
    if ([schemaOutline numberOfSelectedRows] == 0 || [profileTable selectedRow] < 0)
      [contentAddButton setEnabled:NO];
    else
    {
      NSIndexSet *rows= [schemaOutline selectedRowIndexes];
      NSArray *children= [[_selectionDS root] children];
      unsigned int row;
      BOOL flag= NO;
      unsigned int c= [children count];
      
      for (row= [rows firstIndex]; row != NSNotFound; row= [rows indexGreaterThanIndex:row])
      {
        MSchemaItem *item;
        
        item= [schemaOutline itemAtRow:row];
        if (item->type == MSchemaItemType)
        {
          unsigned int i;
          for (i= 0; i < c; i++)
          {
            if ([[children objectAtIndex:i] isEqualToItem:item])
              break;
          }
          if (i == c)
            flag= YES;
        }
      }
      [contentAddButton setEnabled:flag];
    }
  }
  else if ([notification object] == contentOutline)
  {
    if ([contentOutline numberOfSelectedRows] == 0 || [profileTable selectedRow]<0)
      [contentRemoveButton setEnabled:NO];
    else
      [contentRemoveButton setEnabled:YES];
  }  
}

- (IBAction)showConnectionEditor:(id)sender;
{
  MPreferenceEditor *editor = [[MPreferenceEditor alloc] initForConnectionsFile: [[MPreferences preferences] pathForFile:@"mysqlx_user_connections.xml"]];  
  [editor runConnectionEditor];
  [editor release];
  [self loadConnections];
}

//======================== Table DataSource =====================

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  if (aTableView == profileTable)
  {    
    return [_profiles count];
  }
  return 0;
}

- (id)tableView:(NSTableView *)aTableView 
objectValueForTableColumn:(NSTableColumn *)aTableColumn
            row:(int)rowIndex
{
  if (aTableView == profileTable)
  {
    return [_profiles objectAtIndex:rowIndex];
  }
  return nil;
}

@end

