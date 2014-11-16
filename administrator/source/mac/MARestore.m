#import "MARestore.h"
#import <MySQLToolsCommon/MTreeDataSource.h>
#import <MySQLToolsCommon/MButtonImageCell.h>
#import <MySQLToolsCommon/MMySQLDispatcher.h>
#import <MySQLToolsCommon/MSchemaDataSource.h>

static NSString *CharacterSets[]= {
  @"utf8",
  @"latin1",
  @"MACCENTRALEUROPE",
  @"macroman",
  @"sjis",
  @"EUC-JP",
  @"big5",
  @"cp850",
  @"cp866",
  @"latin2",
  @"latin5",
  @"latin7",
  @"hebrew",
  @"tis620",
  @"euckr",
  @"gb2312",
  @"greek",
  @"cp1250",
  @"gbk",
  @"cp1251",
  @"cp1256",
  @"cp1257",
  @"HP-ROMAN8",
  @"KOI8-R",
  @"US-ASCII",
  @"KOI8-U",
  @"ARMSCII-8",
  @"UCS-2",
  nil
};



@interface MARestoreSelectionItem : MSelectableTreeItem
{
  MYX_BACKUP_CONTENT *_content;
  MYX_BACKUP_TABLE *_item;
  @public
  char *_catalog;
  char *_schema;
}
- (MARestoreSelectionItem*)initWithContents:(MYX_BACKUP_CONTENT*)content;
- (MARestoreSelectionItem*)initWithTable:(MYX_BACKUP_TABLE*)item;
- (MARestoreSelectionItem*)initWithSchema:(const char*)schema catalog:(const char*)catalog;
- (MARestoreSelectionItem*)findSchema:(const char*)schema
                              catalog:(const char*)catalog;
- (MYX_BACKUP_TABLE*)item;
@end

@implementation MARestoreSelectionItem
- (MARestoreSelectionItem*)initWithContents:(MYX_BACKUP_CONTENT*)content
{
  self= [super initWithTag:'C' repr:nil];
  if (self)
  {
    unsigned int i;
    _content= content;
    _selected= YES;
    
    for (i= 0; i < content->tables_num; i++)
    {
      MARestoreSelectionItem *item= [self findSchema:content->tables[i].schema
                                    catalog:content->tables[i].catalog];
      MARestoreSelectionItem *titem;
      if (!item)
      {
        item= [[MARestoreSelectionItem alloc] initWithSchema:content->tables[i].schema
                                            catalog:content->tables[i].catalog];
        [self addChild:item];
        [item release];
      }
      titem= [[MARestoreSelectionItem alloc] initWithTable:content->tables+i];
      [item addChild:titem];
      [titem release];
    }
    
  }
  return self;  
}

- (MARestoreSelectionItem*)initWithTable:(MYX_BACKUP_TABLE*)item
{
  if (item->flags & MYX_BTF_IS_VIEW)
    self= [super initWithTag:'V' repr:[NSString stringWithUTF8String:item->table]];
  else if (item->flags & MYX_BTF_IS_PROCEDURE)
    self= [super initWithTag:'P' repr:[NSString stringWithUTF8String:item->table]];
  else if (item->flags & MYX_BTF_IS_FUNCTION)
    self= [super initWithTag:'F' repr:[NSString stringWithUTF8String:item->table]];
  else
    self= [super initWithTag:'T' repr:[NSString stringWithUTF8String:item->table]];
  if (self)
  {
    _item= item;
    _selected= NSOnState;
  }
  return self;
}

- (MARestoreSelectionItem*)initWithSchema:(const char*)schema catalog:(const char*)catalog
{
  self= [super initWithTag:'S' repr:[NSString stringWithFormat:@"%s (%s)",schema,catalog]];
  if (self)
  {
    _item= NULL;
    _catalog= g_strdup(catalog);
    _schema= g_strdup(schema);
    _selected= YES;
  }
  return self;
}

- (void)dealloc
{
  g_free(_catalog);
  g_free(_schema);
  [super dealloc];
}

- (MYX_BACKUP_TABLE*)item
{
  NSAssert(_tag != 'S', @"calling item in bad item");
  return _item;
}

- (MARestoreSelectionItem*)findSchema:(const char*)schema
                     catalog:(const char*)catalog
{
  unsigned int i, c= [_children count];
  for (i= 0; i < c; i++)
  {
    MARestoreSelectionItem *item= [_children objectAtIndex:i];
    
    if (strcmp(item->_catalog?:"", catalog?:"")==0
        && strcmp(item->_schema?:"", schema?:"")==0)
      return item;
  }
  return nil;
}


- (id)valueForIdentifier:(NSString*)identifier
{
  if ([identifier isEqualToString:@"type"])
  {
    switch (_tag)
    {
      case 'T': return @"Table";
      case 'V': return @"View";
      case 'P': return @"Procedure";
      case 'F': return @"Function";
    }
    return nil;
  }
  else
    return [NSNumber numberWithInt:[self selected]];
}

@end


@interface MARestoreDataSource : MTreeDataSource
{
}

- (void)outlineView:(NSOutlineView *)outlineView 
     setObjectValue:(id)object 
     forTableColumn:(NSTableColumn *)tableColumn 
             byItem:(id)item;

@end

@implementation MARestoreDataSource
- (void)outlineView:(NSOutlineView *)outlineView 
     setObjectValue:(id)object 
     forTableColumn:(NSTableColumn *)tableColumn 
             byItem:(id)item
{
  [item setSelected:[object intValue]!=NSOffState];
  
  [outlineView reloadData];
}

@end

//=============================================================


typedef struct {
  const char *path;
  const char *charset;
  MYX_BACKUP_CONTENT *content;
  char *target_catalog;
  char *target_schema;
  int options;
  id me;
} RestoreData;



typedef struct {
  void *data;
  MYX_BACKUP_CONTENT *content;
  MYX_BACKUP_ERROR error;
} LoadFileData;


@interface MARestore(Private)
- (void)openPanelDidEnd:(NSOpenPanel *)sheet returnCode:(int)returnCode contextInfo:(void *)contextInfo;
- (void)backupFileLoaded:(id)arg result:(void*)result;
- (void)restoreFinished:(id)arg result:(void*)result;
@end

@implementation MARestore(Private)

- (void)backupFileLoaded:(id)arg result:(void*)result
{
  LoadFileData *lfd= (LoadFileData*)result;
 
  [[topBox viewWithTag:10] setHidden:YES];
  [parseProgress setHidden:YES];

  if (lfd->content)
  {
    if (_content)
      myx_free_backup_content(_content);
    _content= lfd->content;

    [informationText setStringValue:
      [NSString stringWithFormat:@"%@\n%i\n%lli bytes",[arg lastPathComponent],
        _content->tables_num, get_file_size([arg UTF8String])]];
    
    [_selectionDS setRoot:[[[MARestoreSelectionItem alloc] initWithContents:_content] autorelease]];
    [selectionOutline reloadData];
    
    [restoreButton setEnabled:YES];
  }
  else
  {
    if (lfd->error != MYX_BACKUP_NO_ERROR)
    {
      char *msg= myx_get_backup_error_string(lfd->error);
      NSRunAlertPanel(@"Error",[NSString stringWithFormat:@"Error loading backup file.\n%s",msg?:""],
                      @"OK",nil,nil);
      g_free(msg);
    }
  }
  
  MYX_CATALOGS *cats= [[_owner sharedSchemaDS] catalogs];
  unsigned int c, s;
  [restoreTargetCombo removeAllItems];
  [restoreTargetCombo addItemWithObjectValue:@"Original Schema"];
  for (c= 0; c < cats->catalogs_num; c++)
  {
    for (s= 0; s < cats->catalogs[c].schemata_num; s++)
    {
      [restoreTargetCombo addItemWithObjectValue:[NSString stringWithFormat:@"%s.%s",cats->catalogs[c].catalog_name,cats->catalogs[c].schemata[s].schema_name]];
    }
  }
  [restoreTargetCombo selectItemAtIndex:0];
  
  g_free(lfd);
}

static int parseProgressCallback(bigint bytes_read, bigint bytes_total, void *ud)
{
  MARestore *panel = (MARestore*)ud;
   
  [panel->parseProgress setDoubleValue:(double)bytes_read*100.0/(double)bytes_total];
  
  return 0;
}

static void *loadBackupFile(MYSQL *mysql, char *filename, char *charset, void *data)
{
  LoadFileData *lfd= (LoadFileData*)data;


  lfd->content= myx_get_backup_content(filename, charset,
                                       MYX_BT_SQL_SCRIPT,
                                       1024, // update every 1024 bytes
                                       parseProgressCallback, NULL, lfd->data, &lfd->error, 0, 1);

  if (lfd->error != MYX_BACKUP_NO_ERROR)
    NSLog(@"Error reading backup content file %i", lfd->error);

  return lfd;
}


- (void)restoreFinished:(id)arg result:(void*)result
{
  MYX_BACKUP_ERROR err= (MYX_BACKUP_ERROR)result;
  RestoreData *rdata= (RestoreData*)arg;
  [progressPanel orderOut:self];
  [NSApp abortModal];

  if (err == MYX_BACKUP_NO_ERROR)
  {
    if (_abortRestore)
      NSRunAlertPanel(@"Restore Interrupted", @"Backup Restoration Interrupted by User", nil, nil, nil);
    else
      NSRunAlertPanel(@"Restore Finished", @"Backup Restoration Finished", nil, nil, nil);
  }
  else
  {
    if (err == MYX_BACKUP_SERVER_ERROR)
    {
      NSRunAlertPanel(@"Error",[NSString stringWithFormat:@"MySQL error while restoring backup:\n%@",
        [[_owner dispatcher] lastMySQLError]],
                      @"OK",nil,nil);
    }
    else
    {
      char *msg= myx_get_backup_error_string(err);
      NSRunAlertPanel(@"Error",[NSString stringWithFormat:@"Error restoring backup:\n%s",msg?:""],
                      @"OK",nil,nil);
      g_free(msg);
    }
  }
  g_free(rdata->content->tables);
  g_free(rdata->content);
  g_free(rdata);
}

- (void)openPanelDidEnd:(NSOpenPanel *)sheet returnCode:(int)returnCode contextInfo:(void *)contextInfo
{
  if (returnCode == NSOKButton)
  {
    NSString *file= [sheet filename];
    LoadFileData *lfdata= g_new0(LoadFileData, 1);
    
    [_charset release];
    [_path release];
    
    _path= [file retain];
    _charset= [[[charsetPopUp selectedItem] title] retain];

    [[topBox viewWithTag:10] setHidden:NO];
    [parseProgress setDoubleValue:0.0];
    [parseProgress setHidden:NO];
    
    lfdata->content= NULL;
    lfdata->data= self;
    lfdata->error= 0;
    
    [_owner sharedSchemaDS];

    [[_owner dispatcher] performCallback:(void*(*)(MYSQL*,void*,void*,void*))loadBackupFile
                                argument:(void*)[file UTF8String]
                                argument:(void*)[_charset UTF8String]
                                argument:lfdata
                        finishedSelector:@selector(backupFileLoaded:result:)
                                argument:[file retain]
                                  target:self];
  }
}

static int restoreProgressCallback(bigint bytes_read, bigint bytes_total, void *user_data)
{
  MARestore *panel= (MARestore*)user_data;

  [panel->restoreProgress setDoubleValue:(double)bytes_read*100.0/bytes_total];

  return panel->_abortRestore ? -1 : 0;
}


- (void)logWarning:(id)msg
{
  NSRect frame= [progressPanel frame];
  frame.size= [progressPanel maxSize];
  [progressPanel setFrame:frame display:YES];
  
  [[warningText textStorage] appendAttributedString:[[[NSAttributedString alloc] initWithString: msg] autorelease]];
}


static void restoreWarningCallback(const char *msg, void *data)
{
  MARestore *restore= (MARestore*)data;
  
  [restore performSelectorOnMainThread:@selector(logWarning)
							withObject:restore waitUntilDone:NO];
  
  NSLog(@"Backup Restore: WARNING: %s", msg);
}

static void *restoreBackup(MYSQL *mysql, RestoreData *arg)
{
  MYX_BACKUP_ERROR err;
  
  err= myx_restore_backup(mysql,
                          arg->path, arg->charset,
                          arg->content,
                          arg->target_catalog,
                          arg->target_schema,
                          MYX_BT_SQL_SCRIPT,
                          arg->options,
                          1024,
                          restoreProgressCallback, arg->me,
                          restoreWarningCallback, arg->me);

  return (void*)err;
}



- (void)outlineView:(NSOutlineView *)outlineView 
    willDisplayCell:(id)cell 
     forTableColumn:(NSTableColumn *)tableColumn 
               item:(id)item
{
  if ([[tableColumn identifier] isEqualToString:@"object"])
  {
    [cell setTitle:[item repr]];
    switch ([item tag])
    {
      case 'S':
        [cell setIconImage:_schemaIcon];
        break;
      case 'T':
        [cell setIconImage:_tableIcon];
        break;
      case 'V':
        [cell setIconImage:_viewIcon];
        break;
    }
  }
}

@end

@implementation MARestore

- (IBAction)abortRestore:(id)sender
{
  _abortRestore= YES;
}

- (IBAction)startRestore:(id)sender
{
  RestoreData *arg= g_new(RestoreData, 1);
  MYX_BACKUP_CONTENT *content= g_new0(MYX_BACKUP_CONTENT, 1);
  MARestoreSelectionItem *root= (MARestoreSelectionItem*)[_selectionDS root];
  NSArray *array= [root children];
  unsigned int i, c= [array count];

  content->tables= g_new0(MYX_BACKUP_TABLE, _content->tables_num);
  content->tables_num= 0;
  for (i= 0; i < c; i++)
  {
    NSArray *tables= [[array objectAtIndex:i] children];
    unsigned j, cc= [tables count];

    for (j= 0; j < cc; j++)
    {
      MARestoreSelectionItem *ti= [tables objectAtIndex:j];
      if ([ti selected])
        content->tables[content->tables_num++]= *[ti item];
    }
  }

  if ([restoreTargetCombo indexOfSelectedItem] == 0)
  {
    arg->target_catalog= NULL;
    arg->target_schema= NULL;
  }
  else
  {
    NSString *value= [restoreTargetCombo objectValueOfSelectedItem];
    NSArray *parts= [value componentsSeparatedByString:@"."];
    
    if ([parts count] == 1)
    {
      arg->target_catalog= NULL;
      arg->target_schema= (char*)[[parts lastObject] UTF8String];      
    }
    else
    {
      if ([[parts objectAtIndex:0] isEqualToString:@"def"])
        arg->target_catalog= NULL;
      else
        arg->target_catalog= (char*)[[parts objectAtIndex:0] UTF8String];
      arg->target_schema= (char*)[[parts lastObject] UTF8String];
    }
  }
  
  arg->path= [_path UTF8String];
  arg->charset= [_charset UTF8String];
  arg->options= 0;
  arg->content= content;
  arg->me= self;
  
  if ([createCheck state] == NSOffState)
    arg->options|= MYX_RBS_DONT_CREATE_TARGETS;
  if ([ignoreErrorCheck state] == NSOnState)
    arg->options|= MYX_RBS_FORCE;
  
  {
	NSRect rect= [progressPanel frame];
	rect.size= [progressPanel minSize];
	[progressPanel setFrame:rect display:YES];
  }
    
  [[_owner dispatcher] performCallback:(void*(*)(MYSQL*,void*))restoreBackup
                              argument:arg
                      finishedSelector:@selector(restoreFinished:result:)
                              argument:(id)arg
                                target:self];
  
  _abortRestore= NO;
  [restoreProgress setDoubleValue:0.0];
  [progressText setStringValue:[NSString stringWithFormat:@"Restoring file %@...",
    [_path lastPathComponent]]];
  [NSApp runModalForWindow:progressPanel];
}

- (IBAction)chooseFile:(id)sender
{
  NSOpenPanel *panel= [NSOpenPanel openPanel];
  
  [panel setCanChooseDirectories:NO];
  [panel setCanChooseFiles:YES];
  [panel setAllowsMultipleSelection:NO];
  [panel setAccessoryView:accessoryView];

  [panel beginSheetForDirectory:nil file:nil types:[NSArray arrayWithObject:@"sql"]
                 modalForWindow:[topBox window]
                  modalDelegate:self
                 didEndSelector:@selector(openPanelDidEnd:returnCode:contextInfo:)
                    contextInfo:NULL];
}

#if 0
- (IBAction)detectCharset:(id)sender
{
  NSString *fn= [(NSOpenPanel*)[sender window] filename];
  
  if (fn)
  {
    char *charset= myx_detect_charset_from_sql_file([fn UTF8String]);
    if (charset)
    {
      NSString *cs= [NSString stringWithUTF8String:charset];
      NSMenuItem *item= [charsetPopUp itemWithTitle:cs];
      
      if (item)
        [charsetPopUp selectItem:item];
      else
      {
        [charsetPopUp addItemWithTitle:cs];
        [charsetPopUp selectItemWithTitle:cs];
      }
      g_free(charset);
    }
  }
}
#endif

- (void)dealloc
{
  [_selectionDS release];
  [_schemaIcon release];
  [_tableIcon release];
  [_viewIcon release];
  if (_content)
    myx_free_backup_content(_content);
  [super dealloc];
}


+ (NSImage*)icon
{
  return [NSImage imageNamed:@"OSX-Icons_49.png"];
}

+ (NSString*)label
{
  return @"Restore";
}

+ (NSString*)toolTip
{
  return @"Restore Database Backups.";
}

- (id)initWithOwner: (id<MAdministratorProtocol>)owner
{
  self= [super initWithNibFile: @"Restore" panelOwner: owner];
  if (self)
  {
    unsigned int i;
    MButtonImageCell *bcell= [[[MButtonImageCell alloc] init] autorelease];
    [bcell setButtonType: NSSwitchButton];
    [bcell setControlSize: NSSmallControlSize];
    [bcell setAllowsMixedState: YES];
    
    _defaultFrame= [[self topView] frame];
    
    _schemaIcon= [[NSImage imageNamed:@"16x16_Database.png"] retain];
    _tableIcon= [[NSImage imageNamed:@"16x16_Table.png"] retain];
    _viewIcon= [[NSImage imageNamed:@"16x16_View.png"] retain];
    
    [[selectionOutline tableColumnWithIdentifier:@"object"] setDataCell:bcell];
    
    _selectionDS= [[MARestoreDataSource alloc] init];
    [selectionOutline setDataSource:_selectionDS];
    
    [charsetPopUp removeAllItems];
    for (i= 0; CharacterSets[i]; i++)
    {
      [charsetPopUp addItemWithTitle:CharacterSets[i]];
    }
  }
  return self;
}


@end
