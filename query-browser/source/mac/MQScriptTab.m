//
//  MQScriptTab.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/25/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQScriptTab.h"
#import <MySQLToolsCommon/MSourceTextEditor.h>
#import <MySQLToolsCommon/MSplitView.h>
#import <MySQLToolsCommon/MSourceTextView.h>
#import <MySQLToolsCommon/MSQLSyntaxColoring.h>
#import <MySQLToolsCommon/MSchemaDataSource.h>
#import <MySQLToolsCommon/MConnectionInfo.h>
#import <MySQLToolsCommon/mxUtils.h>

#import "MQScriptExecutionHelper.h"

NSString *MQScriptEditStatusChangedNotification= @"MQScriptEditStatusChangedNotification";


@implementation MQScriptTab

- (id)initWithIdentifier:(NSString*)identifier withConnectionTo:(MConnectionInfo*)info
{
  self= [super initWithIdentifier:identifier];
  if (self)
  {
    NSUserDefaults *defaults= [NSUserDefaults standardUserDefaults];
    
    [NSBundle loadNibNamed:@"ScriptEditor" owner:self];
    
    _info= [info retain];

    if (_info)
    {
      MYX_USER_CONNECTION *conn;
      _mysql= myx_mysql_init();
      if (!_mysql)
      {
        NSLog(@"could not create MySQL connection");
        [self release];
        return nil;
      }
      conn= [info createUserConnection];
      if (myx_connect_to_instance(conn, _mysql) < 0)
      {
        myx_free_user_connection_content(conn);
        g_free(conn);
        MXRunAlertPanelWithMySQLError(@"Error", @"Could not connect to MySQL instance.", _mysql);
        [self release];
        myx_mysql_close(_mysql);
        return nil;
      }
      myx_free_user_connection_content(conn);
      g_free(conn);
    }
    [splitView setDividerThickness:2.0];
    [splitView resizeSubview:[messageTable enclosingScrollView]
                      toSize:0.0];

    [editor setShowGutter:YES];
    [[editor textView] registerForDraggedTypes:[NSArray arrayWithObject:MSchemaItemPboardType]];
    [[editor textView] setAllowsUndo:YES];
    
    [[editor textView] setFont:[NSUnarchiver unarchiveObjectWithData:[defaults objectForKey:@"EditorFont"]]];
    [[editor textView] setBackgroundColor:[NSUnarchiver unarchiveObjectWithData:[defaults objectForKey:@"EditorBackgroundColor"]]];
    [[editor textView] setTextColor:[NSUnarchiver unarchiveObjectWithData:[defaults objectForKey:@"EditorForegroundColor"]]];

    _syn= myx_refresh_dbinfo(_mysql, NULL);

    _colorer= [[MSQLSyntaxColoring alloc] initForTextView:[editor textView]
                                               syntaxInfo:_syn];
    [[editor textView] setSyntaxColorer: _colorer];
    
    _helper= [[MQScriptExecutionHelper alloc] initWithText:editor];
    [_helper setConnection:_mysql];
    [_helper setErrorHandler:self
                    selector:@selector(handleScript:error:)];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(handleScriptEdited:)
                                                 name:NSTextStorageDidProcessEditingNotification
                                               object:[[editor textView] textStorage]];
  /*  [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(updateCursor:)
                                                 name:NSTextViewDidChangeSelectionNotification
                                               object:[editor textView]];
    */
    [_colorer addMarkerName:@"breakpoint"];
    [_colorer addMarkerName:@"statement"];
    [_colorer addMarkerName:@"error" withColor:[NSColor redColor]];
    [_colorer addMarkerName:@"pc" withColor:[NSColor blueColor]];
  }
  return self;
}


- (void)resetMessages
{
  if (_errors)
    myx_mysql_error_msgs_free(_errors);
  _errors= 0;
  [splitView resizeSubview:[messageTable enclosingScrollView]
                    toSize:0.0];
}

- (void)dealloc
{
  [_info release];
  [self resetMessages];
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  myx_free_syn(_syn);
  [_helper release];
  [_colorer release];
  [super dealloc];
}

- (void)setFilename:(NSString*)name
{
  if (_filename != name)
  {
    [_filename release];
    _filename= [name retain];
  }
}

- (void)setDocumentEdited:(BOOL)flag
{
  _documentEdited= flag;
  [[NSNotificationQueue defaultQueue] 
    enqueueNotification:[NSNotification notificationWithName:MQScriptEditStatusChangedNotification
                                                      object:self]
           postingStyle:NSPostWhenIdle
           coalesceMask:(NSNotificationCoalescingOnName|NSNotificationCoalescingOnSender)
               forModes:nil];
}

#if 0
- (void)updateCursor:(NSNotification*)notif
{
  int column, row;
  column= [[editor textView] selectedRange].location;
  row= [[[editor textView] syntaxColorer] lineIndexForCharacter:column];
  column-= [[[editor textView] syntaxColorer] characterRangeOfLine:row].location;
}
#endif


- (void)handleScript:(MQScriptExecutionHelper*)helper error:(NSDictionary*)info
{
  unsigned int i;
  int errCount= 0;
  if (!info)
  {
    [self resetMessages];
    [[[self view] viewWithTag:10] setImage:nil];
    return;
  }

  if ([info objectForKey:@"error"])
  {
    [self setStatusText:[NSString stringWithFormat:@"Error: %@ (%@)",
      [info objectForKey:@"error"], [info objectForKey:@"errno"]]
                   icon:MXGetCommonImage(MXMiniErrorImage)];
  }
  else
    [self setStatusText:@"" icon:nil];
  
  if (!_errors)
  {
    int line= [[info objectForKey:@"line"] intValue];
    _errors= [[info objectForKey:@"msgs"] pointerValue];
    
    for (i= 0; i < _errors->errors_num; i++)
    {
      char *tmp= _errors->errors[i].text;
      _errors->errors[i].text= g_strdup_printf("%i: %s", line+1, tmp);
      g_free(tmp);
      errCount++;
    }
  }
  else
  {
    MYX_MYSQL_ERROR_MSGS *msgs= [[info objectForKey:@"msgs"] pointerValue];
    int line= [[info objectForKey:@"line"] intValue];
    _errors->errors= realloc(_errors->errors, 
                           sizeof(MYX_MYSQL_ERROR_MSG)*(_errors->errors_num+msgs->errors_num));
    memcpy(_errors->errors+_errors->errors_num, msgs->errors, msgs->errors_num*sizeof(MYX_MYSQL_ERROR_MSG));

    for (i= 0; i < msgs->errors_num; i++)
    {
      char *tmp= _errors->errors[_errors->errors_num+i].text;
      _errors->errors[_errors->errors_num+i].text= g_strdup_printf("line %i: %s", line, tmp);
      g_free(tmp);
      errCount++;
    }
    _errors->errors_num+= msgs->errors_num;
    free(msgs->errors);
    free(msgs);
  }
  [messageTable reloadData];
  MXResizeTableColumnsToFit(messageTable,[[messageTable tableColumns] lastObject]);
  
  if (errCount > 1)
    [splitView resizeSubview:[messageTable enclosingScrollView] toSize:100.0];
}


- (void)handleScriptEdited:(NSNotification*)notif
{
  id object= [notif object];
  NSRange lineRange= [[[editor textView] syntaxColorer] lineRangeForCharacterRange:[object editedRange]];
  
  [_helper reparseNearLines:lineRange];
  
//  [self updateCursor:nil];
  [self setDocumentEdited:YES];
}


- (void)setDefaultSchema:(NSString*)schema
{
  if (_defaultSchema != schema)
  {
    [_defaultSchema release];
    _defaultSchema= [schema retain];
    
    myx_use_schema(_mysql,[_defaultSchema UTF8String]);
    
    myx_refresh_dbinfo(_mysql, _syn);
  }
  
  if (schema)
    [self setStatusText:[NSString stringWithFormat:@"Selected schema '%@'.", schema]
                   icon:nil];
  else
    [self setStatusText:@"No schema selected."
                   icon:MXGetCommonImage(MXMiniNoticeImage)];
}


- (NSString*)defaultSchema
{
  return _defaultSchema;
}


- (IBAction)saveScript:(id)sender
{
  NSString *path= _filename;

  if (!_filename)
  {
    NSSavePanel *panel= [NSSavePanel savePanel];
    
    [panel setTitle:@"Save SQL Script"];
    if ([panel runModal] == NSFileHandlingPanelOKButton)
    {
      path= [panel filename];
    }
    else
      path= nil;
  }

  if (path)
  {
    NSString *text= [[editor textView] string];
    FILE *file= fopen([path fileSystemRepresentation], "w");
    if (file)
    {
      const char *utfData= [text UTF8String];
      fwrite(utfData, 1, strlen(utfData), file);
      fclose(file);
      
      if (path != _filename)
      {
        [_filename release];
        _filename= [path retain];
      }
      [self setDocumentEdited:NO];
    }
    else
    {
      NSRunAlertPanel(@"Error",[NSString stringWithFormat:@"Could not write to file: %s",strerror(errno)],
                      @"OK", nil, nil);
    }
  }
}

- (IBAction)copyMessage:(id)sender
{
  int row= [messageTable selectedRow];
  if (row >= 0)
  {
    NSString *msg= [NSString stringWithUTF8String:_errors->errors[row].text];
    
    NSPasteboard *pasteboard= [NSPasteboard generalPasteboard];
    [pasteboard declareTypes:[NSArray arrayWithObject:NSStringPboardType]
                       owner:nil];
    [pasteboard setString:msg forType:NSStringPboardType];    
  }
}


- (IBAction)clearMessages:(id)sender
{
  myx_mysql_error_msgs_free(_errors);
  _errors= NULL;
  
  [messageTable reloadData];
  [splitView resizeSubview:[messageTable enclosingScrollView]
                    toSize:0.0];
}


- (MQScriptEditor*)scriptEditor
{
  return editor;
}

- (MQScriptExecutionHelper*)scriptHelper
{
  return _helper;
}

- (BOOL)isDocumentEdited
{
  return _documentEdited;
}

- (NSString*)documentName
{
  return [_filename lastPathComponent];
}


- (void)setScript:(NSString*)script
{
  [[editor textView] setString:script];
}


- (NSString*)script
{
  return [[editor textView] string];
}


- (void)setStatusText:(NSString*)text icon:(NSImage*)icon
{
  [statusText setStringValue:text];
  [statusImage setImage:icon];
}


- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  if (_errors)
    return _errors->errors_num;
  else
    return 0;
}


- (id)tableView:(NSTableView *)aTableView
 objectValueForTableColumn:(NSTableColumn *)aTableColumn
            row:(int)rowIndex
{
  return [NSString stringWithUTF8String:_errors->errors[rowIndex].text];
}


- (void)tableView:(NSTableView *)aTableView
  willDisplayCell:(id)aCell
   forTableColumn:(NSTableColumn *)aTableColumn
              row:(int)rowIndex
{
  switch (_errors->errors[rowIndex].level)
  {
    case MYX_QEL_NOTE:
      [aCell setImage:MXGetCommonImage(MXMiniNoticeImage)];
      break;
    case MYX_QEL_WARNING:
      [aCell setImage:MXGetCommonImage(MXMiniWarningImage)];
      break;
    case MYX_QEL_ERROR:
      [aCell setImage:MXGetCommonImage(MXMiniErrorImage)];
      break;
  }
}

@end
