
#import "MGRTTableEditor.h"
#import <MySQLToolsCommon/MAccessoryScrollView.h>
#import <MySQLToolsCommon/NSView_extras.h>
#import <MySQLToolsCommon/mxUtils.h>
#import <MySQLToolsCommon/myxutil.h>
#import <MySQLToolsCommon/MTextFieldCell.h>
#include <MySQLGRT/MGRTValue.h>

#define COLUMN_TYPE(c) ((c).datatype_pointer ?  (c).datatype_pointer->group : 0)

// TODO: generalize and move to gui-common, make only a subclass specific for GRT

NSString *MGRTTableRowPboardType= @"MGRTTableRowPboardType";

#define COLUMN_NOT_NULL(column) (!(column).get("isNullable", 0))


static char *row_format_values[]= {
  // Letter case does matter
  "Default", "Dynamic", "Fixed", "Compressed", "Redundant", "Compact", NULL
};

static char *merge_insert_values[]= {
  "", "no", "last", "first", NULL
};

static char *pack_key_values[]= {
  "", "0", "1", NULL
};

static int index_for_value(const char *value, char **options)
{
  int i;
  for (i= 0; options[i]; i++)
  {
    if (strcasecmp(value, options[i])==0)
      return i;
  }
  return 0;
}


@interface MGRTTableEditor(Private)
- (void)addColumn:(id)sender;
- (void)deleteColumn:(id)sender;
- (void)showColumn:(MGRTValue*)column;
- (void)addFK:(id)sender;
- (void)deleteFK:(id)sender;
- (void)addFKColumn:(id)sender;
- (void)deleteFKColumn:(id)sender;
- (void)addIndex:(id)sender;
- (void)deleteIndex:(id)sender;
- (void)addIndexColumn:(id)sender;
- (void)deleteIndexColumn:(id)sender;

- (void)setupFlags:(MGRTValue*)column;

- (void)tableViewSelectionDidChange:(NSNotification *)aNotification;
- (BOOL)commitChanges;
- (void)showObject;
- (void)refreshForTableType;
- (void)columnListChanged;
@end


@implementation MGRTTableEditor(Private)

- (void)addColumn:(id)sender
{
  _tableData->addColumn("new_column");

  [columnList reloadData];
  [self columnListChanged];
  
  [columnList selectRow:_tableData->columnCount()-1 byExtendingSelection:NO];
  [columnList editColumn:1 row:_tableData->columnCount()-1 withEvent:nil select:YES];
}

- (void)deleteColumn:(id)sender
{
  int i= [columnList selectedRow];
  if (i>=0)
  {
    [columnList reloadData];
    _tableData->removeColumn(i);
    [columnList reloadData];
    [self columnListChanged];
  }
}

- (void)columnListChanged
{
  NSPopUpButtonCell *pcell= [[indColumnList tableColumnWithIdentifier:@"column"] dataCell];
  NSPopUpButtonCell *pcell2= [[fkColumnList tableColumnWithIdentifier:@"column"] dataCell];
  int i;
  
  [pcell removeAllItems];
  [pcell2 removeAllItems];
  for (i= 0; i < _tableData->columnCount(); i++)
  {
    const char *name= _tableData->getColumn(i)["name"];
    NSString *str= NSStr(name);
    [pcell addItemWithTitle:str];
    [pcell2 addItemWithTitle:str];
  }
}

- (void)addFK:(id)sender
{
  _tableData->addFK("new_fk");

  [fkList reloadData];
  [fkList selectRow:_tableData->fkCount()-1 byExtendingSelection:NO];  
}

- (void)deleteFK:(id)sender
{
  int i= [fkList selectedRow];
  if (i>=0)
  {
    _tableData->removeFK(i);
    
    [fkList reloadData];
  }
}

- (void)addFKColumn:(id)sender
{
  int index= [fkList selectedRow];

  if (index < 0)
    return;
  
  MGRTValue fk(_tableData->getFK(index));
  
  _tableData->addColumnToFK(fk);

  [fkColumnList reloadData];
  [fkColumnList selectRow:fk["columns"].count()-1 byExtendingSelection:NO];  
  [fkColumnList editColumn:0 row:fk["columns"].count()-1 withEvent:nil select:YES];  
}

- (void)deleteFKColumn:(id)sender
{
  int fkindex= [fkList selectedRow];
  int fcolumn= [fkColumnList selectedRow];

  if (fkindex < 0 || fcolumn < 0)
    return;
  
  MGRTValue fk(_tableData->getFK(fkindex));
  
  _tableData->removeColumnFromFK(fk, fcolumn);
  
  [fkColumnList reloadData];
}

- (void)addIndex:(id)sender
{
  _tableData->addIndex("new_index");
  
  [indexList reloadData];
  [indexList selectRow:_tableData->indexCount()-1 byExtendingSelection:NO];
}

- (void)deleteIndex:(id)sender
{
  int i= [indexList selectedRow];
  if (i>=0)
  {
    _tableData->removeIndex(i);
    
    [indexList reloadData];
  }
}

- (void)addIndexColumn:(id)sender
{
  int indexi= [indexList selectedRow];
  
  if (indexi < 0)
    return;

  MGRTValue index(_tableData->getIndex(indexi));
  
  _tableData->addColumnToIndex(index);
    
  [indColumnList reloadData];
  [indColumnList selectRow:index["columns"].count()-1 byExtendingSelection:NO];  
  [indColumnList editColumn:0 row:index["columns"].count()-1 withEvent:nil select:YES];
}

- (void)deleteIndexColumn:(id)sender
{
  int index= [indexList selectedRow];
  int icolumn= [indColumnList selectedRow];

  if (index < 0 || icolumn < 0)
    return;
  
  _tableData->removeColumnFromIndex(_tableData->getIndex(index), icolumn);

  [indColumnList reloadData];
}


- (void)setupFlags:(MGRTValue*)column
{
  while ([colFlagsMatrix numberOfRows] > 3)
    [colFlagsMatrix removeRow:3];
  
  if (column)
  {
    MGRTValue flags(_tableData->getColumnFlags(*column));
    for (int i= 0; i < flags.count(); i++)
    {
      NSButtonCell *cell;
      
      [colFlagsMatrix addRow];
      cell= [colFlagsMatrix cellAtRow:i+3 column:0];
      
      [cell setTitle:NSStr(flags[i].asString())];
      
      [cell setState:_tableData->columnFlagState(*column, flags[i].asString()) ? NSOnState : NSOffState];
    }
    
    NSRect frame= [colFlagsMatrix frame];
    frame.size.height = (3 + flags.count()) * 18;
    frame.origin.y = 133 - frame.size.height;
    [colFlagsMatrix setFrame:frame];
  }
  else
  {
    NSRect frame= [colFlagsMatrix frame];
    frame.size.height = 52;
    frame.origin.y = 133 - frame.size.height;
    [colFlagsMatrix setFrame:frame];    
  }
}


- (void)addColumnWhenIdle:(NSNotification*)notif
{
  if ([columnList numberOfRows] == 0)
  {
    [self addColumn:nil];
  }
  else
  {
    [columnList selectRow:0 byExtendingSelection:NO];
    [columnList editColumn:1 row:0 withEvent:nil select:YES];
  }
}



- (void)handleTextFinished:(NSNotification*)notif
{
  if ([notif object] == tableNameText)
  {
    NSEvent *event= [NSApp currentEvent]; 
    if ([event type] == NSKeyDown && [[event characters] characterAtIndex:0] == NSTabCharacter)
    {
      [[NSNotificationQueue defaultQueue] enqueueNotification:[NSNotification notificationWithName:@"TableEditorAddColumn" object:self]
                                                 postingStyle:NSPostWhenIdle];
    }
  }
}


- (void)handleTextChanged:(NSNotification*)notif
{
  id obj= [notif object];
  NSString *str= nil;

  if ([obj isKindOfClass:[NSTextField class]])
    str= [obj stringValue];
  if (obj == tableNameText || obj == tableName2Text)
  {
    _tableData->setName([[obj stringValue] UTF8String]);
  }
  else if (obj == colNameText)
  {
    MGRTValue column(_tableData->getColumn([columnList selectedRow]));
    _tableData->setColumnName(column, [str UTF8String]);
    
    [self columnListChanged];
    [columnList reloadData];
  }
  else if (obj == colTypeCombo)
  {
    MGRTValue column(_tableData->getColumn([columnList selectedRow]));
    _tableData->setColumnType(column, [str UTF8String]);
    
    [columnList reloadData];
  }
  else if (obj == colDefaultText)
  {
    MGRTValue column(_tableData->getColumn([columnList selectedRow]));
    column.set("defaultValue", [str UTF8String]);
    
    [columnList reloadData];
  }
  else if (obj == colCommentText)
  {
    MGRTValue gcolumn(_tableData->getColumn([columnList selectedRow]));
    
    gcolumn.set("comment", [[[obj textStorage] string] UTF8String]);
    
    [columnList reloadData];
  }  
}


- (void)showColumn:(MGRTValue*)column
{  
  if (column)
  {
    [colNameText setStringValue:NSStr((const char*)(*column)["name"])];
    
    [colTypeCombo setStringValue:NSStr(_tableData->formatColumnType(*column).c_str())];
    [colDefaultNull setState:(int)(*column)["defaultValueIsNull"] != 0 ? NSOnState : NSOffState];

    if ((int)(*column)["defaultValueIsNull"] != 0)
    {
      [colDefaultText setStringValue:@""];
      [colDefaultText setEnabled:NO];
    }
    else
    {
      [colDefaultText setStringValue:NSStr((const char*)(*column)["defaultValue"])];
      [colDefaultText setEnabled:YES];
    }

    [colCollationPop selectItemWithTitle:NSStr(_tableData->getColumnCollation(*column))];
    
    [colCommentText setString:NSStr((*column)["comment"].asString())];

    [self setupFlags:column];
    
    [[colFlagsMatrix cellAtRow:0 column:0] setState:_tableData->columnIsPK(*column)?NSOnState:NSOffState];
    [[colFlagsMatrix cellAtRow:1 column:0] setState:COLUMN_NOT_NULL(*column)?NSOnState:NSOffState];
    [[colFlagsMatrix cellAtRow:2 column:0] setState:(*column).get("autoIncrement", 0)?NSOnState:NSOffState];
    
    [[colFlagsMatrix cellAtRow:1 column:0] setEnabled:_tableData->columnIsPK(*column)?NO:YES];
    [[colFlagsMatrix cellAtRow:2 column:0] setEnabled:_tableData->columnIsNumeric(*column)?YES:NO];
  }
  else
  {
    [colNameText setStringValue:@""];
    
    [colTypeCombo setStringValue:@""];
    [colDefaultText setStringValue:@""];
    [colDefaultNull setState:NSOffState];

    [self setupFlags:column];
    
    [colCollationPop selectItemWithTitle:@""];
    
    [colCommentText setString:@""];
    
    [[colFlagsMatrix cellAtRow:0 column:0] setEnabled:NO];
    [[colFlagsMatrix cellAtRow:1 column:0] setEnabled:NO];
    [[colFlagsMatrix cellAtRow:2 column:0] setEnabled:NO];
  }
}


- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  if (!_tableData)
    return 0;
  
  if (aTableView == columnList)
    return _tableData->columnCount();
  else if (aTableView == indexList)
  {
    return _tableData->indexCount();
  }
  else if (aTableView == indColumnList)
  {
    int index= [indexList selectedRow];
    if (index < 0)
      return 0;
    return _tableData->indexColumnCount(_tableData->getIndex(index));
  }
  else if (aTableView == fkList)
  {
    return _tableData->fkCount();
  }
  else if (aTableView == fkColumnList)
  {
    int index= [fkList selectedRow];
    if (index < 0)
      return 0;
    return _tableData->fkColumnCount(_tableData->getFK(index));
  }
  return 0;
}


- (void)tableView:(NSTableView *)aTableView 
  willDisplayCell:(id)aCell
   forTableColumn:(NSTableColumn *)aTableColumn 
              row:(int)rowIndex
{
  if (aTableView == columnList)
  {
    MGRTValue column(_tableData->getColumn(rowIndex));
    if ([[aTableColumn identifier] isEqualTo:@"icon"])
    {
      NSImage *img= _columnIcon;
      
      if (_tableData->columnIsPK(column))
        img= _pkIcon;
      else
      {
        switch (_tableData->columnTypeGroup(column))
        {
          case RDG_NUMERIC:
            img= _numericIcon;
            break;
          case RDG_DATETIME:
            img= _dateIcon;
            break;
          case RDG_STRING:
            img= _stringIcon;
            break;
          case RDG_TEXT:
            img= _stringIcon;
            break;
          case RDG_BLOB:
            img= _blobIcon;
            break;
          case RDG_GEO:
            img= _spatialIcon;
            break;
          case RDG_USER:
            img= _userdefIcon;
            break;
          case RDG_STRUCTURED:
            img= _userdefIcon;
            break;
        }
      }      
      [aCell setImage:img];
    }
    else if ([[aTableColumn identifier] isEqualTo:@"default"])
    {
      if ((int)column["defaultValueIsNull"])
        [aCell setImage:_nullIcon];
      else
        [aCell setImage:nil];
    }
  }
}


- (id)tableView:(NSTableView *)aTableView 
objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(int)rowIndex
{
  if (aTableView == columnList)
  {
    MGRTValue column(_tableData->getColumn(rowIndex));
    
    if ([[aTableColumn identifier] isEqualTo:@"name"])
      return NSStr((const char*)column["name"]);
    else if ([[aTableColumn identifier] isEqualTo:@"type"])
      return NSStr(_tableData->formatColumnType(column).c_str());
    else if ([[aTableColumn identifier] isEqualTo:@"nnull"])
    {
      if (_tableData->columnIsPK(column))
        return [NSNumber numberWithInt:NSOnState];
      else
        return [NSNumber numberWithInt:COLUMN_NOT_NULL(column)?NSOnState:NSOffState];
    }
    else if ([[aTableColumn identifier] isEqualTo:@"autoinc"])
    {
      if (_tableData->columnIsNumeric(column))
        return [NSNumber numberWithInt:column.get("autoIncrement",0)?NSOnState:NSOffState];
      else
        return [NSNumber numberWithInt:NSOffState];
    }
    else if ([[aTableColumn identifier] isEqualTo:@"flags"])
    {
      return NSStr(_tableData->getEnabledColumnFlags(column).c_str());
    }
    else if ([[aTableColumn identifier] isEqualTo:@"default"])
    {
      if ((int)column["defaultValueIsNull"])
        return @"";
      else
        return NSStr((const char*)column["defaultValue"]);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"comments"])
      return NSStr((const char*)column["comment"]);
  }
  else if (aTableView == indexList)
  {
    MGRTValue index(_tableData->getIndex(rowIndex));
    
    if ([[aTableColumn identifier] isEqualTo:@"name"])
    {
      return NSStr(index["name"].asString());
    }
    else if ([[aTableColumn identifier] isEqualTo:@"type"])
    {
      int i= [[aTableColumn dataCell] indexOfItemWithTitle:NSStr(index["indexType"].asString())];
      return NSInt(i < 0 ? 0 : i);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"comment"])
    {
      return NSStr(index["comment"].asString());
    }
    return nil;
  }
  else if (aTableView == indColumnList)
  {
    int i= [indexList selectedRow];
    if (i < 0)
      return nil;
    MGRTValue index(_tableData->getIndex(i));
    
    if ([[aTableColumn identifier] isEqualTo:@"column"])
    {
      int i= [[aTableColumn dataCell] indexOfItemWithTitle:NSStr((const char*)index["columns"][rowIndex]["name"])];
      if (i == NSNotFound)
        return 0;
      return NSInt(i);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"order"])
    {
      return NSInt(index["columns"][rowIndex]["descend"].asInt()!=0);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"length"])
    {
      return NSInt(index["columns"][rowIndex]["columnLength"].asInt());
    }
    else if ([[aTableColumn identifier] isEqualTo:@"function"])
    {
      return NSStr(index["columns"][rowIndex]["storedFunction"].asString());
    }
    else if ([[aTableColumn identifier] isEqualTo:@"comment"])
    {
      return NSStr(index["columns"][rowIndex]["comment"].asString());
    }
  }
  else if (aTableView == fkList)
  {
    MGRTValue fk(_tableData->getFK(rowIndex));
    
    if ([[aTableColumn identifier] isEqualTo:@"name"])
    {
      return NSStr(fk["name"].asString());
    }
    else if ([[aTableColumn identifier] isEqualTo:@"ondelete"])
    {
      int i= [[aTableColumn dataCell] indexOfItemWithTitle:NSStr(fk["deleteRule"].asString())];
      return NSInt(i < 0 ? 0 : i);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"onupdate"])
    {
      int i= [[aTableColumn dataCell] indexOfItemWithTitle:NSStr(fk["updateRule"].asString())];
      return NSInt(i < 0 ? 0 : i);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"table"])
    {
      return NSInt([[aTableColumn dataCell] indexOfItemWithTitle:NSStr(fk["referedTableName"].asString())]);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"comment"])
    {
      return NSStr(fk["comment"].asString());
    }
      
    return nil;
  }
  else if (aTableView == fkColumnList)
  {
    int index= [fkList selectedRow];
    if (index < 0)
      return nil;
    MGRTValue fk(_tableData->getFK(index));
    
    if ([[aTableColumn identifier] isEqualTo:@"column"])
    {
      int i= [[aTableColumn dataCell] indexOfItemWithTitle:NSStr(_tableData->getFKColumn(fk, rowIndex))];
      if (i < 0)
        return nil;
      return [NSNumber numberWithInt:i];
    }
    else
    {
      int i= [[aTableColumn dataCell] indexOfItemWithTitle:NSStr(_tableData->getFKRefColumn(fk, rowIndex))];
      if (i < 0)
        return nil;
      return [NSNumber numberWithInt:i];
    }
  }
  return @"";
}


- (void)tableView:(NSTableView *)aTableView 
   setObjectValue:(id)anObject
   forTableColumn:(NSTableColumn *)aTableColumn 
              row:(int)rowIndex
{
  // Column
  if (aTableView == columnList)
  {
    MGRTValue column(_tableData->getColumn(rowIndex));
        
    if ([[aTableColumn identifier] isEqualTo:@"name"])
    {
      _tableData->setColumnName(column, [anObject UTF8String]);
      [self columnListChanged];
    }
    else if ([[aTableColumn identifier] isEqualTo:@"type"])
    {
      NSEvent *event= [NSApp currentEvent];
      
      _tableData->setColumnType(column, [anObject UTF8String]);
      
      if ([event type] == NSKeyDown && [[event characters] characterAtIndex:0] == NSTabCharacter)
      {
        if (rowIndex == [aTableView numberOfRows]-1)
        {
          [self addColumn:nil];
        }
        [aTableView editColumn:1 row:rowIndex+1 withEvent:nil select:YES];
      }
    }
    else if ([[aTableColumn identifier] isEqualTo:@"nnull"])
    {
      column.set("isNullable", [anObject intValue] != NSOnState ? 1 : 0);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"autoinc"])
    {
      if (_tableData->columnIsNumeric(column))
        column.set("autoIncrement", [anObject intValue] == NSOnState ? 1 : 0);
    }  
    else if ([[aTableColumn identifier] isEqualTo:@"flags"])
    {
      _tableData->setEnabledColumnFlags(column, [anObject UTF8String]);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"default"])
    {
      column.set("defaultValue", [anObject UTF8String]);
      if ([colDefaultNull state] == NSOnState)
        [colDefaultNull performClick:nil];
      column.set("defaultValueIsNull", 0);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"comments"])
    {
      column.set("comment", [anObject UTF8String]);
    }

    [self showColumn:&column];
  }
  // FK List
  else if (aTableView == fkList)
  {
    MGRTValue fk(_tableData->getFK(rowIndex));
    
    if ([[aTableColumn identifier] isEqualTo:@"name"])
    {
      fk.set("name", [anObject UTF8String]);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"comment"])
    {
      fk.set("comment", [anObject UTF8String]);
    }
    else if ([anObject intValue] >= 0)
    {
      MGRTValue fk(_tableData->getFK(rowIndex));
      int idx= [anObject intValue];
      if ([[aTableColumn identifier] isEqualTo:@"ondelete"])
      {
        NSPopUpButton *cell= [aTableColumn dataCellForRow:rowIndex];
        fk.set("deleteRule", [[cell itemTitleAtIndex:idx] UTF8String]);
      }
      else if ([[aTableColumn identifier] isEqualTo:@"onupdate"])
      {
        NSPopUpButton *cell= [aTableColumn dataCellForRow:rowIndex];
        fk.set("updateRule", [[cell itemTitleAtIndex:idx] UTF8String]);
      }
      else if ([[aTableColumn identifier] isEqualTo:@"table"])
      {
        NSPopUpButton *cell= [aTableColumn dataCellForRow:rowIndex];
        NSString *table= [cell itemTitleAtIndex:idx];

        if (strcmp2([table UTF8String], (const char*)fk["referedTableName"])!=0)
        {
          int i;
          MGRTValue refTable(_tableData->ownerSchema()["tables"].listItemNamed([table UTF8String]));
          
          _tableData->setFKRefTable(fk, refTable);
          
          // setup the menu in the column dataCell
          cell= [[fkColumnList tableColumnWithIdentifier:@"fcolumn"] dataCell];
          
          // fetch the column names for the table   
          if (refTable.isValid())
          {
            [cell removeAllItems];
            for (i= 0; i < refTable["columns"].count(); i++)
            {
              const char *colname= (const char*)refTable["columns"][i]["name"];
              MGRTTable theTable([_grt grt], refTable.grtValue());
              [cell addItemWithTitle:NSStr(colname)];
              if (_tableData->columnCount() > 0)
              {
                if (theTable.columnIsPK(theTable.getColumn(colname)))
                {
                  // automatically add all PKs from the refered Table              
                  _tableData->addColumnToFK(fk, _tableData->getColumn(0), colname);
                }
              }
            }
          }
          else
            NSLog(@"Could not find table %@ in catalogs list", table);
                    
          [fkColumnList reloadData];
        }
      }
    }
  }
  // Index List
  else if (aTableView == indexList)
  {
    MGRTValue index(_tableData->getIndex(rowIndex));
    
    if ([[aTableColumn identifier] isEqualTo:@"name"])
    {
      index.set("name", [anObject UTF8String]);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"type"])
    {
      index.set("indexType", [[[[aTableColumn dataCell] itemAtIndex:[anObject intValue]] title] UTF8String]);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"comment"])
    {
      index.set("comment", [anObject UTF8String]);
    }
  }  
  // FK Columns
  else if (aTableView == fkColumnList)
  {
    int index= [fkList selectedRow];
    int cindex= [fkColumnList selectedRow];
    MGRTValue fk(_tableData->getFK(index));
    int i= [anObject intValue];
    NSString *name;

    if (i >= 0)
    {
      name= [[aTableColumn dataCell] itemTitleAtIndex:i];
      
      if ([[aTableColumn identifier] isEqualTo:@"column"])
        _tableData->setFKColumn(fk, cindex, [name UTF8String]);
      else
        _tableData->setFKRefColumn(fk, cindex, [name UTF8String]);
      [fkColumnList reloadData];
    }
  }
  // Index Columns
  else if (aTableView == indColumnList)
  {
    int i= [indexList selectedRow];
    int cindex= [indColumnList selectedRow];
    MGRTValue index(_tableData->getIndex(i));

    if ([[aTableColumn identifier] isEqualTo:@"column"])
    {
      int i= [anObject intValue];
      NSString *name= [[aTableColumn dataCell] itemTitleAtIndex:i];
      
      _tableData->setIndexColumn(index, cindex, [name UTF8String]);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"order"])
    {
      int i= [anObject intValue];
      
      _tableData->getIndexColumn(index, cindex).set("descend", i);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"length"])
    {
      _tableData->getIndexColumn(index, cindex).set("columnLength", [anObject intValue]);
    }    
    else if ([[aTableColumn identifier] isEqualTo:@"function"])
    {
      _tableData->getIndexColumn(index, cindex).set("storedFunction", [anObject UTF8String]);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"comment"])
    {
      _tableData->getIndexColumn(index, cindex).set("comment", [anObject UTF8String]);
    }
    [indColumnList reloadData];
  }
}

- (BOOL)tableView:(NSTableView *)tableView
        writeRows:(NSArray *)rows
     toPasteboard:(NSPasteboard *)pboard
{
  if (tableView == columnList)
  {
    _draggedRow= [[rows objectAtIndex:0] intValue];
    
    [pboard declareTypes:[NSArray arrayWithObject:MGRTTableRowPboardType] owner:nil];
    [pboard setData:[NSData data] forType:MGRTTableRowPboardType];
    
    return YES;
  }
  else
    return NO;
}

- (NSDragOperation)tableView:(NSTableView *)tableView
                validateDrop:(id <NSDraggingInfo>)info
                 proposedRow:(int)row
       proposedDropOperation:(NSTableViewDropOperation)operation
{
  NSDragOperation op;
  if (tableView == columnList)
  {
    if ([info draggingSource] == tableView && row != _draggedRow && row >= 0)
      op= NSDragOperationPrivate;
    else
      op= NSDragOperationNone;
  }
  else 
    op= NSDragOperationNone;
  return op;
}

- (BOOL)tableView:(NSTableView *)tableView 
       acceptDrop:(id <NSDraggingInfo>)info
              row:(int)row 
    dropOperation:(NSTableViewDropOperation)operation
{
  if (tableView == columnList)
  {
    if (row > _tableData->columnCount())
      row= _tableData->columnCount();
    
    _tableData->moveColumn(_draggedRow, row);
    
    [tableView reloadData];
    return YES;
  }
  else
    return NO;
}


- (void)tableViewSelectionDidChange:(NSNotification *)aNotification
{
  NSTableView *table= [aNotification object];
  int row= [table selectedRow];
  BOOL enabled= NO;

  if (table == columnList)
  {  
    if (row < 0)
    {
      [colGroup setEnabledRecursive:NO];
      [self showColumn:0];
    }
    else
    {
      MGRTValue column(_tableData->getColumn(row));
      [colGroup setEnabledRecursive:YES];
      [self showColumn:&column];
    }
  }
  else if (table == indexList)
  {
    [indexList reloadData];
    if (row < 0)
      enabled= NO;
    else
      enabled= YES;
    [[indColumnList enclosingScrollView] setEnabledRecursive:enabled];
    [indColumnList reloadData];
  }
  else if (table == fkList)
  {
    [fkList reloadData];
    if (row < 0)
      enabled= NO;
    else
      enabled= YES;
    [[fkColumnList enclosingScrollView] setEnabledRecursive:enabled];
    [fkColumnList reloadData];
  }  
}

- (void)showObject
{
  MGRTValue table(_tableData->value());
  
  [descrText setString:NSStr(_tableData->comment())];
  
  // Column Stuff
  [tableNameText setStringValue:NSStr(table["name"].asString())];
  [tableName2Text setStringValue:NSStr(table["name"].asString())];
  
  [self showColumn:0];
  [columnList deselectAll:nil];
  [colGroup setEnabledRecursive:NO];  
  [columnList reloadData];
  [self columnListChanged];
  
  // Index Stuff
  [indexList deselectAll:nil];
  [[indColumnList enclosingScrollView] setEnabledRecursive:NO];
  [indexList reloadData];
  
  // FK Stuff
  [fkList deselectAll:nil];
  [[fkColumnList enclosingScrollView] setEnabledRecursive:NO];
  [fkList reloadData];

  // Engine Stuff
  const char *engine= table["tableEngine"];
  [enginePop selectItemAtIndex:[[enginePop menu] indexOfItemWithRepresentedObject:NSStr(engine)]];
  [engine2Pop selectItemAtIndex:[[engine2Pop menu] indexOfItemWithRepresentedObject:NSStr(engine)]];
  
  [self refreshForTableType];
  
  // Table list
  MGRTValue schema(_tableData->ownerSchema());
  if (schema.isValid())
  {
    MGRTValue tables(schema["tables"]);
    NSPopUpButtonCell *cell= [[fkList tableColumnWithIdentifier:@"table"] dataCell];
    [cell removeAllItems];
    for (int i= 0; i < tables.count(); i++)
    {
      MGRTValue table(tables[i]);

      [cell addItemWithTitle:NSStr(table["name"].asString())];
    }
  }
  // Charset Stuff
  [self fillCollationPopUp:tblCollationPop];
  [tblCollationPop selectItemWithTitle:NSStr(table["defaultCollationName"].asString())];

  [self fillCharsetPopUp:tblCharset2Pop];
  [tblCollation2Pop removeAllItems];
  
  // Misc. Options
  [[miscOpView viewWithTag:1] selectItemAtIndex:index_for_value(table.get("packKeys", ""), pack_key_values)];
  [[miscOpView viewWithTag:2] setStringValue:NSStr(table.get("password", ""))];
  [[miscOpView viewWithTag:3] setStringValue:NSStr(table.get("nextAutoInc", ""))];
  [[miscOpView viewWithTag:4] setState:table.get("delayKeyWrite",0)?NSOnState:NSOffState];

  // Row Options
  [[rowOpView viewWithTag:1] selectItemAtIndex:index_for_value(table.get("rowFormat", ""), row_format_values)];
  [[rowOpView viewWithTag:2] setState:table.get("checksum",0)?NSOnState:NSOffState];
  [[rowOpView viewWithTag:3] setStringValue:NSStr(table.get("avgRowLength",""))];
  [[rowOpView viewWithTag:4] setStringValue:NSStr(table.get("minRows",""))];
  [[rowOpView viewWithTag:5] setStringValue:NSStr(table.get("maxRows",""))];

  // Storage Options
  [[stoOpView viewWithTag:1] setStringValue:NSStr(table.get("tableDataDir",""))];
  [[stoOpView viewWithTag:3] setStringValue:NSStr(table.get("tableIndexDir",""))];

  // Merge Options
  [[mergeOpView viewWithTag:1] setStringValue:NSStr(table.get("mergeUnion",""))];
  [[mergeOpView viewWithTag:2] selectItemAtIndex:index_for_value(table.get("mergeInsert",""), merge_insert_values)];

  // Table RAID stuff
  [[raidOpView viewWithTag:1] selectItemWithTitle:NSStr(table.get("raidType",""))];
  [[raidOpView viewWithTag:2] setStringValue:NSStr(table.get("raidChunks",""))];
  [[raidOpView viewWithTag:3] setStringValue:NSStr(table.get("raidChunkSize",""))];
  
  [self fillCollationPopUp:colCollationPop];
}


static bool streq(const char *a, const char *b)
{
  if ((!a || !*a) && (!b || !*b))
    return true;
  
  return strcmp(a, b)==0;
}

static void update_value(MGRTValue &table, const char *key, const char *value)
{
  if (!streq(table.get(key, ""), value))
    table.set(key, value);
}

static void update_value(MGRTValue &table, const char *key, int value)
{
  if (table.get(key, 0) != value)    table.set(key, value);
}

- (BOOL)commitChanges
{
  MGRTValue table(_tableData->value());
  static char *raid_type_values[]= {
    "", "striped"
  };

#define ASSIGNSTR(v,s) update_value(table, v, [s UTF8String])
#define OPTIONVALUE(v,s,options) update_value(table, v, options[[s indexOfSelectedItem]])

//  if (_embedded)
//    _tableData->setName([[tableNameText stringValue] UTF8String]);
//  else
//    _tableData->setName([[tableName2Text stringValue] UTF8String]);
  
  _tableData->setComment([[[descrText textStorage] string] UTF8String]);
  
  // Misc. Options
  OPTIONVALUE("packKeys", [miscOpView viewWithTag:1], pack_key_values);
  ASSIGNSTR("password", [[miscOpView viewWithTag:2] stringValue]);
  ASSIGNSTR("nextAutoInc", [[miscOpView viewWithTag:3] stringValue]);
  update_value(table, "delayKeyWrite", [[miscOpView viewWithTag:4] state] == NSOnState);
  
  // Row Options
  OPTIONVALUE("rowFormat", [rowOpView viewWithTag:1], row_format_values);
  update_value(table, "checksum", [[rowOpView viewWithTag:2] state] == NSOnState);
  ASSIGNSTR("avgRowLength", [[rowOpView viewWithTag:3] stringValue]);
  ASSIGNSTR("minRows", [[rowOpView viewWithTag:4] stringValue]);
  ASSIGNSTR("maxRows", [[rowOpView viewWithTag:5] stringValue]);
  
  // Storage Options
  ASSIGNSTR("tableDataDir", [[stoOpView viewWithTag:1] stringValue]);
  ASSIGNSTR("tableIndexDir", [[stoOpView viewWithTag:3] stringValue]);
  
  // Merge Options
  ASSIGNSTR("mergeUnion", [[mergeOpView viewWithTag:1] stringValue]);
  OPTIONVALUE("mergeInsert", [mergeOpView viewWithTag:2], merge_insert_values);
  
  // Table RAID stuff
  OPTIONVALUE("raidType", [raidOpView viewWithTag:1], raid_type_values);
  ASSIGNSTR("raidChunks", [[raidOpView viewWithTag:2] stringValue]);
  ASSIGNSTR("raidChunkSize", [[raidOpView viewWithTag:3] stringValue]);

  return YES;
}


- (void)refreshForTableType
{
  BOOL fkEnabled= NO;

  fkEnabled= strcasecmp(_tableData->value()["tableEngine"].asString(),
                        "InnoDB")==0;
  
  [[fkList enclosingScrollView] setEnabledRecursive:fkEnabled];
  [[fkColumnList enclosingScrollView] setEnabledRecursive:fkEnabled && [fkList selectedRow]>0];
}

@end

//===

@implementation MGRTTableEditor


- (IBAction)handleValueChange:(id)sender
{
  if (sender == tblCollationPop || sender == tblCollation2Pop)
  {
    const char *collation= [[sender titleOfSelectedItem] UTF8String];
    
    _tableData->setDefaultCollation(collation);
  }
  else if (sender == tblCharset2Pop)
  {
    [self fillCollationPopUp:tblCollation2Pop forCharset:[[sender selectedItem] representedObject]];
  }
  else if (sender == enginePop)
  {
    NSString *engine= [[enginePop selectedItem] representedObject];

    _tableData->value().set("tableEngine", [engine UTF8String]);
    [self refreshForTableType];
  }
  else if (sender == engine2Pop || sender == storEnginePop)
  {
    MGRTValue engine((MYX_GRT_VALUE*)[[[storEnginePop selectedItem] representedObject] pointerValue]);

    [storEngineText setStringValue:NSStr(engine["description"].asString())];
    if (_tableData)
    {
      _tableData->value().set("tableEngine", engine["name"].asString());
      [self refreshForTableType];
    }
  }  
}

- (IBAction)handleColumnValueChange:(id)sender
{
  int column= [columnList selectedRow];

  if (sender == colCollationPop)
  {
    const char *collation= [[sender titleOfSelectedItem] UTF8String];
    
    _tableData->getColumn(column).set("collationName", collation);
  }
  else if (sender == colDefaultNull)
  {
    _tableData->getColumn(column).set("defaultValueIsNull", [sender state] == NSOnState);
    if ([sender state] == NSOnState)
    {
      [colDefaultText setEnabled:NO];
    }
    else
    {
      _tableData->getColumn(column).set("defaultValue", "");
      [colDefaultText setEnabled:YES];
    }
    [columnList reloadData];
  }
  else if (sender == colTypeCombo)
  {
    MGRTValue column(_tableData->getColumn([columnList selectedRow]));
    _tableData->setColumnType(column, [[colTypeCombo stringValue] UTF8String]);
    
    [columnList reloadData];
  }  
  else if (sender == colFlagsMatrix)
  {
    MGRTValue gcolumn(_tableData->getColumn(column));
    
    _tableData->setColumnPK(gcolumn, [[colFlagsMatrix cellAtRow:0 column:0] state] == NSOnState);

    if (_tableData->columnIsPK(gcolumn))
    {
      gcolumn.set("isNullable", 0);
      [[colFlagsMatrix cellAtRow:1 column:0] setState:NSOnState];
      [[colFlagsMatrix cellAtRow:1 column:0] setEnabled:NO];
    }
    else
    {
      gcolumn.set("isNullable", [[colFlagsMatrix cellAtRow:1 column:0] state] != NSOnState ? 1 : 0);
      [[colFlagsMatrix cellAtRow:1 column:0] setEnabled:YES];
    }
    
    if (!_tableData->columnIsNumeric(gcolumn))
      gcolumn.set("autoIncrement", 0);
    else
      gcolumn.set("autoIncrement", [[colFlagsMatrix cellAtRow:2 column:0] state] == NSOnState ? 1 : 0);
    
    
    MGRTValue flags(_tableData->getColumnFlags(gcolumn));
    for (int i= 0; i < flags.count(); i++)
    {
      NSButtonCell *cell;
      
      cell= [colFlagsMatrix cellAtRow:i+3 column:0];
      
      _tableData->setColumnFlagState(gcolumn, flags[i].asString(), [cell state] == NSOnState);
    }
    
    [columnList reloadData];
  }
}


- (IBAction)toggleColumnPK:(id)sender
{
  MGRTValue column(_tableData->getColumn([sender clickedRow]));
  _tableData->setColumnPK(column, !_tableData->columnIsPK(column));
  
  [columnList reloadData];
}


- (IBAction)chooseFile:(id)sender
{
  NSOpenPanel *panel= [NSOpenPanel openPanel];
  
  [panel setCanChooseFiles:NO];
  [panel setCanChooseDirectories:YES];
  [panel setTitle:@"Choose Directory"];
  if ([panel runModalForDirectory:nil file:nil types:nil]==NSOKButton)
  {
    NSString *path= [panel directory];
    
    [[[sender superview] viewWithTag:([sender tag] - 1)] setStringValue:path];
  }
}


- (id)init
{
  self= [super initWithWindowNibName: @"GRTTableEditor"];
  if (self)
  {    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(handleTextChanged:)
                                                 name:NSControlTextDidChangeNotification
                                               object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(handleTextChanged:)
                                                 name:NSTextDidChangeNotification
                                               object:nil];    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(handleTextFinished:)
                                                 name:NSControlTextDidEndEditingNotification
                                               object:nil];    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(addColumnWhenIdle:)
                                                 name:@"TableEditorAddColumn"
                                               object:self];
  }
  return self;
}



- (void)dealloc
{  
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  
  delete _tableData;
  
  [_pkIcon release];
  [_columnIcon release];
  [_blobIcon release];
  [_dateIcon release];
  [_numericIcon release];
  [_spatialIcon release];
  [_stringIcon release];
  [_userdefIcon release];
  
  [super dealloc];
}


- (void)setupButtonsForTableView:(NSTableView*)tview
                       addAction:(SEL)addSel
                    deleteAction:(SEL)delSel
{
  NSButton *button;
  MAccessoryScrollView *sv= (MAccessoryScrollView*)[tview enclosingScrollView];
  float size;

//  size= [NSScroller scrollerWidthForControlSize:NSMiniControlSize];
  size= 16;

  button= [[[NSButton alloc] initWithFrame:NSMakeRect(0,0,size,size)] autorelease];
  [button setButtonType: NSMomentaryChangeButton];
  [button setBordered:NO];
  [[button cell] setImagePosition:NSImageOnly];
  [button setImage: MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"mini_add.png")];
  [button setAlternateImage: MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"mini_add_pressed.png")];

  [button setTarget:self];
  [button setAction:addSel];
  [sv addSubview:button];
  [sv addHorizontalAccessoryView:button];
  
  button= [[[NSButton alloc] initWithFrame:NSMakeRect(0,0,size,size)] autorelease];
  [button setButtonType: NSMomentaryChangeButton];
  [button setBordered:NO];
  [[button cell] setImagePosition:NSImageOnly];
  [button setImage: MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"mini_del.png")];
  [button setAlternateImage: MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"mini_del_pressed.png")];
  [button setTarget:self];
  [button setAction:delSel];
  [sv addSubview:button];
  [sv addHorizontalAccessoryView:button];
}


- (void)awakeFromNib
{
  NSBundle *bundle= [NSBundle bundleForClass:[self class]];
  
  [tabView removeTabViewItem:[tabView tabViewItemAtIndex:5]];//XXX
  
  [self setupButtonsForTableView:columnList 
                       addAction:@selector(addColumn:)
                    deleteAction:@selector(deleteColumn:)];
  [self setupButtonsForTableView:fkList
                       addAction:@selector(addFK:)
                    deleteAction:@selector(deleteFK:)];
  [self setupButtonsForTableView:fkColumnList
                       addAction:@selector(addFKColumn:)
                    deleteAction:@selector(deleteFKColumn:)];
  [self setupButtonsForTableView:indexList
                       addAction:@selector(addIndex:)
                    deleteAction:@selector(deleteIndex:)];
  [self setupButtonsForTableView:indColumnList
                       addAction:@selector(addIndexColumn:)
                    deleteAction:@selector(deleteIndexColumn:)];

  [[columnList tableColumnWithIdentifier:@"nnull"] setHeaderCell:
    [[NSTableHeaderCell alloc] initImageCell:MXGetImageFromBundle(bundle,@"editor_table_not_null.png")]];
  [[columnList tableColumnWithIdentifier:@"autoinc"] setHeaderCell:
    [[NSTableHeaderCell alloc] initImageCell:MXGetImageFromBundle(bundle,@"editor_table_auto_inc.png")]];

  _pkIcon= [MXGetImageFromBundle(bundle,@"column_pk.png") retain];
  _columnIcon= [MXGetImageFromBundle(bundle,@"column.png") retain];
  _blobIcon= [MXGetImageFromBundle(bundle,@"datatype_blob.png") retain];
  _dateIcon= [MXGetImageFromBundle(bundle,@"datatype_datetime.png") retain];
  _numericIcon= [MXGetImageFromBundle(bundle,@"datatype_numeric.png") retain];
  _spatialIcon= [MXGetImageFromBundle(bundle,@"datatype_spatial.png") retain];
  _stringIcon= [MXGetImageFromBundle(bundle,@"datatype_string.png") retain];
  _userdefIcon= [MXGetImageFromBundle(bundle,@"datatype_userdefined.png") retain];
  
  _nullIcon= [MXGetImageFromBundle(bundle,@"field_overlay_null.png") retain];
  
  [columnList registerForDraggedTypes:[NSArray arrayWithObject:MGRTTableRowPboardType]];
  
  
  _engines= new MGRTValue(MGRTValue::fromFile([_grt grt], 
                                              [[[NSBundle bundleForClass:[MGRTTableEditor class]] pathForResource:@"storage_engines"
                                                                                                          ofType:@"xml"] UTF8String]));
  
  [storEngineText setCell:[[[MTextFieldCell alloc] initCopyingCell:[storEngineText cell]] autorelease]];
  
  [storEnginePop removeAllItems];
  for (unsigned int i= 0; i < _engines->count(); i++)
  {
    MGRTValue engine((*_engines)[i]);
    if (*engine["name"].asString())
    {
      [storEnginePop addItemWithTitle:NSStr(engine["caption"].asString())];
      [[storEnginePop lastItem] setRepresentedObject:[NSValue valueWithPointer:engine.grtValue()]];
    }
  }
  [self handleValueChange:storEnginePop];
  
  [enginePop removeAllItems];
  [enginePop addItemWithTitle:@"Transactional (InnoDB)"];
  [[enginePop lastItem] setRepresentedObject:@"InnoDB"];
  [enginePop addItemWithTitle:@"Non transactional (MyISAM)"];
  [[enginePop lastItem] setRepresentedObject:@"MyISAM"];
  [enginePop addItemWithTitle:@"Memory only (Memory)"];
  [[enginePop lastItem] setRepresentedObject:@"Memory"];
  [enginePop addItemWithTitle:@"Merge (Merge)"];
  [[enginePop lastItem] setRepresentedObject:@"Merge"];
  [enginePop addItemWithTitle:@"Cluster (NDB)"];
  [[enginePop lastItem] setRepresentedObject:@"NDB"];
  [enginePop addItemWithTitle:@"Berkeley DB (BDB)"];
  [[enginePop lastItem] setRepresentedObject:@"BDB"];
  
  [engine2Pop setMenu:[[storEnginePop menu] copy]];
  
  NSPopUpButtonCell *pcell;
  
  pcell= [[indexList tableColumnWithIdentifier:@"type"] dataCell];
  [pcell removeAllItems];
  [pcell addItemWithTitle:@"INDEX"];
  [pcell addItemWithTitle:@"PRIMARY"];
  [pcell addItemWithTitle:@"UNIQUE"];
  [pcell addItemWithTitle:@"FULLTEXT"];
  [pcell addItemWithTitle:@"SPATIAL"];

  pcell= [[indColumnList tableColumnWithIdentifier:@"order"] dataCell];
  [pcell removeAllItems];
  [pcell addItemWithTitle:@"ASC"];
  [pcell addItemWithTitle:@"DESC"];
  
  pcell= [[fkList tableColumnWithIdentifier:@"ondelete"] dataCell];
  [pcell removeAllItems];
  [pcell addItemWithTitle:@"RESTRICT"];
  [pcell addItemWithTitle:@"CASCADE"];
  [pcell addItemWithTitle:@"SET NULL"];
  [pcell addItemWithTitle:@"NO ACTION"];

  pcell= [[fkList tableColumnWithIdentifier:@"onupdate"] dataCell];
  [pcell removeAllItems];
  [pcell addItemWithTitle:@"RESTRICT"];
  [pcell addItemWithTitle:@"CASCADE"];
  [pcell addItemWithTitle:@"SET NULL"];
  [pcell addItemWithTitle:@"NO ACTION"];
  
  
  _tableTab= [tabView tabViewItemAtIndex:0];
  [_tableTab retain];
  [tabView removeTabViewItem:_tableTab];
}


- (BOOL)commit
{
  if ([self commitChanges])
  {
    _tableData->commit();
    return YES;
  }
  return NO;
}


- (void)revert
{
  _tableData->revert();
}


- (id)contentView
{
  return tabView;
}

- (void)makeEmbedable
{
  _embedded= YES;
  [tabView insertTabViewItem:_tableTab atIndex:0];
}


- (MYX_GRT_VALUE*)editedObject
{
  return _tableData->value().grtValue();
}


- (void)editObject:(MYX_GRT_VALUE*)value
{
  int i, c;
  
  [self toggleAdvanced:nil];
  

  _tableData= new MGRTTable([_grt grt], value);
  
  // column types
  MGRTValue simpleTypes(MGRTValue::fromGlobal([_grt grt], "/rdbmsMgmt/rdbms/Mysql/simpleDatatypes"));
  
  [colTypeCombo removeAllItems];
  c= simpleTypes.count();
  for (i= 0; i < c; i++)
  {
    NSString *tmp;
    if (simpleTypes[i]["numericPrecision"].asInt() > 0)
    {
      tmp= [NSString stringWithFormat:@"%s(11)", simpleTypes[i]["name"].asString()];
    }
    else if (simpleTypes[i]["characterMaximumLength"].asInt() > 0)
    {
      tmp= [NSString stringWithFormat:@"%s(40)", simpleTypes[i]["name"].asString()];
    }
    else
      tmp= NSStr(simpleTypes[i]["name"].asString());
    [colTypeCombo addItemWithObjectValue:tmp];
  }
    
  [self showObject];
}


- (IBAction)toggleAdvanced:(id)sender
{
  NSRect frame= [[columnList enclosingScrollView] frame];
  if ([advancedButton tag] == 0) // switch to adv
  {
    [advancedButton setTag:1];
    [advancedButton setTitle:@"Basic <<"];
    [colGroup setHidden:NO];
    frame.origin.y= 154;
    frame.size.height= NSHeight([[[columnList enclosingScrollView] superview] frame])-165;
    [[columnList enclosingScrollView] setFrame:frame];
  }
  else
  {
    [advancedButton setTag:0];
    [advancedButton setTitle:@"Advanced >>"];
    [colGroup setHidden:YES];
    frame.origin.y= 13;
    frame.size.height= NSHeight([[[columnList enclosingScrollView] superview] frame])-24;
    [[columnList enclosingScrollView] setFrame:frame];
  }
  [[[columnList enclosingScrollView] superview] setNeedsDisplay:YES];
}


- (void)tabView:(NSTabView *)aTabView didSelectTabViewItem:(NSTabViewItem *)tabViewItem
{
  if ([aTabView indexOfTabViewItem:tabViewItem] == 0)
    [advancedButton setHidden:NO];
  else
    [advancedButton setHidden:YES];
}


@end

