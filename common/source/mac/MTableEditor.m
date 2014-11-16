#import "MTableEditor.h"
#import "MAccessoryScrollView.h"
#import "NSView_extras.h"
#import "mxUtils.h"
#import "myxutil.h"
#import "myx_sql_parser_public_interface.h"

#define COLUMN_TYPE(c) ((c).datatype_pointer ?  (c).datatype_pointer->group : 0)

/*
static struct EngineOptions {
  const char *label;
  const char *name;
  MYX_DBM_TABLE_ENGINE type;
  const char *description;
} EngineOptionNameList[] = {
  {"MyISAM", "MyISAM", MYX_DBM_TE_MYISAM,
    "Very fast, disk based storage engine without support for transactions.\n"
    "Offers fulltext search, packed keys, and is the default storage engine."},
  {"InnoDB", "InnoDB", MYX_DBM_TE_INNODB,
    "Transaction safe, disk based storage engine with row locking.\n"
    "Recommended engine for tables that need support for transactions."},
  {"Falcon", "jstar", MYX_DBM_TE_FALCON,
    "Transactional engine for modern scale-out applications.\n"
    "Supports MVCC (multiversion concurrency control)."},
  {"NDB", "NDB", MYX_DBM_TE_NDB,
    "MySQL Cluster storage engine. Transaction safe, memory based with row locking.\n"
    "Recommended storage engine for high performance, real time critical applications."},
  {"Archive", "ARCHIVE", MYX_DBM_TE_ARCHIVE,
    "Archive engine storing data in a compressed form to save storage space.\n"
    "Note that SELECTs require a full table scan."},
  {"CSV", "CSV", MYX_DBM_TE_CSV,
    "Engine that stores data as a comma separated values list, suited for data export.\n"
    "Note that this engine does not support indices."},
  {"BLACKHOLE", "BLACKHOLE", MYX_DBM_TE_BLACKHOLE,
    "The BLACKHOLE storage engine acts as a \"black hole\" that accepts data but\n"
    "throws it away and does not store it."},
  {"BerkeleyDB", "BerkeleyDB", MYX_DBM_TE_BDB,
    "Transaction safe storage engine with page locking.  Also called BDB."},
  {"Federated", "Federated", MYX_DBM_TE_FEDERATED,
    "The FEDERATED storage engine accesses data in tables of remote databases rather\n"
    "than in local tables."},
  {"MEMORY (HEAP)", "MEMORY", MYX_DBM_TE_MEMORY,
    "Extremly fast memory based storage engine that uses hash indices.\n"
    "Recommended storage engine for temporary data that can be lost in case\n"
    "of a server shutdown."},
  {"MERGE", "MERGE", MYX_DBM_TE_MERGE,
    "Collection of MyISAM tables with identical column and index information.\n"
    "Recommended storage engine for log tables or tables with archived data.\n"
    "Note that the list of union tables and insert method has to be defined."},
  {"ISAM (deprecated)", "ISAM", MYX_DBM_TE_ISAM,
    "Deprecated. This storage engine was replaced by the MyISAM storage engine."},
};

static int indexOfEngine(MYX_DBM_TABLE_ENGINE engine)
{
  int i;
  for (i= 0; i < sizeof(EngineOptionNameList)/sizeof(struct EngineOptions); i++)
  {
    if (EngineOptionNameList[i].type == engine)
      return i;
  }
  return 0;
}
*/

NSString *MTableEditorTableDidChange= @"MTableEditorTableDidChange";
NSString *MTableRowPboardType= @"MTableRowPboardType";

@interface MTableEditor(Private)
- (void)addColumn:(id)sender;
- (void)deleteColumn:(id)sender;
- (void)showColumn:(MYX_DBM_COLUMN_DATA*)column;
- (void)addFK:(id)sender;
- (void)deleteFK:(id)sender;
- (void)showFK:(MYX_DBM_FK_DATA*)fk;
- (void)addFKColumn:(id)sender;
- (void)deleteFKColumn:(id)sender;
- (void)addIndex:(id)sender;
- (void)deleteIndex:(id)sender;
- (void)showIndex:(MYX_DBM_INDEX_DATA*)index;
- (void)addIndexColumn:(id)sender;
- (void)deleteIndexColumn:(id)sender;

- (void)fillCollationsFor:(const char*)charset
                    popup:(id)popup;
- (void)handleTextEdited:(NSNotification*)notif;

- (void)tableViewSelectionDidChange:(NSNotification *)aNotification;
- (BOOL)commitChanges;
- (void)revertChanges;
- (void)refreshForTableType;
- (void)columnListChanged;

- (int)indexOfEngine: (MYX_ENGINE *)engine;
@end

static MYX_SCHEMA_TABLE *find_table(MYX_CATALOGS *cats, const char *catalog, 
                                    const char *schema, const char *table)
{
  unsigned int c, s, t;
  if (!catalog)
    catalog= "def";
  
  for (c= 0; c < cats->catalogs_num; c++)
  {
    MYX_CATALOG *cat= cats->catalogs+c; 
    if (strcmp2(cat->catalog_name, catalog)==0)
    {
      for (s= 0; s < cat->schemata_num; s++)
      {
        MYX_SCHEMA *sch= cat->schemata+s;
        if (strcmp2(sch->schema_name, schema)==0)
        {
          NSCAssert(sch->schema_tables!=0, @"schema_tables is not initialized");
          for (t= 0; t < sch->schema_tables->schema_tables_num; t++)
          {
            MYX_SCHEMA_TABLE *tab= sch->schema_tables->schema_tables+t;
            if (strcmp2(tab->table_name, table)==0)
              return tab;
          }
          break;
        }
      }
      break;
    }
  }
  return NULL;
}

@implementation MTableEditor(Private)

- (int)indexOfEngine: (MYX_ENGINE *)engine
{
  int i;
  
  for(i= 0; i < _engines->engines_num; i++)
    if(strcmp(_engines->engines[i].name, engine->name) == 0)
      return i;
      
  return -1;
}

- (void)addColumn:(id)sender
{
  int i;
  i= _table->columns_num++;
  _table->columns= g_realloc(_table->columns, sizeof(MYX_DBM_COLUMN_DATA)*_table->columns_num);
  memset(&_table->columns[i], 0, sizeof(MYX_DBM_COLUMN_DATA));
  _table->columns[i].name= g_strdup("newcolumn");
  _table->columns[i].datatype_name= g_strdup("INT");
  _table->columns[i].datatype_pointer= myx_dbm_get_datatype(_data_types, &_table->columns[i].datatype_name);
  _table->columns[i].default_value= g_strdup("");
  _table->columns[i].default_value_is_null= 0;
  [columnList reloadData];
  [columnList selectRow:i byExtendingSelection:NO];
  [columnList editColumn:0 row:i withEvent:nil select:YES];

  [self columnListChanged];
}

- (void)deleteColumn:(id)sender
{
  int i= [columnList selectedRow];
  if (i>=0)
  {
    MYX_DBM_COLUMN_DATA *column= _table->columns+i;
    int j;
    
    g_free(column->name);
    g_free(column->original_name);
  
    g_free(column->datatype_name);
    g_free(column->datatype_params);
    for (j= 0; j < column->datatype_flags_num; j++)
      g_free(column->datatype_flags[j]);
    
    g_free(column->charset);
    g_free(column->collation);
    
    g_free(column->default_value);
    
    g_free(column->comment);
    
    memmove(_table->columns+i, _table->columns+i+1,
            sizeof(MYX_DBM_COLUMN_DATA)*(_table->columns_num-i-1));
    _table->columns_num--;

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
  for (i= 0; i < _table->columns_num; i++)
  {
    NSString *str= NSStr(_table->columns[i].name);
    [pcell addItemWithTitle:str];
    [pcell2 addItemWithTitle:str];
  }
}

- (void)addFK:(id)sender
{
  int i;
  i= _table->fks_num++;
  _table->fks= g_realloc(_table->fks, sizeof(MYX_DBM_FK_DATA)*_table->fks_num);
  memset(&_table->fks[i], 0, sizeof(MYX_DBM_FK_DATA));
  _table->fks[i].name= g_strdup("newfk");
  [fkList reloadData];
  [fkList selectRow:i byExtendingSelection:NO];  
}

- (void)deleteFK:(id)sender
{
  int i= [fkList selectedRow];
  if (i>=0)
  {
    MYX_DBM_FK_DATA *fk= _table->fks+i;
    int j;
    
    g_free(fk->name);
    g_free(fk->original_name);

    g_free(fk->reference_table_name);
    
    for (j= 0; j < fk->column_mapping_num; j++)
    {
      g_free(fk->column_mapping[j].name);
      g_free(fk->column_mapping[j].value);
    }
    g_free(fk->column_mapping);
    
    memmove(_table->fks+i, _table->fks+i+1,
            sizeof(MYX_DBM_FK_DATA)*(_table->fks_num-i-1));
    _table->fks_num--;
    
    [fkList reloadData];
  }
}

- (void)addFKColumn:(id)sender
{
  int index= [fkList selectedRow];
  int column= [columnList selectedRow];
  MYX_DBM_FK_DATA *fdata;
  char *colname;
  int i;

  if (index < 0)
    return;

  if (column < 0)
    colname= "";
  else
    colname= _table->columns[column].name;

  fdata= _table->fks+index;

  i= fdata->column_mapping_num++;
  fdata->column_mapping= g_realloc(fdata->column_mapping, sizeof(MYX_NAME_VALUE_PAIR)*fdata->column_mapping_num);
  memset(&fdata->column_mapping[i], 0, sizeof(MYX_NAME_VALUE_PAIR));
  fdata->column_mapping[i].name= g_strdup(colname);
  fdata->column_mapping[i].value= 0;
  [fkColumnList reloadData];
  [fkColumnList selectRow:i byExtendingSelection:NO];  
  [fkColumnList editColumn:0 row:i withEvent:nil select:YES];  
}

- (void)deleteFKColumn:(id)sender
{
  int fk= [fkList selectedRow];
  int fcolumn= [fkColumnList selectedRow];
  MYX_DBM_FK_DATA *fdata;

  if (fk < 0 || fcolumn < 0)
    return;
  
  fdata= _table->fks+fk;
  
  g_free(fdata->column_mapping[fcolumn].name);
  g_free(fdata->column_mapping[fcolumn].value);
  memmove(fdata->column_mapping+fcolumn, fdata->column_mapping+fcolumn+1,
          sizeof(MYX_NAME_VALUE_PAIR)*(fdata->column_mapping_num-fcolumn-1));
  fdata->column_mapping_num--;
  
  [fkColumnList reloadData];
}


- (void)addIndex:(id)sender
{
  int i;
  i= _table->indices_num++;
  _table->indices= g_realloc(_table->indices, sizeof(MYX_DBM_INDEX_DATA)*_table->indices_num);
  memset(&_table->indices[i], 0, sizeof(MYX_DBM_INDEX_DATA));
  _table->indices[i].name= g_strdup("newindex");
  [indexList reloadData];
  [indexList selectRow:i byExtendingSelection:NO];
}


- (void)deleteIndex:(id)sender
{
  int i= [indexList selectedRow];
  if (i>=0)
  {
    MYX_DBM_INDEX_DATA *index= _table->indices+i;
    int j;

    g_free(index->name);
    g_free(index->original_name);

    for (j= 0; j < index->columns_num; j++)
    {
      g_free(index->columns[j].name);
      g_free(index->columns[j].len);
      g_free(index->columns[j].value_order);
    }
    g_free(index->columns);

    memmove(_table->indices+i, _table->indices+i+1,
            sizeof(MYX_DBM_INDEX_DATA)*(_table->indices_num-i-1));
    _table->indices_num--;

    [indexList reloadData];
  }
}


static int addColumnToIndex(MYX_DBM_INDEX_DATA *idata, const char *colname)
{
  int i;
  
  // check first if the column already exists
  for (i= 0; i < idata->columns_num; i++)
  {
    if (strcmp2(idata->columns[i].name, colname)==0)
      return i;
  }
  
  i= idata->columns_num++;
  idata->columns= g_realloc(idata->columns, sizeof(MYX_DBM_INDEX_COLUMN_DATA)*idata->columns_num);
  memset(&idata->columns[i], 0, sizeof(MYX_DBM_INDEX_COLUMN_DATA));
  idata->columns[i].name= g_strdup(colname);  
  
  return idata->columns_num-1;
}


static void removeColumnFromIndex(MYX_DBM_INDEX_DATA *idata, const char *colname)
{
  int i;
  
  for (i= 0; i < idata->columns_num; i++)
  {
    if (strcmp2(idata->columns[i].name, colname)==0)
      break;
  }
  
  if (idata->columns_num == i)
    return;
  
  g_free(idata->columns[i].name);
  g_free(idata->columns[i].len);
  g_free(idata->columns[i].value_order);
  memmove(idata->columns+i, idata->columns+i+1,
          sizeof(MYX_DBM_INDEX_COLUMN_DATA)*(idata->columns_num-i-1));
  idata->columns_num--;
}


- (void)addIndexColumn:(id)sender
{
  int index= [indexList selectedRow];
  int column= [columnList selectedRow];
  MYX_DBM_INDEX_DATA *idata;
  char *colname;
  int i;
  
  if (index < 0)
    return;
  
  idata= _table->indices+index;
  
  if (column < 0)
    colname= "";
  else
  {
    colname= _table->columns[column].name;
    
    // if this is the PRIMARY index, then set the column's PRIMARY flag
    if (strcmp2(idata->name, "PRIMARY")==0)
    {
      _table->columns[column].primary_key= 1;
      
      [self showColumn:_table->columns+column];
    }
  }

  i= addColumnToIndex(idata, colname);
  
  [indColumnList reloadData];
  [indColumnList selectRow:i byExtendingSelection:NO];  
  [indColumnList editColumn:0 row:i withEvent:nil select:YES];
}

- (void)deleteIndexColumn:(id)sender
{
  int index= [indexList selectedRow];
  int icolumn= [indColumnList selectedRow];
  MYX_DBM_INDEX_DATA *idata;

  if (index < 0 || icolumn < 0)
    return;

  idata= _table->indices+index;

  // if this is the PRIMARY index, then set the column's PRIMARY flag
  if (strcmp2(idata->name, "PRIMARY")==0)
  {
    int column;
    for (column= 0; column < _table->columns_num; column++)
    {
      if (strcmp2(_table->columns[column].name, idata->columns[icolumn].name)==0)
        break;
    }
    _table->columns[column].primary_key= 0;
    
    [self showColumn:_table->columns+column];
  }
  
  removeColumnFromIndex(idata, idata->columns[icolumn].name);

  [indColumnList reloadData];
}

- (void)handleTextEdited:(NSNotification*)notif
{
  id obj= [notif object];
  MYX_DBM_COLUMN_DATA *column= _table->columns+[columnList selectedRow];

  if (obj == colNameText)
  {
    NSString *str= [obj stringValue];
    g_free(column->name);
    column->name= g_strdup([str UTF8String]);
    
    [self columnListChanged];
  }
  else if (obj == colTypeCombo)
  {
    NSString *str= [obj stringValue];
    char *dt= g_strdup([str UTF8String]);
    MYX_DBM_DATATYPE *dtype= myx_dbm_get_datatype(_data_types, &dt);
    if (dtype)
    {
      column->datatype_pointer= dtype;
      column->datatype_name= myx_dbm_get_datatype_name(dt);
      column->datatype_params= myx_dbm_get_datatype_params(column->datatype_name, dt);
      
      [colAIncCheck setEnabled:COLUMN_TYPE(*column)==MYX_DBM_DTG_NUMERIC?YES:NO];
    }
    g_free(dt);
  }
  else if (obj == colDefaultText)
  {
    NSString *str= [obj stringValue];
    g_free(column->default_value);
    column->default_value= g_strdup([str UTF8String]);
  }
  else if (obj == colCommentText)
  {
    g_free(column->comment);
    column->comment= g_strdup([[obj string] UTF8String]);
  }
  else
    return;

  [columnList reloadData];
}

- (void)showColumn:(MYX_DBM_COLUMN_DATA*)column
{  
  if (column)
  {
    [colNameText setStringValue:NSStr(column->name)];
    
    [colTypeCombo setStringValue:[NSString stringWithFormat:@"%s%s",column->datatype_name,column->datatype_params?:""]];
    [colDefaultNull setState:column->default_value_is_null ? NSOnState : NSOffState];
    if (column->default_value_is_null)
    {
      [colDefaultText setStringValue:@""];
      [colDefaultText setEnabled:NO];
    }
    else
    {
      [colDefaultText setStringValue:NSStr(column->default_value)];
      [colDefaultText setEnabled:YES];
    }
    [colFlagsList reloadData];
        
    [colCharsetPop selectItemAtIndex:[[colCharsetPop menu] indexOfItemWithRepresentedObject:NSStr(column->charset)]];
    [self fillCollationsFor:column->charset popup:colCollationPop];
    [colCollationPop selectItemWithTitle:NSStr(column->collation)];
    
    [colCommentText setString:column->comment == nil ? @"" : NSStr(column->comment)];
    
    [colAIncCheck setState:column->auto_inc?NSOnState:NSOffState];
    [colNNullCheck setState:column->not_null?NSOnState:NSOffState];
    [colPKCheck setState:column->primary_key?NSOnState:NSOffState];
    
    [colNNullCheck setEnabled:column->primary_key?NO:YES];
    [colAIncCheck setEnabled:COLUMN_TYPE(*column)==MYX_DBM_DTG_NUMERIC?YES:NO];
  }
  else
  {
    [colNameText setStringValue:@""];
    
    [colTypeCombo setStringValue:@""];
    [colDefaultText setStringValue:@""];
    [colDefaultNull setState:NSOffState];
    [colFlagsList reloadData];
    
    [colCharsetPop selectItemWithTitle:@""];
    [colCollationPop selectItemWithTitle:@""];
    
    [colCommentText setString:@""];
    
    [colAIncCheck setState:NSOffState];
    [colNNullCheck setState:NSOffState];
    [colPKCheck setState:NSOffState];    
  }
}


- (void)showIndex:(MYX_DBM_INDEX_DATA*)index
{
  if (index)
  {      
    [indNameText setStringValue:NSStr(index->name)];
    [indKindPop selectItemAtIndex:index->index_kind];
    [indTypePop selectItemAtIndex:index->index_type];

    if (strcmp(index->name, "PRIMARY")==0)
      [indBox setEnabledRecursive:NO];
  }
  else
  {
    [indNameText setStringValue:@""];
    [indKindPop selectItemAtIndex:0];
    [indTypePop selectItemAtIndex:0];    
  }  
  [indColumnList reloadData];
}


- (void)showFK:(MYX_DBM_FK_DATA*)fk
{
  if (fk)
  {
    [fkNameText setStringValue:NSStr(fk->name)];
    [fkOnDeletePop selectItemAtIndex:fk->on_delete];
    [fkOnUpdatePop selectItemAtIndex:fk->on_update];
    [fkRefTablePop selectItemWithTitle:NSStr(fk->reference_table_name)];
    
    // setup the menu in the column dataCell
    id cell= [[fkColumnList tableColumnWithIdentifier:@"fcolumn"] dataCell];
    [cell removeAllItems];
    // fetch the column names for the table
    MYX_SCHEMA_TABLE *refTable= 
      find_table(_catalogs, [_catalogName UTF8String], [_schemaName UTF8String],
                 fk->reference_table_name);
    if (refTable)
    {
      unsigned int i;
      for (i= 0; i < refTable->columns_num; i++)
        [cell addItemWithTitle:NSStr(refTable->columns[i].column_name)];
    }
  }
  else
  {
    [fkNameText setStringValue:@""];
    [fkOnDeletePop selectItemAtIndex:0];
    [fkOnUpdatePop selectItemAtIndex:0];
    if ([fkRefTablePop numberOfItems]>0)
      [fkRefTablePop selectItemAtIndex:0];
  }  
  [fkColumnList reloadData];  
}


- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  if (aTableView == columnList)
    return _table ? _table->columns_num : 0;
  else if (aTableView == colFlagsList)
  {
    int r= [columnList selectedRow];
    if (!_table || r < 0 || !_table->columns || !_table->columns[r].datatype_pointer || (r >= _table->columns_num))
      return 0;
    return _table->columns[r].datatype_pointer->flags_num;
  }
  else if (aTableView == indexList)
  {
    if (_table)
      return _table->indices_num;
    else
      return 0;
  }
  else if (aTableView == indColumnList)
  {
    int index= [indexList selectedRow];
    if (index < 0)
      return 0;
    return _table->indices[index].columns_num;
  }
  else if (aTableView == fkList)
  {
    if (_table)
      return _table->fks_num;
    else
      return 0;
  }
  else if (aTableView == fkColumnList)
  {
    int index= [fkList selectedRow];
    if (index < 0 || !_table->fks)
      return 0;
    return _table->fks[index].column_mapping_num;
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
    if ([[aTableColumn identifier] isEqualTo:@"name"])
    {
      NSImage *img= _columnIcon;
      
      if (_table->columns[rowIndex].primary_key)
        img= _pkIcon;
      else if (_table->columns[rowIndex].datatype_pointer)
      {
        switch (_table->columns[rowIndex].datatype_pointer->group)
        {
          case MYX_DBM_DTG_NUMERIC:
            img= _numericIcon;
            break;
          case MYX_DBM_DTG_DATETIME:
            img= _dateIcon;
            break;
          case MYX_DBM_DTG_STRING:
            img= _stringIcon;
            break;
          case MYX_DBM_DTG_BLOB:
            img= _blobIcon;
            break;
          case MYX_DBM_DTG_SPATIAL:
            img= _spatialIcon;
            break;
          case MYX_DBM_DTG_USERDEFINED:
            img= _userdefIcon;
            break;
        }
      }      
      [aCell setImage:img];
    }
    else if ([[aTableColumn identifier] isEqualTo:@"default"])
    {
      if (_table->columns[rowIndex].default_value_is_null)
        [aCell setImage:_nullIcon];
      else
        [aCell setImage:nil];
    }
  }
  else if (aTableView == colFlagsList)
  {
    int r= [columnList selectedRow];
    if (rowIndex < _table->columns[r].datatype_pointer->flags_num)
      [aCell setTitle:[NSString stringWithUTF8String:_table->columns[r].datatype_pointer->flags[rowIndex]]];
  }
}


- (id)tableView:(NSTableView *)aTableView 
objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(int)rowIndex
{
  if (aTableView == columnList)
  {
    if ([[aTableColumn identifier] isEqualTo:@"name"])
      return NSStr(_table->columns[rowIndex].name);
    else if ([[aTableColumn identifier] isEqualTo:@"type"])
      return [NSString stringWithFormat:@"%s%s",_table->columns[rowIndex].datatype_name,_table->columns[rowIndex].datatype_params?:""];
    else if ([[aTableColumn identifier] isEqualTo:@"nnull"])
    {
      if (_table->columns[rowIndex].primary_key)
        return [NSNumber numberWithInt:NSOnState];
      else
        return [NSNumber numberWithInt:_table->columns[rowIndex].not_null?NSOnState:NSOffState];
    }
    else if ([[aTableColumn identifier] isEqualTo:@"autoinc"])
    {
      if (COLUMN_TYPE(_table->columns[rowIndex])==MYX_DBM_DTG_NUMERIC)
        return [NSNumber numberWithInt:_table->columns[rowIndex].auto_inc?NSOnState:NSOffState];
      else
        return [NSNumber numberWithInt:NSOffState];
    }
    else if ([[aTableColumn identifier] isEqualTo:@"flags"])
    {
      int i;
      NSMutableString *ms= [[[NSMutableString alloc] init] autorelease];
      for (i= 0; i < _table->columns[rowIndex].datatype_flags_num; i++)
      {
        if (i > 0)
          [ms appendFormat:@",%s",_table->columns[rowIndex].datatype_flags[i]];
        else
          [ms appendFormat:@"%s",_table->columns[rowIndex].datatype_flags[i]];
      }
      return ms;
    }
    else if ([[aTableColumn identifier] isEqualTo:@"default"])
    {
      if (_table->columns[rowIndex].default_value_is_null)
        return @"";
      else
        return NSStr(_table->columns[rowIndex].default_value);
    }
    else if ([[aTableColumn identifier] isEqualTo:@"comments"])
      return NSStr(_table->columns[rowIndex].comment);
  }
  else if (aTableView == colFlagsList)
  {
    int r= [columnList selectedRow];
    int i;
    int state;
    char *flag;
    if (r < 0) return @"";
    
    flag= _table->columns[r].datatype_pointer->flags[rowIndex];
    
    state= NSOffState;
    for (i= 0; i < _table->columns[r].datatype_flags_num; i++)
    {
      if (strcmp(_table->columns[r].datatype_flags[i],flag)==0)
      {
        state= NSOnState;
        break;
      }
    }
    return [NSNumber numberWithInt:state];
  }
  else if (aTableView == indexList)
  {
    return NSStr(_table->indices[rowIndex].name);
  }
  else if (aTableView == indColumnList)
  {
    int index= [indexList selectedRow];        
    if (index < 0)
      return nil;
    if ([[aTableColumn identifier] isEqualTo:@"column"])
    {
      int i= [[aTableColumn dataCell] indexOfItemWithTitle:NSStr(_table->indices[index].columns[rowIndex].name)];
      if (i == NSNotFound)
        return nil;
      return [NSNumber numberWithInt:i];
    }
    else
      return NSStr(_table->indices[index].columns[rowIndex].len);
  }
  else if (aTableView == fkList)
  {
    return NSStr(_table->fks[rowIndex].name);
  }
  else if (aTableView == fkColumnList)
  {
    int index= [fkList selectedRow];
    if (index < 0)
      return nil;
    if ([[aTableColumn identifier] isEqualTo:@"column"])
    {
      int i= [[aTableColumn dataCell] indexOfItemWithTitle:NSStr(_table->fks[index].column_mapping[rowIndex].name)];
      if (i < 0)
        return nil;
      return [NSNumber numberWithInt:i];
    }
    else
    {
      int i= [[aTableColumn dataCell] indexOfItemWithTitle:NSStr(_table->fks[index].column_mapping[rowIndex].value)];
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
    if ([[aTableColumn identifier] isEqualTo:@"name"])
    {
      g_free(_table->columns[rowIndex].name);
      _table->columns[rowIndex].name= g_strdup([anObject UTF8String]);
      [self columnListChanged];
    }
    else if ([[aTableColumn identifier] isEqualTo:@"type"])
    {
      char *dt= g_strdup([anObject UTF8String]);
      MYX_DBM_DATATYPE *dtype= myx_dbm_get_datatype(_data_types, &dt);
      if (dtype)
      {
        _table->columns[rowIndex].datatype_pointer= dtype;
        _table->columns[rowIndex].datatype_name= myx_dbm_get_datatype_name(dt);
        _table->columns[rowIndex].datatype_params= myx_dbm_get_datatype_params(_table->columns[rowIndex].datatype_name,
                                                                               (char*)[anObject UTF8String]);
      }
      g_free(dt);
      [colFlagsList reloadData];
    }
    else if ([[aTableColumn identifier] isEqualTo:@"nnull"])
    {
      _table->columns[rowIndex].not_null= [anObject intValue] == NSOnState ? 1 : 0;
    }
    else if ([[aTableColumn identifier] isEqualTo:@"autoinc"])
    {
      if (COLUMN_TYPE(_table->columns[rowIndex]) == MYX_DBM_DTG_NUMERIC)
        _table->columns[rowIndex].auto_inc= [anObject intValue] == NSOnState ? 1 : 0;
    }  
    else if ([[aTableColumn identifier] isEqualTo:@"flags"])
    {
      //XXX
    }
    else if ([[aTableColumn identifier] isEqualTo:@"default"])
    {
      g_free(_table->columns[rowIndex].default_value);
      _table->columns[rowIndex].default_value= g_strdup([anObject UTF8String]);
      if ([colDefaultNull state] == NSOnState)
        [colDefaultNull performClick:nil];
      _table->columns[rowIndex].default_value_is_null= 0;
    }
    else if ([[aTableColumn identifier] isEqualTo:@"comments"])
    {
      g_free(_table->columns[rowIndex].comment);
      _table->columns[rowIndex].comment= g_strdup([anObject UTF8String]);
    }

    [self showColumn:_table->columns+rowIndex];
  }
  // Column Flags
  else if (aTableView == colFlagsList)
  {
    int r= [columnList selectedRow];
    char *flag= _table->columns[r].datatype_pointer->flags[rowIndex];
    int index= -1;
    int i;
    for (i= 0; i < _table->columns[r].datatype_flags_num; i++)
    {
      if (strcmp(_table->columns[r].datatype_flags[i], flag)==0)
      {
        index= i;
        break;
      }
    }
    if (index < 0 && [anObject intValue]==NSOnState)
    {
      _table->columns[r].datatype_flags_num++;
      _table->columns[r].datatype_flags= g_realloc(_table->columns[r].datatype_flags, 
                                          sizeof(char*)*(_table->columns[r].datatype_flags_num+1));
      _table->columns[r].datatype_flags[_table->columns[r].datatype_flags_num-1]= g_strdup(flag);
      _table->columns[r].datatype_flags[_table->columns[r].datatype_flags_num]= NULL;
    }
    else if (index >= 0 && [anObject intValue]==NSOffState)
    {
      memmove(_table->columns[r].datatype_flags+index,
              _table->columns[r].datatype_flags+index+1,
              (_table->columns[r].datatype_flags_num-index-1)*sizeof(char*));
      _table->columns[r].datatype_flags_num--;
      if (_table->columns[r].datatype_flags_num == 0)
      {
        g_free(_table->columns[r].datatype_flags);
        _table->columns[r].datatype_flags= NULL;
      }
    }
    [columnList reloadData];
  }
  // FK Columns
  else if (aTableView == fkColumnList)
  {
    int index= [fkList selectedRow];
    int cindex= [fkColumnList selectedRow];
    MYX_NAME_VALUE_PAIR *cdata= _table->fks[index].column_mapping+cindex;
    int i= [anObject intValue];
    NSString *name;
    
    if (i >= 0)
    {
      name= [[aTableColumn dataCell] itemTitleAtIndex:i];
      
      if ([[aTableColumn identifier] isEqualTo:@"column"])
      {
        g_free(cdata->name);
        cdata->name= g_strdup([name UTF8String]);
      }
      else
      {
        g_free(cdata->value);
        cdata->value= g_strdup([name UTF8String]);
      }
      [fkColumnList reloadData];
    }
  }  
  // Index Columns
  else if (aTableView == indColumnList)
  {
    int index= [indexList selectedRow];
    int cindex= [indColumnList selectedRow];
    MYX_DBM_INDEX_COLUMN_DATA *cdata= _table->indices[index].columns+cindex;

    if ([[aTableColumn identifier] isEqualTo:@"column"])
    {
      int i= [anObject intValue];
      NSString *name= [[aTableColumn dataCell] itemTitleAtIndex:i];

      g_free(cdata->name);
      cdata->name= g_strdup([name UTF8String]);
    }
    else
    {
      g_free(cdata->len);
      if ([anObject intValue]==0)
        cdata->len= g_strdup("");
      else
        cdata->len= g_strdup([anObject UTF8String]);
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
    
    [pboard declareTypes:[NSArray arrayWithObject:MTableRowPboardType] owner:nil];
    [pboard setData:[NSData data] forType:MTableRowPboardType];
    
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
  else if (tableView == fkColumnList && [fkColumnList isEnabled] && [fkList selectedRow]>=0)
    op= NSDragOperationPrivate;
  else if (tableView == indColumnList && [indColumnList isEnabled] && [indexList selectedRow]>=0)
    op= NSDragOperationPrivate;
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
    MYX_DBM_COLUMN_DATA column;
    
    if (row >= _table->columns_num)
      row= _table->columns_num-1;
    
    memcpy(&column, _table->columns+_draggedRow, sizeof(column));
    
    if (_draggedRow < row)
    {
      // I1
      // I2 ---
      // I3   |
      // I4 <--
      // I5
      memmove(_table->columns+_draggedRow, _table->columns+_draggedRow+1,
              (row-_draggedRow)*sizeof(column));
      memcpy(_table->columns+row, &column, sizeof(column));
    }
    else
    {
      // I1
      // I2 <--
      // I3   |
      // I4 ---
      // I5    
      memmove(_table->columns+row+1, _table->columns+row,
              (_draggedRow-row)*sizeof(column));
      memcpy(_table->columns+row, &column, sizeof(column));    
    }
    [tableView reloadData];
    return YES;
  }
  else if (tableView == fkColumnList)
  {
    int index= [fkList selectedRow];
    MYX_NAME_VALUE_PAIR *fdata;
    int findex;
    
    [self addFKColumn:nil];
    findex= [fkColumnList selectedRow];
    fdata= _table->fks[index].column_mapping+findex;
    
    g_free(fdata->name);
    fdata->name= g_strdup(_table->columns[_draggedRow].name);
    fdata->value= NULL;
    
    [fkColumnList reloadData];
    return YES;    
  }
  else if (tableView == indColumnList)
  {
    int cindex;
    int index= [indexList selectedRow];
    MYX_DBM_INDEX_COLUMN_DATA *cdata;
    
    [self addIndexColumn:nil];
    cindex= [indColumnList selectedRow];
    cdata= _table->indices[index].columns+cindex;
    
    g_free(cdata->name);
    cdata->name= g_strdup(_table->columns[_draggedRow].name);
    
    [fkColumnList reloadData];
    return YES;
  }
  else
    return NO;
}

- (void)tableViewSelectionDidChange:(NSNotification *)aNotification
{
  NSTableView *table= [aNotification object];
  int row= [table selectedRow];

  if (table == columnList)
  {  
    if (row < 0)
    {
      [colGroup setEnabledRecursive:NO];
      [self showColumn:0];
    }
    else
    {
      [colGroup setEnabledRecursive:YES];
      [self showColumn:_table->columns+row];
    }
  }
  else if (table == indexList)
  {
    if (row < 0)
    {
      [indBox setEnabledRecursive:NO];
      [self showIndex:0];
    }
    else
    {
      [indBox setEnabledRecursive:YES];
      [self showIndex:_table->indices+row];
    }
  }
  else if (table == fkList)
  {
    if (row < 0)
    {
      [fkBox setEnabledRecursive:NO];
      [self showFK:0];
    }
    else
    {
      [fkBox setEnabledRecursive:YES];
      [self showFK:_table->fks+row];
    }
  }  
}


- (void)fillCollationsFor:(const char*)charset
                    popup:(id)popup
{
  int i;
  [popup removeAllItems];
  for (i= 0; i < _charsets->charsets_num; i++)
  {
    MYX_DBM_CHARSET *cs= _charsets->charsets+i;
    int j;
    
    if (strcmp2(cs->name, charset) == 0)
    {
      for (j= 0; j < cs->collations_num; j++)
      {
        [popup addItemWithTitle:NSStr(cs->collations[j].name)];
        if (cs->collations[j].is_default)
          [popup selectItem:[popup lastItem]];
      }
    }
  }  
}

- (void)revertChanges
{
  MYX_LIB_ERROR error;

  [columnList deselectAll: self];

  if (_table)
  {
    myx_dbm_free_table_data(_table);
    _table= NULL;
  }

  if (_tableName)
  {
    _table= myx_dbm_retrieve_table_data(_mysql, _data_types, _engines, [_catalogName UTF8String],
                                        [_schemaName UTF8String], [_tableName UTF8String],
                                        &error);
    if (!_table)
    {
      if (error == MYX_SQL_ERROR)
      {
        MXRunAlertPanelWithMySQLError(@"Error", 
                                      [NSString stringWithFormat:@"Could not retrieve data for table '%@'.", _tableName],
                                      _mysql);
      }
      else
      {
        MXRunAlertPanelWithError(@"Error", 
                                 [NSString stringWithFormat:@"Could not retrieve data for table '%@'.", _tableName],
                                 error);
      }

      return;
    }
  }
  else
  {
    _table= g_new0(MYX_DBM_TABLE_DATA, 1);

    _table->schema= g_strdup([_schemaName UTF8String]);
    _table->charset= g_strdup("utf8");
    
    //_table->table_engine= myx_dbm_get_default_storage_engine(_mysql);
  }

  // Column Stuff
  [tableNameText setStringValue:_tableName ? : @"new_table"];
  [commentText setStringValue:NSStr(_table->comment)];
  
  [self showColumn:0];
  [columnList deselectAll: self];
  [colGroup setEnabledRecursive:NO];  
  [columnList reloadData];
  [self columnListChanged];
  
  // Index Stuff
  [self showIndex:0];
  [indexList deselectAll:nil];
  [indBox setEnabledRecursive:NO];
  [indexList reloadData];
  
  // FK Stuff
  [self showFK:0];
  [fkList deselectAll:nil];
  [fkBox setEnabledRecursive:NO];
  [fkList reloadData];
  
  // Engine Stuff
  if((_table != NULL) && (_table->storage_engine != NULL))
  {
    [enginePop selectItemAtIndex: [self indexOfEngine: _table->storage_engine]];
    [self refreshForTableType];
  }
  
  // Charset Stuff
  [tblCharsetPop selectItemAtIndex:[[tblCharsetPop menu] indexOfItemWithRepresentedObject:NSStr(_table->charset)]];
  [self fillCollationsFor:_table->charset popup:tblCollationPop];
  [tblCollationPop selectItemWithTitle:NSStr(_table->collation)];
  
  // Misc. Options
  [[miscOpView viewWithTag:1] selectItemAtIndex:(int)_table->pack_keys];
  [[miscOpView viewWithTag:2] setStringValue:NSStr(_table->password)];
  [[miscOpView viewWithTag:3] setStringValue:NSStr(_table->next_auto_inc)];
  [[miscOpView viewWithTag:4] setState:_table->delay_key_write?NSOnState:NSOffState];

  // Row Options
  [[rowOpView viewWithTag:1] selectItemAtIndex:(int)_table->row_format];
  [[rowOpView viewWithTag:2] setState:_table->checksum?NSOnState:NSOffState];
  [[rowOpView viewWithTag:3] setStringValue:NSStr(_table->avg_row_length)];
  [[rowOpView viewWithTag:4] setStringValue:NSStr(_table->min_rows)];
  [[rowOpView viewWithTag:5] setStringValue:NSStr(_table->max_rows)];

  // Storage Options
  [[stoOpView viewWithTag:1] setStringValue:NSStr(_table->table_data_dir)];
  [[stoOpView viewWithTag:3] setStringValue:NSStr(_table->table_index_dir)];

  // Merge Options
  [[mergeOpView viewWithTag:1] setStringValue:NSStr(_table->merge_union)];
  [[mergeOpView viewWithTag:2] selectItemAtIndex:(int)_table->merge_insert];

  // Table RAID stuff
  [[raidOpView viewWithTag:1] selectItemAtIndex:(int)_table->raid_type];
  [[raidOpView viewWithTag:2] setStringValue:NSStr(_table->raid_chunks)];
  [[raidOpView viewWithTag:3] setStringValue:NSStr(_table->raid_chunk_size)];
  
  // Federated
  [[federOpView viewWithTag:1] setStringValue:NSStr(_table->federated_connection)];
}


- (BOOL)commitChanges
{
#define ASSIGNSTR(v,s) g_free(v), v= g_strdup([s UTF8String])
  ASSIGNSTR(_table->name, [tableNameText stringValue]);
  ASSIGNSTR(_table->comment, [commentText stringValue]);
  
  // Misc. Options
  _table->pack_keys= [[miscOpView viewWithTag:1] indexOfSelectedItem];
  ASSIGNSTR(_table->password, [[miscOpView viewWithTag:2] stringValue]);
  ASSIGNSTR(_table->next_auto_inc, [[miscOpView viewWithTag:3] stringValue]);
  _table->delay_key_write= [[miscOpView viewWithTag:4] state] == NSOnState;
  
  // Row Options
  _table->row_format= [[rowOpView viewWithTag:1] indexOfSelectedItem];
  _table->checksum= [[rowOpView viewWithTag:2] state] == NSOnState;
  ASSIGNSTR(_table->avg_row_length, [[rowOpView viewWithTag:3] stringValue]);
  ASSIGNSTR(_table->min_rows, [[rowOpView viewWithTag:4] stringValue]);
  ASSIGNSTR(_table->max_rows, [[rowOpView viewWithTag:5] stringValue]);
  
  // Storage Options
  ASSIGNSTR(_table->table_data_dir, [[stoOpView viewWithTag:1] stringValue]);
  ASSIGNSTR(_table->table_index_dir, [[stoOpView viewWithTag:3] stringValue]);
  
  // Merge Options
  ASSIGNSTR(_table->merge_union, [[mergeOpView viewWithTag:1] stringValue]);
  _table->merge_insert= [[mergeOpView viewWithTag:2] indexOfSelectedItem];
  
  // Table RAID stuff
  _table->raid_type= [[raidOpView viewWithTag:1] indexOfSelectedItem];
  ASSIGNSTR(_table->raid_chunks, [[raidOpView viewWithTag:2] stringValue]);
  ASSIGNSTR(_table->raid_chunk_size, [[raidOpView viewWithTag:3] stringValue]);
  
  // Federated stuff
  ASSIGNSTR(_table->federated_connection, [[federOpView viewWithTag:1] stringValue]);

  return YES;
}


- (void)refreshForTableType
{
  /*
  BOOL fkEnabled= NO;
  
  [engineText setStringValue:NSStr(EngineOptionNameList[[enginePop indexOfSelectedItem]].description)];

  switch (_table->table_engine)
  {
    case MYX_DBM_TE_MYISAM:
      break;
    case MYX_DBM_TE_INNODB:
      fkEnabled= YES;
      break;
    case MYX_DBM_TE_MEMORY:
      break;
    case MYX_DBM_TE_MERGE:
      break;
    case MYX_DBM_TE_BDB:
      break;
    case MYX_DBM_TE_ISAM:
      break;
    case MYX_DBM_TE_NDB:
      break;
    case MYX_DBM_TE_FALCON:
      break;
    case MYX_DBM_TE_FEDERATED:
      break;
    case MYX_DBM_TE_ARCHIVE:
      break;
    case MYX_DBM_TE_CSV:
      break;
    case MYX_DBM_TE_BLACKHOLE:
      break;
  }
  
  [fkContainer setEnabledRecursive:fkEnabled];
  if (fkEnabled)
    [fkBox setEnabledRecursive:NO];
  */
  [fkContainer setEnabledRecursive: YES];
  [fkBox setEnabledRecursive: NO];

}

@end

//===

int execute_sql_statement(const char *sql, void *userdata)
{
  return [((MTableEditor *)userdata) executeStatment: sql];
}

@implementation MTableEditor

- (int) executeStatment: (const char *)sql
{
  long long int affected_rows;
  self->_execute_statement_error= MYX_NO_ERROR;
  myx_query_execute_direct(_mysql, sql, &self->_execute_statement_error, &affected_rows);
  return self->_execute_statement_error;
}

- (IBAction)applyChanges:(id)sender
{
  if(_table->fks && ([fkList selectedRow] >= 0))
    [self handleFKValueChange: fkNameText];

  if ([self commitChanges])
  {    
    MYX_LIB_ERROR error;
    MYX_DBM_TABLE_DATA *original;
    char *script= NULL;
    
    // if _tableName == nil, means we're creating a table, not modifying
    if (!_tableName)
      original= myx_dbm_retrieve_table_data(_mysql, _data_types, _engines,
                                            [_catalogName UTF8String],
                                            [_schemaName UTF8String],
                                            _table->name, 
                                            &error);    
    else
      original= myx_dbm_retrieve_table_data(_mysql, _data_types, _engines,
                                            [_catalogName UTF8String],
                                            [_schemaName UTF8String],
                                            [_tableName UTF8String], 
                                            &error);
    if (!original)
    {
      if (_tableName)
      {
        if (error == MYX_SQL_ERROR)
          MXRunAlertPanelWithMySQLError(@"Error", 
                                        [NSString stringWithFormat:@"Could not retrieve information for table '%s.%s' to perform table diff.",
                                          [_schemaName UTF8String], [_tableName UTF8String]], _mysql);
        else
          MXRunAlertPanelWithError(@"Error",
                                   [NSString stringWithFormat:@"Could not retrieve information for table '%s.%s' to perform table diff.",
                                     [_schemaName UTF8String], [_tableName UTF8String]], error);
      }
    }
    
    if (original || !_tableName)
    {
      MYX_DBM_SERVER_VERSION server_version;
      
      server_version.major_version= myx_get_mysql_major_version(_mysql);
      server_version.minor_version= myx_get_mysql_minor_version(_mysql);
      
      script= myx_dbm_get_table_sql_diff(original, _table, &server_version, &error);
      if (original)
        myx_dbm_free_table_data(original);
      if (!script || !*script)
      {
        if (error != MYX_NO_ERROR)
          MXRunAlertPanelWithError(@"Error", 
                                   @"Could not generate differences between original table and modified table.",
                                   error);
        else
          MXRunAlertPanelWithError(@"Apply Changes",
                                   @"No modifications to be applied.",
                                   error);
      }
    }
    
    
    if (script && *script)
    {
      NSString *tmp= [NSString stringWithUTF8String:script];
      
      if ((tmp= [self confirmScript:tmp]))
      {
        MYX_LIB_ERROR error;
        NSString *msg;
        
        if (!_tableName)
          msg= @"Error executing SQL commands to create table.";
        else
          msg= @"Error executing SQL commands to update table.";
        
        
        //myx_query_execute_direct(_mysql, [tmp UTF8String], &error);
        myx_process_sql_statements([tmp UTF8String], execute_sql_statement, self, MYX_SPM_NORMAL_MODE);
        error= self->_execute_statement_error;        
        
        if (error == MYX_SQL_ERROR)
          MXRunAlertPanelWithMySQLError(@"Error", msg, _mysql);
        else if (error != MYX_NO_ERROR)
          MXRunAlertPanelWithError(@"Error", msg, error);
        else
        {
          if (!_tableName)
            NSRunAlertPanel(@"Create Table", @"Table '%@' Created.",
                            @"OK", nil, nil, [NSString stringWithUTF8String:_table->name]);
          else
            NSRunAlertPanel(@"Edit Table", @"Modifications to Table '%@' Were Saved.",
                            @"OK", nil, nil, [NSString stringWithUTF8String:_table->name]);
          
          _tableName= [NSString stringWithUTF8String:_table->name];

          // reload newly saved data
          [self revertChanges];
		  
          [[NSNotificationCenter defaultCenter] postNotificationName:MTableEditorTableDidChange
                                                              object:self];
        }
      }
    }
    g_free(script);
  }
}

- (IBAction)close:(id)sender
{
  [self close];
}

- (IBAction)discardChanges:(id)sender
{
  [self revertChanges];
}

- (IBAction)handleValueChange:(id)sender
{
  if (sender == tblCharsetPop)
  {
    const char *charset= [[[sender selectedItem] representedObject] UTF8String];
    
    [self fillCollationsFor:charset popup:tblCollationPop];
    
    g_free(_table->charset);
    _table->charset= g_strdup(charset);
  }  
  else if (sender == tblCollationPop)
  {
    const char *collation= [[sender titleOfSelectedItem] UTF8String];
    
    g_free(_table->collation);
    _table->collation= g_strdup(collation);
  }
  else if (sender == enginePop)
  {
    //_table->table_engine= [[[enginePop selectedItem] representedObject] intValue];
    _table->storage_engine= _engines->engines + [[[enginePop selectedItem] representedObject] intValue];
    [self refreshForTableType];
  }
}

char ** get_datatype_flags(MYX_DBM_DATATYPE *datatype, char *dtype, unsigned int *num);

- (IBAction)handleColumnValueChange:(id)sender
{
  int column= [columnList selectedRow];

  if (sender == colTypeCombo)
  {
    NSString *type= [colTypeCombo stringValue];

    if (g_strncasecmp(_table->columns[column].datatype_name,[type UTF8String],[type length])!=0)
    {
      char *dt= g_strdup([type UTF8String]);
      MYX_DBM_DATATYPE *dtype= myx_dbm_get_datatype(_data_types, &dt);
      if (dtype)
      {
        _table->columns[column].datatype_pointer= dtype;
        _table->columns[column].datatype_name= myx_dbm_get_datatype_name(dt);
        _table->columns[column].datatype_params= myx_dbm_get_datatype_params(_table->columns[column].datatype_name, dt);
        _table->columns[column].datatype_flags= get_datatype_flags(dtype, _table->columns[column].datatype_name, &_table->columns[column].datatype_flags_num);
        _table->columns[column].default_value_is_null= 0;
        _table->columns[column].default_value= g_strdup("");
        _table->columns[column].charset= 0;
        _table->columns[column].collation= 0;
        _table->columns[column].not_null= 0;
        _table->columns[column].auto_inc= 0;
      }
      g_free(dt);
      [colFlagsList reloadData];
    }
  }
  else if (sender == colCharsetPop)
  {
    const char *charset= [[[sender selectedItem] representedObject] UTF8String];
    
    [self fillCollationsFor:charset popup:colCollationPop];
    
    if (strcmp3(_table->columns[column].charset,charset)!=0)
    {
      g_free(_table->columns[column].charset);
      _table->columns[column].charset= g_strdup(charset);
    }
  }  
  else if (sender == colCollationPop)
  {
    const char *collation= [[sender titleOfSelectedItem] UTF8String];
    
    g_free(_table->columns[column].collation);
    _table->columns[column].collation= g_strdup(collation);    
  }
  else if (sender == colDefaultNull)
  {
    _table->columns[column].default_value_is_null= [sender state] == NSOnState;
    if (_table->columns[column].default_value_is_null)
    {
      [colDefaultText setEnabled:NO];
    }
    else
    {
      g_free(_table->columns[column].default_value);
      _table->columns[column].default_value= g_strdup("");
      [colDefaultText setEnabled:YES];
    }
    [columnList reloadData];
  }
  else if ([sender isMemberOfClass:[NSMatrix class]])
  {
    int i;
    MYX_DBM_INDEX_DATA *idata= NULL;
    
    _table->columns[column].primary_key= [colPKCheck state] == NSOnState ? 1 : 0;
    if (COLUMN_TYPE(_table->columns[column]) != MYX_DBM_DTG_NUMERIC)
      _table->columns[column].auto_inc= 0;
    else
      _table->columns[column].auto_inc= [colAIncCheck state] == NSOnState ? 1 : 0;

    // find the PRIMARY index
    for (i= 0; i < _table->indices_num; i++)
    {
      if (strcmp2(_table->indices[i].name, "PRIMARY")==0)
      {
        idata= _table->indices+i;
        break;
      }
    }
    
    // no PRIMARY index yet, create one
    if (!idata)
    {
      int i= _table->indices_num++;
      _table->indices= g_realloc(_table->indices, sizeof(MYX_DBM_INDEX_DATA)*_table->indices_num);
      memset(&_table->indices[i], 0, sizeof(MYX_DBM_INDEX_DATA));
      _table->indices[i].name= g_strdup("PRIMARY");
      _table->indices[i].index_kind= MYX_DBM_IK_PRIMARY;
      _table->indices[i].index_type= MYX_DBM_IT_DEFAULT;
      idata= _table->indices + i;
    }

    if (_table->columns[column].primary_key)
    {
      addColumnToIndex(idata, _table->columns[column].name);
      [indColumnList reloadData];
      
      [colNNullCheck setState:NSOnState];
      
      _table->columns[column].not_null= 1;
    }
    else
    {
      removeColumnFromIndex(idata, _table->columns[column].name);
      [indColumnList reloadData];
      
      _table->columns[column].not_null= [colNNullCheck state] == NSOnState ? 1 : 0;
    }
    [colNNullCheck setEnabled:_table->columns[column].primary_key?NO:YES];
    
    [columnList reloadData];
  }
}

- (IBAction)handleIndexValueChange:(id)sender
{
  int index= [indexList selectedRow];
  MYX_DBM_INDEX_DATA *idata= _table->indices+index;

  if (sender == indNameText)
  {
    g_free(idata->name);
    idata->name= g_strdup([[sender stringValue] UTF8String]);
    [indexList reloadData];
  }
  else if (sender == indKindPop)
  {
    idata->index_kind= [sender indexOfSelectedItem];
  }
  else if (sender == indTypePop)
  {
    idata->index_type= [sender indexOfSelectedItem];
  }
  else
    NSAssert(0, @"Invalid sender!");
}


- (IBAction)handleFKValueChange:(id)sender
{
  int fk= [fkList selectedRow];
  MYX_DBM_FK_DATA *fdata= _table->fks+fk;
  
  if (!fdata || fk < 0)
    return;
  
  if (sender == fkNameText)
  {
    g_free(fdata->name);
    fdata->name= g_strdup([[sender stringValue] UTF8String]);
    [fkList reloadData];
  }
  else if (sender == fkOnUpdatePop)
  {
    fdata->on_update= [sender indexOfSelectedItem];
  }
  else if (sender == fkOnDeletePop)
  {
    fdata->on_delete= [sender indexOfSelectedItem];
  }
  else if (sender == fkRefTablePop)
  {
    NSString *table= [sender titleOfSelectedItem];
    
    if (strcmp2([table UTF8String], fdata->reference_table_name)!=0)
    {
      int i;
      id cell;
      MYX_SCHEMA_TABLE *refTable;
      
      g_free(fdata->reference_table_name);
      fdata->reference_table_name= g_strdup([table UTF8String]);
      
      // reset mappings
      for (i= 0; i < fdata->column_mapping_num; i++)
      {
        g_free(fdata->column_mapping[i].value);
        fdata->column_mapping[i].value= NULL;
      }
      
      // setup the menu in the column dataCell
      cell= [[fkColumnList tableColumnWithIdentifier:@"fcolumn"] dataCell];
      
      // fetch the column names for the table
      refTable= find_table(_catalogs, [_catalogName UTF8String], [_schemaName UTF8String],
                        fdata->reference_table_name);
      if (refTable)
      {
        [cell removeAllItems];
        for (i= 0; i < refTable->columns_num; i++)
          [cell addItemWithTitle:NSStr(refTable->columns[i].column_name)];
      }
      else
        NSLog(@"Could not find table %s in catalogs list", fdata->reference_table_name);
    }
  }
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
  self= [super initWithWindowNibName: @"TableEditor"];
  if (self)
  {
    [self setShouldCascadeWindows:NO];
    [self loadWindow];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(handleTextEdited:)
                                                 name:NSControlTextDidChangeNotification
                                               object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(handleTextEdited:)
                                                 name:NSTextDidChangeNotification
                                               object:colCommentText];
    [[self window] setDelegate:self];
  }
  return self;
}


- (void)dealloc
{
  if (_data_types)
    myx_free_datatype(_data_types);
  if (_charsets)
    myx_free_charsets(_charsets);

  [[NSNotificationCenter defaultCenter] removeObserver:self];

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
  
  [self setWindowFrameAutosaveName:@"TableEditor"];
  [[self window] setFrameUsingName:@"TableEditor"];
  [[self window] setFrameAutosaveName:@"TableEditor"];
  
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
  
  [columnList registerForDraggedTypes:[NSArray arrayWithObject:MTableRowPboardType]];
  [fkColumnList registerForDraggedTypes:[NSArray arrayWithObject:MTableRowPboardType]];
  [indColumnList registerForDraggedTypes:[NSArray arrayWithObject:MTableRowPboardType]];  
  
  /*

  // currently the list is filled in -setConnection:

  [enginePop removeAllItems];
  for (i= 0; i < sizeof(EngineOptionNameList)/sizeof(struct EngineOptions); i++)
  {
    [enginePop addItemWithTitle:NSStr(EngineOptionNameList[i].label)];
    [[enginePop lastItem] setRepresentedObject:[NSNumber numberWithInt:EngineOptionNameList[i].type]];
  }
  [enginePop selectItemAtIndex:1];
  */
}


- (BOOL)setConnection:(MYSQL*)mysql
{
  int i;
  MYX_LIB_ERROR error;
  NSString *path;
  
  _mysql= mysql;
  path= [[NSBundle bundleForClass:[MTableEditor class]] pathForResource:@"mysqlx_dbm_datatypes" ofType:@"xml"];
  _data_types= myx_datatype_load([path cString], &error);
  
  if (mysql_version_is_later_or_equal_than(_mysql, 4,1))
    _charsets= myx_dbm_retrieve_charsets(_mysql, &error);
  else
  {
    path= [[NSBundle bundleForClass:[MTableEditor class]] pathForResource:@"mysqlx_dbm_charsets" ofType:@"xml"];
    _charsets= myx_charsets_load([path cString], &error);
  }
  
  if (!_charsets || !_data_types)
  {
    NSLog(@"Could not load data files for table editor");
    return NO;
  }
  
  // fill engines popup
  _engines= myx_get_engines(_mysql);

  if (!_engines)
  {
    NSLog(@"Could not load engines for table editor");
    return NO;
  }

  [enginePop removeAllItems];
  [enginePop setAutoenablesItems: NO];
  for(i= 0; i < _engines->engines_num; i++)
  {
    [enginePop addItemWithTitle:NSStr( _engines->engines[i].name)];
    [[enginePop lastItem] setRepresentedObject: [NSNumber numberWithInt: i]];
    [[enginePop lastItem] setEnabled: _engines->engines[i].enabled];
  }

  return YES;
}


- (void)setCatalogs:(MYX_CATALOGS*)catalogs
{
  _catalogs= catalogs;  
}


- (void)showTable:(NSString*)table
          catalog:(NSString*)catalog
           schema:(NSString*)schema
{
  int c, s;
  MYX_SCHEMA_TABLES *my_schema= 0;
  
  if (table)
    _tableName= [table retain];
  else
    _tableName= nil;
  _catalogName= [catalog retain];
  _schemaName= [schema retain];
  // set up the Schema selection popup button
  [schemaPop removeAllItems];
  
  for (c= 0; c < _catalogs->catalogs_num; c++)
  {
    MYX_CATALOG *cat= _catalogs->catalogs+c;
    //XXX if (strcmp(cat->catalog_name, [catalog UTFString])!=0)
    //XXX  continue;
    
    for (s= 0; s < cat->schemata_num; s++)
    {
      MYX_SCHEMA *sch= cat->schemata+s;

      [schemaPop addItemWithTitle:NSStr(sch->schema_name)];
      if (strcmp(sch->schema_name, [schema UTF8String])==0)
      {
        //if (!sch->schema_tables)
        {
          if (sch->schema_tables)
            myx_free_schema_tables(sch->schema_tables);

          // modifying _catalogs will break other places that keep ptrs to data in it
          sch->schema_tables= myx_get_schema_tables(_mysql, [catalog UTF8String], [schema UTF8String]);
        }
        my_schema= sch->schema_tables;
        [schemaPop selectItem:[schemaPop lastItem]];
      }
    }
  }

  // we dont allow it to be changed by now
  [schemaPop setEnabled:NO];

  // fill the table popup for FK setup 
  [fkRefTablePop removeAllItems];
  if (my_schema)
  {
    for (c= 0; c < my_schema->schema_tables_num; c++)
    {
      [fkRefTablePop addItemWithTitle:NSStr(my_schema->schema_tables[c].table_name)];
    }
  }

  // column types
  [colTypeCombo removeAllItems];
  for (c= 0; c < _data_types->datatypes_num; c++)
  {
    NSString *tmp;
    if (_data_types->datatypes[c].has_length_param || _data_types->datatypes[c].has_decimal_param)
      tmp= [NSString stringWithFormat: @"%s()", _data_types->datatypes[c].name];
    else
      tmp= NSStr(_data_types->datatypes[c].name);    
    [colTypeCombo addItemWithObjectValue:tmp];
  }

  // charset and collations
  [colCharsetPop removeAllItems];
  [tblCharsetPop removeAllItems];
  [colCharsetPop addItemWithTitle:@""];
  [[colCharsetPop lastItem] setRepresentedObject:@""];
  [tblCharsetPop addItemWithTitle:@""];
  for (c= 0; c < _charsets->charsets_num; c++)
  {
    [colCharsetPop addItemWithTitle:NSStr(_charsets->charsets[c].desc && *_charsets->charsets[c].desc?_charsets->charsets[c].desc:_charsets->charsets[c].name)];
    [[colCharsetPop lastItem] setRepresentedObject:NSStr(_charsets->charsets[c].name)];
    [tblCharsetPop addItemWithTitle:NSStr(_charsets->charsets[c].desc && *_charsets->charsets[c].desc?_charsets->charsets[c].desc:_charsets->charsets[c].name)];
    [[tblCharsetPop lastItem] setRepresentedObject:NSStr(_charsets->charsets[c].name)];
  }

  // display stuff
  [self revertChanges];
  
  [self showWindow:self];
}


- (void)showNewTableForCatalog:(NSString*)catalog
                        schema:(NSString*)schema
{
  [self showTable:nil catalog:catalog schema:schema];
}


- (NSString*)confirmScript:(NSString*)script
{
  int rc;
  NSString *final;
  
  [confirmText setString:script];
  
  [NSApp beginSheet:confirmPanel
     modalForWindow:[self window]
      modalDelegate:self
     didEndSelector:nil
        contextInfo:nil];

  rc= [NSApp runModalForWindow:confirmPanel];
  
  if (rc == 1)
    final= [[[confirmText string] retain] autorelease];
  else
    final= nil;
  
  [confirmPanel close];
  
  return final;
}


- (IBAction)confirmSave:(id)sender
{
  NSString *script= [confirmText string];
  NSSavePanel *panel= [NSSavePanel savePanel];
  
  if ([panel runModalForDirectory:nil 
                             file:[NSString stringWithFormat:@"%@.sql",[tableNameText stringValue]]]==NSOKButton)
  {
    if (![script writeToFile:[panel filename] 
                  atomically:NO])
    {
      NSRunAlertPanel(@"Error", @"Could not save script to file '%@'.", @"OK", nil, nil, 
                      [panel filename]);
    }
  }
}

- (IBAction)confirmCancel:(id)sender
{
  [NSApp endSheet:confirmPanel returnCode:0];
  [NSApp stopModalWithCode:0];
}

- (IBAction)confirmOK:(id)sender
{
  [NSApp endSheet:confirmPanel returnCode:1];
  [NSApp stopModalWithCode:1];
}

- (void)setReleaseOnClose:(BOOL)flag
{
  _releaseOnClose= flag;
}

- (void)windowWillClose:(NSNotification*)notification
{
  if (_releaseOnClose)
    [self autorelease];
}


- (NSString*)catalog
{
  return _catalogName;
}


- (NSString*)schema
{
  return _schemaName;
}


- (NSString*)table
{
  return _tableName;
}



@end
