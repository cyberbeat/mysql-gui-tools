/* MACatalogs */

#import <Cocoa/Cocoa.h>
#import "MAPanel.h"

@class MFilteredSchemaDataSource;

@interface MACatalogs : MAPanel
{
  IBOutlet NSTabView *tabView;
  IBOutlet NSScrollView *tableScrollView;
  IBOutlet NSTableView *tablesTable;
  IBOutlet NSButton *tableDetailToggleButton;
  IBOutlet NSMatrix *tableStatsMatrix;
  IBOutlet NSTabView *tableDetailsTab;
  IBOutlet NSTextField *tableStatusText;
  IBOutlet NSTextField *tableStatsDataText;
  IBOutlet NSTextField *tableStatsTimeText;
  IBOutlet NSPopUpButton *actionButton;
  
  IBOutlet NSTextField *emptyText;
  
  IBOutlet NSButton *addSchemaButton;
  IBOutlet NSButton *delSchemaButton;

  IBOutlet NSOutlineView *indexOutline;

  IBOutlet NSSearchField *schemataSearch;
  IBOutlet NSOutlineView *schemataOutline;
  
  IBOutlet NSTableView *viewTable;
  
  IBOutlet NSTableView *spTable;
  IBOutlet NSTableView *funcTable;
  
  NSMutableArray *_editors;
  
  NSImage *_tableIcon;
  NSImage *_indexIcon;
  NSImage *_columnIcon;
  NSImage *_viewIcon;
  NSImage *_procIcon;
  NSImage *_funcIcon;
  
  MFilteredSchemaDataSource *_schemaDS;
  
  MYX_SCHEMA_ENTITY_STATUS *_objectStatus;
  int *_tableIndices;
  int *_viewIndices;
  int *_procIndices;
  int *_funcIndices;
  unsigned int _tableCount;
  unsigned int _viewCount;
  unsigned int _funcCount;
  unsigned int _procCount;
}

- (IBAction)toggleTableDetails:(id)sender;
- (IBAction)checkTable:(id)sender;
- (IBAction)repairTable:(id)sender;
- (IBAction)optimizeTable:(id)sender;
- (IBAction)createTable:(id)sender;
- (IBAction)editTable:(id)sender;
- (IBAction)dropTable:(id)sender;
- (IBAction)refreshSchemas:(id)sender;
- (IBAction)refreshContents:(id)sender;
- (IBAction)searchCatalogs:(id)sender;

- (IBAction)createView:(id)sender;
- (IBAction)editView:(id)sender;
- (IBAction)dropView:(id)sender;

- (IBAction)createProcedure:(id)sender;
- (IBAction)editProcedure:(id)sender;
- (IBAction)dropProcedure:(id)sender;

- (IBAction)addSchema:(id)sender;
- (IBAction)delSchema:(id)sender;

@end
