/* MTableEditor */

#import <Cocoa/Cocoa.h>
#include <myx_public_interface.h>
#include <MySQLGRT/myx_grt_public_interface.h>
#include <MySQLGRT/MGRTTable.h>

#import <MySQLGRT/MGRTObjectEditor.h>
#import <MySQLGRT/MGRT.h>

@class MAccessoryScrollView;


@interface MGRTTableEditor : MGRTObjectEditor
{
  IBOutlet NSTextField *tableNameText;
  IBOutlet NSPopUpButton *enginePop;
  IBOutlet NSPopUpButton *tblCollationPop;

  IBOutlet NSTextField *tableName2Text;
  IBOutlet NSPopUpButton *engine2Pop;
  IBOutlet NSPopUpButton *tblCharset2Pop;
  IBOutlet NSPopUpButton *tblCollation2Pop;
  
  IBOutlet NSTabView *tabView;
  IBOutlet NSTableView *columnList;
  
  IBOutlet NSView *colGroup;
  IBOutlet NSTextField *colNameText;
  IBOutlet NSComboBox *colTypeCombo;
  IBOutlet NSPopUpButton *colCollationPop;
  IBOutlet NSTextField *colDefaultText;
  IBOutlet NSButton *colDefaultNull;
  IBOutlet NSTextView *colCommentText;
  IBOutlet NSMatrix *colFlagsMatrix;
  
  IBOutlet NSTableView *indexList;
  IBOutlet NSTableView *indColumnList;
  
  IBOutlet NSTableView *fkList;
  IBOutlet NSTableView *fkColumnList;
  
  IBOutlet NSTableView *stdInsertList;
  IBOutlet NSTextView *descrText;
  
  
  IBOutlet NSPopUpButton *storEnginePop;
  IBOutlet NSTextField *storEngineText;
  
  IBOutlet NSView *miscOpView;
  IBOutlet NSView *rowOpView;
  IBOutlet NSView *stoOpView;
  IBOutlet NSView *mergeOpView;
  IBOutlet NSView *raidOpView;
  
  IBOutlet NSButton *advancedButton;
  
  MYSQL *_mysql;
  MGRTTable *_tableData;
  
  NSImage *_pkIcon;
  NSImage *_columnIcon;
  NSImage *_blobIcon;
  NSImage *_dateIcon;
  NSImage *_numericIcon;
  NSImage *_spatialIcon;
  NSImage *_stringIcon;
  NSImage *_userdefIcon;
  
  NSImage *_nullIcon;

  MGRTValue *_engines;

  NSTabViewItem *_tableTab;
  
  int _draggedRow;
  
  BOOL _embedded;
}
- (IBAction)chooseFile:(id)sender;
- (IBAction)handleValueChange:(id)sender;
- (IBAction)handleColumnValueChange:(id)sender;
- (IBAction)toggleColumnPK:(id)sender;
- (IBAction)toggleAdvanced:(id)sender;


- (void)makeEmbedable;
- (void)editObject:(MYX_GRT_VALUE*)value;
- (id)contentView;

@end
