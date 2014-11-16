/* MTableEditor */

#import <Cocoa/Cocoa.h>
#include "myx_public_interface.h"


extern NSString *MTableEditorTableDidChange;

@class MAccessoryScrollView;

@interface MTableEditor : NSWindowController
{
    IBOutlet NSPopUpButton *colCharsetPop;
    IBOutlet NSPopUpButton *colCollationPop;
    IBOutlet NSTextView *colCommentText;
    IBOutlet NSTextField *colDefaultText;
    IBOutlet NSButton *colDefaultNull;
    IBOutlet NSTableView *colFlagsList;
    IBOutlet NSView *colGroup;
    IBOutlet NSTextField *colNameText;
    IBOutlet NSButtonCell *colAIncCheck;
    IBOutlet NSButtonCell *colUniqueCheck;
    IBOutlet NSButtonCell *colNNullCheck;
    IBOutlet NSButtonCell *colPKCheck;
    IBOutlet NSComboBox *colTypeCombo;
    IBOutlet NSTableView *columnList;
    IBOutlet NSTextField *commentText;
    IBOutlet NSPopUpButton *enginePop;
    IBOutlet NSTextField *engineText;
    IBOutlet NSView *fkContainer;
    IBOutlet NSBox *fkBox;
    IBOutlet NSTableView *fkColumnList;
    IBOutlet NSTableView *fkList;
    IBOutlet NSTextField *fkNameText;
    IBOutlet NSPopUpButton *fkOnDeletePop;
    IBOutlet NSPopUpButton *fkOnUpdatePop;
    IBOutlet NSPopUpButton *fkRefTablePop;
    IBOutlet NSBox *indBox;
    IBOutlet NSTableView *indColumnList;
    IBOutlet NSTableView *indexList;
    IBOutlet NSPopUpButton *indKindPop;
    IBOutlet NSTextField *indNameText;
    IBOutlet NSPopUpButton *indTypePop;
    IBOutlet NSPopUpButton *schemaPop;
    IBOutlet NSTextField *tableNameText;
    IBOutlet NSPopUpButton *tblCharsetPop;
    IBOutlet NSPopUpButton *tblCollationPop;
    IBOutlet NSView *miscOpView;
    IBOutlet NSView *rowOpView;
    IBOutlet NSView *stoOpView;
    IBOutlet NSView *mergeOpView;
    IBOutlet NSView *raidOpView;
    IBOutlet NSView *federOpView;
    
    IBOutlet NSPanel *confirmPanel;
    IBOutlet NSTextView *confirmText;
    
    MYSQL *_mysql;
    MYX_CATALOGS *_catalogs;
    NSString *_tableName;
    NSString *_schemaName;
    NSString *_catalogName;
    
    NSImage *_pkIcon;
    NSImage *_columnIcon;
    NSImage *_blobIcon;
    NSImage *_dateIcon;
    NSImage *_numericIcon;
    NSImage *_spatialIcon;
    NSImage *_stringIcon;
    NSImage *_userdefIcon;
  
    NSImage *_nullIcon;
    
    MYX_DBM_DATATYPES *_data_types;
    MYX_DBM_CHARSETS *_charsets;
    MYX_DBM_TABLE_DATA *_table;
    MYX_ENGINES *_engines;
    
    int _draggedRow;
    
    BOOL _releaseOnClose;
    MYX_LIB_ERROR _execute_statement_error;    
}

- (IBAction)applyChanges:(id)sender;
- (IBAction)close:(id)sender;
- (IBAction)discardChanges:(id)sender;
- (IBAction)chooseFile:(id)sender;
- (IBAction)handleValueChange:(id)sender;
- (IBAction)handleColumnValueChange:(id)sender;
- (IBAction)handleIndexValueChange:(id)sender;
- (IBAction)handleFKValueChange:(id)sender;
- (IBAction)confirmSave:(id)sender;
- (IBAction)confirmCancel:(id)sender;
- (IBAction)confirmOK:(id)sender;

- (BOOL)setConnection:(MYSQL*)mysql;
- (void)setCatalogs:(MYX_CATALOGS*)catalogs;

- (void)setReleaseOnClose:(BOOL)flag;

- (void)showNewTableForCatalog:(NSString*)catalog
                        schema:(NSString*)schema;

- (void)showTable:(NSString*)table
          catalog:(NSString*)catalog
           schema:(NSString*)schema;

- (NSString*)catalog;
- (NSString*)schema;
- (NSString*)table;

- (NSString*)confirmScript:(NSString*)script;

- (int)executeStatment: (const char *)sql;

@end
