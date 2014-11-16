/* MQResultSetView */

#import <Cocoa/Cocoa.h>

#include <myx_public_interface.h>

@class MQResultSetDataSource;
@class MQTableView;
@class MAccessoryScrollView;
@class MSplitView;
@class MConnectionInfo;

typedef enum {
  MQDNone,
  MQDVertical,
  MQDHorizontal
} MQRSSplitDirection;

@interface MQResultSetView : NSObject
{
  IBOutlet MAccessoryScrollView *sview;
  IBOutlet MQTableView *table;
  IBOutlet NSView *view;
  IBOutlet MSplitView *splitView;
  IBOutlet NSTextField *statusLabel;
  IBOutlet NSTableView *messageTable;

  NSString *_query;
  NSString *_defaultSchema;

  MConnectionInfo *_mysqlInfo;
  MYSQL *_mysql;
  id _delegate;
  MQResultSetDataSource *_dataSource;
  
  NSColor *_statusColors[5];
  NSColor *_diffColors[4];
  NSSize _editBarSizes[8];

  NSMutableArray *_viewers;
  MQRSSplitDirection _direction;
  id _messageDS;
  
  id _localParams;
  MQResultSetView *_masterView;

  BOOL _stopQuery;
  BOOL _closeable;
  BOOL _busy;
  BOOL _active;
  BOOL _compact;
  BOOL _resultDisplayed;
  BOOL _transactionOpen;
  
  NSString *_query_save_filename;
}

- (IBAction)addRow:(id)sender;
- (IBAction)deleteRow:(id)sender;
- (IBAction)copyField:(id)sender;
- (IBAction)saveField:(id)sender;
- (IBAction)loadField:(id)sender;
- (IBAction)clearField:(id)sender;
- (IBAction)editField:(id)sender;
- (IBAction)viewField:(id)sender;

- (IBAction)editRS:(id)sender;
- (IBAction)commitRS:(id)sender;
- (IBAction)discardRS:(id)sender;
- (IBAction)goFirst:(id)sender;
- (IBAction)goLast:(id)sender;

- (IBAction)copyMessage:(id)sender;
- (IBAction)clearMessages:(id)sender;

- (void)setActive:(BOOL)flag;
- (BOOL)active;

- (void)splitV:(id)sender;
- (void)splitH:(id)sender;
- (void)unsplit:(id)sender;

- (id)initWithConnectionTo:(MConnectionInfo*)info;

- (void)changeSplitDirection:(MQRSSplitDirection)newDir;
- (void)setDefaultSchema:(NSString*)schema;
- (MYSQL*)mysql;
- (NSString*)defaultCatalog;
- (NSString*)defaultSchema;

- (void)setCompact:(BOOL)flag;

- (MQTableView*)tableView;
- (id)view;

- (void)setDelegate:(id)delegate;

- (void)performQuery:(NSString*)query;
- (void)performQuery:(NSString*)query withParameters:(MYX_STRINGLIST*)parameters;
- (void)performApply;

- (MYX_RESULTSET*)resultset;

- (BOOL)transactionOpen;
- (void)startTransaction;
- (void)commitTransaction;
- (void)rollbackTransaction;

- (void)setMasterView:(MQResultSetView*)rsview;
- (MQResultSetView*)masterView;

- (void)setLocalParameters:(id)params;
- (id)localParameters;

- (void)setCloseable:(BOOL)flag;
- (BOOL)closeable;

- (void)stopQuery;
- (BOOL)isBusy;

- (void)setQuery:(NSString*)query;
- (NSString*)query;
- (void) saveQuery;

- (BOOL)hasChanges;

@end
