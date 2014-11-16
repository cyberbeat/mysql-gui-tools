/* MGRTConnectionPanel */

#import <Cocoa/Cocoa.h>

#import <MySQLGRT/MGRTValue.h>
#import <MySQLGRT/MGRT.h>

#include <myx_public_interface.h>


@class MGRTConnectionPanel;


@interface MGRTConnectionPanel : NSWindowController
{  
  @protected
    IBOutlet NSView *paramsView;
    IBOutlet NSView *rdbmsView;
    IBOutlet NSView *advParamsView;
    IBOutlet NSView *bottomView;
    
    IBOutlet NSButton *connectButton;
    IBOutlet NSButton *detailsButton;
    
    IBOutlet NSPanel *errorPanel;
    IBOutlet NSTextField *errorText;
    IBOutlet NSButton *pingButton;
    IBOutlet NSTextView *pingText;
    IBOutlet id pingTextContainer;
    IBOutlet NSProgressIndicator *progressIndicator;
    
    IBOutlet NSImageView *topImage;
    
    IBOutlet NSView *connectionsView;
    IBOutlet NSPopUpButton *connectionPop;
    IBOutlet NSButton *addButton;
    IBOutlet NSButton *removeButton;

    
    IBOutlet NSPopUpButton *rdbmsPop;
    IBOutlet NSPopUpButton *driverPop;
    IBOutlet NSTextField *driverLabel;
    IBOutlet NSTextField *driverDescLabel;
    
    int _lastSelectedConnection;
        
    NSMutableArray *_parameters;
    
    MGRT *_grt;
    
    id _firstControl;
    
    NSString *_connInfoPath;
    NSString *_connTargetPath;

    id _delegate;
    
    BOOL _connecting;
    BOOL _connectionFailed; // written by thread
    BOOL _showDescriptions;
    BOOL _jdbcOnly;
    BOOL _pickRdbms;
    BOOL _pickSchema;
    
    BOOL _settingConnValues;
}

- (IBAction)cancel:(id)sender;
- (IBAction)connect:(id)sender;
- (IBAction)connectionChanged:(id)sender;
- (IBAction)toggleDetails:(id)sender;
- (IBAction)rdbmsSelected:(id)sender;
- (IBAction)driverSelected:(id)sender;
- (IBAction)saveConnection:(id)sender;
- (IBAction)deleteConnection:(id)sender;

- (id)initWithMGRT:(MGRT*)grt
   connectionsPath:(NSString*)path
        targetPath:(NSString*)target;

- (MGRTValue)writeConnectionToTarget;

- (void)setSelectRdbms;
- (void)setEditsSchema;

- (void)setDelegate:(id)delegate;

- (void)setHeaderImage:(NSImage*)image;

- (void)refreshRdbmsInfo;


- (NSView*)paramsPanel;
- (NSView*)rdbmsPanel;
- (NSView*)advParamsPanel;

- (void)show;

@end




@interface NSObject(MGRTConnectionPanelDelegate)
- (void)connectionPanel:(MGRTConnectionPanel*)panel readinessChanged:(BOOL)flag;
@end


