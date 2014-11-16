/* MConnectionPanel */

#import <Cocoa/Cocoa.h>

#import <MySQLToolsCommon/MConnectionInfo.h>

#include <myx_public_interface.h>


@class MConnectionPanel;

@protocol MConnectionPanelDelegate
- (void)connectionPanel:(MConnectionPanel*)panel 
               finished:(MYSQL*)mysql
                   info:(MConnectionInfo*)conn;

- (void)connectionPanel:(MConnectionPanel*)panel 
                aborted:(BOOL)status;
@end


@interface MConnectionPanel : NSWindowController
{
  @protected
    IBOutlet NSButton *connectButton;
    IBOutlet NSPopUpButton *connectionPopUp;
    IBOutlet NSButton *detailsButton;
    IBOutlet NSPanel *errorPanel;
    IBOutlet NSTextField *errorText;
    IBOutlet NSTextField *hostname;
    IBOutlet NSSecureTextField *password;
    IBOutlet NSButton *pingButton;
    IBOutlet NSTextView *pingText;
    IBOutlet id pingTextContainer;
    IBOutlet NSTextField *port;
    IBOutlet NSStepper *portSpin;
    IBOutlet NSProgressIndicator *progressIndicator;
    IBOutlet NSTextField *username;
    IBOutlet NSView *extraView;
    IBOutlet NSImageView *topImage;
    IBOutlet NSTextField *schema;
    
    IBOutlet NSTextField *socketText;
    IBOutlet NSButton *compress;
    IBOutlet NSButton *ansiQuotes;

    id<MConnectionPanelDelegate> _delegate;

    int _lastSelectedConnection;
    
    NSPipe *pingPipe;
    NSTask *pingTask;
    NSFileHandle *pingPipeHandle;
    
    MYSQL *_mysql;
    BOOL _connecting;
    BOOL _connectionFailed; // written by thread
    BOOL _pickSchema;
    BOOL _allowSkip;
    BOOL _skip;
    NSString *_connectionsFile;
    MYX_USER_CONNECTIONS *_connections;
    MYX_USER_CONNECTION _new_connection;
}

- (IBAction)cancel:(id)sender;
- (IBAction)closeErrorPanel:(id)sender;
- (IBAction)connect:(id)sender;
- (IBAction)connectionChanged:(id)sender;
- (IBAction)pingHost:(id)sender;
- (IBAction)toggleDetails:(id)sender;
- (IBAction)advancedOptionChanged:(id)sender;

- (id)initWithConnectionsFile:(NSString*)file;

- (void)setEditsSchema;
- (void)setAllowSkip;

- (void)setHeaderImage:(NSImage*)image;

- (void)setDelegate:(id)deleg;

- (void)show;
- (void)showAndConnectTo:(NSString*)connection;

@end
