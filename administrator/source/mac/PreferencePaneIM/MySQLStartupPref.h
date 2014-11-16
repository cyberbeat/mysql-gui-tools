/* MySQLStartupPref */

#import <PreferencePanes/PreferencePanes.h>
#include <Security/Authorization.h>
#include <SecurityInterface/SFAuthorizationView.h>
#include <mysql.h>

#include "helper.h"

typedef enum {
  UI_BadInstallation= 1, // /usr/local/mysql not found, cant proceed
  UI_IMNotFound, // IM not found, cant proceed
  UI_IMNotActive, // IM is installed, but disabled
  UI_IMPasswordless, // no password set, cant be connected
  UI_IMCantConnect, // all fine, but cant connect
  UI_IMNotRunning,
  UI_SingleInstance,
  UI_MultiInstance
} UIMode;


@interface MySQLStartupPref : NSPreferencePane
{
  IBOutlet NSButton *imlessButton;
  IBOutlet NSButton *imlessEnableButton;
  IBOutlet NSButton *imlessCheck;
  IBOutlet NSTextField *imlessStateText;
  IBOutlet NSTextField *imlessNotFoundText;
  IBOutlet NSTextField *imlessWarningText;

  IBOutlet NSButton *imConnectButton;
  IBOutlet NSButton *imButton;
  IBOutlet NSButton *imCheck;
  IBOutlet NSButton *imPasswordButton;
  IBOutlet NSTextField *imPasswordText;
  IBOutlet NSImageView *imStateImage;
  IBOutlet NSTextField *imStateText;
  IBOutlet NSTextField *imWarningText;
  
  IBOutlet NSTableView *instanceList;
  IBOutlet NSTextField *serverBasePathText;
  IBOutlet NSButton *serverButton;
  IBOutlet NSTextField *serverDataPathText;
  IBOutlet NSTextField *serverVersionText;
  IBOutlet NSPopUpButton *serverActionButton;
  
  IBOutlet NSPopUpButton *singleActionButton;
  IBOutlet NSTextField *singleBasePathText;
  IBOutlet NSButton *singleButton;
  IBOutlet NSTextField *singleDataPathText;
  IBOutlet NSTextField *singlePortText;
  IBOutlet NSTextField *singleSocketPathText;
  IBOutlet NSTextField *singleStatusText;
  IBOutlet NSTextField *singleVersionText;
  IBOutlet NSTextField *singleWarningText;
  IBOutlet NSImageView *singleIcon;
  IBOutlet NSTabView *tabview;
  
  IBOutlet NSWindow *imNewAccountWindow;
  IBOutlet NSWindow *imLoginWindow;  
  
  IBOutlet SFAuthorizationView *authView;
  
  NSTabViewItem *_instanceTab;
  NSTabViewItem *_instancesTab;
  NSTabViewItem *_imTab;
  NSTabViewItem *_imDisabledTab;
  
  NSImage *_runningIcon;
  NSImage *_stoppedIcon;
  
  NSImage *_runningMiniIcon;
  NSImage *_stoppedMiniIcon;
  
  UIMode _uiMode;
  char *_mysqlPath;

  NSMutableArray *_instanceList;
  NSTimer *_timer;

  MYSQL *_imConnection;
  char *_imUsername;
  char *_imPassword;
  
  BOOL _checking;
  BOOL _dontTryIMConnect;
}
- (IBAction)changeIMAutoStart:(id)sender;
- (IBAction)setIMPassword:(id)sender;
- (IBAction)setServerPassword:(id)sender;
- (IBAction)showHelp:(id)sender;
- (IBAction)toggleIM:(id)sender;
- (IBAction)toggleServer:(id)sender;
- (IBAction)connectIM:(id)sender;
- (IBAction)newAccountClose:(id)sender;
- (IBAction)toggleCreateAccount:(id)sender;
- (IBAction)performAction:(id)sender;

- (void)checkStatus;
@end
