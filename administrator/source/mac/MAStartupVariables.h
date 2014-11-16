/* MAStartupVariables */

#import <Cocoa/Cocoa.h>
#include <Security/Authorization.h>
#import "MAPanel.h"

@class MAXMLGUIController;

@interface MAStartupVariables : MAPanel
{
  IBOutlet NSTextField *noteText;
  IBOutlet NSTextField *bigText;
  IBOutlet NSTabView *tabView;
  IBOutlet NSPopUpButton *popup;
  IBOutlet NSButton *openButton;
  
  IBOutlet NSPanel *pickPanel;
  IBOutlet NSTextView *sampleText;
  IBOutlet NSPopUpButton *sectionPop;
  
  IBOutlet id adView;
  
  MAXMLGUIController *_xmlGUI;
 
  AuthorizationRef _authRef;
}
- (IBAction)discardChanges:(id)sender;
- (IBAction)saveChanges:(id)sender;
- (IBAction)openFile:(id)sender;

- (IBAction)sectionOK:(id)sender;

- (IBAction)moreInfo:(id)sender;

@end
