/* MADataFileEditor */

#import <Cocoa/Cocoa.h>

typedef struct {
  NSString *name;
  NSString *path;
  NSString *total;
  NSString *avail;
  NSString *type;
} VolumeInfo;


@interface MADataFileEditor : NSWindowController
{
    IBOutlet NSTextField *filename;
    IBOutlet NSTableView *fstable;
    IBOutlet NSTextField *size;
    IBOutlet NSPopUpButton *sizeUnit;
    
    VolumeInfo *volumes;
    int volumeCount;
}
- (IBAction)chooseFile:(id)sender;
- (IBAction)okClicked:(id)sender;
- (IBAction)cancelClicked:(id)sender;

- (id)init;

- (NSString*)stringValue;
- (void)setStringValue:(NSString*)value;

@end
