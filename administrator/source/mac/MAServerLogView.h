/* MAServerLogView */

#import <Cocoa/Cocoa.h>
#include "myx_admin_public_interface.h"

typedef MYX_LOGFILE* (*MALogParserFunction)(FILE*, int, int, int*);


@interface MAServerLogView : NSObject
{
    IBOutlet NSTableView *indexTable;
    IBOutlet NSSlider *pageSlider;
    IBOutlet NSTextField *pathText;
    IBOutlet NSTextField *sizeText;
    IBOutlet NSTextView *text;
    IBOutlet id topView;
    
    NSString *_path;
  	NSString *_error;
	  FILE *_file;
    MYX_LOGFILE *_log;
    int *_lineIndex;
    int _logtype;
    
    MALogParserFunction _func;
}
- (IBAction)changePage:(id)sender;
- (IBAction)saveLog:(id)sender;
- (IBAction)selectLog:(id)sender;

- (id)initWithFile:(FILE*)file
			  path:(NSString*)path
			 error:(NSString*)error
			parser:(MALogParserFunction)parser;

- (NSView*)topView;

- (void) setFile: (FILE*)file 
            path: (NSString *) path 
          parser: (MALogParserFunction) parser 
           error: (NSString *)error;

- (int) logType;
- (void) setLogType: (int) type;

@end