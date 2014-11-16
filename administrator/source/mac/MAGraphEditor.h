/* MAGraphEditor */

#import <Cocoa/Cocoa.h>
#include "myx_admin_public_interface.h"

@interface MAGraphEditor : NSObject
{
    IBOutlet id autoMax;
    IBOutlet id formula;
    IBOutlet id graphType;
    IBOutlet id maxCaption;
    IBOutlet id maxFormula;
    IBOutlet id maxValue;
    IBOutlet id minValue;
    IBOutlet id showTitle;
    IBOutlet id title;
    IBOutlet id unit;
    IBOutlet id valueCaption;
    IBOutlet id window;
    
    SEL _applySelector;
    id _target;
    
    id _editedGraph;
}

+ (MAGraphEditor*)editGraph:(NSDictionary*)graph;
- (void)setGraph:(NSDictionary*)graph;
- (NSDictionary*)graph;

- (IBAction)cancelClicked:(id)sender;
- (IBAction)okClicked:(id)sender;
- (NSWindow*)window;

- (void)setApplyAction:(SEL)selector;
- (void)setTarget:(id)target;

- (void)setEditedGraphObject:(id)graph;
- (id)editedGraphObject;

@end
