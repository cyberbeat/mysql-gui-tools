/* MQRSSearchPanel */

#import <Cocoa/Cocoa.h>

@class MQRSSearchDataSource;

@interface MQRSSearchPanel : NSWindowController
{
    IBOutlet NSTableView *columnList;
    IBOutlet NSComboBox *searchCombo;
    
    MQRSSearchDataSource *_searchArray;
    NSString *_searchText;
    
    SEL _searchSelector;
    id _searchTarget;
}
- (IBAction)searchNext:(id)sender;
- (IBAction)searchPrev:(id)sender;

- (void)showPanelToSearchColumns:(NSArray*)columns;

- (void)setSearchSelector:(SEL)selector
                   target:(id)target;

@end
