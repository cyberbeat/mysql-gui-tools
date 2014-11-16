#import "MQRSSearchPanel.h"

@interface MQRSSearchDataSource : NSArray
{
  NSArray *_allColumns;
}
@end


@implementation MQRSSearchDataSource
- (id)initWithColumns:(NSArray*)array
{
  self= [super init];
  if (self)
    _allColumns= array;
  return self;
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  return [_allColumns count];
}

- (id)tableView:(NSTableView *)aTableView 
objectValueForTableColumn:(NSTableColumn *)aTableColumn
            row:(int)rowIndex
{
  return [_allColumns objectAtIndex:rowIndex];
}

- (void)tableView:(NSTableView *)aTableView
   setObjectValue:(id)anObject
   forTableColumn:(NSTableColumn *)aTableColumn
              row:(int)rowIndex
{
  
}

@end



//==============================================================================

@implementation MQRSSearchPanel

- (id)init
{
  self= [super initWithWindowNibName:@"RSSearchPanel" owner:self];
  if (self)
  {
  }
  return self;
}


- (void)dealloc
{
  [_searchArray release];
  [_searchText release];
  [super dealloc];
}

- (void)showPanelToSearchColumns:(NSArray*)columns
{
  _searchArray= [[MQRSSearchDataSource alloc] initWithColumns:columns];
  
  [self showWindow:nil];
}

- (void)updateSearchInfo
{
  _searchText= [searchCombo stringValue];
  
}

- (IBAction)searchNext:(id)sender
{
  
}

- (IBAction)searchPrev:(id)sender
{
  
}


- (void)setSearchSelector:(SEL)selector
                   target:(id)target
{
  _searchSelector= selector;
  _searchTarget= target;
}

@end
