#import "MQFieldViewer.h"

#import "MQBinaryViewer.h"
#import "MQTextViewer.h"
#import "MQImageViewer.h"

@implementation MQFieldViewer

- (IBAction)cancel:(id)sender
{
  [self close];  
}


- (void)applyChanges
{
  if ([_currentViewer respondsToSelector:@selector(editedData)])
  {
    NSData *data= [_currentViewer editedData];
    if (data)
    {
      unsigned int i;
      
      [_data release];
      _data= [data retain];
      
      for (i= 0; i < [_viewers count]; i++)
      {
        if ([_viewers objectAtIndex:i] != _currentViewer)
          [[_viewers objectAtIndex:i] setData:_data];
      }
    }
  }
}


- (IBAction)ok:(id)sender
{
  if (_editing)
  {
    [self applyChanges];
    
    [delegate performSelector:@selector(fieldViewer:saveData:)
                   withObject:self
                   withObject:_data];
  }
  [self close];
}


- (void)windowWillClose:(NSNotification*)notif
{
  [delegate performSelector:@selector(fieldViewerDidClose:)
                 withObject:self];
}


- (void)resetView
{
  unsigned int i, c= [_viewers count];
  id viewer;
  
  [formatPop removeAllItems];  
  for (i= 0; i < c; i++)
  {
    viewer= [_viewers objectAtIndex:i];
    [formatPop addItemWithTitle:[viewer label]];
    [[formatPop lastItem] setRepresentedObject:viewer];
  }
  
  [infoText setStringValue:[NSString stringWithFormat:@"Length: %i bytes", [_data length]]];
  
  viewer= [_viewers objectAtIndex:0];
  [[viewer view] setFrameSize:[contentView frame].size];
  [contentView addSubview:[viewer view]];
}


- (IBAction)changeView:(id)sender
{
  id viewer;

  if (_currentViewer)
    [self applyChanges];

  [[[contentView subviews] lastObject] removeFromSuperview];
  viewer= [[formatPop selectedItem] representedObject];
  [[viewer view] setFrameSize:[contentView frame].size];
  [contentView addSubview:[viewer view]];
  _currentViewer= viewer;
}


- (IBAction)load:(id)sender
{
  NSOpenPanel *panel= [NSOpenPanel openPanel];
  [panel setTitle:@"Load Value From File"];
  if ([panel runModal] == NSFileHandlingPanelOKButton)
  {
    NSData *data= [NSData dataWithContentsOfFile:[panel filename]];
    unsigned int i, c= [_viewers count];
    
    [_data release];
    _data= [data retain];
    
    for (i= 0; i < c; i++)
    {
      id viewer= [_viewers objectAtIndex:i];
      [viewer setData:_data];
    }
    [self resetView];
  }    
}

- (IBAction)save:(id)sender
{
  NSSavePanel *panel= [NSSavePanel savePanel];
  [panel setTitle:@"Save Value to File"];
  if ([panel runModal] == NSFileHandlingPanelOKButton)
  {
    [_data writeToFile:[panel filename] atomically:NO];
  }  
}


- (id)init
{
  self= [super initWithWindowNibName:@"FieldViewer" owner:self];
  if (self)
  {
    [self loadWindow];
    _viewers= [[NSMutableArray alloc] init];
    [[NSNotificationCenter defaultCenter] addObserver:self 
                                             selector:@selector(windowWillClose:)
                                                 name:NSWindowWillCloseNotification
                                               object:[self window]];
  }
  return self;
}


- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [_info release];
  [_viewers release];
  [_data release];
  [super dealloc];
}


- (void)setDelegate:(id)deleg
{
  delegate= deleg;
}

- (void)setInfo:(NSDictionary*)info
{
  [columnText setStringValue:
    [NSString stringWithFormat:@"Column: %@", [info objectForKey:@"columnName"]]];
  _info= [info retain];
}

- (NSDictionary*)info
{
  return _info;
}

- (void)showData:(NSData*)data editable:(BOOL)editable
{
  id viewer;
  NSView *v;
  
  _data= [data retain];
  _editing= editable;
  
  viewer= [[[MQBinaryViewer alloc] initWithData:data] autorelease];
  _currentViewer= viewer;
  [_viewers addObject:viewer];
  [viewer setEditable:editable];
  //[_viewers addObject:[[[MQTextViewer alloc] initWithData:data] autorelease]];
  
  if ([MQImageViewer canDisplayData:_data])
  {
    viewer= [[[MQImageViewer alloc] initWithData:data] autorelease];
    [_viewers addObject:viewer];
    [viewer setEditable:editable];
  }
  
  v= [[self window] contentView];
  if (editable)
  {
    [[v viewWithTag:11] setHidden:NO];
    [[v viewWithTag:12] setHidden:NO];
  }
  else
  {
    [[v viewWithTag:11] setHidden:YES];
    [[v viewWithTag:12] setHidden:YES];
  }

  [self resetView];
}


- (void)showTextData:(NSData*)data editable:(BOOL)editable
{
  id viewer;
  NSView *v;

  _data= [data retain];
  _editing= editable;

  viewer= [[[MQTextViewer alloc] initWithData:data] autorelease];
  _currentViewer= viewer;
  [_viewers addObject:viewer];
  [viewer setEditable:editable];

  viewer= [[[MQBinaryViewer alloc] initWithData:data] autorelease];
  [_viewers addObject:viewer];
  [viewer setEditable:editable];
  
  v= [[self window] contentView];
  if (editable)
  {
    [[v viewWithTag:11] setHidden:NO];
    [[v viewWithTag:12] setHidden:NO];
  }
  else
  {
    [[v viewWithTag:11] setHidden:YES];
    [[v viewWithTag:12] setHidden:YES];
  }

  [self resetView];
}

@end
