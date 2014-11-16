
#import "MAGraphEditor.h"

@implementation MAGraphEditor

- (id)init
{
  self= [super init];
  if (self)
  {
    [NSBundle loadNibNamed:@"HealthGraphEditor" owner:self];

    [window makeKeyAndOrderFront:self];
  }

  return self;
}


+ (MAGraphEditor*)editGraph:(NSDictionary*)graph
{
  MAGraphEditor *editor;

  editor= [[MAGraphEditor alloc] init];

  [editor setGraph:graph];

  return [editor autorelease];
}

- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [_editedGraph release];
  [super dealloc];
}


- (void)setGraph:(NSDictionary*)graph
{
  [autoMax setState:[[graph objectForKey:@"autoExtend"] boolValue]?NSOnState:NSOffState];
  [formula setString: [graph objectForKey:@"valueFormula"]?:@""];
  [graphType selectItemAtIndex: [[graph objectForKey:@"graphType"] intValue]-1];
  [maxCaption setStringValue: [graph objectForKey:@"maxCaption"]?:@""];
  [maxFormula setStringValue: [graph objectForKey:@"maxFormula"]?:@""];
  [maxValue setDoubleValue: [[graph objectForKey:@"maxValue"] doubleValue]];
  [minValue setDoubleValue: [[graph objectForKey:@"minValue"] doubleValue]];
  [showTitle setState:[[graph objectForKey:@"showTitle"] boolValue]?NSOnState:NSOffState];
  [title setStringValue:[graph objectForKey:@"title"]?:@""];
  [unit selectItemAtIndex:[[graph objectForKey:@"graphUnit"] intValue]];
  [valueCaption setStringValue:[graph objectForKey:@"valueCaption"]?:@""];
  
  [graphType setEnabled:NO];
}

- (NSDictionary*)graph
{
  id keys[12];
  id values[12];
  int c= 0;
  
  values[c]= [NSNumber numberWithBool:[autoMax state] == NSOnState];
  keys[c]= @"autoExtend";
  c++;
  values[c]= [formula string];
  keys[c]= @"valueFormula";
  c++;
  values[c]= [NSNumber numberWithInt:[[graphType selectedItem] tag]];
  keys[c]= @"graphType";
  c++;
  if ([[maxCaption stringValue] length] > 0)
  {
	values[c]= [maxCaption stringValue];
	keys[c]= @"maxCaption";
	c++;
  }
  if ([[maxFormula stringValue] length] > 0)
  {
	values[c]= [maxFormula stringValue];
	keys[c]=  @"maxFormula";
	c++;
  }
  values[c]= [NSNumber numberWithDouble:[maxValue doubleValue]];
  keys[c]= @"maxValue";
  c++;
  values[c]= [NSNumber numberWithDouble:[minValue doubleValue]];
  keys[c]= @"minValue";
  c++;
  values[c]= [NSNumber numberWithBool:[showTitle state] == NSOnState];
  keys[c]= @"showTitle";
  c++;
  values[c]= [title stringValue];
  keys[c]= @"title";
  c++;
  values[c]= [NSNumber numberWithInt:[unit indexOfItem:[unit selectedItem]]];
  keys[c]= @"graphUnit";
  c++;
  if ([[valueCaption stringValue] length] > 0)
  {
	values[c]= [valueCaption stringValue];
	keys[c]= @"valueCaption";
	c++;
  }
  
  return [NSDictionary dictionaryWithObjects:values forKeys:keys count:c];
}

- (IBAction)cancelClicked:(id)sender
{
  [window performClose:sender];
}

- (IBAction)okClicked:(id)sender
{
  if (_target && _applySelector)
    [_target performSelector:_applySelector withObject:self];
  
  [window performClose:sender];
}

- (NSWindow*)window
{
  return window;
}

- (void)windowWillClose:(NSNotification *)aNotification
{
  [self autorelease];
}

- (void)setApplyAction:(SEL)selector
{
  _applySelector= selector;
}

- (void)setTarget:(id)target
{
  _target= target;
}


- (void)setEditedGraphObject:(id)graph
{
  if (_editedGraph != graph)
  {
    [_editedGraph release];
    _editedGraph= [graph retain];
  }
}

- (id)editedGraphObject
{
  return _editedGraph;
}

@end
