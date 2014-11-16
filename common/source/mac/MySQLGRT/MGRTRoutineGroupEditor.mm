//
//  MGRTRoutineGroupEditor.mm
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/9/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MGRTRoutineGroupEditor.h"
#import "MGRTRoutineEditor.h"
#import <MySQLToolsCommon/MAccessoryScrollView.h>
#import <MySQLToolsCommon/mxUtils.h>


@implementation MGRTRoutineGroupEditor

- (id)init
{
  self= [super initWithWindowNibName:@"GRTRoutineGroupEditor"];
  if (self)
  {
    _editors= [[NSMutableArray alloc] init];
  }
  return self;
}


- (void)dealloc
{
  [_editors release];
  delete _groupObject;
  [super dealloc]; 
}


- (void)setupButtonsForScrollView:(MAccessoryScrollView*)sv
                        addAction:(SEL)addSel
                     deleteAction:(SEL)delSel
{
  NSButton *button;
  float size;
  
  //  size= [NSScroller scrollerWidthForControlSize:NSMiniControlSize];
  size= 16;
  
  button= [[[NSButton alloc] initWithFrame:NSMakeRect(0,0,size,size)] autorelease];
  [button setButtonType: NSMomentaryChangeButton];
  [button setBordered:NO];
  [[button cell] setImagePosition:NSImageOnly];
  [button setImage: MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"mini_add.png")];
  [button setAlternateImage: MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"mini_add_pressed.png")];
  
  [button setTarget:self];
  [button setAction:addSel];
  [sv addSubview:button];
  [sv addHorizontalAccessoryView:button];
  
  button= [[[NSButton alloc] initWithFrame:NSMakeRect(0,0,size,size)] autorelease];
  [button setButtonType: NSMomentaryChangeButton];
  [button setBordered:NO];
  [[button cell] setImagePosition:NSImageOnly];
  [button setImage: MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"mini_del.png")];
  [button setAlternateImage: MXGetImageFromBundle([NSBundle bundleForClass:[self class]], @"mini_del_pressed.png")];
  [button setTarget:self];
  [button setAction:delSel];
  [sv addSubview:button];
  [sv addHorizontalAccessoryView:button];
}


- (void)addEditor:(MGRTValue*)routine
{
  MGRTRoutineEditor *editor= [[MGRTRoutineEditor alloc] init];
  
  [_content addSubview:[editor baseView]];
  
  [editor setRoutine:routine];
  [editor setDelegate:self];
  
  [_editors addObject:editor];
  [editor release];
}


- (void)addRoutine:(id)sender
{
  MYX_GRT_VALUE *routine;
  
  routine= [_grt performModule:@"Workbench"
                      function:@"addRoutine"
                     arguments:[NSArray arrayWithObjects:
                       [NSValue valueWithPointer:_groupObject->ownerSchema().grtValue()],
                       @"New_Routine", nil]];
  if (routine)
  {
    MGRTValue grtRoutine(routine);
    
    _groupObject->addRoutine(grtRoutine);
    
    if ([sender tag] == 1)
    {
      grtRoutine.set("routineCode", 
                     "-- Comment\n"
                     "-- \n\n"
                     "CREATE PROCEDURE test()\n"
                     "BEGIN\n\n"
                     "END");
    }
    else
    {
      grtRoutine.set("routineCode", 
                     "-- Comment\n"
                     "-- \n\n"
                     "CREATE FUNCTION test() RETURNS INT\n"
                     "BEGIN\n"
                     "  RETURN 0;\n"
                     "END");
    }
    
    [self addEditor:&grtRoutine];
    myx_grt_value_release(routine);
  }
  else
    NSLog(@"Error calling addRoutine: %@", [_grt lastErrorDescription]);
}


- (void)routineEditorDelete:(MGRTRoutineEditor*)sender
{
  MGRTValue *routine= [sender routine];
  
  _groupObject->removeRoutine(*routine);
  
  [[sender baseView] removeFromSuperview];
  [_editors removeObject:sender];
}


- (void)awakeFromNib
{
  NSRect frame;
  
  [scrollView setHasVerticalScroller:YES];
  [scrollView setHasHorizontalScroller:YES];
  [scrollView setBorderType:NSGrooveBorder];
//  [scrollView setA
  
  frame.origin= NSMakePoint(0,0);
  frame.size= [scrollView contentSize];
  _content= [[MVerticalBox alloc] initWithFrame:frame];
  [_content setExpandsHorizontally:YES];
  [scrollView setDocumentView:_content];
  [_content setOffset:NSMakeSize(0, 0)];
  [_content setAutoresizingMask:NSViewWidthSizable|NSViewMaxYMargin];
  [_content release];
}


- (BOOL)commit
{
  NSEnumerator *en;
  MGRTRoutineEditor *editor;
  
  en= [_editors objectEnumerator];

  while ((editor= [en nextObject]))
  {
    [editor saveChanges];
  }
  
  _groupObject->setName([[nameText stringValue] UTF8String]);
  
  _groupObject->commit();
  
  return YES;
}


- (void)revert
{
}


- (void)showObject
{
  for (int i= 0; i < _groupObject->routineCount(); i++)
  {
    MGRTValue routine(_groupObject->getRoutine(i));

    [self addEditor:&routine];
  }

  [nameText setStringValue:NSStr(_groupObject->name())];
}


- (MYX_GRT_VALUE*)editedObject
{
  return _groupObject->value().grtValue();
}




- (void)editObject:(MYX_GRT_VALUE*)value
{
  _groupObject= new MGRTRoutineGroup([_grt grt], value);
  
  [self showObject];
}


@end





