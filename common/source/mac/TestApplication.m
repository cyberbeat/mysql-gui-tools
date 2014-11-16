#import "TestApplication.h"
#import "MConnectionPanel.h"
#import "MBoxLayout.h"
#import "MTableEditor.h"

@interface MTestBoxLayout : MBoxLayout
{
  NSColor *color;
  NSString *name;
}
- (void)setColor:(NSColor*)color name:(NSString*)name;
- (void)drawRect:(NSRect)rect;
@end

@implementation MTestBoxLayout
- (void)setColor:(NSColor*)aColor name:(NSString*)aName;
{
  color= aColor;
  name= aName;
}

- (void)drawRect:(NSRect)rect
{
  NSPoint pt1, pt2;
  NSRect r= [self frame];
  [color set];
  r.size.width-=1;
  r.size.height-=1;
  [NSBezierPath strokeRect:r];
  NSLog(@"painting %@ %@", name, NSStringFromRect([self frame]));
  pt1= [self frame].origin;
  pt2= pt1;
  pt2.x+= [self frame].size.width;
  pt2.y+= [self frame].size.height;
  
  [NSBezierPath strokeLineFromPoint:pt1
                            toPoint:pt2];
  [super drawRect:rect];
}
@end


@implementation TestApplication

- (IBAction)showConnectionPanel:(id)sender
{
  _cpanel= [[MConnectionPanel alloc] 
          initWithConnectionsFile: @"mysqlx_user_connections.xml"];
 
  [_cpanel show];
}

- (IBAction)showBoxTest:(id)sender
{
  MTestBoxLayout *topBox, *hbox1, *hbox2, *hbox3;
  
  topBox= [[MTestBoxLayout alloc] initWithFrame:NSInsetRect([testWindow frame],5,5)];
  [topBox setColor:[NSColor blueColor] name:@"toplevel"];
  [topBox setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
  [topBox setAllBorders:10];
  [topBox setVertical:YES];
  [topBox setSpacing:10];
  [testWindow setContentView:topBox];
  
  hbox1= [[MTestBoxLayout alloc] initWithFrame:NSMakeRect(10,110,100,50)];
  [hbox1 setColor:[NSColor redColor] name:@"hb1"];
  [topBox pack:hbox1 expand:YES];

  hbox2= [[MTestBoxLayout alloc] initWithFrame:NSMakeRect(10,10,100,50)];
  [hbox2 setColor:[NSColor greenColor] name:@"hb2"];
  [topBox pack:hbox2 expand:YES];
  
  hbox3= [[MTestBoxLayout alloc] initWithFrame:NSMakeRect(10,10,100,50)];
  [hbox3 setColor:[NSColor lightGrayColor] name:@"hb3"];
  [topBox pack:hbox3 expand:NO];
  
  [topBox updateLayout];
  
  NSLog(@"%@", NSStringFromRect([hbox1 frame]));
  
  [testWindow makeKeyAndOrderFront:self];
}

- (IBAction)showButtonTest:(id)sender
{
  NSPanel *panel= [[NSPanel alloc] initWithContentRect:NSMakeRect(0,00,360,107)
                                             styleMask:NSTitledWindowMask
                                               backing:NSBackingStoreRetained
                                                 defer:YES];
  NSView *view= [[NSView alloc] initWithFrame:[panel frame]];
  NSTextField *text= [[[NSTextField alloc] initWithFrame:NSMakeRect(17,70,326,17)] autorelease];
  NSProgressIndicator *prog= [[[NSProgressIndicator alloc] initWithFrame:NSMakeRect(18,18,250,20)] autorelease];
  NSButton *button= [[[NSButton alloc] initWithFrame:NSMakeRect(268,12,78,32)] autorelease];
  
  [panel setAcceptsMouseMovedEvents:YES];
//  [panel setAlphaValue:0.9];
   
//  [panel setBecomesKeyOnlyIfNeeded:NO];
  [panel setWorksWhenModal:YES];
  [panel setIgnoresMouseEvents:NO];
  [panel setInitialFirstResponder:button];
  
  [button setAction:@selector(closeWindow:)];
  [button setTarget:panel];
  
  [button setBezelStyle:NSRoundedBezelStyle];
  [button setTitle:@"Stop"];
  
  [panel setHasShadow:YES];
  [text setStringValue:@"Hello World"];
  [text setDrawsBackground:NO];
  [text setBordered:NO];
  [prog startAnimation:self];
  
  [view addSubview:text];
  [view addSubview:prog];
  [view addSubview:button];
  [panel setContentView:view];
  
  [panel makeKeyAndOrderFront:self];
    
  //[NSApp runModalForWindow:panel];
  
}

@end
