#import "MAHomePanel.h"
#import "MAdministrator.h"

@implementation MAHomePanel

+ (NSImage*)icon
{
  return [[[NSImage alloc] initWithContentsOfFile: 
    [[NSBundle mainBundle] pathForResource:@"AdministratorIcon"
                                    ofType:@"icns"]] autorelease];
}

+ (NSString*)label
{
  return @"Show All";
}

+ (NSString*)toolTip
{
  return @"Lists All Administrator Sections.";
}


- (id)initWithOwner: (id<MAdministratorProtocol>)owner
{
  self= [super initWithNibFile: @"Home" panelOwner: owner];  
  if (self)
  {    
    [self build];
    
    _defaultFrame= [[self topView] frame];
  }
  return self;
}  


- (void)build
{
  NSArray *panels= MAGetPanelList();
  unsigned int i;
  float y;
  NSRect rect= [topBox frame];
  
  y= [panels count] * (32+8) + 40;
  
  rect.size.height= y;
  [topBox setFrame:rect];
  
  for (i= 1; i < [panels count]; i++)
  {
    id panel= [panels objectAtIndex:i];
    NSButton *button;
    NSTextField *label;
    
    button= [[NSButton alloc] initWithFrame:NSMakeRect(20, y - (20 + i*(32+8)), 32, 32)];
    [button setBordered:NO];
    [button setImagePosition:NSImageOnly];
    [button setImage:[panel icon]];

    label= [[NSTextField alloc] initWithFrame:NSMakeRect(20 + 32 + 8, y - (20 + i*(32+8)) + 4, 400, 20)];
    [label setStringValue:[panel toolTip]];
    [label setDrawsBackground:NO];
    [label setBordered:NO];
    [label setFont:[NSFont systemFontOfSize:[NSFont smallSystemFontSize]]];
    
    [topBox addSubview:button];
    [topBox addSubview:label];
  }
}

@end
