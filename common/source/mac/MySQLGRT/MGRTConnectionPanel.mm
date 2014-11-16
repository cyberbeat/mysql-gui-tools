#import <MySQLToolsCommon/MDialogs.h>
#import <MySQLToolsCommon/MPreferences.h>
#include <MySQLToolsCommon/myxutil.h>
#import <MySQLToolsCommon/MPreferenceEditor.h>

#import <MySQLGRT/MGRTConnectionPanel.h>

#include <mysqld_error.h>

#define PARAM_LINE_HEIGHT 27


@interface MGRTParameterMapping : NSObject
{
  @public
  NSTextField *label;
  NSControl *control;
  MGRTValue *param;
}
- (void)setValueFromString:(NSString*)value;
- (NSString*)valueAsString;
@end

@implementation MGRTParameterMapping
- (void)dealloc
{
  delete param;
  [super dealloc];
}

- (void)setValueFromString:(NSString*)value
{
  if ([control isKindOfClass:[NSTextField class]])
    [(NSTextField*)control setStringValue:value];
  else if ([control isKindOfClass:[NSButton class]])
    [(NSButton*)control setEnabled:[value isEqualTo:@"0"]?NSOffState:NSOnState];
}

- (NSString*)valueAsString
{
  if ([control isKindOfClass:[NSTextField class]])
    return [control stringValue];
  else if ([control isKindOfClass:[NSButton class]])
    return ([(NSButton*)control state]==NSOnState)?@"1":@"0";
  return nil;
}
@end



@interface MGRTConnectionValue : NSObject
{
  MGRTValue *value;
}
+ (MGRTConnectionValue*)connectionWithValue:(MGRTValue*)value;
- (NSString*)description;
@end;

@implementation MGRTConnectionValue
+ (MGRTConnectionValue*)connectionWithValue:(MGRTValue*)value
{
  MGRTConnectionValue *newValue= [[self alloc] init];
  if (newValue)
  {
    newValue->value= new MGRTValue(*value);    
  }
  return newValue;
}

- (void)dealloc
{
  delete value;
  [super dealloc];
}

- (MGRTValue*)value
{
  return value;
}

- (NSString*)description
{
  return [NSString stringWithUTF8String:(*value)["name"].asString()];
}
@end





@implementation MGRTConnectionPanel

- (id)initWithMGRT:(MGRT*)grt
   connectionsPath:(NSString*)path
        targetPath:(NSString*)target
{
  self= [super initWithWindowNibName: @"GRTConnectPanel"];
  if (self)
  {
    [self loadWindow];
    
    _grt= [grt retain];
    _connInfoPath= [path retain];
    _connTargetPath= [target retain];
    
    _parameters= [[NSMutableArray alloc] init];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(textFieldChanged:)
                                                 name:NSControlTextDidChangeNotification
                                               object:nil];
  }
  return self;
}



- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver: self];
  
  [_grt release];
  [_connInfoPath release];
  [_connTargetPath release];
  [_parameters release];
  
  [super dealloc];
}


- (void)awakeFromNib
{  
//  [[NSNotificationCenter defaultCenter] addObserver:self
//                                           selector:@selector(errorPanelWillClose:)
//                                               name:NSWindowWillCloseNotification
//                                             object:errorPanel];
}


- (NSView*)paramsPanel
{
  return paramsView;
}


- (NSView*)rdbmsPanel
{
  return rdbmsView;
}


- (NSView*)advParamsPanel
{
  return advParamsView;
}


- (void)driverParamValueChanged:(id)sender
{
  if (!_settingConnValues)
  {
    int i, c= [_parameters count];
    BOOL ok= YES;
    for (i = 0; i < c; i++)
    {
      MGRTParameterMapping *mapping= [_parameters objectAtIndex:i];
      
      if ((*mapping->param)["required"].asInt() == 1 || 
          _pickSchema && strcmp((*mapping->param)["name"].asString(), "schema")==0)
      {
        if ([mapping->control isKindOfClass:[NSTextField class]])
        {
          if ([[mapping->control stringValue] isEqualTo:@""])
          {
            ok= NO;
            break;
          }
        }
      }
    }
    [_delegate connectionPanel:self readinessChanged:ok];
    [connectButton setEnabled:ok];
  }
}


- (int)addDriversToPopUp:(MGRTValue*)rdbms
{
  MGRTValue drivers((*rdbms)["drivers"]);
  int i;

  [driverPop removeAllItems];
  
  for (i= 0; i < drivers.count(); i++)
  {
    MGRTValue driver(drivers[i]);

    if (_jdbcOnly && strcmp(driver.contentStruct(), "db.mgmt.JdbcDriver")!=0)
      continue;

    // we dont support driver fetching yet
    if (!driver["isInstalled"].asInt())
      continue;
    
    // driverFilter
  
    // place default driver on top of dropdown
    if ((*rdbms)["defaultDriver"].grtValue() == driver.grtValue())
    {
      [driverPop insertItemWithTitle:NSStr(driver["caption"].asString()) atIndex:0];
      [[driverPop itemAtIndex:0] setRepresentedObject:[NSValue valueWithPointer:driver.grtValue()]];
    }
    else
    {
      [driverPop addItemWithTitle:NSStr(driver["caption"].asString())];
      [[driverPop lastItem] setRepresentedObject:[NSValue valueWithPointer:driver.grtValue()]];
    }
  }
  
  int count= [driverPop numberOfItems];
  [driverPop setEnabled:(count > 1)];
  [driverLabel setEnabled:(count > 1)];
  [driverDescLabel setEnabled:(count > 1)];
  
  return count;
}


- (void)fillStoredConnectionsForDriver:(MGRTValue*)driver
{
  MGRTValue connections(MGRTValue::fromGlobal([_grt grt], [[_connInfoPath stringByAppendingString:@"/storedConns"] UTF8String]));

  [connectionPop removeAllItems];

  for (int i= 0; i < connections.count(); i++)
  {
    MGRTValue connection(connections[i]);
  
    if (strcmp(driver->dictId(), connection["driver"].asString())==0)
    {
      [connectionPop addItemWithTitle:NSStr(connection["name"].asString())];
      [[connectionPop lastItem] setRepresentedObject:[NSValue valueWithPointer:connection.grtValue()]];
    }
  }
  
  [connectionPop addItemWithTitle:@"(New Connection)"];
}


- (void)setConnection:(MYX_GRT_VALUE*)connection
{
  MGRTValue conn(connection);
  
  [addButton setEnabled:connection == 0];
  [removeButton setEnabled:connection != 0];
  
  // if the last entry is selected <New Connection>, clear edits
  if (connection == 0)
  {
    for (int i= 0; i < [_parameters count]; i++)
    {
      MGRTParameterMapping *mapping= [_parameters objectAtIndex:i];

      [mapping setValueFromString:NSStr((*mapping->param)["defaultValue"].asString())];
    }
    //StoredConnComboBox.ItemIndex := -1;
  
    [connectButton setEnabled:NO];
  }
  else
  {
    MGRTValue rdbmsList(MGRTValue::fromGlobal([_grt grt], "/rdbmsMgmt/rdbms"));
    MGRTValue driver(MGRTValue::refObject([_grt grt], conn["driver"].asString()));
    MGRTValue rdbms;
    const char *driverId= driver.dictId();
    
    rdbms.invalidate();
    driver.invalidate();
    
    for (int i= 0; i < rdbmsList.count(); i++)
    {
      rdbms= rdbmsList[i];
      MGRTValue drivers(rdbms["drivers"]);

      for (int j= 0; j < drivers.count(); j++)
      {
        driver= drivers[j];
  
        if (strcmp(driver.dictId(), driverId)==0)
          break;
        
        driver.invalidate();
      }
      
      if (driver.isValid())
        break;

      rdbms.invalidate();
    }
    
    if (!driver.isValid())
    {
      NSLog(@"The driver used by the given connection is not available.");
      return;
    }
    // Select correct RDBMS
    int idx= [rdbmsPop indexOfItemWithRepresentedObject:[NSValue valueWithPointer:rdbms.grtValue()]];
    if (idx > -1)
    {
      if ([rdbmsPop indexOfSelectedItem] != idx)
      {
        [rdbmsPop selectItemAtIndex:idx];
        [self rdbmsSelected:nil];
      }
      // Select correct Driver
      idx = [driverPop indexOfItemWithRepresentedObject:[NSValue valueWithPointer:driver.grtValue()]];
      if (idx > -1) 
      {
        if ([driverPop indexOfSelectedItem] != idx)
        {
          [driverPop selectItemAtIndex:idx];
          [self driverSelected:nil];
        }
      }
      else
        NSLog(@"The driver %s is not available for selection", driver["caption"].asString());
    }
    else
      NSLog(@"The RDBMS %s is not available for selection", rdbms["caption"].asString());
    MGRTValue parameterValues(conn["parameterValues"]);

    for (int i= 0; i < [_parameters count]; i++)
    {
      MGRTParameterMapping *mapping= [_parameters objectAtIndex:i];
      
      [mapping setValueFromString:NSStr(parameterValues[(*mapping->param)["name"].asString()].asString())];
    }
  }

  [self driverParamValueChanged:nil];
}

- (void)refreshRdbmsInfo
{
  int i;
  int mysqlIndex= -1;
  MGRTValue rdbmsList([_grt globalValue:[[_connInfoPath stringByAppendingString:@"/rdbms"] UTF8String]]);

  [rdbmsPop removeAllItems];
  [driverPop removeAllItems];
  
  for (i= 0; i < rdbmsList.count(); i++)
  {
    MGRTValue rdbms(rdbmsList[i]);
  
    if (strcasecmp(rdbms["name"].asString(), "MySQL")!=0)
      continue;
    else
      mysqlIndex= [rdbmsPop numberOfItems];
    
    if ([self addDriversToPopUp:&rdbms] > 0)
    {
      [rdbmsPop addItemWithTitle:[NSString stringWithUTF8String:rdbms["caption"]]];
      [[rdbmsPop lastItem] setRepresentedObject:[NSValue valueWithPointer:rdbms.grtValue()]];
    }
  }
  [rdbmsPop selectItemAtIndex:mysqlIndex];
  [self rdbmsSelected:nil];
  
  [self connectionChanged:nil];
}


- (void)buildDriverControls:(NSView*)container
                     driver:(MGRTValue*)driver
                   advanced:(BOOL)advanced
{
  int i;
  MGRTValue params((*driver)["parameters"]);
  int maxRow= 0;
  float currentTop;
  float currentLeft[100] = {0.0};
  int offsetLeft = 146;
  id prevControl= connectionPop;

  _firstControl= nil;
  
  for (i= 0; i < params.count(); i++)
  {
    MGRTValue param(params[i]);
    int currentRow;
    MGRTParameterMapping *mapping;
    
    if ((param["layoutAdvanced"].asInt()!=0) != advanced)
      continue;
    
    if (!_pickSchema && strcmp(param["name"].asString(), "schema") == 0) 
      continue;
    
    currentRow= param["layoutRow"].asInt();
    if (currentRow == -1)
      currentRow = maxRow + 1;
    maxRow = currentRow;
    
    currentTop = NSHeight([paramsView frame]) - (advanced ? 60 : 60) - (1 + (currentRow - 1) * PARAM_LINE_HEIGHT);
    
    const char *caption= param["caption"];
    const char *descr= param["description"];
    const char *defVal= param["defaultValue"]; 
    const char *paramType= param["paramType"];
    
    mapping= [[[MGRTParameterMapping alloc] init] autorelease];
    
    [_parameters addObject: mapping];
    
    mapping->param= new MGRTValue(param);
    
    float x;

    if (strcmp(paramType, "boolean")!=0 && strcmp(paramType, "tristate")!=0)
    {
      if (caption && *caption)
      {
        float xx;
        mapping->label= [[NSTextField alloc] init];
        [mapping->label setEditable:NO];
        [mapping->label setBordered:NO];
        [[mapping->label cell] setControlSize:NSRegularControlSize];
        [mapping->label setDrawsBackground:NO];
        [mapping->label setStringValue:NSStr(caption)];
        [mapping->label sizeToFit];
        [mapping->label setAlignment:NSRightTextAlignment];
        [mapping->label setAutoresizingMask:NSViewMinYMargin|NSViewMinXMargin];

        // if this is the first param on that row,
        // move the CaptionLbl to the left
        // so the Param edits are aligned left
        if (currentLeft[currentRow] == 0)
          xx= offsetLeft + currentLeft[currentRow] - NSWidth([mapping->label frame]) - 8;
        else
        {
          xx= offsetLeft + currentLeft[currentRow];
          currentLeft[currentRow]+= NSWidth([mapping->label frame]) + 6;
        }
        
        [mapping->label setFrameOrigin:NSMakePoint(xx, currentTop + 4)];
        
        [container addSubview:mapping->label];
      }
            
      // create param edit
      if (strcmp(paramType, "password")==0)
        mapping->control = [[NSSecureTextField alloc] init];
      else
        mapping->control = [[NSTextField alloc] init];
      [(NSTextField*)mapping->control setEditable:YES];
      [mapping->control setAction:@selector(driverParamValueChanged:)];
      [mapping->control setTarget:self];
      x = offsetLeft + currentLeft[currentRow];
      
      [mapping->control setStringValue:NSStr(defVal)];
    }
    else
    {
      // create checkbox
      mapping->control = [[NSButton alloc] init];
      [(NSButton*)mapping->control setButtonType:NSSwitchButton];
      [mapping->control sizeToFit];
      
      currentLeft[currentRow]-= 100;
      
      x= offsetLeft + currentLeft[currentRow];
      
      [mapping->control setAction:@selector(driverParamValueChanged:)];
      [mapping->control setTarget:self];
      
      [(NSButton*)mapping->control setState:strcmp(defVal, "1")==0?NSOnState:NSOffState];
      
      [(NSButton*)mapping->control setTitle:NSStr(caption)];
    }
    
    if (!_firstControl)
      _firstControl= mapping->control;
    
    [mapping->control sizeToFit];
    float width= param.get("layoutWidth", 200);
    if (width < NSWidth([mapping->control frame]))
      width= NSWidth([mapping->control frame]);
    [mapping->control setFrame:NSMakeRect(x, currentTop, width, NSHeight([mapping->control frame]))];
    [mapping->control setAutoresizingMask:NSViewMinYMargin|NSViewMinXMargin];
        
    if (descr && *descr)
      [mapping->control setToolTip: NSStr(descr)];
    
    currentLeft[currentRow] += NSWidth([mapping->control frame]) + 15;
    
    [container addSubview: mapping->control];
    
    [prevControl setNextKeyView:mapping->control];
    prevControl= mapping->control;
    
    // XXX add lookup button
    
    // build param description
    if (_showDescriptions)
    {
      NSTextField *text;
      
      text= [[[NSTextField alloc] init] autorelease];
      [text setStringValue:NSStr(descr)];
      [text setEditable:NO];
      [text sizeToFit];
      [text setFrameOrigin:NSMakePoint(offsetLeft + currentLeft[currentRow], currentTop + 4)];
      [container addSubview:text];
    }
  }

//  [container setFrameSize:NSMakeSize(NSWidth([container frame]), 
//                                     maxRow * PARAM_LINE_HEIGHT + (advanced ? 0 : 30))];
}


- (void)setDelegate:(id)delegate
{
  _delegate= delegate;
}


- (void)textFieldChanged:(NSNotification*)notif
{
  [self driverParamValueChanged:[notif object]];
}



- (IBAction)rdbmsSelected:(id)sender
{
  MGRTValue rdbms((MYX_GRT_VALUE*)[[[rdbmsPop selectedItem] representedObject] pointerValue]);
  
  [self addDriversToPopUp:&rdbms];
  [self driverSelected:nil];
}

- (IBAction)driverSelected:(id)sender
{
  NSArray *subviews= [paramsView subviews];
  for (int i= [subviews count]-1; i>=0; i--)
  {
    NSView *sv= [subviews objectAtIndex:i];
    if (sv != connectionsView)
      [sv removeFromSuperview];
  }
  
  [_parameters removeAllObjects];
  
  [[advParamsView subviews] makeObjectsPerformSelector:@selector(removeFromSuperview)];  
  
  MGRTValue driver((MYX_GRT_VALUE*)[[[driverPop selectedItem] representedObject] pointerValue]);
  
  if (driver["isInstalled"].asInt())
  {
    if ([connectionPop numberOfItems] > 0)
      [connectionPop selectItemAtIndex:0];
    
    [_parameters removeAllObjects];
    [self buildDriverControls:paramsView
                       driver:&driver
                     advanced:NO];
    
//    [mainView setFrameSize:NSMakeSize(NSWidth([mainView frame]), NSHeight([paramsView frame]) + 38)];
    
    [self buildDriverControls:advParamsView
                       driver:&driver
                     advanced:YES];
    
    //[ setFrameSize:NSMakeSize(NSWidth([mainView frame]), NSHeight([paramsView frame]) + 38)];
    
    [self fillStoredConnectionsForDriver:&driver];
  }

  // make 1st responder
  if (_firstControl)
    [[paramsView window] makeFirstResponder:_firstControl];
}


- (void)setEditsSchema
{  
  _pickSchema= YES;
}


- (void)setSelectRdbms
{
  _pickRdbms= YES;
}

- (IBAction)cancel:(id)sender
{
  [NSApp stopModalWithCode:NSCancelButton];
}


- (IBAction)connect:(id)sender
{
  [self writeConnectionToTarget];
  
  [NSApp stopModalWithCode:NSOKButton];
}


- (IBAction)saveConnection:(id)sender
{
  NSString *name= nil;
  
  if ([connectionPop indexOfSelectedItem] < [connectionPop numberOfItems]-1)
    name= [connectionPop titleOfSelectedItem];
  
  MStringRequestSheet *sheet= [MStringRequestSheet sheetWithTitle:@"Store Connection"
                                                           labels:[NSArray arrayWithObject:@"Connection Name:"]];
  [sheet setDefaultValues:[NSArray arrayWithObject:name?:@""]];
  NSArray *result= [sheet runModal:[paramsView window]];
  if (result)
  {
    name= [result lastObject];
   
    MGRTValue newConn([self writeConnectionToTarget]);
        
    newConn.set("name", [name UTF8String]);
    
    // check if there already is a connection with this name stored
    MGRTValue connList([_grt globalValue:[[_connInfoPath stringByAppendingString:@"/storedConns"] UTF8String]]);

    for (int i= 0; i < connList.count(); i++)
    {
      MGRTValue conn(connList[i]);
    
      if (strcmp(conn["driver"].asString(), newConn["driver"].asString())==0
          && strcmp(conn["name"].asString(), [name UTF8String])==0)
      {
        connList.remove(i);
        break;
      }
    }
    
    // Store connection
    connList.append(newConn.copy());
    
    MGRTValue driver((MYX_GRT_VALUE*)[[[driverPop selectedItem] representedObject] pointerValue]);
    [self fillStoredConnectionsForDriver:&driver];

    [connectionPop selectItemAtIndex:[connectionPop numberOfItems]-2];
    [self connectionChanged:nil];
  }

}



- (void)doDeleteConnection:(NSAlert*)alert returnCode:(int)returnCode contextInfo:(void *)contextInfo
{
  [[alert window] orderOut:nil];
  if (returnCode == NSAlertDefaultReturn)
  {
    MGRTValue *delConn= (MGRTValue*)[[[connectionPop selectedItem] representedObject] value];
    
    MGRTValue connList([_grt globalValue:[[_connInfoPath stringByAppendingString:@"/storedConns"] UTF8String]]);
    
    for (int i= 0; i < connList.count(); i++)
    {
      MGRTValue conn(connList[i]);
      
      if (strcmp(conn["driver"].asString(), (*delConn)["driver"].asString())==0
          && strcmp(conn["name"].asString(), (*delConn)["name"].asString())==0)
      {
        connList.remove(i);
        MGRTValue driver((MYX_GRT_VALUE*)[[[driverPop selectedItem] representedObject] pointerValue]);
        [self fillStoredConnectionsForDriver:&driver];
        [self connectionChanged:nil];
        break;
      }
    }
  }
}


- (IBAction)deleteConnection:(id)sender
{
  NSAlert *alert= [NSAlert alertWithMessageText:@"Delete Selected Connection?"
                                  defaultButton:@"Delete" 
                                alternateButton:@"No" 
                                    otherButton:nil
                      informativeTextWithFormat:@"Are you sure you want to delete the selected stored connection?"];

  [alert beginSheetModalForWindow:[self window]
                    modalDelegate:self
                   didEndSelector:@selector(doDeleteConnection:returnCode:contextInfo:)
                      contextInfo:0];
}


- (NSString*)paramValue:(NSString*)param
{
  for (int i= 0; i < [_parameters count]; i++)
  {
    MGRTParameterMapping *mapping= [_parameters objectAtIndex:i];
    
    if (strcmp((*mapping->param)["name"].asString(), [param UTF8String])==0)
    {
      return [mapping valueAsString];
    }
  }
  return nil;
}


- (void)setValue:(NSString*)value forParam:(NSString*)param
{
  for (int i= 0; i < [_parameters count]; i++)
  {
    MGRTParameterMapping *mapping= [_parameters objectAtIndex:i];
    
    if (strcmp((*mapping->param)["name"].asString(), [param UTF8String])==0)
    {
      [mapping setValueFromString:value];
    }
  }
}





- (IBAction)connectionChanged:(id)sender
{
  id pitem= [connectionPop selectedItem];
  id item= [pitem representedObject];
  
  if (!item)
    [self setConnection:0];
  else
    [self setConnection:(MYX_GRT_VALUE*)[item pointerValue]];

  if (_firstControl)
    [[self window] makeFirstResponder:_firstControl];
}


- (IBAction)toggleDetails:(id)sender
{
}


- (void)show
{ 
  [self showWindow: self];  
}

- (void)setHeaderImage:(NSImage*)image
{
  [topImage setImage: image];
}


- (MGRTValue)writeConnectionToTarget
{
  MGRTValue driver((MYX_GRT_VALUE*)[[[driverPop selectedItem] representedObject] pointerValue]);
  MGRTValue conn(MGRTValue::createObject([_grt grt], "db.mgmt.Connection"));
  int i;
  
  conn.set("driver", driver.dictId());
  
  MGRTValue paramValues(MGRTValue::createTypedDict(MYX_STRING_VALUE));
  
  conn.set("parameterValues", paramValues);
  for (i= 0; i < [_parameters count]; i++)
  {
    MGRTParameterMapping *mapping= [_parameters objectAtIndex:i];
    
    if ([mapping->control isKindOfClass:[NSTextField class]])
    {
      paramValues.set((*mapping->param)["name"].asString(), 
                      [[mapping->control stringValue] UTF8String]);
    }
    else if ([mapping->control isKindOfClass:[NSButton class]])
    {
      paramValues.set((*mapping->param)["name"].asString(), 
                      [(NSButton*)mapping->control state] == NSOnState ? "1" : "0");
    }
  }
  
  conn.set("modules", driver["defaultModules"].copy().grtValue());
  
  [_grt setGlobalValue:conn.grtValue()
               forPath:[_connTargetPath UTF8String]];
  
  return conn;
}


@end
