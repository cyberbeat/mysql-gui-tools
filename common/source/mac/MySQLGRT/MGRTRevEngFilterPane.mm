//
//  MGRTRevEngFilterPane.mm
//  MySQLGRT
//
//  Created by Alfredo Kojima on 05/8/30.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MGRTRevEngFilterPane.h"
#include "MGRTValue.h"
#include <MySQLToolsCommon/myxutil.h>
#include <MySQLToolsCommon/NSView_extras.h>

@implementation MGRTRevEngFilterPane



- (void)refreshLists
{
  MGRTValue sourceL= MGRTValue::fromGlobal([_grt grt], "/migration/sourceObjects");
  MGRTValue ignoreL= MGRTValue::fromGlobal([_grt grt], "/migration/ignoreList");
  
  int totalObjectCount= 0;
  
  [_sourceObjects removeAllObjects];
  
  for (int i= 0; i < sourceL.count(); i++)
  {
    MGRTValue obj(myx_grt_list_item_get_reference_value([_grt grt], sourceL.grtValue(), i));
    const char *objStructName;
    
    if (obj.isValid())
      objStructName= obj.contentStruct();
    
    //If the object is of the same struct
    if (obj.isValid()
        && myx_grt_struct_is_or_inherits_from([_grt grt],
                                              objStructName, 
                                              [_structName UTF8String]) == 1) 
    {
      NSDictionary *item;
      MYX_GRT_VALUE *ownerObj;
      NSString *name;
      
      totalObjectCount++;
      
      //Find owner schema
      ownerObj = myx_grt_dict_item_get_reference_value([_grt grt], obj.grtValue(), "owner");
      
      //Build name
      if (ownerObj)
        name= [NSString stringWithFormat:@"%s.%s", 
          myx_grt_dict_item_get_as_string(ownerObj, "name"),
          obj["name"].asString()];
      else
      {
        NSLog(@"Owner object of %s not found", myx_grt_dict_item_get_as_string(obj.grtValue(), "name"));
        continue;
      }

      item= [NSDictionary dictionaryWithObjectsAndKeys:
        [NSValue valueWithPointer:obj.grtValue()], @"object",
        [NSValue valueWithPointer:ownerObj], @"owner",
        name, @"name",
        nil];

      //Check object name against filter list
      BOOL filterHit= NO;
      for (int i= 0; i < ignoreL.count(); i++)
      {
        if (strncmp(ignoreL[i].asString(), [_structName UTF8String], [_structName length]) == 0 &&
            ignoreL[i].asString()[[_structName length]] == ':'
            && strcmp(ignoreL[i].asString() + [_structName length] + 1, [name UTF8String])==0)
          filterHit= YES;
      }

      /*
      for (int j= 0; j < [_ignoreFilters count]; j++)
      {
        const char *filter= [[_ignoreFilters objectAtIndex:j] UTF8String];
        
        if (myx_match_pattern([[NSString stringWithFormat:@"%@:%@", _structName, name] UTF8String],
                              filter, 1, 1)==1)
        {
          filterHit= YES;
          break;
        }
      }*/
      //Check if the object was searched
      if (!filterHit)
      {
        //if (myx_match_pattern([name UTF8String],
        //                      [
        //                     FilterMigrateListEd.SearchEd.Text + '*', 0, 1)==1)
        [_sourceObjects addObject:item];
        //else
        //Inc(SearchIgnores);
      }
      //else
      //[_ FObjIgnoreList.Add(ObjListItem);
    }
  }
  
  [[[box contentView] viewWithTag:13] setStringValue:[NSString stringWithFormat:@"%i / %i", [_sourceObjects count], totalObjectCount]];
  
  if (totalObjectCount == 0)
    [[[box contentView] viewWithTag:10] setState:NSOffState];
  
  [objectList reloadData];
  [ignoreList reloadData];
}


- (void)setup
{
  MYX_GRT_STRUCT *gstruct= myx_grt_struct_get([_grt grt], [_structName UTF8String]);
  const char *caption;
  int inherited;
  NSView *content= [box contentView];
  NSImage *icon= nil, *miniIcon= nil;
  unsigned int imageSize;
  const char *imageData;
  
  if (gstruct)
    caption= myx_grt_struct_get_caption([_grt grt], gstruct, &inherited);
  else
    caption= [_structName UTF8String];
  
  [[content viewWithTag:12] setStringValue:[NSString stringWithUTF8String:caption]];
  [[view viewWithTag:10] setTitle:[NSString stringWithFormat:@"Migrate Objects of Type %s", caption]];

  imageData= myx_grt_struct_get_icon([_grt grt], 
                                     [[[NSBundle bundleForClass:[self class]] resourcePath] UTF8String],
                                     gstruct, MYX_IT_MANY_STANDARD, &imageSize);
  if (imageData)
  {
    icon= [[NSImage alloc] initWithData:[NSData dataWithBytes:imageData length:imageSize]];
  
    [[content viewWithTag:11] setImage:icon];
    [icon release];
  }
  
  imageData= myx_grt_struct_get_icon([_grt grt], 
                                     [[[NSBundle bundleForClass:[self class]] resourcePath] UTF8String],
                                     gstruct, MYX_IT_SMALL, &imageSize);
  if (imageData)
  {
    miniIcon= [[NSImage alloc] initWithData:[NSData dataWithBytes:imageData length:imageSize]];
  
    [miniIcon release];
  }
  _sourceObjects= [[NSMutableArray alloc] init];
  
  [self refreshLists];
}


- (id)initWithMGRT:(MGRT*)grt forStruct:(NSString*)structName
{
  self= [super init];
  if (self)
  {
    NSNib *nib= [[NSNib alloc] initWithNibNamed:@"ReverseEngineeringFilter"
                                         bundle:[NSBundle bundleForClass:[self class]]];
    
    _grt= [grt retain];
    _structName= [structName retain];

    if (![nib instantiateNibWithOwner:self topLevelObjects:nil])
      NSLog(@"Error instantiating nib");
    [nib release];
  }
  return self;
}


- (void)awakeFromNib
{
  [self setup];
  [objectList reloadData];
  [ignoreList reloadData];
  [[box contentView] setEnabledRecursive:NO];
  [self hideDetailed:nil];
}


- (void)dealloc
{
  [_sourceObjects release];
  [_structName release];
  [_grt release];
  
  [super dealloc];
}


- (NSView*)topView
{
  return view;
}


- (IBAction)showDetailed:(id)sender
{
  int i;
  for (i= 15; i <= 21; i++)
  {
    [[[box contentView] viewWithTag:i] setHidden:NO];
  }  
  [[objectList enclosingScrollView] setHidden:NO];
  [[ignoreList enclosingScrollView] setHidden:NO];
  [[[box contentView] viewWithTag:14] setHidden:YES];
  
  [view setFrameSize:NSMakeSize(NSWidth([view frame]), 340)];

  [view setNeedsDisplay:YES];
}


- (IBAction)hideDetailed:(id)sender
{
  int i;
  for (i= 15; i <= 21; i++)
  {
    [[[box contentView] viewWithTag:i] setHidden:YES];
  }
  [[objectList enclosingScrollView] setHidden:YES];
  [[ignoreList enclosingScrollView] setHidden:YES];
  [[[box contentView] viewWithTag:14] setHidden:NO];

  [view setFrameSize:NSMakeSize(NSWidth([view frame]), 100)];  
}


- (IBAction)addItem:(id)sender
{
  MGRTValue ignoreL(MGRTValue::fromGlobal([_grt grt], "/migration/ignoreList"));
  
  if ([sender tag] == 16)
  {
    NSIndexSet *indexes= [objectList selectedRowIndexes];
    if (indexes)
    {
      int index;
      for (index= [indexes firstIndex]; index <= [indexes lastIndex]; index= [indexes indexGreaterThanIndex:index])
      {
        id item= [_sourceObjects objectAtIndex:index];
        ignoreL.append(MGRTValue([[NSString stringWithFormat:@"%@:%@", _structName, [item objectForKey:@"name"]] UTF8String]));
      }
    }
  }
  else
  {
    for (int i= [_sourceObjects count]-1; i >= 0; --i)
    {
      id item= [_sourceObjects objectAtIndex:i];
      
      ignoreL.append(MGRTValue([[NSString stringWithFormat:@"%@:%s", _structName, [item objectForKey:@"name"]] UTF8String]));
    }
  }
  [objectList deselectAll:nil];
  [self refreshLists];
}


- (IBAction)deleteItem:(id)sender
{
  MGRTValue ignoreL(MGRTValue::fromGlobal([_grt grt], "/migration/ignoreList"));

  if ([sender tag] == 17)
  {
    NSIndexSet *indexes= [ignoreList selectedRowIndexes];
    if (indexes)
    {
      int index;
      for (index= [indexes lastIndex]; index >= [indexes firstIndex]; index= [indexes indexLessThanIndex:index])
      {
        ignoreL.remove(index);
      }
    }
  }
  else
  {
    ignoreL.clear();
  }
  [ignoreList deselectAll:nil];
  [self refreshLists];
}


- (void)setSelected:(BOOL)flag
{
  if (flag && [[view viewWithTag:10] state] == NSOffState)
  {
    [[view viewWithTag:10] setState:NSOnState];
    [self toggleMigrate: [view viewWithTag:10]];
  }
  else if (!flag && [[view viewWithTag:10] state] == NSOnState)
  {
    [[view viewWithTag:10] setState:NSOffState];
    [self toggleMigrate: [view viewWithTag:10]];
  }
}


- (IBAction)toggleMigrate:(id)sender
{
  MGRTValue ignoreL(MGRTValue::fromGlobal([_grt grt], "/migration/ignoreList"));
  
  if ([sender state] == NSOnState)
  {
    [[box contentView] setEnabledRecursive:YES];
    myx_grt_list_item_del_as_string(ignoreL.grtValue(), 
                                    [[NSString stringWithFormat:@"%@:*", _structName] UTF8String]);
  }
  else
  {
    [[box contentView] setEnabledRecursive:NO];
    myx_grt_list_item_add_as_string(ignoreL.grtValue(), 
                                    [[NSString stringWithFormat:@"%@:*", _structName] UTF8String]);
  }
}


- (int)numberOfRowsInTableView:(NSTableView *)tableView
{
  if (tableView == objectList)
  {
    return [_sourceObjects count];
  }
  else
  {
    MGRTValue ignoreL= MGRTValue::fromGlobal([_grt grt], "/migration/ignoreList");
    int i, count= 0;
    
    for (i= 0; i < ignoreL.count(); i++)
    {
      if (strncmp(ignoreL[i].asString(), [_structName UTF8String], [_structName length]) == 0 &&
          ignoreL[i].asString()[[_structName length]] == ':')
        count++;
    }
      
    return count;
  }
}


- (id)tableView:(NSTableView *)tableView objectValueForTableColumn:(NSTableColumn *)tableColumn row:(int)row
{
  if (tableView == objectList)
  {
    return [[_sourceObjects objectAtIndex:row] objectForKey:@"name"];
  }
  else
  {
    MGRTValue ignoreL= MGRTValue::fromGlobal([_grt grt], "/migration/ignoreList");

    for (int i= 0; i < ignoreL.count(); i++)
    {
      if (strncmp(ignoreL[i].asString(), [_structName UTF8String], [_structName length]) == 0 &&
          ignoreL[i].asString()[[_structName length]] == ':')
      {
        if (row == 0)
          return NSStr(ignoreL[i].asString() + [_structName length] + 1);
        row--;
      }
    }
    
    return nil;
  }
}


@end
