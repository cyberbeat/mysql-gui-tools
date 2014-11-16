//
//  MGRTObjectEditor.mm
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/9/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MGRTObjectEditor.h"
#import <MySQLToolsCommon/mxUtils.h>

@implementation MGRTObjectEditor

- (BOOL)commit // override
{
  return NO;
}


- (void)revert  // override
{
}


- (void)showObject  // override
{
}


- (MYX_GRT_VALUE*)editedObject // override
{
  return NULL;
}


- (const char*)objectId
{
  return myx_grt_dict_id_item_as_string([self editedObject]);
}


- (IBAction)applyChanges:(id)sender
{
  if ([self commit])
  {
    if ([_delegate respondsToSelector:@selector(objectEditorSaved:)])
      [_delegate objectEditorSaved:self];
  }
}

- (IBAction)close:(id)sender
{
  [self revert];
  
  [self close];
  
  if ([_delegate respondsToSelector:@selector(objectEditorClosed:)])
    [_delegate objectEditorClosed:self];
}

- (IBAction)discardChanges:(id)sender
{
  [self revert];
  
  [self showObject];
}


- (id)initWithWindowNibName:(NSString*)name
{
  self= [super initWithWindowNibName:name];
  if (self)
  {
    [self loadWindow];
    
    
    
    [[self window] setDelegate:self];
  }
  return self;
}

- (void)setMGRT:(MGRT*)grt catalogs:(MGRTValue)catalogs
{
  _grt= [grt retain];
  
  _catalogs= new MGRTValue(catalogs);
}

- (void)setDelegate:(id)delegate
{
  _delegate= delegate;
}

- (void)dealloc 
{
  delete _catalogs;
  [_grt release];

  [super dealloc];
}


- (NSString*)editedObjectName
{
  return [NSString stringWithUTF8String:MGRTValue([self editedObject])["name"].asString()];
}


- (void)fillCollationPopUp:(NSPopUpButton*)popup
{
  [popup removeAllItems];
  
  [popup addItemWithTitle:@""];
  [[popup menu] addItem:[NSMenuItem separatorItem]];

  //XXX add most recently used items
  //[[popup menu] addItem:[NSMenuItem separatorItem]];

  if (_catalogs && _catalogs->isValid())
  {
    MGRTValue charsets((*_catalogs)["characterSets"]);
    if (charsets.isValid())
    {
      int i, c= charsets.count();
      
      for (i= 0; i < c; i++)
      {
        MGRTValue chs(MGRTValue::refObject([_grt grt], charsets[i].asString()));    
        MGRTValue colls(chs["collations"]);
        for (int j= 0; j < colls.count(); j++)
        {
          [popup addItemWithTitle:NSStr(colls[j].asString())];
        }
      }
    }
  }
}


- (void)fillCharsetPopUp:(NSPopUpButton*)popup
{
  [popup removeAllItems];
  
  [popup addItemWithTitle:@""];
  [[popup menu] addItem:[NSMenuItem separatorItem]];

  if (_catalogs && _catalogs->isValid())
  {
    MGRTValue charsets((*_catalogs)["characterSets"]);
    if (charsets.isValid())
    {
      int i, c= charsets.count();
      
      for (i= 0; i < c; i++)
      {
        MGRTValue chs(MGRTValue::refObject([_grt grt], charsets[i].asString()));    
        
        [popup addItemWithTitle:NSStr(chs["name"].asString())];
        [[popup lastItem] setRepresentedObject:NSStr(chs["name"].asString())];
      }
    }
  }
}


- (void)fillCollationPopUp:(NSPopUpButton*)popup forCharset:(NSString*)charset
{
  [popup removeAllItems];
  
  if (_catalogs && _catalogs->isValid() && charset)
  {
    MGRTValue charsets((*_catalogs)["characterSets"]);
    if (charsets.isValid())
    {
      int i, c= charsets.count();
      
      for (i= 0; i < c; i++)
      {
        MGRTValue chs(MGRTValue::refObject([_grt grt], charsets[i].asString()));
        const char *defaultCollation= chs["defaultCollation"].asString();
        
        if (strcmp(chs["name"].asString(), [charset UTF8String])==0)
        {
          MGRTValue colls(chs["collations"]);
          for (int j= 0; j < colls.count(); j++)
          {
            [popup addItemWithTitle:NSStr(colls[j].asString())];
            if (strcmp(defaultCollation, colls[j].asString())==0)
              [popup selectItem:[popup lastItem]];
          }
          break;
        }
      }
    }
  }  
}


- (void)setReleaseOnClose:(BOOL)flag
{
  _releaseOnClose= flag;
}

- (void)windowWillClose:(NSNotification*)notification
{
  if (_releaseOnClose)
    [self autorelease];
}


@end
