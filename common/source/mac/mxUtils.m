//
//  mxUtils.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 9/20/04.
//  Copyright 2004 MySQL AB. All rights reserved.
//

#import "mxUtils.h"

#define N_(s) s


NSString *MXGetErrorString(MYX_LIB_ERROR error)
{
  static NSString *msgs[]= {
    @"",
    N_(@"Can't open file."),
    N_(@"Can't connect to server instance."),
    N_(@"Error parsing XML file."),
    N_(@"Error parsing XML file (bad document)."),
    N_(@"Error parsing XML file (empty document)."),
    N_(@"Error executing SQL command."),
    N_(@"Executing stopped."),
    N_(@"Internal error in libxml (could not change memory allocators)."),
    N_(@"The object was not found in the database."),
    N_(@"Cannot read from file."),
    N_(@"Error during character set conversion."),
    N_(@"Invalid character set specified.")
  };

  if (error < sizeof(msgs)/sizeof(NSString*))
    return NSLocalizedString(msgs[error], nil);
  else
    return @"Unknown error.";
}


void MXRunAlertPanelWithError(NSString *title, NSString *message, MYX_LIB_ERROR error)
{
  NSString *errmsg= MXGetErrorString(error);
  
  NSRunAlertPanel(title, @"%@\n%@", nil, nil, nil, message, 
                  errmsg,nil);
}

void MXRunAlertPanelWithMySQLError(NSString *title, NSString *message, MYSQL *mysql)
{
  char *tmp;
  NSRunAlertPanel(title, @"%@\n%s (error %i)", nil, nil, nil, message, 
                  tmp= myx_mysql_error(mysql), myx_mysql_errno(mysql));
  g_free(tmp);
}


NSImage *MXGetImageFromBundle(NSBundle *bundle, NSString *image)
{
  NSString *path= [bundle pathForResource:[image stringByDeletingPathExtension]
                                   ofType:[image pathExtension]];
  if (!path)
    return nil;
  
  return [[[NSImage alloc] initWithContentsOfFile:path] autorelease];
}


@interface mxUtilsClass : NSObject
{
}
@end
@implementation mxUtilsClass
@end


NSImage *MXGetCommonImage(MXCommonImage img)
{
  static NSString *commonImages[]= {
    @"mini_error.png",
    @"mini_warning.png",
    @"mini_notice.png"
  };
  static NSMutableDictionary *imageCache= nil;
  NSImage *image;
  
  if (!imageCache)
    imageCache= [[NSMutableDictionary alloc] init];
  
  image= [imageCache objectForKey:commonImages[img]];
  if (!image)
  {
    image= MXGetImageFromBundle([NSBundle bundleForClass:[mxUtilsClass class]], commonImages[img]);
    [imageCache setObject:image forKey:commonImages[img]];
  }
  return image;
}


void MXExpandOutline(NSOutlineView *outline, BOOL expandChildren)
{
  id ds= [outline dataSource];
  id item;
  int i;
  int count= [ds outlineView:outline numberOfChildrenOfItem:nil];

  for (i= 0; i < count; i++)
  {
    item= [ds outlineView:outline child:i ofItem:nil];
    [outline expandItem:item expandChildren:expandChildren];
  }
}


static NSDictionary *getExpandedItems(NSOutlineView *outline, id dataSource, 
                                      id pitem, int levels, id column)
{
  NSMutableDictionary *dict= [NSMutableDictionary dictionary];
  unsigned int i, count= [dataSource outlineView:outline numberOfChildrenOfItem:pitem];
  
  for (i= 0; i < count; i++)
  {
    id item= [dataSource outlineView:outline child:i ofItem:pitem];
    if ([outline isExpandable:item] && [outline isItemExpanded:item])
    {
      id key= [dataSource outlineView:outline
              objectValueForTableColumn:column 
                                 byItem:item];
      id value;
      if (levels > 0 || levels < 0)
        value= getExpandedItems(outline, dataSource, item, levels-1, column);
      else
        value= [NSNull null];
      [dict setObject:value forKey:key];
    }
  }
  return dict;
}


NSDictionary *MXGetExpandedOutlineItems(NSOutlineView *outline, int levels, id column)
{
  if (!column) column= [[outline tableColumns] objectAtIndex:0];
  return getExpandedItems(outline, [outline dataSource], nil, levels, column);
}


static void expandItems(NSDictionary *dict, NSOutlineView *outline, 
                        id dataSource, id pitem, id column)
{
  int count= [dataSource outlineView:outline numberOfChildrenOfItem:pitem];
  unsigned int i;

  for (i= 0; i < count; i++)
  {
    id item= [dataSource outlineView:outline child:i ofItem:pitem];
    id key= [dataSource outlineView:outline
            objectValueForTableColumn:column 
                               byItem:item];
    NSDictionary *subdict;
    
    if ((subdict= [dict objectForKey:key]))
    {
      [outline expandItem:item];
      if ([subdict isKindOfClass:[NSDictionary class]] && [subdict count] > 0)
        expandItems(subdict, outline, dataSource, item, column);
    }
  }
}


void MXExpandOutlineItems(NSOutlineView *outline, NSDictionary *items, id column)
{
  if (!column) column= [[outline tableColumns] objectAtIndex:0];
  expandItems(items,outline,[outline dataSource],nil,column);
}


void MXResizeTableColumnToFitValue(NSTableView *table, NSTableColumn *column, NSString *value)
{
  NSFont *font= [[column dataCell] font];
  float width;

  width= [font widthOfString:value]+8;
  if (width > [column width])
    [column setWidth:width];
}


void MXResizeTableColumnsToFit(NSTableView *table, NSTableColumn *column)
{
  NSFont *font= [[column dataCell] font];
  float width;
  unsigned int i, c;
  id ds= [table dataSource];
  
  width= [column width];
  
  c= [ds numberOfRowsInTableView:table];  
  for (i= 0; i < c; i++)
  {
    NSString *value= [ds tableView:table objectValueForTableColumn:column row:i];
    float w;
    
    w= [font widthOfString:value];
    if (w > width)
      width= w;
  }
  if (width > [column width])
    [column setWidth:width];  
}
