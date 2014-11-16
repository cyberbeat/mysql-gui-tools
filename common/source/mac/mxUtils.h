//
//  mxUtils.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 9/20/04.
//  Copyright 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#include "myx_public_interface.h"
#import "myxutil.h"

#ifdef __cplusplus
extern "C" {
#endif
  
typedef enum {
  MXMiniErrorImage,
  MXMiniWarningImage,
  MXMiniNoticeImage
} MXCommonImage;
  
NSString *MXGetErrorString(MYX_LIB_ERROR error);

void MXRunAlertPanelWithError(NSString *title, NSString *message, MYX_LIB_ERROR error);
void MXRunAlertPanelWithMySQLError(NSString *title, NSString *message, MYSQL *mysql);

NSImage *MXGetImageFromBundle(NSBundle *bundle, NSString *image);

NSImage *MXGetCommonImage(MXCommonImage img);

void MXExpandOutline(NSOutlineView *outline, BOOL expandChildren);
NSDictionary *MXGetExpandedOutlineItems(NSOutlineView *outline, int levels, id column);
void MXExpandOutlineItems(NSOutlineView *outline, NSDictionary *items, id column);

void MXResizeTableColumnToFitValue(NSTableView *table, NSTableColumn *column, NSString *value);
void MXResizeTableColumnsToFit(NSTableView *table, NSTableColumn *column);

static inline NSRect MXRectWithSize(NSSize size)
{
  NSRect rect= {{0,0}, size};
  return rect;
}

#ifdef __cplusplus
}
#endif
  