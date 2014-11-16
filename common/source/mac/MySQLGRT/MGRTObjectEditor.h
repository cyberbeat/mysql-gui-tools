//
//  MGRTObjectEditor.h
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/9/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#include <MySQLGRT/MGRT.h>
#include <MySQLGRT/MGRTValue.h>

@interface MGRTObjectEditor : NSWindowController 
{
  MGRT *_grt;
  MGRTValue *_catalogs;
  id _delegate;
  
  BOOL _releaseOnClose;
}

- (IBAction)applyChanges:(id)sender;
- (IBAction)discardChanges:(id)sender;
- (IBAction)close:(id)sender;

- (void)setMGRT:(MGRT*)grt catalogs:(MGRTValue)catalogs;

- (void)setDelegate:(id)delegate;

- (void)setReleaseOnClose:(BOOL)flag;

- (BOOL)commit;
- (void)revert;
- (void)showObject;
- (MYX_GRT_VALUE*)editedObject;
- (const char*)objectId;

- (NSString*)editedObjectName;

- (void)fillCharsetPopUp:(NSPopUpButton*)popup;
- (void)fillCollationPopUp:(NSPopUpButton*)popup;
- (void)fillCollationPopUp:(NSPopUpButton*)popup forCharset:(NSString*)charset;
@end



@interface NSObject(MGRTObjectEditorDelegate)
- (void)objectEditorClosed:(MGRTObjectEditor*)editor;
- (void)objectEditorSaved:(MGRTObjectEditor*)editor;
@end

