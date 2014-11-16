//
//  MDialogs.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Sat Jul 10 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface MStringRequestSheet : NSWindow
{
  NSForm *form;
  int button;
}

+ (MStringRequestSheet*)sheetWithTitle:(NSString*)title labels:(NSArray*)labels;
+ (MStringRequestSheet*)sheetWithTitle:(NSString*)title label:(NSString*)label;
- (id)initWithTitle:(NSString*)title labels:(NSArray*)labels;
// Last button is supposed to be Cancel
- (id)initWithTitle:(NSString*)title labels:(NSArray*)labels buttons:(NSArray*)buttons;
- (void)setDefaultValues:(NSArray*)array;
- (NSArray*)runModal:(NSWindow*)window;
- (int)clickedButton;

@end


