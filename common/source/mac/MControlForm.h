//
//  MControlForm.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 06/4/30.
//  Copyright 2006 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface MFormCell : NSCell {
  NSString *_descriptionString;
  NSFont *_descriptionFont;
  NSCell *_contentCell;

  NSSize _descriptionSize;
  float _titleWidth;
  float _minActionWidth;
}

- (void)setTitleWidth:(float)width;
- (float)titleWidth;
- (void)setTitle:(NSString*)title;
- (NSString*)title;

- (void)setDescriptionFont:(NSFont*)font;
- (NSFont*)descriptionFont;

- (void)setDescription:(NSString*)description;
- (NSString*)descrption;

- (void)setContentCell:(NSCell*)cell;
- (NSCell*)contentCell;
@end



@interface MControlForm : NSControl {
  NSMutableArray *_rows;
  float _spacing;
}

- (id)initWithFrame:(NSRect)rect;

- (void)addFormRow:(MFormCell*)cell;
- (void)insertFormRow:(MFormCell*)cell atIndex:(int)index;

- (MFormCell*)addTextFieldWithTitle:(NSString*)title
                        description:(NSString*)description;
- (MFormCell*)addCheckBoxWithTitle:(NSString*)title
                       description:(NSString*)description;
- (MFormCell*)addPopUpWithTitle:(NSString*)title
                          items:(NSArray*)items
                    description:(NSString*)description;
- (MFormCell*)addFilePickerWithTitle:(NSString*)title
                           directory:(BOOL)flag
                    description:(NSString*)description;

@end
