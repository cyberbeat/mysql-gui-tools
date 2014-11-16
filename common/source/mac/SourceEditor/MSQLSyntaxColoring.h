//
//  MSQLSyntaxColoring.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/20/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "MSyntaxColoring.h"

#include "myx_public_interface.h"

@interface MSQLSyntaxColoring : MSyntaxColoring
{
  MYX_SYN *_syn;
  
  MYX_SQL_HIGHLIGHTING *_hl;
}

- (id)initForTextView:(NSTextView*)textView
           syntaxInfo:(MYX_SYN*)info;

@end
