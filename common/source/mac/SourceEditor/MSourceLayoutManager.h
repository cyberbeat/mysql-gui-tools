//
//  MSourceLayoutManager.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/24/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class MSyntaxColoring;

@interface MSourceLayoutManager : NSLayoutManager 
{
  MSyntaxColoring *_syntaxColor;
}

- (void)setSyntaxColorer:(MSyntaxColoring*)colorer;
- (MSyntaxColoring*)syntaxColorer;

@end
