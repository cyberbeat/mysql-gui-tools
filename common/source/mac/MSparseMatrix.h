//
//  MSparseMatrix.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 3/24/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>

#define MNotFound  ((unsigned long)-1)

typedef struct MSparseMatrixColumn {
  unsigned long column;
  id data;
  struct MSparseMatrixColumn *next;
} MSparseMatrixColumn;

typedef struct {
  unsigned long row;
  MSparseMatrixColumn *column;
} MSparseMatrixRow;


@interface MSparseMatrix : NSObject 
{
  NSZone *_zone;
  
  MSparseMatrixRow *_rows;
  unsigned long _rowCount;
  unsigned long _rowSize;
}

- (id)objectAtRow:(unsigned long)row
		   column:(unsigned long)column;

- (void)setObject:(id)obj
			atRow:(unsigned long)row
		   column:(unsigned long)column;

- (void)removeAllObjects;

- (MSparseMatrixRow*)firstRow;
- (MSparseMatrixRow*)nextRowAfter:(MSparseMatrixRow*)row;

- (MSparseMatrixColumn*)firstColumnInRow:(MSparseMatrixRow*)row;
- (MSparseMatrixColumn*)nextColumnInRow:(MSparseMatrixRow*)row
								  after:(MSparseMatrixColumn*)column;

@end
