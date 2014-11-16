//
//  MSparseMatrix.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 3/24/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MSparseMatrix.h"


@implementation MSparseMatrix

static unsigned long findRow(MSparseMatrixRow *rows, unsigned long count, unsigned long row,
							 BOOL approx)
{
  unsigned long l, r, m;

  if (count == 0 || rows[count-1].row < row)
	return approx ? count : (unsigned long)-1;
  
  l= 0;
  r= count-1;
  
  while (l <= r && r != (unsigned long)-1)
  {
	m= (r+l)/2;
	
	if (rows[m].row < row)
	  l= m+1;
	else if (rows[m].row > row)
	  r= m-1;
	else
	  return m;
  }
  if (approx)
	return m;
  else
	return (unsigned long)-1;
}


- (id)init
{
  self= [super init];
  if (self)
  {
	_zone= NSDefaultMallocZone();
	
	_rowSize= 8;
	_rows= NSZoneMalloc(_zone, _rowSize*sizeof(MSparseMatrixRow));
  }
  return self;
}


- (void)dealloc
{
  [self removeAllObjects];
  if (_rows)
	NSZoneFree(_zone, _rows);
  
  [super dealloc];
}


- (id)objectAtRow:(unsigned long)row
		   column:(unsigned long)column
{
  unsigned long index= findRow(_rows, _rowCount, row, NO);
  MSparseMatrixRow *srow= index == (unsigned long)-1?NULL:_rows+index;
  if (srow)
  {
	MSparseMatrixColumn *scolumn= srow->column;

	while (scolumn && scolumn->column < column)
	  scolumn= scolumn->next;

	if (scolumn && scolumn->column == column)
	  return scolumn->data;
  }
  return nil;
}

- (void)setObject:(id)obj
			atRow:(unsigned long)row
		   column:(unsigned long)column
{
  unsigned long index= findRow(_rows, _rowCount, row, YES);
  MSparseMatrixRow *srow;
  
  if (index == (unsigned long)-1 || index >= _rowCount || _rows[index].row != row)
  {
	if (_rowCount+1 == _rowSize)
	{
	  _rowSize+= 8;
	  _rows= NSZoneRealloc(_zone, _rows, _rowSize*sizeof(MSparseMatrixRow));
	}
	memmove(_rows+index+1, _rows+index, (_rowCount-index)*sizeof(MSparseMatrixRow));
	_rows[index].row= row;
	_rows[index].column= NULL;
	_rowCount++;
	srow= _rows+index;
  }
  else
	srow= _rows+index;

  if (!srow->column)
  {
	srow->column= NSZoneMalloc(_zone, sizeof(MSparseMatrixColumn));
	srow->column->column= column;
	srow->column->data= [obj retain];
	srow->column->next= NULL;
  }
  else
  {
	MSparseMatrixColumn *scolumn= srow->column;

	if (scolumn->column > column)
	{
	  MSparseMatrixColumn *ncolumn;
	  ncolumn= NSZoneMalloc(_zone, sizeof(MSparseMatrixColumn));
	  ncolumn->column= column;
	  ncolumn->data= [obj retain];
	  ncolumn->next= scolumn;
	  srow->column= ncolumn;
	}
	else
	{
	  MSparseMatrixColumn *prev= NULL;
	  
	  while (scolumn && scolumn->column < column)
	  {
		prev= scolumn;
		scolumn= scolumn->next;
	  }

	  if (scolumn && scolumn->column == column)
	  {
		[scolumn->data release];
		scolumn->data= [obj retain];
	  }
	  else
	  {
		MSparseMatrixColumn *ncolumn;
		ncolumn= NSZoneMalloc(_zone, sizeof(MSparseMatrixColumn));
		ncolumn->column= column;
		ncolumn->data= [obj retain];
		ncolumn->next= scolumn;
		prev->next= ncolumn;
	  }
	}
  }  
}

- (void)removeAllObjects
{
  unsigned long r;
  for (r= 0; r < _rowCount; r++)
  {
	while (_rows[r].column)
	{
	  MSparseMatrixColumn *next= _rows[r].column->next;
	  
	  [_rows[r].column->data release];
	  NSZoneFree(_zone, _rows[r].column);
	  
	  _rows[r].column= next;
	}
  }
  _rowCount= 0;
}


- (MSparseMatrixRow*)firstRow
{
  if (_rowCount == 0)
	return NULL;
  else
	return _rows;
}


- (MSparseMatrixRow*)nextRowAfter:(MSparseMatrixRow*)row
{
  if (row < _rows+_rowCount-1)
	return row+1;
  else
	return NULL;
}


- (MSparseMatrixColumn*)firstColumnInRow:(MSparseMatrixRow*)row
{
  return row->column;
}


- (MSparseMatrixColumn*)nextColumnInRow:(MSparseMatrixRow*)row
								  after:(MSparseMatrixColumn*)column
{
  return column->next;
}

@end
