//
//  MSyntaxColoring.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/20/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MSyntaxColoring.h"


typedef struct MarkerInfo {
  NSString *name;
  NSColor *color;
} MarkerInfo;

typedef struct LineInfo {
  NSRange charRange;
  unsigned int attributes;
} LineInfo;


@implementation MSyntaxColoring

static NSString *colorNames[]= {
  @"CommentColor",
  @"StringColor",
  @"IdentifierColor",
  @"SymbolColor",
  @"NumberColor",
  @"FunctionColor",
  nil
};

static float defaultColors[][3]= {
  {0.5,0.5,0.5}, // Comment
  {0.22,0.6,0.22}, // String
  {0.4,0.6,0.8}, // Identifier
  {0.0,0.35,0.82}, // Symbol
  {0.82,0.35,0.82}, // Number
  {0.8,0.4,0.3}, // Function
};


+ (NSMutableDictionary*)defaultColorDictionary
{
  NSMutableDictionary *initialValues= [NSMutableDictionary dictionary];
  unsigned int i;
  
  for (i= 0; colorNames[i]; i++)
  {
    [initialValues setObject:[NSArchiver archivedDataWithRootObject:[NSColor colorWithDeviceRed:defaultColors[i][0]
                                                                                          green:defaultColors[i][1]
                                                                                           blue:defaultColors[i][2]
                                                                                          alpha:1.0]]
                      forKey:colorNames[i]];
  }
  return initialValues;
}


- (id)initForTextView:(NSTextView*)textView
{
  self= [super init];
  if (self)
  {
    NSUserDefaults *defaults= [NSUserDefaults standardUserDefaults];
    unsigned int i;
    
    _colors= [[NSMutableDictionary alloc] init];
    
    for (i= 0; colorNames[i]; i++)
      [_colors setObject:[[NSDictionary alloc] initWithObjectsAndKeys:
        [NSUnarchiver unarchiveObjectWithData:[defaults objectForKey:colorNames[i]]], NSForegroundColorAttributeName,
        
        nil]
                  forKey:colorNames[i]];

    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(didProcessEditing:)
                                                 name:NSTextStorageDidProcessEditingNotification
                                               object:[textView textStorage]];
    
    _textView= textView;
    _linesSize= 4000;
    _lines= malloc(sizeof(LineInfo)*_linesSize);
    _lineCount= 0;

    _markers= malloc(sizeof(MarkerInfo)*32);
  }
  return self;
}


- (void)dealloc
{
  int i;
  for (i= 0; i < _markerCount; i++)
  {
    [_markers[i].color release];
    [_markers[i].name release];
  }
  free(_markers);
  free(_lines);
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [super dealloc];
}


- (void)setDelegate:(id)deleg
{
  _delegate= deleg;
}


- (void)recolorRange:(NSRange)charRange
{
  // dummy, must be overriden
}


- (void)recolorVisible
{
  NSRect visibleRect= [[[_textView enclosingScrollView] contentView] documentVisibleRect];
  NSRange visibleRange= [[_textView layoutManager] glyphRangeForBoundingRect:visibleRect
                                                             inTextContainer:[_textView textContainer]];
  
  [self recolorRange:visibleRange];
}


- (void)updateAfterDeletion:(int)charCount startingAt:(unsigned int)index
            endsWithNewline:(BOOL)newline
{
  unsigned int l, ll;
  unsigned int first= [self lineIndexForCharacter:index];
  int charsToDelete= charCount;
  int linesDeleted= 0;
  int tmp;
  BOOL joinLines= NO;
  
  if (first == NSNotFound)
  {
    return;
  }
  
  if (NSMaxRange(_lines[first].charRange) <= index + charCount)
    joinLines= YES;

  // subtract the deleted characters from line lengths
  tmp= (NSMaxRange(_lines[first].charRange) - index);  
  if (tmp > charsToDelete)
  {
    tmp-= charsToDelete;
    charsToDelete= 0;
  }
  else
  {
    charsToDelete-= tmp;
    tmp= 0;
  }  
  _lines[first].charRange.length= (index - _lines[first].charRange.location) + tmp;
  for (l= first+1; l < _lineCount; l++)
  {
    if (charsToDelete > 0)
    {
      if (_lines[l].charRange.length >= charsToDelete)
      {
        _lines[l].charRange.length-= charsToDelete;
        charsToDelete= 0;
      }
      else
      {
        charsToDelete-= _lines[l].charRange.length;
        _lines[l].charRange.length= 0;
      }
    }
    if (_lines[l].charRange.length > 0 && joinLines)
    {
      joinLines= NO;
      _lines[first].charRange.length+= _lines[l].charRange.length;
      _lines[l].charRange.length= 0;
    }
  }
  // delete empty lines
  for (ll= l= first; l < _lineCount; l++)
  {
    if (_lines[l].charRange.length==0)
    {
      if (l < _lineCount-1 || !newline)
        linesDeleted++;
      else
      {
        _lines[ll]= _lines[l];
        ll++;
      }
    }
    else
    {
      _lines[ll]= _lines[l];
      ll++;
    }
  }
  _lineCount-= linesDeleted;
  // adjust locations
  for (l= first; l < _lineCount; l++)
  {
    if (l > 0)
      _lines[l].charRange.location= NSMaxRange(_lines[l-1].charRange);
    else
      _lines[l].charRange.location= 0;
  }
}


- (void)updateAfterInsertionOfText:(NSString*)newText startingAt:(unsigned int)startIndex
{  
  unsigned int newLength= [newText length];
  unsigned int index, l, leftOver;
  unsigned int insertLine;
  int newLineCount= 0;
  NSRange lineRange;

  // count number of lines that were added
  for (index = 0; index < newLength; newLineCount++)
  {
    lineRange= [newText lineRangeForRange:NSMakeRange(index, 0)];
    index= NSMaxRange(lineRange);
  }
  if ([newText characterAtIndex:newLength-1]=='\n')
    newLineCount++;
  newLineCount--;

  if (_lineCount+newLineCount+1 >= _linesSize)
  {
    _linesSize+= newLineCount+500;
    _lines= realloc(_lines, sizeof(LineInfo)*_linesSize);
  }

  if (_lineCount == 0)
  { 
    lineRange= NSMakeRange(0, 0);
    for (index= 0; index < newLength; _lineCount++)
    {
      lineRange= [newText lineRangeForRange:NSMakeRange(index, 0)];
      _lines[_lineCount].charRange= lineRange;
      _lines[_lineCount].attributes= 0;
      index= NSMaxRange(lineRange);
    }
    if ([newText characterAtIndex:newLength-1]=='\n')
    {
      _lines[_lineCount].charRange= NSMakeRange(NSMaxRange(lineRange), 0);
      _lines[_lineCount].attributes= 0;
      _lineCount++;
    }
    return;
  }

  // line where the insertion is happening
  insertLine= [self lineIndexForCharacter:startIndex];
  if (insertLine == NSNotFound)
    insertLine= _lineCount-1;

  // open up space 
  if (newLineCount > 0)
  {
    memmove(&_lines[insertLine+newLineCount], &_lines[insertLine], (_lineCount-insertLine)*sizeof(LineInfo));
    for (l= insertLine+1; l <= insertLine+newLineCount; l++)
    {
      _lines[l].charRange.length= 0;
      _lines[l].attributes= 0;
    }
  }
  _lineCount+= newLineCount;

  // update lengths
  leftOver= 0;
  if (NSLocationInRange(startIndex, _lines[insertLine].charRange))
  {
    leftOver= NSMaxRange(_lines[insertLine].charRange) - startIndex;
    _lines[insertLine].charRange.length-= leftOver;
  }
  for (l= index= 0; index < newLength; l++)
  {
    lineRange= [newText lineRangeForRange:NSMakeRange(index, 0)];
    _lines[insertLine+l].charRange.length+= lineRange.length;
    index= NSMaxRange(lineRange);
  }
  if ([newText characterAtIndex:newLength-1]=='\n')
    _lines[insertLine+l].charRange.length= leftOver;
  else
    _lines[insertLine+l-1].charRange.length+= leftOver;

  // update locations
  _lines[0].charRange.location= 0;
  for (l= ((insertLine > 0) ? insertLine : 1); l < _lineCount; l++)
    _lines[l].charRange.location= NSMaxRange(_lines[l-1].charRange);
}


- (void)doRecoloring:(NSValue*)range
{
  [self recolorRange:[range rangeValue]];
  [_delegate performSelector:@selector(syntaxColoringDidFinish:)
                  withObject:self];
}

- (void)didProcessEditing:(NSNotification*)notif
{
  NSTextStorage *textStorage = [notif object];
  NSRange editedRange= [textStorage editedRange];
  int change= [textStorage changeInLength];
  unsigned int deletedCount, addedCount;

  deletedCount= editedRange.length - change;
  addedCount= editedRange.length;
  
  // first find out what lines were deleted and remove them
  if (deletedCount > 0)
  {
    BOOL finalNewline;
    unsigned int len= [[textStorage string] length];
    
    if (len > 0)
    {
      finalNewline= [[textStorage string] characterAtIndex:len-1]=='\n';
      [self updateAfterDeletion:deletedCount startingAt:editedRange.location
                endsWithNewline:finalNewline];
    }
    else
      _lineCount= 0;
  }
  
  // then parse the newly inserted portion and add it to the line array
  if (addedCount > 0)
    [self updateAfterInsertionOfText:[[textStorage string] substringWithRange:editedRange]
                          startingAt:editedRange.location];
  
  
  if (0) {
    LineInfo *oldLines= malloc(_lineCount * sizeof(LineInfo));    
    int count= _lineCount;
    
    memcpy(oldLines, _lines, _lineCount * sizeof(LineInfo));
    
    [self reparseLines];
    
    if (_lineCount != count)
    {
      int i;
      NSLog(@"LINECOUNT DIFFERS! %i, real=%i", count, _lineCount);
      for (i= 0; i < count; i++)
      {
          NSLog(@"%i) CALC %i, %i", i,
                oldLines[i].charRange.location, oldLines[i].charRange.length);
      }
      for (i= 0; i < _lineCount; i++)
      {
          NSLog(@"%i) REAL %i, %i", i,
                _lines[i].charRange.location, _lines[i].charRange.length);
      }
      
    }
    else
    {
      int i;BOOL ok= YES;
      for (i= 0; i < count; i++)
      {
        if (!NSEqualRanges(_lines[i].charRange,oldLines[i].charRange))
        {
          NSLog(@"%i) <%i> %i, %i vs current %i,%i", i,
                oldLines[i].attributes,
                _lines[i].charRange.location, _lines[i].charRange.length,
                oldLines[i].charRange.location, oldLines[i].charRange.length);          
          ok= NO;
        }
        
      }
      if (!ok) NSLog(@"WRONG!");
    }
  }
  
  [self performSelector:@selector(doRecoloring:)
             withObject:[NSValue valueWithRange:editedRange]
             afterDelay:0.0
                inModes:[NSArray arrayWithObjects:NSDefaultRunLoopMode,NSModalPanelRunLoopMode,NSEventTrackingRunLoopMode,nil]];
}


- (void)reparseLines
{
  id string= [_textView string];
  unsigned int index, stringLength = [string length];

  _lineCount= 0;
  for (index = 0; index < stringLength; _lineCount++)
  {
    NSRange lineRange= [string lineRangeForRange:NSMakeRange(index, 0)];

    if (_lineCount >= _linesSize)
    {
      _linesSize+= 500;
      _lines= realloc(_lines, sizeof(LineInfo)*_linesSize);
    }
    _lines[_lineCount].charRange= lineRange;
    _lines[_lineCount].attributes= 0;
    index = NSMaxRange(lineRange);
  }
  if (stringLength > 0 && [string characterAtIndex:stringLength-1]=='\n')
  {
    _lines[_lineCount].charRange= NSMakeRange(stringLength, 0);
    _lines[_lineCount].attributes= 0;
    _lineCount++;
  }
}


static inline int locateIndexInRanges(LineInfo *ranges, 
                                      int start, int end,
                                      int searchedIndex)
{
  int m;
  
  while (start <= end)
  {
    m= (end+start)/2;
    
    if (searchedIndex < ranges[m].charRange.location)
      end= m-1;
    else if (searchedIndex >= ranges[m].charRange.location + ranges[m].charRange.length)
      start= m+1;
    else
      return m;
  }
  return NSNotFound;
}

- (NSRange)lineRangeForCharacterRange:(NSRange)range
{
  int begin= locateIndexInRanges(_lines, 0, _lineCount-1, range.location);
  int end;
  if (begin == NSNotFound)
    return NSMakeRange(NSNotFound,0);

  end= locateIndexInRanges(_lines, begin, _lineCount-1, NSMaxRange(range));
  if (end == NSNotFound)
    return NSMakeRange(begin, _lineCount-1-begin);

  return NSMakeRange(begin, end-begin);
}


- (unsigned int)lineIndexForCharacter:(unsigned int)charIndex
{
  return locateIndexInRanges(_lines, 0, _lineCount-1, charIndex);
}

- (NSRange)characterRangeOfLine:(unsigned int)line
{
  NSAssert(line < _lineCount, @"Line index out of range");
  return _lines[line].charRange;
}


- (unsigned int)numberOfLines
{
  return _lineCount;
}


- (NSArray*)completionListForWord:(NSString*)word
{
  return nil;
}


- (void)addMarkerName:(NSString*)name
{
  NSAssert(_markerCount < 32, @"Too many markers, max is 32");
  
  _markers[_markerCount].name= [name retain];
  _markers[_markerCount].color= nil;
  _markerCount++;
}


- (void)addMarkerName:(NSString*)name
            withColor:(NSColor*)color
{
  NSAssert(_markerCount < 32, @"Too many markers, max is 32");
  
  _markers[_markerCount].name= [name retain];
  _markers[_markerCount].color= [color retain];
  _markerCount++;
}

- (NSColor*)colorForMarker:(NSString*)name
{
  int i;
  for (i= 0; i < _markerCount; i++)
    if ([_markers[i].name isEqualTo:name])
      return _markers[i].color;
  return nil;
}

- (unsigned int)maskForMarker:(NSString*)name
{
  int i;
  for (i= 0; i < _markerCount; i++)
    if ([_markers[i].name isEqualTo:name])
      return (1<<i);
  NSLog(@"Use of unknown marker named '%@'", name);
  return 0;
}


- (void)addMarker:(NSString*)name
           atLine:(unsigned int)line
{
  unsigned int mask= [self maskForMarker:name];
  NSAssert(line < _lineCount, @"Invalid line index in addMarker:atLine:");

  _lines[line].attributes|= mask;
}


- (void)removeMarker:(NSString*)name
              atLine:(unsigned int)line
{
  unsigned int mask= [self maskForMarker:name];
  NSAssert1(line < _lineCount || line==0, @"Invalid line index %i", line);
  if (_lineCount!=0)
    _lines[line].attributes&= ~mask;  
}


- (void)removeMarkersAtLine:(unsigned int)line
{
  NSAssert1(line < _lineCount || line==0, @"Invalid line index %i", line);
  if (_lineCount!=0)
    _lines[line].attributes= 0;  
}


- (void)removeMarkerFromAllLines:(NSString*)name
{
  unsigned int mask= [self maskForMarker:name];
  unsigned int i;
  for (i= 0; i < _lineCount; i++)
    _lines[i].attributes &= ~mask;
}


- (BOOL)marker:(NSString*)name
        atLine:(unsigned int)line
{
  unsigned int mask= [self maskForMarker:name];
  NSAssert1(line < _lineCount || line==0, @"Invalid line index %i", line);
  if (_lineCount!=0)  
    return (_lines[line].attributes & mask)!=0;
  else
    return NO;
}

- (NSArray*)markersAtLine:(unsigned int)line
{
  int i;
  NSAssert1(line < _lineCount || line==0, @"Invalid line index %i", line);
  
  if (_lineCount==0 || _lines[line].attributes==0)
    return nil;
  
  NSMutableArray *array= [NSMutableArray arrayWithCapacity:1];
  for (i= 0; i < _markerCount; i++)
  {
    if (_lines[line].attributes & (1<<i))
      [array addObject: _markers[i].name];
  }
  return array;
}


- (NSColor*)colorMarkerAtLine:(unsigned int)line
{
  int i= 0;
  
  //NSAssert1(line < _lineCount || line==0, @"Invalid line index %i", line); 
  
  if (_lineCount==0 || _lines[line].attributes==0)
    return nil;
  
  for (i= 0; i < _markerCount; i++)
  {
    if (_lines[line].attributes & (1<<i) && _markers[i].color)
      return _markers[i].color;
  }
  return nil;
}



@end
