//
//  MShellTextView.m
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/7/13.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MShellTextView.h"

@implementation MShellTextView

- (void)dealloc
{
  [_history release];
  [super dealloc];
}


- (void)setGRTEnvironment:(MGRT*)grt
{
  _grt= grt;
}


- (void)makeReady
{
  NSString *prompt= [_grt shellPrompt];
  
  _promptRange.location= [self selectedRange].location;
  [self insertText:prompt];  
  _promptRange.length= [self selectedRange].location-_promptRange.location;
  _promptShown= YES;

  if (!_history)
    _history= [[NSMutableArray alloc] init];
}


- (void)appendText:(NSString *)aString
{
  BOOL hadPrompt;

  if ([self selectedRange].location < NSMaxRange(_promptRange))
  {
    [self setSelectedRange:NSMakeRange([[self string] length], 0)];
  }
  
  hadPrompt= _promptShown;
  if (_promptShown)
  {
    [self setSelectedRange:_promptRange];
    [self delete:nil];
    _promptShown= NO;
  }
  
  [super insertText:aString];
  
  if (hadPrompt)
    [self makeReady];
}


- (void)insertNewline:(id)sender
{
  NSString *command;
  
  command= [[self string] substringFromIndex:NSMaxRange(_promptRange)];
  [_history insertObject:command atIndex:0];
  _historyIndex= 0;
  
  [super moveToEndOfParagraph:sender];
  [super insertNewline:sender];
  _promptShown= NO;
  [_grt performShellCommand:command];
  [self makeReady];
}


- (void)deleteBackward:(id)sender
{
  NSRange range= [self selectedRange];
  if (range.location > NSMaxRange(_promptRange) || range.length > 0)
    [super deleteBackward:sender];
}


- (void)moveWordBackwardAndModifySelection:(id)sender
{
  NSRange orange= [self selectedRange];
  [super moveWordBackwardAndModifySelection:sender];

  if ([self selectedRange].location < NSMaxRange(_promptRange))
  {
    NSRange range;
    unsigned int end;
    range= [self selectedRange];
    end= NSMaxRange(range);
    range.location= NSMaxRange(_promptRange);
    if (range.location > end)
    {
      [self setSelectedRange:orange];  
      return;
    }
    range.length= end-range.location;
    
    [self setSelectedRange:range];
  }
}


- (void)deleteToBeginningOfLine:(id)sender
{
  NSRange range;
  
  range.location= NSMaxRange(_promptRange);
  range.length= [self selectedRange].location- range.location;
  [self setSelectedRange:range];
  [self delete:sender];
}


- (void)deleteWordBackward:(id)sender
{
  NSRange orange= [self selectedRange];
  
  if (orange.length == 0)
  {
    NSRange range;
    [super moveWordBackwardAndModifySelection:sender];
    if ([self selectedRange].location < NSMaxRange(_promptRange))
    {
      unsigned int end;
      range= [self selectedRange];
      end= NSMaxRange(range);
      range.location= NSMaxRange(_promptRange);
      if (range.location > end)
      {
        [self setSelectedRange:orange];  
        return;
      }
      range.length= end-range.location;
      
      [self setSelectedRange:range];
      [super delete:nil];
    }
    else
      [super deleteWordBackward:sender];
  }
  else
    [super deleteWordBackward:sender];
}



- (void)moveLeft:(id)sender
{
  if ([self selectedRange].location > NSMaxRange(_promptRange))
    [super moveLeft:sender];
  else
    [self setSelectedRange:NSMakeRange(NSMaxRange(_promptRange),0)];
}


- (void)moveUp:(id)sender
{
  if ([self selectedRange].location < NSMaxRange(_promptRange))
    [self setSelectedRange:NSMakeRange(NSMaxRange(_promptRange),0)];
  else
  {
    if ([_history count] > 0 && _historyIndex <= [_history count]-1)
    {
      if (_historyIndex == 0)
      {
        if (_currentString)
          [_currentString release];
        _currentString= [[[self string] substringFromIndex:NSMaxRange(_promptRange)] retain];
      }
      [self setSelectedRange:NSMakeRange(NSMaxRange(_promptRange), 0)];
      [self deleteToEndOfParagraph:nil];
      [self insertText:[_history objectAtIndex:_historyIndex]];
      
      _historyIndex++;
    }
  }
}

- (void)moveDown:(id)sender
{
  if ([self selectedRange].location < NSMaxRange(_promptRange))
    [self setSelectedRange:NSMakeRange(NSMaxRange(_promptRange),0)];
  else
  {
    if (_historyIndex > 0)
    {
      _historyIndex--;
      if (_historyIndex == 0)
      {
        if (_currentString)
        {
          [self setSelectedRange:NSMakeRange(NSMaxRange(_promptRange), 0)];
          [self deleteToEndOfParagraph:nil];
          [self insertText:_currentString];
          [_currentString release];
          _currentString= nil;
        }
      }
      else
      {
        [self setSelectedRange:NSMakeRange(NSMaxRange(_promptRange), 0)];
        [self deleteToEndOfParagraph:nil];
        [self insertText:[_history objectAtIndex:_historyIndex-1]];
      }
    }
    else if (_currentString && 0)
    {
      [self setSelectedRange:NSMakeRange(NSMaxRange(_promptRange), 0)];
      [self deleteToEndOfParagraph:nil];
      [self insertText:_currentString];
      [_currentString release];
      _currentString= nil;
    }
  }
}

@end
