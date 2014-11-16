//
//  MSQLSyntaxColoring.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/20/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MSQLSyntaxColoring.h"


static NSString *tokenNames[]= {
  @"NormalColor",
  @"IdentifierColor",
  @"IdentifierColor",
  @"CommentColor",
  @"StringColor",
  @"SymbolColor",
  @"FunctionColor"
};

@implementation MSQLSyntaxColoring

- (id)initForTextView:(NSTextView*)textView
           syntaxInfo:(MYX_SYN*)info
{
  self= [super initForTextView:textView];
  if (self)
  {
    _syn= info;
    _hl= myx_init_sql_parsing(_syn);
  }
  return self;
}


- (void)dealloc
{
  myx_free_sql_highlighting(_hl);
  [super dealloc];
}


- (void)recolorRange:(NSRange)charRange
{
  NSRange lineRange= [self lineRangeForCharacterRange:charRange];

  if (lineRange.location != NSNotFound)
  {
    NSString *string= [[_textView textStorage] string];
    unsigned int stringLength= [string length];
    unsigned int line;
    
    for (line= lineRange.location; line <= NSMaxRange(lineRange); line++)
    {
      unsigned int i;
      NSRange range;

      range= [self characterRangeOfLine:line];
      
      [[_textView layoutManager] removeTemporaryAttribute:NSForegroundColorAttributeName
                                        forCharacterRange:range];
      
      myx_highlight(_hl, [[string substringWithRange:range] UTF8String], line);
      
      for (i= 0; i < _hl->words_num; i++)
      {
        NSRange wordRange;
        if (_hl->words[i].word_begin == _hl->words[i].word_end)
          continue;
        
        wordRange= NSMakeRange(_hl->words[i].word_begin + range.location,
                               MIN(_hl->words[i].word_end-_hl->words[i].word_begin+1, stringLength-_hl->words[i].word_begin));
        
        [[_textView layoutManager] addTemporaryAttributes:[_colors objectForKey:tokenNames[_hl->words[i].word_type]]
                                        forCharacterRange:wordRange];
      }
    }
  }
}


- (NSArray*)completionListForWord:(NSString*)word
{
  MYX_SYN_SUGGESTIONS *sugg= myx_lookup_word(_syn, [word UTF8String]);
  if (sugg)
  {
    unsigned int i;
    NSMutableArray *array= [NSMutableArray arrayWithCapacity:sugg->suggestions_num];
    
    for (i= 0; i < sugg->suggestions_num; i++)
    {
      if (strncasecmp(sugg->suggestions[i].name, [word UTF8String], [word length])==0)
      {
        if (sugg->suggestions[i].s_type == MYX_SYN_SYMBOL && [word canBeConvertedToEncoding:NSASCIIStringEncoding])
        {
          char *tmp= g_strdup([word UTF8String]);
          int j;
          BOOL lower= NO;
          // if this is a symbol, it's probably in all upper, so we'll try to
          // match whatever case is the user's word
          for (j= 0; tmp[j] && sugg->suggestions[i].name[j]; j++)
          {
            if (islower(tmp[j]))
            {
              sugg->suggestions[i].name[j]= tolower(sugg->suggestions[i].name[j]);
              lower= YES;
            }
          }
          if (lower)
          {
            for (; sugg->suggestions[i].name[j]; j++)
              sugg->suggestions[i].name[j]= tolower(sugg->suggestions[i].name[j]);
          }
        }
        [array addObject:[NSString stringWithUTF8String:sugg->suggestions[i].name]];
      }
    }
    myx_free_syn_suggestions(sugg);
    return array;
  }
  return nil;
}

@end
