//
//  MCanvasUtils.mm
//  GenericCanvas
//
//  Created by Alfredo Kojima on 05/7/8.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MCanvasUtils.h"


NSString *MGCMessageForError(TGCError error)
{
  switch (error)
  {
    case GC_NO_ERROR: return nil;
    case GC_CANT_OPEN_FILE: return @"Cannot open file.";
    case GC_XML_PARSE_ERROR: return @"XML parse error.";
    case GC_XML_INVALID_DOCUMENT: return @"Invalid document.";
    case GC_XML_EMPTY_DOCUMENT: return @"Empty document.";
    case GC_OBJECT_NOT_FOUND: return @"Object not found.";
    case GC_CANT_READ_FROM_FILE: return @"Cannot read from file.";
    case GC_CHARSET_CONVERSION_ERROR: return @"Character set conversion error.";
    case GC_CHARSET_WRONG_CHARSET_SPECIFIED: return @"Wrong character set specified.";
  }
  return nil;
}
