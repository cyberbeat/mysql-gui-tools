#import "MAServerLogView.h"
#include <sys/stat.h>

#define BLOCK_SIZE 4098

static size_t getSize(int fd);

@interface MAServerLogView(Private)
- (void)refresh;

- (int)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(int)rowIndex;
@end

@implementation MAServerLogView(Private)


- (void)refresh
{
  NSMutableString *str= [[[NSMutableString alloc] init] autorelease];
  unsigned int i, offset;

  if (_lineIndex)
    g_free(_lineIndex);
  _lineIndex= 0;
  if (_log)
  {
    _lineIndex= g_malloc(sizeof(int)*_log->lines_num);
    for (offset=0, i= 0; i < _log->lines_num; i++)
    {
      _lineIndex[i]= offset;
      offset+= strlen(_log->lines[i])+1;
      [str appendFormat: @"%s\n",_log->lines[i]];
    }
    [text setString:str];
  }
  else
    [text setString:@"Unable to read log file."];

  [indexTable reloadData];
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  return _log ? _log->events_num : 0;
}

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(int)rowIndex
{
  MYX_LOGFILE_EVENT *event= _log->events + rowIndex;

  if ([[aTableColumn identifier] isEqualToString:@"time"])
  {
    char buffer[64];
    struct tm t;
    t.tm_year= event->date->tm_year;
    t.tm_mon= event->date->tm_mon;
    t.tm_mday= event->date->tm_mday;
    t.tm_hour= event->date->tm_hour;
    t.tm_min= event->date->tm_min;
    t.tm_sec= event->date->tm_sec;
    sprintf(buffer, "%i/%i/%i %i:%i:%i",
            t.tm_mon, t.tm_mday, t.tm_year,
            t.tm_hour, t.tm_min, t.tm_sec);
    strptime(buffer,"%D %T",&t);
 
    return [NSString stringWithUTF8String:buffer];
  }
  else
  {
    switch (event->event_type)
    {
      case MYX_EVENT_START:
        return @"Server startup";
      case MYX_EVENT_END:
        ///row[_columns._icon]= _stop_icon;
        return @"Server shutdown";
      case MYX_EVENT_ERROR:
        ///row[_columns._icon]= _error_icon;
        return @"Error";
      case MYX_EVENT_INNODB_START:
        ///row[_columns._icon]= _start_icon;
        return @"InnoDB startup";
      case MYX_EVENT_INNODB_SHUTDOWN:
        ///row[_columns._icon]= _stop_icon;
        return @"InnoDB shutdown";
      case MYX_EVENT_FORCED_CLOSE_THREAD:
        //row[_columns._icon]= _stop_icon;
        return @"Thread closed";
      case MYX_EVENT_ABORT:
        //row[_columns._icon]= _stop_icon;
        return @"Abort";
      case MYX_EVENT_SELECT:
        //row[_columns._icon]= _stop_icon;
        return @"Select";
      case MYX_EVENT_INIT:
        //row[_columns._icon]= _stop_icon;
        return @"Init DB";
      case MYX_EVENT_CONNECT:
        //row[_columns._icon]= _stop_icon;
        return @"Connect";
      case MYX_EVENT_QUIT:
        //row[_columns._icon]= _stop_icon;
        return @"Quit";
      case MYX_EVENT_QUERY:
        //row[_columns._icon]= _stop_icon;
        return @"Query";
    }
  }
  return nil;
}

-(void) refreshPathText
{
  [pathText setStringValue:_path?:@"Not Found"];
  
  if (_error || !_file)
  {
    [sizeText setStringValue:_error?:@""];
    [pageSlider setMinValue:1.0];
    [pageSlider setMaxValue:1.0];
    [pageSlider setEnabled:NO];	
  }
  else
  {
    int last=0;
    [sizeText setStringValue:[NSString stringWithFormat:@"%i KB",getSize(fileno(_file))/1024]];
    
    _log= (*_func)(_file, BLOCK_SIZE, 1, &last);
    [pageSlider setMinValue:1.0];
    [pageSlider setMaxValue:last];
  }
}

@end


@implementation MAServerLogView

- (IBAction)changePage:(id)sender
{
  int last;
  
  if (_log)
    myx_free_logfile(_log);
  
  _log= (*_func)(_file, BLOCK_SIZE, [pageSlider intValue], &last);
  [self refresh];
}

- (IBAction)saveLog:(id)sender
{
 //XXX 
}

- (IBAction)selectLog:(id)sender
{
  NSRange range;
  int row= [indexTable selectedRow];
  if (row >= 0)
  {
    int line= _log->events[row].line_no;
    range.location= _lineIndex[line];
    range.length= strlen(_log->lines[line]);
  
    [text setSelectedRange:range];
    [text scrollRangeToVisible:range];
  }
}

- (id)initWithFile:(FILE*)file
			  path:(NSString*)path
			 error:(NSString*)error
			parser:(MALogParserFunction)parser
{
  self= [super init];
  if (self)
    [self setFile: file path: path parser: parser error: error];
  return self;
}

- (void) setFile: (FILE*)file 
            path: (NSString *) path 
          parser: (MALogParserFunction) parser 
           error: (NSString *)error
{
  _func= parser;
	_file= file;
  _path= [path retain];
	_error= [error retain];
  
  [self refreshPathText];
}

static size_t getSize(int fd)
{
  struct stat st;
  
  if (fstat(fd, &st) < 0)
	return 0;
  else 
	return st.st_size;
}

- (void)awakeFromNib
{
  [self refreshPathText];

  if (!_error && _file)
    [self refresh];
}

- (NSView*)topView
{
  return topView;
}

- (int) logType
{
  return _logtype;
}

- (void) setLogType: (int) type
{
  _logtype= type;
}


- (void)dealloc
{
  [_path release];
  [_error release];
  if (_log)
    myx_free_logfile(_log);
  if (_file)
	fclose(_file);
  g_free(_lineIndex);
  [super dealloc];
}

@end
