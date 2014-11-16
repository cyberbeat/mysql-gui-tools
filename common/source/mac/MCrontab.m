//
//  MCrontab.m
//  MySQLGUICommon
//
//  Created by Vlad on 06.05.05.
//  Copyright 2005 __MyCompanyName__. All rights reserved.
//

#import "MCrontab.h"

#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>

int readfd(int fd, char **pbuf)
{
	int allocsize = 1024;
	int readcount;

	*pbuf = malloc(allocsize);
	int usedspace = 0;
	int freespace = allocsize;

	while((readcount = read(fd, *pbuf + usedspace, freespace)) > 0)
	{
		freespace -= readcount;
		usedspace += readcount;
		if(freespace == 0)
		{
			char *tmp = malloc(allocsize * 2);
			memcpy(tmp, *pbuf, allocsize);
			*pbuf = tmp;
			freespace = allocsize;
			allocsize *= 2;
		}
	}
	if(freespace == 0)
	{
			char *tmp = malloc(allocsize+1);
			memcpy(tmp, *pbuf, allocsize);
			*pbuf = tmp;
			freespace = 1;
			allocsize += 1;
	}
	(*pbuf)[usedspace] = '\0';
	usedspace++;
	freespace--;

	return usedspace;
}

int launch_sync(char *argv[], char *envp[], char **stdout_text, char **stderr_text)
{
	int status = 0;

	int stdout_pipe[2];
	int stderr_pipe[2];

	pipe(stdout_pipe); 
	pipe(stderr_pipe); 

	pid_t pid = fork();
	if(pid == 0)
	{
		close(STDOUT_FILENO); dup(stdout_pipe[1]); close(stdout_pipe[1]);
		close(STDERR_FILENO); dup(stderr_pipe[1]); close(stderr_pipe[1]);
		execve(argv[0], argv, envp);
	}
	else
	{
		close(stdout_pipe[1]);
		close(stderr_pipe[1]);
		waitpid(pid, &status, 0);
		if(stdout_text != NULL)
		{
			readfd(stdout_pipe[0], stdout_text);
		}
		if(stderr_text != NULL)
		{
			readfd(stderr_pipe[0], stderr_text);
		}
	}
	return status;
}

@implementation MCrontabEntry : NSObject

- (id)init
{
	if(self = [super init]) 
	{
		_minute = -1;
		_hour = -1;
		_month = -1;
		_weekday = -1;
		_day = @"";
		_command = @"";
		
		return self;
	}
	return nil;
}

@end

@implementation MCrontab : NSObject

- (id) init
{
	if(self = [super init])
	{
		_lines = [NSMutableArray arrayWithCapacity: 16];
		return self;
	}
	return nil;
}

- (NSString*) formatEntry: (MCrontabEntry *) entry
{
  NSString *line;
  
  if (entry->_minute < 0)
    line = @"*";
  else
    line = [[NSString alloc] initWithFormat: @"%d", entry->_minute];
  
  if (entry->_hour < 0)
    line = [line stringByAppendingString: @" *"];
  else 
  {
    line = [line stringByAppendingFormat: @" %d", entry->_hour]; 
  }
  
  if ([entry->_day length] == 0)
    line = [line stringByAppendingString: @" *"];
  else
  {
    line = [line stringByAppendingFormat: @" %@", entry->_day]; 
  }
  
  if (entry->_month < 0)
    line = [line stringByAppendingString: @" *"];
  else 
  {
    line = [line stringByAppendingFormat: @" %d", entry->_month]; 
  }
  
  if (entry->_weekday == -1)
    line = [line stringByAppendingString: @" *"];
  else
  {
    NSString *wdays = [[NSString alloc] init];
	int i;
    for (i = 0; i < 7; i++)
    {
      if (entry->_weekday & (1<<i))
      {
        if ([wdays length] == 0)
          wdays = [NSString stringWithFormat: @"%d", i];
        else
		{
		  wdays = [wdays stringByAppendingFormat: @",%d", i];
		}
      }
    }
	line = [line stringByAppendingFormat: @" %@", wdays];
  }

  line = [line stringByAppendingFormat: @" %@", entry->_command];
  return line;
}

- (MCrontabEntry *) findEntryByComment: (NSString *) comment
{
  BOOL next_is_the_one = NO;

  int i;
  for(i = 0; i < [_lines count]; i++)
  {
	NSString *next = [_lines objectAtIndex: i];
	if([next length] == 0)
	{
		continue;
	}
	if([next characterAtIndex: 0] == '#')
	{
		NSRange range = [next rangeOfString: comment];
		if(range.length != 0) 
		{
			next_is_the_one = YES;
		}
	}
	else if(next_is_the_one)
	{
		MCrontabEntry *entry = [[MCrontabEntry alloc] init];
		if([self parseLine: next toEntry: entry])
		{
			return entry;
		}
		else
		{
			return nil;
		}
	}
  }
  return nil; 
}

- (BOOL) removeCommand: (NSString *) command withComment: (NSString *) comment
{
  BOOL next_is_the_one = NO;
  int i;
  
  for(i = 0; i < [_lines count]; i++)
  {
	NSString *next = [_lines objectAtIndex: i];
	if([next length] == 0)
	{
		continue;
	}
	if([next characterAtIndex: 0] == '#')
	{
		NSRange range = [next rangeOfString: comment];
		if(range.length != 0) 
		{
			next_is_the_one = YES;
		}
	}
	else if(next_is_the_one)
	{
		MCrontabEntry *entry = [[MCrontabEntry alloc] init];
		if([self parseLine: next toEntry: entry])
		{
			if(([command length] == 0) || ([command hasPrefix: command]))
			{
				[_lines removeObjectAtIndex: i-1];
				[_lines removeObjectAtIndex: i-1];
				return YES;
			}
		}
	}
  }
  return NO;
}

- (void) addEntry: (MCrontabEntry *) entry withComment: (NSString *) comment
{
	[_lines addObject: [NSString stringWithFormat: @"# %@", comment]];
	[_lines addObject: [self formatEntry: entry]];
}

- (BOOL) load
{
	char *cmdline[3] = {"/usr/bin/crontab", "-l", NULL};
	char *stdout_text = NULL;
	char *stderr_text = NULL;

	launch_sync(cmdline, NULL, &stdout_text, &stderr_text);
	char *tok = strtok(stdout_text, "\n");
	while (tok)
    {
		[_lines addObject: [NSString stringWithCString: tok]];
		tok = strtok(NULL, "\n");
	}

	if (stdout_text)
		free(stdout_text);
	if (stderr_text)
		free(stderr_text);

	return YES;
}

- (BOOL) installTable
{
	char fname[32];
	int fd;
	BOOL ok = YES;

	strcpy(fname, "/tmp/cron.XXXXXX");
  
	// carefull here, we dont want to allow someone else to write in our
	// tmp file and schedule stuff on our behalf
	fd = mkstemp(fname);

	if (fd < 0)
	{
		NSLog(@"could not create tmp file %s", fname);
		return NO;
	}

	int i;
	for(i = 0; i < [_lines count]; i++)
	{
		NSString *next = [_lines objectAtIndex: i];
		if((write(fd, [next cString], [next cStringLength]) < 0) || (write(fd, "\n", 1) < 0))
		{
			NSLog(@"error writing to tmp file %s: %s", fname, strerror(errno));
			ok = NO;
			break;
		}
	}

	close(fd);
  
	if (ok)
	{
		char *cmdline[3] = {"/usr/bin/crontab", fname, NULL};

		char *so;
		char *se;
		// now install the cron file
		launch_sync(cmdline, NULL, &so, &se);
	}
  
	// delete tmp file
	unlink(fname);

	return ok;
}

- (BOOL) parseLine: (NSString *)line toEntry: (MCrontabEntry *) entry
{
	char *tmp = strdup([line cString]);
	char *tok;
  
	tok = strtok(tmp, " ");
	if (!tok)
		goto error;
	if (*tok == '*')
		entry->_minute = -1;
	else
		entry->_minute = atoi(tok);

	tok = strtok(NULL, " ");
	
	if (!tok)
		goto error;
	if (*tok == '*')
		entry->_hour = -1;
	else
		entry->_hour = atoi(tok);

	tok = strtok(NULL, " ");
	if (!tok)
		goto error;
	if (*tok == '*')
		entry->_day = @"";
	else
		entry->_day = [NSString stringWithCString: tok];

	tok = strtok(NULL, " ");
	if (!tok)
		goto error;
	if (*tok == '*')
		entry->_month = -1;
	else
		entry->_month = atoi(tok);

	tok = strtok(NULL, " ");
	if (!tok)
		goto error;
	if (*tok == '*')
		entry->_weekday= -1;
	else
	{
		char *tt = strdup(tok);
		entry->_weekday = 0;
		char *n = tt;
		char *t = strsep(&n, ",");
		if(t)
		{
			entry->_weekday |= (1 << atoi(t));
		}
		while(t = strsep(&n, ","))
		{
			entry->_weekday |= (1 << atoi(t));
		}
		free(tt);
	}

	tok = strtok(NULL, "\n");
	if (!tok)
		goto error;
	entry->_command = [NSString stringWithCString: tok];

	free(tmp);
	return YES;

error:
	free(tmp);
	return YES;
}

- (NSArray *) lines
{
	return _lines;
}

@end
