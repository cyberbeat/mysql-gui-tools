//
//  MADataFileEditor.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on 1/1/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MADataFileEditor.h"
#include <sys/param.h>
#include <sys/mount.h>

@implementation MADataFileEditor


static NSString *formatSize(long bsize, long blocks)
{
  if (blocks > 1024*1024*1024/bsize)
    return [NSString stringWithFormat:@"%liGB", (bsize*(blocks/1024))/(1024*1024)];
  else if (blocks > 1024*1024/bsize)
    return [NSString stringWithFormat:@"%liMB", (bsize*(blocks/1024))/1024];
  else
    return [NSString stringWithFormat:@"%liB", bsize*blocks];
}


static VolumeInfo *detectFileSystems(int *count)
{
  NSFileManager *fm= [NSFileManager defaultManager];
  NSArray *array;
  unsigned int i;
  VolumeInfo *volumes;

  array= [fm directoryContentsAtPath:@"/Volumes"];
  volumes= malloc(sizeof(VolumeInfo)*[array count]);
  *count= 0;
  for (i= 0; i < [array count]; i++)
  {
    NSString *path;
    struct statfs st;

    path= [fm pathContentOfSymbolicLinkAtPath:
      [NSString stringWithFormat:@"/Volumes/%@",[array objectAtIndex:i]]];

    if (statfs([path cString], &st) < 0)
      continue;
    else
    {
      volumes[*count].name= [[array objectAtIndex:i] retain];
      volumes[*count].path= [path retain];
      volumes[*count].total= [formatSize(st.f_bsize, st.f_blocks) retain];
      volumes[*count].avail= [formatSize(st.f_bsize, st.f_bavail) retain];
      volumes[*count].type= [[NSString stringWithCString:st.f_fstypename] retain];
      (*count)++;
    }
  }
  return volumes;
}


- (id)init
{
  self= [super init];
  if (self)
  {
    [NSBundle loadNibNamed:@"DataFileEditor" owner:self];
  }
  return self;
}


- (void)awakeFromNib
{
  volumes= detectFileSystems(&volumeCount);
  [fstable reloadData];
}


- (void)dealloc
{
  if (volumes)
  {
    int i;
    for (i= 0; i < volumeCount; i++)
    {
      [volumes[i].name release];
      [volumes[i].path release];
      [volumes[i].total release];
      [volumes[i].avail release];
      [volumes[i].type release];
    }
    free(volumes);
  }
  [super dealloc];
}


- (IBAction)chooseFile:(id)sender
{
  NSSavePanel *panel= [NSSavePanel savePanel];
  
  [panel setTitle: @"Set Data File Name"];
  [panel setPrompt: @"Set"];
  [panel setNameFieldLabel: @"Data File Name"];
  if ([panel runModal] == NSFileHandlingPanelOKButton)
  {
    [filename setStringValue: [panel filename]];
  }
}


- (IBAction)okClicked:(id)sender
{
  [NSApp stopModalWithCode:NSOKButton];
  [self close];
}


- (IBAction)cancelClicked:(id)sender
{
  [NSApp stopModalWithCode:NSCancelButton];
  [self close];
}


- (BOOL)windowShouldClose:(id)sender
{
  [NSApp stopModalWithCode:NSCancelButton];
  return YES;
}


- (NSString*)stringValue
{
  NSString *fn= [filename stringValue];
  NSString *sz= [size stringValue];
  NSString *unit= [[sizeUnit selectedItem] title];
  
  return [NSString stringWithFormat:@"%@:%@%c",fn,sz,*[unit cString]];
}


- (void)setStringValue:(NSString*)value
{
  NSArray *parts= [value componentsSeparatedByString:@":"];
  char *s= strdup([[parts objectAtIndex:1] cString]);
  
  [filename setStringValue:[parts objectAtIndex:0]];
  
  if (strlen(s)>0)
  {    
    switch (s[strlen(s)-1])
    {
      case 'K':
        s[strlen(s)-1]= 0;
        [size setStringValue:[NSString stringWithCString:s]];
        [sizeUnit selectItemAtIndex:1];
        break;
      case 'M':
        s[strlen(s)-1]= 0;
        [size setStringValue:[NSString stringWithCString:s]];
        [sizeUnit selectItemAtIndex:2];
        break;
      case 'G':
        s[strlen(s)-1]= 0;
        [size setStringValue:[NSString stringWithFormat:@"%ul",strtoul(s, NULL, 0)*1024]];
        [sizeUnit selectItemAtIndex:2];
        break;
      default:
        [size setStringValue:[NSString stringWithCString:s]];
        [sizeUnit selectItemAtIndex:0];
        break;
    }
  }
  else
  {
    [size setStringValue:@"0"];
    [sizeUnit selectItemAtIndex:0];
  }
  free(s);
}


- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  return volumeCount;
}


- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(int)rowIndex
{
  if ([[aTableColumn identifier] isEqualToString:@"volume"])
    return volumes[rowIndex].name;
  else if ([[aTableColumn identifier] isEqualToString:@"path"])
    return volumes[rowIndex].path;
  else if ([[aTableColumn identifier] isEqualToString:@"total"])
    return volumes[rowIndex].total;
  else if ([[aTableColumn identifier] isEqualToString:@"free"])
    return volumes[rowIndex].avail;
  else if ([[aTableColumn identifier] isEqualToString:@"path"])
    return volumes[rowIndex].path;
  else
    return volumes[rowIndex].type;
}

@end
