//
//  MAXMLGUIController.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Wed Jul 21 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

//XXX make a vertically centered textcell

#import "MAXMLGUIController.h"
#import "maUtils.h"
#import <MySQLToolsCommon/NSView_Extras.h>

#import "MADataFileView.h"
#import "MAFileTextField.h"

#ifndef MAX
#define MAX(a,b) ((a)>(b))?(a):(b)
#endif


@interface MAXMLGUIController(Private)
- (void)buildPage:(MYX_GUI_PAGE*)page
              tab:(NSTabView*)tab;
- (NSView*)buildGroup:(MYX_GUI_GROUP*)group width:(float)width;
- (void)takeValuesFromPage:(MYX_GUI_PAGE*)page;
- (void)updateValuesInPage:(MYX_GUI_PAGE*)page;
- (void)toggleOptionEnabled:(id)sender;
@end

static NSTextField *makeLabel(const char *text)
{
  NSTextField *textf= [[[NSTextField alloc] init] autorelease];
  
  [textf setStringValue:[NSString stringWithUTF8String:text]];
  [textf setEditable:NO];
  [textf setDrawsBackground:NO];
  [textf setBordered:NO];
  [textf sizeToFit];
  
  return textf;
}

static NSView *makeWidget(MYX_GUI_WIDGET *widget)
{
  NSView *wid= nil;
  
  switch (widget->widget_type)
  {
    case MYX_CHECKBOX:
    {
      NSButton *btn= [[[NSButton alloc] init] autorelease];
      [btn setButtonType:NSSwitchButton];
      [btn setTitle:[NSString stringWithUTF8String:(char*)widget->caption]];
      [btn sizeToFit];
      wid= btn;
      break;
    }
    case MYX_TEXTEDIT:
    {      
      if (strcmp2((char*)widget->textedit->edit_type,"innodbfilepath")==0)
      {
        wid= [[MADataFileView alloc] init];
      }
      else if (strcmp2((char*)widget->textedit->edit_type,"directory")==0)
      {
        MAFileTextField *tf= [[[MAFileTextField alloc] initWithFrame:NSMakeRect(0,0,180,22)] autorelease];
        [tf setPickDirectory:YES];
        wid= tf;
      }
      else if (strcmp2((char*)widget->textedit->edit_type,"file")==0)
      {
        MAFileTextField *tf= [[[MAFileTextField alloc] initWithFrame:NSMakeRect(0,0,180,22)] autorelease];
        [tf setPickDirectory:NO];
        wid= tf;
      }
      else
      {
        NSTextField *tf= [[[NSTextField alloc] init] autorelease];
        [tf setFrameSize:NSMakeSize(100.0, 22.0)];
        wid= tf;
      }
      break;
    }
    case MYX_SPINEDIT:
    {
      NSStepper *stepper= [[[NSStepper alloc] init] autorelease];
      NSTextField *text= [[[NSTextField alloc] init] autorelease];
      NSNumberFormatter *fmt= [[[NSNumberFormatter alloc] init] autorelease];
      float swidth;
      [fmt setAllowsFloats:NO];
      [fmt setFormat:@"#0"];
      wid= [[[NSView alloc] init] autorelease];
      [text setAlignment:NSRightTextAlignment];
      [text setFormatter:fmt];
      [stepper setAction:@selector(takeIntValueFrom:)];
      [stepper setTarget:text];
      [text setAction:@selector(takeIntValueFrom:)];
      [text setTarget:stepper];
      [text setFrame:NSMakeRect(0.0, 3.0, 70.0, 22.0)];
      [stepper setFrame:NSMakeRect(75.0, 0.0, 19.0, 27.0)];
      [text setTag:0];
      [stepper setAutorepeat:YES];
      [stepper setTag:1];
      [stepper setMinValue:0];
      [stepper setMaxValue:pow(2,32)-1];
      [stepper setIncrement:1];
      [text setAutoresizingMask:NSViewMaxXMargin];
      [stepper setAutoresizingMask:NSViewMaxXMargin];
      [wid addSubview:text];
      [wid addSubview:stepper];
      swidth= 95.0;

      if (widget->spinedit->unitcontrolbox)
      {
        NSPopUpButton *pop= [[[NSPopUpButton alloc] init] autorelease];
        NSArray *parts= [[NSString stringWithUTF8String:(char*)widget->spinedit->unitcontrolbox] componentsSeparatedByString:@";"];
        unsigned int c;

        [pop addItemWithTitle:@""];
        for (c= 0; c < [parts count]; c++)
          [pop addItemWithTitle:[parts objectAtIndex:c]];
        [pop sizeToFit];
        [pop setAutoresizingMask:NSViewMaxXMargin|NSViewMaxYMargin];
        [pop setFrame:NSMakeRect(swidth, 0, [pop frame].size.width, [pop frame].size.height)];
        [pop setTag:2];
        swidth+= [pop frame].size.width;
        [wid addSubview:pop];
      }
      [wid setFrameSize:NSMakeSize(swidth, 27.0)];
      break;
    }
    case MYX_DROPDOWNBOX:
    {
      NSPopUpButton *pop= [[[NSPopUpButton alloc] init] autorelease];
      unsigned int i;
      
      for (i= 0; i < widget->dropdownbox->items_num; i++)
      {
        char *ptr= strchr((char*)widget->dropdownbox->items[i],'=');
        [pop addItemWithTitle:[NSString stringWithCString:(char*)widget->dropdownbox->items[i] length:ptr-(char*)widget->dropdownbox->items[i]]];
        [[pop lastItem] setRepresentedObject:[NSString stringWithCString:ptr+1]];
      }
      [pop sizeToFit];

      wid= pop;
      break;
    }
  }
  return wid;
}


@implementation MAXMLGUIController(Private)

- (void)toggleOptionEnabled:(id)sender
{
  id opid= [[sender cell] representedObject];
  
  if ([sender state] == NSOnState)
    [[_optionCaptions objectForKey:opid] setTextColor:[NSColor blackColor]];
  else
    [[_optionCaptions objectForKey:opid] setTextColor:[NSColor lightGrayColor]];
  [[_optionControls objectForKey:opid] setEnabledRecursive:[sender state]==NSOnState];
}

- (NSView*)buildGroup:(MYX_GUI_GROUP*)group width:(float)width
{
  NSBox *box= [[[NSBox alloc] init] autorelease];
  float y= 17.0;
  float label_width= 0.0;
  int w;
  NSFont *font= [NSFont systemFontOfSize:[NSFont systemFontSize]];
  NSFont *lfont= [NSFont systemFontOfSize:[NSFont smallSystemFontSize]];
  
  [box setTitle:[NSString stringWithUTF8String:(char*)group->caption?:""]];
  [box setBoxType:NSBoxPrimary];
  [box setTitlePosition:NSAtTop];
  [box setFrame:NSMakeRect(0,0,width,100)];

  // calculate max label width
  for (w= 0; w < (int)group->widgets_num; w++)
  {
    if (group->widgets[w].caption)
    {        
      label_width= MAX(label_width, [font widthOfString:[NSString stringWithUTF8String:(char*)group->widgets[w].caption]]);
      if (group->widgets[w].widget_type == MYX_CHECKBOX)
        label_width /= 2;
    }
  }
  
  // build options
  for (w= group->widgets_num-1; w >= 0; w--)
  {
    NSView *widget= nil;
    NSButton *enableBtn= nil;
    NSTextField *label= nil;
    NSTextField *descr= nil;
    NSRect rect;
    float h= 0.0;
    float dh= 0.0;
    float left_width= 0.0;
    NSString *optionId= [NSString stringWithCString:(char*)group->widgets[w].id];

    if (group->widgets[w].widget_type == MYX_CHECKBOX)
    {
      widget= makeWidget(group->widgets+w);
      [box addSubview:widget];
	  [widget setToolTip:[NSString stringWithFormat:@"Option: %s", group->widgets[w].id]];
      rect= [widget frame];
      rect.origin.x= 17.0-5.0 + 20;
      rect.origin.y= y;
      [widget setAutoresizingMask:NSViewMaxXMargin];
      [widget setFrame:rect];
      h= MAX(h, rect.size.height);
      
      left_width= rect.size.width + 20;
    }
    else
    {
      enableBtn= [[[NSButton alloc] initWithFrame:NSMakeRect(17.0-5.0,y+5.0,16,16)] autorelease];
      [box addSubview:enableBtn];
      [enableBtn setButtonType:NSToggleButton];
      [enableBtn setAutoresizingMask:NSViewMaxXMargin];
      [enableBtn setImage:_disabledImg];
      [enableBtn setAlternateImage:_editableImg];
      [enableBtn setImagePosition:NSImageOnly];
      [[enableBtn cell] setBordered:NO];
      [enableBtn setTarget:self];
      [enableBtn setAction:@selector(toggleOptionEnabled:)];
	  [enableBtn setToolTip:@"Indicates whether the option should be written to the configuration file.\nClick to toggle."];
      [[enableBtn cell] setRepresentedObject:optionId];

      label= makeLabel((char*)group->widgets[w].caption?:"");
      [box addSubview:label];
      [label setAlignment:NSRightTextAlignment];
      rect= [label frame];
      rect.size.width= label_width+5.0;
      rect.origin.x= 17.0-5.0+16;
      rect.origin.y= y+5.0;
      [label setAutoresizingMask:NSViewMaxXMargin];
      [label setFrame:rect];
      h= MAX(h, rect.size.height);
      
      widget= makeWidget(group->widgets+w);
      [box addSubview:widget];
      rect= [widget frame];
      rect.size.width= [widget frame].size.width;
      rect.origin.x= 17.0+16.0+label_width+8.0;
      rect.origin.y= y;
      [widget setAutoresizingMask:NSViewMaxXMargin|NSViewMaxYMargin];
      [widget setFrame:rect];
	  [widget setToolTip:[NSString stringWithFormat:@"Option: %s", group->widgets[w].id]];
      h= MAX(h, rect.size.height);
      
      left_width= label_width + rect.size.width;
    }

    descr= makeLabel((char*)group->widgets[w].description?:"!");
    [box addSubview:descr];
    [descr setFont:lfont];
    
    [[descr cell] setWraps:YES];
    rect.origin.x= 17.0+16.0+8.0+left_width+8.0;
    rect.origin.y= y - (dh-22.0)/2.0;
    
    dh= ceil([[descr font] widthOfString:[descr stringValue]]/(width-rect.origin.x-50));
    if (dh < 2) dh= 2;
    dh*= [lfont defaultLineHeightForFont]+1;

    rect.size.width= width - rect.origin.x - 17.0;
    rect.size.height= dh;
    [descr setAutoresizingMask:NSViewMaxYMargin|NSViewWidthSizable];
    [descr setFrame:rect];
  
    if (rect.size.height > h)
      h= MAX(h, rect.size.height-5);
    else
      h= MAX(h, rect.size.height);
    
    // center the objects vertically according to the total height
    if (enableBtn)
    {
      rect= [enableBtn frame];
      [enableBtn setFrameOrigin:NSMakePoint(rect.origin.x, y + (h - rect.size.height)/2)];
    }
    if (label)
    {
      rect= [label frame];
      [label setFrameOrigin:NSMakePoint(rect.origin.x, y + (h - rect.size.height)/2)];
    }    
    if (widget)
    {
      rect= [widget frame];
      [widget setFrameOrigin:NSMakePoint(rect.origin.x, y + (h - rect.size.height)/2)];
    }
    if (descr)
    {
      rect= [descr frame];
      [descr setFrameOrigin:NSMakePoint(rect.origin.x, y + (h - rect.size.height)/2)];
    }
    
    // register the control
    [_optionControls setObject:widget forKey:optionId];
    if (enableBtn)
      [_optionToggles setObject:enableBtn forKey:optionId];
    if (label)
      [_optionCaptions setObject:label forKey:optionId];
    
    y+= h+17.0;
  }
  
  [box setFrameSize:NSMakeSize(width, y + 17.0 + 5.0)];
  
  return box;
}

- (void)buildPage:(MYX_GUI_PAGE*)page
              tab:(NSTabView*)tab
{
  NSTabViewItem *item= [[NSTabViewItem alloc] initWithIdentifier:[NSValue valueWithPointer:page]];
  NSScrollView *sview= [[NSScrollView alloc] init];
  NSView *topv= [[NSView alloc] init];
  NSView *pagev= [item view];
  NSTextField *textf;
  float width;
  float y;
  NSSize tsize= [tab frame].size;
  int i;

  [pagev setFrameSize:tsize];
  [pagev addSubview:sview];  
  [sview setDrawsBackground:NO];
  [sview setHasHorizontalScroller:NO];
  [sview setHasVerticalScroller:YES];
  [sview setBorderType:NSGrooveBorder];
  [sview setFrame:NSMakeRect(5.0, 17.0, tsize.width-5.0*2, tsize.height-17.0*2-10.0)];
  [sview setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable|NSViewMinYMargin];  
  [sview setDocumentView:topv];

  // layout from bottom to top, since the coordinate system is flipped
  width= [sview contentSize].width;
  [topv setAutoresizingMask:NSViewWidthSizable];
  [topv setFrameSize:NSMakeSize(width, 100)];
  y= 10.0;
  // process groups
  for (i= page->groups_num-1; i >= 0; --i)
  {
    NSView *groupv= [self buildGroup:page->groups+i width:width];
    float height= [groupv frame].size.height;
    [groupv setAutoresizingMask:NSViewWidthSizable];
    [groupv setFrame:NSMakeRect(15.0, y, width-15.0*2, height)];
    [topv addSubview:groupv];
    y+= height + 10.0;
  }
  if (y < [sview contentSize].height)
  {
    NSArray *children= [topv subviews];
    float svheight= [sview contentSize].height;
    float yy= svheight - 10.0;
    for (i= 0; i < (int)[children count]; i++)
    {
      NSView *ch= [children objectAtIndex:i];
      [ch setAutoresizingMask:NSViewWidthSizable|NSViewMaxYMargin];
      yy-= [ch frame].size.height;
      [ch setFrameOrigin:NSMakePoint(15.0, yy)];
      yy-= 10.0;
    }
    [topv setFrameSize:NSMakeSize(width, svheight)];
  }
  else
  {
    [topv setFrameSize:NSMakeSize(width, y)];
  }
  [topv release];
  
  [topv scrollPoint:NSMakePoint(0,y)];

  // the description
  textf= makeLabel((char*)page->description);
  [pagev addSubview:textf];
  [textf setAlignment:NSCenterTextAlignment];
  [textf setFrame:NSMakeRect(17.0,tsize.height-4.0-[textf frame].size.height,
                             [pagev frame].size.width-17.0*2, [textf frame].size.height)];
  [textf setAutoresizingMask:NSViewMinYMargin|NSViewWidthSizable];
  
  [item setLabel:[NSString stringWithUTF8String:(char*)page->caption]];
    
  [tab addTabViewItem:item];
  [item release];

  [topv scrollPoint:NSMakePoint(0,y)];
  
  [sview setNeedsDisplay:YES];
}


- (void)takeValuesFromPage:(MYX_GUI_PAGE*)page
{
  unsigned int g, w;
  
  for (g= 0; g < page->groups_num; g++)
  {
    MYX_GUI_GROUP *group= page->groups+g;
    for (w= 0; w < group->widgets_num; w++)
    {
      MYX_GUI_WIDGET *widget= group->widgets+w;
      NSString *optionId= [NSString stringWithUTF8String:(char*)widget->id];
      id view= [_optionControls objectForKey:optionId];
      char *value= (char*)widget->value;
      if (!value) value= (char*)widget->default_value;
      if (!view)
      {
        NSLog(@"view for option %s not found", widget->id);
        continue;
      }

      [[_optionToggles objectForKey:optionId] setState:!widget->active?NSOnState:NSOffState];
      [[_optionToggles objectForKey:optionId] performClick:self];

      switch (widget->widget_type)
      {
        case MYX_CHECKBOX:
        {
          BOOL v= NO;
          if (!value || strcasecmp(value, "Unchecked")==0)
            v= NO;
          else
            v= YES;
          if (widget->checkbox->invert)
            v= !v;
          [view setState:v?NSOnState:NSOffState];
          break;
        }
        case MYX_TEXTEDIT:
        {
          NSString *str= [NSString stringWithUTF8String:value?:""];
          if (strcmp2((char*)widget->textedit->edit_type,"innodbfilepath")==0)
          {
            [view setStringValue:str];
          }
          else if (strcmp2((char*)widget->textedit->edit_type,"directory")==0)
          {
            [(MAFileTextField*)view setPath:str];
          }
          else if (strcmp2((char*)widget->textedit->edit_type,"file")==0)
          {
            [(MAFileTextField*)view setPath:str];
          }
          else
          {
            [view setStringValue:str];
          }
          break;
        }
        case MYX_SPINEDIT:
        {
          NSString *unit= @"";
          NSPopUpButton *pop= [view viewWithTag:2];
          if (value && !isdigit(value[strlen(value)-1]))
          {
            switch (value[strlen(value)-1])
            {
              case 'k': unit= @"k"; break;
              case 'M': unit= @"M"; break;
              case 'G': unit= @"G"; break;
            }
          }
          [[view viewWithTag:1] setIntValue:strtoll(value?:"0", NULL, 0)];
          [[view viewWithTag:0] setIntValue:strtoll(value?:"0", NULL, 0)];
          if (pop)
          {
            if ([pop indexOfItemWithTitle:unit]<0)
              [pop addItemWithTitle:unit];
            [pop selectItemWithTitle:unit];
          }
          break;
        }
        case MYX_DROPDOWNBOX:
        {
          int index= [view indexOfItemWithRepresentedObject:[NSString stringWithCString:value?:""]];
          if (index < 0)
          {
            NSLog(@"Value %s is not in the dropdown list for %s", value, widget->id);
          }
          else
            [view selectItemAtIndex:index];
          break;
        }
      }
    }
  }
}

- (void)updateValuesInPage:(MYX_GUI_PAGE*)page
{
  unsigned int g, w;
  
  for (g= 0; g < page->groups_num; g++)
  {
    MYX_GUI_GROUP *group= page->groups+g;
    for (w= 0; w < group->widgets_num; w++)
    {
      MYX_GUI_WIDGET *widget= group->widgets+w;
      NSString *optionId= [NSString stringWithUTF8String:(char*)widget->id];
      id view= [_optionControls objectForKey:optionId];
      if (!view)
      {
        NSLog(@"view for option %s not found", widget->id);
        continue;
      }

      if (widget->widget_type == MYX_CHECKBOX)
        widget->active= 1;
      else
        widget->active= [[_optionToggles objectForKey:optionId] state] == NSOnState;
      [view setEnabledRecursive:widget->active];
      
      switch (widget->widget_type)
      {
        case MYX_CHECKBOX:
        {
          BOOL v= [view state]==NSOnState;
          if (widget->checkbox->invert)
            v= !v;
          g_free(widget->value);
          widget->value= (unsigned char*)g_strdup(v?"Checked":"Unchecked");
          break;
        }
        case MYX_TEXTEDIT:
        {
          if (strcmp2((char*)widget->textedit->edit_type,"innodbfilepath")==0)
          {
            g_free(widget->value);
            widget->value= (unsigned char*)g_strdup([[view stringValue] UTF8String]);
          }
          else if (strcmp2((char*)widget->textedit->edit_type,"directory")==0)
          {
            g_free(widget->value);
            widget->value= (unsigned char*)g_strdup([[(MAFileTextField*)view path] UTF8String]);
          }
          else if (strcmp2((char*)widget->textedit->edit_type,"file")==0)
          {
            g_free(widget->value);
            widget->value= (unsigned char*)g_strdup([[(MAFileTextField*)view path] UTF8String]);
          }
          else
          {
            g_free(widget->value);
            widget->value= (unsigned char*)g_strdup([[view stringValue] UTF8String]);
          }
          break;
        }
        case MYX_SPINEDIT:
        {
          NSPopUpButton *pop= [view viewWithTag:2];
          
          NSString *str= [NSString stringWithFormat:@"%@%@", [[view viewWithTag:0] stringValue], 
						[[pop selectedItem] title]?:@""];
          
          g_free(widget->value);
          widget->value= (unsigned char*)g_strdup([str UTF8String]);
          break;
        }
        case MYX_DROPDOWNBOX:
        {
          g_free(widget->value);
          widget->value= (unsigned char*)g_strdup([[[view selectedItem] representedObject] UTF8String]);
          break;
        }
      }
    }
  }
}

@end



@implementation MAXMLGUIController

- (void)dealloc
{
  [_optionControls release];
  [_optionCaptions release];
  [_optionToggles release];
  if (_descr)
    myx_free_gui_description(_descr);
  [_editableImg release];
  [_disabledImg release];
  [_file release];
  [_section release];
  [super dealloc];
}

- (MAXMLGUIController*)initInTabView:(NSTabView*)tab
                               mysql:(MYSQL*)mysql
                             version:(const char*)version
                                file:(NSString*)file
                             section:(NSString*)section
{
  self= [super init];
  if (self)
  {
    MYX_ADMIN_LIB_ERROR err;
    unsigned int i;
    
    _file= [file retain];
    _section= [section retain];
    
    _editableImg= [[NSImage imageNamed:@"variable_editable_mac.png"] retain];

    _disabledImg= [[NSImage imageNamed:@"variable_disabled_mac.png"] retain];
        
    _optionToggles= [[NSMutableDictionary alloc] init];
    _optionControls= [[NSMutableDictionary alloc] init];
    _optionCaptions= [[NSMutableDictionary alloc] init];
    _descr= myx_get_gui_description([[[NSBundle mainBundle] pathForResource:@"mysqladmin_startup_variables_description"
                                                                    ofType:@"xml"] UTF8String],
                                    version, MYX_MACOS, &err,
                                    [file UTF8String],
                                    [section isEqualToString:@""] ? NULL : [section UTF8String]);
    if (!_descr)
    {
      MARunAlertPanelWithError(nil,@"Could not open GUI description file.",err);
      [self release];
      return nil;
    }
  
    for (i= 0; i < _descr->pages_num; i++)
    {
      [self buildPage:_descr->pages+i tab:tab];
      [self takeValuesFromPage:_descr->pages+i];
    }
  }
  return self;
}

- (void)revertToSaved
{
  unsigned int i;
  for (i= 0; i < _descr->pages_num; i++)
  {
    [self takeValuesFromPage:_descr->pages+i];
  }
}


- (BOOL)saveTo:(int)fd
{
  MYX_ADMIN_LIB_ERROR rc;
  unsigned int i;
  FILE *file;
  
  // fetch values  
  for (i= 0; i < _descr->pages_num; i++)
  {
    [self updateValuesInPage:_descr->pages+i];
  }
  
  file= fdopen(fd, "w");
  if (!file)
  {
    NSRunAlertPanel(nil, [NSString stringWithFormat:@"Error updating configuration file:%s.", strerror(errno)],
                  @"OK", nil, nil);
    return NO;
  }
  
  rc= myx_update_mysql_cnf_filef(_descr, [_file fileSystemRepresentation],
								 file, [_section UTF8String]);
  fclose(file);
  if (rc != MYX_ADMIN_NO_ERROR)
  {
    MARunAlertPanelWithError(@"Error", @"Error updating configuration file.", rc);
	return NO;
  }

  return YES;
}

@end
