//
//  MWCanvasView.m
//  WBCanvas
//
//  Created by Alfredo Kojima on 05/9/8.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MWCanvasView.h"
#import <MySQLGRT/MGRT.h>

@implementation MWCanvasView


- (void)dealloc
{
  [_grt release];
  [super dealloc];
}


- (void)setMGRT:(MGRT*)grt
{
  if (_grt != grt)
  {
    [_grt release];
    _grt= [grt retain];
  }
}


- (void)setMarker:(id)sender;
{
  char markerName[32];
  MYX_GRT_VALUE *marker, *markerList;
  int index= [sender tag];
  
  sprintf(markerName, "Marker%i", index);
  
  marker= myx_grt_dict_new_obj([_grt grt], "model.Marker", markerName, NULL, NULL);
  
  myx_grt_dict_item_set_value_from_string(marker, "view", 
                                          myx_grt_value_as_string([_grt globalValue:"/workbench/model/currentView"]));
  myx_grt_dict_item_set_value_from_real(marker, "zoom", _zoom);
  myx_grt_dict_item_set_value_from_int(marker, "x", (int)_offset.x);
  myx_grt_dict_item_set_value_from_int(marker, "y", (int)_offset.y);

  
  markerList= [_grt globalValue:"/workbench/model/markers"];
  
  for (int i= myx_grt_list_item_count(markerList)-1; i>=0; i--)
  {
    MYX_GRT_VALUE *item= myx_grt_list_item_get(markerList, i);
    if (strcmp2(myx_grt_dict_item_get_as_string(item, "name"), markerName)==0)
    {
      myx_grt_list_item_del(markerList, i);
      break;
    }
  }
  
  myx_grt_list_item_add(markerList, marker);  
}


- (BOOL)hasMarker:(int)index
{
  char markerName[32];
  MYX_GRT_VALUE *markerList;
  
  sprintf(markerName, "Marker%i", index);

  markerList= [_grt globalValue:"/workbench/model/markers"];
  
  for (int i= myx_grt_list_item_count(markerList)-1; i>=0; i--)
  {
    MYX_GRT_VALUE *item= myx_grt_list_item_get(markerList, i);
    if (strcmp2(myx_grt_dict_item_get_as_string(item, "name"), markerName)==0)
    {
      return YES;
    }
  }
  return NO;
}


- (void)goToMarker:(id)sender
{
  char markerName[32];
  MYX_GRT_VALUE *marker, *markerList;
  int index= [sender tag];
  
  sprintf(markerName, "Marker%i", index);
  
  markerList= [_grt globalValue:"/workbench/model/markers"];
  marker= myx_grt_list_item_get_by_object_name(markerList, markerName);
  
  if (marker)
  {
    /*
  // select tabsheet
     {if (Grt.GlobalAsString['/workbench/model/currentView'] <>
          Grt.DictString[Marker, 'view']) then
            Grt.GlobalAsString['/workbench/model/currentView'] :=
            Grt.DictString[Marker, 'view'];}
     */
  
    [self setZoom:myx_grt_dict_item_get_as_real(marker, "zoom")];
    [self setOffset:NSMakePoint(myx_grt_dict_item_get_as_int(marker, "x"),
                                myx_grt_dict_item_get_as_int(marker, "y"))];    
  }
}


@end
