/* Copyright (C) 2005 MySQL AB

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */

#include "MWBCanvas.h"
/**
 * @file  MWBCanvas.cc
 * @brief 
 */



MWBCanvas::MWBCanvas(MGRT *grt)
  : _grt(grt)
{
}


void MWBCanvas::set_marker(int markerIndex)
{
  char markerName[32];
  MYX_GRT_VALUE *marker, *markerList;

  sprintf(markerName, "Marker%i", markerIndex);

  marker= myx_grt_dict_new_obj(_grt->grt(), "model.Marker", markerName, NULL, NULL);

  myx_grt_dict_item_set_value_from_string(marker, "view",
                                          myx_grt_value_as_string(_grt->global_value("/workbench/model/currentView")));
  myx_grt_dict_item_set_value_from_real(marker, "zoom", _zoom);
  myx_grt_dict_item_set_value_from_int(marker, "x", (int)_offset.x);
  myx_grt_dict_item_set_value_from_int(marker, "y", (int)_offset.y);


  markerList= _grt->global_value("/workbench/model/markers");

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


void MWBCanvas::go_to_marker(int markerIndex)
{
  char markerName[32];
  MYX_GRT_VALUE *marker, *markerList;

  sprintf(markerName, "Marker%i", markerIndex);

  markerList= _grt->global_value("/workbench/model/markers");
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

    set_zoom(myx_grt_dict_item_get_as_real(marker, "zoom"), 
             get_center_for_zoom());
    set_offset(Point(myx_grt_dict_item_get_as_int(marker, "x"),
                     myx_grt_dict_item_get_as_int(marker, "y")));
  }
}


bool MWBCanvas::has_marker(int marker)
{
  char markerName[32];
  MYX_GRT_VALUE *markerList;

  sprintf(markerName, "Marker%i", marker);

  markerList= _grt->global_value("/workbench/model/markers");

  for (int i= myx_grt_list_item_count(markerList)-1; i>=0; i--)
  {
    MYX_GRT_VALUE *item= myx_grt_list_item_get(markerList, i);
    if (strcmp2(myx_grt_dict_item_get_as_string(item, "name"), markerName)==0)
    {
      return true;
    }
  }
  return false;  
}
