<?php
/*
 Generic Runtime Library (GRT)
 Copyright (C) 2005 MySQL AB
 
 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA 
 */

/**
 * Grt callback interface
 * 
 * @author MikeZ
 * @version 1.0, 02/26/05
 */
interface GrtCallback {
	public function setApplicationPath($applicationPath);
	
	public function callGrtFunction($myx_grt_pointer, $module,
			$functionName, $arguments);
	
	public function getGrtGlobalById($myx_grt_pointer, 
			$id);
	
	public function getGrtGlobalAsObject($myx_grt_pointer, 
			$objectPath);
	
	public function getGrtGlobalAsString($myx_grt_pointer, 
			$objectPath);
	
	public function getGrtGlobalAsInt($myx_grt_pointer, 
			$objectPath);
	
	public function getGrtGlobalAsReal($myx_grt_pointer, 
			$objectPath);
	
	public function setGrtGlobalFromObject($myx_grt_pointer, 
			$objectPath, $value);

	public function setGrtGlobalFromString($myx_grt_pointer, 
			$objectPath, $value);
	
	public function setGrtGlobalFromInt($myx_grt_pointer, 
			$objectPath, $value);
	
	public function setGrtGlobalFromReal($myx_grt_pointer, 
			$objectPath, $value);
	
	public function getGrtGlobalListSize($myx_grt_pointer, 
			$objectPath);

	public function getGrtGlobalListItem($myx_grt_pointer, 
			$objectPath, $index);
	
	public function addGrtGlobalListItem($myx_grt_pointer, 
			$objectPath, $item);
	
	public function removeGrtGlobalListItem($myx_grt_pointer, 
			$objectPath, $index);
	
	public function getGrtGlobalDictItem($myx_grt_pointer, 
			$objectPath, $key);
	
	public function addGrtGlobalDictItem($myx_grt_pointer, 
			$objectPath, $key, $value);
	
	public function removeGrtGlobalDictItem($myx_grt_pointer, 
			$objectPath, $key);
			
	public function processMessages($myx_grt_pointer, $msgs);
	
  public function processQueryStatus($myx_grt_pointer);
}
?>