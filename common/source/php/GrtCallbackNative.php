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
 * Grt native callback class
 * 
 * @author MikeZ
 * @version 1.0, 02/26/05
 */
class GrtCallbackNative implements GrtCallback {
	
	private $applicationPath;
	
	
	public function setApplicationPath($applicationPath) {
		$this->applicationPath = $applicationPath;
	}
	
	public function callGrtFunction($myx_grt_pointer, $module,
			$functionName, $arguments) {
		return grtCallGrtFunction($myx_grt_pointer, $module,
			$functionName, $arguments);
	}
	
	public function getGrtGlobalById($myx_grt_pointer, $id) {
		global $com_mysql_grt_global_object;
		
		grtGetGrtGlobalById($myx_grt_pointer, $id);
		
		return $com_mysql_grt_global_object;
	}
	
	public function getGrtGlobalAsObject($myx_grt_pointer, 
			$objectPath) {
		global $com_mysql_grt_global_object;
		
		getGrtGlobalAsObject($myx_grt_pointer, $objectPath);
		
		return $com_mysql_grt_global_object;
	}
	
	public function getGrtGlobalAsString($myx_grt_pointer, 
			$objectPath) {
		return $this->getGrtGlobalAsObject($myx_grt_pointer, $objectPath);
	}
	
	public function getGrtGlobalAsInt($myx_grt_pointer, 
			$objectPath) {
		return $this->getGrtGlobalAsObject($myx_grt_pointer, $objectPath);
	}
	
	public function getGrtGlobalAsReal($myx_grt_pointer, 
			$objectPath) {
		return $this->getGrtGlobalAsObject($myx_grt_pointer, $objectPath);
	}
	
	public function setGrtGlobalFromObject($myx_grt_pointer, 
			$objectPath, $value) {
		global $com_mysql_grt_global_object;
		
		$com_mysql_grt_global_object = $value;
		
		setGrtGlobalFromObject($myx_grt_pointer, $objectPath);
	}

	public function setGrtGlobalFromString($myx_grt_pointer, 
			$objectPath, $value) {
		$this->setGrtGlobalFromObject($myx_grt_pointer, $objectPath, $value);
	}
	
	public function setGrtGlobalFromInt($myx_grt_pointer, 
			$objectPath, $value) {
		$this->setGrtGlobalFromObject($myx_grt_pointer, $objectPath, $value);
	}
	
	public function setGrtGlobalFromReal($myx_grt_pointer, 
			$objectPath, $value) {
		$this->setGrtGlobalFromObject($myx_grt_pointer, $objectPath, $value);
	}
	
	public function getGrtGlobalListSize($myx_grt_pointer, 
			$objectPath) {
		return getGrtGlobalListSize($myx_grt_pointer, $objectPath);
	}

	public function getGrtGlobalListItem($myx_grt_pointer, 
			$objectPath, $index) {
		global $com_mysql_grt_global_object;
		
		getGrtGlobalListItem($myx_grt_pointer, $objectPath, $index);
		
		return $com_mysql_grt_global_object;
	}
	
	public function addGrtGlobalListItem($myx_grt_pointer, 
			$objectPath, $item) {
		global $com_mysql_grt_global_object;
		
		$com_mysql_grt_global_object = $item;
		
		addGrtGlobalListItem($myx_grt_pointer, $objectPath);
	}
	
	public function removeGrtGlobalListItem($myx_grt_pointer, 
			$objectPath, $index) {
		removeGrtGlobalListItem($myx_grt_pointer, $objectPath, $index);
	}
	
	public function getGrtGlobalDictItem($myx_grt_pointer, 
			$objectPath, $key) {
		global $com_mysql_grt_global_object;
		
		getGrtGlobalDictItem($myx_grt_pointer, $objectPath, $key);
		
		return $com_mysql_grt_global_object;
	}
	
	public function addGrtGlobalDictItem($myx_grt_pointer, 
			$objectPath, $key, $value) {
		global $com_mysql_grt_global_object;
		
		$com_mysql_grt_global_object = $value;
		
		addGrtGlobalDictItem($myx_grt_pointer, $objectPath, $key);
	}
	
	public function removeGrtGlobalDictItem($myx_grt_pointer, 
			$objectPath, $key) {
		removeGrtGlobalDictItem($myx_grt_pointer, $objectPath, $key);	
	}

	public function processMessages($myx_grt_pointer, $msgs) {
		processMessages($myx_grt_pointer, $msgs);
	}
  
  public function processQueryStatus($myx_grt_pointer) {
    return processQueryStatus($myx_grt_pointer);
  }
}
?>