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
 * @version 1.0, 02/27/05
 */
class GrtCallbackTest implements GrtCallback {
	
	
	public function setApplicationPath($applicationPath) {
	}
	
	public function callGrtFunction($myx_grt_pointer, $module,
			$functionName, $arguments) {
		return null;
	}
	
	public function getGrtGlobalById($myx_grt_pointer, $id) {
		if ($id == "123456789")
			return $this->getGrtGlobalAsObject($myx_grt_pointer, "/testObject");
		else if ($id == "235205293502983423")
			return $this->getGrtGlobalAsObject($myx_grt_pointer, "/simpleDatatype");
		else if ($id == "248468926343234234")
			return $this->getGrtGlobalAsObject($myx_grt_pointer, "/column");
		else
			return null;
	}
	
	private function prepareGrtXml($obj) {
		return Grt::prepareGrtXml(Grt::getObjectAsXml($obj));
	}
	
	public function getGrtGlobalAsObject($myx_grt_pointer, 
			$objectPath) {
		// A GrtObject
		if ($objectPath == "/testObject")
			return new com_mysql_grt_GrtObject("/testObject");
		else if ($objectPath == "/testObject/name")
			return "TestObject";
		else if ($objectPath == "/testObject/_id")
			return "123456789";
		// A SimpleDatatype
		else if ($objectPath == "/simpleDatatype")
			return new com_mysql_grt_db_SimpleDatatype("/simpleDatatype");
		else if ($objectPath == "/simpleDatatype/name")
			return "VARCHAR";
		else if ($objectPath == "/simpleDatatype/_id")
			return "235205293502983423";
		else if ($objectPath == "/simpleDatatype/characterMaximumLength")
			return 255;
		else if ($objectPath == "/simpleDatatype/characterOctetLength")
			return 0;
		else if ($objectPath == "/simpleDatatype/numericPrecision")
			return 0;
		else if ($objectPath == "/simpleDatatype/numericPrecisionRadix")
			return 0;
		else if ($objectPath == "/simpleDatatype/numericScale")
			return 0;
		else if ($objectPath == "/simpleDatatype/dateTimePrecision")
			return 0;
		// A Column
		else if ($objectPath == "/column")
			return new com_mysql_grt_db_Column("/column");
		else if ($objectPath == "/column/name")
			return "TestColumn";
		else if ($objectPath == "/column/_id")
			return "248468926343234234";
		else if ($objectPath == "/column/precision")
			return 0;
		else if ($objectPath == "/column/scale")
			return 0;
		else if ($objectPath == "/column/isNullable")
			return 1;
		else if ($objectPath == "/column/length")
			return 45;
		else if ($objectPath == "/column/datatypeName")
			return "VARCHAR";
		else if ($objectPath == "/column/defaultValue")
			return "undefined";
		else if ($objectPath == "/column/characterSetName")
			return "UTF8";
		else if ($objectPath == "/column/collationName")
			return "UTF8_general_ci";
		else if ($objectPath == "/column/simpleType")
			return "235205293502983423";
		else
			return null;

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
				
	}

	public function setGrtGlobalFromString($myx_grt_pointer, 
			$objectPath, $value) {
				
	}
	
	public function setGrtGlobalFromInt($myx_grt_pointer, 
			$objectPath, $value) {
				
	}
	
	public function setGrtGlobalFromReal($myx_grt_pointer, 
			$objectPath, $value) {
				
	}
	
	public function getGrtGlobalListSize($myx_grt_pointer, 
			$objectPath) {
		return 0;
	}

	public function getGrtGlobalListItem($myx_grt_pointer, 
			$objectPath, $index) {
		return null;
	}
	
	public function addGrtGlobalListItem($myx_grt_pointer, 
			$objectPath, $item) {
				
	}
	
	public function removeGrtGlobalListItem($myx_grt_pointer, 
			$objectPath, $index) {
				
	}
	
	public function getGrtGlobalDictItem($myx_grt_pointer, 
			$objectPath, $key) {
		return null;
	}
	
	public function addGrtGlobalDictItem($myx_grt_pointer, 
			$objectPath, $key, $value) {
				
	}
	
	public function removeGrtGlobalDictItem($myx_grt_pointer, 
			$objectPath, $key) {
				
	}

	public function processMessages($myx_grt_pointer, $msgs) {
  
	}
  
	public function processQueryStatus($myx_grt_pointer) {
    return 0;
	}
}
?>