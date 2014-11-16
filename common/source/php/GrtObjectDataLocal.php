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
 * GrtObject local data class
 * 
 * @author MikeZ
 * @version 1.0, 02/26/05
 */
class com_mysql_grt_GrtObjectDataLocal implements com_mysql_grt_GrtObjectData {
	
	protected $grtObject;
	
	protected $name;

	protected $_id;

	protected $owner;
	
	public function __construct(com_mysql_grt_GrtObject $grtObject) {
		$this->grtObject = $grtObject;
		
		$this->_id = md5(uniqid(rand()/*, true*/));
	}
	
	public function getName() {
		return $this->name;
	}
	
	public function setName($name) {
		$this->name = $name;
		
		return $this->getName();
	}
	
	public function get_id() {
		return $this->_id;
	}
	
	public function set_id($_id) {
		// remove old _id
		Grt::getInstance()->removeFromObjectCache($this->grtObject);

		$this->_id = $_id;

		Grt::getInstance()->addToObjectCache($this->grtObject);
			
		return $this->get_id();
	}

	public function getOwner() {
		return Grt::getInstance()->getObjectByRefId($this->owner);
	}
	
	public function setOwner($owner) { 
		if ($owner != null)
			$this->owner = $owner->get_id();

		return $this->getOwner();
	}
	
	public function getOwnerById() {
		return $this->owner;
	}
	
	public function setOwnerById($owner_id) {
		$this->owner = $owner_id; 
	}
}
?>