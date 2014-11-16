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
 * GrtObject global data class
 * 
 * @author MikeZ
 * @version 1.0, 02/26/05
 */
class com_mysql_grt_GrtObjectDataGlobal implements com_mysql_grt_GrtObjectData {

	protected $globalObjectPath;
	
	public function __construct($globalObjectPath) {
		$this->globalObjectPath = $globalObjectPath;
	}
	
	protected function getGlobalObjectPath() {
		return $this->globalObjectPath;
	}

	protected function getGrtGlobalAsString($memberName) {
		return Grt::getInstance()->getGrtGlobalAsString(
				$this->getGlobalObjectPath() . "/" . $memberName);
	}

	protected function getGrtGlobalAsInt($memberName) {
		return Grt::getInstance()->getGrtGlobalAsInt(
				$this->getGlobalObjectPath() . "/" . $memberName);
	}

	protected function getGrtGlobalAsReal($memberName) {
		return Grt::getInstance()->getGrtGlobalAsReal(
				$this->getGlobalObjectPath() . "/" . $memberName);
	}

	protected function getGrtGlobalAsObject($memberName) {
		return Grt::getInstance()->getGrtGlobalAsObject(
				$this->getGlobalObjectPath() . "/" . $memberName);
	}

	protected function setGrtGlobalFromString($memberName, $value) {
		Grt::getInstance()->setGrtGlobalFromString(
				$this->getGlobalObjectPath() . "/" . $memberName, $value);
	}

	protected function setGrtGlobalFromInt($memberName, $value) {
		Grt::getInstance()->setGrtGlobalFromInt(
				$this->getGlobalObjectPath() . "/" . $memberName, $value);
	}

	protected function setGrtGlobalFromReal($memberName, $value) {
		Grt::getInstance()->setGrtGlobalFromReal(
				$this->getGlobalObjectPath() . "/" . $memberName, $value);
	}

	protected function setGrtGlobalFromObject($memberName, $value) {
		Grt::getInstance()->setGrtGlobalFromObject(
				$this->getGlobalObjectPath() . "/" . $memberName, $value);
	}
	
	public function getName() {
		return $this->getGrtGlobalAsString("name");
	}
	
	public function setName($name) {
		$this->setGrtGlobalFromString("name", $name);
		
		return $this->getName();
	}
	
	public function get_id() {
		return $this->getGrtGlobalAsString("_id");
	}
	
	public function set_id($_id) {
		$this->setGrtGlobalFromString("_id", $_id);
			
		return $this->get_id();
	}

	public function getOwner() {
		return Grt::getInstance()->getObjectByRefId(
				$this->getGrtGlobalAsObject("owner"));
	}
	
	public function setOwner($owner) { 
		$this->setGrtGlobalFromString("owner", $owner->get_id());

		return $this->getOwner();
	}
	
	public function getOwnerById() {
		return $this->getGrtGlobalAsString("owner");
	}
	
	public function setOwnerById($owner_id) {
		$this->setGrtGlobalFromString("owner", $owner_id);
	}
}
?>