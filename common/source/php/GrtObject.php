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

class com_mysql_grt_GrtObject implements GrtXml {
	
	protected $data = null;
	
	public function __construct($obj) {
		if (is_string($obj)) {
			$this->data = new com_mysql_grt_GrtObjectDataGlobal($obj);
		} else if (($obj == null) || ($obj instanceof com_mysql_grt_GrtObject)) {
			$this->data = new com_mysql_grt_GrtObjectDataLocal($this);
			$this->data->setOwner($obj);
			Grt::getInstance()->addToObjectCache($this);
		}
	}

	public function getName() {
		return $this->data->getName();
	}

	public function setName($name) {
		return $this->data->setName($name);
	}

	public function get_id() {
		return $this->data->get_id();
	}

	public function set_id($_id) {
		return $this->data->set_id($_id);
	}

	public function getOwner() {
		return $this->data->getOwner();
	}

	public function setOwner(com_mysql_grt_GrtObject $owner) {
		return $this->data->setOwner($owner);
	}

	public function getOwnerById() {
		return $this->data->getOwnerById();
	}

	public function setOwnerById($owner_id) {
		$this->data->setOwnerById($owner_id);
	}

	public function getGrtXml() {
		return $this->getGrtXmlKeyTag("");
	}

	public function getGrtXmlKeyTag($keyTag) {
		$xml = "<value type=\"dict\" struct-name=\""
				. str_replace("_", ".", substr(get_class($this), strlen(Grt::GrtPackagePrefix))) . "\"" . $keyTag;

		if ($this->data instanceof com_mysql_grt_GrtObjectDataLocal)
			$xml .= ">\n" . $this->getGrtXmlMembers() . "</value>\n";
		else
			$xml .= " global-object-path=\""
				. Grt::escapeStringForXml($data->getGlobalObjectPath()) . "\" />\n";

		return $xml;
	}

	public function getGrtXmlMembers() {
		$xml = "<value type=\"string\" key=\"_id\">"
				. Grt::escapeStringForXml($this->get_id()) . "</value>\n";

		if ($this->getName() != null)
			$xml .= "<value type=\"string\" key=\"name\">"
					. Grt::escapeStringForXml($this->getName()) . "</value>\n";

		if ($this->getOwner() != null)
			$xml .= "<value type=\"string\" key=\"owner\">"
					. Grt::escapeStringForXml($this->getOwnerById())
					. "</value>\n";

		return $xml;
	}
}
?>