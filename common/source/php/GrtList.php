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

class GrtList implements GrtXml {
	protected $isGlobalObject = false;
	
	protected $globalObjectPath = '';
	
	protected $list = null;
	
	private $contentType = '';

	private $contentStructName = '';

	function __construct($contentStructName, $globalObjectPath) {
		if ($contentStructName != '') {
			$this->contentType = "dict";
			$this->contentStructName = $contentStructName;
		}
		
		$this->isGlobalObject = ($globalObjectPath != '');
		$this->globalObjectPath = $globalObjectPath;
	}
	
	protected function getIsGlobalObject() {
		return $this->isGlobalObject;
	}

	protected function getGlobalObjectPath() {
		return $this->globalObjectPath;
	}

	public function size() {
		if ($this->getIsGlobalObject())
			return Grt::getInstance()->getGrtGlobalListSize(
					$this->getGlobalObjectPath());
		

		if ($this->list == null) {
			$this->list = array();
		}
		
		return count($this->list);
	}
	
	public function addObject($item) {
		if ($this->list == null) {
			$this->list = array();
		}
		
		array_push($this->list, $item);
		
		return $this->getObject($this->size() - 1);
	}
	
	public function getObject($index) {
		if ($this->list == null) {
			$this->list = array();
		}
		
		if (array_key_exists($index, $this->list)) {
			return $this->list[$index];
		} else {
			return null;
		}
	}
	
	public function remove($index) {
		if ($this->list == null) {
			$this->list = array();
		}
		
		unset($this->list[$index]);
		
		return $this;
	}
	
	public function getIndexOfName($name) {
		$key = array_search($name, $this->list);
		
		if ($key === false)
			return -1;
		
		return $key;
	}
	
	public function contains($item) {
		return ($this->getIndexOfName($item) != -1);
	}
	
	public function getOwner() {
		return $this->owner;
	}

	public function setOwner($owner) {
		$this->owner = $owner;
	}
	
	public function getContentType() {
		return $this->contentType;
	}

	public function setContentType($contentType) {
		$this->contentType = $contentType;
	}

	public function getContentStructName() {
		return $this->contentStructName;
	}

	public function setContentStructName($contentStructName) {
		$this->contentStructName = $contentStructName;
	}

	public function getGrtXml() {
		return $this->getGrtXmlKeyTag('');
	}
	
	public function getGrtXmlKeyTag($keyTag) {
		$xml = "<value type=\"list\"";

		if (!$this->getContentType() == '')
			$xml .= " content-type=\"" . $this->getContentType() . "\"";

		if (!$this->getContentStructName() == '')
			$xml .= " content-struct-name=\"" 
					. str_replace('_', '.', $this->getContentStructName())
					. "\"";

		if ($this->getIsGlobalObject())
			$xml .= " global-object-path=\""
					. Grt::escapeStringForXml($this->getGlobalObjectPath()) . "\"";

		$xml .= $keyTag . ">\n";

		for ($i = 0; $i < $this->size(); $i++) {
			$xml .= Grt::getObjectAsXmlWithKeyTag($this->getObject($i), '');
		}

		return $xml . "</value>\n";
	}
	
	public function getGrtRefXml($keyTag) {
		$xml = "<value type=\"list\" option=\"ref\"";

		if ($this->getContentType() != "")
			$xml .= " content-type=\"string\"";

		if ($this->getContentStructName() != "")
			$xml .= " content-struct-name=\"" . $this->getContentStructName()
					. "\"";

		$xml .= $keyTag . ">\n";

		for ($i = 0; $i < $this->size(); $i++) {
			$obj = $this->getObject($i);

			if ($obj instanceof GrtObject) {
				$xml .= "<value type=\"string\" option=\"ref\">"
						. Grt::escapeStringForXml($obj->get_id())
						. "</value>\n";
			}
		}

		return $xml . "</value>\n";
	}
}
?>