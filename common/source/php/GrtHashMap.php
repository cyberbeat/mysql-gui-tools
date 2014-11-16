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

class GrtHashMap implements GrtXml {
	protected $isGlobalObject = false;

	protected $globalObjectPath;

	protected $map;

	private $contentType = '';

	private $contentStructName = '';

	public function __construct($contentStructName, $globalObjectPath) {
		if (!$contentStructName == '') {
			$this->contentType = 'dict';
			$this->contentStructName = $contentStructName;
		}

		$this->isGlobalObject = (!$globalObjectPath == "");
		$this->globalObjectPath = $globalObjectPath;
	}
	
	protected function getIsGlobalObject() {
		return $this->isGlobalObject;
	}

	protected function getGlobalObjectPath() {
		return $this->globalObjectPath;
	}


	public function addObject($key, $item) {
		if ($this->getIsGlobalObject())
			Grt::getInstance()->addGrtGlobalDictItem(
					$this->getGlobalObjectPath(), $key, $item);
		else {
			if ($this->map == null)
				$this->map = array();

			$this->map[$key] = $item;
		}

		return $this->getObject($key);
	}
	
	public function getObject($key) {
		if ($this->getIsGlobalObject())
			return Grt::getInstance()->getGrtGlobalDictItem(
					$this->getGlobalObjectPath(), $key);
		
		if ($this->map == null)
			$this->map = array();

		if (array_key_exists($key, $this->map)) {
			return $this->map[$key];
		} else {
			return null;
		}
	}

	public function remove($key) {
		if ($this->getIsGlobalObject())
			Grt::getInstance()->removeGrtGlobalDictItem(
					$this->getGlobalObjectPath(), $key);
		else {
			if ($this->map == null)
				$this->map = array();

			unset($this->map[$key]);
		}
	}

	public function getKeys() {
		if ($this->map == null)
			$this->map = array();

		return array_keys($this->map);
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
		return $this->getGrtXmlKeyTag("");
	}

	public function getGrtXmlKeyTag($keyTag) {
		$xml = "<value type=\"dict\"" . $keyTag;

		if (!$this->getContentType() == "")
			$xml .= " content-type=\"" + $this->getContentType() . "\"";

		if (!$this->getContentStructName() == "")
			$xml .= " content-struct-name=\"" 
					. str_replace('_', '.', $this->getContentStructName())
					. "\"";

		if ($this->getIsGlobalObject())
			$xml .= " global-object-path=\""
					. Grt::escapeStringForXml($this->getGlobalObjectPath()) . "\"";

		$xml .= ">\n";

		if ($this->map != null) {
			$keys = array_keys($this->map);
			
			foreach ($keys as $key) {
				$xml .= Grt::getObjectAsXmlWithKeyTag($this->getObject($key), " key=\"" . $key
								. "\"");
			}
		}

		return $xml . "</value>\n";
	}
}
?>
