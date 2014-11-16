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

class GrtMessage implements GrtXml {
	private $msgType; // 0..log message, 1..error

	private $msg = "";

	private $msgDetails = array();

	public static $logMessage = 0;

	public static $logError = 1;

	public function __construct($msg, $msgType) {
		$this->msg = $msg;
		$this->msgType = $msgType;
		
		$this->msgDetails = new GrtStringList("", "");
	}

	public function addMessageDetail($msgDetail) {
		$this->msgDetails->add($msgDetail);
	}

	public function getGrtXml() {
		return $this->getGrtXmlKeyTag("");
	}

	public function getGrtXmlKeyTag($keyTag) {
		return "<value type=\"dict\" struct-name=\"GrtMessage\" key=\""
				. $keyTag . "\">" 
				. "<value type=\"int\" key=\"msgType\">"
				. $this->msgType . "</value>\n"
				. "<value type=\"string\" key=\"msg\">" . $this->msg . "</value>\n"
				. $this->msgDetails->getGrtXmlKeyTag(" key=\"details\"") . "</value>\n";
	}
}
?>