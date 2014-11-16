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
 * Grt main class
 * 
 * @author MikeZ
 * @version 1.0, 02/26/05
 */

class Grt {
	private static $instance = null;
	
	private $myxGrtPointer = 0;
	
	private $callback;
	
	private $applicationPath;
	
	private $objects = array();
	
	private $msgs = array();
	
	const MYX_INT_VALUE = 1;

	const MYX_REAL_VALUE = 2;

	const MYX_STRING_VALUE = 3;

	const MYX_LIST_VALUE = 4;

	const MYX_DICT_VALUE = 5;

	const MYX_UNKNOWN_VALUE = -1;
	
	const GrtPackagePrefix = "com_mysql_grt_";

	const GrtModulePackagePrefix = "com_mysql_grt_modules_";

	/**
	 * Private class constructor.
	 */
	private function __construct() {
	}

	/**
	 * Static function to return a pointer to a instance of the Grt class. The
	 * first call triggers the generation of a object and all other calls return
	 * a pointer to that object.
	 * 
	 * @return returns a pointer to the one and only Grt object
	 */
	public static function getInstance() {
		if (self::$instance == null) {
			self::$instance = new Grt();
		}

		return self::$instance;
	}
	
	/**
	 * This function sets the callback object that is used to call back to the
	 * JNI.
	 * 
	 * @param callbackClassName
	 *            name of the class that is used to process the callbacks
	 * @param applicationPath
	 *            the path to the directory where the libraries are located
	 */
	public function setCallback($callbackClassName, $applicationPath) {
		if ($this->callback == null) {
			try {
				$class = new ReflectionClass($callbackClassName);
				$callback = $class->newInstance();
				
				$this->callback = $callback;
				$this->applicationPath = $applicationPath;

				// Set the callback's applicationPath so it can find the libraries
				$callback->setApplicationPath($applicationPath);				
			} catch (Exception $e) {
				return;
			}
		}
	}

	/**
	 * Function to return the instance of a GrtCallbackMock that is used to
	 * process the callback methods
	 * 
	 * @return instance of a GrtCallback class
	 */
	public function getCallback() {
		if ($this->callback == null)
			throw new Exception("The callback object is not set.");

		return $this->callback;
	}
	
	/**
	 * returns the applicationPath string. Has to be set before with
	 * setApplicationPath
	 * 
	 * @return returns the applicationPath string
	 */
	public function getApplicationPath() {
		return $this->applicationPath;
	}

	/**
	 * returns the myxGrtPointer used for callGrtFunction calls casted as a long
	 * 
	 * @return returns the myxGrtPointer
	 */
	public function getMyxGrtPointer() {
		return $this->myxGrtPointer;
	}

	/**
	 * sets the myxGrtPointer used for callGrtFunction calls
	 * 
	 * @param myxGrtPointer
	 *            the pointer casted as a long
	 */
	public function setMyxGrtPointer($myxGrtPointer) {
		$this->myxGrtPointer = $myxGrtPointer;
	}
	
	/**
	 * Adds an object to the Grt's list of used object.
	 * 
	 * @param object
	 *            object to add
	 */
	public function addToObjectCache(com_mysql_grt_GrtObject $object) {
		if (!array_key_exists($object->get_id(), $this->objects))
			$this->objects[$object->get_id()] = $object;
	}
	
	/**
	 * Removes an object from the Grt's list of used object.
	 * 
	 * @param object
	 *            object to add
	 */
	public function removeFromObjectCache(com_mysql_grt_GrtObject $object) {
		if (array_key_exists($object->get_id(), $this->objects))
			unset($this->objects[$object->get_id()]);
	}
	
	/**
	 * Clears the object cache
	 *  
	 */
	public function clearObjectCache() {
		unset($this->objects);
		
		$this->objects = array();
	}
	
	/**
	 * Search a object in the list of used objects by it's _id. If the _id is
	 * not found the function looks in the GRT global object tree
	 * 
	 * @param RefId
	 *            the _Id of the object
	 * @return a pointer to the object or null if not found
	 */
	public function getObjectByRefId($refId) {
		if ($refId == null)
			return null;

		if (array_key_exists($refId, $this->objects)) {
			$obj = $this->objects[$refId];
		} else {
			$obj = null;
		}

		// if the refId is not found in the object cache
		// search for the refId in the global object using the
		// callback function
		if (($obj == null) && ($this->getCallback() != null)) {
			$obj = $this->getCallback()->getGrtGlobalById(
					$this->getMyxGrtPointer(), $refId);
		}
		
		return $obj;
	}
	
	/**
	 * Return an GrtObject of the given class
	 * 
	 * @param className
	 *            name of the class, e.g. db.oracle.table
	 * @param parent
	 *            The parent object, if any
	 * @return a new instance of the requested class or null if the class was
	 *         not found
	 */
	public static function getGrtClassInstance($className, $parent) {
		try {
			$class = new ReflectionClass(Grt::GrtPackagePrefix . $className);
			return $class->newInstance($parent);
		} catch (Exception $e) {
			return null;
		}
	}
	
	/**
	 * Adds the given message string as a normal GRT message to the GRT message
	 * list
	 * 
	 * @param msg
	 *            message string
	 */
	public function addMsg($msg) {
		$grtMsg = new GrtMessage($msg, 0);

		array_push($this->msgs, $grtMsg);

		return $grtMsg;
	}
	
	/**
	 * Adds the given message string as a GRT error to the GRT message list
	 * 
	 * @param msg
	 *            message string
	 */
	public function addErr($msg) {
		$grtMsg = new GrtMessage($msg, 1);

		array_push($this->msgs, $grtMsg);

		return $grtMsg;
	}
	
	/**
	 * Adds the given detail message to the latest GRT message. Beware that this
	 * method is not threadsafe
	 * 
	 * @param msgDetail
	 *            message detail string
	 */
	public function addMsgDetailToLastMsg($msgDetail) {
		if (count($this->msgs) > 0) {
			$msg = $this->msgs[count($this->msgs) - 1];
			$this->addMsgDetailToMsg($msg, $msgDetail);
		}
	}
	
	/**
	 * Adds the given detail message to the given GRT message
	 * 
	 * @param msg
	 *            the GrtMsg the detail string should be added to
	 * @param msgDetail
	 *            message detail string
	 */
	public function addMsgDetailToMsg($msg, $msgDetail) {
		$msg->addMessageDetail($msgDetail);
	}
	
	/**
	 * Returns GRT messages as Vector
	 * 
	 * @param offset
	 *            number of first message to return
	 */
	public function getMessages() {
		return $this->msgs;
	}
	
	/**
	 * Returns GRT messages as a GRT list
	 * 
	 * @param offset
	 *            number of first message to return
	 */
	public static function getMessagesAsList() {
		$msgList = new GrtList("GrtMessage", "");
		$msgs = Grt::getInstance()->getMessages();

		for ($i = 0; $i < count($msgs); $i++) {
			$msgList->addObject($msgs[$i]);
		}

		unset($msgs);
		
		$msgs = array();

		return $msgList;
	}
	
	/**
	 * Clears the GRT list
	 * 
	 */
	public function clearMessages() {
		unset($this->msgs);
		$this->msgs = array();
	}
	
	/**
	 * Forces the output of the GRT messages
	 * 
	 */
	public function flushMessages() {
		if (count($this->msgs) > 0) {
			$msgList = new GrtList("GrtMessage", "");
			
			for ($i = 0; $i < count($this->msgs); $i++) {
				$msgList->addObject($this->msgs[$i]);
			}
			
			$this->clearMessages();
			
			$xml = $msgList->getGrtXml();
			
			$this->getCallback()->processMessages(
				$this->getMyxGrtPointer(), $xml);
        
      return $this->getCallback()->processStatusQuery(
				$this->getMyxGrtPointer());
		}
	}
	
	/**
	 * Returns the numeric type of a value_type name
	 * 
	 * @param s
	 *            name of the value_type
	 */
	public static function getValueTypeFromString($s) {
		$result = Grt::MYX_UNKNOWN_VALUE;
		
		if (strcmp($s, "int") == 0) {
			$result = Grt::MYX_INT_VALUE;
		} else if (strcmp($s, "real") == 0) {
			$result = Grt::MYX_REAL_VALUE;
		} else if (strcmp($s, "string") == 0) {
			$result = Grt::MYX_STRING_VALUE;
		} else if (strcmp($s, "list") == 0) {
			$result = Grt::MYX_LIST_VALUE;
		} else if (strcmp($s, "dict") == 0) {
			$result = Grt::MYX_DICT_VALUE;
		}
			
		return $result;
	}
	
	/**
	 * Adds a XML header and a root node to the given XML string
	 * 
	 * @param xml
	 *            a XML string without a XML header and root node
	 * @return a complete XML document
	 */
	public static function prepareGrtXml($xml) {
		return "<?xml version=\"1.0\"?>\n<data>\n" . $xml . "</data>\n";
	}
	
	/**
	 * Checks if two classes are the same Converts simple types like int to
	 * java.lang.Integer
	 * 
	 * @param c1
	 *            class to compare
	 * @param c2
	 *            class to compare
	 * @return true if the classes are the same
	 */
	private static function checkSameClass($c1, $c2) {
		$className1 = $c1->getName();
		$className2 = $c2->getName();

		return strcmp($className1, $className2);
	}
	
	/**
	 * Parses a given XML Node of the value type DICT Calls other functions
	 * recursive to get the complete information
	 * 
	 * @param valueNode
	 *            XML node to parse
	 * @param parent
	 *            the parent object of this node (if any)
	 * @return the created Object
	 */
	private static function getDictFromGrtXmlNode($valueNode, $parent) {
		$structName = (string) $valueNode['struct-name'];
		$contentType = (string) $valueNode['content-type'];
		$contentStructName = (string) $valueNode['content-struct-name'];
		$globalObjectPath = (string) $valueNode['global-object-path'];
		//$valueListContentType = Grt::getValueTypeFromString($contentType);

		if (($structName != null) && ($structName != "")) {
			try {
				$structName = str_replace('.', '_', $structName);
				
				$class = new ReflectionClass(Grt::GrtPackagePrefix . $structName);
				$obj = null;

				// if this is a regular object
				if ($globalObjectPath == "") {
					if ($parent instanceof com_mysql_grt_GrtObject)
						$obj = $class->newInstance($parent);
					else
						$obj = $class->newInstance(null);
				} else {
					// if this is a global object
					$obj = $class->newInstance($globalObjectPath);
				}

				foreach ($valueNode->{'value'} as $childValueNode) {
					// add all child values to dict
					$item = Grt::getObjectsFromGrtXmlNode($childValueNode, null);

					if ($item !== null) {
						$setter = (string) $childValueNode['key'];

						$option = (string) $childValueNode['option'];

						// build setter name
						if (strlen($setter) == 0)
							continue;
							
						if (strlen($setter) == 1) {
							$setter = strtoupper($setter);
						} else {
							$setter = strtoupper(substr($setter, 0, 1)) 
									. substr($setter, 1, 255);
						}
						$setter = "set" . $setter;
						if (($option != null) && ($option == "ref"))
							$setter = $setter . "ById";

						// loop over all publicMethods and see if there
						// is a setKey() method with the correct
						// parameter
						$methods = $class->getMethods();
						for ($j = 0; $j < count($methods); $j++) {

							if (($methods[$j]->getName() == $setter)
									&& ($methods[$j]->getNumberOfParameters() == 1)) {

								$methods[$j]->invoke($obj, $item);

								break;
							}
						}
					}
				}

				return $obj;
			} catch (Exception $e) {
				return null;
			}
		} else {
			if ($contentType == "string")
				$dict = new GrtStringHashMap($globalObjectPath);
			else			
				$dict = new GrtHashMap($contentStructName, $globalObjectPath);

			foreach ($valueNode->{'value'} as $childValueNode) {
				// add all child values to dict
				$item = Grt::getObjectsFromGrtXmlNode($childValueNode, $dict);

				// check if returned object is not null
				if ($item != null)
					$dict->addObject((string) $childValueNode['key'], $item);
			}

			return $dict;
		}
	}
	
	/**
	 * Parses a given XML Node of the value type LIST Calls other functions
	 * recursive to get the complete information
	 * 
	 * @param valueNode
	 *            XML node to parse
	 * @param parent
	 *            the parent object of this node (if any)
	 * @return the created Object
	 */
	private static function getListFromGrtXmlNode($valueNode, $parent) {
		$list = null;
		$contentType = (string) $valueNode['content-type'];
		$contentStructName = (string) $valueNode['content-struct-name'];
		$globalObjectPath = (string) $valueNode['global-object-path'];
		$valueListContentType = Grt::getValueTypeFromString($contentType);
		
		// TODO: the following is a hack, it should be implemented using a
		// ObjectRefList instead
		//
		// for global reference list with the content type string and a content
		// struct name simulate an object list
		if (($globalObjectPath != null) && !($globalObjectPath == "")
				&& ($valueListContentType == Grt::MYX_STRING_VALUE)
				&& ($contentStructName != null) && ($contentStructName != "")) {
			$valueListContentType = Grt::MYX_DICT_VALUE;
		}
		
		// if a content-struct-name was assigned, create list of given type
		if (($valueListContentType == Grt::MYX_DICT_VALUE)
				&& ($contentStructName != null) && ($contentStructName != "")) {
			try {
				//$class = new ReflectionClass(Grt::GrtPackagePrefix . str_replace($contentStructName, ".", '_')
				//		. "List");
				$class = new ReflectionClass('GrtObjectList');
				
				if ($globalObjectPath == "") {
					$list = $class->newInstance($contentStructName, '');
				} else {
					$list = $class->newInstance($contentStructName, $globalObjectPath);
				}
			} catch (Exception $e) {
				return null;
			}
			// if content-type is "string", create a string list
		} else if ($contentType == "string") {
			$list = new GrtStringList($globalObjectPath, '');
		} else {
			$list = new GrtList($contentStructName, $globalObjectPath);

			$list->setContentType($contentType);
		}

		// add all list items to the list
		foreach ($valueNode->{'value'} as $childValueNode) {
			$item = Grt::getObjectsFromGrtXmlNode($childValueNode, $list);

			// check if returned object is not null
			if ($item != null)
				$list->addObject($item);
		}
		
		if ($list != null)
			$list->setOwner($parent);

		return $list;
	}
	
	/**
	 * Parses a given XML Node of any value type Calls other functions recursive
	 * to get the complete information
	 * 
	 * @param valueNode
	 *            XML node to parse
	 * @param parent
	 *            the parent object of this node (if any)
	 * @return the created Object
	 */
	private static function getObjectsFromGrtXmlNode(SimpleXMLElement $valueNode, $parent) {
		$valueNodeType = Grt::getValueTypeFromString((string) $valueNode['type']);
		
		if ($valueNodeType == Grt::MYX_STRING_VALUE) {
			$value = (string) $valueNode;
			
			if (substr($value, 0, 7) == 'global::') {
				$grt = (object) Grt::getInstance();
				return $grt->getGrtGlobalAsObject(substr($value, 8));
			}
								
			return $value;
		} else if ($valueNodeType == Grt::MYX_INT_VALUE) {
			return (int) $valueNode;
		} else if ($valueNodeType == Grt::MYX_REAL_VALUE) {
			return (float) $valueNode;
		} else if ($valueNodeType == Grt::MYX_DICT_VALUE) {
			return Grt::getDictFromGrtXmlNode($valueNode, $parent);
		} else if ($valueNodeType == Grt::MYX_LIST_VALUE) {
			return Grt::getListFromGrtXmlNode($valueNode, $parent);
		}
		
		return null;
	}
	
	/**
	 * Parses a given Grt XML string
	 * 
	 * @param xml
	 *            the Grt XML string to parse
	 * @return the created Object
	 */
	public static function getObjectsFromGrtXml($xml) {
		if ($xml == null || $xml == "(null)") {
			$obj = null;
		} else {
			$data = simplexml_load_string($xml);
		
			$obj = Grt::getObjectsFromGrtXmlNode($data->{'value'}, null);
		}
		
		return $obj;
	}
	
	/**
	 * Returns the XML representation of the given object
	 * 
	 * @param obj
	 *            the object to serialize
	 * @return the XML string
	 */
	public static function getObjectAsXml($obj) {
		return Grt::getObjectAsXmlWithKeyTag($obj, "");
	}

	/**
	 * Returns the XML representation of the given object
	 * 
	 * @param obj
	 *            the object to serialize
	 * @param keyTag
	 *            the key tag with a leading space if the object is a member of
	 *            a DICT
	 * @return the XML string
	 */
	public static function getObjectAsXmlWithKeyTag($obj, $keyTag) {
		$xml = "";

		if ($obj != null) {
			// get XML from GrtObjects, GrtLists and GrtHashMaps
			if ($obj instanceof GrtXml) {
				$xml .= $obj->getGrtXmlKeyTag($keyTag);
			} else {
				// get XML for primitive types
				if (is_string($obj)) {
					$xml .= "<value type=\"string\"" . $keyTag . ">"
							. Grt::escapeStringForXml($obj) . "</value>\n";
				} else if (is_int($obj)) {
					$xml .= "<value type=\"int\"" . $keyTag . ">"
							. $obj . "</value>\n";
				} else if (is_float($obj)) {
					$xml .= "<value type=\"real\"" . $keyTag . ">"
							. $obj . "</value>\n";
				}
			}
		}

		return $xml;
	}

	public static function escapeStringForXml($s) {
		return str_replace("\"", "&quot;",
			str_replace(">", "&gt;", 
			str_replace("<", "&lt;", 
			str_replace("&", "&amp;", $s))));
	}
	
	/**
	 * Builds a Grt XML module info of the given class which can be returned to
	 * the Grt environment.
	 * 
	 * @param module
	 *            the class that represents a Grt module
	 * @param extendsModule
	 *            name of the Grt module this class extends
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static function getModuleInfoXml($moduleName, $extendsModule) {
		$class = new ReflectionClass($moduleName);
		
		$xml = "<value type=\"dict\">\n" .
			"  <value key=\"name\" type=\"string\">" .
			$class->getName() . "</value>\n" .
			"  <value key=\"functions\" type=\"list\" content-type=\"string\">\n";
			
		$methods = $class->getMethods();
		
		foreach ($methods as $method) {
			if ($method->isStatic() && $method->isPublic()
				&& (strcmp($method->getName(), 'getModuleInfo') != 0)) {
				$xml .= "    <value type=\"string\">".
					$method->getName().":(";
					
				$parameters = $method->getParameters();
				
				foreach ($parameters as $param) {
					$xml .= $param->getName() . ";";
				}
				
				$xml .= "):</value>\n";
			}
		}
		
		$xml .= "  </value>\n";
		
		if (strcmp($extendsModule, "") != 0) {
			$xml .= "  <value key=\"extends\" type=\"string\">" .
					$extendsModule . "</value>\n";
		}
		
		$xml .= "</value>\n";
		
		return self::prepareGrtXml($xml);
	}
	
	/**
	 * Builds a Grt XML string containing an error message. Can be used for Grt
	 * modules to return an error to the Grt environment
	 * 
	 * @param error
	 *            the error message
	 * @return returns a GRT XML string containing the error
	 */
	public static function moduleFunctionError($e) {
		if (is_string($e)) {
			$result = new GrtHashMap("", "");
			$result->addObject("error", $e);
			$result->addObject("detail", "-");
			return Grt::prepareGrtXml(Grt::getObjectAsXml($result));
		} else {
			$result = new GrtHashMap("", "");
			
			if (($e->getCode() != '') && ($e->getCode() != null)) {
				$result->addObject("error", $e->getMessage() . " " . $e->getCode());
			} else {
				$result->addObject("error", $e->getMessage());
			}
			$result->addObject("detail", $e->getFile() . ", Ln " 
					. $e->getLine(). "\n"
					. $e->getTraceAsString() . "\n" );

			return Grt::prepareGrtXml(Grt::getObjectAsXml($result));
		}
	}
	
	/**
	 * Calls a Java module function
	 * 
	 * @param module
	 *            the class that represents the module
	 * @param functionName
	 *            name of the function to call
	 * @param functionSignature
	 *            java signature of the function to call
	 * @param xml
	 *            the arguments of the function represented in GRT xml
	 * @return returns a GRT XML string
	 */
	public static function callModuleFunction($module, $functionName, $functionSignature, $xml) {
		//echo "Function " . $module . "::" . $functionName . $functionSignature . " got called with " . $xml;

		$class = new ReflectionClass($module);
		
		if ($functionSignature != "" && $functionSignature != null && $functionSignature != "()") {
			$paramCount = substr_count($functionSignature, ";");
		} else {
			$paramCount = 0;
		}
		
		$resultValue = null;
		
		foreach ($class->getMethods() as $method) {
			
			if ((strcmp($method->getName(), $functionName) == 0) 
				&& ($paramCount == $method->getNumberOfParameters() ) ) {
				
				if ($xml != null) {
					$grtParams = Grt::getObjectsFromGrtXml($xml);
				} else {
					$grtParams = null;
				}
				
				$params = array();
				
				if (($paramCount == 1) && (!($grtParams instanceof GrtList))) {
					array_push($params, $grtParams);
				} else if ($grtParams instanceof GrtList) {
					$grtParamsList = (object) $grtParams;
					
					if ($paramCount != $grtParamsList->size()) {
						return Grt::moduleFunctionError("The number of submitted "
								. "parameters does not match the "
								. "number of parameters of the php function.");
					}
					
					for ($i = 0; $i < $paramCount; $i++) {
						$params[$i] = $grtParamsList->getObject($i);
					}
				} else if ($grtParams == null) {
					//
				} else {
					return Grt::moduleFunctionError("If the function has more "
							. "than one parameter these parameters "
							. "have to be passed in a list.");
				}
				
				// call method
				try {
					// since invokeArgs is broken use wrapper function
					//$resultValue = $method->invokeArgs(NULL, $params);
					
					$resultValue = reflectionMethodInvokeArgsStatic($method, $params);
					
					// return value
					$result = new GrtHashMap("", "");
					$result->addObject("value", $resultValue);
					$xmlResult = Grt::prepareGrtXml(Grt::getObjectAsXml($result));
					
					// clear object cache
					$grt = (object) Grt::getInstance();
					$grt->clearObjectCache();
					
					$grt->flushMessages();
					
					return $xmlResult;
				} catch (Exception $e) {
					return Grt::moduleFunctionError($e);
				}
			}
		}
		
		return Grt::moduleFunctionError("The called function was not found.");
	}
	
	/**
	 * Calls a GRT module function
	 * 
	 * @param module
	 *            the GRT module that represents the module
	 * @param functionName
	 *            name of the function to call
	 * @param arguments
	 *            the arguments of the function represented in GRT xml
	 * @return returns the result of the function call
	 */
	public function callGrtFunction($module, $functionName, $arguments) {
		$result = Grt::getObjectsFromGrtXml($this->getCallback()->callGrtFunction(
				$this->getMyxGrtPointer(), $module, $functionName,
				Grt::prepareGrtXml(Grt::getObjectAsXml($arguments))));

		if ($result instanceof GrtHashMap) {
			$value = $result->getObject("value");

			if ($value != null)
				return $value;
				
			$value = $result->getObject("error");

			if (is_string($value)) {
				$exceptionMessage = "The function " . $module . ":"
						. $functionName . "() "
						. "returned the following error:\n"
						. $value;
			} else {
				$exceptionMessage = "The function " . $module . ":"
						. $functionName . "() returned an error.";
			}

			throw new Exception($exceptionMessage);
		}

		return null;
	}
	
	/**
	 * This function looks up a global object from the GRT global object tree by
	 * its path and returns it as a GrtObject
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * 
	 * @return returns a global GrtObject from the GRT global object tree
	 */
	public function getGrtGlobalAsGrtObject($objectPath) {
		return Grt::getGrtGlobalAsObject($objectPath);
	}

	/**
	 * This function looks up a global object from the GRT global object tree by
	 * its path and returns it as a Object
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * 
	 * @return returns a global Object from the GRT global object tree
	 */
	public function getGrtGlobalAsObject($objectPath) {
		return $this->getCallback()->getGrtGlobalAsObject(
				$this->getMyxGrtPointer(), $objectPath);
	}

	/**
	 * This function looks up a global object from the GRT global object tree by
	 * its path and returns it as a String
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * 
	 * @return returns a String looked up from the GRT global object tree
	 */
	public function getGrtGlobalAsString($objectPath) {
		return $this->getCallback()->getGrtGlobalAsString(
				$this->getMyxGrtPointer(), $objectPath);
	}

	/**
	 * This function looks up a global object from the GRT global object tree by
	 * its path and returns it as a int
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * 
	 * @return returns an int looked up from the GRT global object tree
	 */
	public function getGrtGlobalAsInt($objectPath) {
		return $this->getCallback()->getGrtGlobalAsInt(
				$this->getMyxGrtPointer(), $objectPath);
	}

	/**
	 * This function looks up a global object from the GRT global object tree by
	 * its path and returns it as a double
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * 
	 * @return returns a double looked up from the GRT global object tree
	 */
	public function getGrtGlobalAsReal($objectPath) {
		return $this->getCallback()->getGrtGlobalAsReal(
			$this->getMyxGrtPointer(), $objectPath);
	}

	/**
	 * This function sets a global object from the GRT global object tree to the
	 * given object value
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * @param value
	 *            the new value of the object
	 */
	public function setGrtGlobalFromObject($objectPath, $value) {
		$this->getCallback()->setGrtGlobalFromObject(
				$this->getMyxGrtPointer(), $objectPath, $value);
	}

	/**
	 * This function sets a global object from the GRT global object tree to the
	 * given String value
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * @param value
	 *            the new value of the object
	 */
	public function setGrtGlobalFromString($objectPath, $value) {
		$this->getCallback()->setGrtGlobalFromString(
			$this->getMyxGrtPointer(), $objectPath, $value);
	}

	/**
	 * This function sets a global object from the GRT global object tree to the
	 * given int value
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * @param value
	 *            the new value of the object
	 */
	public function setGrtGlobalFromInt($objectPath, $value) {
		$this->getCallback()->setGrtGlobalFromInt(
				$this->getMyxGrtPointer(), $objectPath, $value);
	}

	/**
	 * This function sets a global object from the GRT global object tree to the
	 * given double value
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * @param value
	 *            the new value of the object
	 */
	public function setGrtGlobalFromReal($objectPath, $value) {
		$this->getCallback()->setGrtGlobalFromReal(
			$this->getMyxGrtPointer(), $objectPath, $value);
	}

	public function getGrtGlobalListSize($objectPath) {
		return $this->getCallback()->getGrtGlobalListSize(
				$this->getMyxGrtPointer(), $objectPath);
	}

	public function getGrtGlobalListItem($objectPath, $index) {
		return $this->getCallback()->getGrtGlobalListItem(
				$this->getMyxGrtPointer(), $objectPath, $index);
	}

	public function addGrtGlobalListItem($objectPath, $item) {
		$this->getCallback()->addGrtGlobalListItem(
			$this->getMyxGrtPointer(), $objectPath, $item);
	}

	public function removeGrtGlobalListItem($objectPath, $index) {
		$this->getCallback()->removeGrtGlobalListItem(
				$this->getMyxGrtPointer(), $objectPath, $index);
	}

	public function getGrtGlobalDictItem($objectPath, $key) {
		return $this->getCallback()->getGrtGlobalDictItem($this->getMyxGrtPointer(),
				$objectPath, $key);
	}

	public function addGrtGlobalDictItem($objectPath, $key, $item) {
		$this->getCallback()->addGrtGlobalDictItem(
				$this->getMyxGrtPointer(), $objectPath, $key, $item);
	}

	public function removeGrtGlobalDictItem($objectPath, $key) {
		$this->getCallback()->removeGrtGlobalDictItem(
				$this->getMyxGrtPointer(), $objectPath, $key);
	}
}
?>