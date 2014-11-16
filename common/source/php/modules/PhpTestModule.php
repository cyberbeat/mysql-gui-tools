<?php

class PhpTestModule {
	public static function getModuleInfo() {
		return Grt::getModuleInfoXml('PhpTestModule', '');
	}

	public static function helloWorld() {
		Grt::getInstance()->addMsg("Hello world!");
		
		return 'Hello world!';
	}
	
	public static function upperCase($str) {
		return strtoupper($str);
	}
	
	public static function getListSize(GrtList $list) {
		return $list->size();
	}

	public static function concatStrings($s1, $s2) {
		return $s1 . $s2;
	}

	public static function throwException() {
		throw new Exception("Exception Test");
	}
	
	public static function getMessages() {
		$msgList = new GrtList("GrtMessage", "");
		$msgs = Grt::getInstance()->getMessages();

		for ($i = 0; $i < count($msgs); $i++) {
			$msgList->addObject($msgs[$i]);
		}

		Grt::getInstance()->clearMessages();

		return $msgList;
	}

	public static function getGlobalString($objectPath) {
		Grt::getInstance()->addMsg("Calling getGrtGlobalAsString.");
		Grt::getInstance()->addMsgDetailToLastMsg(
				"applicationPath = " . Grt::getInstance()->getApplicationPath());
		Grt::getInstance()->addMsgDetailToLastMsg(
				"callback.class = "
						. get_class(Grt::getInstance()->getCallback()));

		return Grt::getInstance()->getGrtGlobalAsString($objectPath);
	}
	
	public static function testCallbacks() {
		$root = Grt::getInstance()->getGrtGlobalAsObject("/");
		
		$list = new GrtStringList("", "");
		$list->add("Item1");
		$list->add("Item2");		
		$root->addObject("stringList", $list);
		
		$obj = new com_mysql_grt_GrtObject(null);
		$obj->setName("testObject");		
		$root->addObject("object", $obj);
		
		$map = new GrtStringHashMap("", "");
		$map->add("mike", "mzinner@mysql.com");
		$map->add("alfredo", "alfredo@mysql.com");
		$root->addObject("emails", $map);
		
		$catalog = new com_mysql_grt_db_Catalog(null);
		$catalog->setName("sourceCatalog");
		
		$schemata = new GrtObjectList("com_mysql_grt_db_Schema", "");
		$catalog->setSchemata($schemata);
		
		$schema = new com_mysql_grt_db_Schema($catalog);
		$schema->setName("scott");
		$schemata->add($schema);
		
		$root->addObject("sourceCatalog", $catalog);
	}
	
	public static function printPhpInfo() {
		phpinfo();
	}
	
	public static function debugTest() {
		echo xdebug_call_file() . ", func: " . xdebug_call_function() . ", Ln" . xdebug_call_line();
		xdebug_break();
	}
}
?>