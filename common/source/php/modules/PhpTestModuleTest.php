<?php

class PhpTestModuleTest {
	function __construct() {
		assert_options(ASSERT_ACTIVE, 1);
		assert_options(ASSERT_WARNING, 0);
		assert_options(ASSERT_QUIET_EVAL, 1);
		assert_options(ASSERT_CALLBACK, 'grtAssertHandler');
	}
	
	function __destruct() {
		assert_options(ASSERT_ACTIVE, 0);
	}

	/**
	 * Tests the callModuleFunction function by calling helloWorld()
	 */
	public function testCallModuleFunctionHelloWorld() {
		$xml = Grt::callModuleFunction("PhpTestModule", "helloWorld", "", "");

		$result = Grt::getObjectsFromGrtXml($xml);

		assert($result->getObject("error") == null);
		assert($result->getObject("value") == 'Hello world!');
		
		$xml = Grt::callModuleFunction("PhpTestModule", "getMessages", "",
				"");
				
		assert($xml != null);
	}

	/**
	 * Tests the callModuleFunction function by calling upperCase() with a
	 * single object as parameter
	 */
	public function testCallModuleFunctionUpperCase() {
		$testStr = "This is a test of the upperCase function.";
		$testParam = Grt::prepareGrtXml(Grt::getObjectAsXml($testStr));
		$xml = Grt::callModuleFunction("PhpTestModule", "upperCase",
				"(Ljava/lang/String;)", $testParam);

		$result = Grt::getObjectsFromGrtXml($xml);

		assert($result->getObject("error") == null);
		assert($result->getObject("value") == strtoupper($testStr));
	}

	/**
	 * Tests the callModuleFunction function by calling upperCase() with a
	 * GrtList as parameter
	 */
	public function testCallModuleFunctionUpperCaseWithList() {
		$testStr = "This is a test of the upperCase function.";
		
		$paramList = new GrtList("", "");
		$paramList->addObject($testStr);		
		
		$paramXml = Grt::prepareGrtXml(Grt::getObjectAsXml($testStr));
		$xml = Grt::callModuleFunction("PhpTestModule", "upperCase",
				"Ljava/lang/String;", $paramXml);

		$result = Grt::getObjectsFromGrtXml($xml);

		assert($result->getObject("error") == null);
		assert($result->getObject("value") == strtoupper($testStr));
	}

	/**
	 * Tests the callModuleFunction function by calling getListSize()
	 */
	public function testGetListSize() {
		$testList = new GrtList("", "");
		$testList->addObject("1");
		$testList->addObject("2");
		$testList->addObject("3");

		$paramList = new GrtList("", "");
		$paramList->addObject($testList);

		$paramXml = Grt::prepareGrtXml(Grt::getObjectAsXml($paramList));
		$xml = Grt::callModuleFunction("PhpTestModule", "getListSize",
				"Lcom/mysql/grt/GrtList;", $paramXml);

		$result = Grt::getObjectsFromGrtXml($xml);

		$listSize = (int) $result->getObject("value");

		assert($result->getObject("error") == null);
		assert($listSize == 3);
	}

	/**
	 * Tests the callModuleFunction function by calling concatStrings()
	 */
	public function testConcatStrings() {
		$s1 = "Hello";
		$s2 = "World";
		
		$paramList = new GrtList("", "");
		$paramList->addObject($s1);
		$paramList->addObject($s2);
		
		$paramXml = Grt::prepareGrtXml(Grt::getObjectAsXml($paramList));
		$xml = Grt::callModuleFunction("PhpTestModule", "concatStrings",
				"Ljava/lang/String;Ljava/lang/String;", $paramXml);

		$result = Grt::getObjectsFromGrtXml($xml);

		assert($result->getObject("error") == null);
		assert($result->getObject("value") == $s1 . $s2);
	}

	public function testGetMessages() {
		$xml = Grt::callModuleFunction("PhpTestModule", "getMessages", "",
				"");

		assert($xml != null);

		//System.out.println(xml);
	}

	public function testGetGlobalString() {
		Grt::getInstance()->setCallback("GrtCallbackTest", "");

		assert(PhpTestModule::getGlobalString("/testObject/name") == "TestObject");
	}
	
	public function testCallModuleFunctionUpperCaseWithXml() {
		$testStr = 'testing';
		$testParam = '<?xml version="1.0"?><data><value type="string">' . $testStr . '</value></data>';

		$xml = Grt::callModuleFunction("PhpTestModule", "upperCase",
				"(Ljava/lang/String;)", $testParam);

		$result = Grt::getObjectsFromGrtXml($xml);

		assert($result->getObject("error") == null);
		assert($result->getObject("value") == strtoupper($testStr));
	}
}

include('GrtInit.php');
include('modules/PhpTestModule.php');

// run tests
$testClass = new PhpTestModuleTest();

$testClass->testCallModuleFunctionHelloWorld();
$testClass->testCallModuleFunctionUpperCase();
$testClass->testCallModuleFunctionUpperCaseWithList();
$testClass->testGetListSize();
$testClass->testConcatStrings();
$testClass->testGetMessages();
$testClass->testGetGlobalString();
$testClass->testCallModuleFunctionUpperCaseWithXml();

unset($testClass);

?>