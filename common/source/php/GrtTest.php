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

class GrtTest {
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
	 * Tests the prepareGrtXml function
	 */
	public function testPrepareGrtXml() {
		$grtXml = "<value type=\"string\">test</value>";
		$xml = Grt::prepareGrtXml($grtXml);

		assert($xml == "<?xml version=\"1.0\"?>\n<data>\n" . $grtXml
				. "</data>\n");
	}
	
	/**
	 * Tests XML parsing and generation
	 */
	public function testGetObjectsFromGrtXml() {
		$grtXml = "<value type=\"dict\">\n"
				. "<value type=\"dict\" struct-name=\"db.SimpleDatatype\" key=\"simpleType\">\n"
				. "<value type=\"string\" key=\"_id\">423423:89734566:746345234</value>\n"
				. "<value type=\"string\" key=\"name\">VARCHAR</value>\n"
				. "<value type=\"int\" key=\"characterMaximumLength\">255</value>\n"
				. "<value type=\"int\" key=\"characterOctetLength\">0</value>\n"
				. "<value type=\"int\" key=\"numericPrecision\">0</value>\n"
				. "<value type=\"int\" key=\"numericPrecisionRadix\">0</value>\n"
				. "<value type=\"int\" key=\"numericScale\">0</value>\n"
				. "<value type=\"int\" key=\"dateTimePrecision\">0</value>\n"
				. "</value>\n"
				. "<value type=\"list\" content-type=\"dict\" content-struct-name=\"db.Column\" key=\"columns\">\n"
				. "<value type=\"dict\" struct-name=\"db.Column\">\n"
				. "<value type=\"string\" key=\"_id\">328758:2732734:32434234</value>\n"
				. "<value type=\"string\" key=\"name\">Host</value>\n"
				. "<value type=\"int\" key=\"precision\">0</value>\n"
				. "<value type=\"int\" key=\"scale\">0</value>\n"
				. "<value type=\"int\" key=\"isNullable\">1</value>\n"
				. "<value type=\"int\" key=\"length\">30</value>\n"
				. "<value type=\"string\" key=\"datatypeName\">varchar</value>\n"
				. "<value type=\"string\" key=\"defaultValue\">testing</value>\n"
				. "<value type=\"string\" key=\"characterSetName\">default</value>\n"
				. "<value type=\"string\" key=\"collationName\">default</value>\n"
				. "<value type=\"string\" option=\"ref\" key=\"simpleType\">423423:89734566:746345234</value>\n"
				. "</value>\n" . "</value>\n" . "</value>\n";
		$xml = Grt::prepareGrtXml($grtXml);

		$root = Grt::getObjectsFromGrtXml($xml);
		assert($root != null);

		$list = $root->getObject("columns");

		assert($list != null);

		$columnList = $list;

		$column = $columnList->get(0);

		assert($column->getDatatypeName() == "varchar");

		$simpleDatatype = $column->getSimpleType();

		assert($simpleDatatype->getCharacterMaximumLength() == 255);
		
		$xml2 = Grt::prepareGrtXml($root->getGrtXml(""));

		assert($xml2 == $xml);
	}
	
	/**
	 * Tests the callModuleFunction function by calling a non-existing function
	 */
	public function testCallModuleFunctionNonExistingFunction() {
		$xml = Grt::callModuleFunction("PhpTestModule",
				"nonExistingFuntion", "", "");

		$result = Grt::getObjectsFromGrtXml($xml);

		assert($result->getObject("error") != null);
		assert($result->getObject("detail") != null);
	}

	/**
	 * Tests the callModuleFunction function by calling a function that raises
	 * an exception
	 */
	public function testException() {
		$xml = Grt::callModuleFunction("PhpTestModule", "throwException",
				"", "");

		$result = Grt::getObjectsFromGrtXml($xml);

		assert($result->getObject("error") != null);
		assert($result->getObject("detail") != null);
		assert($result->getObject("error") == "Exception Test");
	}

	public function testGetGlobalObjects() {
		// Initialize callback with dummy implementation
		Grt::getInstance()->setCallback("GrtCallbackTest", "");
		
		// Get a global object from that dummy implementation
		$obj = Grt::getInstance()->getGrtGlobalAsGrtObject(
				"/testObject");

		// Check if members return the correct values
		assert($obj->getName() == "TestObject");
		assert($obj->get_id() == "123456789");

		// Get a Column object
		$col = Grt::getInstance()->getGrtGlobalAsGrtObject("/column");

		// Check its name
		assert($col->getName() == "TestColumn");

		// Now follow a object reference by id
		$columnDatatype = $col->getSimpleType();

		// Check if the reference worked and we get the correct member values
		//Assert.assertEquals(columnDatatype.getName(), "VARCHAR");
		//Assert.assertEquals(columnDatatype.getCharacterMaximumLength(), 255);		
		
		// Get the SimpleDatatype directly
		$datatype = Grt::getInstance()->getGrtGlobalAsGrtObject("/simpleDatatype");
		
		assert($datatype->getName() == "VARCHAR");
		assert($datatype->getCharacterMaximumLength() == 255);	

		// Test is the two objects we have created are represented by
		// the "same" PHP object (not ===)
		assert($datatype == $columnDatatype);
	}
}

include('GrtInit.php');
include('modules/PhpTestModule.php');

// run tests
$testClass = new GrtTest();

$testClass->testPrepareGrtXml();
$testClass->testGetObjectsFromGrtXml();
$testClass->testCallModuleFunctionNonExistingFunction();
$testClass->testException();
$testClass->testGetGlobalObjects();

unset($testClass);
?>