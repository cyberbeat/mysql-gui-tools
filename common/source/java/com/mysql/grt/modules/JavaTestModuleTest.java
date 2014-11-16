package com.mysql.grt.modules;

import com.mysql.grt.*;

import junit.framework.TestCase;
import junit.framework.Assert;

/**
 * Test of the Grt Base module class
 * 
 * @author Mike
 * @version 1.0, 11/29/04
 */
public class JavaTestModuleTest extends TestCase {

	/**
	 * main function so the test can be executed
	 */
	public static void main(String[] args) {
		junit.textui.TestRunner.run(JavaTestModuleTest.class);
	}

	/**
	 * Tests the callModuleFunction function by calling helloWorld()
	 */
	public void testCallModuleFunctionHelloWorld() {
		String xml = Grt.callModuleFunction(JavaTestModule.class, "helloWorld",
				"", "");

		GrtHashMap result = (GrtHashMap) Grt.getObjectsFromGrtXml(xml);

		Assert.assertNull(result.getObject("error"));
		Assert.assertEquals("Hello World!", result.getObject("value"));
	}

	/**
	 * Tests the callModuleFunction function by calling upperCase() with a
	 * single object as parameter
	 */
	public void testCallModuleFunctionUpperCase() {
		String testStr = "This is a test of the upperCase function.";
		String testParam = Grt.prepareGrtXml(Grt.getObjectAsXml(Grt
				.prepareGrtXmlHeader(), testStr));
		String xml = Grt.callModuleFunction(JavaTestModule.class, "upperCase",
				"(Ljava/lang/String;)", testParam);

		GrtHashMap result = (GrtHashMap) Grt.getObjectsFromGrtXml(xml);

		Assert.assertNull(result.getObject("error"));
		Assert.assertEquals(result.getObject("value"), testStr.toUpperCase());
	}

	/**
	 * Tests the callModuleFunction function by calling upperCase() with a
	 * GrtList as parameter
	 */
	public void testCallModuleFunctionUpperCaseWithList() {
		String testStr = "This is a test of the upperCase function.";

		GrtList paramList = new GrtList();
		paramList.addObject(testStr);

		String paramXml = Grt.prepareGrtXml(Grt.getObjectAsXml(Grt
				.prepareGrtXmlHeader(), testStr));
		String xml = Grt.callModuleFunction(JavaTestModule.class, "upperCase",
				"Ljava/lang/String;", paramXml);

		GrtHashMap result = (GrtHashMap) Grt.getObjectsFromGrtXml(xml);

		Assert.assertNull(result.getObject("error"));
		Assert.assertEquals(result.getObject("value"), testStr.toUpperCase());
	}

	/**
	 * Tests the callModuleFunction function by calling getListSize()
	 */
	public void testGetListSize() {
		GrtList testList = new GrtList();
		testList.addObject("1");
		testList.addObject("2");
		testList.addObject("3");

		GrtList paramList = new GrtList();
		paramList.addObject(testList);

		String paramXml = Grt.prepareGrtXml(Grt.getObjectAsXml(Grt
				.prepareGrtXmlHeader(), paramList));
		String xml = Grt.callModuleFunction(JavaTestModule.class,
				"getListSize", "Lcom/mysql/grt/GrtList;", paramXml);

		GrtHashMap result = (GrtHashMap) Grt.getObjectsFromGrtXml(xml);

		int listSize = ((Integer) result.getObject("value")).intValue();

		Assert.assertNull(result.getObject("error"));
		Assert.assertEquals(listSize, 3);
	}

	/**
	 * Tests the callModuleFunction function by calling concatStrings()
	 */
	public void testConcatStrings() {
		String s1 = "Hello";
		String s2 = "World";

		GrtList paramList = new GrtList();
		paramList.addObject(s1);
		paramList.addObject(s2);

		String paramXml = Grt.prepareGrtXml(Grt.getObjectAsXml(Grt
				.prepareGrtXmlHeader(), paramList));
		String xml = Grt.callModuleFunction(JavaTestModule.class,
				"concatStrings", "Ljava/lang/String;Ljava/lang/String;",
				paramXml);

		GrtHashMap result = (GrtHashMap) Grt.getObjectsFromGrtXml(xml);

		Assert.assertNull(result.getObject("error"));
		Assert.assertEquals(result.getObject("value"), s1 + s2);
	}

	public void testGetMessages() {
		String xml = Grt.callModuleFunction(BaseJava.class, "getMessages", "",
				"");

		Assert.assertNotNull(xml);

		// System.out.println(xml);
	}

	public void testGetGlobalString() {
		Grt.getInstance().setCallback("GrtCallbackTest", "");

		Assert.assertEquals("TestObject", JavaTestModule
				.getGlobalString("/testObject/name"));
	}
}