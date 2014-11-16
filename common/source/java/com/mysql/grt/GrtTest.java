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

package com.mysql.grt;

import junit.framework.TestCase;
import junit.framework.Assert;
import com.mysql.grt.db.Column;
import com.mysql.grt.db.ColumnList;
import com.mysql.grt.db.SimpleDatatype;
import com.mysql.grt.modules.JavaTestModule;

/**
 * Test of the Grt main class
 * 
 * @author Mike
 * @version 1.0, 11/29/04
 */
public class GrtTest extends TestCase {

	/**
	 * main function so the test can be executed
	 */
	public static void main(String[] args) {
		junit.textui.TestRunner.run(GrtTest.class);
	}

	/**
	 * Tests the prepareGrtXml function
	 */
	public void testPrepareGrtXml() {
		StringBuffer grtXml = new StringBuffer(
				"<value type=\"string\">test</value>");
		String xml = Grt.prepareGrtXmlFooter(
				Grt.prepareGrtXmlHeader().append(grtXml)).toString();

		Assert.assertEquals(xml, "<?xml version=\"1.0\"?>\n<data>\n" + grtXml
				+ "</data>\n");
	}

	/**
	 * Tests XML parsing and generation
	 */
	public void testGetObjectsFromGrtXml() {
		StringBuffer grtXml = Grt
				.prepareGrtXmlHeader()
				.append(
						"<value type=\"dict\">\n"
								+ "<value type=\"dict\" struct-name=\"db.SimpleDatatype\" key=\"simpleType\">\n"
								+ "<value type=\"string\" key=\"_id\">423423:89734566:746345234</value>\n"
								+ "<value type=\"string\" key=\"name\">VARCHAR</value>\n"
								+ "<value type=\"int\" key=\"characterMaximumLength\">255</value>\n"
								+ "<value type=\"int\" key=\"characterOctetLength\">0</value>\n"
								+ "<value type=\"int\" key=\"numericPrecision\">0</value>\n"
								+ "<value type=\"int\" key=\"numericPrecisionRadix\">0</value>\n"
								+ "<value type=\"int\" key=\"numericScale\">0</value>\n"
								+ "<value type=\"int\" key=\"dateTimePrecision\">0</value>\n"
								+ "</value>\n"
								+ "<value type=\"list\" content-type=\"dict\" content-struct-name=\"db.Column\" key=\"columns\">\n"
								+ "<value type=\"dict\" struct-name=\"db.Column\">\n"
								+ "<value type=\"string\" key=\"_id\">328758:2732734:32434234</value>\n"
								+ "<value type=\"string\" key=\"name\">Host</value>\n"
								+ "<value type=\"int\" key=\"precision\">0</value>\n"
								+ "<value type=\"int\" key=\"scale\">0</value>\n"
								+ "<value type=\"int\" key=\"isNullable\">1</value>\n"
								+ "<value type=\"int\" key=\"length\">30</value>\n"
								+ "<value type=\"string\" key=\"datatypeName\">varchar</value>\n"
								+ "<value type=\"string\" key=\"defaultValue\">testing</value>\n"
								+ "<value type=\"string\" key=\"characterSetName\">default</value>\n"
								+ "<value type=\"string\" key=\"collationName\">default</value>\n"
								+ "<value type=\"string\" option=\"ref\" key=\"simpleType\">423423:89734566:746345234</value>\n"
								+ "</value>\n" + "</value>\n" + "</value>\n");
		String xml = Grt.prepareGrtXmlFooter(grtXml).toString();

		GrtHashMap root = (GrtHashMap) Grt.getObjectsFromGrtXml(xml);
		Assert.assertNotNull(root);

		Object list = root.getObject("columns");

		Assert.assertNotNull(list);
		Assert.assertEquals(list.getClass().getName(),
				"com.mysql.grt.db.ColumnList");

		ColumnList columnList = (ColumnList) list;

		Column column = columnList.get(0);

		Assert.assertEquals(column.getDatatypeName(), "varchar");

		SimpleDatatype simpleDatatype = column.getSimpleType();

		/*
		 * System.out.println("Keys in the Grt object cache:");
		 * 
		 * Object[] keys = Grt.getInstance().objects.keySet().toArray(); for
		 * (int i=0; i <keys.length; i++) { System.out.println(keys[i]); }
		 */

		Assert.assertEquals(simpleDatatype.getCharacterMaximumLength(), 255);

		Assert.assertEquals(Grt.prepareGrtXmlFooter(
				root.getGrtXml(Grt.prepareGrtXmlHeader(), "")).toString(), xml);
	}

	/**
	 * Tests the callModuleFunction function by calling a non-existing function
	 */
	public void testCallModuleFunctionNonExistingFunction() {
		String xml = Grt.callModuleFunction(JavaTestModule.class,
				"nonExistingFuntion", "", "");

		GrtHashMap result = (GrtHashMap) Grt.getObjectsFromGrtXml(xml);

		Assert.assertNotNull(result.getObject("error"));
		Assert.assertNotNull(result.getObject("detail"));
	}

	/**
	 * Tests the callModuleFunction function by calling a function that raises
	 * an exception
	 */
	public void testException() {
		String xml = Grt.callModuleFunction(JavaTestModule.class,
				"throwException", "", "");

		GrtHashMap result = (GrtHashMap) Grt.getObjectsFromGrtXml(xml);

		Assert.assertNotNull(result.getObject("error"));
		Assert.assertNotNull(result.getObject("detail"));
		Assert.assertEquals(result.getObject("error"), "Exception Test");
	}

	public void testGetGlobalObjects() {
		// Initialize callback with dummy implementation
		Grt.getInstance().setCallback("GrtCallbackTest", "");

		// Get a global object from that dummy implementation
		GrtObject obj = (GrtObject) Grt.getInstance().getGrtGlobalAsGrtObject(
				"/testObject");

		// Check if members return the correct values
		Assert.assertEquals(obj.getName(), "TestObject");
		Assert.assertEquals(obj.get_id(), "123456789");

		// Get a Column object
		Column col = (Column) Grt.getInstance().getGrtGlobalAsGrtObject(
				"/column");

		// Check its name
		Assert.assertEquals(col.getName(), "TestColumn");

		// Now follow a object reference by id
		SimpleDatatype columnDatatype = col.getSimpleType();

		// Check if the reference worked and we get the correct member values
		// Assert.assertEquals(columnDatatype.getName(), "VARCHAR");
		// Assert.assertEquals(columnDatatype.getCharacterMaximumLength(), 255);

		// Get the SimpleDatatype directly
		SimpleDatatype datatype = (SimpleDatatype) Grt.getInstance()
				.getGrtGlobalAsGrtObject("/simpleDatatype");

		Assert.assertEquals(datatype.getName(), "VARCHAR");
		Assert.assertEquals(datatype.getCharacterMaximumLength(), 255);

		// Test is the two objects we have created are represented by
		// the same Java object
		Assert.assertTrue(datatype != columnDatatype);
	}

}