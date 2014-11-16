package com.mysql.grt.modules;

import junit.framework.TestCase;
import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestSuite;

import com.mysql.grt.*;

//import com.mysql.grt.db.oracle.*;

/**
 * Test of MyxReverseEngineeringOracle class
 * 
 * @author Mike
 * @version 1.0, 11/29/04
 */
public class ReverseEngineeringOracleTest extends TestCase {

	public ReverseEngineeringOracleTest(String name) {
		super(name);
	}

	/**
	 * main function so the test can be executed
	 */
	public static void main(String[] args) {
		junit.textui.TestRunner.run(ReverseEngineeringOracleTest.class);
	}

	/**
	 * The suite function builds the testsuite
	 */
	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTest(new ReverseEngineeringOracleTest("testGetModuleInfo"));
		suite.addTest(new ReverseEngineeringOracleTest("testGetSchemata"));
		suite.addTest(new ReverseEngineeringOracleTest("testReverseEngineer"));
		return suite;
	}

	/**
	 * Test of the getModuleInfo function
	 */
	public void testGetModuleInfo() {
		String xml = ReverseEngineeringOracle.getModuleInfo();
		GrtHashMap moduleInfo = (GrtHashMap) Grt.getObjectsFromGrtXml(xml);

		Assert.assertNotNull(moduleInfo);

		//Check if functions are there
		GrtStringList functionList = (GrtStringList) moduleInfo
				.getObject("functions");
		Assert.assertNotNull(functionList);

		Assert.assertTrue(functionList
				.contains("getSchemata:(Ljava/lang/String;):"));
		Assert
				.assertTrue(functionList
						.contains("reverseEngineer:(Ljava/lang/String;Lcom/mysql/grt/GrtStringList;):"));

		//Check if extens is set correctly
		Assert.assertEquals(moduleInfo.getObject("extends"),
				"ReverseEngineering");
	}

	public void testGetSchemata() {
		/*GrtStringList schemataList = null;
		try {
			schemataList = ReverseEngineeringOracle.getSchemata(
					"oracle.jdbc.OracleDriver",
					"jdbc:oracle:thin:system/sys@mikesthinkpad:1521:mtt");
		} catch (Exception e) {
			e.printStackTrace();
		}

		Assert.assertNotNull(schemataList);*/
	}

	public void testReverseEngineer() {
		/*Catalog catalog = null;
		String xml = "<?xml version=\"1.0\"?>"
				+ "  <data>"
				+ "	  <value type=\"list\" content-type=\"\">"
				+ "    <value type=\"string\">jdbc:oracle:thin:system/sys@mikesthinkpad:1521:mtt</value>"
				+ "    <value type=\"list\" content-type=\"string\">"
				+ "      <value type=\"string\">SCOTT</value>" + "    </value>"
				+ "  </value>" + "</data>";

		String res = Grt.callModuleFunction(ReverseEngineeringOracle.class,
				"reverseEngineer",
				"(Ljava/lang/String;Lcom/mysql/grt/GrtStringList;)", xml);

		//System.out.println(res);

		//try a regular call

		GrtStringList schemata = new GrtStringList();
		schemata.add("SCOTT");

		try {
			catalog = (Catalog) ReverseEngineeringOracle.reverseEngineer(
					"oracle.jdbc.OracleDriver",
					"jdbc:oracle:thin:system/sys@mikesthinkpad:1521:mtt",
					schemata);
		} catch (Exception e) {
			e.printStackTrace();
		}

		Assert.assertNotNull(catalog);*/
	}
}