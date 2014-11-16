package com.mysql.grt.modules;

import junit.framework.TestCase;
import com.mysql.grt.*;

public class ReverseEngineeringAccessTest extends TestCase {

	public ReverseEngineeringAccessTest(String name) {
		super(name);
	}

	/**
	 * main function so the test can be executed
	 */
	public static void main(String[] args) {
		junit.textui.TestRunner.run(ReverseEngineeringAccessTest.class);
	}

	/**
	 * Test of the reverseEngineering
	 */
	public void testReverseEngineering() {
		GrtStringList schemaList = new GrtStringList();
		schemaList.add("Nordwind");

		/*try {
			ReverseEngineeringAccess
					.reverseEngineer(
							"sun.jdbc.odbc.JdbcOdbcDriver",
							"jdbc:odbc:Driver={Microsoft Access Driver (*.mdb)};"
									+ "DBQ=C:\\Dokumente und Einstellungen\\Mike\\"
									+ "Eigene Dateien\\Nordwind.mdb;DriverID=22;READONLY=true}",
							schemaList);
		} catch (Exception e) {
			e.printStackTrace();
		}*/
	}
}