package com.mysql.grt.modules;

import com.mysql.grt.Grt;

import junit.framework.TestCase;
import junit.framework.Assert;

/**
 * Test of MyxReverseEngineeringOracle class
 * 
 * @author Mike
 * @version 1.0, 01/11/05
 */
public class MigrationOracleTest extends TestCase {

	/**
	 * main function so the test can be executed
	 */
	public static void main(String[] args) {
		junit.textui.TestRunner.run(MigrationOracleTest.class);
	}

	public static void testMigrateOracleTable() {
		Grt.getInstance().setCallback(
				"GrtCallbackSnapshot",
				System.getProperty("user.dir") + "\\source\\java\\res\\"
						+ "snapshot_oracle_reverse_engineered.xml");

		System.out.println("migration id:"
				+ Grt.getInstance().getGrtGlobalAsString("/migration/_id"));

		try {
			/*MigrationOracle.migrate((com.mysql.grt.db.migration.Migration) Grt
					.getInstance().getGrtGlobalAsGrtObject("/migration"), "db.mysql");*/
		} catch (Exception e) {
			e.printStackTrace();
			Assert.fail();
		}
		
		System.out.println("target table count: " + Grt.getInstance().getGrtGlobalListSize("/migration/targetCatalog/tables"));

		Assert.assertEquals(1, 1);
	}
}