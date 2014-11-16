package com.mysql.grt.modules;

import java.sql.*;

import com.mysql.grt.*;
import com.mysql.grt.db.mysql.*;

/**
 * GRT Reverse Engineering Class for Oracle 8i/9i
 * 
 * @author Mike
 * @version 1.0, 03/30/05
 * 
 */
public class ReverseEngineeringMysqlJdbc extends ReverseEngineeringGeneric {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(ReverseEngineeringMysqlJdbc.class,
				"ReverseEngineering");
	}

	private static String schemataSelect = "SHOW DATABASES";

	/**
	 * Returns a list of all schemata from the given JDBC connection
	 * 
	 * @param jdbcDriver
	 *            the class name of the JDBC driver
	 * @param jdbcConnectionString
	 *            a JDBC connection string
	 * @return returns a GRT XML string containing a list of schemata names
	 */
	public static GrtStringList getSchemata(
			com.mysql.grt.db.mgmt.Connection dbConn) throws Exception {

		Connection conn = establishConnection(dbConn);

		Grt.getInstance().addMsg("Fetching schemata list.");
		Grt.getInstance().addMsgDetail(schemataSelect);

		GrtStringList schemataList = new GrtStringList();

		Statement stmt = conn.createStatement();
		ResultSet rset = stmt.executeQuery(schemataSelect);
		while (rset.next()) {
			schemataList.add(rset.getString(1));
		}
		stmt.close();
		conn.close();

		Grt.getInstance().addMsg("Return schemata list.");
		return schemataList;
	}

	/**
	 * Does the reverse engineering of the given schematas over the JDBC
	 * connection and returns the GRT objects
	 * 
	 * @param jdbcDriver
	 *            the class name of the JDBC driver
	 * @param jdbcConnectionString
	 *            a JDBC connection string
	 * @param schemataList
	 *            list of schematas to be reverse engineered
	 * @return returns a GRT XML string containing a the reverse engineered
	 *         objects
	 */
	public static com.mysql.grt.db.Catalog reverseEngineer(
			com.mysql.grt.db.mgmt.Connection dbConn, GrtStringList schemataList)
			throws Exception {

		// build parameter list
		GrtList params = new GrtList();
		params.addObject(dbConn);
		params.addObject(schemataList);
		params.addObject(new Integer((Grt.getInstance()
				.getGrtGlobalAsInt("/migration/applicationData/"
						+ "reverseEngineerOnlyTableObjects"))));

		// call native function
		Catalog catalog = (Catalog) Grt.getInstance().callGrtFunction(
				"ReverseEngineeringMysql", "reverseEngineer", params);

		return catalog;
	}
}