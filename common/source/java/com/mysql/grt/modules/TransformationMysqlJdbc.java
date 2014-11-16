package com.mysql.grt.modules;

import java.sql.*;

import com.mysql.grt.*;
import com.mysql.grt.base.*;
import com.mysql.grt.db.mysql.*;

/**
 * GRT Migration Class
 * 
 * @author MikeZ
 * @version 1.0, 01/25/05
 * 
 */
public class TransformationMysqlJdbc {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(TransformationMysqlJdbc.class, "");
	}

	/**
	 * Generates all SQL create statements for the given catalog. The SQL
	 * statement will be set in each database object, e.g. schemata, tables,
	 * views, ...
	 * 
	 * @param catalog
	 *            the catalog object to create the SQL for
	 */
	public static void generateSqlCreateStatements(Catalog catalog,
			GrtHashMap options) {
		GrtList args = new GrtList();

		args.addObject(catalog);
		args.addObject(options);

		try {
			Grt.getInstance().callGrtFunction("TransformationMysql",
					"generateSqlCreateStatements", args);
		} catch (RuntimeException e) {
			// generate sql for tables
			Grt.getInstance().addMsg(
					"The following error occured while "
							+ "creating the SQL code: " + e.getMessage());
			Grt.getInstance().flushMessages();
		}
	}

	/**
	 * Generates the SQL create code for the given object
	 * 
	 * @param obj
	 *            the database object to create the SQL for
	 * @return the SQL create code as String
	 */
	public static String getSqlScript(Catalog catalog, GrtHashMap options) {
		String sql = "";
		GrtList args = new GrtList();

		args.addObject(catalog);
		args.addObject(options);

		try {
			sql = (String) Grt.getInstance().callGrtFunction(
					"TransformationMysql", "getSqlScript", args);
		} catch (RuntimeException e) {
			Grt.getInstance().addMsg(
					"The following error occured while "
							+ "creating the SQL code: " + e.getMessage());
			Grt.getInstance().flushMessages();
		}

		return sql;
	}

	/**
	 * Generates the SQL create code for the given object
	 * 
	 * @param obj
	 *            the database object to create the SQL for
	 * @return the SQL create code as String
	 */
	public static String getSqlCreate(com.mysql.grt.db.DatabaseObject obj) {
		String sql;

		try {
			sql = (String) Grt.getInstance().callGrtFunction(
					"TransformationMysql", "getSqlCreate", obj);
		} catch (RuntimeException e) {
			sql = "/*" + e.getMessage() + "*/";
		}

		return sql;
	}

	/**
	 * Generates the SQL create code for the given object
	 * 
	 * @param obj
	 *            the database object to create the SQL for
	 * @return the SQL create code as String
	 */
	public static String getSqlDrop(com.mysql.grt.db.DatabaseObject obj) {
		String sql;

		try {
			sql = (String) Grt.getInstance().callGrtFunction(
					"TransformationMysql", "getSqlDrop", obj);
		} catch (RuntimeException e) {
			sql = "/*" + e.getMessage() + "*/";
		}

		return sql;
	}

	/**
	 * Generates the SQL create code for the given object
	 * 
	 * @param obj
	 *            the database object to create the SQL for
	 * @return the SQL create code as String
	 */
	public static String getSqlMerge(com.mysql.grt.db.DatabaseObject obj) {
		String sql;

		try {
			sql = (String) Grt.getInstance().callGrtFunction(
					"TransformationMysql", "getSqlMerge", obj);
		} catch (RuntimeException e) {
			sql = "/*" + e.getMessage() + "*/";
		}

		return sql;
	}

	/**
	 * Execute all SQL statements for all objects in the given catalog. The SQL
	 * statement is taken from the object's sql member
	 * 
	 * @param jdbcDriver
	 *            class name of the jdbc driver
	 * @param jdbcConnectionString
	 *            jdbc connection string to the target database
	 * @param catalog
	 *            the catalog to process
	 */
	public static void executeSqlStatements(
			com.mysql.grt.db.mgmt.Connection dbConn, Catalog catalog,
			ObjectLogList logList) throws Exception {

		new TransformationMysqlJdbc().doExecuteSqlStatements(dbConn, catalog,
				logList);
	}

	protected void doExecuteSqlStatements(
			com.mysql.grt.db.mgmt.Connection dbConn, Catalog catalog,
			ObjectLogList logList) throws Exception {

		Connection conn = ReverseEngineeringGeneric.establishConnection(dbConn);
		String sql;

		// Statement stmt = conn.createStatement();

		try {
			sql = (String) Grt.getInstance().callGrtFunction(
					"TransformationMysql", "getScriptHeader", null);

			executeSqlStatements(conn, sql, "Execute script header commands.");
		} catch (SQLException e) {
			// tollerate exception
		}

		// Create schemata
		for (int i = 0; i < catalog.getSchemata().size(); i++) {
			Schema schema = (Schema) catalog.getSchemata().get(i);

			// Create schema
			executeSqlStatement(conn, schema, "Creating schema "
					+ schema.getName() + " ...", logList);

			// Create tables
			Grt.getInstance().addMsg("Creating tables ...");

			for (int j = 0; j < schema.getTables().size(); j++) {
				Table table = (Table) schema.getTables().get(j);

				Grt.getInstance().addProgress(
						"Creating table " + table.getName(),
						(j * 100) / schema.getTables().size());
				Grt.getInstance().flushMessages();

				executeSqlStatement(conn, table, "Creating table "
						+ table.getName() + " ...", logList);
			}

			// Create views
			Grt.getInstance().addMsg("Creating views ...");

			for (int j = 0; j < schema.getViews().size(); j++) {
				View view = (View) schema.getViews().get(j);

				Grt.getInstance().addProgress(
						"Creating view " + view.getName(),
						(j * 100) / schema.getViews().size());
				Grt.getInstance().flushMessages();

				executeSqlStatement(conn, view, "Creating view "
						+ view.getName() + " ...", logList);
			}

			// Create procedures
			Grt.getInstance().addMsg("Creating procedures ...");

			for (int j = 0; j < schema.getRoutines().size(); j++) {
				Routine proc = (Routine) schema.getRoutines().get(j);

				Grt.getInstance().addProgress(
						"Creating view " + proc.getName(),
						(j * 100) / schema.getRoutines().size());
				Grt.getInstance().flushMessages();

				executeSqlStatement(conn, proc, "Creating procedure "
						+ proc.getName() + " ...", logList);
			}

			// Hide progress bar
			Grt.getInstance().addProgress("", -1);
			Grt.getInstance().flushMessages();
		}

		try {
			sql = (String) Grt.getInstance().callGrtFunction(
					"TransformationMysql", "getScriptFooter", null);

			executeSqlStatements(conn, sql, "Execute script footer commands.");
		} catch (SQLException e) {
			// tollerate exception
		}

		conn.close();
	}

	private void executeSqlStatement(Connection conn,
			com.mysql.grt.db.DatabaseObject obj, String action,
			ObjectLogList logList) {

		// generate a new object log
		ObjectLog objectLog = new ObjectLog(logList.getOwner());
		objectLog.setLogObject(obj);

		try {
			executeSqlStatements(conn, obj.getSql(), action);
		} catch (SQLException e) {
			ObjectLogEntry entry = new ObjectLogEntry(objectLog);
			entry.setName(e.getMessage());
			entry.setEntryType(2);

			objectLog.getEntries().add(entry);
		} finally {
			logList.add(objectLog);
		}
	}

	protected void executeSqlStatements(Connection conn, String sql,
			String action) throws SQLException {
		GrtStringList cmds;

		cmds = (GrtStringList) Grt.getInstance().callGrtFunction(
				"TransformationMysql", "splitSqlCommands", sql);

		for (int i = 0; i < cmds.size(); i++)
			executeSqlStatement(conn, cmds.get(i), action);
	}

	private void executeSqlStatement(Connection conn, String sql, String action)
			throws SQLException {

		// check if there is a SQL statement to execute
		if ((sql != null) && (!sql.equals(""))) {
			Grt.getInstance().addMsg(action);
			Grt.getInstance().addMsgDetail(sql);

			PreparedStatement stmt = conn.prepareStatement(sql);
			try {
				stmt.execute();
			} catch (SQLException e) {
				Grt.getInstance().addMsg(
						"An error occured while executing the SQL statement.");
				Grt.getInstance().addMsgDetail(e.getMessage());

				throw (e);
			} finally {
				if (stmt != null)
					stmt.close();
			}
		}
	}

	public static com.mysql.grt.db.DatabaseObject getScriptHeader(
			GrtHashMap options) {
		com.mysql.grt.db.DatabaseObject header;

		try {
			header = (com.mysql.grt.db.DatabaseObject) Grt.getInstance()
					.callGrtFunction("TransformationMysql", "getScriptHeader",
							options);
		} catch (RuntimeException e) {
			header = null;
		}

		return header;
	}

	public static com.mysql.grt.db.DatabaseObject getScriptFooter(
			GrtHashMap options) {
		com.mysql.grt.db.DatabaseObject footer;

		try {
			footer = (com.mysql.grt.db.DatabaseObject) Grt.getInstance()
					.callGrtFunction("TransformationMysql", "getScriptFooter",
							null);
		} catch (RuntimeException e) {
			footer = null;
		}

		return footer;
	}
}