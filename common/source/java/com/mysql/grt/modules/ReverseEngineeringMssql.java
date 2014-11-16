package com.mysql.grt.modules;

import java.sql.*;

import com.mysql.grt.*;
import com.mysql.grt.db.IndexColumn;
import com.mysql.grt.db.mssql.*;

/**
 * GRT Reverse Engineering Class for MSSQL 2000
 * 
 * @author MikeZ
 * @version 1.0, 05/23/04
 * 
 */
public class ReverseEngineeringMssql extends ReverseEngineeringGeneric {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(ReverseEngineeringMssql.class,
				"ReverseEngineering");
	}

	/*
	 * private static String catalogsSelect = "SELECT TABLE_SCHEMA FROM
	 * INFORMATION_SCHEMA.TABLES " + "GROUP BY TABLE_SCHEMA UNION " + "SELECT
	 * ROUTINE_SCHEMA AS SCHEMANAME FROM INFORMATION_SCHEMA.ROUTINES " + "GROUP
	 * BY ROUTINE_SCHEMA";
	 */

	/**
	 * Returns a list of all catalogs from the given JDBC connection
	 * 
	 * @param dbConn
	 *            the connection to use
	 * 
	 * @return returns a GRT XML string containing a list of schemata names
	 */
	public static GrtStringList getCatalogs(
			com.mysql.grt.db.mgmt.Connection dbConn) throws Exception {
		GrtStringList catalogList = new GrtStringList();

		GrtStringHashMap paramValues = dbConn.getParameterValues();
		paramValues.add("database", "");

		// connect to the database
		Connection conn = establishConnection(dbConn);
		try {

			Grt.getInstance().addMsg("Fetching catalog list.");
			Grt.getInstance().addMsgDetail("CALL sp_databases;");
			Grt.getInstance().flushMessages();

			CallableStatement stmt = conn.prepareCall("sp_databases");
			try {
				ResultSet rset = stmt.executeQuery();
				while (rset.next()) {
					catalogList.add(rset.getString(1));
				}
			} finally {
				stmt.close();
			}
		} finally {
			conn.close();
		}

		return catalogList;
	}

	private static String schemataSelect = "SELECT TABLE_SCHEMA AS SCHEMANAME, "
			+ " max(TABLE_CATALOG) AS CATALOGNAME FROM INFORMATION_SCHEMA.TABLES "
			+ "GROUP BY TABLE_SCHEMA UNION "
			+ "SELECT ROUTINE_SCHEMA AS SCHEMANAME, max(ROUTINE_CATALOG) AS CATALOGNAME "
			+ "FROM INFORMATION_SCHEMA.ROUTINES " + "GROUP BY ROUTINE_SCHEMA";

	private static String schemataSelect70 = "SELECT TABLE_SCHEMA AS SCHEMANAME, "
			+ " max(TABLE_CATALOG) AS CATALOGNAME FROM INFORMATION_SCHEMA.TABLES "
			+ "GROUP BY TABLE_SCHEMA";

	/**
	 * Returns a list of all schemata from the given JDBC connection
	 * 
	 * @param dbConn
	 *            the connection to use
	 * 
	 * @return returns a GRT XML string containing a list of schemata names
	 */
	public static GrtStringList getSchemata(
			com.mysql.grt.db.mgmt.Connection dbConn) throws Exception {

		GrtStringList schemataList = new GrtStringList();

		// connect to the database
		Connection conn = establishConnection(dbConn);
		try {
			String sql;

			DatabaseMetaData metaData = conn.getMetaData();
			if (metaData.getDatabaseMajorVersion() <= 7)
				sql = schemataSelect70;
			else
				sql = schemataSelect;

			Grt.getInstance().addMsg("Fetching schemata list.");
			Grt.getInstance().addMsgDetail(sql);
			Grt.getInstance().flushMessages();

			Statement stmt = conn.createStatement();
			try {
				ResultSet rset = stmt.executeQuery(sql);
				while (rset.next()) {
					schemataList.add(rset.getString(2) + "."
							+ rset.getString(1));
				}
			} finally {
				stmt.close();
			}
		} finally {
			conn.close();
		}

		Grt.getInstance().addMsg("Return schemata list.");
		Grt.getInstance().flushMessages();

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

		boolean reverseEngineerOnlyTableObjects = (Grt.getInstance()
				.getGrtGlobalAsInt(
						"/migration/applicationData/"
								+ "reverseEngineerOnlyTableObjects") == 1);

		// connect to the database
		Connection conn = establishConnection(dbConn);

		// create reveng instance
		ReverseEngineeringMssql revEng = new ReverseEngineeringMssql();

		Catalog catalog = new Catalog(null);
		catalog.setName(dbConn.getParameterValues().get("database"));

		catalog.setVersion(getVersion(dbConn));

		Grt.getInstance().addMsg("Build simple MSSQL datatypes.");
		Grt.getInstance().flushMessages();
		revEng.buildSimpleDatatypes(dbConn, catalog);

		for (int i = 0; i < schemataList.size(); i++) {
			String schemaName = schemataList.get(i);
			schemaName = schemaName.substring(catalog.getName().length() + 1);

			Schema schema = new Schema(catalog);
			schema.setName(schemaName);
			catalog.getSchemata().add(schema);

			// Get Tables
			if (revEng.reverseEngineerTables(conn, catalog, schema) == 0
					&& !reverseEngineerOnlyTableObjects) {

				// Get Views
				revEng.reverseEngineerViews(conn, catalog, schema);

				// Get SPs
				revEng.reverseEngineerProcedures(conn, catalog, schema);
			}
		}

		// make sure the Fks use real references instead of
		// text names where possible
		revEng.reverseEngineerUpdateFkReferences(catalog);

		return catalog;
	}

	protected void buildSimpleDatatypes(
			com.mysql.grt.db.mgmt.Connection dbConn, Catalog catalog) {
		com.mysql.grt.db.mgmt.Driver driver = dbConn.getDriver();
		com.mysql.grt.db.mgmt.Rdbms rdbms = (com.mysql.grt.db.mgmt.Rdbms) driver
				.getOwner();
		com.mysql.grt.db.SimpleDatatypeList rdbmsDatatypeList = rdbms
				.getSimpleDatatypes();
		com.mysql.grt.db.SimpleDatatypeList schemaDatatypeList = new com.mysql.grt.db.SimpleDatatypeList();

		for (int i = 0; i < rdbmsDatatypeList.size(); i++) {
			schemaDatatypeList.add(rdbmsDatatypeList.get(i));
		}

		catalog.setSimpleDatatypes(schemaDatatypeList);
	}

	private static String tableCountSelect = "SELECT COUNT(*) AS TABLECOUNT "
			+ "FROM INFORMATION_SCHEMA.TABLES "
			+ "WHERE TABLE_TYPE='BASE TABLE' AND TABLE_SCHEMA=?";

	private static String tableSelect = "SELECT * FROM INFORMATION_SCHEMA.TABLES "
			+ "WHERE TABLE_TYPE='BASE TABLE' AND TABLE_SCHEMA=?";

	protected int reverseEngineerTables(Connection conn, Catalog catalog,
			Schema schema) throws Exception {

		int tableCount = 0;
		int currentTableNumber = 0;

		Grt.getInstance().addMsg(
				"Fetch the number of tables in the schema " + schema.getName()
						+ ".");
		Grt.getInstance().addMsgDetail(tableCountSelect);

		PreparedStatement stmt = conn.prepareStatement(tableCountSelect);
		stmt.setString(1, schema.getName());

		ResultSet tblRset = stmt.executeQuery();
		if (tblRset.next()) {
			tableCount = tblRset.getInt(1);
		}
		stmt.close();

		Grt.getInstance().addMsg(
				"Fetching " + tableCount + " table(s) of the schema "
						+ schema.getName() + ".");
		Grt.getInstance().addMsgDetail(tableSelect);
		Grt.getInstance().flushMessages();

		stmt = conn.prepareStatement(tableSelect);
		stmt.setString(1, schema.getName());

		tblRset = stmt.executeQuery();

		while (tblRset.next()) {
			// Create new table
			Table table = new Table(schema);
			schema.getTables().add(table);

			currentTableNumber++;

			table.setName(tblRset.getString("TABLE_NAME"));

			Grt.getInstance().addProgress(
					"Processing table " + table.getName() + ".",
					(currentTableNumber * 100) / tableCount);
			if (Grt.getInstance().flushMessages() != 0) {
				Grt.getInstance().addMsg("Migration canceled by user.");
				return 1;
			}

			reverseEngineerTableColumns(conn, catalog, schema, table);

			reverseEngineerTablePK(conn, catalog, schema, table);

			reverseEngineerTableIndices(conn, catalog, schema, table);

			reverseEngineerTableFKs(conn, catalog, schema, table);
		}

		Grt.getInstance().addProgress("", -1);
		Grt.getInstance().flushMessages();

		stmt.close();

		return 0;
	}

	private static String tableColumnSelect = "SELECT COLUMN_NAME, COLUMN_DEFAULT, "
			+ " IS_NULLABLE, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, CHARACTER_OCTET_LENGTH, "
			+ " NUMERIC_PRECISION, NUMERIC_PRECISION_RADIX, NUMERIC_SCALE, DATETIME_PRECISION, "
			+ " CHARACTER_SET_CATALOG, CHARACTER_SET_SCHEMA, CHARACTER_SET_NAME, "
			+ " COLLATION_NAME, DOMAIN_CATALOG, DOMAIN_SCHEMA, DOMAIN_NAME, "
			+ " (c.status & 128) / 128 AS IS_IDENTITY_COLUMN "
			+ "FROM INFORMATION_SCHEMA.COLUMNS, sysobjects t, sysusers u, syscolumns c "
			+ "WHERE TABLE_SCHEMA=? AND TABLE_NAME=? AND "
			+ "u.name=TABLE_SCHEMA AND t.name=TABLE_NAME AND "
			+ "u.uid=t.uid AND c.id=t.id AND "
			+ "c.name=COLUMN_NAME "
			+ "ORDER BY ORDINAL_POSITION";

	private static String tableColumnSelect2005 = "SELECT COLUMN_NAME, COLUMN_DEFAULT, "
		+ " IS_NULLABLE, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, CHARACTER_OCTET_LENGTH, "
		+ " NUMERIC_PRECISION, NUMERIC_PRECISION_RADIX, NUMERIC_SCALE, DATETIME_PRECISION, "
		+ " CHARACTER_SET_CATALOG, CHARACTER_SET_SCHEMA, CHARACTER_SET_NAME, "
		+ " COLLATION_NAME, DOMAIN_CATALOG, DOMAIN_SCHEMA, DOMAIN_NAME, "
		+ " (c.status & 128) / 128 AS IS_IDENTITY_COLUMN "
		+ "FROM INFORMATION_SCHEMA.COLUMNS, sysobjects t, syscolumns c "
		+ "WHERE TABLE_SCHEMA=? AND TABLE_NAME=? AND "
		+ "t.name=TABLE_NAME AND "
		+ "c.id=t.id AND "
		+ "c.name=COLUMN_NAME "
		+ "ORDER BY ORDINAL_POSITION";

	protected void reverseEngineerTableColumns(Connection conn,
			Catalog catalog, Schema schema, Table table) {

		try {
			Grt.getInstance().addMsg("Fetching column information.");

			int version = conn.getMetaData().getDatabaseMajorVersion();
			String columnSelect;
			switch (version)
			{
				case 9:
					// Starting with version 9 the assignments between schemata and sysusers seems to have changed. 
					columnSelect = tableColumnSelect2005;
					break;
				default:
					columnSelect = tableColumnSelect;
			}
			Grt.getInstance().addMsgDetail(columnSelect);

			PreparedStatement stmt = conn.prepareStatement(columnSelect);
			stmt.setString(1, schema.getName());
			stmt.setString(2, table.getName());

			ResultSet colRset = stmt.executeQuery();
			while (colRset.next()) {
				// create new column
				Column column = new Column(table);
				table.getColumns().add(column);

				column.setName(colRset.getString("COLUMN_NAME"));
				column.setDatatypeName(colRset.getString("DATA_TYPE"));

				// Get Simple Type
				int datatypeIndex = catalog.getSimpleDatatypes()
						.getIndexOfName(column.getDatatypeName().toUpperCase());

				if (datatypeIndex > -1) {
					column.setSimpleType(catalog.getSimpleDatatypes().get(
							datatypeIndex));
				} else {
					column.setSimpleType(catalog.getSimpleDatatypes().get(
							catalog.getSimpleDatatypes().getIndexOfName(
									"VARCHAR")));
					column.setLength(255);

					Grt.getInstance().addMsg(
							"WARNING: The datatype " + column.getDatatypeName()
									+ " was not been defined yet.");
				}

				column.setLength(colRset.getInt("CHARACTER_MAXIMUM_LENGTH"));
				column.setPrecision(colRset.getInt("NUMERIC_PRECISION"));
				column.setScale(colRset.getInt("NUMERIC_SCALE"));

				// Nullable
				if (colRset.getString("IS_NULLABLE").compareToIgnoreCase("YES") == 0) {
					column.setIsNullable(1);
				} else {
					column.setIsNullable(0);
				}

				// Character set
				column.setCharacterSetName(colRset
						.getString("CHARACTER_SET_NAME"));

				column.setCollationName(colRset.getString("COLLATION_NAME"));

				// Default Value
				String defaultValue = colRset.getString("COLUMN_DEFAULT");
				if (defaultValue != null) {
					// remove () and (N) from (N'xxxx') but not from (NULL)
					if ((defaultValue.startsWith("(N")) && (defaultValue
							.endsWith(")"))
							&& !(defaultValue.equalsIgnoreCase("(NULL)")))
						defaultValue = defaultValue.substring(2, defaultValue
								.length() - 1);
					else if (!defaultValue.equals(""))
						defaultValue = defaultValue.substring(1, defaultValue
								.length() - 1);

					// remove (()) from numeric types
					if ((column.getSimpleType().getGroup().getName()
							.equals("numeric"))
							&& defaultValue.startsWith("((")
							&& defaultValue.endsWith("))")) {
						defaultValue = defaultValue.substring(2, defaultValue
								.length() - 3);
					}
					
					// remove () from numeric types
					if ((column.getSimpleType().getGroup().getName()
							.equals("numeric"))
							&& defaultValue.startsWith("(")
							&& defaultValue.endsWith(")")) {
						defaultValue = defaultValue.substring(1, defaultValue
								.length() - 1);
					}

					column.setDefaultValue(defaultValue);
				}

				// Identity column
				column.setIdentity(colRset.getInt("IS_IDENTITY_COLUMN"));
			}

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String tablePKSP = "sp_pkeys @table_owner=?, @table_name=?";

	protected void reverseEngineerTablePK(Connection conn, Catalog catalog,
			Schema schema, Table table) {

		// String sql;

		try {
			Grt.getInstance().addMsg("Fetching primary key information.");
			Grt.getInstance().addMsgDetail(tablePKSP);

			PreparedStatement stmt = conn.prepareCall(tablePKSP);
			stmt.setString(1, schema.getName());
			stmt.setString(2, table.getName());

			ResultSet colRset = stmt.executeQuery();

			Index primaryKey = null;

			while (colRset.next()) {
				if (primaryKey == null) {
					primaryKey = new Index(table);
					primaryKey.setName("PRIMARY");

					primaryKey.setIsPrimary(1);

					table.getIndices().add(primaryKey);

					table.setPrimaryKey(primaryKey);
				}

				IndexColumn indexColumn = new IndexColumn(primaryKey);
				indexColumn.setName(colRset.getString("COLUMN_NAME"));
				indexColumn.setColumnLength(0);
				indexColumn.setDescend(0);

				// find reference table column
				for (int j = 0; j < table.getColumns().size(); j++) {
					Column column = (Column) (table.getColumns().get(j));

					if (column.getName().compareToIgnoreCase(
							indexColumn.getName()) == 0) {
						indexColumn.setReferedColumn(column);
						break;
					}
				}

				primaryKey.getColumns().add(indexColumn);
			}

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String tableIndexSelect = "SELECT u.name as TABLE_SCHEMA, "
			+ " o.name as TABLE_NAME, i.name AS INDEX_NAME, c.name AS COLUMN_NAME, "
			+ " (i.status & 1) AS IGNORE_DUPLICATE_KEYS, "
			+ " (i.status & 2) / 2 AS IS_UNIQUE, "
			+ " (i.status & 4) / 4 AS IGNORE_DUPLICATE_ROWS, "
			+ " (i.status & 16) / 16 AS IS_CLUSTERED, "
			+ " (i.status & 2048) / 2048 AS IS_PRIMARY_KEY, "
			+ " (i.status & 4096) / 4096 AS IS_UNIQUE_KEY "
			+ "FROM sysindexes i, sysobjects o, sysusers u, sysindexkeys k, syscolumns c "
			+ "WHERE u.uid=o.uid AND i.id = o.id AND k.indid=i.indid AND k.id=i.id AND "
			+ " c.id=i.id AND c.colid=k.colid AND "
			+ " i.indid > 0 AND i.indid < 255 AND o.type = 'U' AND "
			+ " (i.status & 64)=0 AND (i.status & 8388608)=0 AND "
			+ " (i.status & 2048)=0 AND " + " u.name=? AND o.name=? "
			+ "ORDER BY i.name, k.keyno";

	protected void reverseEngineerTableIndices(Connection conn,
			Catalog catalog, Schema schema, Table table) {

		try {
			Grt.getInstance().addMsg("Fetching indices information.");
			Grt.getInstance().addMsgDetail(tableIndexSelect);

			PreparedStatement stmt = conn.prepareStatement(tableIndexSelect);
			stmt.setString(1, schema.getName());
			stmt.setString(2, table.getName());

			ResultSet rset = stmt.executeQuery();

			String indexName = "";
			Index index = null;

			while (rset.next()) {
				String newIndexName = rset.getString("INDEX_NAME");

				if ((rset.getInt("IS_PRIMARY_KEY") != 0)
						|| (newIndexName.equalsIgnoreCase("PRIMARY")))
					continue;

				if (indexName.compareToIgnoreCase(newIndexName) != 0) {
					if (index != null)
						table.getIndices().add(index);

					index = new Index(table);
					index.setName(newIndexName);
					indexName = newIndexName;

					if (rset.getInt("IS_UNIQUE") != 0)
						index.setUnique(1);
					else
						index.setUnique(0);

					index.setDeferability(0);

					index.setClustered(rset.getInt("IS_CLUSTERED"));
					index.setIgnoreDuplicateRows(rset
							.getInt("IGNORE_DUPLICATE_ROWS"));
				}

				IndexColumn indexColumn = new IndexColumn(index);
				indexColumn.setName(rset.getString("COLUMN_NAME"));
				indexColumn.setColumnLength(0);
				indexColumn.setDescend(0);

				// find reference table column
				for (int j = 0; j < table.getColumns().size(); j++) {
					Column column = (Column) (table.getColumns().get(j));

					if (column.getName().compareToIgnoreCase(
							indexColumn.getName()) == 0) {
						indexColumn.setReferedColumn(column);
						break;
					}
				}

				index.getColumns().add(indexColumn);
			}

			if (index != null)
				table.getIndices().add(index);

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String tableFKSelect = "SELECT fk.name AS CONSTRAINT_NAME, "
			+ " c.name AS COLUMN_NAME, ref_u.name AS REF_SCHEMA_NAME, "
			+ " ref_tbl.name AS REF_TABLE_NAME, ref_c.name AS REF_COLUMN_NAME, "
			+ " CASE WHEN (ObjectProperty(sfk.constid, 'CnstIsUpdateCascade')=1) THEN "
			+ "  'CASCADE' ELSE 'NO ACTION' END as UPDATE_RULE, "
			+ " CASE WHEN (ObjectProperty(sfk.constid, 'CnstIsDeleteCascade')=1) THEN "
			+ "  'CASCADE' ELSE 'NO ACTION' END as DELETE_RULE "
			+ "FROM sysusers u, sysobjects t, sysobjects fk, sysforeignkeys sfk, "
			+ " syscolumns c, sysobjects ref_tbl, sysusers ref_u, syscolumns ref_c "
			+ "WHERE u.name=? AND t.name=? AND "
			+ " t.uid=u.uid AND t.xtype='U' AND "
			+ " sfk.fkeyid=t.id AND fk.id=sfk.constid AND "
			+ " c.id=t.id AND c.colid=sfk.fkey AND "
			+ " ref_tbl.id=sfk.rkeyid AND ref_tbl.uid=ref_u.uid AND "
			+ " ref_c.id=ref_tbl.id AND ref_c.colid=sfk.rkey "
			+ "ORDER BY sfk.constid, sfk.keyno";

	protected void reverseEngineerTableFKs(Connection conn, Catalog catalog,
			Schema schema, Table table) {

		try {
			Grt.getInstance().addMsg("Fetching FK information.");
			Grt.getInstance().addMsgDetail(tableFKSelect);

			PreparedStatement stmt = conn.prepareStatement(tableFKSelect);
			stmt.setString(1, schema.getName());
			stmt.setString(2, table.getName());

			ResultSet rset = stmt.executeQuery();

			String fkName = "";
			ForeignKey foreignKey = null;

			while (rset.next()) {
				String newFkName = rset.getString("CONSTRAINT_NAME");

				if (fkName.compareToIgnoreCase(newFkName) != 0) {
					if (foreignKey != null)
						table.getForeignKeys().add(foreignKey);

					fkName = newFkName;

					foreignKey = new ForeignKey(table);
					foreignKey.setName(newFkName);

					foreignKey.setDeferability(0);

					foreignKey.setDeleteRule(rset.getString("DELETE_RULE"));
					foreignKey.setUpdateRule(rset.getString("UPDATE_RULE"));

					foreignKey.setReferedTableSchemaName(rset
							.getString("REF_SCHEMA_NAME"));
					foreignKey.setReferedTableName(rset
							.getString("REF_TABLE_NAME"));
				}

				foreignKey.getReferedColumnNames().add(
						rset.getString("REF_COLUMN_NAME"));

				// find reference table column
				String colName = rset.getString("COLUMN_NAME");
				for (int j = 0; j < table.getColumns().size(); j++) {
					Column column = (Column) (table.getColumns().get(j));

					if (column.getName().compareToIgnoreCase(colName) == 0)
						foreignKey.getColumns().add(column);
				}
			}

			if (foreignKey != null)
				table.getForeignKeys().add(foreignKey);

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String viewSelect = "SELECT * FROM INFORMATION_SCHEMA.VIEWS "
			+ "WHERE TABLE_SCHEMA=?";

	protected void reverseEngineerViews(Connection conn, Catalog catalog,
			Schema schema) throws Exception {

		Grt.getInstance().addMsg(
				"Fetch all views of the schema " + schema.getName() + ".");
		Grt.getInstance().addMsgDetail(viewSelect);
		Grt.getInstance().flushMessages();

		PreparedStatement stmt = conn.prepareStatement(viewSelect);
		stmt.setString(1, schema.getName());

		ResultSet rset = stmt.executeQuery();

		while (rset.next()) {
			// Create new view
			View view = new View(schema);
			schema.getViews().add(view);

			view.setName(rset.getString("TABLE_NAME"));

			Grt.getInstance().addMsg("Processing view " + view.getName() + ".");

			view.setQueryExpression(rset.getString("VIEW_DEFINITION"));

			if (rset.getString("CHECK_OPTION").equalsIgnoreCase("NONE"))
				view.setWithCheckCondition(0);
			else
				view.setWithCheckCondition(1);
		}

		stmt.close();

		Grt.getInstance().addMsg("Views fetched.");
	}

	private static String procedureCountSelect = "SELECT COUNT(*) AS NUM "
			+ "FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_SCHEMA=?";

	private static String procedureSelect = "SELECT ROUTINE_NAME, ROUTINE_TYPE, "
			+ "MODULE_NAME, ROUTINE_DEFINITION "
			+ "FROM INFORMATION_SCHEMA.ROUTINES WHERE ROUTINE_SCHEMA=?";

	protected int reverseEngineerProcedures(Connection conn, Catalog catalog,
			Schema schema) throws Exception {
		int spCount = 0;
		int currentSpNumber = 0;

		Grt.getInstance().addMsg(
				"Fetch count of stored procedures of the schema "
						+ schema.getName() + ".");
		Grt.getInstance().addMsgDetail(procedureCountSelect);

		try {
			PreparedStatement stmt = conn
					.prepareStatement(procedureCountSelect);
			stmt.setString(1, schema.getName());

			ResultSet rset = stmt.executeQuery();

			if (rset.next()) {
				spCount = rset.getInt("NUM");
			}

			Grt.getInstance().addMsg(
					"Fetching " + spCount
							+ " stored procedure(s) of the schema "
							+ schema.getName() + ".");
			Grt.getInstance().addMsgDetail(procedureSelect);
			Grt.getInstance().flushMessages();

			stmt = conn.prepareStatement(procedureSelect);
			stmt.setString(1, schema.getName());

			rset = stmt.executeQuery();

			while (rset.next()) {
				// Create new view
				Routine proc = new Routine(schema);
				schema.getRoutines().add(proc);

				proc.setName(rset.getString("ROUTINE_NAME"));

				currentSpNumber++;

				Grt.getInstance().addProgress(
						"Processing procedure " + proc.getName() + ".",
						(currentSpNumber * 100) / spCount);
				if (Grt.getInstance().flushMessages() != 0) {
					Grt.getInstance().addMsg("Migration canceled by user.");
					return 1;
				}

				Grt.getInstance().addMsg(
						"Processing procedure " + proc.getName() + ".");

				proc.setRoutineType(rset.getString("ROUTINE_TYPE"));

				proc.setRoutineCode(rset.getString("ROUTINE_DEFINITION"));
			}

			stmt.close();

			Grt.getInstance().addMsg("Routines fetched.");

			Grt.getInstance().addProgress("", -1);

		} catch (Exception e) {
			Grt.getInstance().addMsg("Stored procedures cannot be fetched.");
			Grt.getInstance().addMsgDetail(e.getMessage());
		}

		return 0;
	}
}