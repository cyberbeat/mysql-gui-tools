package com.mysql.grt.modules;

import java.sql.*;

import com.mysql.grt.*;
import com.mysql.grt.db.sybase.*;

/**
 * GRT Reverse Engineering Class for Sybase
 * 
 * @author MikeZ
 * @version 1.0, 06/23/06
 * 
 */
public class ReverseEngineeringSybase extends ReverseEngineeringGeneric {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(ReverseEngineeringSybase.class,
				"ReverseEngineering");
	}

	private static String catalogsSelect = "SELECT name FROM master.dbo.sysdatabases "
			+ "ORDER BY 1";

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
			Grt.getInstance().addMsgDetail(catalogsSelect);
			Grt.getInstance().flushMessages();

			PreparedStatement stmt = conn.prepareStatement(catalogsSelect);
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

	private static String schemataSelect = "SELECT USERNAME = name "
			+ "FROM sysusers WHERE gid != uid ORDER BY 1";

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
			String catalog = dbConn.getParameterValues().get("database");

			Grt.getInstance().addMsg("Fetching schemata list.");
			Grt.getInstance().addMsgDetail(schemataSelect);
			Grt.getInstance().flushMessages();

			Statement stmt = conn.createStatement();
			try {
				ResultSet rset = stmt.executeQuery(schemataSelect);
				while (rset.next()) {
					schemataList.add(catalog + "." + rset.getString(1));
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
		ReverseEngineeringSybase revEng = new ReverseEngineeringSybase();

		Catalog catalog = new Catalog(null);
		catalog.setName(dbConn.getParameterValues().get("database"));

		catalog.setVersion(getVersion(dbConn));

		Grt.getInstance().addMsg("Build simple Sybase datatypes.");
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

	private static String tableCountSelect = "SELECT TABLE_COUNT = COUNT(*) "
			+ "FROM sysobjects O, sysindexes I "
			+ "WHERE USER_NAME(uid) = ? AND O.type = 'U' AND O.id = I.id AND I.indid IN (0, 1)";

	private static String tableSelect = "SELECT TABLE_NAME = O.name "
			+ "FROM sysobjects O, sysindexes I "
			+ "WHERE USER_NAME(uid) = ? AND O.type = 'U' AND O.id = I.id AND I.indid IN (0, 1) "
			+ "ORDER BY 1";

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

	private static String tableColumnSelect = "SELECT COLUMN_NAME = ISNULL(C.name, ''), "
			+ " DATA_TYPE = T.name, C.length, NUMERIC_PRECISION = C.prec, "
			+ " NUMERIC_SCALE = C.scale, "
			+ " IS_NULLABLE = CONVERT(BIT, (C.status & 0x08)) "
			+ "FROM syscolumns C, systypes T, sysobjects A "
			+ "WHERE USER_NAME(A.uid) = ? AND "
			+ " A.id = C.id AND C.id = OBJECT_ID(?) AND "
			+ "C.usertype*=T.usertype " + "ORDER BY C.colid";

	protected void reverseEngineerTableColumns(Connection conn,
			Catalog catalog, Schema schema, Table table) {

		try {
			Grt.getInstance().addMsg("Fetching column information.");
			Grt.getInstance().addMsgDetail(tableColumnSelect);

			PreparedStatement stmt = conn.prepareStatement(tableColumnSelect);
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

				column.setLength(colRset.getInt("LENGTH"));
				column.setPrecision(colRset.getInt("NUMERIC_PRECISION"));
				column.setScale(colRset.getInt("NUMERIC_SCALE"));

				// Nullable
				if (colRset.getString("IS_NULLABLE").compareToIgnoreCase("1") == 0) {
					column.setIsNullable(1);
				} else {
					column.setIsNullable(0);
				}
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

	private static String tableIndexSelect = "SELECT INDEX_NAME = A.name, "
			+ " INDEX_TYPE = CASE WHEN ((A.status&16) = 16 OR (A.status2&512) = 512) THEN 'Clustered' "
			+ "  WHEN (A.indid = 255) THEN 'Text/Image' "
			+ "  ELSE 'NonClustered' END, "
			+ " IS_UNIQUE = CASE WHEN ((A.status&2) = 2) THEN 1 ELSE 0 END, "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 1), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 2), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 3), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 4), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 5), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 6), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 7), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 8), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 9), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 10), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 11), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 12), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 13), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 14), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 15), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 16), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 17), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 18), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 19), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 20), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 21), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 22), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 23), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 24), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 25), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 26), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 27), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 28), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 29), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 30), "
			+ " INDEX_COL(USER_NAME(B.uid) + '.' + B.name, indid, 31) "
			+ "FROM sysindexes A,  sysobjects B "
			+ "WHERE A.indid > 0 AND A.indid < 255 AND A.status2 & 2 != 2 AND "
			+ " B.id = A.id AND B.type = 'U' AND "
			+ " USER_NAME(B.uid) = ? AND B.name=? " + "ORDER BY 1, 2, 3";

	protected void reverseEngineerTableIndices(Connection conn,
			Catalog catalog, Schema schema, Table table) {

		try {
			Grt.getInstance().addMsg("Fetching indices information.");
			Grt.getInstance().addMsgDetail(tableIndexSelect);

			PreparedStatement stmt = conn.prepareStatement(tableIndexSelect);
			stmt.setString(1, schema.getName());
			stmt.setString(2, table.getName());

			ResultSet rset = stmt.executeQuery();
			Index index = null;

			while (rset.next()) {
				String newIndexName = rset.getString("INDEX_NAME");

				index = new Index(table);
				index.setName(newIndexName);

				if (rset.getInt("IS_UNIQUE") != 0)
					index.setUnique(1);
				else
					index.setUnique(0);

				index.setDeferability(0);

				String indexType = rset.getString("INDEX_TYPE");
				if (indexType.equalsIgnoreCase("Clustered"))
					index.setClustered(1);
				else
					index.setClustered(0);

				int indexColumnPos = 4;
				String indexColumnName = rset.getString(indexColumnPos++);

				while (indexColumnPos < 31 + 4 && indexColumnName != null) {
					IndexColumn indexColumn = new IndexColumn(index);
					indexColumn.setName(indexColumnName);
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

					indexColumnName = rset.getString(indexColumnPos++);
				}

				table.getIndices().add(index);
			}

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String tableFKSelect = "SELECT CONSTRAINT_NAME = OBJECT_NAME(A.constrid), "
			+ " REF_CATALOG_NAME = ISNULL(A.pmrydbname,DB_NAME()), "
			+ " REF_SCHEMA_NAME = USER_NAME(B.uid), "
			+ " REF_TABLE_NAME = OBJECT_NAME(A.reftabid), "
			+ " COLUMN_COUNT = A.keycnt, "
			+ " FK_COLUMN1 = COL_NAME(A.tableid, A.fokey1, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey2, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey3, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey4, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey5, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey6, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey7, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey8, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey9, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey10, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey11, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey12, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey13, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey14, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey15, DB_ID(A.frgndbname)), "
			+ " COL_NAME(A.tableid, A.fokey16, DB_ID(A.frgndbname)), "
			+ " REFERENCED_COLUMN1 = COL_NAME(A.reftabid, A.refkey1, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey2, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey3, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey4, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey5, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey6, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey7, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey8, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey9, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey10, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey11, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey12, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey13, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey14, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey15, DB_ID(A.pmrydbname)), "
			+ " COL_NAME(A.reftabid, A.refkey16, DB_ID(A.pmrydbname)) "
			+ "FROM sysreferences A, sysobjects B "
			+ "WHERE USER_NAME(B.uid) = ? AND "
			+ " A.tableid = OBJECT_ID(?) AND "
			+ " A.reftabid = B.id AND (A.pmrydbname IS NULL) ";

	protected void reverseEngineerTableFKs(Connection conn, Catalog catalog,
			Schema schema, Table table) {

		try {
			Grt.getInstance().addMsg("Fetching FK information.");
			Grt.getInstance().addMsgDetail(tableFKSelect);

			PreparedStatement stmt = conn.prepareStatement(tableFKSelect);
			stmt.setString(1, schema.getName());
			stmt.setString(2, table.getName());

			ResultSet rset = stmt.executeQuery();

			ForeignKey foreignKey = null;

			while (rset.next()) {
				String newFkName = rset.getString("CONSTRAINT_NAME");

				foreignKey = new ForeignKey(table);
				foreignKey.setName(newFkName);

				foreignKey.setDeferability(0);

				foreignKey.setDeleteRule("NO ACTION");
				foreignKey.setUpdateRule("NO ACTION");

				foreignKey.setReferedTableSchemaName(rset
						.getString("REF_SCHEMA_NAME"));
				foreignKey
						.setReferedTableName(rset.getString("REF_TABLE_NAME"));

				int fkColumnPos = 6;
				int fkColumnCount = rset.getInt("COLUMN_COUNT");
				String refColumnName = rset.getString(fkColumnPos + 16);
				String fkColumnName = rset.getString(fkColumnPos);

				while (fkColumnPos < fkColumnCount + 6 && fkColumnName != null) {
					foreignKey.getReferedColumnNames().add(refColumnName);

					// find reference table column
					for (int j = 0; j < table.getColumns().size(); j++) {
						Column column = (Column) (table.getColumns().get(j));

						if (column.getName().compareToIgnoreCase(fkColumnName) == 0)
							foreignKey.getColumns().add(column);
					}

					refColumnName = rset.getString(fkColumnPos + 16);
					fkColumnName = rset.getString(fkColumnPos++);
				}

				table.getForeignKeys().add(foreignKey);
			}

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String viewSelect = "SELECT VIEW_NAME = B.name, VIEW_DEFINITION = A.text "
			+ "FROM syscomments A,sysobjects B "
			+ "WHERE USER_NAME(B.uid) = ? AND " + "B.type='V' AND A.id=B.id";

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

			view.setName(rset.getString("VIEW_NAME"));

			Grt.getInstance().addMsg("Processing view " + view.getName() + ".");

			view.setQueryExpression(rset.getString("VIEW_DEFINITION"));

			view.setWithCheckCondition(0);
		}

		stmt.close();

		Grt.getInstance().addMsg("Views fetched.");
	}

	private static String procedureCountSelect = "SELECT NUM = COUNT(*) "
			+ "FROM sysobjects O "
			+ "WHERE USER_NAME(O.uid) = ? AND O.type = 'P'";

	private static String procedureSelect = "SELECT ROUTINE_NAME = O.name, "
			+ " ROUTINE_DEFINITION = A.text "
			+ "FROM syscomments A, sysobjects O "
			+ "WHERE USER_NAME(O.uid) = ? AND A.id=O.id AND O.type = 'P'";

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

				proc.setRoutineType("PROCEDURE");

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
