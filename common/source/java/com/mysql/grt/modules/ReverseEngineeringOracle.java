package com.mysql.grt.modules;

import java.sql.*;

import com.mysql.grt.*;
import com.mysql.grt.db.oracle.*;

/**
 * GRT Reverse Engineering Class for Oracle 8i/9i
 * 
 * @author Mike
 * @version 1.0, 11/26/04
 * 
 */
public class ReverseEngineeringOracle extends ReverseEngineeringGeneric {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(ReverseEngineeringOracle.class,
				"ReverseEngineering");
	}

	private static String schemataSelect = "SELECT USERNAME FROM ALL_USERS ORDER BY USERNAME";

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

		// connect to the database
		Connection conn = establishConnection(dbConn);

		Grt.getInstance().addMsg("Fetching schemata list.");
		Grt.getInstance().addMsgDetail(schemataSelect);
		Grt.getInstance().flushMessages();

		GrtStringList schemataList = new GrtStringList();

		Statement stmt = conn.createStatement();
		ResultSet rset = stmt.executeQuery(schemataSelect);
		while (rset.next()) {
			schemataList.add(rset.getString(1));
		}
		stmt.close();
		conn.close();

		Grt.getInstance().addMsg("Return schemata list.");
		Grt.getInstance().flushMessages();

		return schemataList;
	}

	private static String statisticProcedureCall = "CALL DBMS_UTILITY.ANALYZE_SCHEMA"
			+ "(?,  'ESTIMATE', 50, 0, 'FOR TABLE')";

	/**
	 * Does the reverse engineering of the given schematas over the JDBC
	 * connection and returns the GRT objects
	 * 
	 * @param jdbcDriver
	 *            the class name of the JDBC driver
	 * @param jdbcConnectionString
	 *            a JDBC connection string
	 * @param schemataList
	 *            list of schemata to be reverse engineered
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
		ReverseEngineeringOracle revEng = new ReverseEngineeringOracle();

		Catalog catalog = new Catalog(null);
		catalog.setName("OracleCatalog");

		catalog.setVersion(getVersion(dbConn));

		Grt.getInstance().addMsg("Build simple Oracle datatypes.");
		Grt.getInstance().flushMessages();
		revEng.buildSimpleDatatypes(dbConn, catalog);

		for (int i = 0; i < schemataList.size(); i++) {
			Schema schema = new Schema(catalog);
			schema.setName((String) (schemataList.get(i)));
			catalog.getSchemata().add(schema);

			if (catalog.getVersion().getMajor() < 10) {
				Grt.getInstance().addMsg(
						"Call Oracle stored procedure ANALYZE_SCHEMA for "
								+ schema.getName() + ".");
				Grt.getInstance().addMsgDetail(statisticProcedureCall);
				Grt.getInstance().flushMessages();

				PreparedStatement stmt = conn
						.prepareStatement(statisticProcedureCall);
				stmt.setString(1, schema.getName());

				stmt.execute();
			}

			// Get Tables
			if (revEng.reverseEngineerTables(conn, catalog, schema) == 0
					&& !reverseEngineerOnlyTableObjects) {

				// Get Views
				revEng.reverseEngineerViews(conn, catalog, schema);

				// Get SPs
				if (revEng.reverseEngineerProcedures(conn, catalog, schema) == 0) {

					// Get Sequences
					revEng.reverseEngineerSequences(conn, catalog, schema);
				}
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
		com.mysql.grt.db.SimpleDatatypeList catalogDatatypeList = new com.mysql.grt.db.SimpleDatatypeList();

		for (int i = 0; i < rdbmsDatatypeList.size(); i++) {
			catalogDatatypeList.add(rdbmsDatatypeList.get(i));
		}

		catalog.setSimpleDatatypes(catalogDatatypeList);
	}

	private static String tableCountSelect = "SELECT COUNT(*) AS TABLECOUNT FROM ALL_TABLES t, ALL_OBJECTS a "
			+ "WHERE t.OWNER=? AND a.OWNER=t.OWNER AND a.OBJECT_NAME=t.TABLE_NAME AND "
			+ " a.OBJECT_TYPE='TABLE' AND a.STATUS='VALID' ";

	private static String tableSelect = "SELECT t.* FROM ALL_TABLES t, ALL_OBJECTS a "
			+ "WHERE t.OWNER=? AND a.OWNER=t.OWNER AND a.OBJECT_NAME=t.TABLE_NAME AND "
			+ " a.OBJECT_TYPE='TABLE' AND a.STATUS='VALID' "
			+ "ORDER BY t.OWNER, t.TABLE_NAME";

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
			tableCount = tblRset.getInt("TABLECOUNT");
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

			try {
				table.setTablespaceName(tblRset.getString("TABLESPACE_NAME"));
				table.setClusterName(tblRset.getString("CLUSTER_NAME"));
				table.setPctFreeBlockSpace(tblRset.getInt("PCT_FREE"));
				table.setPctUsedBlockSpace(tblRset.getInt("PCT_USED"));
				table.setInitialTrans(tblRset.getInt("INI_TRANS"));
				table.setMaxTrans(tblRset.getInt("MAX_TRANS"));
				table.setInitialExtent(tblRset.getInt("INITIAL_EXTENT"));
				table.setNextExtent(tblRset.getInt("NEXT_EXTENT"));
				table.setMinExtents(tblRset.getInt("MIN_EXTENTS"));
				table.setMaxExtents(tblRset.getInt("MAX_EXTENTS"));
				table.setPctIncrease(tblRset.getInt("PCT_INCREASE"));
			} catch (Throwable t) {
				// ignore exceptions because this is non-essential data
			}

			try {
				table.setNumRows(tblRset.getInt("NUM_ROWS"));
				table.setBlocks(tblRset.getInt("BLOCKS"));
				table.setEmptyBlocks(tblRset.getInt("EMPTY_BLOCKS"));
				table.setAvgSpace(tblRset.getInt("AVG_SPACE"));
				table.setAvgRowLen(tblRset.getInt("AVG_ROW_LEN"));
			} catch (Throwable t) {
				// ignore exceptions because this is non-essential data
			}

			if (tblRset.getString("NESTED").compareToIgnoreCase("yes") == 0) {
				table.setIsNestedTable(1);
			} else {
				table.setIsNestedTable(0);
			}
		}

		Grt.getInstance().addProgress("", -1);
		Grt.getInstance().flushMessages();

		Grt.getInstance().addMsg("Fetch column information.");
		Grt.getInstance().flushMessages();
		reverseEngineerTableColumns(conn, catalog, schema);

		Grt.getInstance().addMsg("Fetch PK information.");
		Grt.getInstance().flushMessages();
		reverseEngineerTablePK(conn, catalog, schema);

		Grt.getInstance().addMsg("Fetch index information.");
		Grt.getInstance().flushMessages();
		reverseEngineerTableIndices(conn, catalog, schema);

		Grt.getInstance().addMsg("Fetch FK information.");
		Grt.getInstance().flushMessages();
		reverseEngineerTableFKs(conn, catalog, schema);

		Grt.getInstance().addMsg("Fetch trigger information.");
		Grt.getInstance().flushMessages();
		reverseEngineerTableTriggers(conn, catalog, schema);

		Grt.getInstance().addProgress("", -1);
		Grt.getInstance().flushMessages();

		stmt.close();

		return 0;
	}

	private static String tableColumnsSelect8 = "SELECT tc.TABLE_NAME, tc.COLUMN_NAME, "
			+ " tc.DATA_TYPE, tc.DATA_TYPE_MOD, tc.DATA_LENGTH, tc.DATA_PRECISION, "
			+ " tc.DATA_SCALE, tc.NULLABLE, tc.DEFAULT_LENGTH, tc.DENSITY, "
			+ " tc.NUM_NULLS, tc.NUM_BUCKETS, tc.CHARACTER_SET_NAME, tc.DATA_DEFAULT "
			+ "FROM ALL_TAB_COLUMNS tc, ALL_TABLES t "
			+ "WHERE tc.OWNER=? AND t.OWNER=tc.OWNER AND tc.TABLE_NAME=t.TABLE_NAME "
			+ "ORDER BY tc.TABLE_NAME, tc.COLUMN_ID";

	private static String tableColumnsSelect9 = "SELECT tc.TABLE_NAME, tc.COLUMN_NAME, "
			+ " tc.DATA_TYPE, tc.DATA_TYPE_MOD, tc.CHAR_LENGTH, tc.DATA_LENGTH, "
			+ " tc.DATA_PRECISION, tc.DATA_SCALE, tc.NULLABLE, tc.DEFAULT_LENGTH, "
			+ " tc.DENSITY, tc.NUM_NULLS, tc.NUM_BUCKETS, tc.CHARACTER_SET_NAME, "
			+ " tc.DATA_DEFAULT "
			+ "FROM ALL_TAB_COLUMNS tc, ALL_TABLES t "
			+ "WHERE tc.OWNER=? AND t.OWNER=tc.OWNER AND tc.TABLE_NAME=t.TABLE_NAME "
			+ "ORDER BY tc.TABLE_NAME, tc.COLUMN_ID";

	protected void reverseEngineerTableColumns(Connection conn,
			Catalog catalog, Schema schema) {
		try {
			if (schema.getTables().size() < 1)
				return;

			// get first table
			int tableIndex = 0;
			Table table = (Table) schema.getTables().get(tableIndex);

			String sql;
			if (catalog.getVersion().getMajor() <= 8)
				sql = tableColumnsSelect8;
			else
				sql = tableColumnsSelect9;

			Grt.getInstance().addMsg("Fetching column information.");
			Grt.getInstance().addMsgDetail(sql);

			PreparedStatement stmt = conn.prepareStatement(sql);
			stmt.setString(1, schema.getName());

			ResultSet colRset = stmt.executeQuery();
			while (colRset.next()) {
				String tableName = colRset.getString("TABLE_NAME");

				// if the table name has changed, go to next matching table
				while (!tableName.equals(table.getName())) {
					table = (Table) schema.getTables().get(++tableIndex);
				}

				// create new column
				Column column = new Column(table);
				table.getColumns().add(column);

				column.setName(colRset.getString("COLUMN_NAME"));
				column.setDatatypeName(colRset.getString("DATA_TYPE"));
				column.setDatatypeModifier(colRset.getString("DATA_TYPE_MOD"));

				// Get Simple Type
				int datatypeIndex = catalog.getSimpleDatatypes()
						.getIndexOfName(column.getDatatypeName());

				if (datatypeIndex > -1) {
					column.setSimpleType(catalog.getSimpleDatatypes().get(
							datatypeIndex));
				} else {
					column.setSimpleType(catalog.getSimpleDatatypes().get(
							catalog.getSimpleDatatypes().getIndexOfName(
									"VARCHAR2")));
					column.setLength(255);

					Grt.getInstance().addMsg(
							"WARNING: The datatype " + column.getDatatypeName()
									+ " was not been defined yet.");
				}

				if (column.getDatatypeName().equalsIgnoreCase("VARCHAR2")
						|| column.getDatatypeName().equalsIgnoreCase(
								"NVARCHAR2")
						|| column.getDatatypeName().equalsIgnoreCase("CHAR")
						|| column.getDatatypeName().equalsIgnoreCase("NCHAR")
						|| column.getDatatypeName().equalsIgnoreCase("LONG")) {
					try {
						// in Oracle 9i there is CHAR_LENGTH
						if (catalog.getVersion().getMajor() >= 9)
							column.setLength(colRset.getInt("CHAR_LENGTH"));
						else
							column.setLength(colRset.getInt("DATA_LENGTH"));
					} catch (Exception e) {
						column.setLength(colRset.getInt("DATA_LENGTH"));
					}
				} else
					column.setLength(colRset.getInt("DATA_LENGTH"));

				column.setPrecision(colRset.getInt("DATA_PRECISION"));
				if (colRset.wasNull())
					column.setPrecision(22);
				column.setScale(colRset.getInt("DATA_SCALE"));

				// Nullable
				if (colRset.getString("NULLABLE").compareToIgnoreCase("Y") == 0) {
					column.setIsNullable(1);
				} else {
					column.setIsNullable(0);
				}

				// Default Value Length
				column.setDefaultLength(colRset.getInt("DEFAULT_LENGTH"));

				// Density
				column.setDensity(colRset.getDouble("DENSITY"));

				// Number of Nulls in column
				column.setNumberOfNulls(colRset.getInt("NUM_NULLS"));

				// Number of Buckets in histogram for the column
				column.setNumberOfBuckets(colRset.getInt("NUM_BUCKETS"));

				// Character set: CHAR_CS or NCHAR_CS
				column.setCharacterSetName(colRset
						.getString("CHARACTER_SET_NAME"));

				// Default Value
				String defaultValue = colRset.getString("DATA_DEFAULT");

				if (defaultValue != null && !defaultValue.equals("")) {
					// remove () from numeric types
					if (column.getSimpleType().getGroup().getName().equals(
							"numeric")
							&& defaultValue.startsWith("(")
							&& defaultValue.endsWith(")")) {
						defaultValue = defaultValue.substring(1, defaultValue
								.length() - 1);
					}

					// ignore Oracle specific functions
					if ((!defaultValue.equalsIgnoreCase("USER"))
							&& (!defaultValue.equalsIgnoreCase("SYSDATE"))
							&& (!defaultValue.equalsIgnoreCase("sys_guid()")))
						column.setDefaultValue(defaultValue.trim());
				}
			}

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String tablePKSelect8 = "SELECT c.TABLE_NAME, i.COLUMN_NAME "
			+ "FROM ALL_CONSTRAINTS c, ALL_TABLES t, ALL_IND_COLUMNS i "
			+ "WHERE c.OWNER=? AND t.OWNER=c.OWNER AND c.TABLE_NAME=t.TABLE_NAME AND "
			+ " c.CONSTRAINT_TYPE='P' AND c.CONSTRAINT_NAME=i.INDEX_NAME AND "
			+ " i.TABLE_OWNER=c.OWNER AND i.TABLE_NAME=c.TABLE_NAME "
			+ "ORDER BY c.TABLE_NAME, i.COLUMN_POSITION";

	private static String tablePKSelect9 = "SELECT c.TABLE_NAME, i.COLUMN_NAME "
			+ "FROM ALL_CONSTRAINTS c, ALL_TABLES t, ALL_IND_COLUMNS i "
			+ "WHERE c.OWNER=? AND t.OWNER=c.OWNER AND c.TABLE_NAME=t.TABLE_NAME AND "
			+ " c.CONSTRAINT_TYPE='P' AND c.INDEX_NAME=i.INDEX_NAME AND "
			+ " i.TABLE_OWNER=c.OWNER AND i.TABLE_NAME=c.TABLE_NAME "
			+ "ORDER BY c.TABLE_NAME, i.COLUMN_POSITION";

	protected void reverseEngineerTablePK(Connection conn, Catalog catalog,
			Schema schema) {
		try {
			if (schema.getTables().size() < 1)
				return;

			// get first table
			int tableIndex = 0;
			Table table = (Table) schema.getTables().get(tableIndex);

			String sql;
			if (catalog.getVersion().getMajor() <= 8)
				sql = tablePKSelect8;
			else
				sql = tablePKSelect9;

			Grt.getInstance().addMsg("Fetching primary key information.");
			Grt.getInstance().addMsgDetail(sql);

			PreparedStatement stmt = conn.prepareStatement(sql);
			stmt.setString(1, schema.getName());

			ResultSet colRset = stmt.executeQuery();

			Index primaryKey = null;

			while (colRset.next()) {
				String tableName = colRset.getString("TABLE_NAME");

				// if the table name has changed, go to next matching table
				while (!tableName.equals(table.getName())) {
					table = (Table) schema.getTables().get(++tableIndex);
					primaryKey = null;
				}

				if (primaryKey == null) {
					primaryKey = new Index(table);
					primaryKey.setName("PRIMARY");

					primaryKey.setIsPrimary(1);

					table.getIndices().add(primaryKey);

					table.setPrimaryKey(primaryKey);
				}

				String indexColumnName = colRset.getString("COLUMN_NAME");
				int index = table.getColumns().getIndexOfName(indexColumnName);

				if (index > -1) {
					// create new index column
					IndexColumn indexColumn = new IndexColumn(primaryKey);
					indexColumn.setName(indexColumnName);

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
			}

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String tableIndexSelect = "SELECT i.*, "
			+ " ic.COLUMN_NAME, ic.COLUMN_LENGTH, ic.DESCEND "
			+ "FROM ALL_INDEXES i, ALL_IND_COLUMNS ic, ALL_CONSTRAINTS c, ALL_TABLES t "
			+ "WHERE i.TABLE_OWNER=? AND t.OWNER=i.OWNER AND i.TABLE_NAME=t.TABLE_NAME AND "
			+ " ic.TABLE_OWNER=i.TABLE_OWNER AND "
			+ " ic.TABLE_NAME=i.TABLE_NAME AND "
			+ " ic.INDEX_NAME=i.INDEX_NAME AND c.OWNER(+)=i.OWNER AND "
			+ " c.CONSTRAINT_NAME(+)=i.INDEX_NAME AND "
			+ " (c.CONSTRAINT_TYPE is null OR c.CONSTRAINT_TYPE<>'P') "
			+ "ORDER BY i.TABLE_NAME, ic.INDEX_NAME, ic.COLUMN_POSITION";

	private static String tableIndexColNameLookup = "SELECT DEFAULT$ AS COLUMN_NAME "
			+ "FROM SYS.COL$ "
			+ "WHERE NAME=? AND OBJ# in "
			+ "  (SELECT OBJECT_ID FROM ALL_OBJECTS "
			+ "  WHERE OWNER=? AND OBJECT_NAME=?)";

	protected void reverseEngineerTableIndices(Connection conn,
			Catalog catalog, Schema schema) {

		try {
			if (schema.getTables().size() < 1)
				return;

			// get first table
			int tableIndex = 0;
			Table table = (Table) schema.getTables().get(tableIndex);

			Grt.getInstance().addMsg("Fetching indices information.");
			Grt.getInstance().addMsgDetail(tableIndexSelect);

			PreparedStatement stmt = conn.prepareStatement(tableIndexSelect);
			stmt.setString(1, schema.getName());

			ResultSet rset = stmt.executeQuery();

			String indexName = "";
			// String indexOwner = "";
			Index index = null;

			while (rset.next()) {
				String tableName = rset.getString("TABLE_NAME");

				// if the table name has changed, go to next matching table
				while (!tableName.equals(table.getName())) {
					if (index != null)
						table.getIndices().add(index);

					table = (Table) schema.getTables().get(++tableIndex);
					indexName = "";
					index = null;
				}

				// String newIndexOwner = rset.getString("OWNER");
				String newIndexName = rset.getString("INDEX_NAME");

				/* && (indexOwner.compareToIgnoreCase(newIndexOwner) != 0) */
				if (indexName.compareToIgnoreCase(newIndexName) != 0) {
					if (index != null)
						table.getIndices().add(index);

					index = new Index(table);
					index.setName(newIndexName);
					indexName = newIndexName;

					index.setIndexType(rset.getString("INDEX_TYPE"));
					if (rset.getString("UNIQUENESS").compareToIgnoreCase(
							"UNIQUE") == 0)
						index.setUnique(1);
					else
						index.setUnique(0);

					index.setDeferability(0);

					if (rset.getString("COMPRESSION").compareToIgnoreCase(
							"ENABLED") == 0)
						index.setCompression(1);
					else
						index.setCompression(0);

					index.setPrefixLength(rset.getInt("PREFIX_LENGTH"));

					index.setTablespace(rset.getString("TABLESPACE_NAME"));

					index.setInitialTrans(rset.getInt("INI_TRANS"));
					index.setMaxTrans(rset.getInt("MAX_TRANS"));
					index.setInitialExtent(rset.getInt("INITIAL_EXTENT"));
					index.setNextExtent(rset.getInt("NEXT_EXTENT"));
					index.setMinExtents(rset.getInt("MIN_EXTENTS"));
					index.setMaxExtents(rset.getInt("MAX_EXTENTS"));
					index.setPctIncrease(rset.getInt("PCT_INCREASE"));
					index.setPctTreshold(rset.getInt("PCT_THRESHOLD"));

					index.setBlevel(rset.getInt("BLEVEL"));
					index.setLeafBlocks(rset.getInt("LEAF_BLOCKS"));
					index.setDistinctKeys(rset.getInt("DISTINCT_KEYS"));
					index.setAvgLeafBlocksPerKey(rset
							.getInt("AVG_LEAF_BLOCKS_PER_KEY"));
					index.setAvgDataBlocksPerKey(rset
							.getInt("AVG_DATA_BLOCKS_PER_KEY"));
					index.setClusteringFactor(rset.getInt("CLUSTERING_FACTOR"));

					index.setNumRows(rset.getInt("NUM_ROWS"));

					if (rset.getString("GENERATED").compareToIgnoreCase("Y") == 0)
						index.setGenerated(1);
					else
						index.setGenerated(0);
				}

				String indexColumnName = rset.getString("COLUMN_NAME");
				boolean indexColumnDefined = false;

				try {
					// resolve column names for functional indices
					if (indexColumnName.startsWith("SYS_")) {

						Grt.getInstance().addMsg("Lookup index column names.");
						Grt.getInstance().addMsgDetail(tableIndexColNameLookup);

						PreparedStatement colNameStmt = conn
								.prepareStatement(tableIndexColNameLookup);

						colNameStmt.setString(1, indexColumnName);
						colNameStmt.setString(2, schema.getName());
						colNameStmt.setString(3, table.getName());

						try {
							ResultSet colNameRset = colNameStmt.executeQuery();

							if (colNameRset.next()) {
								indexColumnName = colNameRset.getString(
										"COLUMN_NAME").substring(1);

								indexColumnName = indexColumnName.substring(0,
										indexColumnName.length() - 1);
							}
						} finally {
							colNameStmt.close();
						}
					}
				} catch (Exception e) {
					Grt.getInstance().addErr(e.getMessage());

					return;
				}

				// check if an index column with this name has already been
				// added t(functional indices)
				for (int i = 0; i < index.getColumns().size(); i++) {
					if (index.getColumns().get(i).getName().equalsIgnoreCase(
							indexColumnName)) {
						indexColumnDefined = true;
						break;
					}
				}
				if (indexColumnDefined)
					continue;

				// create new index column
				IndexColumn indexColumn = new IndexColumn(index);
				indexColumn.setName(indexColumnName);
				indexColumn.setColumnLength(rset.getInt("COLUMN_LENGTH"));

				if (rset.getString("DESCEND").compareToIgnoreCase("Y") == 0)
					indexColumn.setDescend(1);
				else
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

	private static String tableFKSelect = "SELECT c.TABLE_NAME, c.CONSTRAINT_NAME, "
			+ "c.\"DEFERRABLE\", c.DELETE_RULE, cc.COLUMN_NAME,	"
			+ "r.OWNER AS R_SCHEMA, r.TABLE_NAME AS R_TABLE, "
			+ "rc.COLUMN_NAME AS R_COLUMN "
			+ "FROM ALL_CONSTRAINTS c, ALL_CONS_COLUMNS cc, ALL_CONSTRAINTS r, "
			+ "ALL_CONS_COLUMNS rc WHERE c.OWNER=? AND "
			+ "c.CONSTRAINT_TYPE = 'R' AND c.R_OWNER=r.OWNER AND "
			+ "c.R_CONSTRAINT_NAME=r.CONSTRAINT_NAME AND "
			+ "c.CONSTRAINT_NAME = cc.CONSTRAINT_NAME AND c.OWNER = cc.OWNER AND "
			+ "r.CONSTRAINT_NAME = rc.CONSTRAINT_NAME AND r.OWNER = rc.OWNER AND "
			+ "cc.POSITION = rc.POSITION ORDER BY c.TABLE_NAME, c.CONSTRAINT_NAME, cc.POSITION";

	protected void reverseEngineerTableFKs(Connection conn, Catalog catalog,
			Schema schema) {

		try {
			if (schema.getTables().size() < 1)
				return;

			// get first table
			int tableIndex = 0;
			Table table = (Table) schema.getTables().get(tableIndex);

			Grt.getInstance().addMsg("Fetching FK information.");
			Grt.getInstance().addMsgDetail(tableFKSelect);

			PreparedStatement stmt = conn.prepareStatement(tableFKSelect);
			stmt.setString(1, schema.getName());

			ResultSet rset = stmt.executeQuery();

			String fkName = "";
			ForeignKey foreignKey = null;

			while (rset.next()) {
				String tableName = rset.getString("TABLE_NAME");

				// if the table name has changed, go to next matching table
				while (!tableName.equals(table.getName())) {
					if (foreignKey != null)
						table.getForeignKeys().add(foreignKey);

					table = (Table) schema.getTables().get(++tableIndex);
					fkName = "";
					foreignKey = null;
				}

				String newFkName = rset.getString("CONSTRAINT_NAME");

				if (fkName.compareToIgnoreCase(newFkName) != 0) {
					if (foreignKey != null)
						table.getForeignKeys().add(foreignKey);

					fkName = newFkName;

					foreignKey = new ForeignKey(table);
					foreignKey.setName(newFkName);

					if (rset.getString("DEFERRABLE").equals("NOT DEFERRABLE"))
						foreignKey.setDeferability(0);
					else
						foreignKey.setDeferability(1);

					foreignKey.setDeleteRule(rset.getString("DELETE_RULE"));
					foreignKey.setUpdateRule("NO ACTION");

					foreignKey.setReferedTableSchemaName(rset
							.getString("R_SCHEMA"));
					foreignKey.setReferedTableName(rset.getString("R_TABLE"));
				}

				foreignKey.getReferedColumnNames().add(
						rset.getString("R_COLUMN"));

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

	private static String tableTriggerSelect = "SELECT t.TABLE_NAME, t.TRIGGER_NAME, "
			+ "t.TRIGGER_TYPE, t.TRIGGERING_EVENT, t.BASE_OBJECT_TYPE, "
			+ "t.COLUMN_NAME, t.REFERENCING_NAMES, t.WHEN_CLAUSE, "
			+ "t.STATUS, t.DESCRIPTION, t.ACTION_TYPE, t.TRIGGER_BODY "
			+ "FROM ALL_TRIGGERS t, ALL_TABLES ta "
			+ "WHERE t.TABLE_OWNER=? AND ta.OWNER=t.OWNER AND t.TABLE_NAME=ta.TABLE_NAME "
			+ "ORDER BY t.TABLE_NAME";

	protected void reverseEngineerTableTriggers(Connection conn,
			Catalog catalog, Schema schema) {

		try {
			if (schema.getTables().size() < 1)
				return;

			// get first table
			int tableIndex = 0;
			Table table = (Table) schema.getTables().get(tableIndex);

			Grt.getInstance().addMsg("Fetching FK information.");
			Grt.getInstance().addMsgDetail(tableTriggerSelect);

			PreparedStatement stmt = conn.prepareStatement(tableTriggerSelect);
			stmt.setString(1, schema.getName());

			ResultSet rset = stmt.executeQuery();

			while (rset.next()) {
				String tableName = rset.getString("TABLE_NAME");

				// if the table name has changed, go to next matching table
				while (!tableName.equals(table.getName())) {
					table = (Table) schema.getTables().get(++tableIndex);
				}

				Trigger trigger = new Trigger(table);

				trigger.setName(rset.getString("TRIGGER_NAME"));
				trigger.setOldName(trigger.getName());

				trigger.setComment(rset.getString("DESCRIPTION"));

				trigger.setEvent(rset.getString("TRIGGERING_EVENT"));

				trigger.setCondition(rset.getString("WHEN_CLAUSE"));

				trigger.setStatement(rset.getString("TRIGGER_BODY"));

				trigger.setOrder(0);

				String s = rset.getString("TRIGGER_TYPE");

				if (s.toUpperCase().startsWith("BEFORE"))
					trigger.setTiming("BEFORE");
				else
					trigger.setTiming("AFTER");

				if (s.toUpperCase().endsWith("STATEMENT"))
					trigger.setOrientation("STATEMENT");
				else if (s.toUpperCase().endsWith("ROW"))
					trigger.setOrientation("ROW");
				else if (s.toUpperCase().endsWith("EVENT"))
					trigger.setOrientation("EVENT");

				trigger.setReferenceNewRow(rset.getString("REFERENCING_NAMES"));

				if (rset.getString("STATUS").equalsIgnoreCase("ENABLED"))
					trigger.setEnabled(1);
				else
					trigger.setEnabled(0);

				table.getTriggers().add(trigger);
			}

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String viewSelect = "SELECT v.*, c.STATUS as CHECK_ENABLED "
			+ "FROM ALL_VIEWS v, ALL_CONSTRAINTS c, ALL_OBJECTS a WHERE v.OWNER=? AND "
			+ "  c.TABLE_NAME(+)=v.VIEW_NAME AND c.CONSTRAINT_TYPE(+)='V' AND "
			+ "  a.OWNER=v.OWNER AND a.OBJECT_NAME=v.VIEW_NAME AND "
			+ "  a.OBJECT_TYPE='VIEW' AND a.STATUS='VALID' "
			+ "ORDER BY v.OWNER, v.VIEW_NAME";

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

			view.setQueryExpression(rset.getString("TEXT"));

			try {
				view.setTypedText(rset.getString("TYPE_TEXT"));
				view.setOidText(rset.getString("OID_TEXT"));
				view.setViewTypeOwner(rset.getString("VIEW_TYPE_OWNER"));
				view.setViewType(rset.getString("VIEW_TYPE"));
				view.setSuperViewName(rset.getString("SUPERVIEW_NAME"));
			} catch (Exception e) {
				// ignore missing fields in Oracle 8
			}

			if (rset.getString("CHECK_ENABLED") != null)
				view.setWithCheckCondition(1);
			else
				view.setWithCheckCondition(0);
		}

		stmt.close();

		reverseEngineerViewColumns(conn, catalog, schema);

		Grt.getInstance().addMsg("Views fetched.");
	}

	private static String viewColumnsSelect = "SELECT tc.TABLE_NAME, tc.COLUMN_NAME "
			+ "FROM ALL_TAB_COLUMNS tc, ALL_VIEWS v "
			+ "WHERE tc.OWNER=? AND v.OWNER=tc.OWNER AND tc.TABLE_NAME=v.VIEW_NAME "
			+ "ORDER BY tc.TABLE_NAME, tc.COLUMN_ID";

	protected void reverseEngineerViewColumns(Connection conn, Catalog catalog,
			Schema schema) {
		try {
			if (schema.getTables().size() < 1)
				return;

			// get first view
			int viewIndex = 0;
			View view = (View) schema.getViews().get(viewIndex);

			Grt.getInstance().addMsg("Fetching column information.");
			Grt.getInstance().addMsgDetail(viewColumnsSelect);

			PreparedStatement stmt = conn.prepareStatement(viewColumnsSelect);
			stmt.setString(1, schema.getName());

			ResultSet colRset = stmt.executeQuery();
			while (colRset.next()) {
				String tableName = colRset.getString("TABLE_NAME");

				// if the table name has changed, go to next matching table
				while (!tableName.equals(view.getName())) {
					view = (View) schema.getViews().get(++viewIndex);
				}

				// create new column
				view.getColumns().add(colRset.getString("COLUMN_NAME"));
			}

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String procedureCountSelect = "SELECT COUNT(*) AS NUM "
			+ "FROM ALL_PROCEDURES p, ALL_OBJECTS a WHERE p.OWNER=? AND "
			+ "  a.OWNER=p.OWNER AND a.OBJECT_NAME=p.OBJECT_NAME AND "
			+ "  (a.OBJECT_TYPE='PROCEDURE' OR a.OBJECT_TYPE='FUNCTION') AND a.STATUS='VALID' "
			+ "ORDER BY p.OBJECT_NAME";

	private static String procedureSelect = "SELECT p.*, "
			+ "(SELECT max(s.TYPE) FROM ALL_SOURCE s "
			+ " WHERE s.OWNER=? AND s.NAME=p.OBJECT_NAME) as TYPE "
			+ "FROM ALL_PROCEDURES p, ALL_OBJECTS a WHERE p.OWNER=? AND "
			+ "  a.OWNER=p.OWNER AND a.OBJECT_NAME=p.OBJECT_NAME AND "
			+ "  (a.OBJECT_TYPE='PROCEDURE' OR a.OBJECT_TYPE='FUNCTION') AND a.STATUS='VALID' "
			+ "ORDER BY p.OBJECT_NAME";

	private static String procedureCodeSelect = "SELECT TEXT "
			+ "FROM ALL_SOURCE WHERE OWNER=? AND NAME=? " + "ORDER BY LINE";

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
			Grt.getInstance().addMsgDetail(procedureCodeSelect);
			Grt.getInstance().flushMessages();

			stmt = conn.prepareStatement(procedureSelect);
			stmt.setString(1, schema.getName());
			stmt.setString(2, schema.getName());

			rset = stmt.executeQuery();

			while (rset.next()) {
				// Create new view
				Routine proc = new Routine(schema);
				schema.getRoutines().add(proc);

				proc.setName(rset.getString("OBJECT_NAME"));

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

				proc.setRoutineType(rset.getString("TYPE"));

				proc.setImplTypeOwner(rset.getString("IMPLTYPEOWNER"));
				proc.setImplTypeName(rset.getString("IMPLTYPENAME"));

				if (rset.getString("AGGREGATE").equalsIgnoreCase("YES"))
					proc.setAggregate(1);
				else
					proc.setAggregate(0);

				if (rset.getString("PIPELINED").equalsIgnoreCase("YES"))
					proc.setPipelined(1);
				else
					proc.setPipelined(0);

				if (rset.getString("PARALLEL").equalsIgnoreCase("YES"))
					proc.setParallel(1);
				else
					proc.setParallel(0);

				PreparedStatement stmtCode = conn
						.prepareStatement(procedureCodeSelect);
				stmtCode.setString(1, schema.getName());
				stmtCode.setString(2, proc.getName());

				ResultSet rsetCode = stmtCode.executeQuery();

				StringBuffer code = new StringBuffer(1024);
				while (rsetCode.next()) {
					code.append(rsetCode.getString("TEXT"));
				}

				stmtCode.close();

				proc.setRoutineCode(code.toString());
			}

			stmt.close();

			Grt.getInstance().addMsg("Stored procedures fetched.");

			Grt.getInstance().addProgress("", -1);

		} catch (Exception e) {
			Grt.getInstance().addMsg("Stored procedures cannot be fetched.");
			Grt.getInstance().addMsgDetail(e.getMessage());
		}

		return 0;
	}

	private static String sequencesCountSelect = "SELECT COUNT(*) AS NUM "
			+ "FROM ALL_SEQUENCES s, ALL_OBJECTS a WHERE s.SEQUENCE_OWNER=? AND "
			+ "  a.OWNER=s.SEQUENCE_OWNER AND a.OBJECT_NAME=s.SEQUENCE_NAME AND "
			+ "  a.OBJECT_TYPE='SEQUENCE' AND a.STATUS='VALID'";

	private static String sequencesSelect = "SELECT s.SEQUENCE_NAME, s.MIN_VALUE, "
			+ " s.MAX_VALUE, s.INCREMENT_BY, s.CYCLE_FLAG, s.ORDER_FLAG, "
			+ " s.CACHE_SIZE, s.LAST_NUMBER "
			+ "FROM ALL_SEQUENCES s, ALL_OBJECTS a WHERE s.SEQUENCE_OWNER=? AND "
			+ "  a.OWNER=s.SEQUENCE_OWNER AND a.OBJECT_NAME=s.SEQUENCE_NAME AND "
			+ "  a.OBJECT_TYPE='SEQUENCE' AND a.STATUS='VALID' "
			+ "ORDER BY s.SEQUENCE_NAME";

	protected void reverseEngineerSequences(Connection conn, Catalog catalog,
			Schema schema) throws Exception {
		// String sql;
		int sequencesCount = 0;
		int currentSequenceNumber = 0;

		Grt.getInstance().addMsg(
				"Fetch the number sequences of the schema " + schema.getName()
						+ ".");
		Grt.getInstance().addMsgDetail(sequencesCountSelect);

		try {
			PreparedStatement stmt = conn
					.prepareStatement(sequencesCountSelect);
			stmt.setString(1, schema.getName());

			ResultSet rset = stmt.executeQuery();

			if (rset.next()) {
				sequencesCount = rset.getInt("NUM");
			}

			Grt.getInstance().addMsg(
					"Fetch " + sequencesCount + " sequence(s) of the schema "
							+ schema.getName() + ".");
			Grt.getInstance().addMsgDetail(sequencesSelect);
			Grt.getInstance().flushMessages();

			stmt = conn.prepareStatement(sequencesSelect);
			stmt.setString(1, schema.getName());

			rset = stmt.executeQuery();

			while (rset.next()) {

				Sequence seq = new Sequence(schema);

				seq.setName(rset.getString("SEQUENCE_NAME"));

				currentSequenceNumber++;

				if (currentSequenceNumber % 5 == 0) {
					Grt.getInstance().addProgress(
							"Processing sequence " + seq.getName() + ".",
							(currentSequenceNumber * 100) / sequencesCount);
					Grt.getInstance().flushMessages();
				}

				seq.setMinValue(rset.getString("MIN_VALUE"));
				seq.setMaxValue(rset.getString("MAX_VALUE"));
				seq.setIncrementBy(rset.getString("INCREMENT_BY"));

				if (rset.getString("CYCLE_FLAG").compareToIgnoreCase("Y") == 0)
					seq.setCycleFlag(1);
				else
					seq.setCycleFlag(0);

				if (rset.getString("ORDER_FLAG").compareToIgnoreCase("Y") == 0)
					seq.setOrderFlag(1);
				else
					seq.setOrderFlag(0);

				seq.setCacheSize(rset.getString("CACHE_SIZE"));
				seq.setLastNumber(rset.getString("LAST_NUMBER"));

				schema.getSequences().add(seq);
			}

			stmt.close();

			Grt.getInstance().addMsg("Sequences fetched.");

			Grt.getInstance().addProgress("", -1);

		} catch (Exception e) {
			Grt.getInstance().addMsg("Sequences could not be fetched.");
			Grt.getInstance().addMsg(e.getMessage());
		}
	}
}