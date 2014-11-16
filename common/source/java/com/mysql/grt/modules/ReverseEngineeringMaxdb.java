package com.mysql.grt.modules;

import java.sql.*;

import com.mysql.grt.*;
import com.mysql.grt.db.maxdb.*;

/**
 * GRT Reverse Engineering Class for MaxDB
 * 
 * @author Mike
 * @version 1.0, 11/26/04
 * 
 */
public class ReverseEngineeringMaxdb extends ReverseEngineeringGeneric {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(ReverseEngineeringMaxdb.class,
				"ReverseEngineering");
	}

	/* MaxDB <=7.5 - no SQL schemata, schemata (namespaces) based on users */
	private static String schemataSelect75 = "SELECT USERNAME FROM USERS ORDER BY USERNAME ASC";

	/*
	 * MaxDB >=7.6 - real SQL schema and CREATE SCHEMA, in coexistance with the
	 * user logic
	 */
	private static String schemataSelect76 = "SELECT SCHEMANAME FROM SCHEMAS ORDER BY SCHEMANAME ASC";

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

		String schemataSelect = "";

		// connect to the database
		Connection conn = establishConnection(dbConn);

		// As of 7.6 MaxDB has support for real SQL schemata (CREATE SCHEMA...)
		DatabaseMetaData metaData = conn.getMetaData();

		if ((metaData.getDatabaseMajorVersion() >= 7)
				&& (metaData.getDatabaseMinorVersion() >= 6)) {
			schemataSelect = schemataSelect76;
		} else {
			schemataSelect = schemataSelect75;
		}

		Grt.getInstance().addMsg("Fetching schemata list...");
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
		ReverseEngineeringMaxdb revEng = new ReverseEngineeringMaxdb();

		Catalog catalog = new Catalog(null);
		catalog.setName("MaxdbCatalog");
		catalog.setVersion(getVersion(dbConn));

		Grt.getInstance().addMsg("Build simple MaxDB datatypes.");
		Grt.getInstance().flushMessages();
		revEng.buildSimpleDatatypes(dbConn, catalog);

		for (int i = 0; i < schemataList.size(); i++) {
			Schema schema = new Schema(catalog);
			schema.setName((String) (schemataList.get(i)));
			catalog.getSchemata().add(schema);

			// Get Tables
			if (revEng.reverseEngineerTables(conn, catalog, schema) == 0
					&& !reverseEngineerOnlyTableObjects) {

				// Get Views
				revEng.reverseEngineerViews(conn, catalog, schema);

				// Get SPs
				if (revEng.reverseEngineerProcedures(conn, catalog, schema) == 0) {

					// Get Sequences
					if (revEng.reverseEngineerSequences(conn, catalog, schema) == 0) {

						// Get table Synonyms
						revEng.reverseEngineerSynonyms(conn, catalog, schema);

					}
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

	private static String tableCountSelect75 = "SELECT COUNT(*) AS TABLECOUNT FROM TABLES "
			+ "WHERE OWNER=? AND TYPE='TABLE'";

	private static String tableCountSelect76 = "SELECT COUNT(*) AS TABLECOUNT FROM TABLES "
			+ "WHERE SCHEMANAME=? AND TYPE='TABLE'";

	private static String tableSelect75 = "SELECT * FROM TABLES "
			+ "WHERE OWNER=? AND TYPE='TABLE' ORDER BY TABLENAME ASC";

	private static String tableSelect76 = "SELECT * FROM TABLES "
			+ "WHERE SCHEMANAME=? AND TYPE = 'TABLE' ORDER BY TABLENAME ASC";

	protected int reverseEngineerTables(Connection conn, Catalog catalog,
			Schema schema) throws Exception {

		int tableCount = 0;
		int currentTableNumber = 0;
		String sqlSelect = "";

		sqlSelect = (this.useRealSchema(conn)) ? tableCountSelect76
				: tableCountSelect75;

		Grt.getInstance().addMsg(
				"Fetch the number of tables in the schema " + schema.getName()
						+ ".");
		Grt.getInstance().addMsgDetail(sqlSelect);

		PreparedStatement stmt = conn.prepareStatement(sqlSelect);
		stmt.setString(1, schema.getName());

		ResultSet tblRset = stmt.executeQuery();
		if (tblRset.next()) {
			tableCount = tblRset.getInt("TABLECOUNT");
		}
		stmt.close();

		sqlSelect = (this.useRealSchema(conn)) ? tableSelect76 : tableSelect75;

		Grt.getInstance().addMsg(
				"Fetching " + tableCount + " table(s) of the schema "
						+ schema.getName() + ".");
		Grt.getInstance().addMsgDetail(sqlSelect);
		Grt.getInstance().flushMessages();

		stmt = conn.prepareStatement(sqlSelect);
		stmt.setString(1, schema.getName());

		tblRset = stmt.executeQuery();

		while (tblRset.next()) {
			// Create new table
			Table table = new Table(schema);
			schema.getTables().add(table);

			currentTableNumber++;

			table.setName(tblRset.getString("TABLENAME"));

			Grt.getInstance().addProgress(
					"Processing table " + table.getName() + ".",
					(currentTableNumber * 100) / tableCount);
			if (Grt.getInstance().flushMessages() != 0) {
				Grt.getInstance().addMsg("Migration canceled by user.");
				return 1;
			}

			table.setNoFixedLengthColumn((tblRset.getString("DYNAMIC")
					.compareToIgnoreCase("NO") == 0) ? 1 : 0);
			table.setSample((tblRset.getString("SAMPLE_ROWS") == "") ? tblRset
					.getString("SAMPLE_PERCENT") : tblRset
					.getString("SAMPLE_ROWS"));
			table.setAlterDate(tblRset.getString("ALTERDATE"));
			table.setAlterTime(tblRset.getString("ALTERTIME"));
			table.setArchive(tblRset.getString("ARCHIVE"));
			table.setComment(tblRset.getString("COMMENT"));
			table.setCreateDate(tblRset.getString("CREATEDATE"));
			table.setCreateTime(tblRset.getString("CREATETIME"));
			// TODO: this needs to be escaped, otherwise it breaks the XML
			// table.setTableid(tblRset.getString("TABLEID"));
			table.setUnloaded(tblRset.getString("UNLOADED"));
			table.setUpdStatDate(tblRset.getString("UPDSTATDATE"));
			table.setUpdStatTime(tblRset.getString("UPDSTATTIME"));
			table.setVariableColumns(tblRset.getString("VARIABLE_COLUMNS"));
			table.setPrivileges(tblRset.getString("PRIVILEGES"));

			reverseEngineerTableColumns(conn, catalog, schema, table);

			reverseEngineerTablePK(conn, catalog, schema, table);

			reverseEngineerTableIndices(conn, catalog, schema, table);

			reverseEngineerTableFKs(conn, catalog, schema, table);

			reverseEngineerTableTriggers(conn, catalog, schema, table);
		}

		Grt.getInstance().addProgress("", -1);
		Grt.getInstance().flushMessages();

		stmt.close();

		return 0;
	}

	private static String tableColumnSelect75 = "SELECT * FROM COLUMNS "
			+ "WHERE OWNER=? AND TABLENAME=? "
			+ "ORDER BY TABLENAME ASC, POS ASC, KEYPOS ASC";

	private static String tableColumnSelect76 = "SELECT * FROM COLUMNS "
			+ "WHERE SCHEMANAME=? AND TABLENAME=? "
			+ "ORDER BY TABLENAME ASC, POS ASC, KEYPOS ASC";

	protected void reverseEngineerTableColumns(Connection conn,
			Catalog catalog, Schema schema, Table table) {

		String defaultValue = "";

		try {
			Grt.getInstance().addMsg("Fetching column information.");
			Grt.getInstance().addMsgDetail(
					(this.useRealSchema(conn)) ? tableColumnSelect76
							: tableColumnSelect75);

			PreparedStatement stmt = conn.prepareStatement((this
					.useRealSchema(conn)) ? tableColumnSelect76
					: tableColumnSelect75);
			stmt.setString(1, schema.getName());
			stmt.setString(2, table.getName());

			ResultSet colRset = stmt.executeQuery();
			while (colRset.next()) {
				// create new column
				Column column = new Column(table);
				table.getColumns().add(column);

				column.setName(colRset.getString("COLUMNNAME"));
				column.setDatatypeName(colRset.getString("DATATYPE"));
				column.setComment(colRset.getString("COMMENT"));

				column.setDefaultFunction(colRset.getString("DEFAULTFUNCTION"));

				defaultValue = colRset.getString("DEFAULT");
				column.setDefaultValue(defaultValue);
				if ((colRset.wasNull())
						&& (colRset.getString("NULLABLE").compareToIgnoreCase(
								"YES") == 0)) {
					column.setDefaultValueIsNull(1);
				} else {
					column.setDefaultValueIsNull(0);
				}

				if (colRset.getString("NULLABLE").compareToIgnoreCase("YES") == 0) {
					column.setIsNullable(1);
				} else {
					column.setIsNullable(0);
				}

				column.setCodeType(colRset.getString("CODETYPE"));

				// TODO: check the meaning of length
				// MaxDB manual: Length or precision of column
				column.setLength(colRset.getInt("LEN"));
				column.setPrecision(colRset.getInt("LEN"));

				// TODO: check meaning - OK for FIXED
				column.setScale(colRset.getInt("DEC"));

				column.setAlterDate(colRset.getString("ALTERDATE"));
				column.setAlterTime(colRset.getString("ALTERTIME"));
				column.setCreateDate(colRset.getString("CREATEDATE"));
				column.setCreateTime(colRset.getString("CREATETIME"));
				column.setPrivileges(colRset.getString("COLUMNPRIVILEGES"));
				column.setDomainOwner(colRset.getString("DOMAINOWNER"));
				column.setDomainName(colRset.getString("DOMAINNAME"));

				// Get Simple Type
				int datatypeIndex = catalog.getSimpleDatatypes()
						.getIndexOfName(column.getDatatypeName());

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

			}

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String tablePKSelect75 = "SELECT COLUMNNAME FROM COLUMNS "
			+ "WHERE OWNER=? AND TABLENAME=? AND MODE='KEY' "
			+ "ORDER BY KEYPOS ASC";

	private static String tablePKSelect76 = "SELECT COLUMNNAME FROM COLUMNS "
			+ "WHERE SCHEMANAME=? AND TABLENAME=? AND MODE='KEY' "
			+ "ORDER BY KEYPOS ASC";

	protected void reverseEngineerTablePK(Connection conn, Catalog catalog,
			Schema schema, Table table) throws Exception {

		String tablePKSelect = "";

		try {

			Grt.getInstance().addMsg("Fetching primary key information.");
			tablePKSelect = (this.useRealSchema(conn)) ? tablePKSelect76
					: tablePKSelect75;
			Grt.getInstance().addMsgDetail(tablePKSelect);

			PreparedStatement stmt = conn.prepareStatement(tablePKSelect);
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

				String indexColumnName = colRset.getString("COLUMNNAME");
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

	private static String tableIndexSelect75 = "SELECT"
			+ "  idx.OWNER, idx.INDEXNAME, "
			+ "  idx.TYPE as INDEX_TYPE, idx.CREATEDATE as INDEX_CREATEDATE, "
			+ "  idx.CREATETIME as INDEX_CREATETIME, idx.INDEX_USED, "
			+ "  idx.DISABLED, idx.COMMENT as INDEX_COMMENT, "
			+ "  idxcols.COLUMNNAME, "
			+ "  idxcols.SORT, idxcols.COLUMNNO, idxcols.DATATYPE, idxcols.LEN, "
			+ "  idxcols.CREATEDATE as COLUMN_CREATEDATE, idxcols.CREATETIME as COLUMN_CREATETIME, "
			+ "  idxcols.COMMENT as COLUMN_COMMENT " + "FROM "
			+ "  INDEXES AS idx, INDEXCOLUMNS as idxcols " + "WHERE "
			+ "  idx.OWNER=? " + "  AND idx.TABLENAME=? "
			+ "  AND idx.OWNER = idxcols.OWNER "
			+ "  AND idx.TABLENAME = idxcols.TABLENAME "
			+ "  AND idx.INDEXNAME = idxcols.INDEXNAME " + "ORDER BY "
			+ "  INDEXNAME ASC, COLUMNNO ASC";

	private static String tableIndexSelect76 = "SELECT "
			+ "  idx.SCHEMANAME, idx.OWNER, idx.INDEXNAME, idx.FILEID as INDEX_FILEID, "
			+ "  idx.TYPE as INDEX_TYPE, idx.CREATEDATE as INDEX_CREATEDATE, "
			+ "  idx.CREATETIME as INDEX_CREATETIME, idx.INDEX_USED, idx.FILESTATE as INDEX_FILESTATE, "
			+ "  idx.DISABLED, idx.COMMENT as INDEX_COMMENT, "
			+ "  idxcols.FILEID as COLUMN_FILEID, idxcols.COLUMNNAME, "
			+ "  idxcols.FILESTATE as COLUMN_FILESTATE, "
			+ "  idxcols.SORT, idxcols.COLUMNNO, idxcols.DATATYPE, idxcols.LEN, "
			+ "  idxcols.CREATEDATE as COLUMN_CREATEDATE, idxcols.CREATETIME as COLUMN_CREATETIME, "
			+ "  idxcols.COMMENT as COLUMN_COMMENT " + "FROM "
			+ "  INDEXES AS idx, INDEXCOLUMNS as idxcols " + "WHERE "
			+ "  idx.SCHEMANAME=? " + "  AND idx.TABLENAME=? "
			+ "  AND idx.SCHEMANAME = idxcols.SCHEMANAME "
			+ "  AND idx.TABLENAME = idxcols.TABLENAME "
			+ "  AND idx.INDEXNAME = idxcols.INDEXNAME " + "ORDER BY "
			+ "  INDEXNAME ASC, COLUMNNO ASC";

	private static String tableIndexStatisticsSelect75 = "SELECT * FROM INDEXSTATISTICS WHERE "
			+ "OWNER=? AND TABLENAME=? AND INDEXNAME=?";

	private static String tableIndexStatisticsSelect76 = "SELECT * FROM INDEXSTATISTICS WHERE "
			+ "SCHEMANAME=? AND TABLENAME=? AND INDEXNAME=?";

	protected void reverseEngineerTableIndices(Connection conn,
			Catalog catalog, Schema schema, Table table) {

		String tableIndexSelect = "";
		String tableIndexStatisticsSelect = "";

		try {

			if (this.useRealSchema(conn)) {
				tableIndexSelect = tableIndexSelect76;
				tableIndexStatisticsSelect = tableIndexStatisticsSelect76;
			} else {
				tableIndexSelect = tableIndexSelect75;
				tableIndexStatisticsSelect = tableIndexStatisticsSelect75;
			}

			Grt.getInstance().addMsg("Fetching indices information.");
			Grt.getInstance().addMsgDetail(tableIndexSelect);
			Grt.getInstance().addMsgDetail(tableIndexStatisticsSelect);

			PreparedStatement stmt = conn.prepareStatement(tableIndexSelect);
			stmt.setString(1, schema.getName());
			stmt.setString(2, table.getName());

			ResultSet rset = stmt.executeQuery();

			String indexName = "";
			Index index = null;

			while (rset.next()) {
				String newIndexName = rset.getString("INDEXNAME");

				if (indexName.compareTo(newIndexName) != 0) {
					if (index != null)
						table.getIndices().add(index);

					index = new Index(table);
					index.setName(newIndexName);
					indexName = newIndexName;

					/* Properties from the sytem table INDEXES */
					index.setCreateDate(rset.getString("INDEX_CREATEDATE"));
					index.setCreateTime(rset.getString("INDEX_CREATETIME"));
					index.setComment(rset.getString("INDEX_COMMENT"));
					index.setDeferability(0);

					if (rset.getString("DISABLED").compareToIgnoreCase("YES") == 0) {
						index.setDisabled(1);
					} else {
						index.setDisabled(0);
					}

					if (this.useRealSchema(conn)) {
						// TODO: this needs to be escaped, otherwise it breaks
						// the XML
						// index.setFileid(rset.getString("INDEX_FILEID"));
						/* TODO: currently SAP does not document the meaning */
						index.setFilestate(rset.getString("INDEX_FILESTATE"));
					}
					index.setIndexType(rset.getString("INDEX_TYPE"));

					if (rset.getInt("INDEX_USED") > 0) {
						index.setIndexUsed(1);
					} else {
						index.setIndexUsed(0);
					}

					if (rset.getString("INDEX_TYPE").compareToIgnoreCase(
							"UNIQUE") == 0) {
						index.setUnique(1);
					} else {
						index.setUnique(0);
					}

					// Index statistics
					PreparedStatement stmtStats = conn
							.prepareStatement(tableIndexStatisticsSelect);
					stmtStats.setString(1, schema.getName());
					stmtStats.setString(2, table.getName());
					stmtStats.setString(3, index.getName());

					// That code is ugly, but don't blame me for MaxDB's strange
					// storage
					// and Java's clean code philosophy - I love PHP ;-)

					String valueDesc = "";
					ResultSet rsetStats = stmtStats.executeQuery();
					while (rsetStats.next()) {
						valueDesc = rsetStats.getString("DESCRIPTION");

						if (valueDesc
								.compareToIgnoreCase("Avg primary keys per list") == 0) {
							index.setAvgNumPkPerList(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Avg secondary key length") == 0) {
							index.setAverageSkLength(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Avg separator length") == 0) {
							index.setAvgSeperatorLength(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc.compareToIgnoreCase("Filetype") == 0) {
							index.setFilestate(rsetStats
									.getString("CHAR_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Index levels") == 0) {
							index.setIndexLevels(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc.compareToIgnoreCase("Index pages") == 0) {
							index.setIndexPages(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc.compareToIgnoreCase("Leaf  pages") == 0) {
							index.setLeafPages(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Max primary keys per list") == 0) {
							index.setMaxNumPkPerList(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Max secondary key length") == 0) {
							index.setMaxSkLength(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Max separator length") == 0) {
							index.setMaxSeperatorLength(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Min primary keys per list") == 0) {
							index.setMinNumPkPerList(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Min secondary key length") == 0) {
							index.setMinSkLength(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Min separator length") == 0) {
							index.setMinSeperatorLength(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Primary keys") == 0) {
							index.setNumRowsPk(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc.compareToIgnoreCase("Root pno") == 0) {
							index
									.setRootPage(rsetStats
											.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Secondary keys (index lists)") == 0) {
							index.setNumDistinctValues(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Space used in all   pages (%)") == 0) {
							index.setTotalSpaceUsed(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Space used in index pages (%)") == 0) {
							index.setIndexPagesSpace(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Space used in index pages (%) max") == 0) {
							index.setIndexPageSpaceMax(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Space used in index pages (%) min") == 0) {
							index.setIndexPageSpaceMin(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Space used in leaf  pages (%)") == 0) {
							index.setLeafPagesSpace(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Space used in leaf  pages (%) max") == 0) {
							index.setLeafPagesSpaceMax(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Space used in leaf  pages (%) min") == 0) {
							index.setLeafPagesSpaceMin(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Space used in root  page  (%)") == 0) {
							index.setRootPageSpace(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc.compareToIgnoreCase("Used  pages") == 0) {
							index.setUsedPages(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Values with selectivity <=  1%") == 0) {
							index.setSelectivityLess1Percent(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Values with selectivity <=  5%") == 0) {
							index.setSelectivityLess5Percent(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Values with selectivity <= 10%") == 0) {
							index.setSelectivityLess10Percent(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Values with selectivity <= 25%") == 0) {
							index.setSelectivityLess25Percent(rsetStats
									.getInt("NUMERIC_VALUE"));

						} else if (valueDesc
								.compareToIgnoreCase("Values with selectivity >  25%") == 0) {
							index.setSelectivityGreater25Percent(rsetStats
									.getInt("NUMERIC_VALUE"));

						}
					}

				}

				IndexColumn indexColumn = new IndexColumn(index);
				indexColumn.setName(rset.getString("COLUMNNAME"));

				indexColumn.setColumnLength(rset.getInt("LEN"));
				indexColumn.setComment(rset.getString("COLUMN_COMMENT"));
				indexColumn.setCreateDate(rset.getString("COLUMN_CREATEDATE"));
				indexColumn.setCreateTime(rset.getString("COLUMN_CREATETIME"));

				if (rset.getString("SORT").compareToIgnoreCase("ASC") == 0) {
					indexColumn.setDescend(1);
				} else {
					indexColumn.setDescend(0);
				}

				if (this.useRealSchema(conn)) {
					// TODO: this needs to be escaped, otherwise it breaks the
					// XML
					// indexColumn.setFileid(rset.getString("COLUMN_FILEID"));
					indexColumn
							.setFilestate(rset.getString("COLUMN_FILESTATE"));
				}

				// find reference table column
				for (int j = 0; j < table.getColumns().size(); j++) {
					Column column = (Column) (table.getColumns().get(j));

					if (column.getName().compareTo(indexColumn.getName()) == 0) {
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

	private static String tableFKSelect75 = "SELECT "
			+ "  fk.OWNER, fk.TABLENAME, fk.FKEYNAME, fk.RULE, "
			+ "  fk.CREATEDATE, fk.CREATETIME, "
			+ "  fkcols.COLUMNNAME, fkcols.REFOWNER, "
			+ "  fkcols.REFTABLENAME, fkcols.REFCOLUMNNAME, fkcols.RULE "
			+ "FROM " + "  FOREIGNKEYS as fk, "
			+ "  FOREIGNKEYCOLUMNS as fkcols " + "WHERE "
			+ "  fk.OWNER=? AND fk.TABLENAME=?"
			+ "  AND fk.OWNER = fkcols.OWNER "
			+ "  AND fk.TABLENAME = fkcols.TABLENAME "
			+ "  AND fk.FKEYNAME = fkcols.FKEYNAME " + "ORDER BY "
			+ "  FKEYNAME ASC";

	private static String tableFKSelect76 = "SELECT "
			+ "  fk.SCHEMANAME, fk.OWNER, fk.TABLENAME, fk.FKEYNAME, fk.RULE, "
			+ "  fk.CREATEDATE, fk.CREATETIME, "
			+ "  fkcols.COLUMNNAME, fkcols.REFSCHEMANAME, fkcols.REFOWNER, "
			+ "  fkcols.REFTABLENAME, fkcols.REFCOLUMNNAME, fkcols.RULE "
			+ "FROM " + "  FOREIGNKEYS as fk, "
			+ "  FOREIGNKEYCOLUMNS as fkcols " + "WHERE "
			+ "  fk.SCHEMANAME=? AND fk.TABLENAME=? "
			+ "  AND fk.SCHEMANAME = fkcols.SCHEMANAME "
			+ "  AND fk.TABLENAME = fkcols.TABLENAME "
			+ "  AND fk.FKEYNAME = fkcols.FKEYNAME " + "ORDER BY "
			+ "  FKEYNAME ASC";

	protected void reverseEngineerTableFKs(Connection conn, Catalog catalog,
			Schema schema, Table table) {

		String tableFKSelect = "";

		try {
			Grt.getInstance().addMsg("Fetching FK information.");
			tableFKSelect = (this.useRealSchema(conn)) ? tableFKSelect76
					: tableFKSelect75;
			Grt.getInstance().addMsgDetail(tableFKSelect);

			PreparedStatement stmt = conn.prepareStatement(tableFKSelect);
			stmt.setString(1, schema.getName());
			stmt.setString(2, table.getName());

			ResultSet rset = stmt.executeQuery();

			String fkName = "";
			ForeignKey foreignKey = null;

			while (rset.next()) {
				String newFkName = rset.getString("FKEYNAME");

				if (fkName.compareTo(newFkName) != 0) {
					if (foreignKey != null)
						table.getForeignKeys().add(foreignKey);

					foreignKey = new ForeignKey(table);

					foreignKey.setName(newFkName);

					foreignKey.setCreateDate(rset.getString("CREATEDATE"));
					foreignKey.setCreateTime(rset.getString("CREATETIME"));
					foreignKey.setDeferability(0);

					foreignKey.setDeleteRule(rset.getString("RULE")
							.substring(7));

					if (this.useRealSchema(conn)) {
						foreignKey.setReferedTableSchemaName(rset
								.getString("REFSCHEMANAME"));
					} else {
						foreignKey.setReferedTableSchemaName(rset
								.getString("REFOWNER"));
					}
					foreignKey.setReferedTableName(rset
							.getString("REFTABLENAME"));
				}

				foreignKey.getReferedColumnNames().add(
						rset.getString("REFCOLUMNNAME"));

				// find reference table column
				String colName = rset.getString("REFCOLUMNNAME");
				for (int j = 0; j < table.getColumns().size(); j++) {
					Column column = (Column) (table.getColumns().get(j));

					if (column.getName().compareTo(colName) == 0)
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

	private static String tableTriggerSelect75 = "SELECT * FROM TRIGGERS WHERE "
			+ "OWNER=? AND TABLENAME=? ORDER BY TRIGGERNAME ASC";

	private static String tableTriggerSelect76 = "SELECT * FROM TRIGGERS WHERE "
			+ "SCHEMANAME=? AND TABLENAME=? ORDER BY TRIGGERNAME ASC";

	protected void reverseEngineerTableTriggers(Connection conn,
			Catalog catalog, Schema schema, Table table) {

		String tableTriggerSelect = "";
		String triggerStatements = "";

		try {
			Grt.getInstance().addMsg("Fetching FK information.");
			tableTriggerSelect = (this.useRealSchema(conn)) ? tableTriggerSelect76
					: tableTriggerSelect75;
			Grt.getInstance().addMsgDetail(tableTriggerSelect);

			PreparedStatement stmt = conn.prepareStatement(tableTriggerSelect);
			stmt.setString(1, schema.getName());
			stmt.setString(2, table.getName());

			ResultSet rset = stmt.executeQuery();

			while (rset.next()) {
				Trigger trigger = new Trigger(table);

				trigger.setName(rset.getString("TRIGGERNAME"));
				trigger.setOldName(trigger.getName());

				trigger.setComment(rset.getString("COMMENT"));
				trigger.setCreateDate(rset.getString("CREATEDATE"));
				trigger.setCreateTime(rset.getString("CREATETIME"));
				trigger.setDefinition(rset.getString("DEFINITION"));
				trigger.setTiming("AFTER");
				trigger.setOrder(0);
				trigger.setEnabled(1);

				if (rset.getString("INSERT").equalsIgnoreCase("YES")) {
					trigger.setEvent("INSERT");
				} else if (rset.getString("UPDATE").equalsIgnoreCase("YES")) {
					trigger.setEvent("UPDATE");
				} else if (rset.getString("DELETE").equalsIgnoreCase("YES")) {
					trigger.setEvent("DELETE");
				}

				triggerStatements = trigger.getDefinition().trim();
				triggerStatements = triggerStatements.substring(
						triggerStatements.indexOf("(") + 1, triggerStatements
								.lastIndexOf(")") - 1);
				trigger.setStatement(triggerStatements.trim());

				table.getTriggers().add(trigger);
			}

			stmt.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String viewSelect75 = "SELECT " + "  v.*, vd.DEFINITION "
			+ "FROM " + "  VIEWS AS v, " + "  VIEWDEFS AS vd " + "WHERE "
			+ "  v.OWNER=? " + "  AND v.OWNER = vd.OWNER "
			+ "  AND v.VIEWNAME = vd.VIEWNAME " + "  AND v.TYPE='VIEW'"
			+ "ORDER BY " + "  VIEWNAME ASC";

	private static String viewSelect76 = "SELECT " + "  v.*, vd.DEFINITION "
			+ "FROM " + "  VIEWS AS v, " + "  VIEWDEFS AS vd " + "WHERE "
			+ "  v.SCHEMANAME=? " + "  AND v.SCHEMANAME = vd.SCHEMANAME "
			+ "  AND v.VIEWNAME = vd.VIEWNAME " + "  AND v.TYPE='VIEW' "
			+ "ORDER BY " + "  VIEWNAME ASC";

	protected void reverseEngineerViews(Connection conn, Catalog catalog,
			Schema schema) throws Exception {

		String viewSelect = "";

		viewSelect = (this.useRealSchema(conn)) ? viewSelect76 : viewSelect75;

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

			view.setName(rset.getString("VIEWNAME"));
			Grt.getInstance().addMsg("Processing view " + view.getName() + ".");

			view.setAlterDate(rset.getString("ALTERDATE"));
			view.setAlterTime(rset.getString("ALTERTIME"));
			view.setComment(rset.getString("COMMENT"));
			view.setCreateDate(rset.getString("CREATEDATE"));
			view.setCreateTime(rset.getString("CREATETIME"));

			if (rset.getString("COMPLEX").compareToIgnoreCase("YES") == 0) {
				view.setIsReadOnly(1);
			} else {
				view.setIsReadOnly(0);
			}

			view.setPrivileges(rset.getString("PRIVILEGES"));
			view.setQueryExpression(rset.getString("DEFINITION"));
			view.setUnloaded(rset.getString("UNLOADED"));
			view.setUpdStatDate(rset.getString("UPDSTATDATE"));
			view.setUpdStatTime(rset.getString("UPDSTATTIME"));

			if (rset.getString("WITHCHECKOPTION").compareToIgnoreCase("YES") == 0) {
				view.setWithCheckCondition(1);
			} else {
				view.setWithCheckCondition(0);
			}

		}

		stmt.close();

		Grt.getInstance().addMsg("Views fetched.");
	}

	private static String ProcedureSelect = "SELECT "
			+ "  DBPROCNAME AS PROCNAME, CREATEDATE, CREATETIME, "
			+ "  OWNER, COMMENT, DEFINITION " + "FROM " + "  DBPROCEDURES "
			+ "WHERE " + "  PACKAGE IS NULL " + "  AND EXECUTION_KIND IS NULL "
			+ "  AND LANGUAGE = 'SPL' ";

	private static String FunctionSelect = "SELECT "
			+ "  FUNCTIONNAME AS PROCNAME, CREATEDATE, CREATETIME, "
			+ "  OWNER, COMMENT, DEFINITION " + "FROM " + "  FUNCTIONS "
			+ "WHERE ";

	private static String ProcedureCountSelect = "SELECT "
			+ "COUNT(*) AS NUM_PROCEDURES FROM DBPROCEDURES "
			+ "WHERE PACKAGE IS NULL AND EXECUTION_KIND IS NULL AND LANGUAGE = 'SPL' ";

	private static String FunctionCountSelect = "SELECT "
			+ "COUNT(*) AS NUM_FUNCTIONS FROM FUNCTIONS WHERE ";

	protected int reverseEngineerProcedures(Connection conn, Catalog catalog,
			Schema schema) throws Exception {
		int spCount = 0;
		int currentSpNumber = 0;
		String procedureSelect = "";
		String procedureCountSelect = "";

		if (this.useRealSchema(conn)) {
			procedureSelect = ProcedureSelect + "AND SCHEMANAME=?";
			procedureCountSelect = ProcedureCountSelect + "AND SCHEMANAME=?";
		} else {
			procedureSelect = ProcedureSelect + "AND OWNER=?";
			procedureCountSelect = ProcedureCountSelect + "AND OWNER=?";
		}
		procedureSelect += " ORDER BY PROCNAME ASC";

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
				spCount = rset.getInt("NUM_PROCEDURES");
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

				Routine proc = new Routine(schema);
				schema.getRoutines().add(proc);

				proc.setName(rset.getString("PROCNAME"));

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

				proc.setComment(rset.getString("COMMENT"));
				proc.setRoutineCode(rset.getString("DEFINITION"));
				proc.setRoutineType("PROCEUDRE");
				proc.setCreateDate(rset.getString("CREATEDATE"));
				proc.setCreateTime(rset.getString("CREATETIME"));
			}
			stmt.close();

			Grt.getInstance().addMsg("Stored procedures fetched.");

			if (this.useRealSchema(conn)) {

				String functionSelect = FunctionSelect
						+ "SCHEMANAME=? ORDER BY PROCNAME ASC";
				String functionCountSelect = FunctionCountSelect
						+ "SCHEMANAME=?";

				stmt = conn.prepareStatement(functionCountSelect);
				stmt.setString(1, schema.getName());
				rset = stmt.executeQuery();
				if (rset.next()) {
					spCount = rset.getInt("NUM_FUNCTIONS");
				}

				Grt.getInstance().addMsg(
						"Fetching " + spCount
								+ " stored functions(s) of the schema "
								+ schema.getName() + ".");
				Grt.getInstance().addMsgDetail(functionSelect);
				Grt.getInstance().flushMessages();

				stmt = conn.prepareStatement(functionSelect);
				stmt.setString(1, schema.getName());

				rset = stmt.executeQuery();
				while (rset.next()) {

					Routine proc = new Routine(schema);
					schema.getRoutines().add(proc);

					proc.setName(rset.getString("PROCNAME"));

					currentSpNumber++;

					Grt.getInstance().addProgress(
							"Processing stored function " + proc.getName()
									+ ".", (currentSpNumber * 100) / spCount);
					if (Grt.getInstance().flushMessages() != 0) {
						Grt.getInstance().addMsg("Migration canceled by user.");
						return 1;
					}

					Grt.getInstance().addMsg(
							"Processing function " + proc.getName() + ".");

					proc.setComment(rset.getString("COMMENT"));
					proc.setRoutineCode(rset.getString("DEFINITION"));
					proc.setRoutineType("PROCEUDRE");
					proc.setCreateDate(rset.getString("CREATEDATE"));
					proc.setCreateTime(rset.getString("CREATETIME"));
				}
				stmt.close();

				Grt.getInstance().addMsg("Stored functions fetched.");
			}

			Grt.getInstance().addProgress("", -1);

		} catch (Exception e) {
			Grt
					.getInstance()
					.addMsg(
							"Stored procedures and stored functions cannot be fetched.");
			Grt.getInstance().addMsgDetail(e.getMessage());
		}

		return 0;
	}

	private static String sequencesCountSelect76 = "SELECT COUNT(*) AS NUM_SEQUENCES "
			+ "FROM SEQUENCES WHERE SCHEMANAME=?";

	private static String sequencesSelect76 = "SELECT * FROM SEQUENCES "
			+ "WHERE SCHEMANAME=? ORDER BY SEQUENCE_NAME ASC";

	private static String sequencesCountSelect75 = "SELECT COUNT(*) AS NUM_SEQUENCES "
			+ "FROM SEQUENCES WHERE OWNER=?";

	private static String sequencesSelect75 = "SELECT * FROM SEQUENCES "
			+ "WHERE OWNER=?";

	protected int reverseEngineerSequences(Connection conn, Catalog catalog,
			Schema schema) throws Exception {
		int sequencesCount = 0;
		int currentSequenceNumber = 0;
		String sequencesSelect = "";
		String sequencesCountSelect = "";

		if (this.useRealSchema(conn)) {
			sequencesSelect = sequencesSelect76;
			sequencesCountSelect = sequencesCountSelect76;
		} else {
			sequencesSelect = sequencesSelect75;
			sequencesCountSelect = sequencesCountSelect75;
		}

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
				sequencesCount = rset.getInt("NUM_SEQUENCES");
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

				seq.setCacheSize(rset.getString("CACHE_SIZE"));
				seq.setComment(rset.getString("COMMENT"));
				seq.setCreateDate(rset.getString("CREATEDATE"));
				seq.setCreateTime(rset.getString("CREATETIME"));

				if (rset.getString("CYCLE_FLAG").compareToIgnoreCase("Y") == 0) {
					seq.setCycleFlag(1);
				} else {
					seq.setCycleFlag(0);
				}

				seq.setIncrementBy(rset.getString("INCREMENT_BY"));
				seq.setLastNumber(rset.getString("LAST_NUMBER"));
				seq.setMaxValue(rset.getString("MAX_VALUE"));
				seq.setMinValue(rset.getString("MIN_VALUE"));

				if (rset.getString("ORDER_FLAG").compareToIgnoreCase("Y") == 0) {
					seq.setOrderFlag(1);
				} else {
					seq.setOrderFlag(0);
				}

				schema.getSequences().add(seq);
			}

			stmt.close();

			Grt.getInstance().addMsg("Sequences fetched.");

			Grt.getInstance().addProgress("", -1);

		} catch (Exception e) {
			Grt.getInstance().addMsg("Sequences could not be fetched.");
			Grt.getInstance().addMsg(e.getMessage());
		}

		return 0;
	}

	private static String synonymsSelect76 = "SELECT * FROM SYNONYMS "
			+ "WHERE TABLESCHEMANAME=? ORDER BY TABLENAME ASC";

	private static String synonymsSelect75 = "SELECT * FROM SYNONYMS "
			+ "WHERE TABLEOWNER=? ORDER BY TABLENAME ASC";

	/**
	 * Creates a list of table synonyms
	 * 
	 * @param conn
	 * @param catalog
	 * @param schema
	 * @return int
	 * @throws Exception
	 */
	protected int reverseEngineerSynonyms(Connection conn, Catalog catalog,
			Schema schema) throws Exception {

		String synonymsSelect = "";
		synonymsSelect = (this.useRealSchema(conn)) ? synonymsSelect76
				: synonymsSelect75;

		Grt.getInstance().addMsg(
				"Fetch all table synonyms of the schema " + schema.getName()
						+ ".");
		Grt.getInstance().addMsgDetail(synonymsSelect);
		Grt.getInstance().flushMessages();

		PreparedStatement stmt = conn.prepareStatement(synonymsSelect);
		stmt.setString(1, schema.getName());

		ResultSet rset = stmt.executeQuery();

		while (rset.next()) {

			Synonym syn = new Synonym(schema);
			schema.getSynonyms().add(syn);

			syn.setName(rset.getString("SYNONYMNAME"));
			Grt.getInstance().addMsg(
					"Processing synonym " + syn.getName() + ".");

			syn.setComment(rset.getString("COMMENT"));
			syn.setCreateDate(rset.getString("CREATEDATE"));
			syn.setCreateTime(rset.getString("CREATETIME"));

			if (rset.getString("PUBLIC").compareToIgnoreCase("YES") == 0) {
				syn.setIsPublic(1);
			} else {
				syn.setIsPublic(0);
			}

			// TODO: this is a consequence of the query
			// How do we handle system internal synonyms?
			syn.setReferedSchemaName(schema.getName());

			// find reference table column
			String tableName = rset.getString("TABLENAME");
			for (int i = 0; i < schema.getTables().size(); i++) {
				Table table = (Table) (schema.getTables().get(i));

				if (table.getName().compareTo(tableName) == 0) {
					syn.setReferedObject(table);
				}
			}
		}

		stmt.close();

		Grt.getInstance().addMsg("Synonyms fetched.");

		return 0;
	}

	/**
	 * Checks the MaxDB version and returns 1 if the version supports real SQL
	 * schemas
	 * 
	 * @param conn
	 * @return
	 * @throws Exception
	 */
	protected boolean useRealSchema(Connection conn) throws Exception {

		DatabaseMetaData metaData = conn.getMetaData();

		return ((metaData.getDatabaseMajorVersion() >= 7) && (metaData
				.getDatabaseMinorVersion() >= 6));
	}
}