package com.mysql.grt.modules;

import java.sql.*;

import com.mysql.grt.*;
import com.mysql.grt.db.*;

import java.util.regex.*;
import java.util.List;
import java.util.ArrayList;

/**
 * GRT Reverse Engineering Class using generic JDBC metadata functions
 * 
 * @author Mike
 * @version 1.0, 11/26/04
 * 
 */
public class ReverseEngineeringAccess extends ReverseEngineeringGeneric {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(ReverseEngineeringAccess.class,
				"ReverseEngineering");
	}

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

		Grt.getInstance().addMsg(
				"Create a dummy schema list because "
						+ "Access only has one schema for each file.");

		GrtStringList schemataList = new GrtStringList();

		// Filter schemaName
		String schemaName = dbConn.getParameterValues().get("databaseFile");

		Pattern p = Pattern.compile("(\\w*)\\.mdb", Pattern.CASE_INSENSITIVE);
		Matcher m = p.matcher(schemaName);

		if (m.find())
			schemaName = schemaName.substring(m.start(), m.end() - 4);
		else
			schemaName = "Access";

		schemataList.add(schemaName);

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
	public static Catalog reverseEngineer(
			com.mysql.grt.db.mgmt.Connection dbConn, GrtStringList schemataList)
			throws Exception {

		boolean reverseEngineerOnlyTableObjects = (Grt.getInstance()
				.getGrtGlobalAsInt(
						"/migration/applicationData/"
								+ "reverseEngineerOnlyTableObjects") == 1);

		ReverseEngineeringAccess revEng = new ReverseEngineeringAccess();

		// connect to the database
		Connection conn = establishConnection(dbConn);

		Catalog catalog = new Catalog(null);
		catalog.setName("Catalog");

		catalog.setVersion(getVersion(dbConn));

		Grt.getInstance().addMsg("Build simple datatypes.");
		revEng.buildSimpleDatatypes(conn, catalog);

		// If an empty list was given, use a list with an empty string instead
		if (schemataList.size() == 0)
			schemataList.add("");

		for (int i = 0; i < schemataList.size(); i++) {
			Schema schema = new Schema(catalog);
			schema.setName((String) (schemataList.get(i)));
			catalog.getSchemata().add(schema);

			// Get Tables
			revEng.reverseEngineerTables(conn, catalog, schema);

			if (!reverseEngineerOnlyTableObjects) {

				// Get Views
				revEng.reverseEngineerViews(conn, catalog, schema);
			}
		}

		// make sure the Fks use real references instead of
		// text names where possible
		revEng.reverseEngineerUpdateFkReferences(catalog);

		return catalog;
	}

	protected void reverseEngineerTables(Connection conn, Catalog catalog,
			Schema schema) throws Exception {

		Grt.getInstance().addMsg("Fetch all tables of the given schemata.");

		ResultSet rset = conn.getMetaData().getTables(null, null, null,
				new String[] { "TABLE" });
		while (rset.next()) {
			String tableName = rset.getString("TABLE_NAME");

			if (tableName.startsWith("MSys"))
				continue;

			// Create new table
			Table table = new Table(schema);
			schema.getTables().add(table);

			table.setName(tableName);

			reverseEngineerTableColumns(conn, catalog, schema, table);

			reverseEngineerTableIndices(conn, catalog, schema, table);

			reverseEngineerTableFKs(conn, catalog, schema, table);
		}
	}

	protected void reverseEngineerTableColumns(Connection conn,
			Catalog catalog, Schema schema, Table table) {

		try {
			Grt.getInstance().addMsg(
					"Fetching column information of table " + table.getName()
							+ ".");

			ResultSet rset = conn.getMetaData().getColumns(null, null,
					table.getName(), null);
			while (rset.next()) {
				// create new column
				Column column = new Column(table);

				column.setName(rset.getString("COLUMN_NAME"));
				column.setDatatypeName(rset.getString("TYPE_NAME"));

				if (column.getDatatypeName().equals("BIGBINARY"))
					continue;

				table.getColumns().add(column);

				// Get Simple Type
				column.setSimpleType(catalog.getSimpleDatatypes().get(
						catalog.getSimpleDatatypes().getIndexOfName(
								column.getDatatypeName())));

				column.setLength(rset.getInt("COLUMN_SIZE"));
				column.setPrecision(column.getLength());
				column.setScale(rset.getInt("DECIMAL_DIGITS"));

				if (rset.getInt("NULLABLE") == java.sql.DatabaseMetaData.columnNullable)
					column.setIsNullable(1);
				else
					column.setIsNullable(0);

				column.setDefaultValue(rset.getString("COLUMN_DEF"));
			}

			rset.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	protected void reverseEngineerTableIndices(Connection conn,
			Catalog catalog, Schema schema, Table table) {
		try {
			Grt.getInstance().addMsg(
					"Fetching index information of table " + table.getName()
							+ ".");

			String indexName = "";
			Index index = null;

			ResultSet rset = conn.getMetaData().getIndexInfo(null, null,
					table.getName(), false, true);
			while (rset.next()) {
				String newIndexName = rset.getString("INDEX_NAME");

				if (newIndexName == null)
					continue;

				if (indexName.compareToIgnoreCase(newIndexName) != 0) {
					if (index != null)
						table.getIndices().add(index);

					index = new Index(table);
					index.setName(newIndexName);
					indexName = newIndexName;

					// do handle PrimaryKey indices
					if ((newIndexName.compareToIgnoreCase("PrimaryKey") == 0)
							|| (newIndexName.compareToIgnoreCase("Primary") == 0)
							|| (newIndexName
									.compareToIgnoreCase("Primärschlüssel") == 0)) {
						index.setName("PRIMARY");

						index.setIsPrimary(1);

						table.setPrimaryKey(index);
					} else
						index.setIsPrimary(0);

					if (rset.getBoolean("NON_UNIQUE"))
						index.setUnique(0);
					else
						index.setUnique(1);
				}

				IndexColumn indexColumn = new IndexColumn(index);
				indexColumn.setName(rset.getString("COLUMN_NAME"));
				indexColumn.setColumnLength(0);

				if (rset.getString("ASC_OR_DESC").compareToIgnoreCase("D") == 0)
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

		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	private static String tableFKSelect = "SELECT szRelationship, "
			+ " szColumn, szReferencedObject, szReferencedColumn, grbit "
			+ "FROM MSysRelationships " + "WHERE szObject=? "
			+ "ORDER BY szRelationship, icolumn";

	protected void reverseEngineerTableFKs(Connection conn, Catalog catalog,
			Schema schema, Table table) {

		try {
			Grt.getInstance().addMsg("Fetching FK information.");
			Grt.getInstance().addMsgDetail(tableFKSelect);

			PreparedStatement stmt = conn.prepareStatement(tableFKSelect);
			stmt.setString(1, table.getName());

			ResultSet rset = stmt.executeQuery();

			String fkName = "";
			ForeignKey foreignKey = null;

			while (rset.next()) {
				String newFkName = rset.getString("szRelationship");
				int grbit = rset.getInt("grbit");

				// no referential integrity
				if ((grbit & 2) == 1)
					continue;

				if (fkName.compareToIgnoreCase(newFkName) != 0) {
					if (foreignKey != null)
						table.getForeignKeys().add(foreignKey);

					fkName = newFkName;

					foreignKey = new ForeignKey(table);
					foreignKey.setName(newFkName);

					foreignKey.setDeferability(0);

					if ((grbit & 2) != 0) {
						foreignKey.setDeleteRule("NO ACTION");
						foreignKey.setUpdateRule("NO ACTION");
					} else {
						if ((grbit & 256) != 0)
							foreignKey.setUpdateRule("CASCADE");
						else
							foreignKey.setUpdateRule("RESTRICT");

						if ((grbit & 4096) != 0)
							foreignKey.setDeleteRule("CASCADE");
						else
							foreignKey.setDeleteRule("RESTRICT");
					}

					/*
					 * foreignKey.setDeleteRule("NO ACTION");
					 * foreignKey.setUpdateRule("NO ACTION");
					 * 
					 * if ((grbit & 256) != 0)
					 * foreignKey.setUpdateRule("CASCADE");
					 * 
					 * if ((grbit & 4096) != 0)
					 * foreignKey.setDeleteRule("CASCADE");
					 */

					foreignKey.setReferedTableSchemaName(schema.getName());
					foreignKey.setReferedTableName(rset
							.getString("szReferencedObject"));
				}

				foreignKey.getReferedColumnNames().add(
						rset.getString("szReferencedColumn"));

				// find reference table column
				String colName = rset.getString("szColumn");
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
			Grt.getInstance().addMsg("Cannot fetch foreign key information.");
			Grt.getInstance().addMsgDetail(e.getMessage());
		}
	}

	private static String viewSelect = "SELECT DISTINCTROW o.Name, o.Id, o.Flags "
			+ "FROM MSysObjects o "
			+ "WHERE o.Type = 5 AND Left(o.Name, 1) <> '~'"
			+ "ORDER BY o.Name;";

	private static String viewClauseSelect = "SELECT q.Attribute, q.Expression, "
			+ "q.Flag, q.Name1, q.Name2 "
			+ "FROM MSysQueries q "
			+ "WHERE q.ObjectId = ? " + "ORDER BY q.Attribute;";

	final static int MSA_CLAUSE_SELECT = 6;

	final static int MSA_CLAUSE_FROM = 5;

	final static int MSA_CLAUSE_JOIN = 7;

	final static int MSA_CLAUSE_JOIN_INNER_JOIN = 1;

	final static int MSA_CLAUSE_WHERE = 8;

	final static int MSA_CLAUSE_ORDER = 11;

	final static int QRY_CLAUSE_SELECT = 0;

	final static int QRY_CLAUSE_FROM = 1;

	final static int QRY_CLAUSE_WHERE = 2;

	final static int QRY_CLAUSE_GROUP = 3;

	final static int QRY_CLAUSE_HAVING = 4;

	final static int QRY_CLAUSE_ORDER = 5;

	protected void reverseEngineerViews(Connection conn, Catalog catalog,
			Schema schema) throws Exception {

		Grt.getInstance().addMsg("Fetch all views of the given schemata.");

		Statement stmt = conn.createStatement();
		ResultSet rset = null;

		try {
			rset = stmt.executeQuery(viewSelect);
		} catch (SQLException e) {
			Grt.getInstance().addMsg(
					"The views cannot be "
							+ "reverse engineered because the MS Access "
							+ "system tables cannot be accessed. "
							+ "Take a look at the manual to enable access "
							+ "to the system tables. " + e.getMessage());
			return;
		}

		while (rset.next()) {
			// Create new view
			View view = new View(schema);
			schema.getViews().add(view);

			view.setName(rset.getString("Name"));

			// int viewFlags = rset.getInt("Flags");

			int Id = rset.getInt("Id");

			// Fetch query expression clauses

			StringBuffer[] clauses = new StringBuffer[6];

			List fromTables = new ArrayList();

			PreparedStatement stmtClauses = conn
					.prepareStatement(viewClauseSelect);
			stmtClauses.setInt(1, Id);

			ResultSet rsetClauses = stmtClauses.executeQuery();
			while (rsetClauses.next()) {
				int cAttr = rsetClauses.getInt("Attribute");
				String cExp = rsetClauses.getString("Expression");
				int cFlag = rsetClauses.getInt("Flag");
				String cName1 = rsetClauses.getString("Name1");
				String cName2 = rsetClauses.getString("Name2");

				if (cAttr == MSA_CLAUSE_SELECT) {
					if (clauses[QRY_CLAUSE_SELECT] == null) {
						clauses[QRY_CLAUSE_SELECT] = new StringBuffer("SELECT ");
					} else {
						clauses[QRY_CLAUSE_SELECT].append(", ");
					}
					clauses[QRY_CLAUSE_SELECT].append(cExp);
				} else if (cAttr == MSA_CLAUSE_FROM) {
					// collect from tables so they do not collide with joins
					fromTables.add(cName1);
				} else if (cAttr == MSA_CLAUSE_JOIN) {
					if (cFlag == MSA_CLAUSE_JOIN_INNER_JOIN) {
						// INNER JOIN
						if (clauses[QRY_CLAUSE_FROM] == null) {
							clauses[QRY_CLAUSE_FROM] = new StringBuffer("FROM "
									+ cName1 + " INNER JOIN " + cName2 + " ON "
									+ cExp);
						} else {
							clauses[QRY_CLAUSE_FROM].append(", " + cName1
									+ " INNER JOIN " + cName2 + " ON " + cExp);
						}

						// remove the tables from the join from the fromTables
						// list
						fromTables.remove(cName1);
						fromTables.remove(cName2);
					}
				} else if (cAttr == MSA_CLAUSE_WHERE) {
					if (clauses[QRY_CLAUSE_WHERE] == null) {
						clauses[QRY_CLAUSE_WHERE] = new StringBuffer("WHERE ");
					} else {
						clauses[QRY_CLAUSE_WHERE].append(" AND ");
					}
					clauses[QRY_CLAUSE_WHERE].append(cExp);
				} else if (cAttr == MSA_CLAUSE_ORDER) {
					if (clauses[QRY_CLAUSE_ORDER] == null) {
						clauses[QRY_CLAUSE_ORDER] = new StringBuffer(
								"ORDER BY ");
					} else {
						clauses[QRY_CLAUSE_ORDER].append(", ");
					}
					clauses[QRY_CLAUSE_ORDER].append(cExp);
				}
			}
			stmtClauses.close();

			// Attach remaining tables to FROM clause
			for (int i = 0; i < fromTables.size(); i++) {
				if (clauses[QRY_CLAUSE_FROM] == null) {
					clauses[QRY_CLAUSE_FROM] = new StringBuffer("FROM ");
				} else {
					clauses[QRY_CLAUSE_FROM].append(", ");
				}
				clauses[QRY_CLAUSE_FROM].append(fromTables.get(i));
			}

			// Build query expression from clauses
			StringBuffer queryExpr = clauses[QRY_CLAUSE_SELECT];
			if (queryExpr == null)
				queryExpr = new StringBuffer("SELECT * ");

			for (int i = 1; i <= 5; i++) {
				if (clauses[i] != null) {
					queryExpr.append("\n");
					queryExpr.append(clauses[i]);
				}
			}

			view.setQueryExpression(queryExpr.toString());
		}
		stmt.close();
	}
}