package com.mysql.grt.modules;

import java.net.URLEncoder;
import java.sql.*;
import java.text.MessageFormat;
import java.text.ParsePosition;
import java.util.regex.*;

import com.mysql.grt.*;
import com.mysql.grt.db.*;
import com.mysql.grt.db.mgmt.*;

/**
 * GRT Reverse Engineering Class using generic JDBC metadata functions
 * 
 * @author Mike
 * @version 1.0, 11/26/04
 * 
 */
public class ReverseEngineeringGeneric {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(ReverseEngineeringGeneric.class, "");
	}

	public static String replace(String searchIn, String searchFor,
			String replaceWith) {
		if (searchIn == null) {
			return null;
		}
		if (searchFor == null || "".equals(searchFor)) {
			final String msg = "searchFor '" + searchFor + "' (searchIn = '"
					+ searchIn + "') (replaceWith='" + replaceWith + "')";
			throw new IllegalArgumentException(msg);
		}

		StringBuffer buf = new StringBuffer(searchIn.length());
		final int searchForLength = searchFor.length();
		int begin = 0;
		int end = 0;
		while (true) {
			end = searchIn.indexOf(searchFor, begin);
			if (end == -1) {
				break;
			}
			buf.append(searchIn.substring(begin, end));
			buf.append(replaceWith);
			begin = end + searchForLength;
		}

		buf.append(searchIn.substring(begin));
		return buf.toString();
	}

	/**
	 * Protected function that establishes a connection to the database
	 * 
	 * @param dbConn
	 *            the Connection to connect to
	 * 
	 * @return returns a Connection on success
	 */
	protected static java.sql.Connection establishConnection(
			com.mysql.grt.db.mgmt.Connection dbConn) throws Exception {
		com.mysql.grt.db.mgmt.Driver driver = dbConn.getDriver();

		if (!JdbcDriver.class.isInstance(driver))
			throw new Exception("The submitted driver is not a JDBC driver.");

		Grt.getInstance().addMsg("Initializing JDBC driver ... ");
		Grt.getInstance().addMsgDetail("Driver class " + driver.getCaption());

		// get classname
		String driverClassName = ((JdbcDriver) driver).getClassName();
		// if there is no classname entry in the driver, take it from the params
		if (driverClassName.equals(""))
			driverClassName = dbConn.getParameterValues().get("classname");
		Class.forName(driverClassName).newInstance();

		// build connection string
		// first, check if the jdbcConnStr param is set
		String jdbcConnectionString = dbConn.getParameterValues().get(
				"jdbcConnStr");
		if ((jdbcConnectionString == null) || jdbcConnectionString.equals("")) {
			// if the param is not set, take build the connection string from
			// the template
			jdbcConnectionString = ((JdbcDriver) driver)
					.getConnectionStringTemplate();

			GrtStringHashMap paramValues = dbConn.getParameterValues();
			for (int i = 0; i < paramValues.getKeys().length; i++) {
				String key = paramValues.getKeys()[i];
				String value = paramValues.get(key);
				
				// if this is the MySQL JDBC driver, encode the value
				if (driverClassName.equals("com.mysql.jdbc.Driver"))
					value = URLEncoder.encode(value, "UTF-8");

				jdbcConnectionString = replace(jdbcConnectionString, "%"
						+ key + "%", value);
				
				/*String value = paramValues.get(key).replaceAll("\\\\",
						"\\\\\\\\");

				if (!value.equals("")) {
					// if this is the MySQL JDBC driver, encode the value
					if (driverClassName.equals("com.mysql.jdbc.Driver"))
						value = URLEncoder.encode(value, "UTF-8");

					jdbcConnectionString = jdbcConnectionString.replaceAll("%"
							+ key + "%", value);

				} else
					jdbcConnectionString = Grt.replace(jdbcConnectionString,
							"%" + key + "%", "");*/
			}
		}

		Grt.getInstance().addMsg("Opening connection ... ");
		Grt.getInstance().addMsgDetail("Connection " + jdbcConnectionString);
		Grt.getInstance().flushMessages();

		// get the connection
		java.sql.Connection conn = null;

		// if an explicit username is given, use it
		String explicitUsername = dbConn.getParameterValues().get(
				"explicit_username");
		String explicitPassword = dbConn.getParameterValues().get(
				"explicit_password");
		if (explicitUsername != null && !explicitUsername.equalsIgnoreCase("")) {
			conn = DriverManager.getConnection(jdbcConnectionString,
					explicitUsername, explicitPassword);
		} else
			conn = DriverManager.getConnection(jdbcConnectionString);

		return conn;
	}

	/**
	 * Get version info of the RDBMS
	 * 
	 * This function connects to the target database system and retrieves the
	 * version information
	 * 
	 * @param dbConn
	 *            a Connection
	 * @return returns the version information
	 */

	public static Version getVersion(com.mysql.grt.db.mgmt.Connection dbConn)
			throws Exception {

		// connect to the database
		java.sql.Connection conn = establishConnection(dbConn);

		Grt.getInstance().addMsg("Getting version information ... ");

		Version versionInfo = new Version(null);

		String skipVersionDetection = dbConn.getParameterValues().get(
				"skipVersionDetection");

		if ((skipVersionDetection == null) || !skipVersionDetection.equals("1")) {
			try {
				DatabaseMetaData metaData = conn.getMetaData();

				com.mysql.grt.db.mgmt.Driver driver = dbConn.getDriver();

				if (!JdbcDriver.class.isInstance(driver))
					throw new Exception(
							"The submitted driver is not a JDBC driver.");

				Grt.getInstance().addMsg("Initializing JDBC driver ... ");
				Grt.getInstance().addMsgDetail(
						"Driver class " + driver.getCaption());

				versionInfo.setName(metaData.getDatabaseProductVersion());
				versionInfo.setMajor(metaData.getDatabaseMajorVersion());
				versionInfo.setMinor(metaData.getDatabaseMinorVersion());

				// get the release number
				MessageFormat mf = new MessageFormat(
						"{0}.{0}.{0,number,integer}{1}");
				Object[] objs = mf.parse(metaData.getDatabaseProductVersion(),
						new ParsePosition(0));
				if (objs != null)
					versionInfo
							.setRelease(Integer.parseInt(objs[0].toString()));
			} catch (Throwable t) {
				// ignore exceptions if the driver does not support the version
				// functions and try to recover from getDatabaseProductVersion()
				// we found no other way to prevent the AbstractMethodError

				if (versionInfo.getName() != null
						&& !versionInfo.getName().equalsIgnoreCase("")) {
					Pattern p = Pattern.compile(
							".*?(\\d+)\\.(\\d+)\\.(\\d+).*", Pattern.DOTALL);
					Matcher m = p.matcher(versionInfo.getName());
					if (m.matches()) {
						versionInfo.setMajor(Integer.parseInt(m.group(1)));
						versionInfo.setMinor(Integer.parseInt(m.group(2)));
						versionInfo.setRelease(Integer.parseInt(m.group(3)));
					}
				}
			}
		}

		return versionInfo;
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

		java.sql.Connection conn = establishConnection(dbConn);

		Grt.getInstance().addMsg("Fetching schemata list.");

		GrtStringList schemataList = new GrtStringList();

		ResultSet rset = conn.getMetaData().getSchemas();
		while (rset.next()) {
			String schemaName = rset.getString("TABLE_SCHEM");

			if (schemaName != null)
				schemataList.add(schemaName);
		}
		rset.close();
		conn.close();

		if (schemataList.size() == 0) {
			schemataList.add("DEFAULT");
			GrtHashMap dataBulkTransferParams = (GrtHashMap) Grt.getInstance()
					.getGrtGlobalAsObject("/migration/dataBulkTransferParams");

			dataBulkTransferParams.addObject("excludeSourceSchemaName", "yes");
		}

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

		ReverseEngineeringGeneric revEng = new ReverseEngineeringGeneric();

		java.sql.Connection conn = establishConnection(dbConn);

		Catalog catalog = new Catalog(null);
		catalog.setName("GenericCatalog");

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

				// Get SPs
				revEng.reverseEngineerProcedures(conn, catalog, schema);
			}
		}

		// make sure the Fks use real references instead of
		// text names where possible
		revEng.reverseEngineerUpdateFkReferences(catalog);

		conn.close();

		return catalog;
	}

	protected void buildSimpleDatatypes(java.sql.Connection conn,
			Catalog catalog) throws Exception {

		GrtList simpleDatatypes = new GrtList(catalog);
		DatatypeGroup group = new DatatypeGroup(catalog);
		group.setName("Generic Datatype Group");
		group.setCaption("Generic Datatype Group");
		group
				.setDescription("A generic datatype group holding JDBC datatypes.");

		catalog.getCustomData().addObject("simpleDatatypes", simpleDatatypes);
		catalog.getCustomData().addObject("datatypeGroup", group);

		ResultSet rset = conn.getMetaData().getTypeInfo();
		while (rset.next()) {
			SimpleDatatype simpleType = new SimpleDatatype(catalog);
			simpleType.setGroup(group);

			catalog.getSimpleDatatypes().add(simpleType);

			simpleType.setName(rset.getString("TYPE_NAME"));

			long precision = rset.getLong("PRECISION");
			if (precision >= new Long("4294967296").longValue())
				precision = precision / 1000000 * -1;
			simpleType.setNumericPrecision(new Long(precision).intValue());

			simpleType.setNumericPrecisionRadix(rset.getInt("NUM_PREC_RADIX"));
			simpleType.setNumericScale(rset.getInt("MAXIMUM_SCALE"));

			simpleDatatypes.addObject(simpleType);
		}
		rset.close();
	}

	protected void reverseEngineerTables(java.sql.Connection conn,
			Catalog catalog, Schema schema) throws Exception {

		Grt.getInstance().addMsg("Fetch all tables of given schemata.");

		ResultSet rset = conn.getMetaData().getTables(null, schema.getName(),
				null, new String[] { "TABLE" });
		while (rset.next()) {
			// Create new table
			Table table = new Table(schema);
			schema.getTables().add(table);

			table.setName(rset.getString("TABLE_NAME"));

			reverseEngineerTableColumns(conn, catalog, schema, table);

			reverseEngineerTablePK(conn, catalog, schema, table);

			reverseEngineerTableIndices(conn, catalog, schema, table);

			reverseEngineerTableFKs(conn, catalog, schema, table);
		}
		rset.close();
	}

	protected void reverseEngineerTableColumns(java.sql.Connection conn,
			Catalog catalog, Schema schema, Table table) {

		try {
			Grt.getInstance().addMsg(
					"Fetching column information of table " + table.getName()
							+ ".");

			ResultSet rset = conn.getMetaData().getColumns(null,
					schema.getName(), table.getName(), null);

			boolean hasDefaultValueColumn = false;
			ResultSetMetaData rsmd = rset.getMetaData();
			for (int i = 1; i <= rsmd.getColumnCount(); i++)
				if (rsmd.getColumnName(i).equalsIgnoreCase("COLUMN_DEF"))
					hasDefaultValueColumn = true;

			while (rset.next()) {
				// create new column
				Column column = new Column(table);
				table.getColumns().add(column);

				column.setName(rset.getString("COLUMN_NAME"));
				column.setDatatypeName(rset.getString("TYPE_NAME"));

				// Get Simple Type
				int datatypeIndex = catalog.getSimpleDatatypes()
						.getIndexOfName(column.getDatatypeName());
				if (datatypeIndex > -1)
					column.setSimpleType(catalog.getSimpleDatatypes().get(
							datatypeIndex));

				column.setLength(rset.getInt("COLUMN_SIZE"));
				column.setPrecision(column.getLength());
				column.setScale(rset.getInt("DECIMAL_DIGITS"));

				// make sure precision is greater than scale
				if (column.getPrecision() < column.getScale()) {
					column.setPrecision(16);

					if (column.getPrecision() < column.getScale())
						column.setPrecision(column.getScale() + 1);
				}

				if (rset.getInt("NULLABLE") == java.sql.DatabaseMetaData.columnNullable)
					column.setIsNullable(1);
				else
					column.setIsNullable(0);

				// prevent VARCHAR(0) columns
				if (column.getDatatypeName().equalsIgnoreCase("VARCHAR")
						&& column.getLength() == 0)
					column.setLength(255);

				if (hasDefaultValueColumn)
					column.setDefaultValue(rset.getString("COLUMN_DEF"));
				else
					column.setDefaultValueIsNull(1);
			}

			rset.close();
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	protected void reverseEngineerTablePK(java.sql.Connection conn,
			Catalog catalog, Schema schema, Table table) {

		// String sql;

		try {
			Grt.getInstance().addMsg("Fetching primary key information.");

			ResultSet rset = conn.getMetaData().getPrimaryKeys(null, null,
					table.getName());

			Index primaryKey = null;
			String primaryKeyName = null;

			while (rset.next()) {
				if (primaryKey == null) {
					primaryKey = new Index(table);
					primaryKey.setName("PRIMARY");

					primaryKey.setIsPrimary(1);

					// add PK to indices
					table.getIndices().add(primaryKey);

					table.setPrimaryKey(primaryKey);
				}

				primaryKeyName = rset.getString("PK_NAME");

				IndexColumn indexColumn = new IndexColumn(primaryKey);
				indexColumn.setName(rset.getString("COLUMN_NAME"));
				indexColumn.setColumnLength(0);

				// find reference table column
				for (int j = 0; j < table.getColumns().size(); j++) {
					Column column = (Column) (table.getColumns().get(j));

					if (column.getName().compareToIgnoreCase(
							indexColumn.getName()) == 0) {
						indexColumn.setReferedColumn(column);

						// text and blob have to have a index column length
						if ((indexColumn.getColumnLength() == 0)
								&& (column.getSimpleType().getGroup().getName()
										.equals("text") || column
										.getSimpleType().getGroup().getName()
										.equals("blob")))
							indexColumn.setColumnLength(10);

						break;
					}
				}

				primaryKey.getColumns().add(indexColumn);
			}

			// remove primary key from list of indices
			if (primaryKeyName != null) {
				for (int i = 0; i < table.getIndices().size(); i++) {
					String indexName = table.getIndices().get(i).getName();

					if (primaryKeyName.compareToIgnoreCase(indexName) == 0) {
						table.getIndices().remove(i);
						break;
					}
				}
			}
		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	protected void reverseEngineerTableIndices(java.sql.Connection conn,
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

				if (!indexName.equalsIgnoreCase(newIndexName)) {
					if (index != null)
						table.getIndices().add(index);

					indexName = newIndexName;

					index = new Index(table);
					index.setName(indexName);

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

						// text and blob have to have a index column length
						if ((indexColumn.getColumnLength() == 0)
								&& (column.getSimpleType() != null)
								&& (column.getSimpleType().getGroup() != null)
								&& (column.getSimpleType().getGroup().getName()
										.equals("text") || column
										.getSimpleType().getGroup().getName()
										.equals("blob")))
							indexColumn.setColumnLength(10);

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

	protected void reverseEngineerTableFKs(java.sql.Connection conn,
			Catalog catalog, Schema schema, Table table) {

		try {
			Grt.getInstance().addMsg("Fetching FK information.");

			ResultSet rset = conn.getMetaData().getImportedKeys(null, null,
					table.getName());

			String fkName = "";
			ForeignKey foreignKey = null;

			while (rset.next()) {
				String newFkName = rset.getString("FK_NAME");

				if (fkName.compareToIgnoreCase(newFkName) != 0) {
					if (foreignKey != null)
						table.getForeignKeys().add(foreignKey);

					fkName = newFkName;
					foreignKey = new ForeignKey(table);
					foreignKey.setName(fkName);

					foreignKey.setDeferability(rset.getInt("DEFERRABILITY"));

					switch (rset.getShort("DELETE_RULE")) {
					case DatabaseMetaData.importedKeyCascade:
						foreignKey.setDeleteRule("CASCADE");
						break;
					case DatabaseMetaData.importedKeyRestrict:
						foreignKey.setDeleteRule("RESTRICT");
						break;
					case DatabaseMetaData.importedKeySetNull:
						foreignKey.setDeleteRule("SET NULL");
						break;
					default:
						foreignKey.setDeleteRule("NO ACTION");
						break;
					}

					switch (rset.getShort("UPDATE_RULE")) {
					case DatabaseMetaData.importedKeyCascade:
						foreignKey.setUpdateRule("CASCADE");
						break;
					case DatabaseMetaData.importedKeyRestrict:
						foreignKey.setUpdateRule("RESTRICT");
						break;
					case DatabaseMetaData.importedKeySetNull:
						foreignKey.setUpdateRule("SET NULL");
						break;
					default:
						foreignKey.setUpdateRule("NO ACTION");
						break;
					}

					String fkSchemaName = rset.getString("PKTABLE_SCHEM");

					if (rset.wasNull())
						foreignKey.setReferedTableSchemaName(schema.getName());
					else
						foreignKey.setReferedTableSchemaName(fkSchemaName);

					foreignKey.setReferedTableName(rset
							.getString("PKTABLE_NAME"));
				}

				foreignKey.getReferedColumnNames().add(
						rset.getString("PKCOLUMN_NAME"));

				// find reference table column
				String colName = rset.getString("FKCOLUMN_NAME");
				boolean found = false;
				for (int j = 0; j < table.getColumns().size(); j++) {
					Column column = (Column) (table.getColumns().get(j));

					if (column.getName().compareToIgnoreCase(colName) == 0) {
						foreignKey.getColumns().add(column);
						found = true;
						break;
					}
				}

				if (!found)
					Grt.getInstance().addErr(
							"Column " + colName + " not found in table "
									+ table.getName() + ".");
			}

			if (foreignKey != null)
				table.getForeignKeys().add(foreignKey);

		} catch (Exception e) {
			Grt.getInstance().addErr(e.getMessage());
		}
	}

	protected void reverseEngineerUpdateFkReferences(Catalog catalog) {
		SchemaList schemata = catalog.getSchemata();

		// do for all schemata
		for (int i = 0; i < schemata.size(); i++) {
			Schema schema = schemata.get(i);
			TableList tables = schema.getTables();

			// do for all tables
			for (int j = 0; j < tables.size(); j++) {
				ForeignKeyList fks = tables.get(j).getForeignKeys();

				// do for all foreign keys
				for (int k = 0; k < fks.size(); k++) {
					ForeignKey fk = fks.get(k);
					String refSchemaName = fk.getReferedTableSchemaName();
					Schema refSchema;

					// get the refered schema
					if ((refSchemaName != null) && (!refSchemaName.equals(""))
							&& !refSchemaName.equals(schema.getName()))
						refSchema = schemata.getItemByName(refSchemaName);
					else
						refSchema = schema;

					if (refSchema != null) {
						String refTableName = fk.getReferedTableName();

						// get the refered table
						Table refTable = refSchema.getTables().getItemByName(
								refTableName);
						if (refTable != null) {
							GrtStringList refColNames = fk
									.getReferedColumnNames();
							ColumnList refTableCols = refTable.getColumns();

							ColumnList refCols = fk.getReferedColumns();
							if (refCols == null) {
								fk.setColumns(new ColumnList());
								refCols = fk.getReferedColumns();
							}

							// set the table reference in the fk
							fk.setReferedTable(refTable);

							for (int l = 0; l < refColNames.size(); l++) {
								Column refCol = refTableCols
										.getItemByName(refColNames.get(l));

								if (refCol != null) {
									// add column reference to the fk column
									// list
									fk.getReferedColumns().add(refCol);
								}
							}
						}
					}
				}
			}
		}
	}

	protected void reverseEngineerViews(java.sql.Connection conn,
			Catalog catalog, Schema schema) throws Exception {

		Grt.getInstance().addMsg("Fetch all views of the given schemata.");
	}

	protected void reverseEngineerProcedures(java.sql.Connection conn,
			Catalog catalog, Schema schema) throws Exception {

		Grt.getInstance().addMsg(
				"Fetch all stored procedures of the given schemata.");
	}
}
