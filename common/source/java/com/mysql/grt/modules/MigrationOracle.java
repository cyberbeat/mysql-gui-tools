package com.mysql.grt.modules;

import com.mysql.grt.*;
import com.mysql.grt.db.Catalog;
import com.mysql.grt.db.migration.*;
import com.mysql.grt.db.oracle.*;

/**
 * GRT Migration Class for Oracle 8i/9i
 * 
 * @author Mike
 * @version 1.0, 01/11/05
 * 
 */
public class MigrationOracle extends MigrationGeneric {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the info about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(MigrationOracle.class, "MigrationGeneric");
	}

	/**
	 * Collects information about the migration methods to let the user choose
	 * which one to take
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	public static MethodList migrationMethods() {
		MethodList methods = MigrationGeneric.migrationMethods();

		// add methods to method list
		MigrationOracle mig = new MigrationOracle();
		methods.add(mig.getMigrateSchemaToMysqlInfo());
		methods.add(mig.getMigrateTableToMysqlInfo());
		methods.add(mig.getMigrateColumnToMysqlInfo());
		methods.add(mig.getMigrateViewToMysqlInfo());
		methods.add(mig.getMigrateRoutineToMysqlInfo());

		return methods;
	}

	/**
	 * Performs a migration based on a Migration object
	 * 
	 * @param migObj
	 *            migration object to migrate
	 * @param targetPackageName
	 *            name of the package that should be used to generate the target
	 *            objects, e.g. db.mysql
	 */
	public static void migrate(com.mysql.grt.db.migration.Migration migObj,
			com.mysql.grt.db.mgmt.Rdbms targetRdbms,
			com.mysql.grt.db.Version version) throws Exception {

		Grt.getInstance().addMsg("Starting Oracle migration...");

		new MigrationOracle().migrateCatalog(migObj, targetRdbms, version);
	}

	/**
	 * Performs a data transfer from the given source catalog to the target
	 * catalog
	 * 
	 * @param sourceJdbcDriver
	 *            class name of the source jdbc driver
	 * @param sourceJdbcConnectionString
	 *            jdbc connection string to the source database
	 * @param sourceCatalog
	 *            the source catalog
	 * @param targetJdbcDriver
	 *            class name of the target jdbc driver
	 * @param targetJdbcConnectionString
	 *            jdbc connection string to the target database
	 * @param targetCatalog
	 *            the target catalog
	 * @param params
	 *            parameters that define how the migration is performed
	 */
	public static void dataBulkTransfer(
			com.mysql.grt.db.mgmt.Connection sourceDbConn,
			Catalog sourceCatalog,
			com.mysql.grt.db.mgmt.Connection targetDbConn,
			com.mysql.grt.db.Catalog targetCatalog, GrtStringHashMap params,
			com.mysql.grt.base.ObjectLogList logList) throws Exception {

		new MigrationOracle().doDataBulkTransfer(sourceDbConn, sourceCatalog,
				targetDbConn, targetCatalog, params, logList);
	}

	/**
	 * migrates the name of an identifier
	 * 
	 * @param name
	 *            the source name of the identifier
	 * @return the migrated identifier name
	 */
	protected String migrateIdentifier(String name) {
		return name.toLowerCase();
	}

	/**
	 * migrates the sourceSchema and stores the targetCatalog in the global
	 * migration object
	 * 
	 * @param migObj
	 *            migration object to migrate
	 * @param targetPackageName
	 *            name of the package that should be used to generate the target
	 *            objects, e.g. db.mysql
	 * @param sourceSchema
	 *            the source schema that should be migrated
	 */
	protected com.mysql.grt.db.mysql.Schema migrateSchemaToMysql(
			com.mysql.grt.db.migration.Migration migObj, Schema sourceSchema,
			GrtStringHashMap migrationParams, GrtObject parent) {

		// call super migrate function to do basic migration
		com.mysql.grt.db.mysql.Schema targetSchema = super
				.migrateSchemaToMysql(migObj, sourceSchema, migrationParams,
						parent);

		// migrate sequences
		for (int i = 0; i < sourceSchema.getSequences().size(); i++) {
			Sequence sourceSequence = (Sequence) sourceSchema.getSequences()
					.get(i);

			migUtils.migrateObject(this, migObj, sourceSequence, targetSchema);
		}

		return targetSchema;
	}

	/**
	 * Migrates an Oracle table to a MySQL table
	 * 
	 * @param sourceTable
	 *            the object to migrate
	 * @param migrationParams
	 *            parameters used to define the target object
	 * 
	 * @return returns MySQL table object
	 */
	protected com.mysql.grt.db.mysql.Table migrateTableToMysql(
			com.mysql.grt.db.migration.Migration migObj, Table sourceTable,
			GrtStringHashMap migrationParams, GrtObject parent) {

		// call migration method from the super class
		com.mysql.grt.db.mysql.Table targetTable;
		targetTable = super.migrateTableToMysql(migObj, sourceTable,
				migrationParams, parent);

		// do oracle specific things

		// return new created, migrated object
		return targetTable;
	}

	/**
	 * Migrates a column to a MySQL column
	 * 
	 * @param migObj
	 *            the migration object
	 * @param sourceColumn
	 *            the object to migrate
	 * @param migrationParams
	 *            parameters used to define the target object
	 * @param parent
	 *            parent object of the migrated object
	 * 
	 * @return returns a MySQL object
	 */
	protected com.mysql.grt.db.mysql.Column migrateColumnToMysql(
			com.mysql.grt.db.migration.Migration migObj, Column sourceColumn,
			GrtStringHashMap migrationParams, GrtObject parent) {

		// create target table
		com.mysql.grt.db.mysql.Column targetColumn;
		targetColumn = new com.mysql.grt.db.mysql.Column(parent);

		// log creation of target object
		migUtils.addMigrationLogEntry(migObj, sourceColumn, targetColumn);

		// do migration
		targetColumn.setName(migUtils.getTargetName(migrationParams,
				migrateIdentifier(sourceColumn.getName())));
		targetColumn.setOldName(sourceColumn.getName());
		targetColumn.setDefaultValue(sourceColumn.getDefaultValue());
		targetColumn.setIsNullable(sourceColumn.getIsNullable());
		targetColumn.setPrecision(sourceColumn.getPrecision());
		targetColumn.setScale(sourceColumn.getScale());
		targetColumn.setLength(sourceColumn.getLength());

		// migrate datatype
		com.mysql.grt.db.SimpleDatatypeList simpleDatatypes = migObj
				.getTargetCatalog().getSimpleDatatypes();

		String sourceDatatypeName = sourceColumn.getDatatypeName();

		if (!migrateColumnParamsToMySql(targetColumn, migrationParams)) {
			// character types
			if (sourceDatatypeName.equals("VARCHAR2")
					|| sourceDatatypeName.equals("NVARCHAR2")) {
				GrtStringList columnFlags = new GrtStringList();
				columnFlags.add("BINARY");
				targetColumn.setFlags(columnFlags);
				
				if (sourceColumn.getLength() < 256) {
					// normal varchar up to 255 chars
					targetColumn.setDatatypeName("VARCHAR");
				} else if (sourceColumn.getLength() < 65536) {
					// MySQL 5 can deal with VARCHAR holding up to 65535
					// characters
					if (migObj.getTargetCatalog().getVersion().getMajor() >= 5)
						targetColumn.setDatatypeName("VARCHAR");
					// for older versions use medium text up to 65535
					else
						targetColumn.setDatatypeName("MEDIUMTEXT");
				} else {
					// long text
					targetColumn.setDatatypeName("LONGTEXT");
				}
			}
			// character types with fixed length
			else if (sourceDatatypeName.equals("CHAR")
					|| sourceDatatypeName.equals("NCHAR")) {				
				GrtStringList columnFlags = new GrtStringList();
				columnFlags.add("BINARY");
				targetColumn.setFlags(columnFlags);
				
				// fixed length character types
				if (sourceColumn.getLength() < 256) {
					// normal varchar up to 255 chars
					targetColumn.setDatatypeName("CHAR");
				} else if (sourceColumn.getLength() < 65536) {
					// MySQL 5 can deal with VARCHAR holding up to 65535
					// characters and in InnoDB CHAR == VARCHAR
					if (migObj.getTargetCatalog().getVersion().getMajor() >= 5)
						targetColumn.setDatatypeName("VARCHAR");
					// for older versions use medium text up to 65535
					else
						targetColumn.setDatatypeName("MEDIUMTEXT");
				} else {
					// long text
					targetColumn.setDatatypeName("LONGTEXT");
				}
			}
			// character types with unknown length
			else if (sourceDatatypeName.equals("CLOB")
					|| sourceDatatypeName.equals("LONG")) {
				GrtStringList columnFlags = new GrtStringList();
				columnFlags.add("BINARY");
				targetColumn.setFlags(columnFlags);
				
				targetColumn.setDatatypeName("LONGTEXT");
			}
			// binary types
			else if (sourceDatatypeName.equals("RAW")) {
				targetColumn.setDatatypeName("MEDIUMBLOB");
			}
			// binary types
			else if (sourceDatatypeName.equals("LONG RAW")
					|| sourceDatatypeName.equals("BLOB")) {
				targetColumn.setDatatypeName("LONGBLOB");
			}
			// numeric types
			else if (sourceDatatypeName.equals("NUMBER")
					|| sourceDatatypeName.equals("DECIMAL")) {
				if (sourceColumn.getScale() == 0) {
					if (targetColumn.getPrecision() < 10) {
						targetColumn.setDatatypeName("INT");
					} else {
						if (targetColumn.getPrecision() < 19)
							targetColumn.setDatatypeName("BIGINT");
						else
							targetColumn.setDatatypeName("DECIMAL");
					}
				} else {
					targetColumn.setDatatypeName("DECIMAL");
				}
				
				// make sure Precision and Scale are set correctly
				if (targetColumn.getPrecision() > 65) {
					targetColumn.setPrecision(65);
				}				
				if (targetColumn.getScale() > 30) {
					targetColumn.setScale(30);
					migUtils.addMigrationLogEntry(migObj, sourceColumn,
							targetColumn, "The precision of this column has been set to the maximum allowed value (30). "
							+ "This might cause loss of data.",
							MigrationUtils.logWarning);
				}
				if (targetColumn.getScale() > targetColumn.getPrecision()) {
					targetColumn.setScale(targetColumn.getPrecision() - 1);
				}
			} else if (sourceDatatypeName.equals("REAL")
					|| sourceDatatypeName.equals("DOUBLE PRECISION")) {
				targetColumn.setDatatypeName("DECIMAL");
				targetColumn.setScale(30);
			} else if (sourceDatatypeName.equals("FLOAT")) {
				targetColumn.setDatatypeName("DOUBLE");
				
				targetColumn.setPrecision(-1);
				targetColumn.setScale(-1);
			// datetime types
			} else if (sourceDatatypeName.equals("DATE")) {
				targetColumn.setDatatypeName("DATETIME");

				if ((sourceColumn.getDefaultValue() != null)
						&& sourceColumn.getDefaultValue().equalsIgnoreCase(
								"sysdate"))
					targetColumn.setDefaultValue("");
			}
			// timestamp types
			else if (sourceDatatypeName.indexOf("TIMESTAMP") > -1) {
				// timestamp types
				targetColumn.setDatatypeName("DATETIME");
			}
			// not covered yet
			else {
				targetColumn.setDatatypeName("VARCHAR");
				targetColumn.setLength(255);

				migUtils.addMigrationLogEntry(migObj, sourceColumn,
						targetColumn, "The datatype "
								+ sourceColumn.getDatatypeName()
								+ " cannot be migrated.",
						MigrationUtils.logError);
			}
		}

		// lookup the simple datatype and set it in the column
		int simpleDatatypeIndex = simpleDatatypes.getIndexOfName(targetColumn
				.getDatatypeName());
		if (simpleDatatypeIndex > -1)
			targetColumn
					.setSimpleType(simpleDatatypes.get(simpleDatatypeIndex));

		// make sure N* are utf8
		if (sourceDatatypeName.equals("NVARCHAR2")
				|| sourceDatatypeName.equals("NCHAR")
				|| sourceDatatypeName.equals("NCLOB")) {
			targetColumn.setCharacterSetName("utf8");
			targetColumn.setCollationName("utf8_bin");
		}

		// return new created, migrated object
		return targetColumn;
	}

	/**
	 * Migrates an Oracle view to a MySQL view
	 * 
	 * @param sourceView
	 *            the object to migrate
	 * @param migrationParams
	 *            parameters used to define the target object
	 * 
	 * @return returns MySQL view object
	 */
	protected com.mysql.grt.db.mysql.View migrateViewToMysql(
			com.mysql.grt.db.migration.Migration migObj, View sourceView,
			GrtStringHashMap migrationParams, GrtObject parent) {

		// create target view
		com.mysql.grt.db.mysql.View targetView = super.migrateViewToMysql(
				migObj, sourceView, migrationParams, parent);

		// return new created, migrated object
		return targetView;
	}

	/**
	 * Migrates an Oracle Routine to a MySQL Routine
	 * 
	 * @param sourceView
	 *            the object to migrate
	 * @param migrationParams
	 *            parameters used to define the target object
	 * 
	 * @return returns MySQL view object
	 */
	protected com.mysql.grt.db.mysql.Routine migrateRoutineToMysql(
			com.mysql.grt.db.migration.Migration migObj, Routine sourceProc,
			GrtStringHashMap migrationParams, GrtObject parent) {

		// create target Routine
		com.mysql.grt.db.mysql.Routine targetProc = super
				.migrateRoutineToMysql(migObj, sourceProc, migrationParams,
						parent);

		// return new created, migrated object
		return targetProc;
	}

	/**
	 * Generates information about the schema to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateSchemaToMysqlInfo() {

		// create method description
		Method method = new Method(null);
		method.setName("migrateSchemaToMysql");
		method.setModuleName("MigrationOracle");
		method.setCaption("Oracle Default");
		method.setDesc("Default method to migrate an Oracle schema to MySQL.");
		method.setSourceStructName("db.oracle.Schema");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateSchemaToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates information about the Oracle Table to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateTableToMysqlInfo() {

		// create migrateOracleTable method
		Method method = new Method(null);
		method.setName("migrateTableToMysql");
		method.setModuleName("MigrationOracle");
		method.setCaption("Oracle Default");
		method.setDesc("Default method to migrate an Oracle table to MySQL.");
		method.setSourceStructName("db.oracle.Table");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateTableToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates information about the Column to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateColumnToMysqlInfo() {

		// create method description
		Method method = new Method(null);
		method.setName("migrateColumnToMysql");
		method.setModuleName("MigrationOracle");
		method.setCaption("Oracle Default");
		method.setDesc("Default method to migrate a Oracle column to MySQL.");
		method.setSourceStructName("db.oracle.Column");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateColumnToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates information about the View to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateViewToMysqlInfo() {

		// create migrateOracleTable method
		Method method = new Method(null);
		method.setName("migrateViewToMysql");
		method.setModuleName("MigrationOracle");
		method.setCaption("Oracle Default");
		method.setDesc("Default method to migrate an Oracle view to MySQL.");
		method.setSourceStructName("db.oracle.View");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateViewToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates information about the View to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateRoutineToMysqlInfo() {

		// create migrateOracleTable method
		Method method = new Method(null);
		method.setName("migrateRoutineToMysql");
		method.setModuleName("MigrationOracle");
		method.setCaption("Oracle Default");
		method.setDesc("Default method to migrate an "
				+ "Oracle routine to MySQL.");
		method.setSourceStructName("db.oracle.Routine");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateRoutineToMysqlInfoParameters(method);

		return method;
	}
}