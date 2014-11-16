package com.mysql.grt.modules;

import com.mysql.grt.*;
import com.mysql.grt.db.migration.Method;
import com.mysql.grt.db.migration.MethodList;
import com.mysql.grt.db.sybase.*;

/**
 * GRT Migration Class for Sybase 12.x
 * 
 * @author Mike
 * @version 1.0, 06/23/06
 * 
 */

public class MigrationSybase extends MigrationGeneric {
	
	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(MigrationSybase.class, "MigrationGeneric");
	}
	
	/**
	 * Collects information about the migration methods to let the user choose
	 * which one to take
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	public static MethodList migrationMethods() {
		MethodList methods = MigrationGeneric.migrationMethods();

		// add methods to methodlist
		MigrationSybase mig = new MigrationSybase();
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

		Grt.getInstance().addMsg("Starting Sybase migration...");

		new MigrationSybase().migrateCatalog(migObj, targetRdbms, version);
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

		new MigrationSybase().doDataBulkTransfer(sourceDbConn, sourceCatalog,
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
		return name;
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

		Grt.getInstance().addMsg(
				"Migrating schema " + sourceSchema.getName() + " ...");
		Grt.getInstance().flushMessages();

		// call super migrate function to do basic migration
		com.mysql.grt.db.mysql.Schema targetSchema = super
				.migrateSchemaToMysql(migObj, sourceSchema, migrationParams,
						parent);

		// change schema name to catalog_schema
		targetSchema.setName(sourceSchema.getOwner().getName() + "_"
				+ sourceSchema.getName());

		return targetSchema;
	}
	
	/**
	 * Migrates a source table to a MySQL table
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

		// do sybase specific things

		// return new created, migrated object
		return targetTable;
	}
	
	/**
	 * Migrates a column to a MySQL column
	 * 
	 * @param sourceColumn
	 *            the object to migrate
	 * @param migrationParams
	 *            parameters used to define the target object
	 * @param parent
	 *            parent object of the migrated object
	 * 
	 * @return returns MySQL table object
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
			if (sourceDatatypeName.equalsIgnoreCase("VARCHAR")
					|| sourceDatatypeName.equalsIgnoreCase("NVARCHAR")
					|| sourceDatatypeName.equalsIgnoreCase("UNIVARCHAR")) {
				// text types
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
			} else if (sourceDatatypeName.equalsIgnoreCase("TEXT")
					|| sourceDatatypeName.equalsIgnoreCase("NTEXT")) {
				targetColumn.setDatatypeName("LONGTEXT");
			} else if (sourceDatatypeName.equalsIgnoreCase("CHAR")
					|| sourceDatatypeName.equalsIgnoreCase("NCHAR")
					|| sourceDatatypeName.equalsIgnoreCase("UNICHAR")) {
				// fixed length character types
				if (sourceColumn.getLength() < 256) {
					// normal varchar up to 255 chars
					targetColumn.setDatatypeName("CHAR");
				} else {
					// long text
					targetColumn.setDatatypeName("LONGTEXT");
				}
			}
			// binary types
			else if (sourceDatatypeName.equalsIgnoreCase("IMAGE")
					|| sourceDatatypeName.equalsIgnoreCase("BINARY")
					|| sourceDatatypeName.equalsIgnoreCase("VARBINARY")) {
				if (sourceColumn.getLength() < 256) {
					if (sourceDatatypeName.equalsIgnoreCase("IMAGE")) {
						// tiny blob up to 255 byte
						targetColumn.setDatatypeName("TINYBLOB");
					} else if (sourceDatatypeName.equalsIgnoreCase("BINARY")) {
						// tiny blob up to 255 byte
						targetColumn.setDatatypeName("BINARY");
					} else if (sourceDatatypeName.equalsIgnoreCase("VARBINARY")) {
						// tiny blob up to 255 byte
						targetColumn.setDatatypeName("VARBINARY");
					}
				} else if (sourceColumn.getLength() < 65536) {
					// medium blob up to 65535 byte
					targetColumn.setDatatypeName("MEDIUMBLOB");
				} else {
					// long blob
					targetColumn.setDatatypeName("LONGBLOB");
				}
			}
			// numeric types
			else if (sourceDatatypeName.equalsIgnoreCase("DECIMAL")
					|| sourceDatatypeName.equalsIgnoreCase("NUMERIC")) {
				targetColumn.setDatatypeName("DECIMAL");
			} else if (sourceDatatypeName.equalsIgnoreCase("MONEY")) {
				targetColumn.setDatatypeName("DECIMAL");
				targetColumn.setPrecision(19);
				targetColumn.setScale(4);
			} else if (sourceDatatypeName.equalsIgnoreCase("SMALLMONEY")) {
				targetColumn.setDatatypeName("DECIMAL");
				targetColumn.setPrecision(10);
				targetColumn.setScale(4);
			} else if (sourceDatatypeName.equalsIgnoreCase("DOUBLE PRECISION")) {
				targetColumn.setDatatypeName("DOUBLE");
				targetColumn.setScale(-1);
			} else if (sourceDatatypeName.equalsIgnoreCase("FLOAT")) {
				targetColumn.setDatatypeName("FLOAT");
				targetColumn.setScale(-1);
			} else if (sourceDatatypeName.equalsIgnoreCase("REAL")) {
				targetColumn.setDatatypeName("FLOAT");
				targetColumn.setScale(-1);
			}
			// datetime types
			else if (sourceDatatypeName.equalsIgnoreCase("DATETIME")
					|| sourceDatatypeName.equalsIgnoreCase("SMALLDATETIME")) {
				targetColumn.setDatatypeName("DATETIME");
			}
			// timestamp types
			else if (sourceDatatypeName.equalsIgnoreCase("TIMESTAMP")) {
				targetColumn.setDatatypeName("TIMESTAMP");
			}
			// integer types
			else if (sourceDatatypeName.equalsIgnoreCase("BIGINT")) {
				targetColumn.setDatatypeName("BIGINT");
			} else if (sourceDatatypeName.equalsIgnoreCase("INT")) {
				targetColumn.setDatatypeName("INT");
			} else if (sourceDatatypeName.equalsIgnoreCase("SMALLINT")) {
				targetColumn.setDatatypeName("SMALLINT");
			} else if (sourceDatatypeName.equalsIgnoreCase("TINYINT")
					|| sourceDatatypeName.equalsIgnoreCase("BIT")) {
				targetColumn.setDatatypeName("TINYINT");
				targetColumn.getFlags().add("UNSIGNED");
			} else if (sourceDatatypeName.equalsIgnoreCase("UNIQUEIDENTIFIER")) {
				targetColumn.setDatatypeName("VARCHAR");
				targetColumn.setLength(64);
			}
			// not covered yet
			else {
				migUtils.addMigrationLogEntry(migObj, sourceColumn,
						targetColumn, "The datatype "
								+ sourceColumn.getDatatypeName()
								+ " cannot be migrated.",
						MigrationUtils.logError);
			}
		}

		// lookup the simple value and set it in the column
		int simpleDatatypeIndex = simpleDatatypes.getIndexOfName(targetColumn
				.getDatatypeName());
		if (simpleDatatypeIndex > -1)
			targetColumn
					.setSimpleType(simpleDatatypes.get(simpleDatatypeIndex));

		// make sure N* are utf8
		if (sourceDatatypeName.equalsIgnoreCase("NVARCHAR")
				|| sourceDatatypeName.equalsIgnoreCase("NCHAR")
				|| sourceDatatypeName.equalsIgnoreCase("NTEXT")) {
			targetColumn.setCharacterSetName("utf8");
			targetColumn.setCollationName("utf8_general_ci");
		}

		// AutoIncrement, only for INT datatypes
		String datatypeName = targetColumn.getDatatypeName();
		if (datatypeName.equalsIgnoreCase("INT")
				|| datatypeName.equalsIgnoreCase("INTEGER")
				|| datatypeName.equalsIgnoreCase("TINYINT")
				|| datatypeName.equalsIgnoreCase("SMALLINT")
				|| datatypeName.equalsIgnoreCase("BIGINT"))
			targetColumn.setAutoIncrement(sourceColumn.getIdentity());

		// return new created, migrated object
		return targetColumn;
	}

	/**
	 * Migrates a source view to a MySQL view
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
	 * Migrates a source Routine to a MySQL Routine
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

		// copy Routine code 1:1
		targetProc.setRoutineCode(sourceProc.getRoutineCode());

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
		method.setModuleName("MigrationSybase");
		method.setCaption("Sybase Default");
		method.setDesc("Default method to migrate an Sybase schema to MySQL.");
		method.setSourceStructName("db.sybase.Schema");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateSchemaToMysqlInfoParameters(method);

		return method;
	}
	
	/**
	 * Generates information about the Table to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateTableToMysqlInfo() {

		// create method description
		Method method = new Method(null);
		method.setName("migrateTableToMysql");
		method.setModuleName("MigrationSybase");
		method.setCaption("Sybase Default");
		method.setDesc("Default method to migrate a Sybase table to MySQL.");
		method.setSourceStructName("db.sybase.Table");
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
		method.setModuleName("MigrationSybase");
		method.setCaption("Sybase Default");
		method.setDesc("Default method to migrate a Sybase column to MySQL.");
		method.setSourceStructName("db.sybase.Column");
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
		method.setModuleName("MigrationSybase");
		method.setCaption("Sybase Default");
		method.setDesc("Default method to migrate an Sybase view to MySQL.");
		method.setSourceStructName("db.sybase.View");
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
		method.setModuleName("MigrationSybase");
		method.setCaption("Sybase Default");
		method.setDesc("Default method to migrate an "
				+ "Sybase routine to MySQL.");
		method.setSourceStructName("db.sybase.Routine");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateRoutineToMysqlInfoParameters(method);

		return method;
	}
}
