package com.mysql.grt.modules;

import com.mysql.grt.Grt;
import com.mysql.grt.GrtObject;
import com.mysql.grt.GrtStringHashMap;
import com.mysql.grt.db.mysql.*;
import com.mysql.grt.db.migration.Method;
import com.mysql.grt.db.migration.MethodList;

/**
 * GRT Migration Class for MySQL
 * 
 * @author Mike
 * @version 1.0, 06/17/05
 * 
 */
public class MigrationMysql extends MigrationGeneric {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(MigrationMysql.class, "MigrationGeneric");
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
		MigrationMysql mig = new MigrationMysql();
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

		Grt.getInstance().addMsg("Starting MySQL migration...");

		new MigrationMysql().migrateCatalog(migObj, targetRdbms, version);
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

		return targetSchema;
	}

	/**
	 * Migrates an MySQL table to a MySQL table
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

		// do mysql specific things
		String engine = migrationParams.get("engine");
		if ((engine == null) || engine.equalsIgnoreCase(""))
			targetTable.setTableEngine(sourceTable.getTableEngine());

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
		targetColumn.setDatatypeExplicitParams(sourceColumn
				.getDatatypeExplicitParams());

		// migrate datatype
		com.mysql.grt.db.SimpleDatatypeList simpleDatatypes = migObj
				.getTargetCatalog().getSimpleDatatypes();

		String sourceDatatypeName = sourceColumn.getDatatypeName();

		if (!migrateColumnParamsToMySql(targetColumn, migrationParams)) {
			targetColumn.setDatatypeName(sourceDatatypeName);
		}

		targetColumn.setSimpleType(simpleDatatypes.get(simpleDatatypes
				.getIndexOfName(sourceDatatypeName)));

		targetColumn.setCharacterSetName(sourceColumn.getCharacterSetName());
		targetColumn.setCollationName(sourceColumn.getCollationName());
		
		targetColumn.setAutoIncrement(sourceColumn.getAutoIncrement());
		
		// migrate flags
		for (int i = 0; i < sourceColumn.getFlags().size(); i++) {
			targetColumn.getFlags().add(sourceColumn.getFlags().get(i));
		}

		// return new created, migrated object
		return targetColumn;
	}

	/**
	 * Migrates an MySQL view to a MySQL view
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
		com.mysql.grt.db.mysql.View targetView;
		targetView = new com.mysql.grt.db.mysql.View(parent);

		// log creation of target object
		migUtils.addMigrationLogEntry(migObj, sourceView, targetView);

		// do migration
		targetView.setName(migUtils.getTargetName(migrationParams,
				migrateIdentifier(sourceView.getName())));
		targetView.setOldName(sourceView.getName());

		targetView.setWithCheckCondition(sourceView.getWithCheckCondition());

		// migrate SQL
		String query = sourceView.getQueryExpression().trim();

		// detect WITH CHECK OPTION in sql and remove it
		if (query.toUpperCase().endsWith("WITH CHECK OPTION"))
			query = query.substring(0, query.length() - 17).trim();

		targetView.setQueryExpression(query);

		targetView.setCommentedOut(0);

		// return new created, migrated object
		return targetView;
	}

	/**
	 * Migrates an MySQL Routine to a MySQL Routine
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

		com.mysql.grt.db.mysql.Routine targetProc;
		targetProc = new com.mysql.grt.db.mysql.Routine(parent);

		// log creation of target object
		migUtils.addMigrationLogEntry(migObj, sourceProc, sourceProc);

		// do migration
		targetProc.setName(migUtils.getTargetName(migrationParams,
				migrateIdentifier(sourceProc.getName())));
		targetProc.setOldName(sourceProc.getName());
		targetProc.setRoutineType(sourceProc.getRoutineType());

		// migrate SQL
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
		method.setModuleName("MigrationMysql");
		method.setCaption("MySQL Default");
		method.setDesc("Default method to migrate an MySQL schema to MySQL.");
		method.setSourceStructName("db.mysql.Schema");
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
		method.setModuleName("MigrationMysql");
		method.setCaption("MySQL Default");
		method.setDesc("Default method to migrate an MySQL table to MySQL.");
		method.setSourceStructName("db.mysql.Table");
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
		method.setModuleName("MigrationMysql");
		method.setCaption("MySQL Default");
		method.setDesc("Default method to migrate a MySQL column to MySQL.");
		method.setSourceStructName("db.mysql.Column");
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
		method.setModuleName("MigrationMysql");
		method.setCaption("MySQL Default");
		method.setDesc("Default method to migrate a MySQL view to MySQL.");
		method.setSourceStructName("db.mysql.View");
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
		method.setModuleName("MigrationMysql");
		method.setCaption("MySQL Default");
		method.setDesc("Default method to migrate an "
				+ "MySQL routine to MySQL.");
		method.setSourceStructName("db.mysql.Routine");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateRoutineToMysqlInfoParameters(method);

		return method;
	}

}