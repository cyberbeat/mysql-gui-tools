package com.mysql.grt.modules;

import com.mysql.grt.*;
import com.mysql.grt.db.migration.*;
import com.mysql.grt.db.*;

public class MigrationAccess extends MigrationGeneric {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(MigrationAccess.class, "Migration");
	}

	/**
	 * Collects information about migration the methods to let the user choose
	 * which one to take
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	public static MethodList migrationMethods() {
		MethodList methods = MigrationGeneric.migrationMethods();

		// add methods to methodlist
		MigrationAccess mig = new MigrationAccess();
		methods.add(mig.getMigrateColumnToMysqlInfo());
		methods.add(mig.getMigrateViewToMysqlInfo());

		return methods;
	}

	/**
	 * Performs a migration based on a Migration object
	 * 
	 * @param mig
	 *            migration object to migrate
	 * @param targetPackageName
	 *            name of the package that should be used to generate the target
	 *            objects, e.g. db.mysql
	 */
	public static void migrate(com.mysql.grt.db.migration.Migration migObj,
			com.mysql.grt.db.mgmt.Rdbms targetRdbms,
			com.mysql.grt.db.Version version) throws Exception {

		new MigrationAccess().migrateCatalog(migObj, targetRdbms, version);
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

		if (params != null)
			params.add("excludeSourceSchemaName", "yes");

		new MigrationAccess().doDataBulkTransfer(sourceDbConn, sourceCatalog,
				targetDbConn, targetCatalog, params, logList);
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
		method.setModuleName("MigrationAccess");
		method.setCaption("Access Default");
		method.setDesc("Default method to migrate a Access column to MySQL.");
		method.setSourceStructName("db.Column");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateColumnToMysqlInfoParameters(method);

		// set default parameter autoDecimalDigits to "yes"
		method.getParams().add("autoDecimalDigits", "yes");

		ParameterGroup params = method.getParamGroups().get(0);
		params.getParams().add("autoDecimalDigits", "yes");

		return method;
	}

	/**
	 * migrates the name of an identifier
	 * 
	 * @param name
	 *            the source name of the identifier
	 * @return the migrated identifier name
	 */
	protected String migrateIdentifier(String name) {
		/*
		 * return name.toLowerCase().replaceAll("ä", "ae").replaceAll("ü", "ue")
		 * .replaceAll("ö", "oe").replaceAll("ß", "ss");
		 */
		return name.replaceAll("/", "_");
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

		// create target column
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
		targetColumn.setScale(sourceColumn.getScale());
		targetColumn.setPrecision(sourceColumn.getPrecision());
		targetColumn.setLength(sourceColumn.getLength());

		// migrate datatype
		SimpleDatatypeList simpleDatatypes = migObj.getTargetCatalog()
				.getSimpleDatatypes();

		String sourceDatatypeName = sourceColumn.getDatatypeName();

		// check migration params and only assign datatype if it was not
		// set in the migration parameters
		if (!migrateColumnParamsToMySql(targetColumn, migrationParams)) {
			if (sourceDatatypeName.equalsIgnoreCase("VARCHAR")) {
				targetColumn.setDatatypeName("VARCHAR");
				targetColumn.setLength(sourceColumn.getLength());
			} else if (sourceDatatypeName.equalsIgnoreCase("INTEGER")
					|| sourceDatatypeName.equalsIgnoreCase("INT")) {
				targetColumn.setDatatypeName("INT");
			} else if (sourceDatatypeName.equalsIgnoreCase("SMALLINT")) {
				targetColumn.setDatatypeName("SMALLINT");
			} else if (sourceDatatypeName.equalsIgnoreCase("COUNTER")) {
				targetColumn.setDatatypeName("INT");
				targetColumn.setAutoIncrement(1);
				targetColumn.setIsNullable(0);
			} else if (sourceDatatypeName.equalsIgnoreCase("BIT")) {
				targetColumn.setDatatypeName("TINYINT");
			} else if (sourceDatatypeName.equalsIgnoreCase("BYTE")) {
				targetColumn.setDatatypeName("TINYINT");
				targetColumn.getFlags().add("UNSIGNED");
			} else if (sourceDatatypeName.equalsIgnoreCase("REAL")
					|| sourceDatatypeName.equalsIgnoreCase("DOUBLE")) {
				targetColumn.setDatatypeName("DOUBLE");

				if ((targetColumn.getScale() == 0)
						&& (migrationParams != null)
						&& (migrationParams.get("autoDecimalDigits")
								.equalsIgnoreCase("yes"))) {
					targetColumn.setScale(targetColumn.getPrecision() / 3);
				}
			} else if (sourceDatatypeName.equalsIgnoreCase("CURRENCY")
					|| (sourceDatatypeName.equalsIgnoreCase("DECIMAL"))) {
				targetColumn.setDatatypeName("DECIMAL");
			} else if (sourceDatatypeName.equalsIgnoreCase("LONGBINARY")) {
				targetColumn.setDatatypeName("LONGBLOB");
			} else if (sourceDatatypeName.equalsIgnoreCase("LONGCHAR")) {
				targetColumn.setDatatypeName("LONGTEXT");
			} else if (sourceDatatypeName.equalsIgnoreCase("DATETIME")) {
				targetColumn.setDatatypeName("DATETIME");
			} else {
				targetColumn.setDatatypeName("VARCHAR");

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

		// return new created, migrated object
		return targetColumn;
	}

	/**
	 * Generates information about the View to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateViewToMysqlInfo() {

		// create migrateAccessTable method
		Method method = new Method(null);
		method.setName("migrateViewToMysql");
		method.setModuleName("MigrationAccess");
		method.setCaption("Access Default");
		method.setDesc("Default method to migrate an Access view to MySQL.");
		method.setSourceStructName("db.View");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateViewToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Migrates a view to a MySQL view
	 * 
	 * @param sourceTable
	 *            the object to migrate
	 * @param migrationParams
	 *            parameters used to define the target object
	 * 
	 * @return returns MySQL table object
	 */
	protected com.mysql.grt.db.mysql.View migrateViewToMysql(
			com.mysql.grt.db.migration.Migration migObj, View sourceView,
			GrtStringHashMap migrationParams, GrtObject parent) {

		Grt.getInstance().addMsg(
				"Migrating view " + sourceView.getName() + " ...");

		// create target view
		com.mysql.grt.db.mysql.View targetView;
		targetView = new com.mysql.grt.db.mysql.View(parent);

		// log creation of target object
		migUtils.addMigrationLogEntry(migObj, sourceView, targetView);

		// do migration
		targetView.setName(migrateIdentifier(sourceView.getName()));

		// comment SQL out for the moment
		targetView.setCommentedOut(1);

		String query = sourceView.getQueryExpression();

		if (query != null) {
			targetView.setQueryExpression(query.replaceAll("\\[", "`")
					.replaceAll("\\]", "`"));
		}

		migUtils.addMigrationLogEntry(migObj, sourceView, targetView,
				"The generated SQL has to be checked manually.",
				MigrationUtils.logWarning);

		// return new created, migrated object
		return targetView;
	}
}