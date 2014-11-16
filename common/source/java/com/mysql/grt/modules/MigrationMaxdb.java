package com.mysql.grt.modules;

import com.mysql.grt.*;
import com.mysql.grt.db.SimpleDatatypeList;
import com.mysql.grt.db.migration.*;
import com.mysql.grt.db.maxdb.*;

/**
 * GRT Migration Class for Maxdb 7.5/7.6
 * 
 * @author Mike
 * @version 1.0, 01/11/05
 * 
 */
public class MigrationMaxdb extends MigrationGeneric {
	
	/**
	 * Sequential number added to FK names to ensure their uniqueness
	 * 
	 */
	private int fkSeqNum = 0;

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(MigrationMaxdb.class, "MigrationGeneric");
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
		MigrationMaxdb mig = new MigrationMaxdb();
		methods.add(mig.getMigrateSchemaToMysqlInfo());
		methods.add(mig.getMigrateTableToMysqlInfo());
		methods.add(mig.getMigrateColumnToMysqlInfo());
		methods.add(mig.getMigrateForeignKeyToMysqlInfo());
		methods.add(mig.getMigrateViewToMysqlInfo());
		methods.add(mig.getMigrateRoutineToMysqlInfo());
		methods.add(mig.getMigrateSynonymToMysql());
		methods.add(mig.getMigrateTriggerToMysql());
		
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

		Grt.getInstance().addMsg("Starting MaxDB migration...");

		new MigrationMaxdb().migrateCatalog(migObj, targetRdbms, version);
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

		new MigrationMaxdb().doDataBulkTransfer(sourceDbConn, sourceCatalog,
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
	 * 
	 */
	protected com.mysql.grt.db.mysql.Schema migrateSchemaToMysql(
			com.mysql.grt.db.migration.Migration migObj, Schema sourceSchema,
			GrtStringHashMap migrationParams, GrtObject parent) {

		// call super migrate function to do basic migration
		com.mysql.grt.db.mysql.Schema targetSchema = super
				.migrateSchemaToMysql(migObj, sourceSchema, migrationParams,
						parent);
		
		for (int i = 0; i < sourceSchema.getSynonyms().size(); i++) {
			Synonym sourceSynonym = (Synonym)sourceSchema.getSynonyms().get(i);

			Grt.getInstance().addProgress(
					"Migrating synonym " + sourceSynonym.getName(),
					(i * 100) / sourceSchema.getSynonyms().size());
			if (Grt.getInstance().flushMessages() != 0) {
				Grt.getInstance().addMsg("Migration canceled by user.");
				return targetSchema;
			}

			com.mysql.grt.db.mysql.Synonym targetSynonym = (com.mysql.grt.db.mysql.Synonym) migUtils.migrateObject(this, migObj,
					sourceSynonym, targetSchema);

			if (targetSynonym != null)
				targetSchema.getSynonyms().add(targetSynonym);
		}

		return targetSchema;
	}

	/**
	 * Migrates an MaxDB table to a MySQL table
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

		String comment = "";

		// call migration method from the super class
		com.mysql.grt.db.mysql.Table targetTable;
		targetTable = super.migrateTableToMysql(migObj, sourceTable,
				migrationParams, parent);

		// MaxDB specific things

		// MaxDB has some table stats and some flags, that can't be migrated
		// noFixedLenghtColumn() might go into ROWFORMAT, but InnoDB uses
		// COMPACT anyway.
		if (sourceTable.getComment() != null) {
			comment += " Comment: " + sourceTable.getComment();
		}
		comment += " Hints: ";
		if (sourceTable.getPrivileges() != null)
			comment += " Privileges: " + sourceTable.getPrivileges();

		comment += " Created: " + sourceTable.getCreateDate() + " / "
				+ sourceTable.getCreateTime();
		comment += " Altered: " + sourceTable.getAlterDate() + " / "
				+ sourceTable.getAlterTime();

		if (sourceTable.getUpdStatDate() != null)
			comment += " Statistics: " + sourceTable.getUpdStatDate() + " / "
					+ sourceTable.getUpdStatTime();

		if (sourceTable.getSample() != null)
			comment += " Sample: " + sourceTable.getSample();

		if (sourceTable.getArchive() != null)
			comment += " Archive: " + sourceTable.getArchive();

		if (sourceTable.getVariableColumns() != null)
			comment += " Variable columns: " + sourceTable.getVariableColumns();

		if (sourceTable.getTableid() != null)
			comment += " TableId: " + sourceTable.getTableid();

		if (sourceTable.getNoFixedLengthColumn() > 0)
			comment += " No fixed length column "
					+ sourceTable.getNoFixedLengthColumn();

		targetTable.setComment(comment);

		// Trigger
		for (int i = 0; i < sourceTable.getTriggers().size(); i++) {
			Trigger sourceTrigger = (Trigger)sourceTable.getTriggers().get(i);

			com.mysql.grt.db.mysql.Trigger targetTrigger = (com.mysql.grt.db.mysql.Trigger) migUtils.migrateObject(this, migObj,
					sourceTrigger, targetTable);

			if (targetTrigger != null)
				targetTable.getTriggers().add(targetTrigger);
		}
		
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

		String comments = "";

		// create target table
		com.mysql.grt.db.mysql.Column targetColumn;
		targetColumn = new com.mysql.grt.db.mysql.Column(parent);

		// log creation of target object
		migUtils.addMigrationLogEntry(migObj, sourceColumn, targetColumn);

		// do migration
		targetColumn.setName(migUtils.getTargetName(migrationParams,
				migrateIdentifier(sourceColumn.getName())));
		targetColumn.setOldName(sourceColumn.getName());
		targetColumn.setIsNullable(sourceColumn.getIsNullable());
		targetColumn.setPrecision(sourceColumn.getPrecision());
		targetColumn.setScale(sourceColumn.getScale());
		targetColumn.setLength(sourceColumn.getLength());

		if (sourceColumn.getComment() != null) {
			comments += " Comment: " + sourceColumn.getComment();
		}
		comments += " Hints: ";
		comments += " Privileges: " + sourceColumn.getPrivileges();
		comments += " Created: " + sourceColumn.getCreateDate() + " / "
				+ sourceColumn.getCreateTime();
		comments += " Altered: " + sourceColumn.getAlterDate() + " / "
				+ sourceColumn.getAlterTime();

		if (sourceColumn.getDomainName() != null)
			comments += " Domain (name/owner): " + sourceColumn.getDomainName()
					+ " / " + sourceColumn.getDomainOwner();

		if (sourceColumn.getDefaultFunction() != null)
			comments += " Default function: "
					+ sourceColumn.getDefaultFunction();

		targetColumn.setComment(comments);

		String defaultValue = sourceColumn.getDefaultValue();
		if (sourceColumn.getDefaultValue() != null
				&& !sourceColumn.getDefaultValue().startsWith("DEFAULT SERIAL")) {
			targetColumn.setDefaultValue(defaultValue);
		} else {
			targetColumn.setDefaultValue("");
		}

		// migrate datatype
		SimpleDatatypeList simpleDatatypes = migObj.getTargetCatalog()
				.getSimpleDatatypes();

		String sourceDatatypeName = sourceColumn.getDatatypeName();

		if (!migrateColumnParamsToMySql(targetColumn, migrationParams)) {

			if (sourceDatatypeName.equals("SMALLINT")) {

				// NUMERIC
				// -32768 - +32767 = FIXED(5.0) + CHECK
				// no counterpart to MySQL signed, unsigned is done by check
				// constraints
				targetColumn.setDatatypeName("SMALLINT");

			} else if (sourceDatatypeName.equals("INTEGER")) {
				// -2147483648 and 2147483647 = FIXED(10.0) + CHECK
				// no counterpart to MySQL signed, unsigned is done by check
				// constraints
				targetColumn.setDatatypeName("INT");

			} else if (sourceDatatypeName.equals("FLOAT")) {
				// The data type FLOAT (p) defines a floating point number
				// (floating_point_literal). A column is defined that has a
				// floating point number with precision p (0<p<=38).
				// TODO: MySQL DECIMAL cannot hold all MAXDB float values
				targetColumn.setDatatypeName("DECIMAL");

				if ((migObj.getTargetCatalog().getVersion().getMajor() == 5 && migObj
						.getTargetCatalog().getVersion().getMinor() == 0)
						&& (migObj.getTargetCatalog().getVersion().getRelease() >= 3 && migObj
								.getTargetCatalog().getVersion().getRelease() <= 5)) {
					// From 5.0.3 - 5.0.5 the length was 64
					targetColumn.setPrecision(64);
				} else {
					// Prior to 5.0.3 the length was even higher (255), so 65
					// should be save.
					targetColumn.setPrecision(65);
				}

				if (sourceColumn.getLength() <= 30) {
					targetColumn.setScale(sourceColumn.getLength());
				} else {
					targetColumn.setScale(30);
				}

			} else if (sourceDatatypeName.equals("FIXED")) {
				// The data type FIXED (p,s) defines a fixed point number
				// (fixed_point_literal). A column is defined that has a fixed
				// point
				// number with precision p and s number of decimal places
				// (0<p<=38, s<=p).
				// If no s is specified, it is assumed that the decimal places
				// are 0.
				// TODO: MySQL can have upmost 30 decimals, whereas MaxDB can
				// have 37.
				targetColumn.setDatatypeName("DECIMAL");
				if (sourceColumn.getScale() <= 30) {
					targetColumn.setScale(sourceColumn.getScale());
				} else {
					targetColumn.setScale(30);
				}

			} else if (sourceDatatypeName.equals("VARCHAR")) {

				// STRING

				// VARCHAR [(n)]: 0<n<=8000, VARCHAR [(n)] UNICODE: 0<n<=4000

				// TODO: getRelease() seems broken!
				boolean haveLongCharAndBinary = ((migObj.getTargetCatalog()
						.getVersion().getMajor() > 5)
						|| (migObj.getTargetCatalog().getVersion().getMajor() == 5 && migObj
								.getTargetCatalog().getVersion().getMinor() >= 1) || (migObj
						.getTargetCatalog().getVersion().getMajor() == 5
						&& migObj.getTargetCatalog().getVersion().getMinor() == 0 && migObj
						.getTargetCatalog().getVersion().getRelease() >= 3));
				
				if (sourceColumn.getCodeType().equals("BYTE")) {
					// binary data

					if (haveLongCharAndBinary) {
						// MySQL 5.0.3+, VARBINARY = VARCHAR = max length 65535
						targetColumn.setDatatypeName("VARBINARY");
						
					} else {
						// prior to 5.0.3, we have VARCHAR + binary collation
						// but that's not equal
						// to MaxDB VARCHAR BYTE, VARCHAR BYTE = binary data
						if (sourceColumn.getLength() < 256) {
							targetColumn.setDatatypeName("TINYBLOB");
						} else {
							targetColumn.setDatatypeName("BLOB");
						}
					}

				} else {
					// non-binary data - ASCII or UNICODE

					if (sourceColumn.getLength() < 256) {
						targetColumn.setDatatypeName("VARCHAR");
					} else {
						if (haveLongCharAndBinary) {
							targetColumn.setDatatypeName("VARCHAR");
						} else {
							targetColumn.setDatatypeName("TEXT");
						}
					}
					// check for UNICODE - no special handling for ASCII,
					// let's use the server default character set for ASCII
					if (sourceColumn.getCodeType().equals("UNICODE")) {
						targetColumn.setCharacterSetName("ucs2");
					}

				}

			} else if (sourceDatatypeName.equals("CHAR")) {
				// CHAR[ACTER] [(n)]: 0<n<=8000, CHAR[ACTER] [(n)] UNICODE:
				// 0<n<=4000

				// TODO: getRelease() seems broken!
				boolean haveLongCharAndBinary = ((migObj.getTargetCatalog()
						.getVersion().getMajor() > 5)
						|| (migObj.getTargetCatalog().getVersion().getMajor() == 5 && migObj
								.getTargetCatalog().getVersion().getMinor() >= 1) || (migObj
						.getTargetCatalog().getVersion().getMajor() == 5
						&& migObj.getTargetCatalog().getVersion().getMinor() == 0 && migObj
						.getTargetCatalog().getVersion().getRelease() >= 3));
				
				if (sourceColumn.getCodeType().equals("BYTE")) {
					// binary data

					if (haveLongCharAndBinary) {
						// MySQL 5.0.3+, BINARY = CHAR = max length 255

						if (sourceColumn.getLength() < 256) {
							targetColumn.setDatatypeName("BINARY");
						} else {
							targetColumn.setDatatypeName("BLOB");
						}

					} else {
						// prior to 5.0.3, we have CHAR + binary collation but
						// that's not equal
						// to MaxDB CHAR BYTE, CHAR BYTE = binary data
						if (sourceColumn.getLength() < 256) {
							targetColumn.setDatatypeName("TINYBLOB");
						} else {
							targetColumn.setDatatypeName("BLOB");
						}
					}

				} else {
					// non-binary data - ASCII or UNICODE

					if (sourceColumn.getLength() < 256) {
						targetColumn.setDatatypeName("CHAR");
					} else {
						targetColumn.setDatatypeName("TEXT");
					}
					// check for UNICODE - no special handling for ASCII,
					// let's use the server default character set for ASCII
					if (sourceColumn.getCodeType().equals("UNICODE")) {
						targetColumn.setCharacterSetName("ucs2");
					}

				}

			} else if (sourceDatatypeName.equals("LONG")) {

				// TEXT and BLOB

				// LONG [VARCHAR] [ASCII | BYTE]: A maximum of 2 GB of
				// characters can be written in a LONG column.
				// LONG [VARCHAR] UNICODE: A maximum of 2 GB bytes can be
				// written in a LONG column.

				if (sourceColumn.getCodeType().equals("ASCII")) {
					targetColumn.setDatatypeName("LONGTEXT");
				} else if (sourceColumn.getCodeType().equals("UNICODE")) {
					targetColumn.setDatatypeName("LONGTEXT");
					targetColumn.setCharacterSetName("ucs2");
				} else if (sourceColumn.getCodeType().equals("BYTE")) {
					targetColumn.setDatatypeName("LONGBLOB");
				}

			} else if (sourceDatatypeName.equals("DATE")) {

				// DATETIME
				// MaxDB starts from 0001-01-01, MySQL says it supports only
				// 1001-01-01 and up.
				// All values below might work, but there's no warranty
				targetColumn.setDatatypeName("DATE");

			} else if (sourceDatatypeName.equals("TIME")) {

				targetColumn.setDatatypeName("TIME");

			} else if (sourceDatatypeName.equals("TIMESTAMP")) {

				// Possible data loss: MaxDB supports microseconds, MySQL does
				// not.
				targetColumn.setDatatypeName("DATETIME");

			} else if (sourceDatatypeName.equals("BOOLEAN")) {

				targetColumn.setDatatypeName("TINYINT");

			} else {

				// UNKNOWN

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
		//		 
		// return new created, migrated object
		return targetColumn;
	}

	/**
	 * Migrates a foreign key to MySQL
	 * 
	 * @param sourceRoutine
	 *            the object to migrate
	 * @param migrationParams
	 *            parameters used to define the target object
	 * 
	 * @return returns MySQL table object
	 */
	protected com.mysql.grt.db.mysql.ForeignKey migrateForeignKeyToMysql(
			com.mysql.grt.db.migration.Migration migObj,
			ForeignKey sourceForeignKey, GrtStringHashMap migrationParams,
			GrtObject parent) {

		// create foreign key
		com.mysql.grt.db.mysql.ForeignKey targetForeignKey;
		targetForeignKey = new com.mysql.grt.db.mysql.ForeignKey(parent);

		// log creation of target object
		migUtils.addMigrationLogEntry(migObj, sourceForeignKey,
				targetForeignKey);

		// do migration 
		// NOTE: we have to add a sequence number to the FK name to ensure that it's
		// unique within a schema. MaxDB does not require FK constraint names to be unique,
		// but InnoDB does
		targetForeignKey.setName(migUtils.getTargetName(migrationParams,
				migrateIdentifier(sourceForeignKey.getName().concat(Integer.toString(fkSeqNum++)))));
		targetForeignKey.setOldName(sourceForeignKey.getName());

		String overrideRules = migrationParams.get("overrideRules");
		if ((overrideRules != null)
				&& (overrideRules.compareToIgnoreCase("yes") == 0)) {
			targetForeignKey.setDeleteRule(migrationParams
					.get("defaultDeleteRule"));
			targetForeignKey.setUpdateRule(migrationParams
					.get("defaultUpdateRule"));
		} else {
			targetForeignKey.setDeleteRule(sourceForeignKey.getDeleteRule());
			targetForeignKey.setUpdateRule(sourceForeignKey.getUpdateRule());
		}
		
		targetForeignKey.setDeferability(sourceForeignKey.getDeferability());
		targetForeignKey
				.setReferedTableSchemaName(migrateIdentifier(sourceForeignKey
						.getReferedTableSchemaName()));
		targetForeignKey.setReferedTableName(migrateIdentifier(sourceForeignKey
				.getReferedTableName()));

		Table sourceTable = (Table) sourceForeignKey.getOwner();

		// migrate FK columns names
		for (int i = 0; i < sourceForeignKey.getColumns().size(); i++) {
			Column sourceColumnRef = (Column) sourceForeignKey.getColumns()
					.get(i);

			// get original source column
			Column sourceColumn = (Column) sourceTable.getColumns().get(
					sourceTable.getColumns().getIndexOfName(
							sourceColumnRef.getName()));

			com.mysql.grt.db.mysql.Column targetColumn = (com.mysql.grt.db.mysql.Column) (migUtils
					.findTargetObject(sourceColumn));

			targetForeignKey.getColumns().add(targetColumn);

		}

		// find target refered table
		Table refTable = (Table) sourceForeignKey.getReferedTable();
		if (refTable != null) {
			com.mysql.grt.db.mysql.Table targetRefTable = (com.mysql.grt.db.mysql.Table) (migUtils.findTargetObject(refTable));

			if (targetRefTable != null)
				targetForeignKey.setReferedTable(targetRefTable);
		}

		// migrate FK columns refs
		for (int i = 0; i < sourceForeignKey.getColumns().size(); i++) {
			Column sourceColumnRef = (Column) sourceForeignKey
					.getReferedColumns().get(i);

			com.mysql.grt.db.mysql.Column targetColumn = (com.mysql.grt.db.mysql.Column) (migUtils
					.findTargetObject(sourceColumnRef));

			if (targetColumn != null)
				targetForeignKey.getReferedColumns().add(targetColumn);
		}

		for (int i = 0; i < sourceForeignKey.getReferedColumnNames().size(); i++) {
			targetForeignKey.getReferedColumnNames().add(
					migrateIdentifier(sourceForeignKey.getReferedColumnNames()
							.get(i)));
		}
				
		if (targetForeignKey.getDeleteRule().equalsIgnoreCase("SET NULL")) {
			
			// TODO: MikeZ says it's a known limitation of the MT that the messages
			// do not show up on the overview of the "Manual Editing" screen
			migUtils.addMigrationLogEntry(migObj, sourceForeignKey, targetForeignKey, 
					"Currently MySQL does not support the foreign key DELETE rule SET NULL. " +
					"The generated SQL statement will be invalid. Please correct the SQL statement.", 
					MigrationUtils.logError);
			
		}

		// return new created, migrated object
		return targetForeignKey;
	}

	/**
	 * Migrates an MaxDB view to a MySQL view
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
	 * Migrates an MaxDB Routine to a MySQL Routine
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
	 * Migrates an MaxDB Synonym to a MySQL Synonym which will be mapped to a View in the C code
	 * 
	 * @param sourceSynonym
	 *            the object to migrate
	 * @param migrationParams
	 *            parameters used to define the target object
	 * 
	 * @return returns MySQL synonym object
	 */
	protected com.mysql.grt.db.mysql.Synonym migrateSynonymToMysql(
			com.mysql.grt.db.migration.Migration migObj, Synonym sourceSynonym,
			GrtStringHashMap migrationParams, GrtObject parent) {
		
		com.mysql.grt.db.mysql.Synonym targetSynonym = new com.mysql.grt.db.mysql.Synonym(parent);
		
//		 skip build-in synonyms which do not belong to the "users schema"
		String skipBuildin = migrationParams.get("skipBuildinSynonyms");
				
		if (skipBuildin != null && skipBuildin.equalsIgnoreCase("yes") && 
				(
						sourceSynonym.getName().equals("ACTIVECONFIGURATION") ||
						sourceSynonym.getName().equals("ALLOCATORSTATISTIC") ||
						sourceSynonym.getName().equals("ALL_CATALOG") ||
						sourceSynonym.getName().equals("ALL_COL_COMMENTS") ||
						sourceSynonym.getName().equals("ALL_COL_PRIVS") ||
						sourceSynonym.getName().equals("ALL_COL_PRIVS_MADE") ||
						sourceSynonym.getName().equals("ALL_COL_PRIVS_RECD") ||
						sourceSynonym.getName().equals("ALL_CONSTRAINTS") ||
						sourceSynonym.getName().equals("ALL_CONS_COLUMNS") ||
						sourceSynonym.getName().equals("ALL_DB_LINKS") ||
						sourceSynonym.getName().equals("ALL_DEF_AUDIT_OPTS") ||
						sourceSynonym.getName().equals("ALL_DEPENDENCIES") ||
						sourceSynonym.getName().equals("ALL_ERRORS") ||
						sourceSynonym.getName().equals("ALL_INDEXES") ||
						sourceSynonym.getName().equals("ALL_IND_COLUMNS") ||
						sourceSynonym.getName().equals("ALL_OBJECTS") ||
						sourceSynonym.getName().equals("ALL_SEQUENCES") ||
						sourceSynonym.getName().equals("ALL_SNAPSHOTS") ||
						sourceSynonym.getName().equals("ALL_SOURCE") ||
						sourceSynonym.getName().equals("ALL_SYNONYMS") ||
						sourceSynonym.getName().equals("ALL_TABLES") ||
						sourceSynonym.getName().equals("ALL_TAB_COLUMNS") ||
						sourceSynonym.getName().equals("ALL_TAB_COMMENTS") ||
						sourceSynonym.getName().equals("ALL_TAB_PRIVS") ||
						sourceSynonym.getName().equals("ALL_TAB_PRIVS_MADE") ||
						sourceSynonym.getName().equals("ALL_TAB_PRIVS_RECD") ||
						sourceSynonym.getName().equals("ALL_TRIGGERS") ||				
						sourceSynonym.getName().equals("ALL_TRIGGER_COLS") ||
						sourceSynonym.getName().equals("ALL_USERS") ||
						sourceSynonym.getName().equals("ALL_VIEWS") ||
						sourceSynonym.getName().equals("AUDIT_ACTIONS") ||
						sourceSynonym.getName().equals("BACKUPTHREADS") ||
						sourceSynonym.getName().equals("CACHESTATISTICS") ||
						sourceSynonym.getName().equals("CAT") ||
						sourceSynonym.getName().equals("CATALOGCACHESTATISTICS") ||
						sourceSynonym.getName().equals("CLASSCONTAINERS") ||
						sourceSynonym.getName().equals("CLASSCONTAINER_CHAINS") ||
						sourceSynonym.getName().equals("CLASSCONTAINER_KEYS") ||
						sourceSynonym.getName().equals("CLASSCONTAINER_ROOTS") ||
						sourceSynonym.getName().equals("CLU") ||
						sourceSynonym.getName().equals("COLS") ||
						sourceSynonym.getName().equals("COLUMNPRIVILEGES") ||
						sourceSynonym.getName().equals("COLUMNS") ||
						sourceSynonym.getName().equals("COMMANDCACHESTATISTICS") ||
						sourceSynonym.getName().equals("COMMANDSTATISTICS") ||
						sourceSynonym.getName().equals("CONFIGURATION") ||
						sourceSynonym.getName().equals("CONNECTEDUSERS") ||
						sourceSynonym.getName().equals("CONNECTPARAMETERS") ||
						sourceSynonym.getName().equals("CONSISTENTVIEWS") ||
						sourceSynonym.getName().equals("CONSTRAINTS") ||
						sourceSynonym.getName().equals("DATACACHE") ||
						sourceSynonym.getName().equals("DATASTATISTICS") ||
						sourceSynonym.getName().equals("DATASTATISTICSRESET") ||
						sourceSynonym.getName().equals("DATAVOLUMES") ||
						sourceSynonym.getName().equals("DBA_2PC_NEIGHBORS") ||
						sourceSynonym.getName().equals("DBA_2PC_PENDING") ||
						sourceSynonym.getName().equals("DBA_AUDIT_EXISTS") ||
						sourceSynonym.getName().equals("DBA_AUDIT_OBJECT") ||
						sourceSynonym.getName().equals("DBA_AUDIT_SESSION") ||				
						sourceSynonym.getName().equals("DBA_AUDIT_STATEMENT") ||
						sourceSynonym.getName().equals("DBA_AUDIT_TRAIL") ||
						sourceSynonym.getName().equals("DBA_BLOCKERS") ||
						sourceSynonym.getName().equals("DBA_CATALOG") ||
						sourceSynonym.getName().equals("DBA_CLUSTERS") ||
						sourceSynonym.getName().equals("DBA_CLU_COLUMNS") ||
						sourceSynonym.getName().equals("DBA_COL_COMMENTS") ||
						sourceSynonym.getName().equals("DBA_COL_PRIVS") ||
						sourceSynonym.getName().equals("DBA_CONSTRAINTS") ||
						sourceSynonym.getName().equals("DBA_CONS_COLUMNS") ||
						sourceSynonym.getName().equals("DBA_DATA_FILES") ||
						sourceSynonym.getName().equals("DBA_DB_LINKS") ||
						sourceSynonym.getName().equals("DBA_DDL_LOCKS") ||
						sourceSynonym.getName().equals("DBA_DEPENDENCIES") ||
						sourceSynonym.getName().equals("DBA_DML_LOCKS") ||			
						sourceSynonym.getName().equals("DBA_ERRORS") ||
						sourceSynonym.getName().equals("DBA_EXP_FILES") ||
						sourceSynonym.getName().equals("DBA_EXP_OBJECTS") ||
						sourceSynonym.getName().equals("DBA_EXP_VERSION") ||
						sourceSynonym.getName().equals("DBA_EXTENTS") ||			
						sourceSynonym.getName().equals("DBA_FREE_SPACE") ||
						sourceSynonym.getName().equals("DBA_INDEXES") ||
						sourceSynonym.getName().equals("DBA_IND_COLUMNS") ||
						sourceSynonym.getName().equals("DBA_LOCKS") ||
						sourceSynonym.getName().equals("DBA_OBJECTS") ||
						sourceSynonym.getName().equals("DBA_OBJECT_SIZE") ||
						sourceSynonym.getName().equals("DBA_OBJ_AUDIT_OPTS") ||
						sourceSynonym.getName().equals("DBA_PRIV_AUDIT_OPTS") ||
						sourceSynonym.getName().equals("DBA_PROFILES") ||
						sourceSynonym.getName().equals("DBA_ROLES") ||
						sourceSynonym.getName().equals("DBA_ROLE_PRIVS") ||
						sourceSynonym.getName().equals("DBA_ROLLBACK_SEGS") ||
						sourceSynonym.getName().equals("DBA_SEGMENTS") ||
						sourceSynonym.getName().equals("DBA_SEQUENCES") ||
						sourceSynonym.getName().equals("DBA_SNAPSHOTS") ||
						sourceSynonym.getName().equals("DBA_SNAPSHOT_LOGS") ||
						sourceSynonym.getName().equals("DBA_SOURCE") ||
						sourceSynonym.getName().equals("DBA_STMT_AUDIT_OPTS") ||
						sourceSynonym.getName().equals("DBA_SYNONYMS") ||
						sourceSynonym.getName().equals("DBA_SYS_PRIVS") ||
						sourceSynonym.getName().equals("DBA_TABLES") ||
						sourceSynonym.getName().equals("DBA_TABLESPACES") ||
						sourceSynonym.getName().equals("DBA_TAB_COLUMNS") ||
						sourceSynonym.getName().equals("DBA_TAB_COMMENTS") ||
						sourceSynonym.getName().equals("DBA_TAB_PRIVS") ||
						sourceSynonym.getName().equals("DBA_TRIGGERS") ||
						sourceSynonym.getName().equals("DBA_TRIGGER_COLS") ||
						sourceSynonym.getName().equals("DBA_TS_QUOTAS") ||
						sourceSynonym.getName().equals("DBA_USERS") ||
						sourceSynonym.getName().equals("DBA_VIEWS") ||
						sourceSynonym.getName().equals("DBA_WAITERS") ||
						sourceSynonym.getName().equals("DBPARAMETERS") ||
						sourceSynonym.getName().equals("DBPROCEDURES") ||
						sourceSynonym.getName().equals("DBPROCPARAMINFO") ||
						sourceSynonym.getName().equals("DBPROCPARAMS") ||
						sourceSynonym.getName().equals("DBTIMES") ||
						sourceSynonym.getName().equals("DB_STATE") ||
						sourceSynonym.getName().equals("DICT") ||
						sourceSynonym.getName().equals("DICTIONARY") ||
						sourceSynonym.getName().equals("DICT_COLUMNS") ||
						sourceSynonym.getName().equals("DOMAINCONSTRAINTS") ||
						sourceSynonym.getName().equals("DOMAINS") ||
						sourceSynonym.getName().equals("DUAL") ||
						sourceSynonym.getName().equals("ESTIMATED_PAGES") ||
						sourceSynonym.getName().equals("EXCEPTIONS") ||
						sourceSynonym.getName().equals("FILEDIRECTORIES") ||
						sourceSynonym.getName().equals("FILES") ||
						sourceSynonym.getName().equals("FOREIGNKEYCOLUMNS") ||
						sourceSynonym.getName().equals("FOREIGNKEYS") ||
						sourceSynonym.getName().equals("FUNCTIONS") ||			
						sourceSynonym.getName().equals("GARBAGECOLLECTOR_STATISTICS") ||
						sourceSynonym.getName().equals("GLOBAL_NAME") ||
						sourceSynonym.getName().equals("HISTORYINFO") ||
						sourceSynonym.getName().equals("HOTSTANDBYCOMPONENT") ||
						sourceSynonym.getName().equals("HOTSTANDBYGROUP") ||
						sourceSynonym.getName().equals("IND") ||
						sourceSynonym.getName().equals("INDEXCOLUMNS") ||				
						sourceSynonym.getName().equals("INDEXES") ||
						sourceSynonym.getName().equals("INDEXPAGES") ||
						sourceSynonym.getName().equals("INDEXSTATISTICS") ||
						sourceSynonym.getName().equals("INSTANCE") ||
						sourceSynonym.getName().equals("INTERNAL_STATE") ||
						sourceSynonym.getName().equals("IOBUFFERCACHES") ||
						sourceSynonym.getName().equals("INDEXES") ||
						sourceSynonym.getName().equals("INDEXPAGES") ||
						sourceSynonym.getName().equals("INDEXSTATISTICS") ||
						sourceSynonym.getName().equals("INSTANCE") ||
						sourceSynonym.getName().equals("INTERNAL_STATE") ||
						sourceSynonym.getName().equals("IOBUFFERCACHES") ||
						sourceSynonym.getName().equals("IOTHREADSTATISTICS") ||
						sourceSynonym.getName().equals("IOTHREADSTATISTICSRESET") ||
						sourceSynonym.getName().equals("LOCKLISTSTATISTICS") ||
						sourceSynonym.getName().equals("LOCKS") ||
						sourceSynonym.getName().equals("LOCKSTATISTICS") ||
						sourceSynonym.getName().equals("LOCK_HOLDER") ||
						sourceSynonym.getName().equals("LOCK_REQUESTOR") ||
						sourceSynonym.getName().equals("LOCK_WAITS") ||
						sourceSynonym.getName().equals("LOGINFORMATION") ||
						sourceSynonym.getName().equals("LOGQUEUESTATISTICS") ||
						sourceSynonym.getName().equals("LOGSTATISTICS") ||
						sourceSynonym.getName().equals("LOGSTATISTICSRESET") ||
						sourceSynonym.getName().equals("LOGVOLUMES") ||
						sourceSynonym.getName().equals("MACHINECONFIGURATION") ||
						sourceSynonym.getName().equals("MACHINEUTILIZATION") ||
						sourceSynonym.getName().equals("MAPCHARSETS") ||			
						sourceSynonym.getName().equals("MEMORYALLOCATORSTATISTICS") ||
						sourceSynonym.getName().equals("MEMORYHOLDERS") ||
						sourceSynonym.getName().equals("MEMORY_HOLDERS") ||
						sourceSynonym.getName().equals("MONITOR") ||
						sourceSynonym.getName().equals("MONITOR_CACHES") ||
						sourceSynonym.getName().equals("MONITOR_LOAD") ||
						sourceSynonym.getName().equals("MONITOR_LOCK") ||
						sourceSynonym.getName().equals("MONITOR_LOG") ||
						sourceSynonym.getName().equals("MONITOR_LONG") ||
						sourceSynonym.getName().equals("MONITOR_OMS") ||
						sourceSynonym.getName().equals("MONITOR_PAGES") ||
						sourceSynonym.getName().equals("MONITOR_ROW") ||
						sourceSynonym.getName().equals("MONITOR_TRANS") ||
						sourceSynonym.getName().equals("OBJ") ||
						sourceSynonym.getName().equals("OBJECTLOCKS") ||
						sourceSynonym.getName().equals("OMSDIAGNOSE") ||
						sourceSynonym.getName().equals("OMSLOCKS") ||
						sourceSynonym.getName().equals("OMS_HEAP_STATISTICS") ||
						sourceSynonym.getName().equals("OMS_LOCKOBJ_INFO") ||
						sourceSynonym.getName().equals("OMS_MEMORY_USAGE") ||
						sourceSynonym.getName().equals("OMS_VERSIONS") ||
						sourceSynonym.getName().equals("OPTIMIZERINFORMATION") ||
						sourceSynonym.getName().equals("OPTIMIZERSTATISTICS") ||
						sourceSynonym.getName().equals("PACKAGES") ||
						sourceSynonym.getName().equals("PAGES") ||
						sourceSynonym.getName().equals("PARAMETERS") ||
						sourceSynonym.getName().equals("PARSINFOS") ||
						sourceSynonym.getName().equals("PUBLIC_DEPENDENCY") ||
						sourceSynonym.getName().equals("READERWRITERLOCKINFORMATION") ||
						sourceSynonym.getName().equals("READERWRITERLOCKSTATISTICS") ||
						sourceSynonym.getName().equals("READERWRITERLOCKSTATISTICSRESET") ||
						sourceSynonym.getName().equals("READERWRITERLOCKWAITINGTASKS") ||
						sourceSynonym.getName().equals("RESOURCE_COST") ||
						sourceSynonym.getName().equals("RESTARTINFORMATION") ||
						sourceSynonym.getName().equals("ROLEPRIVILEGES") ||
						sourceSynonym.getName().equals("ROLES") ||
						sourceSynonym.getName().equals("ROLE_ROLE_PRIVS") ||
						sourceSynonym.getName().equals("ROLE_SYS_PRIVS") ||	
						sourceSynonym.getName().equals("ROLE_TAB_PRIVS") ||
						sourceSynonym.getName().equals("ROOTS") ||
						sourceSynonym.getName().equals("RUNNING_COMMANDS") ||
						sourceSynonym.getName().equals("RUNNING_PARSEIDS") ||
						sourceSynonym.getName().equals("SCHEMAPRIVILEGES") ||
						sourceSynonym.getName().equals("SCHEMAS") ||
						sourceSynonym.getName().equals("SEQ") ||
						sourceSynonym.getName().equals("SEQUENCES") ||
						sourceSynonym.getName().equals("SERVERDBS") ||
						sourceSynonym.getName().equals("SERVERDBSTATISTICS") ||
						sourceSynonym.getName().equals("SERVERTASKS") ||
						sourceSynonym.getName().equals("SESSIONS") ||
						sourceSynonym.getName().equals("SESSION_PRIVS") ||
						sourceSynonym.getName().equals("SESSION_ROLES") ||
						sourceSynonym.getName().equals("SNAPSHOTS") ||
						sourceSynonym.getName().equals("SPINLOCKPOOLSTATISTICS") ||
						sourceSynonym.getName().equals("SPINLOCKS") ||
						sourceSynonym.getName().equals("SPINLOCKSTATISTICS") ||
						sourceSynonym.getName().equals("STMT_AUDIT_OPTION_MAP") ||
						sourceSynonym.getName().equals("SYN") ||
						sourceSynonym.getName().equals("SYNONYMS") ||
						sourceSynonym.getName().equals("SYSCHECKTABLELOG") ||
						sourceSynonym.getName().equals("SYSCMD_ANALYZE") ||
						sourceSynonym.getName().equals("SYSDATA_ANALYZE") ||				
						sourceSynonym.getName().equals("SYSMONDATA") ||
						sourceSynonym.getName().equals("SYSMONITOR") ||
						sourceSynonym.getName().equals("SYSMON_ACTIVE_TASK") ||
						sourceSynonym.getName().equals("SYSMON_BACKUPIOACCESS") ||
						sourceSynonym.getName().equals("SYSMON_CONNECTION") ||
						sourceSynonym.getName().equals("SYSMON_DW") ||
						sourceSynonym.getName().equals("SYSMON_DW_ACTIVE") ||
						sourceSynonym.getName().equals("SYSMON_DW_RUNNABLE") ||
						sourceSynonym.getName().equals("SYSMON_IOACCESS") ||
						sourceSynonym.getName().equals("SYSMON_IOTHREAD") ||
						sourceSynonym.getName().equals("SYSMON_REGION") ||
						sourceSynonym.getName().equals("SYSMON_RUNNABLE") ||
						sourceSynonym.getName().equals("SYSMON_SPECIAL_THREAD") ||
						sourceSynonym.getName().equals("SYSMON_STORAGE") ||
						sourceSynonym.getName().equals("SYSMON_SV") ||
						sourceSynonym.getName().equals("SYSMON_SV_ACTIVE") ||
						sourceSynonym.getName().equals("SYSMON_SV_RUNNABLE") ||
						sourceSynonym.getName().equals("SYSMON_TASK") ||
						sourceSynonym.getName().equals("SYSMON_TASK_DETAIL") ||
						sourceSynonym.getName().equals("SYSMON_TOTALCOUNT") ||
						sourceSynonym.getName().equals("SYSMON_UKTHREAD") ||
						sourceSynonym.getName().equals("SYSMON_US") ||
						sourceSynonym.getName().equals("SYSMON_US_ACTIVE") ||
						sourceSynonym.getName().equals("SYSMON_US_RUNNABLE") ||
						sourceSynonym.getName().equals("SYSPARSEID") ||
						sourceSynonym.getName().equals("SYSSTATISTICS") ||
						sourceSynonym.getName().equals("SYSTEMTRIGGERS") ||
						sourceSynonym.getName().equals("SYSUPDSTATLOG") ||			
						sourceSynonym.getName().equals("SYSUPDSTATWANTED") ||
						sourceSynonym.getName().equals("TABLEPRIVILEGES") ||
						sourceSynonym.getName().equals("TABLES") ||
						sourceSynonym.getName().equals("TABLESTATISTICS") ||
						sourceSynonym.getName().equals("TABLE_PRIVILEGE_MAP") ||
						sourceSynonym.getName().equals("TABS") ||
						sourceSynonym.getName().equals("TASKLOADBALANCINGINFORMATION") ||
						sourceSynonym.getName().equals("TASKLOADBALANCINGTASKGROUPSTATES") ||
						sourceSynonym.getName().equals("TASKLOADBALANCINGTASKGROUPSTATES") ||
						sourceSynonym.getName().equals("TASKLOADBALANCINGTASKMOVES") ||
						sourceSynonym.getName().equals("TRANSACTIONHISTORY") ||
						sourceSynonym.getName().equals("TRANSACTIONS") ||
						sourceSynonym.getName().equals("TRIGGERS") ||
						sourceSynonym.getName().equals("UNLOADEDSTATEMENTS") ||
						sourceSynonym.getName().equals("USERS") ||		
						sourceSynonym.getName().equals("USERSTATISTICS") ||
						sourceSynonym.getName().equals("USER_AUDIT_OBJECT") ||
						sourceSynonym.getName().equals("USER_AUDIT_SESSION") ||
						sourceSynonym.getName().equals("USER_AUDIT_STATEMENT") ||
						sourceSynonym.getName().equals("USER_AUDIT_TRAIL") ||
						sourceSynonym.getName().equals("USER_CATALOG") ||
						sourceSynonym.getName().equals("USER_CLUSTERS") ||
						sourceSynonym.getName().equals("USER_CLU_COLUMNS") ||
						sourceSynonym.getName().equals("USER_COL_COMMENTS") ||
						sourceSynonym.getName().equals("USER_COL_PRIVS") ||
						sourceSynonym.getName().equals("USER_COL_PRIVS_MADE") ||
						sourceSynonym.getName().equals("USER_COL_PRIVS_RECD") ||
						sourceSynonym.getName().equals("USER_CONSTRAINTS") ||
						sourceSynonym.getName().equals("USER_CONS_COLUMNS") ||
						sourceSynonym.getName().equals("USER_DB_LINKS") ||
						sourceSynonym.getName().equals("USER_DEPENDENCIES") ||
						sourceSynonym.getName().equals("USER_ERRORS") ||
						sourceSynonym.getName().equals("USER_EXTENTS") ||
						sourceSynonym.getName().equals("USER_FREE_SPACE") ||
						sourceSynonym.getName().equals("USER_INDEXES") ||
						sourceSynonym.getName().equals("USER_IND_COLUMNS") ||
						sourceSynonym.getName().equals("USER_OBJECTS") ||
						sourceSynonym.getName().equals("USER_OBJECT_SIZE") ||
						sourceSynonym.getName().equals("USER_OBJ_AUDIT_OPTS") ||
						sourceSynonym.getName().equals("USER_RESOURCE_LIMITS") ||
						sourceSynonym.getName().equals("USER_ROLE_PRIVS") ||
						sourceSynonym.getName().equals("USER_SEGMENTS") ||
						sourceSynonym.getName().equals("USER_SEQUENCES") ||
						sourceSynonym.getName().equals("USER_SNAPSHOTS") ||
						sourceSynonym.getName().equals("USER_SNAPSHOT_LOGS") ||
						sourceSynonym.getName().equals("USER_SOURCE") ||
						sourceSynonym.getName().equals("USER_SYNONYMS") ||
						sourceSynonym.getName().equals("USER_SYS_PRIVS") ||
						sourceSynonym.getName().equals("USER_TABLES") ||
						sourceSynonym.getName().equals("USER_TABLESPACES") ||
						sourceSynonym.getName().equals("USER_TAB_COLUMNS") ||
						sourceSynonym.getName().equals("USER_TAB_COMMENTS") ||
						sourceSynonym.getName().equals("USER_TAB_PRIVS") ||
						sourceSynonym.getName().equals("USER_TAB_PRIVS_MADE") ||
						sourceSynonym.getName().equals("USER_TAB_PRIVS_RECD") ||
						sourceSynonym.getName().equals("USER_TRIGGERS") ||
						sourceSynonym.getName().equals("USER_TRIGGER_COLS") ||
						sourceSynonym.getName().equals("USER_TS_QUOTAS") ||
						sourceSynonym.getName().equals("USER_USERS") ||
						sourceSynonym.getName().equals("USER_VIEWS") ||
						sourceSynonym.getName().equals("V$NLS_PARAMETERS") ||			
						sourceSynonym.getName().equals("VERSION") ||
						sourceSynonym.getName().equals("VERSIONS") ||
						sourceSynonym.getName().equals("VIEWCOLUMNS") ||
						sourceSynonym.getName().equals("VIEWDEFS") ||
						sourceSynonym.getName().equals("VIEWS")
					)
			) {
			targetSynonym = null;
			
		} else {
							
			// Synonym name = View name
			targetSynonym.setName(sourceSynonym.getName());			
		
			// Table that is refered by the Synonym
			Table refTable = (Table) sourceSynonym.getReferedObject();
			if (refTable != null) {
				com.mysql.grt.db.mysql.Table targetRefTable = (com.mysql.grt.db.mysql.Table) (migUtils.findTargetObject(refTable));

				if (targetRefTable != null)
					targetSynonym.setReferedObject(targetRefTable);
			}
		}
		
		return targetSynonym;
	}

	
	/**
	 * Migrates a column to a MySQL column
	 * 
	 * @param sourceTrigger
	 *            the object to migrate
	 * @param migrationParams
	 *            parameters used to define the target object
	 * @param parent
	 *            parent object of the migrated object
	 * 
	 * @return returns MySQL trigger object
	 */
	protected com.mysql.grt.db.mysql.Trigger migrateTriggerToMysql(
			com.mysql.grt.db.migration.Migration migObj, Trigger sourceTrigger,
			GrtStringHashMap migrationParams, GrtObject parent) {
		
		com.mysql.grt.db.mysql.Trigger targetTrigger = new com.mysql.grt.db.mysql.Trigger(parent);

		targetTrigger.setName(sourceTrigger.getName());
		targetTrigger.setOldName(sourceTrigger.getName());
		targetTrigger.setComment(sourceTrigger.getComment());
		targetTrigger.setTiming(sourceTrigger.getTiming());
		targetTrigger.setEvent(sourceTrigger.getEvent());
		targetTrigger.setStatement(sourceTrigger.getStatement());
		
		/*
		 
		 
		
//		 Table that is refered by the Synonym
		Table refTable = (Table) sourceSynonym.getReferedObject();
		if (refTable != null) {
			com.mysql.grt.db.mysql.Table targetRefTable = (com.mysql.grt.db.mysql.Table) (migUtils.findTargetObject(refTable));

			if (targetRefTable != null)
				targetSynonym.setReferedObject(targetRefTable);
		}
*/
	
		return targetTrigger;
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
		method.setModuleName("MigrationMaxdb");
		method.setCaption("MaxDB Default");
		method.setDesc("Default method to migrate an MaxDB schema to MySQL.");
		method.setSourceStructName("db.maxdb.Schema");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateSchemaToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates information about the MaxDB Table to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateTableToMysqlInfo() {

		// create migrateMaxdbTable method
		Method method = new Method(null);
		method.setName("migrateTableToMysql");
		method.setModuleName("MigrationMaxdb");
		method.setCaption("MaxDB Default");
		method.setDesc("Default method to migrate an MaxDB table to MySQL.");
		method.setSourceStructName("db.maxdb.Table");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateTableToMysqlInfoParameters(method);

		return method;
	}

	protected void addMigrateTableToMysqlInfoParameters(Method method) {

		// specify the parameters the method understands
		GrtStringHashMap paramNames = new GrtStringHashMap();
		method.setParams(paramNames);
		paramNames.add("engine", "INNODB");
		paramNames.add("charset", "");
		paramNames.add("collation", "");
		paramNames.add("addAutoincrement", "yes");

		// create a list of parameter groups the user can choose from
		ParameterGroupList paramGroupList = new ParameterGroupList();
		method.setParamGroups(paramGroupList);

		// add parameter group
		ParameterGroup paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Data consistency");
		paramGroup
				.setDesc("Standard parameter group. "
						+ "The migrated tables will use the "
						+ "InnoDB storage "
						+ "engine to offer transactional and "
						+ "foreign key support.");
		GrtStringHashMap params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("engine", "INNODB");
		params.add("addAutoincrement", "no");

		// add parameter group
		paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Statistical data");
		paramGroup.setDesc("Choose this parameter group for tables that "
				+ "contain lots of data which does not need "
				+ "transaction safety. This method is ideal "
				+ "for logging information or statistical data.");
		params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("engine", "MyISAM");
		params.add("addAutoincrement", "no");

		// add parameter group
		paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Data consistency / multilanguage");
		paramGroup.setDesc("The migrated tables will use the "
				+ "InnoDB storage " + "engine to offer transactional and "
				+ "foreign key support and use " + "UTF8 as default charset.");
		params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("engine", "INNODB");
		params.add("charset", "utf8");
		params.add("collation", "utf8_general_ci");
		params.add("addAutoincrement", "no");

		// add parameter group
		paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Data consistency / UNICODE");
		paramGroup.setDesc("The migrated tables will use the "
				+ "InnoDB storage " + "engine to offer transactional and "
				+ "foreign key support and use "
				+ "UNICODE as default charset.");
		params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("engine", "INNODB");
		params.add("charset", "ucs2");
		params.add("collation", "ucs2_general_ci");
		params.add("addAutoincrement", "no");

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
		method.setModuleName("MigrationMaxdb");
		method.setCaption("MaxDB Default");
		method.setDesc("Default method to migrate a MaxDB column to MySQL.");
		method.setSourceStructName("db.maxdb.Column");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateColumnToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates information about the ForeignKey to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateForeignKeyToMysqlInfo() {

		// create method description
		Method method = new Method(null);
		method.setName("migrateForeignKeyToMysql");
		method.setModuleName("MigrationMaxdb");
		method.setCaption("MaxDB");
		method.setDesc("MaxDB method to migrate a foreign key to MySQL.");
		method.setSourceStructName("db.maxdb.ForeignKey");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateForeignKeyToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates information about the View to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateViewToMysqlInfo() {

		// create migrateMaxdbTable method
		Method method = new Method(null);
		method.setName("migrateViewToMysql");
		method.setModuleName("MigrationMaxdb");
		method.setCaption("MaxDB Default");
		method.setDesc("Default method to migrate an MaxDB view to MySQL.");
		method.setSourceStructName("db.maxdb.View");
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

		// create migrateMaxdbTable method
		Method method = new Method(null);
		method.setName("migrateRoutineToMysql");
		method.setModuleName("MigrationMaxdb");
		method.setCaption("MaxDB Default");
		method.setDesc("Default method to migrate an MaxDB routine to MySQL.");
		method.setSourceStructName("db.maxdb.Routine");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateRoutineToMysqlInfoParameters(method);

		return method;
	}
	
	/**
	 * Generates information about the Synonym to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateSynonymToMysql() {
		
		// create migrateMaxdbTable method
		Method method = new Method(null);
		method.setName("migrateSynonymToMysql");
		method.setModuleName("MigrationMaxdb");
		method.setCaption("MaxDB Default");
		method.setDesc("Default method to migrate a MaxDB synonym to MySQL.");
		method.setSourceStructName("db.maxdb.Synonym");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		addMigrateSynonymToMysqlInfoParameters(method);

		return method;

	}
	
	protected void addMigrateSynonymToMysqlInfoParameters(Method method) {

		// specify the parameters the method understands
		GrtStringHashMap paramNames = new GrtStringHashMap();
		method.setParams(paramNames);
		paramNames.add("skipBuildinSynonyms", "yes");

		// create a list of parameter groups the user can choose from
		ParameterGroupList paramGroupList = new ParameterGroupList();
		method.setParamGroups(paramGroupList);

		// add parameter group
		ParameterGroup paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Skip build-in synonyms");
		paramGroup
				.setDesc("Standard parameter group. " +
						"Build-in synonyms will be skipped."
						);
		GrtStringHashMap params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("skipBuildinSynonyms", "yes");
		
	}
	
	/**
	 * Generates information about the Triger to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateTriggerToMysql() {
		
		// create migrateMaxdbTable method
		Method method = new Method(null);
		method.setName("migrateTriggerToMysql");
		method.setModuleName("MigrationMaxdb");
		method.setCaption("MaxDB Default");
		method.setDesc("Default method to migrate a MaxDB trigger to MySQL.");
		method.setSourceStructName("db.maxdb.Trigger");
		method.setTargetPackageName("db.mysql");
		method.setRating(1);

		return method;
	}

}