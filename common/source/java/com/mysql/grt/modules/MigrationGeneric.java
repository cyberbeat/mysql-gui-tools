package com.mysql.grt.modules;

import java.sql.*;
import java.io.InputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.text.SimpleDateFormat;
import java.math.BigDecimal;

import com.mysql.grt.*;
import com.mysql.grt.base.*;
import com.mysql.grt.db.migration.*;
import com.mysql.grt.db.*;

/**
 * GRT Migration Class
 * 
 * @author MikeZ
 * @version 1.0, 01/16/05
 * 
 */
public class MigrationGeneric {

	protected MigrationUtils migUtils = new MigrationUtils();

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(MigrationGeneric.class, "");
	}

	/**
	 * Collects information about migration the methods to let the user choose
	 * which one to take
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	public static MethodList migrationMethods() {

		MigrationGeneric mig = new MigrationGeneric();

		MethodList methods = new MethodList();

		// add information about available methods to methodlist
		methods.add(mig.getMigrateSchemaToMysqlInfo());
		methods.add(mig.getMigrateTableToMysqlInfo());
		methods.add(mig.getMigrateColumnToMysqlInfo());
		methods.add(mig.getMigrateIndexToMysqlInfo());
		methods.add(mig.getMigrateForeignKeyToMysqlInfo());
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

		Grt.getInstance().addMsg("Starting generic migration...");

		new MigrationGeneric().migrateCatalog(migObj, targetRdbms, version);
	}

	public static void dataBulkTransfer(
			com.mysql.grt.db.mgmt.Connection sourceDbConn,
			Catalog sourceCatalog,
			com.mysql.grt.db.mgmt.Connection targetDbConn,
			Catalog targetCatalog, GrtStringHashMap params,
			com.mysql.grt.base.ObjectLogList logList) throws Exception {

		new MigrationGeneric().doDataBulkTransfer(sourceDbConn, sourceCatalog,
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
		if (name != null)
			return name;
		else
			return "";
	}

	/**
	 * migrates the sourceCatalog and stores the targetCatalog in the global
	 * migration object
	 * 
	 * @param migObj
	 *            migration object to migrate
	 * @param targetPackageName
	 *            name of the package that should be used to generate the target
	 *            objects, e.g. db.mysql
	 */
	protected void migrateCatalog(com.mysql.grt.db.migration.Migration migObj,
			com.mysql.grt.db.mgmt.Rdbms targetRdbms,
			com.mysql.grt.db.Version version) {

		// create target catalog based on the targetPackageName
		Catalog targetCatalog = migObj.setTargetCatalog((Catalog) Grt
				.getGrtClassInstance(targetRdbms.getDatabaseObjectPackage()
						+ ".Catalog", migObj));

		targetCatalog.setName("Standard");
		targetCatalog.setOldName(targetCatalog.getName());

		// set the version of the target database
		if (version != null) {
			version.setOwner(targetCatalog);

			targetCatalog.setVersion(version);
		} else if (targetRdbms.getName().equalsIgnoreCase("Mysql")) {
			version = new Version(targetCatalog);
			version.setMajor(5);
			version.setMinor(0);
			version.setRelease(21);
			version.setName("5.0.21");

			targetCatalog.setVersion(version);
		} else {
			version = new Version(targetCatalog);
			version.setMajor(1);
			version.setName("1.0.0");

			targetCatalog.setVersion(version);
		}

		buildSimpleDatatypes(targetCatalog, targetRdbms);

		// migrate all source schemata to target schemata
		for (int i = 0; i < migObj.getSourceCatalog().getSchemata().size(); i++) {
			Schema sourceSchema = migObj.getSourceCatalog().getSchemata()
					.get(i);

			// migrate schema
			Schema targetSchema = (Schema) migUtils.migrateObject(this, migObj,
					sourceSchema, targetCatalog);

			// add generated schema to targetCatalog
			targetCatalog.getSchemata().add(targetSchema);
		}

		Grt.getInstance().addMsg("Migration completed.");
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

		// create target schemata based on the targetPackageName
		com.mysql.grt.db.mysql.Schema targetSchema = new com.mysql.grt.db.mysql.Schema(
				parent);

		// log creation of target object
		migUtils.addMigrationLogEntry(migObj, sourceSchema, targetSchema);

		// migrate schema
		targetSchema.setName(migUtils.getTargetName(migrationParams,
				migrateIdentifier(sourceSchema.getName())));
		targetSchema.setOldName(sourceSchema.getName());

		// consider migration parameters
		if (migrationParams != null) {
			String charset = migrationParams.get("charset");
			if (charset != null)
				targetSchema.setDefaultCharacterSetName(charset);

			String collation = migrationParams.get("collation");
			if (charset != null)
				targetSchema.setDefaultCollationName(collation);
		}

		// migrate tables
		Grt.getInstance().addMsg("Migrating tables ...");

		for (int i = 0; i < sourceSchema.getTables().size(); i++) {
			Table sourceTable = sourceSchema.getTables().get(i);

			Grt.getInstance().addProgress(
					"Migrating table " + sourceTable.getName(),
					(i * 100) / sourceSchema.getTables().size());
			if (Grt.getInstance().flushMessages() != 0) {
				Grt.getInstance().addMsg("Migration canceled by user.");
				return targetSchema;
			}

			Table targetTable = (Table) migUtils.migrateObject(this, migObj,
					sourceTable, targetSchema);

			if (targetTable != null)
				targetSchema.getTables().add(targetTable);
		}

		// migrate tables - foreign keys (they need to be migrated after all
		// tables are migrated so the references can be found
		for (int i = 0; i < sourceSchema.getTables().size(); i++) {
			Table sourceTable = sourceSchema.getTables().get(i);

			migrateTableForeignKeysToMysql(migObj, sourceTable);
		}

		// migrate views
		Grt.getInstance().addMsg("Migrating views ...");

		for (int i = 0; i < sourceSchema.getViews().size(); i++) {
			View sourceView = sourceSchema.getViews().get(i);

			Grt.getInstance().addProgress(
					"Migrating view " + sourceView.getName(),
					(i * 100) / sourceSchema.getViews().size());
			if (Grt.getInstance().flushMessages() != 0) {
				Grt.getInstance().addMsg("Migration canceled by user.");
				return targetSchema;
			}

			targetSchema.getViews().add(
					(View) migUtils.migrateObject(this, migObj, sourceView,
							targetSchema));
		}

		// migrate Routines
		Grt.getInstance().addMsg("Migrating routines ...");

		for (int i = 0; i < sourceSchema.getRoutines().size(); i++) {
			Routine sourceRoutine = sourceSchema.getRoutines().get(i);

			Grt.getInstance().addProgress(
					"Migrating routine " + sourceRoutine.getName(),
					(i * 100) / sourceSchema.getRoutines().size());
			if (Grt.getInstance().flushMessages() != 0) {
				Grt.getInstance().addMsg("Migration canceled by user.");
				return targetSchema;
			}

			targetSchema.getRoutines().add(
					(Routine) migUtils.migrateObject(this, migObj,
							sourceRoutine, targetSchema));
		}

		// Hide progress bar
		Grt.getInstance().addProgress("", -1);
		Grt.getInstance().flushMessages();

		return targetSchema;
	}

	/**
	 * Migrates a table to a MySQL table
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

		// create target table
		com.mysql.grt.db.mysql.Table targetTable;
		targetTable = new com.mysql.grt.db.mysql.Table(parent);

		// log creation of target object
		migUtils.addMigrationLogEntry(migObj, sourceTable, targetTable);

		// do migration
		targetTable.setName(migUtils.getTargetName(migrationParams,
				migrateIdentifier(sourceTable.getName())));
		targetTable.setOldName(sourceTable.getName());

		// consider migration parameters
		boolean addAutoincParam = false;
		if (migrationParams != null) {
			String charset = migrationParams.get("charset");
			if (charset != null)
				targetTable.setDefaultCharacterSetName(charset);

			String collation = migrationParams.get("collation");
			if (charset != null)
				targetTable.setDefaultCollationName(collation);

			String engine = migrationParams.get("engine");
			if (engine != null)
				targetTable.setTableEngine(engine);

			String addAutoincrement = migrationParams.get("addAutoincrement");
			if ((addAutoincrement != null)
					&& (addAutoincrement.equalsIgnoreCase("yes")))
				addAutoincParam = true;

		}

		// charset
		targetTable.setDefaultCharacterSetName(migrationParams.get("charset"));
		targetTable.setDefaultCollationName(migrationParams.get("collation"));

		// migrate columns
		for (int i = 0; i < sourceTable.getColumns().size(); i++) {
			Column sourceColumn = sourceTable.getColumns().get(i);
			Column targetColumn = (Column) migUtils.migrateObject(this, migObj,
					sourceColumn, targetTable);

			targetTable.getColumns().add(targetColumn);
		}

		// migrate indices
		for (int i = 0; i < sourceTable.getIndices().size(); i++) {
			Index sourceIndex = sourceTable.getIndices().get(i);

			Index targetIndex = (Index) migUtils.migrateObject(this, migObj,
					sourceIndex, targetTable);

			if (targetIndex != null)
				targetTable.getIndices().add(targetIndex);
		}

		// primary key
		Index sourcePk = sourceTable.getPrimaryKey();
		if (sourcePk != null) {
			Index targetPk = (Index) (migUtils.findTargetObject(sourcePk));

			if (targetPk != null)
				targetPk.setIsPrimary(1);

			targetTable.setPrimaryKey(targetPk);

			// make sure all columns are not null
			for (int i = 0; i < targetPk.getColumns().size(); i++) {
				Column refCol = targetPk.getColumns().get(i).getReferedColumn();
				if (refCol.getIsNullable() == 1)
					refCol.setIsNullable(0);
			}
		}

		if ((addAutoincParam)
				&& (targetTable.getPrimaryKey() != null)
				&& (targetTable.getPrimaryKey().getColumns().size() == 1)
				&& (targetTable.getPrimaryKey().getColumns().get(0)
						.getReferedColumn().getDatatypeName()
						.equalsIgnoreCase("INTEGER"))) {
			com.mysql.grt.db.mysql.Column pkColumn;
			pkColumn = (com.mysql.grt.db.mysql.Column) targetTable
					.getPrimaryKey().getColumns().get(0).getReferedColumn();

			if (pkColumn != null)
				pkColumn.setAutoIncrement(1);
		}

		// make sure there is only one autoInc value
		int autoIncCount = 0;
		for (int i = 0; i < targetTable.getColumns().size(); i++) {
			com.mysql.grt.db.mysql.Column col = (com.mysql.grt.db.mysql.Column) targetTable
					.getColumns().get(i);

			if (col.getAutoIncrement() == 1) {
				if (autoIncCount == 0) {
					// if this table has a PK
					if (targetTable.getPrimaryKey() != null) {
						// and the current column is not part of the PK
						IndexColumnList indexColumns = targetTable
								.getPrimaryKey().getColumns();
						boolean indexFoundInPk = false;

						for (int j = 0; j < indexColumns.size(); j++) {
							if (indexColumns.get(j).getReferedColumn() == col) {
								indexFoundInPk = true;
								break;
							}
						}
						if (indexFoundInPk) {
							autoIncCount = 1;
						} else {
							// reset AutoInc
							col.setAutoIncrement(0);
						}
					} else {
						// if there is no PK but an AI column, create a new PK
						// with that column
						com.mysql.grt.db.mysql.Index targetPk = new com.mysql.grt.db.mysql.Index(
								targetTable);
						com.mysql.grt.db.mysql.IndexColumn pkCol = new com.mysql.grt.db.mysql.IndexColumn(
								targetPk);

						targetPk.setName("PRIMARY");
						targetPk.setIndexType("PRIMARY");
						targetPk.setIsPrimary(1);

						pkCol.setName(col.getName());
						pkCol.setReferedColumn(col);

						targetPk.getColumns().add(pkCol);
						targetTable.setPrimaryKey(targetPk);

						targetTable.getIndices().add(targetPk);

						autoIncCount = 1;
					}
				} else {
					col.setAutoIncrement(0);
				}
			}
		}

		// return new created, migrated object
		return targetTable;
	}

	protected void migrateTableForeignKeysToMysql(
			com.mysql.grt.db.migration.Migration migObj, Table sourceTable) {
		Table targetTable = (Table) (migUtils.findTargetObject(sourceTable));

		// migrate foreign keys
		if (targetTable != null) {
			for (int i = 0; i < sourceTable.getForeignKeys().size(); i++) {
				ForeignKey sourceForeignKey = sourceTable.getForeignKeys().get(
						i);

				targetTable.getForeignKeys().add(
						(ForeignKey) migUtils.migrateObject(this, migObj,
								sourceForeignKey, targetTable));
			}
		}
	}

	/**
	 * Applys the migration parameter to the column
	 * 
	 * @param targetColumn
	 *            the column who's settings should be changed
	 * @param migrationParams
	 *            parameters used to define the target object
	 * 
	 * @return returns true if the datatype got set
	 */
	protected boolean migrateColumnParamsToMySql(
			com.mysql.grt.db.mysql.Column targetColumn,
			GrtStringHashMap migrationParams) {
		boolean result = false;

		if (migrationParams != null) {
			boolean forceDecimalDigits = ((migrationParams
					.get("forceDecimalDigits") != null) && migrationParams.get(
					"forceDecimalDigits").equalsIgnoreCase("yes"));

			if (forceDecimalDigits
					&& (migrationParams.get("forcePrecisionValue") != null)
					&& !migrationParams.get("forcePrecisionValue").equals("")) {
				targetColumn.setPrecision(Integer.parseInt(migrationParams
						.get("forcePrecisionValue")));

				if ((migrationParams.get("forceScaleValue") != null)
						&& (!migrationParams.get("forceScaleValue").equals(""))) {
					targetColumn.setScale(Integer.parseInt(migrationParams
							.get("forceScaleValue")));
				}
			}

			boolean forceLength = ((migrationParams.get("forceLength") != null) && migrationParams
					.get("forceLength").equalsIgnoreCase("yes"));

			if (forceLength
					&& (migrationParams.get("forceLengthValue") != null)
					&& !migrationParams.get("forceLengthValue").equals("")) {
				targetColumn.setLength(Integer.parseInt(migrationParams
						.get("forceLengthValue")));
			}

			if ((migrationParams.get("forceDatatypeName") != null)
					&& !migrationParams.get("forceDatatypeName").equals("")) {
				targetColumn.setDatatypeName(migrationParams
						.get("forceDatatypeName"));
				result = true;
			}
		}

		return result;
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
		SimpleDatatypeList simpleDatatypes = migObj.getTargetCatalog()
				.getSimpleDatatypes();

		String sourceDatatypeName = sourceColumn.getDatatypeName();

		if (!migrateColumnParamsToMySql(targetColumn, migrationParams)) {
			// try to find datatype in target list
			com.mysql.grt.db.SimpleDatatypeList targetSimpleDatatypes = migObj
					.getTargetCatalog().getSimpleDatatypes();
			for (int i = 0; i < targetSimpleDatatypes.size(); i++) {
				com.mysql.grt.db.SimpleDatatype datatype = targetSimpleDatatypes
						.get(i);
				if (datatype.getName().equalsIgnoreCase(sourceDatatypeName)) {
					targetColumn.setDatatypeName(datatype.getName());
					targetColumn.setSimpleType(datatype);
					break;
				}
			}

			if (!targetColumn.getName().equals("")) {
				DatatypeMappingList mappings = migObj
						.getGenericDatatypeMappings();

				for (int i = 0; i < mappings.size(); i++) {
					DatatypeMapping mapping = mappings.get(i);

					if (mapping.getSourceDatatypeName().equalsIgnoreCase(
							sourceDatatypeName.trim())) {
						// check length condition
						if (((mapping.getLengthConditionFrom() > 0) && (sourceColumn
								.getLength() < mapping.getLengthConditionFrom()))
								|| ((mapping.getLengthConditionTo() > 0) && (sourceColumn
										.getLength() > mapping
										.getLengthConditionTo()))) {
							continue;
						}

						// check precision condition
						if (((mapping.getPrecisionConditionFrom() > 0) && (sourceColumn
								.getPrecision() < mapping
								.getPrecisionConditionFrom()))
								|| ((mapping.getPrecisionConditionTo() > 0) && (sourceColumn
										.getPrecision() > mapping
										.getPrecisionConditionTo()))) {
							continue;
						}

						// check scale condition
						if (((mapping.getScaleConditionFrom() > 0) && (sourceColumn
								.getScale() < mapping.getScaleConditionFrom()))
								|| ((mapping.getScaleConditionTo() > 0) && (sourceColumn
										.getScale() > mapping
										.getScaleConditionTo()))) {
							continue;
						}

						targetColumn.setDatatypeName(mapping
								.getTargetDatatypeName());

						if (mapping.getAutoIncrement() == 1) {
							targetColumn.setAutoIncrement(1);
							targetColumn.setDefaultValue("");
							targetColumn.setDefaultValueIsNull(1);
						}

						if (mapping.getUnsigned() == 1)
							targetColumn.getFlags().add("UNSIGNED");

						if (!mapping.getCharacterSet().equalsIgnoreCase(""))
							targetColumn.setCharacterSetName(mapping
									.getCharacterSet());

						if (!mapping.getCollation().equalsIgnoreCase(""))
							targetColumn.setCollationName(mapping
									.getCollation());

						if (mapping.getLength() > -1)
							targetColumn.setLength(mapping.getLength());

						if (mapping.getPrecision() != -1)
							targetColumn.setPrecision(mapping.getPrecision());

						if (mapping.getScale() != -1)
							targetColumn.setScale(mapping.getScale());
						break;
					}
				}
			}
		}

		// lookup the simple datatype and set it in the column
		int simpleDatatypeIndex = simpleDatatypes.getIndexOfName(targetColumn
				.getDatatypeName());
		if (simpleDatatypeIndex > -1)
			targetColumn
					.setSimpleType(simpleDatatypes.get(simpleDatatypeIndex));

		if ((targetColumn.getDatatypeName() == null)
				|| targetColumn.getDatatypeName().equals("")) {
			String msg = "The datatype " + sourceColumn.getDatatypeName()
					+ " cannot be migrated.";
			migUtils.addMigrationLogEntry(migObj, sourceColumn, targetColumn,
					msg, MigrationUtils.logError);

			Grt.getInstance().addMsg(msg);
			Grt.getInstance().flushMessages();

			/*
			 * migUtils.addMigrationLogEntry(migObj, sourceColumn.getOwner(),
			 * targetColumn.getOwner(), "A datatype cannot be migrated.",
			 * MigrationUtils.logWarning);
			 */

			targetColumn.setSimpleType(simpleDatatypes.get(0));
			targetColumn
					.setDatatypeName(targetColumn.getSimpleType().getName());
		}

		// convert TIMESTAMP DEFAULT NOW to TIMESTAMP CURRENT_TIMESTAMP
		if (targetColumn.getDatatypeName().equalsIgnoreCase("TIMESTAMP")
				&& targetColumn.getDefaultValue() != null
				&& (targetColumn.getDefaultValue().equalsIgnoreCase("NOW") || targetColumn
						.getDefaultValue().equalsIgnoreCase("'NOW'")))
			targetColumn.setDefaultValue("CURRENT_TIMESTAMP");

		// return new created, migrated object
		return targetColumn;
	}

	/**
	 * Migrates a table index to MySQL
	 * 
	 * @param sourceIndex
	 *            the object to migrate
	 * @param migrationParams
	 *            parameters used to define the target object
	 * 
	 * @return returns MySQL table object
	 */
	protected com.mysql.grt.db.mysql.Index migrateIndexToMysql(
			com.mysql.grt.db.migration.Migration migObj, Index sourceIndex,
			GrtStringHashMap migrationParams, GrtObject parent) {

		// create index
		com.mysql.grt.db.mysql.Index targetIndex;
		targetIndex = new com.mysql.grt.db.mysql.Index(parent);

		// log creation of target object
		migUtils.addMigrationLogEntry(migObj, sourceIndex, targetIndex);

		// do migration
		targetIndex.setName(migUtils.getTargetName(migrationParams,
				migrateIdentifier(sourceIndex.getName())));
		if (targetIndex.getName().length() > 32)
			targetIndex.setName(targetIndex.getName().substring(0, 31));
		
		targetIndex.setOldName(sourceIndex.getName());
		targetIndex.setUnique(sourceIndex.getUnique());

		// deal with migrationParams, forcedIndexLength
		int forcedIndexLength;

		try {
			forcedIndexLength = Integer.parseInt(migrationParams
					.get("forcedIndexLength"));
		} catch (NumberFormatException e) {
			forcedIndexLength = 0;
		}

		// migrate index columns
		Table sourceTable = (Table) sourceIndex.getOwner();

		for (int i = 0; i < sourceIndex.getColumns().size(); i++) {
			IndexColumn sourceIndexColumn = sourceIndex.getColumns().get(i);

			// get original source column
			int columnIndex = sourceTable.getColumns().getIndexOfName(
					sourceIndexColumn.getName());

			if (columnIndex == -1)
				continue;

			Column sourceColumn = sourceTable.getColumns().get(columnIndex);

			// create new IndexColumn
			IndexColumn targetColumn = new IndexColumn(targetIndex);

			Column referedColumn = (Column) (migUtils
					.findTargetObject(sourceColumn));

			targetColumn.setReferedColumn(referedColumn);

			targetIndex.getColumns().add(targetColumn);

			// check index length
			if (forcedIndexLength > 0)
				targetColumn.setColumnLength(forcedIndexLength);
			else {
				int indexLength = sourceIndexColumn.getColumnLength();

				SimpleDatatype datatype = referedColumn.getSimpleType();
				if (datatype != null) {
					DatatypeGroup group = datatype.getGroup();

					if (group != null) {
						String groupName = group.getName();

						// only strings, text or blob have a index column length
						if ((indexLength > 0)
								&& (!(groupName.equals("string")
										|| groupName.equals("text") || groupName
										.equals("blob"))))
							indexLength = 0;

						// text and blob have to have a index column length
						if ((indexLength == 0)
								&& (groupName.equals("text") || groupName
										.equals("blob")))
							indexLength = 45;
					}
				}

				// limit max index length to 500 per index column,
				// so there can be at least 2 index columns without issues
				if (indexLength > 500)
					indexLength = 500;

				// finally check, if the indexLength is longer than the column's
				// length itself
				if ((referedColumn.getLength() > 0)
						&& (indexLength > referedColumn.getLength()))
					indexLength = referedColumn.getLength();

				targetColumn.setColumnLength(indexLength);
			}
		}

		// return new created, migrated object
		return targetIndex;
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
		targetForeignKey.setName(migUtils.getTargetName(migrationParams,
				migrateIdentifier(sourceForeignKey.getName())));
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
			Column sourceColumnRef = sourceForeignKey.getColumns().get(i);

			// get original source column
			Column sourceColumn = sourceTable.getColumns().get(
					sourceTable.getColumns().getIndexOfName(
							sourceColumnRef.getName()));

			Column targetColumn = (Column) (migUtils
					.findTargetObject(sourceColumn));

			targetForeignKey.getColumns().add(targetColumn);

		}

		// find target refered table
		Table refTable = (Table) sourceForeignKey.getReferedTable();
		if (refTable != null) {
			Table targetRefTable = (Table) (migUtils.findTargetObject(refTable));

			if (targetRefTable != null)
				targetForeignKey.setReferedTable(targetRefTable);
		}

		// migrate FK columns refs
		for (int i = 0; i < sourceForeignKey.getColumns().size(); i++) {
			Column sourceColumnRef = sourceForeignKey.getReferedColumns()
					.get(i);

			Column targetColumn = (Column) (migUtils
					.findTargetObject(sourceColumnRef));

			if (targetColumn != null)
				targetForeignKey.getReferedColumns().add(targetColumn);
		}

		for (int i = 0; i < sourceForeignKey.getReferedColumnNames().size(); i++) {
			targetForeignKey.getReferedColumnNames().add(
					migrateIdentifier(sourceForeignKey.getReferedColumnNames()
							.get(i)));
		}

		// return new created, migrated object
		return targetForeignKey;
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

		// copy SQL
		String query = sourceView.getQueryExpression().trim();

		// detect WITH CHECK OPTION in sql and remove it
		if (query.toUpperCase().endsWith("WITH CHECK OPTION"))
			query = query.substring(0, query.length() - 17).trim();

		targetView.setQueryExpression(query);

		// copy column names
		for (int i = 0; i < sourceView.getColumns().size(); i++) {
			targetView.getColumns().add(sourceView.getColumns().get(i));
		}

		// comment SQL out for the moment
		targetView.setCommentedOut(1);

		migUtils.addMigrationLogEntry(migObj, sourceView, targetView,
				"The generated SQL has to be checked manually.",
				MigrationUtils.logWarning);

		// return new created, migrated object
		return targetView;
	}

	/**
	 * Migrates a Routine to a MySQL procedure
	 * 
	 * @param sourceProc
	 *            the object to migrate
	 * @param migrationParams
	 *            parameters used to define the target object
	 * 
	 * @return returns MySQL Routine object
	 */
	protected com.mysql.grt.db.mysql.Routine migrateRoutineToMysql(
			com.mysql.grt.db.migration.Migration migObj, Routine sourceProc,
			GrtStringHashMap migrationParams, GrtObject parent) {

		com.mysql.grt.db.mysql.Routine targetProc;
		targetProc = new com.mysql.grt.db.mysql.Routine(parent);

		// do migration
		targetProc.setName(migUtils.getTargetName(migrationParams,
				migrateIdentifier(sourceProc.getName())));
		targetProc.setOldName(sourceProc.getName());
		targetProc.setRoutineType(sourceProc.getRoutineType());

		// copy SQL
		if (sourceProc.getRoutineCode() != null)
			targetProc.setRoutineCode(sourceProc.getRoutineCode().trim());

		// comment SQL out for the moment
		targetProc.setCommentedOut(1);

		migUtils.addMigrationLogEntry(migObj, sourceProc, targetProc,
				"The generated SQL has to be checked manually.",
				MigrationUtils.logWarning);

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
		method.setModuleName("MigrationGeneric");
		method.setCaption("Generic");
		method.setDesc("Generic method to migrate a schema to MySQL.");
		method.setSourceStructName("db.Schema");
		method.setTargetPackageName("db.mysql");
		method.setRating(0);

		addMigrateSchemaToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates the predefined migration parameters for
	 * getMigrateSchemaToMysqlInfo
	 * 
	 * @param method
	 *            the method to which the parameters should be added
	 * 
	 */
	protected void addMigrateSchemaToMysqlInfoParameters(Method method) {

		// specify the parameters the method understands
		GrtStringHashMap paramNames = new GrtStringHashMap();
		method.setParams(paramNames);
		paramNames.add("charset", "latin1");
		paramNames.add("collation", "latin1_swedish_ci");

		// create a list of parameter groups the user can choose from
		ParameterGroupList paramGroupList = new ParameterGroupList();
		method.setParamGroups(paramGroupList);

		// add parameter group
		ParameterGroup paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Latin1");
		paramGroup
				.setDesc("Use this parameter group to use Latin1 as default character set for the schema.");
		GrtStringHashMap params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("charset", "latin1");
		params.add("collation", "latin1_swedish_ci");

		// add parameter group
		paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Multilanguage");
		paramGroup
				.setDesc("Use this parameter group to use UTF8 as default character set for the schema.");
		params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("charset", "utf8");
		params.add("collation", "utf8_general_ci");
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
		method.setModuleName("MigrationGeneric");
		method.setCaption("Generic");
		method.setDesc("Generic method to migrate a table to MySQL.");
		method.setSourceStructName("db.Table");
		method.setTargetPackageName("db.mysql");
		method.setRating(0);

		addMigrateTableToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates the predefined migration parameters for
	 * getMigrateTableToMysqlInfo
	 * 
	 * @param method
	 *            the method to which the parameters should be added
	 * 
	 */
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
		params.add("addAutoincrement", "yes");

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
		params.add("addAutoincrement", "yes");

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
		params.add("addAutoincrement", "yes");
	}

	/**
	 * Generates information about the Routine to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateColumnToMysqlInfo() {

		// create method description
		Method method = new Method(null);
		method.setName("migrateColumnToMysql");
		method.setModuleName("MigrationGeneric");
		method.setCaption("Generic");
		method.setDesc("Generic method to migrate a column to MySQL.");
		method.setSourceStructName("db.Column");
		method.setTargetPackageName("db.mysql");
		method.setRating(0);

		addMigrateColumnToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates the predefined migration parameters for
	 * getMigrateColumnToMysqlInfo
	 * 
	 * @param method
	 *            the method to which the parameters should be added
	 * 
	 */
	protected void addMigrateColumnToMysqlInfoParameters(Method method) {

		// specify the parameters the method understands
		GrtStringHashMap paramNames = new GrtStringHashMap();
		method.setParams(paramNames);
		paramNames.add("forceDatatypeName", "");
		paramNames.add("forceLength", "no");
		paramNames.add("forceLengthValue", "");
		paramNames.add("forceDecimalDigits", "no");
		paramNames.add("forceScaleValue", "");
		paramNames.add("forcePrecisionValue", "");
		paramNames.add("autoDecimalDigits", "no");

		// create a list of parameter groups the user can choose from
		ParameterGroupList paramGroupList = new ParameterGroupList();
		method.setParamGroups(paramGroupList);

		// add parameter group
		ParameterGroup paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Standard parameters");
		paramGroup.setDesc("This is the standard parameter group "
				+ "to migrate colums.");
		GrtStringHashMap params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("forceDatatypeName", "");
		params.add("forceLength", "no");
		params.add("forceLengthValue", "");
		params.add("forceDecimalDigits", "no");
		params.add("forceScaleValue", "");
		params.add("forcePrecisionValue", "");
		params.add("autoDecimalDigits", "no");
	}

	/**
	 * Generates information about the Routine to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateIndexToMysqlInfo() {

		// create method description
		Method method = new Method(null);
		method.setName("migrateIndexToMysql");
		method.setModuleName("MigrationGeneric");
		method.setCaption("Generic");
		method.setDesc("Generic method to migrate an index to MySQL.");
		method.setSourceStructName("db.Index");
		method.setTargetPackageName("db.mysql");
		method.setRating(0);

		addMigrateIndexToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates the predefined migration parameters for
	 * getMigrateForeignKeyToMysqlInfo
	 * 
	 * @param method
	 *            the method to which the parameters should be added
	 * 
	 */
	protected void addMigrateIndexToMysqlInfoParameters(Method method) {

		// specify the parameters the method understands
		GrtStringHashMap paramNames = new GrtStringHashMap();
		method.setParams(paramNames);
		paramNames.add("forcedIndexLength", "0");

		// create a list of parameter groups the user can choose from
		ParameterGroupList paramGroupList = new ParameterGroupList();
		method.setParamGroups(paramGroupList);

		// add parameter group
		ParameterGroup paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Standard parameters");
		paramGroup.setDesc("This is the standard parameter group "
				+ "to migrate indices.");
		GrtStringHashMap params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("forcedIndexLength", "0");

		paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Fast inserts");
		paramGroup.setDesc("Use this paremeter group to "
				+ "limit the length of an index on a VARCHAR or "
				+ "TEXT column to the first 10 characters. Note that "
				+ "this might slow down SELECT commands.");
		params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("forcedIndexLength", "10");
	}

	/**
	 * Generates information about the Routine to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateForeignKeyToMysqlInfo() {

		// create method description
		Method method = new Method(null);
		method.setName("migrateForeignKeyToMysql");
		method.setModuleName("MigrationGeneric");
		method.setCaption("Generic");
		method.setDesc("Generic method to migrate a foreign key to MySQL.");
		method.setSourceStructName("db.ForeignKey");
		method.setTargetPackageName("db.mysql");
		method.setRating(0);

		addMigrateForeignKeyToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates the predefined migration parameters for
	 * getMigrateForeignKeyToMysqlInfo
	 * 
	 * @param method
	 *            the method to which the parameters should be added
	 * 
	 */
	protected void addMigrateForeignKeyToMysqlInfoParameters(Method method) {

		// specify the parameters the method understands
		GrtStringHashMap paramNames = new GrtStringHashMap();
		method.setParams(paramNames);
		paramNames.add("overrideRules", "no");
		paramNames.add("defaultDeleteRule", "NO ACTION");
		paramNames.add("defaultUpdateRule", "NO ACTION");

		// create a list of parameter groups the user can choose from
		ParameterGroupList paramGroupList = new ParameterGroupList();
		method.setParamGroups(paramGroupList);

		// add parameter group
		ParameterGroup paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Standard parameters");
		paramGroup.setDesc("This is the standard parameter group "
				+ "to migrate foreign keys.");
		GrtStringHashMap params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("overrideRules", "no");
		params.add("defaultDeleteRule", "NO ACTION");
		params.add("defaultUpdateRule", "NO ACTION");
	}

	/**
	 * Generates information about the Table to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateViewToMysqlInfo() {

		// create method description
		Method method = new Method(null);
		method.setName("migrateViewToMysql");
		method.setModuleName("MigrationGeneric");
		method.setCaption("Generic");
		method.setDesc("Generic method to migrate a view to MySQL.");
		method.setSourceStructName("db.View");
		method.setTargetPackageName("db.mysql");
		method.setRating(0);

		addMigrateViewToMysqlInfoParameters(method);

		return method;
	}

	/**
	 * Generates the predefined migration parameters for
	 * getMigrateViewToMysqlInfo
	 * 
	 * @param method
	 *            the method to which the parameters should be added
	 * 
	 */
	protected void addMigrateViewToMysqlInfoParameters(Method method) {

		// specify the parameters the method understands
		GrtStringHashMap paramNames = new GrtStringHashMap();
		method.setParams(paramNames);
		paramNames.add("forceCheckOption", "no");

		// create a list of parameter groups the user can choose from
		ParameterGroupList paramGroupList = new ParameterGroupList();
		method.setParamGroups(paramGroupList);

		// add parameter group
		ParameterGroup paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Standard parameters");
		paramGroup.setDesc("This is the standard parameter group "
				+ "to migrate views.");
		GrtStringHashMap params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("forceCheckOption", "no");
	}

	/**
	 * Generates information about the View to MySQL migration method
	 * 
	 * @return returns a Method with predefined migration parameters
	 */
	private Method getMigrateRoutineToMysqlInfo() {

		// create migrateTable method
		Method method = new Method(null);
		method.setName("migrateRoutineToMysql");
		method.setModuleName("MigrationGeneric");
		method.setCaption("Generic");
		method.setDesc("Generic method to migrate a " + " routine to MySQL.");
		method.setSourceStructName("db.Routine");
		method.setTargetPackageName("db.mysql");
		method.setRating(0);

		addMigrateRoutineToMysqlInfoParameters(method);

		return method;
	}

	protected void addMigrateRoutineToMysqlInfoParameters(Method method) {

		// specify the parameters the method understands
		GrtStringHashMap paramNames = new GrtStringHashMap();
		method.setParams(paramNames);
		paramNames.add("Skip", "no");

		// create a list of parameter groups the user can choose from
		ParameterGroupList paramGroupList = new ParameterGroupList();
		method.setParamGroups(paramGroupList);

		// add parameter group
		ParameterGroup paramGroup = new ParameterGroup(method);
		paramGroupList.add(paramGroup);
		paramGroup.setName("Standard parameters");
		paramGroup.setDesc("This is the standard parameter group "
				+ "to migrate routines.");
		GrtStringHashMap params = new GrtStringHashMap();
		paramGroup.setParams(params);
		params.add("Skip", "no");
	}

	protected void buildSimpleDatatypes(Catalog catalog,
			com.mysql.grt.db.mgmt.Rdbms targetRdbms) {

		com.mysql.grt.db.SimpleDatatypeList rdbmsDatatypeList = targetRdbms
				.getSimpleDatatypes();

		for (int i = 0; i < rdbmsDatatypeList.size(); i++) {
			catalog.getSimpleDatatypes().add(rdbmsDatatypeList.get(i));
		}
	}

	private static String foreignKeysDisable = "SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, "
			+ "FOREIGN_KEY_CHECKS=0;";

	private static String foreignKeysReEnable = "SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;";

	protected void doDataBulkTransfer(
			com.mysql.grt.db.mgmt.Connection sourceDbConn,
			Catalog sourceCatalog,
			com.mysql.grt.db.mgmt.Connection targetDbConn,
			Catalog targetCatalog, GrtStringHashMap params,
			com.mysql.grt.base.ObjectLogList logList) throws Exception {

		OutputStreamWriter outputStreamWriter = null;

		// Check options
		boolean doOnlineTransferData = true;
		boolean doCreateScript = false;

		String optionString = params.get("TransferData");
		if ((optionString != null) && (optionString.equalsIgnoreCase("no")))
			doOnlineTransferData = false;

		optionString = params.get("CreateScript");
		if ((optionString != null) && (optionString.equalsIgnoreCase("yes")))
			doCreateScript = true;

		if (!doOnlineTransferData && !doCreateScript) {
			Grt.getInstance().addMsg(
					"Neither online data transfer "
							+ "nor the creation of a script "
							+ "file was selected. Skipping "
							+ "bulk data transfer.");
			return;
		}

		if (doOnlineTransferData) {
			Grt.getInstance().addMsg("Starting online data bulk transfer ...");
		}

		if (doCreateScript) {
			String scriptFileName = params.get("ScriptFileName");

			if ((scriptFileName != null) && (!scriptFileName.equals(""))) {
				Grt.getInstance().addMsg("Opening output script file ...");

				File scriptFile = new File(scriptFileName);

				FileOutputStream outputStream = new FileOutputStream(scriptFile);

				outputStreamWriter = new OutputStreamWriter(outputStream,
						"UTF-8");

				outputStreamWriter
						.write("-- ----------------------------------------------------------------------\r\n"
								+ "-- SQL data bulk transfer script generated by the MySQL Migration Toolkit\r\n"
								+ "-- ----------------------------------------------------------------------\r\n\r\n");

				outputStreamWriter.write("-- Disable foreign key checks\r\n"
						+ foreignKeysDisable + "\r\n\r\n");
			}
		}

		Grt.getInstance().addMsg("Initializing source JDBC driver ...");
		Connection sourceConn = ReverseEngineeringGeneric
				.establishConnection(sourceDbConn);

		Connection targetConn = null;

		if (doOnlineTransferData) {
			Grt.getInstance().addMsg("Initializing target JDBC driver ...");
			targetConn = ReverseEngineeringGeneric
					.establishConnection(targetDbConn);

			// deactivate autocommit for target connection
			targetConn.setAutoCommit(false);

			// disable FK checks
			Statement targetStmt = targetConn.createStatement();
			targetStmt.execute(foreignKeysDisable);
			targetStmt.close();
		}

		// handle all schemata
		for (int i = 0; i < sourceCatalog.getSchemata().size(); i++) {
			Schema sourceSchema = sourceCatalog.getSchemata().get(i);
			Schema targetSchema = null;

			Grt.getInstance().addMsg(
					"Processing schema " + sourceSchema.getName() + " ...");
			Grt.getInstance().flushMessages();

			// search target column based on old name
			for (int j = 0; j < targetCatalog.getSchemata().size(); j++) {
				if (targetCatalog.getSchemata().get(j).getOldName().equals(
						sourceSchema.getName())) {
					targetSchema = targetCatalog.getSchemata().get(j);
					break;
				}
			}

			if (targetSchema == null) {
				Grt.getInstance().addErr(
						"The source schema " + sourceSchema.getName()
								+ " is skipped because the target "
								+ "schema is not found.");
				continue;
			}

			// loop over all tables
			for (int j = 0; j < sourceSchema.getTables().size(); j++) {
				Table sourceTable = sourceSchema.getTables().get(j);
				Table targetTable = null;

				// search target column based on old name
				for (int k = 0; k < targetSchema.getTables().size(); k++) {
					if (targetSchema.getTables().get(k).getOldName().equals(
							sourceTable.getName())) {
						targetTable = targetSchema.getTables().get(k);
						break;
					}
				}

				if (targetTable == null) {
					Grt.getInstance().addMsg(
							"The source table " + sourceTable.getName()
									+ " is skipped because the target "
									+ "table is not found.");
					continue;
				}

				if (targetTable.getClass() == com.mysql.grt.db.mysql.Table.class)
					if (doDataBulkTransferTableToMysql(sourceDbConn,
							sourceConn, sourceTable, targetDbConn, targetConn,
							targetTable, params, outputStreamWriter, logList) != 0)
						break;
			}

			// Hide Progress Bar
			Grt.getInstance().addProgress("", -1);
			Grt.getInstance().flushMessages();
		}

		if (doOnlineTransferData) {
			// commit inserted data
			targetConn.commit();

			// go back to autocommit
			targetConn.setAutoCommit(true);

			// re-enable FK checks
			Statement targetStmt = targetConn.createStatement();
			targetStmt.execute(foreignKeysReEnable);
			targetStmt.close();
		}

		if (outputStreamWriter != null) {
			outputStreamWriter
					.write("-- Re-enable foreign key checks\r\n"
							+ foreignKeysReEnable + "\r\n\r\n"
							+ "-- End of script\r\n");
			outputStreamWriter.close();
		}

		Grt.getInstance().addMsg("Data bulk transfer finished.");
	}

	protected int doDataBulkTransferTableToMysql(
			com.mysql.grt.db.mgmt.Connection sourceDbConn,
			Connection sourceConn, Table sourceTable,
			com.mysql.grt.db.mgmt.Connection targetDbConn,
			Connection targetConn, Table targetTable, GrtStringHashMap params,
			OutputStreamWriter outputStreamWriter,
			com.mysql.grt.base.ObjectLogList logList) {

		// Check options
		boolean doOnlineTransferData = true;
		boolean doExcludeBlob = false;
		boolean doExcludeSourceSchemaName = false;
		boolean doOverrideBlobLimit = false;
		boolean doBlobStreaming = false;
		boolean doRightTrimForText = false;
		boolean doSplittedSelects = false;
		boolean doNotQuoteSourceIds = false;
		int doLimitRows = -1;

		String optionString = params.get("TransferData");
		if ((optionString != null) && (optionString.equalsIgnoreCase("no")))
			doOnlineTransferData = false;

		optionString = params.get("ExcludeBlob");
		if ((optionString != null) && (optionString.equalsIgnoreCase("yes")))
			doExcludeBlob = true;

		optionString = params.get("excludeSourceSchemaName");
		if ((optionString != null) && (optionString.equalsIgnoreCase("yes")))
			doExcludeSourceSchemaName = true;

		optionString = params.get("OverrideBlobLimit");
		if ((optionString != null) && (optionString.equalsIgnoreCase("yes")))
			doOverrideBlobLimit = true;

		optionString = params.get("BlobStreaming");
		if ((optionString != null) && (optionString.equalsIgnoreCase("yes")))
			doBlobStreaming = true;

		optionString = params.get("RightTrimForText");
		if ((optionString != null) && (optionString.equalsIgnoreCase("yes")))
			doRightTrimForText = true;

		optionString = params.get("MaxRowsNum");
		if ((optionString != null) && !optionString.equalsIgnoreCase("")
				&& !optionString.equalsIgnoreCase("-1"))
			doLimitRows = Integer.parseInt(optionString);
		
		optionString = params.get("DoNotQuoteSourceIds");
		if ((optionString != null) && (optionString.equalsIgnoreCase("yes")))
			doNotQuoteSourceIds = true;

		String sourceQuoteChar;
		String targetQuoteChar;
		String countSelect = "SELECT count(*) AS total_num FROM ";
		StringBuffer select = new StringBuffer("SELECT ");
		StringBuffer insert;
		StringBuffer insertHeader = new StringBuffer("INSERT INTO ");
		int columnCount = 0;

		ObjectLog logObj = new ObjectLog(null);
		logObj.setLogObject(sourceTable);
		logObj.setRefObject(targetTable);

		try {
			// Allow to skip source quotations
			if (!doNotQuoteSourceIds)
				sourceQuoteChar = sourceConn.getMetaData()
					.getIdentifierQuoteString();
			else
				sourceQuoteChar = "";
			
			if (doOnlineTransferData) {
				targetQuoteChar = targetConn.getMetaData()
						.getIdentifierQuoteString();
			} else {
				targetQuoteChar = params.get("targetQuoteChar");
				if ((targetQuoteChar == null) || (targetQuoteChar.equals("")))
					targetQuoteChar = "`";
			}
				
			// build column list
			StringBuffer selectCols = new StringBuffer();
			StringBuffer insertCols = new StringBuffer();
			for (int i = 0; i < sourceTable.getColumns().size(); i++) {
				Column sourceColumn = sourceTable.getColumns().get(i);
				Column targetColumn = null;

				// search target column based on old name
				for (int j = 0; j < targetTable.getColumns().size(); j++) {
					if (targetTable.getColumns().get(j).getOldName().equals(
							sourceColumn.getName())) {
						targetColumn = targetTable.getColumns().get(j);
						break;
					}
				}

				// exclude BLOBs if the option was selected
				if ((doExcludeBlob)
						&& (sourceColumn.getDatatypeName()
								.equalsIgnoreCase("BLOB")))
					continue;

				// only transfer this column if there is a target column
				if (targetColumn != null) {
					if (selectCols.length() != 0) {
						selectCols.append(", ");
						insertCols.append(", ");
					}

					selectCols.append(sourceQuoteChar + sourceColumn.getName()
							+ sourceQuoteChar);

					insertCols.append(targetQuoteChar + targetColumn.getName()
							+ targetQuoteChar);

					columnCount++;
				}
			}

			select.append(selectCols);
			select.append(" FROM ");

			if (!doExcludeSourceSchemaName) {
				select.append(sourceQuoteChar
						+ sourceTable.getOwner().getName() + sourceQuoteChar
						+ ".");
				countSelect += sourceQuoteChar
						+ sourceTable.getOwner().getName() + sourceQuoteChar
						+ ".";
			}

			select.append(sourceQuoteChar + sourceTable.getName()
					+ sourceQuoteChar);
			countSelect += sourceQuoteChar + sourceTable.getName()
					+ sourceQuoteChar;

			insertHeader.append(targetQuoteChar
					+ targetTable.getOwner().getName() + targetQuoteChar + "."
					+ targetQuoteChar + targetTable.getName() + targetQuoteChar
					+ "(");
			insertHeader.append(insertCols);
			insertHeader.append(")\r\nVALUES ");

			// Get number of rows to transfer
			int rowCount = 0;
			int rowTotalCount = 0;
			int currentRowNumber = 0;
			int rowBlockOffset = 0;
			int rowBlockSize = 20000;
			Statement sourceStmt = sourceConn.createStatement();

			GrtMessage msg = Grt.getInstance().addMsg(
					"Getting the number of rows of table "
							+ sourceTable.getName());
			Grt.getInstance().addMsgDetail(msg, countSelect);
			Grt.getInstance().flushMessages();

			ResultSet rset = sourceStmt.executeQuery(countSelect);

			if (rset.next()) {
				rowTotalCount = rset.getInt(1);
				rowCount = rowTotalCount;
			}

			if (doLimitRows == -1 || doLimitRows > rowCount)
				doLimitRows = rowCount;

			// Starting the transfer
			msg = Grt.getInstance().addMsg(
					"Transfering data from table " + sourceTable.getName()
							+ " (" + String.valueOf(doLimitRows) + "/"
							+ String.valueOf(rowCount) + " rows)");
			Grt.getInstance().addMsgDetail(msg, select.toString());
			Grt.getInstance().flushMessages();

			rowCount = doLimitRows;

			Grt.getInstance().addProgress("Open source resultset.", 0);
			Grt.getInstance().flushMessages();

			if (sourceDbConn.getDriver().getOwner().getName().equalsIgnoreCase(
					"Mysql")
					&& (rowCount > rowBlockSize)) {
				doSplittedSelects = true;
				rset = sourceStmt.executeQuery(select.toString() + "\nLIMIT "
						+ rowBlockOffset + ", " + rowBlockSize);
			} else {
				rset = sourceStmt.executeQuery(select.toString());
			}
			ResultSetMetaData rsetMetaData = rset.getMetaData();

			// Remember column types
			int[] colTypes = new int[columnCount];
			for (int i = 0; i < columnCount; i++) {
				colTypes[i] = rsetMetaData.getColumnType(i + 1);
			}

			SimpleDateFormat dateFormat = new SimpleDateFormat(
					"yyyy-MM-dd HH:mm:ss");
			SimpleDateFormat timeFormat = new SimpleDateFormat("HH:mm:ss");

			Grt.getInstance().addProgress("Started row transfer", 0);
			Grt.getInstance().flushMessages();

			// loop for splitted selects
			do {
				insert = new StringBuffer();
				insert.append(insertHeader);
				boolean isFirstDataTuppel = true;
				int bufferedRows = 0;

				// if splitted selects are used, get next block after
				if (doSplittedSelects && (rset == null)) {
					rowBlockOffset += rowBlockSize;
					rset = sourceStmt
							.executeQuery(select.toString() + "\nLIMIT "
									+ rowBlockOffset + ", " + rowBlockSize);
				}

				// loop over all rows
				while (rset.next()) {
					currentRowNumber++;

					if (currentRowNumber > rowCount) {
						currentRowNumber = rowCount;
						break;
					}

					if ((currentRowNumber % 100 == 0) && (rowCount > 0)) {
						Grt.getInstance().addProgress(
								currentRowNumber + " rows transfered.",
								(currentRowNumber * 100) / rowTotalCount);

						if (Grt.getInstance().flushMessages() != 0) {
							Grt.getInstance().addMsg(
									"Bulk transfer canceled by user.");
							return 1;
						}
					}

					// check if we need to add a ,
					if (isFirstDataTuppel) {
						insert = insert.append('(');
						isFirstDataTuppel = false;
					} else
						insert = insert.append(",\r\n  (");

					// add the column data to the insert
					for (int i = 0; i < columnCount; i++) {
						if (i > 0)
							insert.append(", ");

						String columnTypeName = rsetMetaData
								.getColumnTypeName(i + 1);

						if ((colTypes[i] == java.sql.Types.BIGINT)
								|| (colTypes[i] == java.sql.Types.INTEGER)
								|| (colTypes[i] == java.sql.Types.SMALLINT)
								|| (colTypes[i] == java.sql.Types.TINYINT)) {
							String value = rset.getString(i + 1);

							if (value != null)
								insert.append(value);
							else
								insert.append("NULL");
						} else if ((colTypes[i] == java.sql.Types.DECIMAL)
								|| (colTypes[i] == java.sql.Types.DOUBLE)
								|| (colTypes[i] == java.sql.Types.FLOAT)
								|| (colTypes[i] == java.sql.Types.NUMERIC)
								|| (colTypes[i] == java.sql.Types.REAL)) {
							BigDecimal value = rset.getBigDecimal(i + 1);

							if (value != null)
								insert.append(value.toString());
							else
								insert.append("NULL");
						} else if ((colTypes[i] == java.sql.Types.LONGVARCHAR)
								|| (colTypes[i] == java.sql.Types.VARCHAR)
								|| (colTypes[i] == java.sql.Types.CHAR)) {
							String value = rset.getString(i + 1);

							if (doRightTrimForText)
								value = value.replaceAll("\\s+$", "");

							if (value != null) {
								insert.append("'");
								insert.append(getEscapedString(value));
								insert.append("'");
							} else
								insert.append("NULL");
						} else if (colTypes[i] == java.sql.Types.DATE) {
							Timestamp value = rset.getTimestamp(i + 1);

							if (value != null) {
								insert.append("'");
								insert.append(dateFormat.format(value));
								insert.append("'");
							} else
								insert.append("NULL");
						} else if (colTypes[i] == java.sql.Types.TIME) {
							Time value = rset.getTime(i + 1);

							if (value != null) {
								insert.append("'");
								insert.append(timeFormat.format(value));
								insert.append("'");
							} else
								insert.append("NULL");
						} else if (colTypes[i] == java.sql.Types.BLOB
								|| columnTypeName.equalsIgnoreCase("LONG RAW")
								|| columnTypeName.equalsIgnoreCase("LONG")
								|| columnTypeName.equalsIgnoreCase("RAW")
								|| columnTypeName.equalsIgnoreCase("BINARY")
								|| columnTypeName.equalsIgnoreCase("VARBINARY")
								|| columnTypeName.equalsIgnoreCase("IMAGE")) {
							try {
								if (!doBlobStreaming) {
									InputStream blobStream = rset
											.getBinaryStream(i + 1);

									if (blobStream != null) {
										insert
												.append(getEscapedHexString(
														blobStream,
														doOverrideBlobLimit));
										blobStream.close();
									} else
										insert.append("NULL");
								} else {
									insert.append("NULL");
								}
							} catch (Exception e) {
								Grt
										.getInstance()
										.addErr(
												"The following error occured while "
														+ "transfering binary data from "
														+ sourceTable.getName()
														+ ", column "
														+ rsetMetaData
																.getColumnName(i + 1)
														+ "\n" + e.getMessage());

								insert.append("NULL");
							}
						} else if (columnTypeName.equals("BIT")) {
							if (sourceDbConn.getDriver().getName().equals(
									"Access")) {
								String value = rset.getString(i + 1);
								insert.append(String.valueOf(Integer
										.parseInt(value)
										* -1));
							} else
								insert.append(rset.getString(i + 1));
						} else if (colTypes[i] == java.sql.Types.DATE) {
							Date value = rset.getDate(i + 1);

							if (value != null) {
								insert.append("'");
								insert.append(dateFormat.format(value));
								insert.append("'");
							} else
								insert.append("NULL");
						} else if ((colTypes[i] == java.sql.Types.BOOLEAN)
								|| ((colTypes[i] == java.sql.Types.BIT) && columnTypeName
										.equals("TINYINT"))) {
							boolean value = rset.getBoolean(i + 1);

							if (value)
								insert.append("1");
							else
								insert.append("0");
						} else {
							// if the type is not of java.sql.Types, take a look
							// at
							// the type's name

							if (columnTypeName.equalsIgnoreCase("BIT")) {
								insert.append(rset.getString(i + 1));
							} else if (columnTypeName.equalsIgnoreCase("DATE")
									|| columnTypeName
											.equalsIgnoreCase("DATETIME")
									|| columnTypeName
											.equalsIgnoreCase("SMALLDATETIME")
									|| (columnTypeName.indexOf("TIMESTAMP") > -1)) {
								Timestamp value;

								try {
									value = rset.getTimestamp(i + 1);
								} catch (SQLException e) {
									try {
										value = Timestamp.valueOf(rset
												.getString(i + 1));
									} catch (Exception e1) {
										value = new Timestamp(0);
									}
								}

								if (value != null) {
									insert.append("'");
									insert.append(dateFormat.format(value));
									insert.append("'");
								} else
									insert.append("NULL");
							} else if (columnTypeName.equalsIgnoreCase("TEXT")
									|| columnTypeName.equalsIgnoreCase("NTEXT")) {
								String value = rset.getString(i + 1);

								if (value != null) {
									insert.append("'");
									insert.append(getEscapedString(value));
									insert.append("'");
								} else
									insert.append("NULL");
							} else if (columnTypeName.equalsIgnoreCase("CLOB")) {
								Reader reader = null;
								try {
									reader = rset.getCharacterStream(i + 1);
								} catch (SQLException e) {
									insert.append("''");
								}
								if (reader == null) {
									insert.append("NULL");
								} else {
									insert.append("'");

									char[] charbuf = new char[4096];
									for (int j = reader.read(charbuf); j > 0; j = reader
											.read(charbuf)) {
										insert.append(getEscapedString(charbuf,
												j));
									}

									insert.append("'");
								}
							} else if (columnTypeName.equalsIgnoreCase("TINYINT UNSIGNED")) {
								String value = rset.getString(i + 1);

								if (value != null)
									insert.append(value);
								else
									insert.append("NULL");
							} else
								insert.append("''");
						}
					}
					insert.append(')');

					bufferedRows++;

					if (insert.length() > 1024 * 800) {
						insert.append(";\r\n");

						String sql = insert.toString();
						insert = new StringBuffer();
						insert.append(insertHeader);
						isFirstDataTuppel = true;

						if (doOnlineTransferData) {
							// execute insert statments
							Statement targetStmt = targetConn.createStatement();
							try {
								targetStmt.setEscapeProcessing(false);
								targetStmt.execute(sql);
							} catch (Exception e) {
								// add error log entry
								ObjectLogEntry logEntry = new ObjectLogEntry(
										null);
								logEntry.setName(e.getMessage());
								logEntry.setEntryType(2);

								logObj.getEntries().add(logEntry);

								// remove the number of rows that could not be
								// transfered
								currentRowNumber -= bufferedRows;
								rowCount -= bufferedRows;
							}
							if (targetStmt != null)
								targetStmt.close();

							bufferedRows = 0;
						}

						if (outputStreamWriter != null)
							outputStreamWriter.write(sql);
					}
				}

				if (bufferedRows > 0) {
					insert.append(";\r\n");

					String sql = insert.toString();

					if (doOnlineTransferData) {
						// execute insert statments
						Statement targetStmt = targetConn.createStatement();
						try {
							targetStmt.setEscapeProcessing(false);
							targetStmt.execute(sql);
						} catch (Exception e) {
							// add error log entry
							ObjectLogEntry logEntry = new ObjectLogEntry(null);
							logEntry.setName(e.getMessage());
							logEntry.setEntryType(2);

							logObj.getEntries().add(logEntry);

							currentRowNumber -= bufferedRows;
						}
						if (targetStmt != null)
							targetStmt.close();
					}

					if (outputStreamWriter != null)
						outputStreamWriter.write(sql);
				}

				if ((rowCount > 0) && (outputStreamWriter != null))
					outputStreamWriter.write("\r\n");

				// if doSplittedSelects is turned on and there are still rows
				// to be transfered, go back to the beginning of the loop
				rset = null;
			} while (doSplittedSelects && currentRowNumber < rowCount);

			sourceStmt.close();

			Grt.getInstance().addMsgDetail(msg,
					Integer.toString(currentRowNumber) + " row(s) transfered.");

			// add log message entry with the number of transfered rows
			ObjectLogEntry logEntry = new ObjectLogEntry(null);
			logEntry.setName(Integer.toString(currentRowNumber)
					+ " row(s) transfered.");
			logEntry.setEntryType(0);
			logObj.getEntries().add(logEntry);

			// special handling for BLOB streaming
			if (doBlobStreaming) {
				// commit inserted data
				targetConn.commit();

				doDataBulkTransferTableBlobsToMysql(sourceDbConn, sourceConn,
						sourceTable, targetDbConn, targetConn, targetTable,
						params, outputStreamWriter, logList, currentRowNumber);
			}

		} catch (Exception e) {
			String msgString = "The following error occured while "
					+ "transfering data from " + sourceTable.getName() + "\n"
					+ e.getMessage();
			Grt.getInstance().addErr(msgString);

			// add log message entry with the error
			ObjectLogEntry logEntry = new ObjectLogEntry(null);
			logEntry.setName(msgString);
			logEntry.setEntryType(1);
			logObj.getEntries().add(logEntry);
		}

		logList.add(logObj);

		return 0;
	}

	static StringBuffer getEscapedString(String s) {
		StringBuffer b = new StringBuffer();

		if (s != null) {
			for (int i = 0; i < s.length(); i++) {
				char c = s.charAt(i);

				if (c == '"')
					b.append("\\\"");
				/*else if (c == '{')
					b.append("\\{");*/
				else if (c == '\'')
					b.append("\\'");
				else if (c == 0)
					b.append("\\0");
				else if (c == '\n')
					b.append("\\n");
				else if (c == '\r')
					b.append("\\r");
				else if (c == '\\')
					b.append("\\\\");
				else
					b.append(c);
			}
		}

		return b;
	}

	static StringBuffer getEscapedString(char[] charbuf, int len) {
		StringBuffer b = new StringBuffer();

		if (charbuf != null) {
			for (int i = 0; i < len; i++) {
				char c = charbuf[i];

				if (c == '"')
					b.append("\\\"");
				/*else if (c == '{')
					b.append("\\{");*/
				else if (c == '\'')
					b.append("\\'");
				else if (c == 0)
					b.append("\\0");
				else if (c == '\n')
					b.append("\\n");
				else if (c == '\r')
					b.append("\\r");
				else if (c == '\\')
					b.append("\\\\");
				else
					b.append(c);
			}
		}

		return b;
	}

	static StringBuffer getEscapedHexString(InputStream is,
			boolean doOverrideBlobLimit) throws Exception {
		StringBuffer b = new StringBuffer();
		byte buffer[] = new byte[1024];
		int length;
		int currentSize = 0;

		b.append("0x");

		while ((length = is.read(buffer, 0, 1024)) != -1) {
			currentSize += 1024;
			if ((currentSize > 1048576 * 4) && (!doOverrideBlobLimit)) {
				throw new RuntimeException("BLOB is larger than 4MB.");
			}

			for (int i = 0; i < length; i++) {
				if ((buffer[i] & 0xff) < 16)
					b.append('0');

				b.append(Integer.toHexString(buffer[i] & 0xff));
			}
		}

		return b;
	}

	protected int doDataBulkTransferTableBlobsToMysql(
			com.mysql.grt.db.mgmt.Connection sourceDbConn,
			Connection sourceConn, Table sourceTable,
			com.mysql.grt.db.mgmt.Connection targetDbConn,
			Connection targetConn, Table targetTable, GrtStringHashMap params,
			OutputStreamWriter outputStreamWriter,
			com.mysql.grt.base.ObjectLogList logList, int rowCount)
			throws SQLException {

		// there has to be a PK
		if (sourceTable.getPrimaryKey() == null)
			return 0;

		boolean doExcludeSourceSchemaName = false;
		String optionString;

		optionString = params.get("excludeSourceSchemaName");
		if ((optionString != null) && (optionString.equalsIgnoreCase("yes")))
			doExcludeSourceSchemaName = true;

		String sourceQuoteChar = sourceConn.getMetaData()
				.getIdentifierQuoteString();
		String targetQuoteChar = targetConn.getMetaData()
				.getIdentifierQuoteString();

		// build primary key column list
		String selectCols = "";
		String updateCols = "";
		String updatePkCols = "";
		int pkColCount = 0;
		for (int i = 0; i < sourceTable.getPrimaryKey().getColumns().size(); i++) {
			Column sourceColumn = sourceTable.getPrimaryKey().getColumns().get(
					i).getReferedColumn();
			Column targetColumn = null;

			// search target column based on old name
			for (int j = 0; j < targetTable.getColumns().size(); j++) {
				if (targetTable.getColumns().get(j).getOldName().equals(
						sourceColumn.getName())) {
					targetColumn = targetTable.getColumns().get(j);
					break;
				}
			}

			if (targetColumn != null) {
				if (!updatePkCols.equals("")) {
					selectCols += ", ";
					updatePkCols += " AND ";
				}

				selectCols += sourceQuoteChar + sourceColumn.getName()
						+ sourceQuoteChar;
				updatePkCols += targetQuoteChar + targetColumn.getName()
						+ targetQuoteChar + " = ? ";

				pkColCount++;
			}
		}

		if (pkColCount == 0)
			return 0;

		// build BLOB column list
		int columnCount = 0;
		for (int i = 0; i < sourceTable.getColumns().size(); i++) {
			Column sourceColumn = sourceTable.getColumns().get(i);
			Column targetColumn = null;

			// search target column based on old name
			for (int j = 0; j < targetTable.getColumns().size(); j++) {
				if (targetTable.getColumns().get(j).getOldName().equals(
						sourceColumn.getName())) {
					targetColumn = targetTable.getColumns().get(j);
					break;
				}
			}

			// only look at BLOBs
			String sourceDatatype = sourceColumn.getDatatypeName();
			if (!(sourceDatatype.equalsIgnoreCase("BLOB")
					|| sourceDatatype.equalsIgnoreCase("MEDIUMBLOB")
					|| sourceDatatype.equalsIgnoreCase("LONGBLOB")
					|| sourceDatatype.equalsIgnoreCase("TINYBLOB")
					|| sourceDatatype.equalsIgnoreCase("LONG RAW")
					|| sourceDatatype.equalsIgnoreCase("RAW")
					|| sourceDatatype.equalsIgnoreCase("LONG")
					|| sourceDatatype.equalsIgnoreCase("BINARY")
					|| sourceDatatype.equalsIgnoreCase("VARBINARY") || sourceDatatype
					.equalsIgnoreCase("IMAGE")))
				continue;

			// only transfer this column if there is a target column
			if (targetColumn != null) {
				if (!selectCols.equals(""))
					selectCols += ", ";

				selectCols += sourceQuoteChar + sourceColumn.getName()
						+ sourceQuoteChar;

				if (!updateCols.equals(""))
					updateCols += " AND ";

				updateCols += targetQuoteChar + targetColumn.getName()
						+ targetQuoteChar + " = ? ";

				columnCount++;
			}
		}

		if (columnCount == 0)
			return 0;

		String select = "SELECT " + selectCols + " FROM ";

		if (!doExcludeSourceSchemaName) {
			select += sourceQuoteChar + sourceTable.getOwner().getName()
					+ sourceQuoteChar + ".";
		}

		select += sourceQuoteChar + sourceTable.getName() + sourceQuoteChar;

		String update = "UPDATE " + targetQuoteChar
				+ targetTable.getOwner().getName() + targetQuoteChar + "."
				+ targetQuoteChar + targetTable.getName() + targetQuoteChar
				+ " SET " + updateCols + "\r\nWHERE " + updatePkCols;

		Grt.getInstance().addMsg("Starting online BLOB data bulk transfer ...");
		Grt.getInstance().addMsg(select);
		Grt.getInstance().addMsg(update);

		Grt.getInstance().addProgress("Started BLOB transfer", 0);
		Grt.getInstance().flushMessages();

		// use a special connection to enable BLOB streaming
		String jdbcConnectionString = "jdbc:mysql://"
				+ targetDbConn.getParameterValues().get("host") + ":"
				+ targetDbConn.getParameterValues().get("port") + "/?user="
				+ targetDbConn.getParameterValues().get("username")
				+ "&password="
				+ targetDbConn.getParameterValues().get("password")
				+ "&useStreamLengthsInPrepStmts=false";

		Connection blobTargetConn = DriverManager
				.getConnection(jdbcConnectionString);
		blobTargetConn.setAutoCommit(false);

		Statement sourceStmt = sourceConn.createStatement();
		ResultSet rset = sourceStmt.executeQuery(select);
		// ResultSetMetaData rsetMetaData = rset.getMetaData();

		int currentRowNumber = 0;
		int blobNumber = 0;
		while (rset.next()) {
			currentRowNumber++;

			PreparedStatement targetStmt = blobTargetConn
					.prepareStatement(update);

			// update progress every 10 rows
			if ((currentRowNumber % 10 == 0) && (rowCount > 0)) {
				Grt.getInstance()
						.addProgress(
								(currentRowNumber * columnCount)
										+ " blobs transfered.",
								(currentRowNumber * 100) / rowCount);

				if (Grt.getInstance().flushMessages() != 0) {
					Grt.getInstance().addMsg(
							"Bulk BLOB transfer canceled by user.");
					return 1;
				}
			}

			// set the BLOB columns
			for (int i = 0; i < columnCount; i++) {
				InputStream blobStream = rset.getBinaryStream(pkColCount + i
						+ 1);

				if (blobStream == null)
					targetStmt.setNull(i + 1, java.sql.Types.BLOB);
				else {
					targetStmt.setBinaryStream(i + 1, blobStream, 1);
					blobNumber++;
				}
			}

			// set the PK parameter
			for (int i = 0; i < pkColCount; i++) {
				targetStmt
						.setString(columnCount + i + 1, rset.getString(i + 1));
			}

			try {
				targetStmt.execute();
			} catch (Exception e) {
				Grt.getInstance()
						.addErr(
								"The following error occured while "
										+ "transfering BLOB data from "
										+ sourceTable.getName() + "\n"
										+ e.getMessage());
			}

			targetStmt.close();

			// commit after each row.
			blobTargetConn.commit();
		}
		sourceStmt.close();

		blobTargetConn.commit();
		blobTargetConn.close();

		Grt.getInstance().addMsg(
				"Online BLOB data bulk transfer finished, " + blobNumber
						+ " BLOB(s) transfered...");
		Grt.getInstance().flushMessages();

		return 0;
	}
}