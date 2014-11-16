package com.mysql.grt.modules;

import java.util.HashMap;
import java.util.regex.Pattern;
import java.util.List;
import java.util.ArrayList;
import com.mysql.grt.*;
import com.mysql.grt.base.*;
import com.mysql.grt.db.migration.*;

/**
 * GRT Migration Class
 * 
 * @author MikeZ
 * @version 1.0, 01/23/05
 * 
 */
public class MigrationUtils {

	private HashMap logObjects = new HashMap();

	private HashMap sourceTargetObjectMapping = new HashMap();

	private List ignoreList;

	protected final static int logNoText = 0;

	protected final static int logWarning = 1;

	protected final static int logError = 2;
 
	/**
	 * Checks if an object is on the ignore list
	 * 
	 * @param mig
	 *            the migration object
	 * @param sourceObject
	 *            the object that should be migrated
	 * @return the migrated object
	 */
	public boolean isOnIgnoreList(com.mysql.grt.db.migration.Migration migObj,
			GrtObject sourceObject) {
		// only instances of DatabaseObject can be on the ignore list
		// if they are not a Schema
		if ((sourceObject instanceof com.mysql.grt.db.DatabaseObject)
				&& !(sourceObject instanceof com.mysql.grt.db.Schema)) {

			// if the ignore list was not needed till now
			// build it
			if (ignoreList == null) {
				ignoreList = new ArrayList();

				for (int i = 0; i < migObj.getIgnoreList().size(); i++) {
					String ignoreString = (String) migObj.getIgnoreList()
							.getObject(i);
					
					ignoreString = Grt.replace(ignoreString, "$", "\\$");

					ignoreString = ignoreString.replaceAll("\\\\", "\\\\")
							.replaceAll("\\.",
									"\\\\.").replaceAll("\\*", ".*")
							.replaceAll("\\?", ".");

					ignoreList.add(Pattern.compile(ignoreString));
				}
			}

			// the format of objectName is, e.g. db.oracle.Table:SCOTT.emp
			String objectName = sourceObject.getClass().getName().substring(14)
					+ ":" + sourceObject.getOwner().getName() + "."
					+ sourceObject.getName();

			// check if that object is on the ignore list
			for (int i = 0; i < ignoreList.size(); i++) {
				Pattern p = (Pattern) ignoreList.get(i);

				// if this is a match, the object is on the ignore list
				// so null is returned
				if (p.matcher(objectName).matches())
					return true;
			}
		}
		return false;
	}

	/**
	 * Migrates an object based on the mappings defined in the migration object.
	 * If no explicit mapping is defined, the object is migrated using the
	 * default migration method for the source object's class
	 * 
	 * @param callerObject
	 *            the object that is calling this function
	 * @param mig
	 *            the migration object
	 * @param sourceObject
	 *            the object that should be migrated
	 * @param parent
	 *            the object will become the parent of the migrated object
	 * @return the migrated object
	 */
	public GrtObject migrateObject(Object callerObject,
			com.mysql.grt.db.migration.Migration migObj,
			GrtObject sourceObject, GrtObject parent) {

		if (isOnIgnoreList(migObj, sourceObject))
			return null;

		// check if we have a special mapping for this item
		MappingList mappings = migObj.getMappingDefinitions();

		if (mappings != null) {
			for (int i = 0; i < mappings.size(); i++) {
				Mapping mapping = mappings.get(i);

				if (mapping.getSourceObject().get_id().equals(
						sourceObject.get_id())) {

					String sourceStructName = Grt.GrtPackagePrefix
							+ mapping.getSourceStructName();

					return callMigrationMethod(callerObject, migObj, mapping
							.getModuleName(), mapping.getMethodName(),
							sourceStructName, sourceObject,
							mapping.getParams(), parent);
				}
			}
		}

		// if it was not found, use default mapping if there is any
		MappingList defaultMappings = migObj.getMappingDefaults();

		if (defaultMappings != null) {
			for (int i = 0; i < defaultMappings.size(); i++) {
				Mapping mapping = defaultMappings.get(i);

				try {
					Class mappingSourceClass = Class
							.forName(Grt.GrtPackagePrefix
									+ mapping.getSourceStructName());

					// if this method returns the correct class
					if (mappingSourceClass.isInstance(sourceObject)) {
						return callMigrationMethod(callerObject, migObj,
								mapping.getModuleName(), mapping
										.getMethodName(), mappingSourceClass
										.getName(), sourceObject, mapping
										.getParams(), parent);
					}
				} catch (ClassNotFoundException e) {
					// if the class is not found do not migrate
				}
			}
		}

		// now check the complete method list to see if there is a appropriate
		// method to migrate the source object. If there are several, take the
		// highes rated as it will be the best
		MethodList methods = migObj.getMigrationMethods();

		if (methods != null) {
			com.mysql.grt.db.migration.Method bestMethod = null;
			Class bestMethodSourceClass = null;

			for (int i = 0; i < methods.size(); i++) {
				com.mysql.grt.db.migration.Method method = methods.get(i);

				try {
					Class mappingSourceClass = Class
							.forName(Grt.GrtPackagePrefix
									+ method.getSourceStructName());

					// if this method returns the correct class
					// and we do not have any other method yet or
					// the method just found has a higher rating than the
					// one we had before the method is stored as to best one
					// so far
					if ((mappingSourceClass.isInstance(sourceObject))
							&& ((bestMethod == null) || (method.getRating() > bestMethod
									.getRating()))) {
						bestMethod = method;
						bestMethodSourceClass = mappingSourceClass;
					}
				} catch (ClassNotFoundException e) {
					// if the class is not found, ignore it
				}
			}

			try {
				if ((bestMethod != null) && (bestMethodSourceClass != null))
					return callMigrationMethod(callerObject, migObj, bestMethod
							.getModuleName(), bestMethod.getName(),
							bestMethodSourceClass.getName(), sourceObject,
							bestMethod.getParams(), parent);
			} catch (Exception e) {
				addMigrationLogEntry(
						migObj,
						sourceObject,
						null,
						"An error occured during the call of the "
								+ bestMethod.getName()
								+ " function to migrate an object from the class "
								+ sourceObject.getClass().getName() + ".",
						logError);
			}
		}

		// if no mapping was found at all, set make an entry in to the log and
		// return null
		addMigrationLogEntry(migObj, sourceObject, null,
				"There is no method defined to migration an object of the type "
						+ sourceObject.getClass().getName() + ".", logError);

		return null;
	}

	/**
	 * Calls a migration method base on the module and method name
	 * 
	 * @param moduleName
	 *            the module the method belongs to, e.g. MigrationOracle
	 * @param methodName
	 *            the name of the method
	 * @param sourceObject
	 *            the object to migrate
	 * @param params
	 *            the parameters that should be used to migrate the source
	 *            object
	 * @return the migrated object
	 */
	public GrtObject callMigrationMethod(Object callerObject,
			com.mysql.grt.db.migration.Migration migObj, String moduleName,
			String methodName, String sourceObjectClassName,
			GrtObject sourceObject, GrtStringHashMap params, GrtObject parent) {
		try {
			// find the module's class
			Class moduleClass = Class.forName(Grt.GrtModulePackagePrefix
					+ moduleName);

			// prepare the method's arguments
			Class[] arguments = new Class[] {
					com.mysql.grt.db.migration.Migration.class,
					Class.forName(sourceObjectClassName),
					GrtStringHashMap.class, GrtObject.class };

			// find the method
			java.lang.reflect.Method method = moduleClass.getDeclaredMethod(
					methodName, arguments);

			// invoke migration method
			return (GrtObject) method.invoke(callerObject, new Object[] {
					migObj, sourceObject, params, parent });
		} catch (Exception e) {
			String name = sourceObject.getName();
			if (name == null)
				name = "<NULL>";
			
			addMigrationLogEntry(migObj, sourceObject, null,
					"An exception occured when the migration method was invoked for "
							+ name + " (" + e.getMessage()
							+ ").", logError);
			return null;
		}
	}

	public void addMigrationLogEntry(
			com.mysql.grt.db.migration.Migration migObj, GrtObject sourceObj,
			GrtObject targetObj) {
		addMigrationLogEntry(migObj, sourceObj, targetObj, null, logNoText);
	}

	public void addMigrationLogEntry(
			com.mysql.grt.db.migration.Migration migObj, GrtObject sourceObj,
			GrtObject targetObj, String message, int messageType) {

		// Add sourceObj and targetObj to sourceTargetObjectMapping
		// so it is faster to find the sourceObj to a given targetObj
		if ((sourceObj != null) && (targetObj != null))
			sourceTargetObjectMapping.put(sourceObj.get_id(), targetObj
					.get_id());

		// try to find existing log entry for this object
		ObjectLog objectLog = (ObjectLog) logObjects.get(sourceObj);

		// if no entry is found add a new one
		if (objectLog == null) {
			objectLog = new ObjectLog(null);
			objectLog.setLogObject(sourceObj);

			if (targetObj != null)
				objectLog.setRefObject(targetObj);

			// insert log entry into log list and get the global reference back
			objectLog = migObj.getMigrationLog().add(objectLog);

			// store the entry on the hashmap, so it can be found faster
			logObjects.put(sourceObj, objectLog);
		}

		if ((messageType != logNoText) && (message != null)) {
			// if there is a targetObj defined, set it
			if (targetObj != null)
				objectLog.setRefObject(targetObj);

			// create new log message
			ObjectLogEntry logMessage = new ObjectLogEntry(null);

			// set message members
			logMessage.setName(message);
			logMessage.setEntryType(messageType);

			// store it in the global logEntry
			objectLog.getEntries().add(logMessage);
		}
	}

	public GrtObject findTargetObject(GrtObject sourceObj) {
		return (GrtObject) Grt.getInstance().getObjectByRefId(
				(String) (sourceTargetObjectMapping.get(sourceObj.get_id())));
	}

	public String getTargetName(GrtStringHashMap migrationParams,
			String sourceName) {
		String targetName = null;

		if (migrationParams != null)
			targetName = migrationParams.get("targetName");

		if ((targetName != null) && !targetName.equals(""))
			return targetName;
		else
			return sourceName;
	}
}