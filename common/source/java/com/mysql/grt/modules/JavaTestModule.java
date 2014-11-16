package com.mysql.grt.modules;

import com.mysql.grt.*;
import com.mysql.grt.db.*;

//import java.sql.*;
//import java.io.*;

public class JavaTestModule {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(JavaTestModule.class, "");
	}

	public static String helloWorld() {
		Grt.getInstance().addMsg("Hello world!");

		return "Hello World!";
	}

	public static String upperCase(String str) {
		return str.toUpperCase();
	}

	public static int getListSize(GrtList list) {
		return list.size();
	}

	public static String concatStrings(String s1, String s2) {
		return s1 + s2;
	}

	public static void throwException() throws Exception {
		throw new Exception("Exception Test");
	}

	public static String getGlobalString(String objectPath) {
		Grt.getInstance().addMsg("Calling getGrtGlobalAsString.");
		Grt.getInstance().addMsgDetail(
				"applicationPath = " + Grt.getInstance().getApplicationPath());
		Grt.getInstance().addMsgDetail(
				"callback.class = "
						+ Grt.getInstance().getCallback().getClass().getName());

		return Grt.getInstance().getGrtGlobalAsString(objectPath);
	}

	public static void testCallbacks() {
		GrtHashMap root = (GrtHashMap) Grt.getInstance().getGrtGlobalAsObject(
				"/");

		GrtStringList list = new GrtStringList();
		list.add("Item1");
		list.add("Item2");
		root.addObject("stringList", list);

		GrtObject obj = new GrtObject(null);
		obj.setName("testObject");
		root.addObject("object", obj);

		GrtStringHashMap map = new GrtStringHashMap();
		map.add("mike", "mzinner@mysql.com");
		map.add("alfredo", "alfredo@mysql.com");
		root.addObject("emails", map);

		Catalog catalog = new Catalog(null);
		catalog.setName("sourceCatalog");

		SchemaList schemata = new SchemaList();
		catalog.setSchemata(schemata);

		Schema schema = new Schema(catalog);
		schema.setName("scott");
		schemata.add(schema);

		root.addObject("sourceCatalog", catalog);
	}

	/*
	 * public static String getColumnFlags(com.mysql.grt.db.Column col) { String
	 * flags = "";
	 * 
	 * for (int i = 0; i < col.getFlags().size(); i++) { if (!flags.equals(""))
	 * flags += " ";
	 * 
	 * flags += col.getFlags().get(i); }
	 * 
	 * return flags; }
	 */

	/*
	 * public static void insertOracleBlob(String tableName, String
	 * pkColumnName, String blobColumnName, Integer id,
	 * com.mysql.grt.db.mgmt.Connection Connection, String filename) throws
	 * Exception { File testFile = new File(filename);
	 * 
	 * if (testFile.length() == 0) return;
	 * 
	 * Connection conn = com.mysql.grt.modules.ReverseEngineeringGeneric
	 * .establishConnection(Connection);
	 * 
	 * conn.setAutoCommit(false);
	 * 
	 * Statement stmt = conn.createStatement();
	 * 
	 * stmt.executeUpdate("INSERT INTO " + tableName + "(" + pkColumnName + "," +
	 * blobColumnName + ") VALUES(" + id + ", empty_blob())");
	 * 
	 * ResultSet rset = stmt.executeQuery("SELECT " + blobColumnName + " FROM " +
	 * tableName + " WHERE " + pkColumnName + "=" + id + " FOR UPDATE");
	 * 
	 * if (rset.next()) { Blob testBlob = rset.getBlob(1); OutputStream
	 * blobOutputStream = ((oracle.sql.BLOB) testBlob) .getBinaryOutputStream();
	 * 
	 * InputStream fileStream = new java.io.FileInputStream(testFile);
	 * 
	 * byte[] l_buffer = new byte[10 * 1024]; int l_nread = 0;
	 * 
	 * while ((l_nread = fileStream.read(l_buffer)) != -1)
	 * blobOutputStream.write(l_buffer, 0, l_nread);
	 * 
	 * fileStream.close(); blobOutputStream.close(); }
	 * 
	 * conn.commit(); conn.close(); }
	 */
}