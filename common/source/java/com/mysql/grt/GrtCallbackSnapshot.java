/*
 Generic Runtime Library (GRT)
 Copyright (C) 2005 MySQL AB

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA 
 */

package com.mysql.grt;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.HashMap;

public class GrtCallbackSnapshot implements GrtCallback {

	private Map objPathCache = new HashMap();

	private Map objIdCache = new HashMap();

	private Object rootValue;

	public void setApplicationPath(String applicationPath) {
		StringBuffer xml = new StringBuffer();

		try {
			FileInputStream inStream = new FileInputStream(new File(
					applicationPath));

			BufferedReader in = new BufferedReader(new InputStreamReader(
					inStream, "utf8"));

			String s;
			while ((s = in.readLine()) != null) {
				xml.append(s);
			}
			in.close();
			inStream.close();

			rootValue = Grt.getObjectsFromGrtXml(xml.toString());

			// build objPathCache
			FileOutputStream outStream = new FileOutputStream(new File(
					applicationPath + ".txt"));
			BufferedWriter out = new BufferedWriter(new OutputStreamWriter(
					outStream, "utf8"));

			buildObjCaches(out, rootValue, "");

			out.close();
			outStream.close();

		} catch (Exception e) {
			System.out.println("The file " + applicationPath
					+ " cannot be found.");
			rootValue = null;
		}

	}

	private void buildObjCaches(BufferedWriter out, Object value,
			String objectPath) throws IOException {

		out.write(objectPath + " - ");

		if (value instanceof GrtObject) {
			out.write(((GrtObject) value).getName() + "\r\n");

			Class c = value.getClass();
			Method[] methods = c.getMethods();

			for (int i = 0; i < methods.length; i++) {
				try {
					if ((!methods[i].getName().startsWith("get"))
							|| (methods[i].getName().equals("getGrtXml"))
							|| (methods[i].getName().equals("getGrtXmlMembers")))
						continue;

					String path = objectPath
							+ "/"
							+ methods[i].getName().substring(3, 4)
									.toLowerCase()
							+ methods[i].getName().substring(4);
					Object fieldValue = methods[i].invoke(value, null);

					if (fieldValue != null)
						buildObjCachesField(out, fieldValue, path);
				} catch (Exception e) {
					out.write("The following exception occured "
							+ "while adding a value by calling "
							+ ((GrtObject) value).getName() + "."
							+ methods[i].getName() + " to the objPathCache ("
							+ e.getMessage() + ").\r\n");
				}
			}
		} else if (value instanceof GrtList) {
			out.write("List\r\n");
			GrtList list = (GrtList) value;

			for (int i = 0; i < list.size(); i++) {
				String path = objectPath + "/" + Integer.toString(i);
				Object listValue = list.getObject(i);

				buildObjCachesField(out, listValue, path);
			}
		} else if (value instanceof GrtHashMap) {
			out.write("Map\r\n");
			GrtHashMap map = (GrtHashMap) value;

			String[] keys = map.getKeys();

			for (int i = 0; i < keys.length; i++) {
				String path = objectPath + "/" + keys[i];
				Object mapValue = map.getObject(keys[i]);

				buildObjCachesField(out, mapValue, path);
			}
		} else {
			out.write(value.getClass().getName() + " = " + value.toString()
					+ "\r\n");
		}

	}

	private void buildObjCachesField(BufferedWriter out, Object value,
			String objectPath) throws IOException {
		objPathCache.put(objectPath, value);

		if (value instanceof GrtObject) {
			objIdCache.put(((GrtObject) value).get_id(), value);
		}

		buildObjCaches(out, value, objectPath);
	}

	public String callGrtFunction(long myx_grt_pointer, String module,
			String functionName, String arguments) {
		return null;
	}

	public Object getGrtGlobalById(long myx_grt_pointer, String id) {
		return objIdCache.get(id);
	}

	public Object getGrtGlobalAsObject(long myx_grt_pointer, String objectPath) {
		return objPathCache.get(objectPath);
	}

	public String getGrtGlobalAsString(long myx_grt_pointer, String objectPath) {
		return (String) getGrtGlobalAsObject(myx_grt_pointer, objectPath);
	}

	public int getGrtGlobalAsInt(long myx_grt_pointer, String objectPath) {
		return ((Integer) getGrtGlobalAsObject(myx_grt_pointer, objectPath))
				.intValue();
	}

	public double getGrtGlobalAsReal(long myx_grt_pointer, String objectPath) {
		return ((Double) getGrtGlobalAsObject(myx_grt_pointer, objectPath))
				.doubleValue();
	}

	public void setGrtGlobalFromObject(long myx_grt_pointer, String objectPath,
			Object value) {

	}

	public void setGrtGlobalFromString(long myx_grt_pointer, String objectPath,
			String value) {

	}

	public void setGrtGlobalFromInt(long myx_grt_pointer, String objectPath,
			int value) {

	}

	public void setGrtGlobalFromReal(long myx_grt_pointer, String objectPath,
			double value) {

	}

	public int getGrtGlobalListSize(long myx_grt_pointer, String objectPath) {
		GrtList list = (GrtList) objPathCache.get(objectPath);
		return list.size();
	}

	public Object getGrtGlobalListItem(long myx_grt_pointer, String objectPath,
			int index) {
		return getGrtGlobalAsObject(myx_grt_pointer, objectPath + "/"
				+ Integer.toString(index));
	}

	public void addGrtGlobalListItem(long myx_grt_pointer, String objectPath,
			Object item) {

	}

	public void removeGrtGlobalListItem(long myx_grt_pointer,
			String objectPath, int index) {

	}

	public Object getGrtGlobalDictItem(long myx_grt_pointer, String objectPath,
			String key) {
		return getGrtGlobalAsObject(myx_grt_pointer, objectPath + "/" + key);
	}

	public void addGrtGlobalDictItem(long myx_grt_pointer, String objectPath,
			String key, Object value) {

	}

	public void removeGrtGlobalDictItem(long myx_grt_pointer,
			String objectPath, String key) {

	}

	public void processMessages(long myx_grt_pointer, String msgs) {

	}

	public int processStatusQuery(long myx_grt_pointer) {
		return 0;
	}

	public int getGrtGlobalDictKeyCount(long myx_grt_pointer, String objectPath) {
		return 0;
	}

	public String getGrtGlobalDictKey(long myx_grt_pointer, String objectPath,
			int index) {
		return "";
	}
	
	public void setBridgeDataObject(long myx_grt_pointer, String objectPath, Object dataObj) {
		
	}
	
	public Object getBridgeDataObject(long myx_grt_pointer, String objectPath) {
		return null;
	}
}