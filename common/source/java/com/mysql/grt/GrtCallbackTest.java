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

import com.mysql.grt.db.*;

public class GrtCallbackTest implements GrtCallback {

	public void setApplicationPath(String applicationPath) {
	}

	public String callGrtFunction(long myx_grt_pointer, String module,
			String functionName, String arguments) {
		return null;
	}

	public Object getGrtGlobalById(long myx_grt_pointer, String id) {
		if (id.equals("123456789"))
			return getGrtGlobalAsObject(myx_grt_pointer, "/testObject");
		else if (id.equals("235205293502983423"))
			return getGrtGlobalAsObject(myx_grt_pointer, "/simpleDatatype");
		else if (id.equals("248468926343234234"))
			return getGrtGlobalAsObject(myx_grt_pointer, "/column");
		else
			return null;
	}

	/*
	 * private String prepareGrtXml(Object obj) { return
	 * Grt.prepareGrtXml(Grt.getObjectAsXml(obj)); }
	 */

	public Object getGrtGlobalAsObject(long myx_grt_pointer, String objectPath) {
		// A GrtObject
		if (objectPath.equals("/testObject"))
			return new GrtObject("/testObject");
		else if (objectPath.equals("/testObject/name"))
			return "TestObject";
		else if (objectPath.equals("/testObject/_id"))
			return "123456789";
		// A SimpleDatatype
		else if (objectPath.equals("/simpleDatatype"))
			return new SimpleDatatype("/simpleDatatype");
		else if (objectPath.equals("/simpleDatatype/name"))
			return "VARCHAR";
		else if (objectPath.equals("/simpleDatatype/_id"))
			return "235205293502983423";
		else if (objectPath.equals("/simpleDatatype/characterMaximumLength"))
			return new Integer(255);
		else if (objectPath.equals("/simpleDatatype/characterOctetLength"))
			return new Integer(0);
		else if (objectPath.equals("/simpleDatatype/numericPrecision"))
			return new Integer(0);
		else if (objectPath.equals("/simpleDatatype/numericPrecisionRadix"))
			return new Integer(0);
		else if (objectPath.equals("/simpleDatatype/numericScale"))
			return new Integer(0);
		else if (objectPath.equals("/simpleDatatype/dateTimePrecision"))
			return new Integer(0);
		// A Column
		else if (objectPath.equals("/column"))
			return new Column("/column");
		else if (objectPath.equals("/column/name"))
			return "TestColumn";
		else if (objectPath.equals("/column/_id"))
			return "248468926343234234";
		else if (objectPath.equals("/column/precision"))
			return new Integer(0);
		else if (objectPath.equals("/column/scale"))
			return new Integer(0);
		else if (objectPath.equals("/column/isNullable"))
			return new Integer(1);
		else if (objectPath.equals("/column/length"))
			return new Integer(45);
		else if (objectPath.equals("/column/datatypeName"))
			return "VARCHAR";
		else if (objectPath.equals("/column/defaultValue"))
			return "undefined";
		else if (objectPath.equals("/column/characterSetName"))
			return "UTF8";
		else if (objectPath.equals("/column/collationName"))
			return "UTF8_general_ci";
		else if (objectPath.equals("/column/simpleType"))
			return "235205293502983423";
		else
			return null;
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
		return 0;
	}

	public Object getGrtGlobalListItem(long myx_grt_pointer, String objectPath,
			int index) {
		return null;
	}

	public void addGrtGlobalListItem(long myx_grt_pointer, String objectPath,
			Object item) {

	}

	public void removeGrtGlobalListItem(long myx_grt_pointer,
			String objectPath, int index) {

	}

	public Object getGrtGlobalDictItem(long myx_grt_pointer, String objectPath,
			String key) {
		return null;
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