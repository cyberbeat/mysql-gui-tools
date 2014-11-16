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

import java.io.*;

public class GrtCallbackNative implements GrtCallback {

	public void setApplicationPath(String applicationPath)
	{
		if (System.getProperty("os.name").indexOf("Windows") > -1) 
		{
			// check if debug lib is there
			File f = new File(applicationPath + "msvcr71d.dll");
			if (f.exists()) {
				System.load(applicationPath + "msvcr71d.dll");
			} else {
				// if the debug lib is not there, it has to be a release
				f = new File(applicationPath + "msvcr71.dll");
				if (f.exists()) {
					System.load(applicationPath + "msvcr71.dll");
				}
			}

			System.loadLibrary("iconv");
			System.loadLibrary("intl");
			System.loadLibrary("zlib");
			System.loadLibrary("pcre3");
			System.loadLibrary("libglib-2.0-0");
			//System.loadLibrary("libgmodule-2.0-0");
			//System.loadLibrary("libgobject-2.0-0");
			System.loadLibrary("libgthread-2.0-0");
			System.loadLibrary("libxml2");
			System.loadLibrary("libmysql");
			System.loadLibrary("libmysqlx");
			System.loadLibrary("libmysqlgrt");
			System.loadLibrary("libmysqlutil");

			//System.loadLibrary("libmysqlgrtbuiltinmodule");
		} 
		else if (System.getProperty("os.name").indexOf("Linux") > -1) 
                {
                        System.loadLibrary("myx_grt_java");
                } 
                else if (System.getProperty("os.name").toLowerCase().indexOf("solaris") > -1 || System.getProperty("os.name").toLowerCase().indexOf("sunos") > -1) 
                {
                        //System.loadLibrary("myx_grt_java.so");
                } 
                else if (System.getProperty("os.name").toLowerCase().startsWith("mac os x")) 
                {
                        System.loadLibrary(applicationPath + "myx_grt_java.dylib");
                }
                else
                {
                        Grt.getInstance().addMsg("Operating system not supported. Native callback not set");
                        Grt.getInstance().flushMessages();
                }
	}

	/**
	 * Calls a GRT function from the given module
	 * 
	 * @param myx_grt_pointer
	 *            pointer (casted as long) that points to the native grt
	 *            instance
	 * @param module
	 *            name of the module that implements the function
	 * @param name
	 *            name of the function to call
	 * @param params
	 *            parameters that should be passed to the function as grt XML
	 *            string
	 */
	public native String callGrtFunction(long myx_grt_pointer, String module,
			String functionName, String arguments);

	public native Object getGrtGlobalById(long myx_grt_pointer, String id);

	public native Object getGrtGlobalAsObject(long myx_grt_pointer,
			String objectPath);

	public native String getGrtGlobalAsString(long myx_grt_pointer,
			String objectPath);

	public native int getGrtGlobalAsInt(long myx_grt_pointer, String objectPath);

	public native double getGrtGlobalAsReal(long myx_grt_pointer,
			String objectPath);

	public native void setGrtGlobalFromObject(long myx_grt_pointer,
			String objectPath, Object value);

	public native void setGrtGlobalFromString(long myx_grt_pointer,
			String objectPath, String value);

	public native void setGrtGlobalFromInt(long myx_grt_pointer,
			String objectPath, int value);

	public native void setGrtGlobalFromReal(long myx_grt_pointer,
			String objectPath, double value);

	public native int getGrtGlobalListSize(long myx_grt_pointer,
			String objectPath);

	public native Object getGrtGlobalListItem(long myx_grt_pointer,
			String objectPath, int index);

	public native void addGrtGlobalListItem(long myx_grt_pointer,
			String objectPath, Object item);

	public native void removeGrtGlobalListItem(long myx_grt_pointer,
			String objectPath, int index);

	public native Object getGrtGlobalDictItem(long myx_grt_pointer,
			String objectPath, String key);

	public native void addGrtGlobalDictItem(long myx_grt_pointer,
			String objectPath, String key, Object value);

	public native void removeGrtGlobalDictItem(long myx_grt_pointer,
			String objectPath, String key);

	public native int getGrtGlobalDictKeyCount(long myx_grt_pointer,
			String objectPath);

	public native String getGrtGlobalDictKey(long myx_grt_pointer,
			String objectPath, int index);

	public native void processMessages(long myx_grt_pointer, String msgs);

	public native int processStatusQuery(long myx_grt_pointer);
	
	public native void setBridgeDataObject(long myx_grt_pointer, String objectPath, Object dataObj);
	
	public native Object getBridgeDataObject(long myx_grt_pointer, String objectPath);
}
