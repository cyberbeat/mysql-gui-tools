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

public interface GrtCallback {
	public abstract void setApplicationPath(String applicationPath);

	public abstract String callGrtFunction(long myx_grt_pointer, String module,
			String functionName, String arguments);

	public abstract Object getGrtGlobalById(long myx_grt_pointer, String id);

	public abstract Object getGrtGlobalAsObject(long myx_grt_pointer,
			String objectPath);

	public abstract String getGrtGlobalAsString(long myx_grt_pointer,
			String objectPath);

	public abstract int getGrtGlobalAsInt(long myx_grt_pointer,
			String objectPath);

	public abstract double getGrtGlobalAsReal(long myx_grt_pointer,
			String objectPath);

	public abstract void setGrtGlobalFromObject(long myx_grt_pointer,
			String objectPath, Object value);

	public abstract void setGrtGlobalFromString(long myx_grt_pointer,
			String objectPath, String value);

	public abstract void setGrtGlobalFromInt(long myx_grt_pointer,
			String objectPath, int value);

	public abstract void setGrtGlobalFromReal(long myx_grt_pointer,
			String objectPath, double value);

	public abstract int getGrtGlobalListSize(long myx_grt_pointer,
			String objectPath);

	public abstract Object getGrtGlobalListItem(long myx_grt_pointer,
			String objectPath, int index);

	public abstract void addGrtGlobalListItem(long myx_grt_pointer,
			String objectPath, Object item);

	public abstract void removeGrtGlobalListItem(long myx_grt_pointer,
			String objectPath, int index);

	public abstract Object getGrtGlobalDictItem(long myx_grt_pointer,
			String objectPath, String key);

	public abstract void addGrtGlobalDictItem(long myx_grt_pointer,
			String objectPath, String key, Object value);

	public abstract void removeGrtGlobalDictItem(long myx_grt_pointer,
			String objectPath, String key);

	public abstract int getGrtGlobalDictKeyCount(long myx_grt_pointer,
			String objectPath);

	public abstract String getGrtGlobalDictKey(long myx_grt_pointer,
			String objectPath, int index);

	public abstract void processMessages(long myx_grt_pointer, String msgs);

	public abstract int processStatusQuery(long myx_grt_pointer);
	
	public abstract void setBridgeDataObject(long myx_grt_pointer, String objectPath, Object dataObj);
	
	public abstract Object getBridgeDataObject(long myx_grt_pointer, String objectPath);
}