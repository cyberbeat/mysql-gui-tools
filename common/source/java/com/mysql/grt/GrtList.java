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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GrtList implements GrtXml {

	protected boolean isGlobalObject = false;

	protected String globalObjectPath;

	protected List list;

	protected Map nameLookupMap;

	private String contentType = "";

	private String contentStructName = "";

	private Object owner;

	public GrtList() {
		super();
	}

	public GrtList(GrtObject owner) {
		this();

		this.owner = owner;
	}

	public GrtList(String contentStructName) {
		this();

		if (!contentStructName.equals("")) {
			this.contentType = "dict";
			this.contentStructName = contentStructName;
		}
	}

	public GrtList(String contentStructName, String globalObjectPath) {
		this(contentStructName);

		this.isGlobalObject = !globalObjectPath.equals("");
		this.globalObjectPath = globalObjectPath;
	}

	protected boolean getIsGlobalObject() {
		return isGlobalObject;
	}

	protected String getGlobalObjectPath() {
		return globalObjectPath;
	}

	public int size() {
		if (getIsGlobalObject())
			return Grt.getInstance()
					.getGrtGlobalListSize(getGlobalObjectPath());
		else {
			if (list == null)
				list = new ArrayList();

			return list.size();
		}
	}

	public Object addObject(Object item) {
		if (getIsGlobalObject())
			Grt.getInstance().addGrtGlobalListItem(getGlobalObjectPath(), item);
		else {
			if (list == null)
				list = new ArrayList();

			list.add(item);
		}

		// return the object we just added
		return getObject(size() - 1);
	}

	public Object getObject(int index) {
		if (getIsGlobalObject()) {
			Object obj = Grt.getInstance().getGrtGlobalListItem(
					getGlobalObjectPath(), index);

			// if this is a reference list, make a lookup
			if ((contentType == "dict") && (obj.getClass() == String.class)) {
				obj = Grt.getInstance().getObjectByRefId((String) obj);
			}

			return obj;

		} else {
			if (list == null)
				list = new ArrayList();

			return list.get(index);
		}
	}

	public GrtList remove(int index) {
		if (getIsGlobalObject())
			Grt.getInstance().removeGrtGlobalListItem(getGlobalObjectPath(),
					index);
		else {
			if (list == null)
				list = new ArrayList();

			list.remove(index);
		}

		return this;
	}

	public int getIndexOfName(String name) {
		if (nameLookupMap == null) {
			// if the nameLookupMap has not been built yet, do it now
			nameLookupMap = new HashMap();

			for (int i = 0; i < size(); i++) {
				Object obj = getObject(i);

				if (obj instanceof GrtObject) {
					String objName = ((GrtObject) obj).getName();

					if (objName != null)
						nameLookupMap.put(objName, new Integer(i));
				}
			}
		}

		Integer index = (Integer) nameLookupMap.get(name);

		if (index != null)
			return index.intValue();
		else
			return -1;
	}

	public boolean contains(Object item) {
		if (getIsGlobalObject()) {
			for (int i = 0; i < size(); i++) {
				if (getObject(i).equals(item))
					return true;
			}

			return false;
		} else {
			if (list == null)
				list = new ArrayList();

			return list.contains(item);
		}
	}

	public Object getOwner() {
		return owner;
	}

	public void setOwner(Object owner) {
		this.owner = owner;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getContentStructName() {
		return contentStructName;
	}

	public void setContentStructName(String contentStructName) {
		this.contentStructName = contentStructName;
	}

	public StringBuffer getGrtXml(StringBuffer xml) {
		return getGrtXml(xml, "");
	}

	public StringBuffer getGrtXml(StringBuffer xml, String keyTag) {
		xml.append("<value type=\"list\"");

		if (!getContentType().equals(""))
			xml.append(" content-type=\"" + getContentType() + "\"");

		if (!getContentStructName().equals(""))
			xml.append(" content-struct-name=\"" + getContentStructName()
					+ "\"");

		if (getIsGlobalObject())
			xml.append(" global-object-path=\""
					+ Grt.escapeStringForXml(getGlobalObjectPath()) + "\"");

		xml.append(keyTag + ">\n");

		for (int i = 0; i < size(); i++) {
			Grt.getObjectAsXml(xml, getObject(i), "");
		}

		xml.append("</value>\n");

		return xml;
	}

	public StringBuffer getGrtRefXml(StringBuffer xml, String keyTag) {
		xml.append("<value type=\"list\" option=\"ref\"");

		if (getContentType() != "")
			xml.append(" content-type=\"string\"");

		if (getContentStructName() != "")
			xml.append(" content-struct-name=\"" + getContentStructName()
					+ "\"");

		xml.append(keyTag + ">\n");

		for (int i = 0; i < size(); i++) {
			Object obj = getObject(i);

			if (GrtObject.class.isAssignableFrom(obj.getClass())) {
				xml.append("<value type=\"string\" option=\"ref\">"
						+ Grt.escapeStringForXml(((GrtObject) obj).get_id())
						+ "</value>\n");
			}
		}

		xml.append("</value>\n");

		return xml;
	}
}