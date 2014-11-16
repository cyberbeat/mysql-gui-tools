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

import java.util.HashMap;
import java.util.Iterator;

public class GrtHashMap implements GrtXml {

	protected boolean isGlobalObject = false;

	protected String globalObjectPath;

	protected HashMap map;

	private String contentType = "";

	private String contentStructName = "";

	// private GrtObject owner;

	public GrtHashMap() {
		super();
	}

	public GrtHashMap(GrtObject owner) {
		this();

		// this.owner = owner;
	}

	public GrtHashMap(String contentStructName) {
		this();

		if (!contentStructName.equals("")) {
			this.contentType = "dict";
			this.contentStructName = contentStructName;
		}
	}

	public GrtHashMap(String contentStructName, String globalObjectPath) {
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

	public Object addObject(String key, Object item) {
		if (getIsGlobalObject())
			Grt.getInstance().addGrtGlobalDictItem(getGlobalObjectPath(), key,
					item);
		else {
			if (map == null)
				map = new HashMap();

			map.put(key, item);
		}

		return getObject(key);
	}

	public Object getObject(String key) {
		if (getIsGlobalObject())
			return Grt.getInstance().getGrtGlobalDictItem(
					getGlobalObjectPath(), key);
		else {
			if (map == null)
				map = new HashMap();

			return map.get(key);
		}
	}

	public void remove(String key) {
		if (getIsGlobalObject())
			Grt.getInstance().removeGrtGlobalDictItem(getGlobalObjectPath(),
					key);
		else {
			if (map == null)
				map = new HashMap();

			map.remove(key);
		}
	}

	public String[] getKeys() {
		if (map == null)
			map = new HashMap();

		if (getIsGlobalObject()) {
			map.clear();

			int KeyCount = Grt.getInstance().getGrtGlobalDictKeyCount(
					getGlobalObjectPath());

			for (int i = 0; i < KeyCount; i++) {
				String key = Grt.getInstance().getGrtGlobalDictKey(
						getGlobalObjectPath(), i);
				Object item = getObject(key);

				map.put(key, item);
			}
		}

		return (String[]) map.keySet().toArray(new String[map.keySet().size()]);
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
		xml.append("<value type=\"dict\"" + keyTag);

		if (!getContentType().equals(""))
			xml.append(" content-type=\"" + getContentType() + "\"");

		if (!getContentStructName().equals(""))
			xml.append(" content-struct-name=\"" + getContentStructName()
					+ "\"");

		if (getIsGlobalObject())
			xml.append(" global-object-path=\""
					+ Grt.escapeStringForXml(getGlobalObjectPath()) + "\"");

		xml.append(">\n");

		if (map != null) {
			Iterator keys = map.keySet().iterator();

			while (keys.hasNext()) {
				String key = (String) keys.next();
				Grt.getObjectAsXml(xml, getObject(key), " key=\"" + key + "\"");
			}
		}

		xml.append("</value>\n");

		return xml;
	}
}