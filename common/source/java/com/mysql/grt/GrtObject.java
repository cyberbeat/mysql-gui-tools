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

public class GrtObject implements GrtXml {

	protected GrtObjectData data = null;

	protected GrtObject() {
	}

	public GrtObject(Object obj) {
		if ((obj == null) || (obj instanceof com.mysql.grt.GrtObject)) {
			data = new GrtObjectDataLocal(this);
			data.setOwner((GrtObject) obj);
			Grt.getInstance().addToObjectCache(this);
		} else if (obj instanceof String) {
			data = new GrtObjectDataGlobal((String) obj);
		}
	}

	public String getName() {
		return data.getName();
	}

	public String setName(String name) {
		return data.setName(name);
	}

	public String get_id() {
		return data.get_id();
	}

	public String set_id(String _id) {
		return data.set_id(_id);
	}

	public GrtObject getOwner() {
		return data.getOwner();
	}

	public GrtObject setOwner(GrtObject owner) {
		return data.setOwner(owner);
	}

	public String getOwnerById() {
		return data.getOwnerById();
	}

	public void setOwnerById(String owner_id) {
		data.setOwnerById(owner_id);
	}

	public StringBuffer getGrtXml(StringBuffer xml) {
		return getGrtXml(xml, "");
	}

	public StringBuffer getGrtXml(StringBuffer xml, String keyTag) {
		xml.append("<value type=\"dict\" struct-name=\""
				+ this.getClass().getName().substring(14) + "\"" + keyTag);

		if (data instanceof GrtObjectDataLocal) {
			xml.append(">\n");
			getGrtXmlMembers(xml);
			xml.append("</value>\n");
		} else {
			xml.append(" global-object-path=\""
					+ Grt.escapeStringForXml(((GrtObjectDataGlobal) data)
							.getGlobalObjectPath()) + "\" />\n");
		}

		return xml;
	}

	public StringBuffer getGrtXmlMembers(StringBuffer xml) {
		xml.append("<value type=\"string\" key=\"_id\">"
				+ Grt.escapeStringForXml(get_id()) + "</value>\n");

		if (getName() != null)
			xml.append("<value type=\"string\" key=\"name\">"
					+ Grt.escapeStringForXml(getName()) + "</value>\n");

		if (getOwner() != null)
			xml.append("<value type=\"string\" key=\"owner\">"
					+ Grt.escapeStringForXml(getOwner().get_id())
					+ "</value>\n");

		return xml;
	}

	public void setBridgeDataObject(Object dataObj) {
		data.setBridgeDataObject(dataObj);
	}

	public Object getBridgeDataObject() {
		return data.getBridgeDataObject();
	}
}