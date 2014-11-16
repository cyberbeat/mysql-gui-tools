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

import java.rmi.dgc.VMID;
import java.rmi.server.UID;

/**
 * @author Mike
 *
 */
public class GrtObjectDataLocal implements GrtObjectData {
	
	protected GrtObject grtObject;
	
	protected String name;

	protected String _id = new VMID().toString() + new UID().toString();

	protected String owner;
	
	public GrtObjectDataLocal(GrtObject grtObject) {
		this.grtObject = grtObject;
	}

	public String getName() {
		return name;
	}

	public String setName(String name) {
		this.name = name;

		return getName();
	}

	public String get_id() {
		return _id;
	}

	public String set_id(String _id) {
		// remove old _id
		Grt.getInstance().removeFromObjectCache(grtObject);

		this._id = _id;

		Grt.getInstance().addToObjectCache(grtObject);
			
		return get_id();
	}

	public GrtObject getOwner() {
		return (GrtObject) Grt.getInstance().getObjectByRefId(owner);
	}

	public GrtObject setOwner(GrtObject owner) {
		if (owner != null)
			this.owner = owner.get_id();

		return getOwner();
	}
	
	public String getOwnerById() {
		return owner;
	}
	
	public void setOwnerById(String owner_id) {
		this.owner = owner_id; 
	}
	
	public void setBridgeDataObject(Object dataObj) {
		
	}
	
	public Object getBridgeDataObject() {
		return null;
	}
}
