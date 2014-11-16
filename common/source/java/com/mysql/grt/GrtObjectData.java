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

/**
 * @author Mike
 *
 */
public interface GrtObjectData {
	public String getName();
	
	public String setName(String name);
	
	public String get_id();
	
	public String set_id(String _id);

	public GrtObject getOwner();
	
	public GrtObject setOwner(GrtObject owner);
	
	public String getOwnerById();
	
	public void setOwnerById(String owner_id);
	
	//public String getGrtXml(String keyTag);
	
	public void setBridgeDataObject(Object dataObj);
	
	public Object getBridgeDataObject();
}
