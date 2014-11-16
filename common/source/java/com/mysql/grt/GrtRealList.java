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

public class GrtRealList extends GrtList {

	public GrtRealList() {
		super();
	}
	
	public GrtRealList(GrtObject owner) {
		super(owner);
	}
			
	public GrtRealList(String globalObjectPath) {
		super("", globalObjectPath);
	}
	
	public GrtRealList(String contentStructName, String globalObjectPath) {
		super(contentStructName, globalObjectPath);
	}
	
	public void add(Double item) {
		super.addObject(item);
	}

	public Double get(int index) {
		return (Double) super.getObject(index);
	}
	
	public String getContentType() {
		return "real";
	}

	public String getContentStructName() {
		return "";
	}
}
