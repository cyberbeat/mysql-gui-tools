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

public class GrtIntList extends GrtList {

	public GrtIntList() {
		super();
	}

	public GrtIntList(GrtObject owner) {
		super(owner);
	}

	public GrtIntList(String globalObjectPath) {
		super("", globalObjectPath);
	}

	public GrtIntList(String contentStructName, String globalObjectPath) {
		super(contentStructName, globalObjectPath);
	}

	public void add(Integer item) {
		super.addObject(item);
	}

	public Integer get(int index) {
		return (Integer) super.getObject(index);
	}

	public String getContentType() {
		return "int";
	}

	public String getContentStructName() {
		return "";
	}
}
