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

package com.mysql.grt.modules;

import java.util.Vector;
import java.io.File;

import com.mysql.grt.*;

/**
 * @author Mike
 * 
 * Java base module
 */
public class BaseJava {

	/**
	 * Static function to return information about this class to the GRT
	 * environment
	 * 
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfo() {
		return Grt.getModuleInfoXml(BaseJava.class, "");
	}

	public static String engineVersion() {
		return "Java " + System.getProperty("java.version") + " "
				+ System.getProperty("java.vendor");
	}

	public static GrtList getMessages() {
		GrtList msgList = new GrtList("GrtMessage");
		Vector msgs = Grt.getInstance().getMessages();

		for (int i = 0; i < msgs.size(); i++) {
			msgList.addObject(msgs.get(i));
		}

		msgs.clear();

		return msgList;
	}

	public static int javaClassExists(String className) {
		int res = 0;

		try {
			if (Class.forName(className) != null)
				res = 1;
		} catch (ClassNotFoundException e) {
			// ignore exception
		}

		return res;
	}

	public static int java2GrtXmlLogging(Integer enable) {
		Grt.getInstance().java2GrtXmlLogging(enable.intValue());

		return Grt.getInstance().java2GrtXmlLogging();
	}

	public static void clearLogs() {
		Grt.deleteDir(new File(Grt.getInstance().getApplicationDataPath()
				+ "log"));
	}
}