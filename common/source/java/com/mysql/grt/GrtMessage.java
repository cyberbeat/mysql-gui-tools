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

public class GrtMessage implements GrtXml {
	private int msgType = 1; // 0..message, 1..error, 2.. progress

	private String msg = "";

	private GrtStringList msgDetails = new GrtStringList();

	private int progress = -1;

	final static int logMessage = 0;

	final static int logError = 1;

	final static int logProgress = 2;

	public GrtMessage(String msg, int msgType) {
		this.msg = msg;
		this.msgType = msgType;
	}

	public GrtMessage(String msg, int msgType, int progress) {
		this.msg = msg;
		this.msgType = msgType;
		this.progress = progress;
	}

	public void addMessageDetail(String msgDetail) {
		msgDetails.add(msgDetail);
	}

	public StringBuffer getGrtXml(StringBuffer xml) {
		return getGrtXml(xml, "");
	}

	public StringBuffer getGrtXml(StringBuffer xml, String keyTag) {
		xml.append("<value type=\"dict\" struct-name=\"GrtMessage\" key=\""
				+ keyTag + "\">" + "<value type=\"int\" key=\"msgType\">"
				+ Integer.toString(msgType) + "</value>\n"
				+ "<value type=\"int\" key=\"progress\">"
				+ Integer.toString(progress) + "</value>\n"
				+ "<value type=\"string\" key=\"msg\">" + msg + "</value>\n");

		msgDetails.getGrtXml(xml, " key=\"details\"");

		xml.append("</value>\n");

		return xml;
	}
}