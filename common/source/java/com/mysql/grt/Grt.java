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

import java.util.Vector;
import java.util.HashMap;
import java.lang.Class;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.io.FileWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import java.io.File;
import java.io.IOException;
import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

/**
 * Grt main class
 * 
 * @author Mike
 * @version 1.0, 11/29/04
 */
public class Grt {
	static Grt instance;

	private long myxGrtPointer = 0;

	private GrtCallback callback = null;

	private String applicationPath;

	private String applicationDataPath = "";

	private HashMap objects = new HashMap();

	private Vector msgs = new Vector();

	private int doJava2GrtXmlLogging = 0;

	public static final int MYX_INT_VALUE = 1;

	public static final int MYX_REAL_VALUE = 2;

	public static final int MYX_STRING_VALUE = 3;

	public static final int MYX_LIST_VALUE = 4;

	public static final int MYX_DICT_VALUE = 5;

	public static final int MYX_UNKNOWN_VALUE = -1;

	public static final String GrtPackagePrefix = "com.mysql.grt.";

	public static final String GrtModulePackagePrefix = "com.mysql.grt.modules.";

	/**
	 * Private class constructor.
	 */
	private Grt() {
		// dummy call to avoid Eclipse warning
		setMyxGrtPointer(0);
	}

	/**
	 * Static function to return a pointer to a instance of the Grt class. The
	 * first call triggers the generation of a object and all other calls return
	 * a pointer to that object.
	 * 
	 * @return returns a pointer to the one and only Grt object
	 */
	public static Grt getInstance() {
		if (instance == null)
			instance = new Grt();

		return instance;
	}

	/**
	 * String replace function because String.replaceAll is broken
	 * 
	 * @param orig
	 *            the original string
	 * @param searchFor
	 *            the substring to search for
	 * @param replacement
	 *            the string the substring should be replaced with
	 * 
	 * @return returns the string with all searchFor substrings replaced
	 */
	public static String replace(String orig, String searchFor,
			String replacement) {
		if (orig == null || searchFor == null || searchFor.equals("")) {
			String msg = "Can not perform replace." //
					+ " orig: \"" + orig + "\"" //
					+ " searchFor: \"" + searchFor + "\"" //
					+ " replacement: \"" + replacement;
			throw new IllegalArgumentException(msg);
		}

		int sizeWildGuess = orig.length() + replacement.length() + 20;
		StringBuffer buf = new StringBuffer(sizeWildGuess);
		replaceInner(buf, orig, searchFor, replacement);
		return buf.toString();
	}

	private static void replaceInner(StringBuffer buf, String orig,
			String searchFor, String replacement) {

		int index = orig.indexOf(searchFor);
		if (index < 0) {
			buf.append(orig);
			return;
		}

		String left = orig.substring(0, index);
		buf.append(left);
		buf.append(replacement);

		String right = orig.substring(index + searchFor.length());
		replaceInner(buf, right, searchFor, replacement);
	}

	/**
	 * Deletes a directory and all its files and sub-directories
	 * 
	 * @param dir
	 *            the directory to delete
	 * 
	 * @return true, if the directory could be deleted
	 */
	public static boolean deleteDir(File dir) {
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				boolean success = deleteDir(new File(dir, children[i]));
				if (!success) {
					return false;
				}
			}
		}

		// The directory is now empty so delete it
		return dir.delete();
	}

	/**
	 * This function sets the callback object that is used to call back to the
	 * JNI.
	 * 
	 * @param callbackClassName
	 *            name of the class that is used to process the callbacks
	 * @param applicationPath
	 *            the path to the directory where the libraries are located
	 */
	public void setCallback(String callbackClassName, String applicationPath) {
		if (this.callback == null) {
			GrtCallback callback;

			try {
				Class c = Class.forName(GrtPackagePrefix + callbackClassName);
				Constructor con = c.getConstructor(new Class[] {});
				callback = (GrtCallback) con.newInstance(new Object[] {});
			} catch (Exception e) {
				return;
			}

			this.callback = callback;
			this.applicationPath = applicationPath;

			// Set the callback's applicationPath so it can find the libraries
			callback.setApplicationPath(applicationPath);
		}
	}

	/**
	 * Function to return the instance of a GrtCallbackMock that is used to
	 * process the callback methods
	 * 
	 * @return instance of a GrtCallback class
	 */
	public GrtCallback getCallback() {
		if (callback == null)
			throw new RuntimeException("The callback object is not set.");

		return callback;
	}

	/**
	 * Enables or disables the logging of xml passed between Java and the GRT
	 * for objects. The xml will be written to a Java2Grt.xml in the application
	 * dir.
	 * 
	 * @param enable
	 *            if set to 1, the logging is enabled
	 */
	public void java2GrtXmlLogging(int enable) {
		if (enable == 1)
			this.doJava2GrtXmlLogging = 1;
		else
			this.doJava2GrtXmlLogging = 0;
	}

	/**
	 * Returns the current state of the logging of xml passed between Java and
	 * the GRT
	 * 
	 * @return returns 1 if the logging is enabled
	 */
	public int java2GrtXmlLogging() {
		return this.doJava2GrtXmlLogging;
	}

	/**
	 * returns the applicationPath string. Has to be set before with
	 * setApplicationPath
	 * 
	 * @return returns the applicationPath string
	 */
	public String getApplicationPath() {
		return applicationPath;
	}

	/**
	 * returns the applicationPath string. Has to be set before with
	 * setApplicationPath
	 * 
	 * @return returns the applicationPath string
	 */
	public String getApplicationDataPath() {
		if (applicationDataPath == "")
			applicationDataPath = (String) Grt.getInstance().callGrtFunction(
					"Base", "getAppDataDir", null);

		return applicationDataPath;
	}

	/**
	 * returns the myxGrtPointer used for callGrtFunction calls casted as a long
	 * 
	 * @return returns the myxGrtPointer
	 */
	private long getMyxGrtPointer() {
		return myxGrtPointer;
	}

	/**
	 * sets the myxGrtPointer used for callGrtFunction calls
	 * 
	 * @param myxGrtPointer
	 *            the pointer casted as a long
	 */
	private void setMyxGrtPointer(long myxGrtPointer) {
		this.myxGrtPointer = myxGrtPointer;
	}

	/**
	 * Adds an object to the Grt's list of used object.
	 * 
	 * @param object
	 *            object to add
	 */
	public void addToObjectCache(GrtObject object) {
		if (!objects.containsKey(object.get_id()))
			objects.put(object.get_id(), object);
	}

	/**
	 * Removes an object from the Grt's list of used object.
	 * 
	 * @param object
	 *            object to add
	 */
	protected void removeFromObjectCache(GrtObject object) {
		if (objects.containsKey(object.get_id()))
			objects.remove(object.get_id());
	}

	/**
	 * Clears the object cache
	 * 
	 */
	protected void clearObjectCache() {
		Grt.getInstance().objects.clear();
	}

	/**
	 * Search a object in the list of used objects by it's _id. If the _id is
	 * not found the function looks in the GRT global object tree
	 * 
	 * @param RefId
	 *            the _Id of the object
	 * @return a pointer to the object or null if not found
	 */
	public GrtObject getObjectByRefId(String refId) {
		if (refId == null)
			return null;

		GrtObject obj = (GrtObject) objects.get(refId);

		// if the refId is not found in the object cache
		// search for the refId in the global object using the
		// callback function
		if ((obj == null) && (getCallback() != null)) {
			obj = (GrtObject) getCallback().getGrtGlobalById(
					getMyxGrtPointer(), refId);
		}

		return obj;
	}

	/**
	 * Return an GrtObject of the given class
	 * 
	 * @param className
	 *            name of the class, e.g. db.oracle.table
	 * @param parent
	 *            The parent object, if any
	 * @return a new instance of the requested class or null if the class was
	 *         not found
	 */
	public static Object getGrtClassInstance(String className, Object parent) {
		try {
			Class c = Class.forName(Grt.GrtPackagePrefix + className);

			Constructor con = c.getConstructor(new Class[] { Object.class });
			GrtObject obj = (GrtObject) con
					.newInstance(new Object[] { parent });

			return obj;
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * Adds the given message string as a normal GRT message to the GRT message
	 * list
	 * 
	 * @param msg
	 *            message string
	 * @return a new generated GrtMessage
	 */
	public GrtMessage addMsg(String msg) {
		GrtMessage grtMsg = new GrtMessage(msg, GrtMessage.logMessage);

		msgs.add(grtMsg);

		return grtMsg;
	}

	/**
	 * Adds the given message string as a normal GRT message to the GRT message
	 * list
	 * 
	 * @param msg
	 *            message string
	 * @param progress
	 *            the current progress, a value between 0 and 100
	 * @return a new generated GrtMessage
	 */
	public GrtMessage addProgress(String msg, int progress) {
		GrtMessage grtMsg = new GrtMessage(msg, GrtMessage.logProgress,
				progress);

		msgs.add(grtMsg);

		return grtMsg;
	}

	/**
	 * Adds the given message string as a GRT error to the GRT message list
	 * 
	 * @param msg
	 *            message string
	 * @return a new generated GrtMessage
	 */
	public GrtMessage addErr(String msg) {
		GrtMessage grtMsg = new GrtMessage(msg, GrtMessage.logError);

		msgs.add(grtMsg);

		return grtMsg;
	}

	/**
	 * Adds the given detail message to the latest GRT message. Beware that this
	 * method is not threadsafe
	 * 
	 * @param msgDetail
	 *            message detail string
	 */
	public void addMsgDetail(String msgDetail) {
		if (msgs.size() > 0) {
			GrtMessage msg = ((GrtMessage) msgs.get(msgs.size() - 1));
			addMsgDetail(msg, msgDetail);
		}
	}

	/**
	 * Adds the given detail message to the given GRT message
	 * 
	 * @param msg
	 *            the GrtMsg the detail string should be added to
	 * @param msgDetail
	 *            message detail string
	 */
	public void addMsgDetail(GrtMessage msg, String msgDetail) {
		msg.addMessageDetail(msgDetail);
	}

	/**
	 * Returns GRT messages as Vector
	 * 
	 * @param offset
	 *            number of first message to return
	 * @return the Grt message vector
	 */
	public Vector getMessages() {
		return msgs;
	}

	/**
	 * Forces the output of the GRT messages
	 * 
	 */
	public int flushMessages() {
		int res = 0;

		if ((msgs.size() > 0) && (callback != null)) {
			GrtList msgList = new GrtList("GrtMessage");

			for (int i = 0; i < msgs.size(); i++) {
				msgList.addObject(msgs.get(i));
			}

			msgs.clear();

			// print messages
			getCallback().processMessages(getMyxGrtPointer(),
					msgList.getGrtXml(new StringBuffer()).toString());

			// get status
			res = getCallback().processStatusQuery(getMyxGrtPointer());
		}

		return res;
	}

	/**
	 * Returns the numeric type of a value_type name
	 * 
	 * @param s
	 *            name of the value_type
	 */
	public static int getValueTypeFromString(String s) {
		if (s.compareTo("int") == 0)
			return Grt.MYX_INT_VALUE;
		else if (s.compareTo("real") == 0)
			return Grt.MYX_REAL_VALUE;
		else if (s.compareTo("string") == 0)
			return Grt.MYX_STRING_VALUE;
		else if (s.compareTo("list") == 0)
			return Grt.MYX_LIST_VALUE;
		else if (s.compareTo("dict") == 0)
			return Grt.MYX_DICT_VALUE;
		else
			return Grt.MYX_UNKNOWN_VALUE;
	}

	/**
	 * Adds a XML header and a root node to the given XML string
	 * 
	 * @param xmlContent
	 *            a XML string or null
	 * @return a stringlist that can be used with GRT XML functions
	 */
	public static StringBuffer prepareGrtXmlHeader(String xmlContent) {
		StringBuffer xml = new StringBuffer("<?xml version=\"1.0\"?>\n<data>\n");

		if ((xmlContent != null) && (!xmlContent.equals("")))
			xml.append(xmlContent);

		return xml;
	}

	/**
	 * Adds a XML header and a root node to the given XML string
	 * 
	 * @return a stringlist that can be used with GRT XML functions
	 */
	public static StringBuffer prepareGrtXmlHeader() {
		return prepareGrtXmlHeader(null);
	}

	/**
	 * Adds a XML footer to given XML StringBuffer
	 * 
	 * @param xml
	 *            a XML string with a GRT XML header
	 * @return a complete XML document as StringBuffer
	 */
	protected static StringBuffer prepareGrtXmlFooter(StringBuffer xml) {

		xml.append("</data>\n");

		return xml;
	}

	/**
	 * Adds a XML footer to given XML StringBuffer and returns a string
	 * 
	 * @param xml
	 *            a XML string with a GRT XML header
	 * @return a complete XML document as String
	 */
	public static String prepareGrtXml(StringBuffer xml) {

		return prepareGrtXmlFooter(xml).toString();
	}

	/**
	 * Checks if two classes are the same Converts simple types like int to
	 * java.lang.Integer
	 * 
	 * @param c1
	 *            class to compare
	 * @param c2
	 *            class to compare
	 * @return true if the classes are the same
	 */
	private static boolean checkSameClass(Class c1, Class c2) {
		String className1 = c1.getName();
		String className2 = c2.getName();

		if (className1.equals("int"))
			className1 = "java.lang.Integer";

		if (className2.equals("int"))
			className2 = "java.lang.Integer";

		// TODO: consider double

		return className1.equals(className2);
	}

	private static boolean assignGrtListToGrtObject(GrtObject obj,
			Method setter, GrtList sourceList, Class objListClass) {

		try {
			Constructor con = objListClass
					.getConstructor(new Class[] { GrtObject.class });
			GrtList paramList = (GrtList) con.newInstance(new Object[] { obj });

			Class paramListContentClass = Class.forName(GrtPackagePrefix
					+ paramList.getContentStructName());

			Class sourceListContentClass = Class.forName(GrtPackagePrefix
					+ sourceList.getContentStructName());

			if (paramListContentClass.isAssignableFrom(sourceListContentClass)) {
				for (int k = 0; k < sourceList.size(); k++) {
					paramList.addObject(sourceList.getObject(k));
				}

				setter.invoke(obj, new Object[] { paramList });

				return true;
			}
		} catch (Exception e) {
		}

		return false;
	}

	/**
	 * Parses a given XML Node of the value type DICT Calls other functions
	 * recursive to get the complete information
	 * 
	 * @param valueNode
	 *            XML node to parse
	 * @param parent
	 *            the parent object of this node (if any)
	 * @return the created Object
	 */
	private static Object getDictFromGrtXmlNode(Node valueNode, Object parent) {
		String structName = ((Element) valueNode).getAttribute("struct-name");
		String contentType = ((Element) valueNode).getAttribute("content-type");
		String contentStructName = ((Element) valueNode)
				.getAttribute("content-struct-name");
		String globalObjectPath = ((Element) valueNode)
				.getAttribute("global-object-path");

		if ((structName != null) && (structName != "")) {
			try {
				Class c = Class.forName(GrtPackagePrefix + structName);
				GrtObject obj;

				// if this is a regular object
				if (globalObjectPath.equals("")) {
					Constructor con = c
							.getConstructor(new Class[] { Object.class });
					obj = (GrtObject) con.newInstance(new Object[] { null });
				} else {
					// if this is a global object
					Constructor con = c
							.getConstructor(new Class[] { String.class });
					obj = (GrtObject) con
							.newInstance(new Object[] { globalObjectPath });
				}

				NodeList valueChildNodeList = valueNode.getChildNodes();

				for (int i = 0; i < valueChildNodeList.getLength(); i++) {
					Node childValueNode = valueChildNodeList.item(i);

					// add all child values to dict
					// only work with real value nodes
					if ((childValueNode.getNodeType() == Node.ELEMENT_NODE)
							&& (((Element) childValueNode).getTagName()
									.equals("value"))) {
						// check if returned object is not null
						Object item = getObjectsFromGrtXmlNode(childValueNode,
								obj);

						if (item != null) {
							String setter = ((Element) childValueNode)
									.getAttribute("key");

							String option = ((Element) childValueNode)
									.getAttribute("option");

							String itemType = ((Element) childValueNode)
									.getAttribute("type");

							// build setter name
							if (setter.length() == 0) {
								continue;
							} else if (setter.length() == 1) {
								setter = setter.toUpperCase();
							} else {
								setter = Character
										.toUpperCase(setter.charAt(0))
										+ setter.substring(1);
							}
							setter = "set" + setter;
							if ((option != null) && (option.equals("ref"))
									&& !itemType.equalsIgnoreCase("list"))
								setter = setter + "ById";

							// loop over all publicMethods and see if there
							// is a setKey() method with the correct
							// parameter
							Method[] methods = c.getMethods();
							for (int j = 0; j < methods.length; j++) {

								if ((methods[j].getName().equals(setter))
										&& (methods[j].getParameterTypes().length > 0)) {

									// check if the classes are both inherited
									// from GrtList
									if (GrtList.class.isAssignableFrom(item
											.getClass())
											&& GrtList.class
													.isAssignableFrom(methods[j]
															.getParameterTypes()[0])) {
										// if so, try to assign the lists
										if (assignGrtListToGrtObject(
												obj,
												methods[j],
												(GrtList) item,
												methods[j].getParameterTypes()[0]))
											break;

									}

									if ((checkSameClass(methods[j]
											.getParameterTypes()[0], item
											.getClass()))) {

										methods[j].invoke(obj,
												new Object[] { item });

										break;
									}
								}
							}
						}
					}
				}

				return obj;
			} catch (Exception e) {
				e.printStackTrace();
				return null;
			}
		} else {
			GrtHashMap dict;

			if (contentType.equals("string"))
				dict = new GrtStringHashMap(globalObjectPath);
			else
				dict = new GrtHashMap(contentStructName, globalObjectPath);
			NodeList valueChildNodeList = valueNode.getChildNodes();

			for (int i = 0; i < valueChildNodeList.getLength(); i++) {
				Node childValueNode = valueChildNodeList.item(i);

				// add all child values to dict
				// only work with real value nodes
				if ((childValueNode.getNodeType() == Node.ELEMENT_NODE)
						&& (((Element) childValueNode).getTagName()
								.equals("value"))) {
					// check if returned object is not null
					Object item = getObjectsFromGrtXmlNode(childValueNode, dict);

					if (item != null)
						dict.addObject(((Element) childValueNode)
								.getAttribute("key"), item);
				}
			}

			return dict;
		}

	}

	/**
	 * Parses a given XML Node of the value type LIST Calls other functions
	 * recursive to get the complete information
	 * 
	 * @param valueNode
	 *            XML node to parse
	 * @param parent
	 *            the parent object of this node (if any)
	 * @return the created Object
	 */
	private static Object getListFromGrtXmlNode(Node valueNode, Object parent) {
		GrtList list = null;
		String contentType = ((Element) valueNode).getAttribute("content-type");
		String contentStructName = ((Element) valueNode)
				.getAttribute("content-struct-name");
		String globalObjectPath = ((Element) valueNode)
				.getAttribute("global-object-path");
		int valueListContentType = getValueTypeFromString(contentType);
		boolean objectRefList = false;

		// TODO: the following is a hack, it should be implemented using a
		// ObjectRefList instead
		//
		// for global reference list with the content type string and a content
		// struct name simulate an object list
		if ((globalObjectPath != null) && !globalObjectPath.equals("")
				&& (valueListContentType == Grt.MYX_STRING_VALUE)
				&& (contentStructName != null) && (contentStructName != "")) {
			valueListContentType = Grt.MYX_DICT_VALUE;
		} else if (((globalObjectPath == null) || globalObjectPath.equals(""))
				&& (valueListContentType == Grt.MYX_STRING_VALUE)
				&& (contentStructName != null) && (contentStructName != "")) {
			valueListContentType = Grt.MYX_DICT_VALUE;
			objectRefList = true;
		}

		// if a content-struct-name was assigned, create list of given type
		if ((valueListContentType == Grt.MYX_DICT_VALUE)
				&& (contentStructName != null) && (contentStructName != "")) {
			try {
				Class c = Class.forName(GrtPackagePrefix + contentStructName
						+ "List");
				if (globalObjectPath.equals("")) {
					Constructor con = c
							.getConstructor(new Class[] { String.class });
					list = (GrtList) con
							.newInstance(new Object[] { contentStructName });
				} else {
					Constructor con = c.getConstructor(new Class[] {
							String.class, String.class });
					list = (GrtList) con.newInstance(new Object[] {
							contentStructName, globalObjectPath });
				}
			} catch (Exception e) {
				e.printStackTrace();
				return null;
			}
			// if content-type is "string", create a string list
		} else if (contentType.equals("string")) {
			list = new GrtStringList(globalObjectPath);
		} else {
			list = new GrtList(contentStructName, globalObjectPath);

			list.setContentType(contentType);
		}

		// add all list items to the list
		NodeList valueChildNodeList = valueNode.getChildNodes();

		for (int i = 0; i < valueChildNodeList.getLength(); i++) {
			Node childValueNode = valueChildNodeList.item(i);

			// add all child values to list
			// only work with real value nodes
			if ((childValueNode.getNodeType() == Node.ELEMENT_NODE)
					&& (((Element) childValueNode).getTagName().equals("value"))) {
				// check if returned object is not null
				Object item = getObjectsFromGrtXmlNode(childValueNode, list);

				// if this is a objectRefList, take real object instead
				if (objectRefList) {
					item = Grt.getInstance().getObjectByRefId((String) item);
				}

				if (item != null)
					list.addObject(item);
			}
		}

		if (list != null)
			list.setOwner(parent);

		return list;
	}

	/**
	 * Parses a given XML Node of any value type Calls other functions recursive
	 * to get the complete information
	 * 
	 * @param valueNode
	 *            XML node to parse
	 * @param parent
	 *            the parent object of this node (if any)
	 * @return the created Object
	 */
	private static Object getObjectsFromGrtXmlNode(Node valueNode, Object parent) {
		int valueNodeType = getValueTypeFromString(((Element) valueNode)
				.getAttribute("type"));

		if (valueNodeType == Grt.MYX_STRING_VALUE) {
			Node childNode = valueNode.getChildNodes().item(0);

			while (childNode != null) {
				if (childNode.getNodeType() == Node.TEXT_NODE) {
					String value = ((Text) childNode).getData();

					if (value.startsWith("global::"))
						return Grt.getInstance().getGrtGlobalAsObject(
								value.substring(8));
					else
						return value;
				}
				childNode = childNode.getNextSibling();
			}

			return "";
		} else if (valueNodeType == Grt.MYX_INT_VALUE) {
			Node childNode = valueNode.getChildNodes().item(0);
			do {
				if (childNode.getNodeType() == Node.TEXT_NODE) {
					return new Integer(Integer.parseInt(((Text) childNode)
							.getData()));
				}
				childNode = childNode.getNextSibling();
			} while (childNode != null);

			return new Integer(0);
		} else if (valueNodeType == Grt.MYX_REAL_VALUE) {
			Node childNode = valueNode.getChildNodes().item(0);
			do {
				if (childNode.getNodeType() == Node.TEXT_NODE) {
					return new Double(Double.parseDouble(((Text) childNode)
							.getData()));
				}
				childNode = childNode.getNextSibling();
			} while (childNode != null);

			return new Double(0);
		} else if (valueNodeType == Grt.MYX_DICT_VALUE) {
			return getDictFromGrtXmlNode(valueNode, parent);
		} else if (valueNodeType == Grt.MYX_LIST_VALUE) {
			return getListFromGrtXmlNode(valueNode, parent);
		} else
			return null;
	}

	/**
	 * Parses a given Grt XML string
	 * 
	 * @param xml
	 *            the Grt XML string to parse
	 * @return the created Object
	 */
	public static Object getObjectsFromGrtXml(String xml) {
		Document document;
		ByteArrayInputStream stream;

		Object obj = null;

		if (xml == null)
			return null;

		try {
			stream = new ByteArrayInputStream(xml.getBytes("UTF-8"));
		} catch (UnsupportedEncodingException e) {
			return null;
		}

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		// factory.setValidating(true);
		// factory.setNamespaceAware(true);
		try {
			DocumentBuilder builder = factory.newDocumentBuilder();
			document = builder.parse(stream);

			Element root = document.getDocumentElement();
			NodeList rootNodeList = root.getChildNodes();

			// Find first element node
			for (int i = 0; i < rootNodeList.getLength(); i++) {
				Node rootNode = rootNodeList.item(i);

				if ((rootNode.getNodeType() == Node.ELEMENT_NODE)
						&& (((Element) rootNode).getTagName().equals("value"))) {
					obj = getObjectsFromGrtXmlNode(rootNode, null);
					break;
				}
			}

		} catch (SAXException sxe) {
			// Error generated during parsing)
			Exception x = sxe;
			if (sxe.getException() != null)
				x = sxe.getException();
			x.printStackTrace();

		} catch (ParserConfigurationException pce) {
			// Parser with specified options can't be built
			pce.printStackTrace();

		} catch (IOException ioe) {
			// I/O error
			ioe.printStackTrace();
		}

		return obj;
	}

	/**
	 * Returns the XML representation of the given object
	 * 
	 * @param obj
	 *            the object to serialize
	 * @return the XML string
	 */
	public static StringBuffer getObjectAsXml(StringBuffer xml, Object obj) {
		return getObjectAsXml(xml, obj, "");
	}

	/**
	 * Returns the XML representation of the given object
	 * 
	 * @param obj
	 *            the object to serialize
	 * @param keyTag
	 *            the key tag with a leading space if the object is a member of
	 *            a DICT
	 * @return the XML string
	 */
	public static StringBuffer getObjectAsXml(StringBuffer xml, Object obj,
			String keyTag) {

		if (obj != null) {
			// get XML from GrtObjects, GrtLists and GrtHashMaps
			if (obj instanceof GrtXml) {
				((GrtXml) obj).getGrtXml(xml, keyTag);
			} else {
				// get XML for primitive types
				if (obj.getClass() == String.class) {
					xml
							.append("<value type=\"string\"" + keyTag + ">"
									+ escapeStringForXml(obj.toString())
									+ "</value>\n");
				} else if (obj.getClass() == Integer.class) {
					xml.append("<value type=\"int\"" + keyTag + ">"
							+ obj.toString() + "</value>\n");
				} else if (obj.getClass() == Double.class) {
					xml.append("<value type=\"real\"" + keyTag + ">"
							+ obj.toString() + "</value>\n");
				}
			}
		}

		return xml;
	}

	public static String escapeStringForXml(String s) {
		return s.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(
				">", "&gt;").replaceAll("\"", "&quot;")
				.replaceAll("\u0000", "");
	}

	/**
	 * Builds a Grt XML module info of the given class which can be returned to
	 * the Grt environment.
	 * 
	 * @param module
	 *            the class that represents a Grt module
	 * @param extendsModule
	 *            name of the Grt module this class extends
	 * @return returns a GRT XML string containing the infos about this class
	 */
	public static String getModuleInfoXml(Class module, String extendsModule) {
		StringBuffer xml = prepareGrtXmlHeader("<value type=\"dict\">\n"
				+ "  <value key=\"name\" type=\"string\">"
				+ module.getName()
				+ "</value>"
				+ "  <value key=\"functions\" type=\"list\" content-type=\"string\">\n");

		// List public static methods that take a String parameter and return a
		// String
		Method[] methods = module.getMethods();

		for (int i = 0; i < methods.length; i++) {
			if ((Modifier.isStatic(methods[i].getModifiers()))
					&& (!methods[i].getName().equals("getModuleInfo"))) {
				xml.append("    <value type=\"string\">" + methods[i].getName()
						+ ":(");

				Class[] params = methods[i].getParameterTypes();
				for (int j = 0; j < params.length; j++) {
					if (params[j].getName().equals("int"))
						xml.append("int;");
					if (params[j].getName().equals("double"))
						xml.append("double;");
					else
						xml.append("L" + params[j].getName().replace('.', '/')
								+ ";");
				}

				xml.append("):</value>\n");
			}
		}

		xml.append("</value>\n");

		if (!extendsModule.equals(""))
			xml.append("  <value key=\"extends\" type=\"string\">"
					+ extendsModule + "</value>\n");

		xml.append("</value>\n");

		return prepareGrtXmlFooter(xml).toString();
	}

	/**
	 * Builds a Grt XML string containing an error message. Can be used for Grt
	 * modules to return an error to the Grt environment
	 * 
	 * @param error
	 *            the error message
	 * @return returns a GRT XML string containing the error
	 */
	public static String moduleFunctionError(String error) {
		GrtHashMap result = new GrtHashMap();
		result.addObject("error", error);
		return prepareGrtXmlFooter(
				getObjectAsXml(prepareGrtXmlHeader(), result)).toString();
	}

	/**
	 * Builds a Grt XML string containing an error message. Can be used for Grt
	 * modules to return an error to the Grt environment
	 * 
	 * @param e
	 *            a Throwable object
	 * @return returns a GRT XML string containing the error
	 */
	public static String moduleFunctionError(Throwable e) {
		GrtHashMap res = new GrtHashMap();

		String errorMessage = e.getMessage();
		if (errorMessage == null) {
			e = e.getCause();
			errorMessage = e.getMessage();

			if (errorMessage == null)
				errorMessage = e.getClass().getName();
		}

		StackTraceElement[] stack = e.getStackTrace();

		String stackTrace = "";
		for (int i = 0; i < stack.length; i++) {
			stackTrace = stackTrace + stack[i].toString() + "\n";
		}

		res.addObject("error", errorMessage);
		res.addObject("detail", stackTrace);

		return prepareGrtXmlFooter(getObjectAsXml(prepareGrtXmlHeader(), res))
				.toString();
	}

	/**
	 * Calls a Java module function
	 * 
	 * @param module
	 *            the class that represents the module
	 * @param functionName
	 *            name of the function to call
	 * @param functionSignature
	 *            java signature of the function to call
	 * @param xml
	 *            the arguments of the function represented in GRT xml
	 * @return returns a GRT XML string
	 */
	public static String callModuleFunction(Class module, String functionName,
			String functionSignature, String xml) {

		// if functionSignature start with a (, remove ( and only take string to
		// the
		// ) character
		if (functionSignature.startsWith("(")) {
			int index = functionSignature.indexOf(')');
			if (index >= 0)
				functionSignature = functionSignature.substring(1, index);
		}

		// re-build the list of parameters
		Class[] paramTypes = null;
		if (!functionSignature.equals("") && !functionSignature.equals("()")) {
			String[] paramTypeString = functionSignature.split(";");

			paramTypes = new Class[paramTypeString.length];

			for (int i = 0; i < paramTypeString.length; i++) {
				String className = paramTypeString[i].substring(1).replace('/',
						'.');
				try {
					paramTypes[i] = Class.forName(className);
				} catch (ClassNotFoundException e) {
					return moduleFunctionError(e.getMessage());
				}
			}
		}

		// search for the method
		Method method = null;
		try {
			method = module.getMethod(functionName, paramTypes);
		} catch (Exception e) {
			return moduleFunctionError(e);
		}

		// convert Grt values to java objects
		Object[] params = null;
		if (paramTypes != null) {
			Object grtParams = Grt.getObjectsFromGrtXml(xml);

			// if there is only one parameter, tollerate other
			// objects than GrtLists
			if ((paramTypes.length == 1) && (!(grtParams instanceof GrtList))) {
				params = new Object[1];
				params[0] = grtParams;
			} else if (grtParams instanceof GrtList) {
				// if there is more than one parameter, a GrtList
				// must be used
				GrtList grtParamsList = (GrtList) grtParams;

				// make sure number of parameters match
				if (paramTypes.length != grtParamsList.size()) {
					return moduleFunctionError("The number of submitted "
							+ "parameters does not match the "
							+ "number of parameters of the java function.");
				}

				params = new Object[grtParamsList.size()];
				for (int i = 0; i < grtParamsList.size(); i++) {
					params[i] = grtParamsList.getObject(i);
				}
			} else {
				return moduleFunctionError("If the function has more "
						+ "than one parameter these parameters "
						+ "have to be passed in a list.");
			}
		}

		// call method
		Object resultValue = null;
		try {
			resultValue = method.invoke(null, params);
		} catch (Exception e) {
			return moduleFunctionError(e);
		}

		// return value
		GrtHashMap result = new GrtHashMap();
		result.addObject("value", resultValue);
		String xmlResult = prepareGrtXmlFooter(
				getObjectAsXml(prepareGrtXmlHeader(), result)).toString();

		if (Grt.getInstance().doJava2GrtXmlLogging == 1) {
			try {
				boolean logDirExists = false;
				File dir = new File(Grt.getInstance().getApplicationDataPath()
						+ "log");

				if (dir.exists() && dir.isDirectory()) {
					logDirExists = true;
				} else {
					logDirExists = dir.mkdir();
				}

				if (logDirExists) {
					FileWriter xmlFile = new FileWriter(Grt.getInstance()
							.getApplicationDataPath()
							+ "log/Java2Grt_"
							+ module.getName()
							+ "."
							+ functionName + ".xml");
					xmlFile.write(xmlResult);
					xmlFile.close();
				}
			} catch (Exception e) {
			}
		}

		// clear object cache
		Grt.getInstance().clearObjectCache();

		// flush messages if there are any
		Grt.getInstance().flushMessages();

		return xmlResult;
	}

	/**
	 * Calls a GRT module function
	 * 
	 * @param module
	 *            the GRT module that represents the module
	 * @param functionName
	 *            name of the function to call
	 * @param arguments
	 *            the arguments of the function represented in GRT xml
	 * @return returns the result of the function call
	 */
	public Object callGrtFunction(String module, String functionName,
			Object arguments) throws RuntimeException {
		String argumentsXml = prepareGrtXmlFooter(
				getObjectAsXml(prepareGrtXmlHeader(), arguments)).toString();
		String resultXml = getCallback().callGrtFunction(getMyxGrtPointer(),
				module, functionName, argumentsXml);

		Object result = getObjectsFromGrtXml(resultXml);

		if (result instanceof GrtHashMap) {
			Object value = ((GrtHashMap) result).getObject("value");

			if (value != null) {
				return value;
			} else {
				String exceptionMessage;
				value = ((GrtHashMap) result).getObject("error");

				if (value instanceof String) {
					exceptionMessage = "The function " + module + ":"
							+ functionName + "() "
							+ "returned the following error:\n"
							+ ((String) value);
				} else {
					exceptionMessage = "The function " + module + ":"
							+ functionName + "() " + "returned an error.";
				}

				throw new RuntimeException(exceptionMessage);
			}

		} else {
			return null;
		}
	}

	/**
	 * This function looks up a global object from the GRT global object tree by
	 * its path and returns it as a GrtObject
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * 
	 * @return returns a global GrtObject from the GRT global object tree
	 */
	public GrtObject getGrtGlobalAsGrtObject(String objectPath) {
		return (GrtObject) getGrtGlobalAsObject(objectPath);
	}

	/**
	 * This function looks up a global object from the GRT global object tree by
	 * its path and returns it as a Object
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * 
	 * @return returns a global Object from the GRT global object tree
	 */
	public Object getGrtGlobalAsObject(String objectPath) {
		return getCallback().getGrtGlobalAsObject(getMyxGrtPointer(),
				objectPath);
	}

	/**
	 * This function looks up a global object from the GRT global object tree by
	 * its path and returns it as a String
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * 
	 * @return returns a String looked up from the GRT global object tree
	 */
	public String getGrtGlobalAsString(String objectPath) {
		return getCallback().getGrtGlobalAsString(getMyxGrtPointer(),
				objectPath);
	}

	/**
	 * This function looks up a global object from the GRT global object tree by
	 * its path and returns it as a int
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * 
	 * @return returns an int looked up from the GRT global object tree
	 */
	public int getGrtGlobalAsInt(String objectPath) {
		return getCallback().getGrtGlobalAsInt(getMyxGrtPointer(), objectPath);
	}

	/**
	 * This function looks up a global object from the GRT global object tree by
	 * its path and returns it as a double
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * 
	 * @return returns a double looked up from the GRT global object tree
	 */
	public double getGrtGlobalAsReal(String objectPath) {
		return getCallback().getGrtGlobalAsReal(getMyxGrtPointer(), objectPath);
	}

	/**
	 * This function sets a global object from the GRT global object tree to the
	 * given object value
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * @param value
	 *            the new value of the object
	 */
	public void setGrtGlobalFromObject(String objectPath, Object value) {
		getCallback().setGrtGlobalFromObject(getMyxGrtPointer(), objectPath,
				value);
	}

	/**
	 * This function sets a global object from the GRT global object tree to the
	 * given String value
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * @param value
	 *            the new value of the object
	 */
	public void setGrtGlobalFromString(String objectPath, String value) {
		getCallback().setGrtGlobalFromString(getMyxGrtPointer(), objectPath,
				value);
	}

	/**
	 * This function sets a global object from the GRT global object tree to the
	 * given int value
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * @param value
	 *            the new value of the object
	 */
	public void setGrtGlobalFromInt(String objectPath, int value) {
		getCallback()
				.setGrtGlobalFromInt(getMyxGrtPointer(), objectPath, value);
	}

	/**
	 * This function sets a global object from the GRT global object tree to the
	 * given double value
	 * 
	 * @param objectPath
	 *            path of the object in the GRT global object tree
	 * @param value
	 *            the new value of the object
	 */
	public void setGrtGlobalFromReal(String objectPath, double value) {
		getCallback().setGrtGlobalFromReal(getMyxGrtPointer(), objectPath,
				value);
	}

	public int getGrtGlobalListSize(String objectPath) {
		return getCallback().getGrtGlobalListSize(getMyxGrtPointer(),
				objectPath);
	}

	public Object getGrtGlobalListItem(String objectPath, int index) {
		return getCallback().getGrtGlobalListItem(getMyxGrtPointer(),
				objectPath, index);
	}

	public void addGrtGlobalListItem(String objectPath, Object item) {
		getCallback()
				.addGrtGlobalListItem(getMyxGrtPointer(), objectPath, item);
	}

	public void removeGrtGlobalListItem(String objectPath, int index) {
		getCallback().removeGrtGlobalListItem(getMyxGrtPointer(), objectPath,
				index);
	}

	public Object getGrtGlobalDictItem(String objectPath, String key) {
		return getCallback().getGrtGlobalDictItem(getMyxGrtPointer(),
				objectPath, key);
	}

	public void addGrtGlobalDictItem(String objectPath, String key, Object item) {
		getCallback().addGrtGlobalDictItem(getMyxGrtPointer(), objectPath, key,
				item);
	}

	public void removeGrtGlobalDictItem(String objectPath, String key) {
		getCallback().removeGrtGlobalDictItem(getMyxGrtPointer(), objectPath,
				key);
	}

	public int getGrtGlobalDictKeyCount(String objectPath) {
		return getCallback().getGrtGlobalDictKeyCount(getMyxGrtPointer(),
				objectPath);
	}

	public String getGrtGlobalDictKey(String objectPath, int index) {
		return getCallback().getGrtGlobalDictKey(getMyxGrtPointer(),
				objectPath, index);
	}

	public void setBridgeDataObject(String objectPath, Object dataObj) {
		getCallback().setBridgeDataObject(getMyxGrtPointer(), objectPath,
				dataObj);
	}

	public Object getBridgeDataObject(String objectPath) {
		return getCallback()
				.getBridgeDataObject(getMyxGrtPointer(), objectPath);
	}
}