<?php
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

include('Grt.php');

	/**
	 * Workaround for broken ReflectionMethod->InvokeArgs(null, ***)
	 * 
	 * @param file
	 *            name of the file in which the assertion failed
	 * @param line
	 *            number of the line in which the assertion failed
	 * @param code
	 *            the code that caused the assertion to fail
	 */
function grtAssertHandler($file, $line, $code)
{
   echo "Assertion Failed:\n File '$file' \nLine '$line'\nCode '$code'\n";
}

	/**
	 * Autoload function to simulate Java packages
	 *
	 *   To be able to use the same file structure as for Java the autoload function
	 * simulates Java packages. A class with the name com_mysql_grt_db_Column is loaded
	 * from ./db/Columns.php. The Grt::GrtPackagePrefix is stripped from the file.
	 * 
	 * @param class_name
	 *            name of class to be loaded
	 */
function __autoload($class_name) {
	if (($class_name != '') && ($class_name != null)) {
	
		if (substr_count($class_name, '_') == 0) {
			$filename = getcwd() . '/' . $class_name . '.php';
		} else {
    		$filename = getcwd() . '/' . str_replace('_', '/', 
	        	substr($class_name, strlen(Grt::GrtPackagePrefix))) . '.php';
		}
	
	    require_once $filename;
	}
}

	/**
	 * Workaround for broken ReflectionMethod->InvokeArgs(null, ***)
	 * 
	 * @param method
	 *            the static ReflectionMethod that should be called
	 * @param params
	 *            the params of the method in an array() 
	 * 
	 * @return returns the return value of the called function
	 */
function reflectionMethodInvokeArgsStatic($method, $params) {
	$paramCount = count($params);
	$resultValue = null;
	
	switch ($paramCount) {
		case 0:			
			$resultValue = $method->invoke(null, null);
			break;
		case 1:
			$resultValue = $method->invoke(null, $params[0]);
			break;
		case 2:
			$resultValue = $method->invoke(null, $params[0], $params[1]);
			break;
		case 3:
			$resultValue = $method->invoke(null, $params[0], $params[1], $params[2]);
			break;
		case 4:
			$resultValue = $method->invoke(null, $params[0], $params[1], $params[2], $params[3]);
			break;
		case 5:
			$resultValue = $method->invoke(null, $params[0], $params[1], $params[2], $params[3], $params[4]);
			break;
		case 6:
			$resultValue = $method->invoke(null, $params[0], $params[1], $params[2], $params[3], $params[4], 
					$params[5]);
			break;
		case 7:
			$resultValue = $method->invoke(null, $params[0], $params[1], $params[2], $params[3], $params[4], 
					$params[5], $params[6]);
			break;
		case 8:
			$resultValue = $method->invoke(null, $params[0], $params[1], $params[2], $params[3], $params[4], 
					$params[5], $params[6], $params[7]);
			break;
		case 9:
			$resultValue = $method->invoke(null, $params[0], $params[1], $params[2], $params[3], $params[4], 
					$params[5], $params[6], $params[7], $params[8]);
			break;
		case 10:
			$resultValue = $method->invoke(null, $params[0], $params[1], $params[2], $params[3], $params[4], 
					$params[5], $params[6], $params[7], $params[8], $params[9]);
			break;
	}
	
	return $resultValue;
}

	/**
	 * Checks if the object is an instance of the given class or 
	 * subclass
	 * 
	 * @param $obj
	 *            the object to check
	 * @param $class_name
	 *            name of the class
	 * 
	 * @return returns 1 if the object is an instance
	 */
function isInstanceOrSubclass($obj, $class_name) {
	$class = new ReflectionClass($class_name);
	
	if (($class->isInstance($obj)) || (is_subclass_of($obj, $class_name))) {
		return 1;
	}
	
	return 0;
}

?>