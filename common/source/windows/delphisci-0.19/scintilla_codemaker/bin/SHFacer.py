# PasFacer.py - Updates pascal units used to create an object pascal scintilla wrapper.

import string
import sys
import os
import SHFace

def Contains(s,sub):
	return string.find(s, sub) != -1

def pType(s):
	if s == "bool":
		return "bool"
	elif s == "position":
		return "long"
	elif s == "colour":
		return "COLORREF"
	elif s == "string":
		return "const char*"
	elif s == "stringresult":
		return "char*"
	elif s == "cells":
		return "char*"
	elif s == "textrange":
		return "TextRange*"
	elif s == "findtext":
		return "TextToFind*"
	elif s == "keymod":
		return "DWORD"
	elif s == "formatrange":
		return "long"
	elif s == "int":
		return "int"
	return ""

def fixConstant(c):
	#r = string.replace(c, "|", "or")
	#r = string.replace(r, "0x", "$")
	return c

def printDefines(f, out):
	for name in f.order:
		v = f.features[name]
		if v["Category"] != "Deprecated":
			if v["FeatureType"] in ["fun", "get", "set"]:
				featureDefineName = "SCI_" + string.upper(name)
				out.write("#define " + featureDefineName + " " + fixConstant(v["Value"]) + ";\n")
			elif v["FeatureType"] in ["evt"]:
				featureDefineName = "SCN_" + string.upper(name)
				out.write("#define " + featureDefineName + " " + fixConstant(v["Value"]) + ";\n")
			elif v["FeatureType"] in ["val"]:
				featureDefineName = string.upper(name)
				out.write("#define " + featureDefineName + " " + fixConstant(v["Value"]) + ";\n")

def genfuncPrototype(name, fun):
	retval = name
	retval = retval + "("
	bp1 = 0
	if fun["Param1Type"] != "" and fun["Param1Type"] != " ":
		retval = retval + pType(fun["Param1Type"]) + " " + fun["Param1Name"]
		bp1 = 1
	if fun["Param2Type"] != "" and fun["Param2Type"] != " ":
		if bp1 == 1:
			retval = retval + ", "
		retval = retval + pType(fun["Param2Type"]) + " " + fun["Param2Name"] 
	retval = retval + ")"
	return retval

def getfuncHeader(name, fun, extra):
	retval = genfuncPrototype(name, fun)
	line = ""
	if pType(fun["ReturnType"]) == "":
		line = line + "void " + extra + retval
	else:
		line = line + pType(fun["ReturnType"]) + " " + extra + retval
	return line

def printFunctionDefs(f, out):
	for name in f.order:
		v = f.features[name]
		if v["Category"] != "Deprecated":
			if v["FeatureType"] in ["fun", "set", "get"]:
				line = ""
				if not v["Comment"] in ["", " "]:
					line = line + "		/**\n";
					
					for cline in v["Comment"]:
						line = line + "		 * " + cline + "\n";
					line = line + "		 */\n		";
				else:
					line = "		";
				line = line + getfuncHeader(name, v, "") + ";\n"
				out.write(line)

def isFunction(v):
	if pType(v["ReturnType"]) == "":
		ret = 0
	else:
		ret = 1
	return ret

def printFunctionImpl(f, out):
	for name in f.order:
		v = f.features[name]
		if v["Category"] != "Deprecated":
			if v["FeatureType"] in ["fun", "set", "get"]:
				header = getfuncHeader(name, v, "CScintilla::") + "\n"
				out.write(header)
				out.write("{\n")
				line = "	"
				endline = ""
				if not isFunction(v):
					line = line + "SPerform("
				else:
					line = line + "return "
					if pType(v["ReturnType"]) != "long":
						if pType(v["ReturnType"]) != "bool":
							line = line + "(" + pType(v["ReturnType"]) + ")"
						else:
							endline = " != 0"
					line = line + "SPerform("
				line = line + "SCI_" + string.upper(name) + ", "
				if not (v["Param1Type"] in ["", " "]):
					add = ""
					if pType(v["Param1Type"]) != "long":
						line = line + "(long)"
					line= line + v["Param1Name"] + add + ", "
				else:
					line = line + "0, "
				if not (v["Param2Type"] in ["", " "]):
					add = ""
					if pType(v["Param2Type"]) != "long":
						line = line + "(long)"
					line = line + v["Param2Name"] + add + ")"
				else:
					line = line + "0)"
				line = line + endline + ";"
				out.write(line + "\n}\n\n")

def printInlineFunctionImpl(f, out):
	for name in f.order:
		v = f.features[name]
		if v["Category"] != "Deprecated":
			if v["FeatureType"] in ["fun", "set", "get"]:
			
				line = ""
				indent = "\t\t"
				indent2 = "\t\t\t"
			
				if not v["Comment"] in ["", " "]:
					line = indent + "/**\n";
					
					for cline in v["Comment"]:
						line = line + indent + " * " + cline + "\n";
					line = line + indent + " */\n";
				
				out.write(line)
			
				header = getfuncHeader(name, v, "") + "\n"
				out.write(indent + header)
				out.write(indent + "{\n")
				
				line = ""
				endline = ""
				if not isFunction(v):
					line = line + "SPerform("
				else:
					line = line + "return "
					if pType(v["ReturnType"]) != "long":
						if pType(v["ReturnType"]) != "bool":
							line = line + "(" + pType(v["ReturnType"]) + ")"
						else:
							endline = " != 0"
					line = line + "SPerform("
				line = line + "SCI_" + string.upper(name) + ", "
				if not (v["Param1Type"] in ["", " "]):
					add = ""
					if pType(v["Param1Type"]) != "long":
						line = line + "(long)"
					line= line + v["Param1Name"] + add + ", "
				else:
					line = line + "0, "
				if not (v["Param2Type"] in ["", " "]):
					add = ""
					if pType(v["Param2Type"]) != "long":
						line = line + "(long)"
					line = line + v["Param2Name"] + add + ")"
				else:
					line = line + "0)"
				line = line + endline + ";"
				out.write(indent2 + line + "\n" + indent + "}\n\n")

def genMainControl(input, output, definition):
	copying = 1
	for line in input.readlines():
		if copying:
			output.write(line)
		#if Contains(line, "//++Autogenerated"):
		#	copying = 0
		#	genfn(definition, output)
		if Contains(line, "//++EventTypes"):
			copying = 0
			printEventDefs(definition, output)
		if Contains(line,"//++EventPrivates"):
			copying = 0
			printEventPrivates(definition, output)
		if Contains(line, "//++EventProperties"):
			copying = 0
			printEventProperties(definition, output)
		if Contains(line,"//++FuncDef"):
			copying = 0
			printFunctionDefs(definition, output)
		if Contains(line,"//++Const"):
			copying = 0
			printDefines(definition, output)
		if Contains(line,"//++FuncImp"):
			copying = 0
			printFunctionImpl(definition, output)
		if Contains(line,"//++InlineFuncImp"):
			copying = 0
			printInlineFunctionImpl(definition, output)
		if Contains(line,"//++EventImpl"):
			copying = 0
			printEventImpl(definition, output)
		if Contains(line,"//--"):
			copying = 1
			output.write(line)

def genConsts(input, output, definition):
	copying = 1
	for line in input.readlines():
		if copying:
			output.write(line)
		if Contains(line,"//++Const"):
			copying = 0
			printDefines(definition, output)
		if Contains(line,"//--") and copying == 0:
			copying = 1
			output.write(line)

def Regenerate(filename, outputfilename, definition, fn):
	tempname = "CPPFacer.tmp"
	out = open(tempname,"w")
	hfile = open(filename)
	#CopyWithInsertion(hfile, out, definition)
	fn(hfile, out, definition)
	out.close()
	hfile.close()
	if(os.access(outputfilename, os.F_OK)):
		os.unlink(outputfilename)
	os.rename(tempname, outputfilename)

# Program Start
f = SHFace.Face()
f.ReadFromFile("..\\include\\Scintilla.iface")
Regenerate("..\\originals\\scintillaif.cpp", "..\\cpp\\scintillaif.cpp", f, genMainControl)
Regenerate("..\\originals\\scintillaif.h", "..\\cpp\\scintillaif.h", f, genMainControl)
Regenerate("..\\originals\\atlscintilla.h", "..\\cpp\\atlscintilla.h", f, genMainControl)