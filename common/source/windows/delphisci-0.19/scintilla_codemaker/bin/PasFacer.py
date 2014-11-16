# PasFacer.py - Updates pascal units used to create an object pascal scintilla wrapper.

import string
import sys
import os
import PFace
import getopt

scriptlang="vb"
scriptcommentstart="'"
scriptnolexers=1
scriptonlyvalues=1
outputscripts=0
verbose=0
scriptdest="c:\\myeditorole\\scripts\\"
scriptsrcdir="..\\originals\\"
pascaldestdir="..\\pascal\\"
constantfilename="sciconsts"

def Contains(s,sub):
	return string.find(s, sub) != -1

def pType(s):
	if s == "bool":
		return "Boolean"
	elif s == "position":
		return "LongInt"
	elif s == "colour":
		return "TColor"
	elif s == "string":
		return "PChar"
	elif s == "stringresult":
		return "PChar"
	elif s == "cells":
		return "PChar"
	elif s == "textrange":
		return "PTextRange"
	elif s == "findtext":
		return "PTextToFind"
	elif s == "keymod":
		return "LongInt"
	elif s == "formatrange":
		return "PRangeToFormat"
	elif s == "int":
		return "LongInt"
	return ""

def fixConstant(c):
	r = string.replace(c, "|", "or")
	r = string.replace(r, "0x", "$")
	return r

def fixConstantByLang(c,oroperator,hexoperator):
	r = string.replace(c, "|", oroperator)
	r = string.replace(r, "0x", hexoperator)
	return r

def printDefines(f, out):
	for name in f.order:
		v = f.features[name]
		if v["Category"] != "Deprecated":
			if v["FeatureType"] in ["fun", "get", "set"]:
				featureDefineName = "SCI_" + string.upper(name)
				out.write("    " + featureDefineName + " = " + fixConstant(v["Value"]) + ";\n")
			elif v["FeatureType"] in ["evt"]:
				featureDefineName = "SCN_" + string.upper(name)
				out.write("    " + featureDefineName + " = " + fixConstant(v["Value"]) + ";\n")
			elif v["FeatureType"] in ["val"]:
				featureDefineName = string.upper(name)
				out.write("    " + featureDefineName + " = " + fixConstant(v["Value"]) + ";\n")


def printDefinesAsLang(lang,f, out):
	pre=""
	term=";"
	sep=" = "
	oroperator="|"
	hexoperator="0x"

	if lang=="vb":
		pre="const "
		term=""
		oroperator=" or "
		hexoperator="&h"		
	elif lang=="javascript":
		pre=""
		term=";"
	elif lang=="python":
		term=""
	elif lang=="perl":
		pre=""
		term=","
		sep="=>"
	elif lang=="phpscript":
		pre="define(\""
		term=");"
		sep="\","

	for name in f.order:
		v = f.features[name]
		if v["Category"] != "Deprecated":
			if scriptonlyvalues==0:
				if v["FeatureType"] in ["fun", "get", "set"]:
					featureDefineName = "SCI_" + string.upper(name)
					out.write("	" +pre+ featureDefineName + sep+ fixConstantByLang(v["Value"],oroperator,hexoperator) + term+"\n")
				elif v["FeatureType"] in ["evt"]:
					featureDefineName = "SCN_" + string.upper(name)
					out.write("	" + pre+featureDefineName + sep + fixConstantByLang(v["Value"],oroperator,hexoperator) + term+"\n")
				elif v["FeatureType"] in ["val"]:
					featureDefineName = string.upper(name)
					out.write("	" + pre+featureDefineName + sep + fixConstantByLang(v["Value"],oroperator,hexoperator) + term+"\n")
			else:
				if v["FeatureType"] in ["val"]:
					if scriptnolexers==1:
						if not Contains(string.upper(name),"SCLEX_"):
							featureDefineName = string.upper(name)
							out.write("	" + pre+featureDefineName + sep + fixConstantByLang(v["Value"],oroperator,hexoperator) + term+"\n")
					else:
						featureDefineName = string.upper(name)
						out.write("	" + pre+featureDefineName + sep + fixConstantByLang(v["Value"],oroperator,hexoperator) + term+"\n")
				elif v["FeatureType"] in ["evt"]:
					featureDefineName = "SCN_" + string.upper(name)
					out.write("	" + pre+featureDefineName + sep + fixConstantByLang(v["Value"],oroperator,hexoperator) + term+"\n")


def genEventPrototype(name, evt):
	wconst='';
	retval = string.lower(name) + " = procedure(Sender : TObject"
	for parm in evt["Params"].order:
		v = evt["Params"].params[parm]
		if pType(v["ParamType"])!="PChar":
			wconst='const ';
		else:
			wconst='';
		if parm in ["length","Length"]:
			parm="len"
		retval = retval + "; " + wconst+parm + " : " + pType(v["ParamType"])
	retval = retval + ")"
	return retval

def printEventDefs(f, out):
	for name in f.order:
		v = f.features[name]
		if v["Category"] != "Deprecated":
			if v["FeatureType"] in ["evt"]:
				eventDefineName = "TSCEvent_" + genEventPrototype(name, v)
				eventDefineName = eventDefineName + " of object;\n"
				out.write("    " + eventDefineName)

def printEventPrivates(f, out):
	for name in f.order:
		v = f.features[name]
		if v["Category"] != "Deprecated":
			if v["FeatureType"] in ["evt"]:
				eventtname = "TSCEvent_" + string.lower(name) + ";\n"
				eventvname = "    FOn" + string.lower(name) + " : " + eventtname;
				out.write("    " + eventvname)

def printEventProperties(f, out):
	for name in f.order:
		v = f.features[name]
		if v["Category"] != "Deprecated":
			if v["FeatureType"] in ["evt"]:
				#property OnStyleNeeded : TSCEvent_styleneeded read FOnstyleneeded write FOnstyleneeded;
				prop = "FOn" + string.lower(name)
				line = "property On" + name + " : TSCEvent_" + string.lower(name)
				line = line + " read " + prop + " write " + prop + ";\n"
				out.write("    " + line)

def printEventImpl(f, out):
	for name in f.order:
		v = f.features[name]
		if v["Category"] != "Deprecated":
			if v["FeatureType"] in ["evt"]:
				##2000 : if assigned(FOnstyleneeded) then FOnstyleneeded(Self, scn^.position);
				#line = "      " + v["Value"] + " : if assigned("
				#line = line + "FOn" + string.lower(name) + ") then FOn" + string.lower(name) + "(Self"
				line = "      " + v["Value"] + " : doSci"+name
				wasfirst=1
				parmcnt=0
				if v["Value"]!="2015":
					for parm in v["Params"].order:
						z = v["Params"].params[parm]
						parmcnt=parmcnt+1
						if wasfirst==1:
							wasfirst=0

							line = line + "(scn^." + parm
						else:
							line = line + ", scn^." + parm
					if parmcnt>0:
						line = line + ");\n"
					else:
						line =line+";\n";
					out.write(line)


def genfuncPrototype(name, fun):
	hasptr = 0
	retval = name
	badded = 0
	if fun["Param1Type"] != "" and fun["Param1Type"] != " ":
		badded = 1
		retval = retval + "(" + fun["Param1Name"] + " : " + pType(fun["Param1Type"])
	if fun["Param2Type"] != "" and fun["Param2Type"] != " ":
		if badded == 1:
			retval = retval + "; "
		else:
			retval = retval + "("
		badded = 1
		retval = retval + fun["Param2Name"] + " : " + pType(fun["Param2Type"])
	if badded == 1:
		retval = retval + ")"
	return retval

def getfuncHeader(name, fun, extra):
	retval = genfuncPrototype(name, fun)
	line = ""
	if pType(fun["ReturnType"]) == "":
		line = line + "procedure " + extra + retval + ";\n"
	else:
		line = line + "function "  + extra + retval + " : " + pType(fun["ReturnType"]) + ";\n"
	return line

def printFunctionDefs(f, out):
	for name in f.order:
		v = f.features[name]
		if v["Category"] != "Deprecated":
			if v["FeatureType"] in ["fun", "set", "get"]:
				line = "    "
				if name=="FindText":
					line = line + getfuncHeader(name+"X", v, "")
				else:
					line = line + getfuncHeader(name, v, "")
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
				if name=="FindText":
					header = getfuncHeader(name+"X", v, "TScintillaBase.")
				else:
					header = getfuncHeader(name, v, "TScintillaBase.")
				out.write(header)
				out.write("begin\n")
				line = "  "
				endline = ""
				if not isFunction(v):
					line = line + "SPerform("
				else:
					line = line + "result := "

					if pType(v["ReturnType"]) != "LongInt":
						line = line + pType(v["ReturnType"]) + "("
						endline = ")"
					line = line + "SPerform("
				line = line + "SCI_" + string.upper(name) + ", "
				if not (v["Param1Type"] in ["", " "]):
					add = ""
					if pType(v["Param1Type"]) == "TColor":
						line = line + "ColorToRGB("
						add = ")"
					elif pType(v["Param1Type"]) != "LongInt":
						line = line + "LongInt("
						add = ")"
					line= line + v["Param1Name"] + add + ", "
				else:
					line = line + "0, "
				if not (v["Param2Type"] in ["", " "]):
					add = ""
					if pType(v["Param2Type"]) == "TColor":
						line = line + "ColorToRGB("
						add = ")"
					elif pType(v["Param2Type"]) != "LongInt":
						line = line + "LongInt("
						add = ")"
					line = line + v["Param2Name"] + add + ")"
				else:
					line = line + "0)"
				line = line + endline + ";"
				out.write(line + "\nend;\n\n")

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

def genConstsLang(input, output, definition):
	copying = 1
	for line in input.readlines():
		if copying:
			output.write(line)
		if Contains(line,scriptcommentstart+"++Const"):
			copying = 0
			printDefinesAsLang(scriptlang,definition, output)
		if Contains(line,scriptcommentstart+"--") and copying == 0:
			copying = 1
			output.write(line)

def Regenerate(filename, outputfilename, definition, fn):
	tempname = "PasFacer.tmp"
	out = open(tempname,"w")
	hfile = open(filename)
	#CopyWithInsertion(hfile, out, definition)
	fn(hfile, out, definition)
	out.close()
	hfile.close()
	if(os.access(outputfilename, os.F_OK)):
		os.unlink(outputfilename)
	os.rename(tempname, outputfilename)

def usage():
	sys.stdout.writelines("PasFacer -[opsanv]\n\n");
	sys.stdout.writelines("-o<path> = Destination dir for the script files\n")
	sys.stdout.writelines("-p<path> = Destination dir for the pascal files\n")
	sys.stdout.writelines("-s       = Write script files.\n")
	sys.stdout.writelines("-a       = Write all constants\n")
	sys.stdout.writelines("-n       = Write all values, including lexervalues.\n")
	sys.stdout.writelines("-x       = Write all values, excluding lexervalues.\n\n")
	sys.stdout.writelines("-v       = Verbose\n\n")
	sys.stdout.writelines("The default is to write only the pascal files.\n\n")
	sys.stdout.writelines("The default for scripts is to write all values, excluding lexervalues.\n\n")
	sys.stdout.writelines("This program outputs pascal scintilla interface files, and \nscript constant files for a variety of script languages.\n")
	sys.stdout.writelines("By now it supports JavaScript, VBScript,Perl,Python,PHPScript.\n\n")
	sys.stdout.writelines("NOTE: It won't generate a file if the original file doesn\'t exist in the directory:\n\""+scriptsrcdir+"\"")


def RegenScript(ext):
	if os.access(scriptsrcdir+constantfilename+ext,os.F_OK)==1:
		Regenerate(scriptsrcdir+constantfilename+ext, scriptdest+constantfilename+ext, f, genConstsLang)
	elif verbose==1:
		sys.stdout.writelines("Error: \""+scriptsrcdir+constantfilename+ext+"\" not found. Couldn't generate \""+scriptdest+constantfilename+ext+"\"")

# Program Start
try:
	opts, args = getopt.getopt(sys.argv[1:], "shao:nvxp:", ["outputscript","help", "scriptincludeall","scriptoutputdir=","scriptincludelexers","scriptonlyvalues","verbose","pascaloutputdir="])
	for o,a in opts:
		if o in ["-s","--outputscript"]:
			outputscripts=1
		if o in ["-h","--help"]:
			usage()
			sys.exit()
		if o in ["-o","--scriptoutputdir"]:
			scriptdest=a
		if o in ["-a","--scriptincludeall"]:
			scriptnolexers=0
			scriptonlyvalues=0
		if o in ["-n","--scriptincludelexers"]:
			scriptnolexers=0
		if o in ["-v","--verbose"]:
			verbose=1
		if o in ["-x","--scriptonlyvalues"]:
			scriptonlyvalues=1
		if o in ["-p","--pascaloutputdir"]:
			pascaldestdir=a

except getopt.GetoptError:
	# print help information and exit:
	outputscripts=0

if outputscripts==1:
	sys.stdout.writelines("Regenerating SciLexer.pas, SciSupport.pas, and SciConsts.* files..\n")
	sys.stdout.writelines("Writing SciLexer.pas and SciSupport.pas to \""+pascaldestdir+"\".\n")
	sys.stdout.writelines("Writing the sciconsts.* files to \""+scriptdest+"\".\n\n")
	sys.stdout.write("for the scripts ")
	if scriptnolexers==0 and scriptonlyvalues==0:
		sys.stdout.write("all constants")
	elif scriptonlyvalues==1:
		sys.stdout.write("only values, ")
		if scriptnolexers==0:
			sys.stdout.write("including")
		else:
			sys.stdout.write("excluding")
		sys.stdout.write(" SCLEX_*")
	elif scriptonlyvalues==0:
		sys.stdout.write("only values")
		
	sys.stdout.writelines(" are written.\n")
else:
	sys.stdout.writelines("Regenerating SciLexer.pas and SciSupport.pas..\nWriting them to \""+pascaldestdir+"\".\n")

sys.stdout.flush()
f = PFace.Face()
f.ReadFromFile("..\\include\\Scintilla.iface")
Regenerate("..\\originals\\SciLexer.pas", pascaldestdir+"SciLexer.pas", f, genMainControl)
Regenerate("..\\originals\\SciSupport.pas", pascaldestdir+"SciSupport.pas", f, genConsts)
if (outputscripts==1):
	#if we an original sciconsts.vbs then regenerate it, otherwise, just ignore.
	scriptlang="vb"
	scriptcommentstart="'"
	RegenScript(".vbs")
	scriptlang="javascript"
	scriptcommentstart="//"
	RegenScript(".js")
	scriptlang="python"
	scriptcommentstart="#"
	RegenScript(".py")
	scriptlang="perl"
	scriptcommentstart="#"
	RegenScript(".pl")
	scriptlang="phpscript"
	scriptcommentstart="//"
	RegenScript(".phs")
