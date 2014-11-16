@echo off

pushd ..\..

echo Create directories ...

if not exist bin mkdir bin
if not exist bin\xml mkdir bin\xml
if not exist bin\lua mkdir bin\lua


echo Copy grtsh ...

copy tools\grtsh\Debug\grtsh.exe bin\. 1> nul 


echo Copy MySQL libraries ...

copy library\base-library\Lib_Debug\*.dll bin\. 1> nul 
copy library\generic-runtime\Lib_Debug\l*.dll bin\. 1> nul 
copy library\grt-modules\Lib_Debug\*.dll bin\. 1> nul 
copy library\sql-parser\Lib_Debug\*.dll bin\. 1> nul 
copy library\utilities\Lib_Debug\*.dll bin\. 1> nul 

popd