@echo off

pushd ..\..

echo Create directories ...

if not exist bin mkdir bin


echo Copy MySQL libraries ...

copy library\base-library\Lib_Debug\*.dll bin\. 1> nul 
copy library\generic-runtime\Lib_Debug\l*.dll bin\. 1> nul 
copy library\grt-modules\Lib_Debug\*.dll bin\. 1> nul 
copy library\sql-parser\Lib_Debug\*.dll bin\. 1> nul 
copy library\utilities\Lib_Debug\*.dll bin\. 1> nul 

popd