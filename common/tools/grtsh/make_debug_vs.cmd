@echo off

echo Build grtsh...

devenv grtsh.sln /build Lib_Debug /nologo 1> nul 


call copy_to_bin.cmd


pushd ..\..

echo Copy external libraries ...

copy ..\win-external-libs\lib\expat\*.dll bin\. 1> nul
copy ..\win-external-libs\lib\glib-2.0\*.dll bin\. 1> nul
copy ..\win-external-libs\lib\libmysql\*.dll bin\. 1> nul
copy ..\win-external-libs\lib\libxml\*.dll bin\. 1> nul
copy ..\win-external-libs\lib\php-5.0.3\*.dll bin\. 1> nul
copy ..\win-external-libs\lib\python\*.dll bin\. 1> nul
copy ..\win-external-libs\lib\windows\*d.dll bin\. 1> nul
copy ..\win-external-libs\lib\zlib\*.dll bin\. 1> nul
copy ..\win-external-libs\lib\fcgi\*.dll bin\. 1> nul


echo Copy XML files ...

copy res\grt\structs*.xml bin\xml\


echo Copy Lua files ...

copy source\lua\*.lua bin\lua\

popd

goto endOfScript

:errorOccured
echo An Error occured!
pause

:endOfScript
echo .