@echo off

echo Extracting Admin PO files...
xgettext.exe --default-domain=mysql-gui-common --language=C++ --add-comments --keyword=_ -o ..\res\linux\po\mysql-gui-common.po ..\source\linux\*.cc
if not errorlevel 1 goto endOfScript
echo An Error occured!
pause

:endOfScript
echo .
