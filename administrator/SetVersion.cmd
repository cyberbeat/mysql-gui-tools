@echo off
rem ---------------------------------------------------------
rem Take new WiX files from source folder

copy setup\windows\source\mysql_administrator.xml setup\windows\mysql_administrator.xml 1> nul
copy setup\windows\source\mysql_administrator_fragment.xml setup\windows\mysql_administrator_fragment.xml 1> nul

rem ---------------------------------------------------------
rem Set the correct version

cd ..\output
SetVersion -p..\administrator\source\windows\Consts.ini -x..\administrator\setup\windows\mysql_administrator.xml -f..\administrator\setup\windows\mysql_administrator_fragment.xml -s..\administrator\setup\windows\set_source_dir.cmd -r..\administrator\res\windows\MySQLAdministrator.rc %*
cd ..\administrator

rem ---------------------------------------------------------
rem Create new project resource

cd res\windows
brcc32 MySQLAdministrator.rc
if not exist MySQLAdministrator.RES goto ERROR1
copy MySQLAdministrator.RES ..\..\source\windows\MySQLAdministrator.res 1> nul
cd ..\..

EXIT /B 0

:ERROR1
echo Error: Could not compile project resource.
pause
EXIT /B 1
