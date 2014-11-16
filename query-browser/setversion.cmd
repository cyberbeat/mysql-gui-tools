@echo off
rem ---------------------------------------------------------
rem Take new WiX files from source folder

copy setup\windows\source\mysql_query_browser.xml setup\windows\mysql_query_browser.xml 1> nul
copy setup\windows\source\mysql_query_browser_fragment.xml setup\windows\mysql_query_browser_fragment.xml 1> nul

rem ---------------------------------------------------------
rem Set the correct version

cd ..\output
SetVersion -p..\query-browser\source\windows\Consts.ini -x..\query-browser\setup\windows\mysql_query_browser.xml -f..\query-browser\setup\windows\mysql_query_browser_fragment.xml -s..\query-browser\setup\windows\set_source_dir.cmd -r..\query-browser\res\windows\MySQLQueryBrowser.rc
cd ..\query-browser

rem ---------------------------------------------------------
rem Create new project resource

cd res\windows
brcc32 MySQLQueryBrowser.rc
if not exist MySQLQueryBrowser.RES goto ERROR1
copy MySQLQueryBrowser.RES ..\..\source\windows\MySQLQueryBrowser.res 1> nul
cd ..\..

EXIT /B 0

:ERROR1
echo Error: Could not compile project resource.
pause
EXIT /B 1
