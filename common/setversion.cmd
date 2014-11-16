@echo off
rem ---------------------------------------------------------
rem Take new WiX files from source folder

copy setup\windows\source\mysql_gui_tools.xml setup\windows\mysql_gui_tools.xml 1> nul
copy setup\windows\source\mysql_gui_tools_fragment.xml setup\windows\mysql_gui_tools_fragment.xml 1> nul

rem ---------------------------------------------------------
rem Set the correct version

pushd ..\output
SetVersion -x..\common\setup\windows\mysql_gui_tools.xml -f..\common\setup\windows\mysql_gui_tools_fragment.xml -s..\common\setup\windows\set_source_dir.cmd
popd

rem ---------------------------------------------------------
rem Create new project resource

EXIT /B 0

:ERROR1
echo Error: Could not compile project resource.
pause
EXIT /B 1
