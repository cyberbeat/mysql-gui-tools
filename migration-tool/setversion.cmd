@echo off
rem ---------------------------------------------------------
rem Take new WiX files from source folder

copy setup\windows\source\mysql_migration_toolkit.xml setup\windows\mysql_migration_toolkit.xml 1> nul
copy setup\windows\source\mysql_migration_toolkit_fragment.xml setup\windows\mysql_migration_toolkit_fragment.xml 1> nul

rem ---------------------------------------------------------
rem Set the correct version

pushd ..\output
SetVersion -p..\migration-tool\source\windows\Consts.ini -x..\migration-tool\setup\windows\mysql_migration_toolkit.xml -f..\migration-tool\setup\windows\mysql_migration_toolkit_fragment.xml -s..\migration-tool\setup\windows\set_source_dir.cmd -r..\migration-tool\res\windows\MySQLMigrationTool.rc
popd

rem ---------------------------------------------------------
rem Create new project resource

cd res\windows
brcc32 MySQLMigrationTool.rc
if not exist MySQLMigrationTool.RES goto ERROR1
copy MySQLMigrationTool.RES ..\..\source\windows\MySQLMigrationTool.res 1> nul
cd ..\..

EXIT /B 0

:ERROR1
echo Error: Could not compile project resource.
pause
EXIT /B 1
