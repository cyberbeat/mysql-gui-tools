REM CommandInterpreter: $(COMSPEC)
@echo off

rem ------------------------------
rem create php code from GRT structs

if not exist ..\source\php\db mkdir ..\source\php\db
if not exist ..\source\php\db\migration mkdir ..\source\php\db\migration
if not exist ..\source\php\db\oracle mkdir ..\source\php\db\oracle

rem migration
..\..\mysql-gui-common\bin\windows\grtsh -p ..\res\grt\structs.db.migration.xml ..\source\php\db\migration\

rem migration
..\..\mysql-gui-common\bin\windows\grtsh -p ..\res\grt\structs.db.oracle.xml ..\source\php\db\oracle\