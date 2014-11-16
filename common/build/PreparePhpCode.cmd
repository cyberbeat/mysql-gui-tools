REM CommandInterpreter: $(COMSPEC)
@echo off

rem ------------------------------
rem create php code from GRT structs

echo Processing base ...
..\bin\windows\grtsh -p ..\res\grt\structs.base.xml ..\source\php\base\
if errorlevel 1 goto errorOccured

echo Processing canvas ...
..\bin\windows\grtsh -p ..\res\grt\structs.canvas.xml ..\source\php\canvas\
if errorlevel 1 goto errorOccured

echo Processing db ...
..\bin\windows\grtsh -p ..\res\grt\structs.db.xml ..\source\php\db\
if errorlevel 1 goto errorOccured

echo Processing db.mysql ...
..\bin\windows\grtsh -p ..\res\grt\structs.db.mysql.xml ..\source\php\db\mysql\
if errorlevel 1 goto errorOccured

echo Processing forms ...
..\bin\windows\grtsh -p ..\res\grt\structs.forms.xml ..\source\php\forms\
if errorlevel 1 goto errorOccured

echo Processing db.mgmt ...
..\bin\windows\grtsh -p ..\res\grt\structs.db.mgmt.xml ..\source\php\db\mgmt\
if errorlevel 1 goto errorOccured

echo Processing db.migration ...
..\bin\windows\grtsh -p ..\res\grt\structs.db.migration.xml ..\source\php\db\migration\
if errorlevel 1 goto errorOccured

echo Processing db.oracle ...
..\bin\windows\grtsh -p ..\res\grt\structs.db.oracle.xml ..\source\php\db\oracle\
if errorlevel 1 goto errorOccured

echo Processing db.mssql ...
..\bin\windows\grtsh -p ..\res\grt\structs.db.mssql.xml ..\source\php\db\mssql\
if errorlevel 1 goto errorOccured
goto endOfScript

:errorOccured
echo An Error occured!
pause

:endOfScript
echo .