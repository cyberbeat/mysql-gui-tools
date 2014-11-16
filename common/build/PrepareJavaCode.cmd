REM CommandInterpreter: $(COMSPEC)
@echo off

rem ------------------------------
rem copy *.jars

echo Creating output folders ...

if not exist ..\source\java\output mkdir ..\source\java\output
if not exist ..\source\java\lib mkdir ..\source\java\lib

echo Copying JAR files ...

copy ..\..\win-external-libs\lib\java\*.jar ..\source\java\lib 1> nul
copy ..\res\java\*.jar ..\source\java\lib 1> nul

rem ------------------------------
rem create java code from GRT structs

echo Cleaning up previouse builds...
rmdir /s /q ..\source\java\com\mysql\grt\base 1> nul 2> nul
rmdir /s /q ..\source\java\com\mysql\grt\canvas 1> nul 2> nul
rmdir /s /q ..\source\java\com\mysql\grt\db 1> nul 2> nul
rmdir /s /q ..\source\java\com\mysql\grt\forms 1> nul 2> nul
rmdir /s /q ..\source\java\com\mysql\grt\model 1> nul 2> nul

echo Generating Java code...

set grtpath="..\..\output\grtsh.exe"
if not exist ..\..\output\grtsh.exe set grtpath="..\..\release\grtsh.exe"

echo Processing base ...
%grtpath% -j ..\res\grt\structs.base.xml ..\source\java\com\mysql\grt\base\
if errorlevel 1 goto error

echo Processing canvas ...
%grtpath% -j ..\res\grt\structs.canvas.xml ..\source\java\com\mysql\grt\canvas\
if errorlevel 1 goto error

echo Processing db ...
%grtpath% -j ..\res\grt\structs.db.xml ..\source\java\com\mysql\grt\db\
if errorlevel 1 goto error

echo Processing db.mysql ...
%grtpath% -j ..\res\grt\structs.db.mysql.xml ..\source\java\com\mysql\grt\db\mysql\
if errorlevel 1 goto error

echo Processing forms ...
%grtpath% -j ..\res\grt\structs.forms.xml ..\source\java\com\mysql\grt\forms\
if errorlevel 1 goto error

echo Processing db.mgmt ...
%grtpath% -j ..\res\grt\structs.db.mgmt.xml ..\source\java\com\mysql\grt\db\mgmt\
if errorlevel 1 goto error

echo Processing db.migration ...
%grtpath% -j ..\res\grt\structs.db.migration.xml ..\source\java\com\mysql\grt\db\migration\
if errorlevel 1 goto error

echo Processing db.oracle ...
%grtpath% -j ..\res\grt\structs.db.oracle.xml ..\source\java\com\mysql\grt\db\oracle\
if errorlevel 1 goto error

echo Processing db.mssql ...
%grtpath% -j ..\res\grt\structs.db.mssql.xml ..\source\java\com\mysql\grt\db\mssql\
if errorlevel 1 goto error

echo Processing db.maxdb ...
%grtpath% -j ..\res\grt\structs.db.maxdb.xml ..\source\java\com\mysql\grt\db\maxdb\
if errorlevel 1 goto error

echo Processing db.sybase ...
%grtpath% -j ..\res\grt\structs.db.sybase.xml ..\source\java\com\mysql\grt\db\sybase\
if errorlevel 1 goto error

echo Processing db.query ...
%grtpath% -j ..\res\grt\structs.db.query.xml ..\source\java\com\mysql\grt\db\query\
if errorlevel 1 goto error

echo Processing model ...
%grtpath% -j ..\res\grt\structs.model.xml ..\source\java\com\mysql\grt\model\
if errorlevel 1 goto error

echo Processing workbench ...
%grtpath% -j ..\res\grt\structs.db.workbench.xml ..\source\java\com\mysql\grt\db\workbench\
if errorlevel 1 goto error

echo Generation of Java code completed.

goto finish

:error
echo There was an error during java file creation.

:finish
echo .
