REM CommandInterpreter: $(COMSPEC)
@echo off

REM --------------------------------------
echo Cleaning .class files ...

rmdir /s /q ..\source\java\output\com\ 1> nul 2> nul

REM --------------------------------------
echo Building Java ...

javac -g -nowarn -d ..\source\java\output -classpath ..\source\java\lib\junit.jar ..\source\java\com\mysql\grt\*.java ..\source\java\com\mysql\grt\base\*.java ..\source\java\com\mysql\grt\canvas\*.java ..\source\java\com\mysql\grt\modules\*.java ..\source\java\com\mysql\grt\db\*.java ..\source\java\com\mysql\grt\db\mysql\*.java ..\source\java\com\mysql\grt\db\oracle\*.java ..\source\java\com\mysql\grt\db\mssql\*.java ..\source\java\com\mysql\grt\db\maxdb\*.java ..\source\java\com\mysql\grt\db\sybase\*.java ..\source\java\com\mysql\grt\db\migration\*.java ..\source\java\com\mysql\grt\db\mgmt\*.java ..\source\java\com\mysql\grt\forms\*.java ..\source\java\com\mysql\grt\model\*.java ..\source\java\com\mysql\grt\db\workbench\*.java
if errorlevel 1 goto error

echo .

rem del /S /Q ..\source\java\*.class 1> nul 2> nul
goto finish

:error
echo Compiling Java classes failed.

:finish
echo .

