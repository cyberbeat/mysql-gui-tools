@echo off

rem Explicitely recreate the intermediate java folder. The jar tools seems to have problems with it.
if exist ..\..\release\intermediate\java rmdir ..\..\release\intermediate\java /Q /S 2> nul
mkdir ..\..\release\intermediate\java

if not exist ..\..\release\java\lib mkdir ..\..\release\java\lib

echo -------------------------------------------------------
echo Converting GRT structs to Java classes ...
echo -------------------------------------------------------
call PrepareJavaCode.cmd

echo -------------------------------------------------------
echo Compiling grt java files...
echo -------------------------------------------------------
javac -g:none -d ..\..\release\intermediate\java -classpath ..\source\java\lib\junit.jar ..\source\java\com\mysql\grt\*.java ..\source\java\com\mysql\grt\base\*.java ..\source\java\com\mysql\grt\modules\*.java ..\source\java\com\mysql\grt\canvas\*.java ..\source\java\com\mysql\grt\db\*.java ..\source\java\com\mysql\grt\db\mysql\*.java ..\source\java\com\mysql\grt\db\oracle\*.java ..\source\java\com\mysql\grt\db\mssql\*.java ..\source\java\com\mysql\grt\db\sybase\*.java ..\source\java\com\mysql\grt\db\maxdb\*.java ..\source\java\com\mysql\grt\db\migration\*.java ..\source\java\com\mysql\grt\db\mgmt\*.java ..\source\java\com\mysql\grt\forms\*.java ..\source\java\com\mysql\grt\model\*.java ..\source\java\com\mysql\grt\db\workbench\*.java
echo.
if errorlevel 1 goto error

echo -------------------------------------------------------
echo Creating GRT jar file...
echo -------------------------------------------------------
rem Remove the modules class files. They will not be in the jar but as java and class files with debug info in the java folder.
rmdir ..\..\release\intermediate\java\com\mysql\grt\modules /S /Q > nul 2> nul
pushd ..\..\release\intermediate\java\
jar cf mysql-grt-java-1.0.0-bin.jar com
if errorlevel 1 (
  popd
  goto error
)

move mysql-grt-java-1.0.0-bin.jar ..\..\java\lib\
rmdir com /S /Q > nul 2> nul
popd
if errorlevel 1 goto error

echo -------------------------------------------------------
echo Compiling module java files...
echo -------------------------------------------------------
rem Compile class files for modules
javac -g -d ..\..\release\java -classpath ..\source\java\lib\junit.jar;..\..\release\java\lib\mysql-grt-java-1.0.0-bin.jar ..\source\java\com\mysql\grt\modules\*.java
rem Copy the source files to the class files.
copy ..\source\java\com\mysql\grt\modules ..\..\release\java\com\mysql\grt\modules
echo.
if errorlevel 1 goto error

echo -------------------------------------------------------
echo Copying third-party jar files...
echo -------------------------------------------------------
copy ..\res\java\*.jar ..\..\release\java\lib\ > nul
if errorlevel 1 goto error

echo -------------------------------------------------------
echo Copying Eclipse project files...
echo -------------------------------------------------------

copy "..\source\java\.classpath" ..\..\release\java\. > nul
copy "..\source\java\.project" ..\..\release\java\. > nul
if not errorlevel 1 goto finish

:error

:finish

