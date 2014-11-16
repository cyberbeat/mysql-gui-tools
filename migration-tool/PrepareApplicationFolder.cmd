@echo off

if not [%1] == [] pushd %1

rem ------------------------------
rem create directories

echo Create runtime image directories ...
if not exist ..\..\Output mkdir ..\..\Output

if not exist bin\windows\xml mkdir bin\windows\xml
if not exist bin\windows\lua mkdir bin\windows\lua
if not exist bin\windows\scripts mkdir bin\windows\scripts
if not exist bin\windows\images\structs mkdir bin\windows\images\structs
if not exist bin\windows\fonts mkdir bin\windows\fonts
if not exist bin\windows\res mkdir bin\windows\res

echo .

rem ------------------------------
rem copy appropriate licence

echo Copying licence files ...
if exist bin\windows\COPYING del bin\windows\COPYING
if exist bin\windows\MySQLEULA.txt del bin\windows\MySQLEULA.txt

copy ..\common\res\COPYING bin\windows\COPYING
echo .

rem ------------------------------
rem copy xml files

copy ..\common\res\grt\structs*.xml bin\windows\xml\
copy res\grt\GenericDatatypeMapping.xml bin\windows\xml\.

rem ------------------------------
rem copy fonts

echo Copying doc files
copy ..\common\res\fonts\VeraMoBd.ttf bin\windows\fonts\VeraMoBd.ttf
copy ..\common\res\fonts\VeraMoBI.ttf bin\windows\fonts\VeraMoBI.ttf
copy ..\common\res\fonts\VeraMoIt.ttf bin\windows\fonts\VeraMoIt.ttf
copy ..\common\res\fonts\VeraMono.ttf bin\windows\fonts\VeraMono.ttf
echo .

rem ------------------------------
rem copy jars

echo Copying third party jars
if not exist ..\common\source\java\lib\jtds-1.2.jar copy ..\common\res\java\jtds-1.2.jar ..\common\source\java\lib\jtds-1.2.jar
if not exist ..\common\source\java\lib\junit.jar copy ..\common\res\java\junit.jar ..\common\source\java\lib\junit.jar
if not exist ..\common\source\java\lib\mysql-connector-java-3.1.11a-bin.jar copy ..\common\res\java\mysql-connector-java-3.1.11a-bin.jar ..\common\source\java\lib\mysql-connector-java-3.1.11a-bin.jar
if not exist ..\common\source\java\lib\sapdbc-7_6_00_12_4339.jar copy ..\common\res\java\sapdbc-7_6_00_12_4339.jar ..\common\source\java\lib\sapdbc-7_6_00_12_4339.jar
if not exist ..\common\source\java\lib\ojdbc14.jar copy ..\win-external-libs\lib\java\ojdbc14.jar ..\common\source\java\lib\ojdbc14.jar
echo .

rem ------------------------------
rem copy images

echo Copying image files
copy ..\common\images\grt\structs\*.png bin\windows\images\structs\
copy images\grt\structs\*.png bin\windows\images\structs\
echo .

rem ------------------------------
rem lua source files

echo Copying lua files...
copy ..\common\source\lua\*.lua bin\windows\lua\
copy source\lua\*.lua bin\windows\lua\
echo .

rem ------------------------------
rem script files

echo Copying script files...
copy source\scripts\*.* bin\windows\scripts\
echo .

if not [%1] == [] popd
