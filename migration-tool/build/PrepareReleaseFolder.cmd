@echo off

if not [%1] == [] pushd %1

rem ------------------------------
rem create directories

echo Create runtime image directories ...
if not exist ..\..\release\intermediate mkdir ..\..\release\intermediate

if not exist ..\..\release\xml mkdir ..\..\release\xml
if not exist ..\..\release\lua mkdir ..\..\release\lua
if not exist ..\..\release\scripts mkdir ..\..\release\scripts
if not exist ..\..\release\images\structs mkdir ..\..\release\images\structs
if not exist ..\..\release\images\grt\db mkdir ..\..\release\images\grt\db
if not exist ..\..\release\fonts mkdir ..\..\release\fonts
if not exist ..\..\release\res mkdir ..\..\release\res

echo .

rem ------------------------------
rem copy appropriate licence

echo Copying licence files ...
if exist ..\..\release\COPYING del ..\..\release\COPYING
if exist ..\..\release\MySQLEULA.txt del ..\..\release\MySQLEULA.txt

copy ..\..\common\res\COPYING ..\..\release\COPYING > nul
echo .

rem ------------------------------
rem copy xml files

copy ..\..\common\res\grt\structs*.xml ..\..\release\xml\ > nul
copy ..\res\grt\GenericDatatypeMapping.xml ..\..\release\xml\ > nul

rem ------------------------------
rem copy fonts

echo Copying doc files...
copy ..\..\common\res\fonts\VeraMoBd.ttf ..\..\release\fonts\VeraMoBd.ttf > nul
copy ..\..\common\res\fonts\VeraMoBI.ttf ..\..\release\fonts\VeraMoBI.ttf > nul
copy ..\..\common\res\fonts\VeraMoIt.ttf ..\..\release\fonts\VeraMoIt.ttf > nul
copy ..\..\common\res\fonts\VeraMono.ttf ..\..\release\fonts\VeraMono.ttf > nul
echo .

rem ------------------------------
rem copy images

echo Copying image files
copy ..\..\common\images\grt\structs\*.png ..\..\release\images\structs\ > nul
copy ..\images\grt\structs\*.png ..\..\release\images\structs\ > nul
copy ..\..\common\images\grt\*.png ..\..\release\images\ > nul
copy ..\..\common\images\grt\structs\*.png ..\..\release\images\structs\ > nul
copy ..\..\common\images\grt\icons\db\*.png ..\..\release\images\grt\db\ > nul
echo .

rem ------------------------------
rem lua source files

echo Copying lua files...
copy ..\..\common\source\lua\*.lua ..\..\release\lua\ > nul
copy ..\source\lua\*.lua ..\..\release\lua\ > nul
echo .

rem ------------------------------
rem script files

echo Copying script files...
copy ..\source\scripts\*.* ..\..\release\scripts\ > nul
echo .

if not [%1] == [] popd
