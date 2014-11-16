@echo off

if not [%1] == [] pushd %1

rem ------------------------------
rem create directories

echo Create runtime image directories ...
if not exist ..\Output mkdir ..\Output

if not exist bin\windows\xml mkdir bin\windows\xml
if not exist bin\windows\locale mkdir bin\windows\locale
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

echo Copying XML files
copy ..\common\res\mysqlx_dbm_charsets.xml bin\windows\xml\mysqlx_dbm_charsets.xml
copy ..\common\res\mysqlx_dbm_datatypes.xml bin\windows\xml\mysqlx_dbm_datatypes.xml

copy res\mysqladmin_health.xml bin\windows\xml\mysqladmin_health.xml
copy res\mysqladmin_startup_variables_description.dtd bin\windows\xml\mysqladmin_startup_variables_description.dtd
copy res\mysqladmin_startup_variables_description.xml bin\windows\xml\mysqladmin_startup_variables_description.xml
copy res\mysqladmin_status_variables.xml bin\windows\xml\mysqladmin_status_variables.xml
copy res\mysqladmin_system_variables.xml bin\windows\xml\mysqladmin_system_variables.xml

echo .

rem ------------------------------
rem copy fonts

rem ------------------------------
rem copy translations

echo Copying translations files
copy res\MakeMo.cmd bin\windows\locale\
copy ..\common\res\how_to_translate.txt bin\windows\locale\
copy ..\common\res\languages_list.txt bin\windows\locale\

pushd build
call CollectExternalTranslations.cmd
call MakeTranslations.cmd "..\bin\windows"
popd

if not [%1] == [] popd

echo .
