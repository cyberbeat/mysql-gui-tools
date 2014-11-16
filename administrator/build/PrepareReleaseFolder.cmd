@echo off

rem Create application directories

echo ----------------------------------------------------------------
echo Creating application directories ...
echo ----------------------------------------------------------------
if not exist ..\..\release\xml mkdir ..\..\release\xml
if not exist ..\..\release\locale mkdir ..\..\release\locale
if not exist ..\..\release\intermediate mkdir ..\..\release\intermediate
echo .

echo ----------------------------------------------------------------
echo Copying licence files ...
echo ----------------------------------------------------------------
if exist ..\..\release\COPYING del ..\..\release\COPYING
if exist ..\..\release\MySQLEULA.txt del ..\..\release\MySQLEULA.txt

copy ..\..\common\res\COPYING ..\..\release\COPYING
echo .

echo ----------------------------------------------------------------
echo Copying XML files...
echo ----------------------------------------------------------------
copy ..\..\common\res\mysqlx_dbm_charsets.xml ..\..\release\xml\mysqlx_dbm_charsets.xml
copy ..\..\common\res\mysqlx_dbm_datatypes.xml ..\..\release\xml\mysqlx_dbm_datatypes.xml

copy ..\res\mysqladmin_health.xml ..\..\release\xml\mysqladmin_health.xml
copy ..\res\mysqladmin_startup_variables_description.dtd ..\..\release\xml\mysqladmin_startup_variables_description.dtd
copy ..\res\mysqladmin_startup_variables_description.xml ..\..\release\xml\mysqladmin_startup_variables_description.xml
copy ..\res\mysqladmin_status_variables.xml ..\..\release\xml\mysqladmin_status_variables.xml
copy ..\res\mysqladmin_system_variables.xml ..\..\release\xml\mysqladmin_system_variables.xml

echo .

rem copy fonts

rem copy translations

echo Copying translations files
copy ..\res\MakeMo.cmd ..\..\release\locale\
copy ..\..\common\res\how_to_translate.txt ..\..\release\locale\
copy ..\..\common\res\languages_list.txt ..\..\release\locale\

call MakeTranslations.cmd "..\..\release"

echo .
