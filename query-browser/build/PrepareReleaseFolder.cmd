@echo off

rem ------------------------------
rem Create application directories

echo --------------------------------------------
echo Creating application directories ...
echo --------------------------------------------
if not exist ..\..\release\intermediate mkdir ..\..\release\intermediate
if not exist ..\..\release\xml mkdir ..\..\release\xml
if not exist ..\..\release\doc mkdir ..\..\release\doc
if not exist ..\..\release\fonts mkdir ..\..\release\fonts
if not exist ..\..\release\locale mkdir ..\..\release\locale
echo .

echo --------------------------------------------
echo Copying licence files ...
echo --------------------------------------------
if exist ..\..\release\COPYING del ..\..\release\COPYING
if exist ..\..\release\MySQLEULA.txt del ..\..\release\MySQLEULA.txt
copy ..\..\common\res\COPYING ..\..\release\COPYING
echo .

echo --------------------------------------------
echo Copying XML files...
echo --------------------------------------------
copy ..\..\common\res\mysqlx_dbm_charsets.xml ..\..\release\xml\mysqlx_dbm_charsets.xml
copy ..\..\common\res\mysqlx_dbm_datatypes.xml ..\..\release\xml\mysqlx_dbm_datatypes.xml
echo .

echo --------------------------------------------
echo Copying doc files...
echo --------------------------------------------
copy ..\res\mysqlqb_functions.xml ..\..\release\xml\
copy ..\res\mysqlqb_statements.xml ..\..\release\xml\
copy ..\res\doc\*.html ..\..\release\doc\
copy ..\res\doc\*.css ..\..\release\doc\
rem In the release build there is a separate UpdateDocs batch that also copies the chm help file.
echo .

rem ------------------------------
rem copy fonts

echo --------------------------------------------
echo Copying font files...
echo --------------------------------------------
copy ..\..\common\res\fonts\VeraMoBd.ttf ..\..\release\fonts\VeraMoBd.ttf
copy ..\..\common\res\fonts\VeraMoBI.ttf ..\..\release\fonts\VeraMoBI.ttf
copy ..\..\common\res\fonts\VeraMoIt.ttf ..\..\release\fonts\VeraMoIt.ttf
copy ..\..\common\res\fonts\VeraMono.ttf ..\..\release\fonts\VeraMono.ttf
echo .

rem ------------------------------
rem copy translations

echo Copying localization helper files
copy ..\res\MakeMo.cmd ..\..\release\locale\
copy ..\..\common\res\how_to_translate.txt ..\..\release\locale\
copy ..\..\common\res\languages_list.txt ..\..\release\locale\
echo.

echo -------------------------------------------------------
echo Generating translation files...
echo -------------------------------------------------------
call MakeTranslations.cmd "..\..\release"

echo .
