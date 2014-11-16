@echo off

if not [%1] == [] pushd %1

rem ------------------------------
rem create directories

echo Create runtime image directories ...
if not exist ..\Output mkdir ..\Output

if not exist bin\windows\xml mkdir bin\windows\xml
if not exist bin\windows\doc mkdir bin\windows\doc
if not exist bin\windows\fonts mkdir bin\windows\fonts
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

echo Copying XML files...
copy ..\common\res\mysqlx_dbm_charsets.xml bin\windows\xml\mysqlx_dbm_charsets.xml
copy ..\common\res\mysqlx_dbm_datatypes.xml bin\windows\xml\mysqlx_dbm_datatypes.xml
echo .

rem ------------------------------
rem copy docs

echo Copying doc files...
copy res\mysqlqb_functions.xml bin\windows\xml\
copy res\mysqlqb_statements.xml bin\windows\xml\
copy res\doc\*.html bin\windows\doc\
copy res\doc\*.css bin\windows\doc\
scp docbuild@docsrva.mysql.com:/home/docbuild/docs-built/query-browser/en/chm/query-browser.chm bin\windows\MySQLQueryBrowser.chm
echo .

rem ------------------------------
rem copy fonts

echo Copying font files...
copy ..\common\res\fonts\VeraMoBd.ttf bin\windows\fonts\VeraMoBd.ttf
copy ..\common\res\fonts\VeraMoBI.ttf bin\windows\fonts\VeraMoBI.ttf
copy ..\common\res\fonts\VeraMoIt.ttf bin\windows\fonts\VeraMoIt.ttf
copy ..\common\res\fonts\VeraMono.ttf bin\windows\fonts\VeraMono.ttf
echo .

rem ------------------------------
rem copy translations

echo Copying translations files
copy res\MakeMo.cmd bin\windows\locale\
copy ..\common\res\how_to_translate.txt bin\windows\locale\
copy ..\common\res\languages_list.txt bin\windows\locale\
echo.

echo -------------------------------------------------------
echo Generating translation files...
echo -------------------------------------------------------
pushd build
call MakeTranslations.cmd "..\bin\windows"
popd
echo.

if not [%1] == [] popd

echo .
