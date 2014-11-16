@echo off

rem 2 parameters are expected. The first one is the language identifier.
rem The second one the target path where the results are stored.

if [%1]==[mysql-administrator-template] goto finish


if not exist %~2\locale\%1\LC_MESSAGES mkdir "%~2\locale\%1\LC_MESSAGES"

echo Merging with common strings ...
..\..\win-external-libs\bin\msgcat -s -o ..\po\tmp.po ..\po\%1.po ..\..\common\po\%1.po
if errorlevel 1 goto finish

echo Generating binary MO file for %1 ...
..\..\win-external-libs\bin\msgfmt -o "%~2\locale\%1\LC_MESSAGES\administrator.mo" ..\po\tmp.po

:finish

if exist ..\po\tmp.po del ..\po\tmp.po

echo .