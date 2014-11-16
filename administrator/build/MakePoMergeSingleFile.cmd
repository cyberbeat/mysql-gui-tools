@echo off

if "%1"=="..\po\mysql-administrator-template.po" goto finish

echo Merging %1 file...
..\..\win-external-libs\bin\msgmerge --no-fuzzy-matching -U -s %1 ..\po\mysql-administrator-template.po

:finish
echo .
