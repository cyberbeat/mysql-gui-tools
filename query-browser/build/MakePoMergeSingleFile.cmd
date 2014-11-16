@echo off

if [%1]==[mysql-query-browser-template] goto finish

echo Merging %1 file...
..\..\win-external-libs\bin\msgmerge --no-fuzzy-matching -U -s %1 ..\po\mysql-query-browser-template.po

:finish
echo .
