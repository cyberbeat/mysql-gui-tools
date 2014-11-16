@echo off

if "%1"=="..\res\po\mysql-gui-common-template.po" goto finish

echo Merging %1 file...
..\..\win-external-libs\bin\msgmerge --no-fuzzy-matching -U -s %1 ..\po\mysql-gui-common-template.po

:finish
echo .
