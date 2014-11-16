@echo off

echo Concatenation of .po files from different OSs...
..\..\mysql-gui-win-res\bin\msgcat -s -o ..\res\po\mysql-gui-common-template.po ..\res\linux\po\mysql-gui-common.po ..\res\windows\po\default.po
if not errorlevel 1 goto endOfScript
echo An Error occured!
pause

:endOfScript
echo .
