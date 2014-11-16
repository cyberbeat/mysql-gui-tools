@echo off

echo Generate ignore file from source ..
dxgettext.exe *.pas *.dfm *.dpr -b ..\source\windows -o ..\res\windows\po --createignorepo
del ..\res\windows\po\default.po
echo.
if errorlevel 1 goto error

echo Generate new po file from source ..
dxgettext.exe *.pas *.dfm *.dpr -b ..\source\windows -o ..\res\windows\po --useignorepo
echo.
if errorlevel 1 goto error

echo Update the master template file (include translations from all platforms) ..
rem ..\..\win-external-libs\bin\msgcat -s -o ..\po\mysql-gui-common-template.po ..\res\linux\po\mysql-gui-common.po ..\res\windows\po\default.po
..\..\win-external-libs\bin\msgcat -s -o ..\po\mysql-gui-common-template.po ..\res\windows\po\default.po
echo.
if errorlevel 1 goto error

echo Update all po files that have already been translated
for %%x in (..\po\*.po) do call MakePoMergeSingleFile.cmd %%x
echo.
if errorlevel 1 goto error

echo All translation files have been updated.
goto finish

:error
echo An Error occured!

:finish
echo .
