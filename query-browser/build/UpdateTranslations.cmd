@echo off

echo Generate ignore file from source ..
dxgettext.exe *.pas *.dfm *.dpr -b ..\source\windows -o ..\res\windows\po --createignorepo
del ..\res\windows\po\default.po
if errorlevel 1 goto error

echo Generate new po file from source ..
dxgettext.exe *.pas *.dfm *.dpr -b ..\source\windows -o ..\res\windows\po --useignorepo
if errorlevel 1 goto error

echo Update the master template file (include translations from all platforms) ..
..\..\win-external-libs\bin\msgcat -s -o ..\po\mysql-query-browser-template.po ..\res\linux\po\mysql-query-browser.po ..\res\windows\po\default.po
if errorlevel 1 goto error

echo Update all po files that have already been translated
for %%x in (..\po\*.po) do call MakePoMergeSingleFile.cmd %%x
if errorlevel 1 goto error

echo All translation files have been updated.
goto endOfScript

:error
echo An Error occured!

:endOfScript
echo .
