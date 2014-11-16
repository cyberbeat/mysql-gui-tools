@echo off

echo Extracting common PO files...
dxgettext.exe *.pas *.dfm *.dpr -b ..\source\windows -o ..\res\windows\po --useignorepo
if not errorlevel 1 goto endOfScript
echo An Error occured!
pause

:endOfScript
echo .
