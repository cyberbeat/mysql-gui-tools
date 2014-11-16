@echo off

echo Collecting external text sources into the external.po file.
if exist ..\bin\Windows\locale\administrator-missing.po (
..\..\win-external-libs\bin\msgcat -s -o ..\res\windows\po\external.po ..\res\windows\po\external.po ..\bin\Windows\locale\administrator-missing.po
if errorlevel 1 goto error
echo Missing MA strings collected.
)

echo External sources finished.
goto finish

:error
echo An Error occured!

:finish
echo .
