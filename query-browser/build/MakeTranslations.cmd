@echo off

echo Generating mo files...
for %%x in (..\po\*.po) do call MakeMoSingleFile.cmd %%~nx %1
if not errorlevel 1 goto finish

:error
echo An Error occured!

:finish
echo .
