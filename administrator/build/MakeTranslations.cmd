@echo off

echo Generating mo files...

if [%1] == [] (
  set targetPath=..\bin\windows
) else (
  set targetPath=%1
)

for %%x in (..\po\*.po) do call MakeMoSingleFile.cmd %%~nx %targetPath%
if not errorlevel 1 goto finish

:error
echo An Error occured!

:finish
echo .
